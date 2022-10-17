unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Threading,
  System.Math,
  System.Math.Vectors,
  Generics.Collections,
  Generics.Defaults,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Viewport3D,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Objects,
  FMX.Utils,

  IntersectMath;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Text1: TText;
    Timer1: TTimer;
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Timer1Timer(Sender: TObject);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TDirection = (dNorth=0, dWest, dSouth, dEast);
  TDirectionHelper = record helper for TDirection
    function Opposite: TDirection;
  end;

  TEdge = class(TBasicLine)
    index: integer;
    procedure Extend(direction: TDirection);
  end;

  TTile = class
    X, Y: integer;
    origin: TPoint;
    touches_edges: array[dNorth..dEast] of TEdge;
    function Get_neighbor(direction: TDirection): TTile;
    function Get_edge(direction: TDirection): TEdge;
    procedure Get_or_create_edges;
    procedure Try_to_create_edge(direction: TDirection);
    procedure Create_new_edge(location: TDirection);
    constructor Create(position: TPoint);
    procedure   Remove(position: TPoint);
  end;

  TLayers = class
    dict: TObjectDictionary<string,TBitmap>;
    buffer: TBitmap;
    light: TBitmap;
    rect: TRect;
    default_brush: TBrush;
    invalidated: boolean;
    procedure Push_to_buffer(bitmapName: string; opacity: single = 1);
    procedure Push_all;
    procedure Clear_Canvas;
    procedure Draw_light;
    procedure Invalidate;
    constructor Create;
    destructor  Destroy; override;
  end;

const
  Grid_size = 20;
  Form_size = 800;

var
  Form1: TForm1;
  painting_tiles: boolean;
  mouse_position: TPointF;
  light_position: TPointF;
  tile_size,tile_count: integer;
  ray_count,intersect_count: integer;
  visibility_polygon_points: integer;
  display_stats: boolean;

  tiles:      array of array of TTile;
  edges:      TObjectList<TEdge>;
  vertices:   TList<TPoint>;
  polygons:   TList<TPolygon>;
  rays:       TThreadList<TVector>;
  intersects: TThreadList<TPointF>;
  visibility_polygon: TPolygon;

  Layers: TLayers;
  Raycast_tasks: array of ITask;
  debug_data: TStringList;


procedure Draw_dynamic_objects;
procedure Draw_fixed_objects;
procedure Update_dynamic_objects;
procedure Update_fixed_objects;
procedure Redraw_scene;

implementation

{$R *.fmx}


// loosely following tutorial from https://www.youtube.com/watch?v=fc3nnG2CG8U

procedure Breakpoint_placeholder;
begin

end;

function Get_vector_angle(ray: TVector): Extended;
begin
  var ray_as_point:= TPointF(ray);
  result:= ArcTan2(ray_as_point.Y,ray_as_point.X);
end;

procedure Place_scene_tiles;
begin
  const max_value = Grid_size-1;
  for var i := 0 to Grid_size-1 do
    begin
      TTile.Create(TPoint.Create(i,0));
      TTile.Create(TPoint.Create(0,i));
      TTile.Create(TPoint.Create(max_value,i));
      TTile.Create(TPoint.Create(i,max_value));
    end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  tile_count:= 0;
  setLength(tiles, 20, 20);
  tile_size:= round(Form1.Viewport3D1.Width / Grid_size); // 40x40;

  Layers:= TLayers.Create;

  edges:=       TObjectList<TEdge>.Create(true);
  vertices:=    TList<TPoint>.Create;
  polygons:=    TList<TPolygon>.Create;
  rays:=        TThreadList<TVector>.Create;
  intersects:=  TThreadList<TPointF>.Create;

  TTile.Create(TPoint.Create(4,4));
  TTile.Create(TPoint.Create(5,4));
  TTile.Create(TPoint.Create(4,5));
  TTile.Create(TPoint.Create(5,5));

  (*
  TTile.Create(TPoint.Create(10,10));
  TTile.Create(TPoint.Create(10,11));
  TTile.Create(TPoint.Create(11,10));
  TTile.Create(TPoint.Create(11,11));
  *)
  Place_scene_tiles;
end;

procedure Simplify_visibility_polygon;
var points: TList<TPointF>;

  function Points_on_same_axis(p1,p2: TPointF): boolean;
  begin
    var same_x_axis:= p1.X = p2.X;
    var same_y_axis:= p1.Y = p2.Y;
    result:= same_x_axis OR same_y_axis;
  end;

  procedure Remove_duplicate_points;
  begin
    for var i:= points.Count-1 downto 1 do
      begin
        var p1:= points[i];
        var p2:= points[i-1];

        var duplicate_point:= p1=p2;
        if  duplicate_point then
          points.Remove(p1);
      end;
  end;

  procedure Skip_unnecessary_points_on_edges;
  var p1,p2,p3: TPointF;

    function Middle_point_unnecessary(p1,p2,p3: TPointF): boolean;
    begin
      result:= false;
      if not Points_on_same_axis(p1,p3) then exit;
      if vertices.Contains(p2.Round)    then exit;

      result:= Points_on_same_axis(p1,p2) AND Points_on_same_axis(p2,p3);
    end;

    function Safe_get_point(index: integer): TPointF;
    begin
      if index<0 then
         index:= points.Count+index;

      result:= points[index];
    end;

  begin
    for var i:= points.Count-1 downto 0 do
      begin
        p1:= Safe_get_point(i);
        p2:= Safe_get_point(i-1);
        p3:= Safe_get_point(i-2);
        if Middle_point_unnecessary(p1,p2,p3) then
          points.Remove(p2);
      end;
  end;

  procedure Export_vis_poly_points_to_file(filename: string);
  begin
    var exported:= TStringList.Create;
    try
      for var point in points do
        exported.Add( point.X.ToString +', '+ point.Y.ToString);
      exported.SaveToFile(filename);
    finally
      exported.Free;
    end;
  end;

  procedure Move_points_to_list;
  begin
    for var point in visibility_polygon do
      points.Add(point);
    setLength(visibility_polygon,0);
  end;

  procedure Move_points_back_to_polygon;
  begin
    setLength(visibility_polygon,points.Count);
    for var i:= 0 to points.Count-1 do
      visibility_polygon[i]:= points[i];
  end;

begin
  visibility_polygon_points:= length(visibility_polygon);

  points:= TList<TPointF>.Create;
  try
    Move_points_to_list;
    //Export_vis_poly_points_to_file('poly1.txt');
    Remove_duplicate_points;
    Skip_unnecessary_points_on_edges;
    Move_points_back_to_polygon;
    //Export_vis_poly_points_to_file('poly2.txt');

  finally
    points.Free;
  end;
end;

procedure Draw_Visibility_Polygon;
begin
  var layer:= Layers.dict.Items['vis_poly'];
  layer.Canvas.BeginScene;
  try
    layer.Canvas.Clear(TAlphaColorRec.Null);
    var lightBrush:= TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Black);
    layer.Canvas.Fill:= lightBrush;
    layer.Canvas.FillPolygon(visibility_polygon,1);

    (*
    const circle_radius = 2;
    var purpleBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Purple);
    purpleBrush.Thickness:=1;

    for var polypoint in visibility_polygon do
      begin
        var rect:= TRectF.Create(
          polypoint.X-circle_radius,
          polypoint.Y-circle_radius,
          polypoint.X+circle_radius,
          polypoint.Y+circle_radius
        );
        layer.Canvas.DrawEllipse(rect,1,purpleBrush);
      end;
    *)

  finally
    layer.Canvas.EndScene;
  end;
end;

procedure Draw_Intersects;
begin
  const circle_radius = 3;

  var greenBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Lime);
  greenBrush.Thickness:=1;

  var list:= intersects.LockList;
  try
    for var intersect in list do
      begin
        var rect:= TRectF.Create(
          intersect.X-circle_radius,
          intersect.Y-circle_radius,
          intersect.X+circle_radius,
          intersect.Y+circle_radius
        );
        form1.Viewport3D1.Canvas.DrawEllipse(rect,1,greenBrush);
      end;

  finally
    intersects.UnlockList;
  end;
end;

procedure Draw_Rays;
var light_point: TPointF;
begin
  var yellowBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Yellow);
  yellowBrush.Thickness:=1;

  light_point:= TPointF(light_position);

  var list:= rays.LockList;
  try
  for var ray in list do
    form1.Viewport3D1.Canvas.DrawLine(light_point,TPointF(ray),1,yellowBrush);
  finally
    rays.UnlockList;
  end;
end;

procedure Draw_Vertices;
begin
  const circle_radius = 3;
  var redBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);
  redBrush.Thickness:=3;

  for var vertex in vertices do
    begin
      var rect:= TRectF.Create(
        vertex.X-circle_radius,
        vertex.Y-circle_radius,
        vertex.X+circle_radius,
        vertex.Y+circle_radius
        );
      form1.Viewport3D1.Canvas.DrawEllipse(rect,1,redBrush);
    end;
end;

procedure Draw_Edges;
begin
  var whiteBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.White);
  whiteBrush.Thickness:=1;

  for var edge in edges do
    form1.Viewport3D1.Canvas.DrawLine(edge.starts,edge.ends,1,whiteBrush);
end;

procedure Draw_Tiles;
begin
  var layer:= Layers.dict.Items['tiles'];
  layer.Canvas.BeginScene;
  try
    layer.Canvas.Clear(TAlphaColorRec.Null);
    var blueBrush:= TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Blue);

    for var x := 0 to Grid_size-1 do
    for var y := 0 to Grid_size-1 do
      begin
        var tile:= tiles[X,Y];
        if tile=nil then continue;

        var rect:= TRectF.Create(tile.origin,tile_size,tile_size);
        layer.Canvas.FillRect(rect,1,blueBrush);
      end;

  finally
    layer.Canvas.EndScene;
  end;
end;

procedure Draw_Polygons;
begin
  var purpleBrush:= TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Purple);
  form1.Viewport3D1.Canvas.Fill:= purpleBrush;

  for var polygon in polygons do
    form1.Viewport3D1.Canvas.FillPolygon(polygon,0.5);
end;

function Add_new_ray(ray:TVector): boolean;
begin
  result:= true;
  if (ray = TVector.Zero) OR
     (ray.Length>5000)    then exit(false);

  var list:= rays.LockList;
  try
     if (list.Contains(ray)) then exit(false);
  finally
    rays.UnlockList;
  end;

  Rays.Add(ray);
end;

function Compensate_vector_for_light_position(ray:TVector): TVector;
var point,compensated_point: TPointF;
begin
  point:= TPointF(ray);
  compensated_point:= point - light_position;
  result:= TVector.Create(compensated_point);
end;

function Point_to_string(point: TPointF): string;
begin
  result:= point.X.ToString +', '+point.Y.ToString;
end;

function Find_closest_intersect(ray: TVector): TVector;
var intersect: TPointF;
begin
  var shortest_ray:= TVector.Create(Infinity,Infinity);
  var shortest_len:= Infinity;

  var line_ray:= TBasicLine.Create;
  try
    line_ray.starts:= TPointF(light_position);
    line_ray.ends:=   TPointF(ray);

    for var edge in edges do
      begin
        var line_edge:= TBasicLine(edge);

        try
          intersect:= Find_intersection_point(line_edge,line_ray);
        except
          on E: ELineSegmentDontIntersect do continue;
          on E: ELinesParallel            do continue;
        end;

        intersects.add(intersect);

        var len:= TPointF(light_position).Distance(intersect);
        if  len < shortest_len then
          begin
            shortest_len:= len;
            shortest_ray:= TVector.Create(intersect);
          end;
      end;

    result:= shortest_ray;

  finally
    if line_ray <> nil then
      line_ray.Free;
  end;
end;

function Adjusted_point(point: TPointF): TPointF;
begin
  result:= point - light_position;
end;

function Distance_light_to_point(point: TpointF): single;
begin
  result:= TPointF(light_position).Distance(point);
end;

function Distance_light_to_unadjusted_vector(ray: TVector): single;
begin
  var vector_point:= TPointF(ray);
  result:= Distance_light_to_point(vector_point);
end;

const new_vector_length = 2000;
const angle_move = 0.01;

function Create_new_ray(angle: double): TVector;
begin
  var rdx:= new_vector_length * cos(angle);
  var rdy:= new_vector_length * sin(angle);
  var adjust_for_light_pos:= TPointF.Create(rdx,rdy) + light_position;
  var raw_vector:= TVector.Create(adjust_for_light_pos);
  result:= raw_vector;
end;

procedure Add_ray_which_can_see_further(ray1,ray2: TVector);
begin
  var ray1_len:= Distance_light_to_unadjusted_vector(ray1);
  var ray2_len:= Distance_light_to_unadjusted_vector(ray2);

  if ray1_len > ray2_len then
    Add_new_ray(ray1)
  else
    Add_new_ray(ray2);
end;

procedure Start_raycast_task(index: integer; vertex: TPointF);
begin
  Raycast_tasks[index] := TTask.Run(procedure ()
    begin
      var v_middle:= TVector.Create(vertex);
      var v_middle_shortest:= Find_closest_intersect(v_middle);
      Add_new_ray(v_middle_shortest);

      var direct_LOS:= v_middle = v_middle_shortest;
      if not direct_LOS then exit;

      var adjusted_vertex:= Adjusted_point(vertex);
      var angle:= Get_vector_angle(adjusted_vertex);
      var angle_higher:= angle + angle_move;
      var angle_lower := angle - angle_move;

      var v_higher:= Create_new_ray( angle_higher );
      var v_lower := Create_new_ray( angle_lower  );
      var v_higher_shortest:= Find_closest_intersect(v_higher);
      var v_lower__shortest:= Find_closest_intersect(v_lower);

      Add_ray_which_can_see_further(v_higher_shortest,v_lower__shortest);
    end);
end;

procedure Cast_rays;

begin
  Setlength(Raycast_tasks, vertices.Count);

  TParallel.For(0, vertices.Count-1, procedure(index: Integer)
  begin
    Start_raycast_task(index,vertices[index]);
  end);

  TTask.WaitForAll(Raycast_tasks);

  var list:= rays.LockList;
  try
    ray_count:= list.Count;
  finally
    rays.UnlockList;
  end;

  var list2:= intersects.LockList;
  try
    intersect_count:= list2.Count;
  finally
    intersects.UnlockList;
  end;
end;

procedure Calculate_Rays;

  procedure Sort_rays_by_angle;
  var Comparison: TComparison<TVector>;
  begin
    Comparison := function(const Left, Right: TVector): Integer
    begin
      var left2:= Compensate_vector_for_light_position(left);
      var left_angle:= Get_vector_angle(left2);
      var right2:= Compensate_vector_for_light_position(right);
      var right_angle:= Get_vector_angle(right2);

      var raw:= left_angle - right_angle;
      if raw>0 then
        result:= ceil(raw)
      else
        result:= floor(raw);
    end;

    var list:= rays.LockList;
    try
      list.Sort(TComparer<TVector>.Construct(Comparison));
    finally
      rays.UnlockList;
    end;
  end;

begin
  Rays.Clear;
  intersects.Clear;
  debug_data.Clear;

  Cast_rays;
  Sort_rays_by_angle;
end;

procedure Calculate_vertices;

  function Vertex_already_known(tested_vertex: TPointF): boolean;
  begin
    result:= false;
    for var vertex in vertices do
      if vertex = tested_vertex then
        exit(true);
  end;

begin
  vertices.Clear;

  for var edge in edges do
    begin
      if not Vertex_already_known(edge.starts.Round) then
        vertices.Add(edge.starts.Round);
      if not Vertex_already_known(edge.ends.Round) then
        vertices.Add(edge.ends.Round);
    end;
end;

procedure Calculate_Visibility_Polygon;

  procedure Add_point_to_vis_polygon(point: TPointF);
  begin
    setLength(visibility_polygon,length(visibility_polygon)+1);
    visibility_polygon[length(visibility_polygon)-1]:= point;
  end;

begin
  setLength(visibility_polygon,0);

  var list:= rays.LockList;
  try
    for var ray in list do
      begin
        var ray_point:= TPointF(ray);
        Add_point_to_vis_polygon(ray_point);
      end;
  finally
    rays.UnlockList;
  end;

  Simplify_visibility_polygon;
end;

procedure Calculate_polygons;
var new_endpoint: TPointF;
    polygon: TPolygon;

  procedure Add_point_to_polygon(point: TPointF);
  begin
    setLength(polygon,length(polygon)+1);
    polygon[length(polygon)-1]:= point;
  end;

  function Find_attached_edge(last_endpoint: TPointF; polyEdges: TObjectList<TEdge>): TEdge;
  begin
    for var edge in polyEdges do
      begin
        if edge.starts=last_endpoint then
          exit( polyEdges.Extract(edge) );

        if edge.ends=last_endpoint then
          exit( polyEdges.Extract(edge) );
      end;

    raise Exception.Create('Disconnected edge!');
  end;

begin
  polygons.Clear;

  var polyEdges:= TObjectList<TEdge>.Create(true);
  polyEdges.AddRange(edges);

  while polyEdges.Count>0 do
    begin
      var edge:= polyEdges.ExtractAt(0);

      setLength(polygon,0);
      var first_point:= edge.starts;
      Add_point_to_polygon(edge.starts.Round);
      Add_point_to_polygon(edge.ends.Round);
      var last_endpoint:= edge.ends;

      repeat
        edge:= Find_attached_edge(last_endpoint.Round, polyEdges);

        if edge.starts=last_endpoint then
          new_endpoint:= edge.ends.Round
        else
          new_endpoint:= edge.starts.Round;

        last_endpoint:= new_endpoint;
        Add_point_to_polygon(new_endpoint);

      until first_point = new_endpoint;

      Add_point_to_polygon(first_point.Round);
      polygons.Add(polygon);
    end;
end;

procedure Calculate_edges;

  procedure Delete_all_known_edges;
  begin
    if edges.Count>0 then
      edges.DeleteRange(0,edges.Count);

    for var x := 0 to Grid_size-1 do
    for var y := 0 to Grid_size-1 do
      begin
        var tile:= tiles[X,Y];
        if tile=nil then continue;

        for var direction := TDirection.dNorth to TDirection.dEast do
          tile.touches_edges[direction]:= nil;
      end;
  end;

begin
  Delete_all_known_edges;

  for var x := 0 to Grid_size-1 do
  for var y := 0 to Grid_size-1 do
    begin
      var tile:= tiles[X,Y];
      if tile=nil then continue;
      tile.Get_or_create_edges;
    end;
end;

function Mouse_coords_to_tile_pos(mouseX, mouseY: Single): TPoint;
begin
  result.X := round(mouseX) div Tile_size;
  result.Y := round(mouseY) div Tile_size;
end;

procedure Work_with_tile_under_cursor(mouse_over_tile: TPoint);
begin
  if mouse_over_tile.X>Grid_size-1 then exit;
  if mouse_over_tile.Y>Grid_size-1 then exit;

  var tile:= tiles[mouse_over_tile.X,mouse_over_tile.Y];
  var tile_already_exists:= tile<>nil;

  if painting_tiles then
    begin
      if tile_already_exists then exit;
      TTile.Create(mouse_over_tile);
    end
  else
    begin
      if not tile_already_exists then exit;
      tile.Remove(mouse_over_tile);
    end;

  Redraw_scene;
end;

procedure Move_light(X,Y: single);
begin
  var new_pos:= TPoint.Create(round(X),round(Y));
  if light_position = new_pos then exit;

  light_position:= new_pos;
  Update_dynamic_objects;
  Layers.Invalidate;
  Redraw_scene;
end;

{ TTile }

constructor TTile.Create(position: TPoint);
begin
  self.X:= position.X;
  self.Y:= position.Y;
  self.origin:= TPoint.Create(X*tile_size,Y*tile_size);
  tiles[position.X,position.Y]:= self;
  Update_fixed_objects;
  inc(tile_count);
end;

procedure TTile.Remove(position: TPoint);
begin
  tiles[position.X,position.Y].Free;
  tiles[position.X,position.Y]:= nil;
  Update_fixed_objects;
  dec(tile_count);
end;

function TTile.Get_neighbor(direction: TDirection): TTile;
begin
  result:= nil;

  if (direction = TDirection.dNorth) then
    begin
      if (Y = 0) then exit;
      result:= tiles[X,Y-1];
    end;

  if (direction = TDirection.dWest) then
    begin
      if (X = 0) then exit;
      result:= tiles[X-1,Y];
    end;

  if (direction = TDirection.dSouth) then
    begin
      if (Y = Grid_size - 1) then exit;
      result:= tiles[X,Y+1];
    end;

  if (direction = TDirection.dEast) then
    begin
      if (X = Grid_size - 1) then exit;
      result:= tiles[X+1,Y];
    end;
end;

function TTile.Get_edge(direction: TDirection): TEdge;
begin
  result:= touches_edges[direction];
end;

procedure TTile.Create_new_edge(location: TDirection);
begin
  var edge:= TEdge.Create;
  edge.index:= edges.Count+1;

  case location of
    dNorth:
      begin
        edge.starts:= origin;
        edge.ends:=   TPoint.Create(origin.X+tile_size,origin.Y);
      end;
    dWest:
      begin
        edge.starts:= origin;
        edge.ends:=   TPoint.Create(origin.X,origin.Y+tile_size);
      end;
    dSouth:
      begin
        edge.starts:= TPoint.Create(origin.X,origin.Y+tile_size);
        edge.ends:=   TPoint.Create(origin.X+tile_size,origin.Y+tile_size);
      end;
    dEast:
      begin
        edge.starts:= TPoint.Create(origin.X+tile_size,origin.Y);
        edge.ends:=   TPoint.Create(origin.X+tile_size,origin.Y+tile_size);
      end;
  end;

  self.touches_edges[location]:= edge;
  edges.Add(edge);
end;

procedure TTile.Try_to_create_edge(direction: TDirection);
begin
  if (self.X=1) AND (self.Y=1) then
    Breakpoint_placeholder;

  var neighbor:= Get_neighbor(direction);                                       // north
  var needs_edge_on_that_side:= neighbor = nil;
  if not needs_edge_on_that_side then exit;

  var secondary_direction: TDirection;
  case direction of
    dNorth,dSouth:secondary_direction:= TDirection.dWest;
    dWest,dEast:  secondary_direction:= TDirection.dNorth;
  end;

  var neighbor2:= Get_neighbor(secondary_direction);                            // west
  var second_neighbor_exists:= neighbor2 <> nil;
  if second_neighbor_exists then
    begin
      var edge:= neighbor2.Get_edge(direction);
      if edge<>nil then
        begin
          edge.Extend(secondary_direction.Opposite);
          self.touches_edges[direction]:= edge;
        end
      else
        Create_new_edge(direction);
    end
  else
    Create_new_edge(direction);
end;

procedure TTile.Get_or_create_edges;
begin
  for var direction := TDirection.dNorth to TDirection.dEast do
    self.Try_to_create_edge(direction);
end;

{ TEdge }

procedure TEdge.Extend(direction: TDirection);
begin
  case direction of
    dNorth: ends.Y:= ends.Y-Tile_size;
    dSouth: ends.Y:= ends.Y+Tile_size;
    dWest:  ends.X:= ends.X-Tile_size;
    dEast:  ends.X:= ends.X+Tile_size;
  end;
end;

{ TDirectionHelper }

function TDirectionHelper.Opposite: TDirection;
begin
  case self of
    dNorth: result:= dSouth;
    dWest:  result:= dEast;
    dSouth: result:= dNorth;
    dEast:  result:= dWest;
    else raise Exception.Create('Direction Error');
  end;
end;

procedure Update_stats_text;
begin
  if not display_stats then exit;
  var mouse_over_tile:= Mouse_coords_to_tile_pos(mouse_position.X,mouse_position.Y);

  (*
  form1.Viewport3D1.Canvas.BeginScene;
  form1.Viewport3D1.Canvas.FillText();
  form1.Viewport3D1.Canvas.EndScene;
  *)

  form1.Text1.text:=
    'Mouse: X:'+mouse_position.X.ToString +', Y:'+ mouse_position.Y.ToString  +sLineBreak+
    'Tile: X:'+ mouse_over_tile.X.ToString+', Y:'+ mouse_over_tile.Y.ToString +sLineBreak+
    sLineBreak+
    'Statistics: '+ sLineBreak+
    'Tiles: '+      Tile_count.ToString +sLineBreak+
    'Edges: '+      Edges.count.ToString +sLineBreak+
    'Vertices: '+   Vertices.count.ToString +sLineBreak+
    'Polygons: '+   Polygons.count.ToString +sLineBreak+
    'Rays: '+       ray_count.ToString +sLineBreak+
    'Intersects: '+ intersect_count.ToString +sLineBreak+
    'Vis poly raw: '+visibility_polygon_points.ToString +sLineBreak+
    'Vis poly simplified: '+length(visibility_polygon).ToString;

  form1.Text1.Repaint;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if form1.Viewport3D1.Tag=0 then
    begin
      form1.Viewport3D1.Tag:= 1;
      Redraw_scene;
    end;

  Update_stats_text;
end;

procedure Determine_painting_mode(X,Y: single);
begin
  var mouse_over_tile:= Mouse_coords_to_tile_pos(X, Y);
  var tile:= tiles[mouse_over_tile.X,mouse_over_tile.Y];
  var tile_already_exists:= tile<>nil;
  painting_tiles:= not tile_already_exists;
end;

procedure TForm1.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  display_stats:= false;
  form1.Text1.Visible:= display_stats;

  case button of
    TMouseButton.mbLeft: Determine_painting_mode(X,Y);
  end;
end;

procedure TForm1.Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  display_stats:= true;
  form1.Text1.Visible:= display_stats;

  case button of
    TMouseButton.mbLeft: Work_with_tile_under_cursor( Mouse_coords_to_tile_pos(X, Y) );
    TMouseButton.mbRight:Move_light(X,Y);
  end;

  Redraw_scene;
end;

procedure TForm1.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  mouse_position:= TPoint.Create(round(X),round(Y));
  if (ssLeft in Shift) then
    begin
      var mouse_over_tile:= Mouse_coords_to_tile_pos(X, Y);
      Work_with_tile_under_cursor(mouse_over_tile);
    end;
  if (ssRight in Shift) then
    begin
      Move_light(X,Y);
    end;

  Update_stats_text;
end;

procedure Draw_dynamic_objects;
begin
  if form1.Viewport3D1.Tag=0 then exit;

  //Draw_Rays;
  //Draw_Intersects;
  Draw_Visibility_Polygon;
end;

procedure Draw_fixed_objects;
begin
  if form1.Viewport3D1.Tag=0 then exit;

  Draw_Tiles;
  //Draw_Edges;
  //Draw_Vertices;
  //Draw_Polygons;
end;

procedure Update_dynamic_objects;
begin
  Calculate_Rays;
  Calculate_Visibility_Polygon;
  Layers.Invalidate;
end;

procedure Update_fixed_objects;
begin
  Calculate_edges;
  Calculate_vertices;
  //Calculate_polygons;

  Draw_fixed_objects;
  Layers.Invalidate;
end;

procedure Redraw_scene;
begin
  Draw_fixed_objects;
  Draw_dynamic_objects;

  Layers.Invalidate;
  Layers.Push_all;
end;

{ TLayers }

constructor TLayers.Create;
begin
  dict:= TObjectDictionary<string,TBitmap>.Create;
  with form1.Viewport3D1.Canvas do
    rect:= TRect.Create(0,0,Width,Height);

  buffer:= TBitmap.Create(rect.Width,rect.Height);
  light := TBitmap.Create(rect.Width,rect.Height);
  light.LoadFromFile('light.png');

  dict.Add('tiles',     TBitmap.Create(rect.Width,rect.Height));
  dict.Add('edges',     TBitmap.Create(rect.Width,rect.Height));
  dict.Add('vertices',  TBitmap.Create(rect.Width,rect.Height));
  dict.Add('polygons',  TBitmap.Create(rect.Width,rect.Height));
  dict.Add('rays',      TBitmap.Create(rect.Width,rect.Height));
  dict.Add('intersects',TBitmap.Create(rect.Width,rect.Height));
  dict.Add('vis_poly',  TBitmap.Create(rect.Width,rect.Height));
  dict.Add('light_poly',TBitmap.Create(rect.Width,rect.Height));

  default_brush:= TBrush.Create(TBrushKind.Solid,TAlphaColorRec.Teal);
  invalidated:= true;
end;

destructor TLayers.Destroy;
begin
  dict.Free;
  inherited;
end;

procedure TLayers.Draw_light;
var PolyData, LightData : TBitmapData;
begin
  var layer_vis_poly:=  Layers.dict.Items['vis_poly'];
  var layer_light_poly:=Layers.dict.Items['light_poly'];

  var mask:= layer_vis_poly.CreateMask;
  layer_light_poly.Assign(light);
  layer_light_poly.ApplyMask(mask,0,0);
  layer_light_poly.SaveToFile('masked.png');
  exit;


  if not (layer_vis_poly. Map(TMapAccess.ReadWrite,PolyData )) OR
     not (light.          Map(TMapAccess.Read,     LightData)) then
     raise Exception.Create('Bitmap mapping failure');

  try
    for var X := 0 to layer_vis_poly.Width  - 1 do
    for var Y := 0 to layer_vis_poly.Height - 1 do
      begin
        var Pixel:= PolyData.GetPixel(X, Y);
        case Pixel of
          TAlphaColorRec.White:
            begin
              var lightPixel:= LightData.GetPixel(X, Y);
              PolyData.SetPixel(X, Y, lightPixel);
            end;
          TAlphaColorRec.Black: ;
          else
            PolyData.SetPixel(X, Y, TAlphaColorRec.Black);
        end;
      end;

  finally
    layer_vis_poly.   Unmap(PolyData);
    layer_light_poly. Unmap(LightData);
  end;
end;

procedure TLayers.Invalidate;
begin
  invalidated:= true;
end;

procedure TLayers.Clear_Canvas;
begin
  buffer.Canvas.BeginScene;
  buffer.Canvas.Clear(TAlphaColorRec.Teal);
  buffer.Canvas.Flush;
  buffer.Canvas.EndScene;
end;

procedure TLayers.Push_all;
begin
  if invalidated then
    begin
      invalidated:= false;
      Clear_Canvas;
      Push_to_buffer('tiles');
      Draw_light;
      Push_to_buffer('light_poly',1);

      with form1.Viewport3D1.Canvas do
        begin
          BeginScene;
          DrawBitmap(buffer,rect,rect,1,true);
          EndScene;
        end;
    end;

  buffer.SaveToFile('buffer.png');
end;

procedure TLayers.Push_to_buffer(bitmapName: string; opacity: single = 1);
begin
  var layer:= Layers.dict.Items[bitmapName];

  buffer.Canvas.BeginScene;
  buffer.Canvas.DrawBitmap(layer,rect,rect,opacity,true);
  buffer.Canvas.EndScene;

  layer.SaveToFile(bitmapName+'.png');
end;

initialization
  debug_data:= TStringList.Create;

finalization
  debug_data.Free;

end.
