unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
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
    procedure FormCreate(Sender: TObject);
    procedure Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Timer1Timer(Sender: TObject);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
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

const
  Grid_size = 20;
  Form_size = 800;

var
  Form1: TForm1;
  painting_tiles: boolean;
  tile_size: integer;
  tile_count: integer;
  created_rays: integer;

  tiles:      array of array of TTile;
  edges:      TObjectList<TEdge>;
  vertices:   TList<TPoint>;
  polygons:   TList<TPolygon>;
  rays:       TList<TVector>;
  intersects: TList<TPointF>;

  visibility_polygon: TPolygon;

implementation

{$R *.fmx}

// loosely following tutorial from https://www.youtube.com/watch?v=fc3nnG2CG8U

procedure Breakpoint_placeholder;
begin

end;

procedure Place_edges_on_screen_borders;
begin
  var index:= 0;
  for var direction := TDirection.dNorth to TDirection.dEast do
    begin
      var edge:= TEdge.Create;
      edge.index:= index;
      case direction of
        dNorth:
          begin
            edge.starts:= TPointF.Zero;
            edge.ends  := TPointF.Create(Form1.Width,0);
          end;
        dWest:
          begin
            edge.starts:= TPointF.Zero;
            edge.ends  := TPointF.Create(0,Form1.Height);
          end;
        dSouth:
          begin
            edge.starts:= TPointF.Create(0,Form1.Height);
            edge.ends  := TPointF.Create(Form1.Width,Form1.Height);
          end;
        dEast:
          begin
            edge.starts:= TPointF.Create(Form1.Width,0);
            edge.ends  := TPointF.Create(Form1.Width,Form1.Height);
          end;
        else raise Exception.Create('Error Message');
      end;

      edges.Add(edge);
      inc(index);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  tile_count:= 0;
  setLength(tiles, 20, 20);
  tile_size:= round(Form1.Viewport3D1.Width / Grid_size); // 40x40;

  edges:=       TObjectList<TEdge>.Create(true);
  vertices:=    TList<TPoint>.Create;
  polygons:=    TList<TPolygon>.Create;
  rays:=        TList<TVector>.Create;
  intersects:=  TList<TPointF>.Create;

  TTile.Create(TPoint.Create(5,1));
  TTile.Create(TPoint.Create(5,2));
  TTile.Create(TPoint.Create(5,3));
  TTile.Create(TPoint.Create(5,4));
  TTile.Create(TPoint.Create(5,5));
end;

procedure Draw_intersection(intersect: TPointF);
begin
  const circle_radius = 3;

  var greenBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Lime);
  greenBrush.Thickness:=1;

  var rect:= TRectF.Create(
    intersect.X-circle_radius,
    intersect.Y-circle_radius,
    intersect.X+circle_radius,
    intersect.Y+circle_radius
    );
  form1.Viewport3D1.Canvas.DrawEllipse(rect,1,greenBrush);
end;

procedure Check_ray_against_edges(ray: TVector);
begin
  for var edge in edges do
    begin
      var line1:= TBasicLine(edge);
      var line2:= TBasicLine.Create;
      try
        line2.starts:= TPointF.Zero;
        line2.ends:= TPointF(ray);

        var intersect: TPointF;
        try
          intersect:= Find_intersection_point(line1,line2);
        except
          continue;
        end;

        if intersect = edge.starts  then continue;
        if intersect = edge.ends    then continue;

        intersects.add(intersect);
        var new_ray:= TVector.Create(intersect);
        rays.Add(new_ray);

        Draw_intersection(intersect);

      finally
        if line2 <> nil then
          line2.Free;
      end;
    end;
end;

procedure Define_all_rays;
begin
  for var vertex in vertices do
    begin
      var ray:= TVector.Create(vertex);
      Check_ray_against_edges(ray);
      rays.Add(ray);
    end;

  created_rays:= rays.Count;

  var all_vector_angles:= TList<Single>.Create;
  try
    for var ray in rays do
      begin
        var ray_as_point:= TPointF(ray);
        var angle:= ArcTan2(ray_as_point.Y,ray_as_point.X);
        //var deg:= RadToDeg(angle);

        if not all_vector_angles.Contains(angle) then
          all_vector_angles.Add(angle);
      end;

    const tolerance = 0.0001;

    var angle_list:= TStringList.Create;
    try
      for var angle in all_vector_angles do
        angle_list.Add(angle.ToString);
      angle_list.SaveToFile('angles.txt');
    finally
      angle_list.Free;
    end;

    var ray_angles:= TStringList.Create;
    try
      for var ray in rays do
        begin
          var ray_as_point:= TPointF(ray);
          var ray_angle:= ArcTan2(ray_as_point.Y,ray_as_point.X);
          ray_angles.Add(ray_angle.ToString);
        end;
      ray_angles.SaveToFile('ray_angles.txt');
    finally
      ray_angles.Free;
    end;

    for var angle in all_vector_angles do
      begin
        var previous_shortest_ray:= TVector.Create(Infinity,Infinity);

        for var ray in rays do
          begin
            var ray_as_point:= TPointF(ray);
            var ray_angle:= ArcTan2(ray_as_point.Y,ray_as_point.X);

            var identical_angles:= ray_angle = angle;
            var within_tolerance:= abs(ray_angle - angle) < tolerance;
            if not identical_angles AND within_tolerance then
              Breakpoint_placeholder;

            if not(identical_angles OR within_tolerance) then continue;

            if ray.Length < previous_shortest_ray.Length then
              begin
                if rays.Contains(previous_shortest_ray) then
                   rays.Remove(previous_shortest_ray);
                previous_shortest_ray:= ray;
              end
            else
              rays.Remove(ray);
          end;
      end;

    var ray_angles2:= TStringList.Create;
    try
      for var ray in rays do
        begin
          var ray_as_point:= TPointF(ray);
          var ray_angle:= ArcTan2(ray_as_point.Y,ray_as_point.X);
          ray_angles2.Add(ray_angle.ToString);
        end;
      ray_angles2.SaveToFile('ray_angles2.txt');
    finally
      ray_angles2.Free;
    end;

  finally
    all_vector_angles.Free;
  end;
end;

procedure Draw_Rays;
begin
  Rays.Clear;
  setLength(visibility_polygon,0);
  Define_all_rays;

  var yellowBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Yellow);
  yellowBrush.Thickness:=1;

  var vector_to_center:= TVector.Create(TPointF.Create(Form_size,Form_size));
  var center_point:= TPointF(vector_to_center);

  var top_left_point:= TPointF.Zero;

  for var ray in rays do
    form1.Viewport3D1.Canvas.DrawLine(top_left_point,TPointF(ray),10,yellowBrush);
end;

function Vertex_already_known(tested_vertex: TPoint): boolean;
begin
  result:= false;
  for var vertex in vertices do
    if vertex = tested_vertex then
      exit(true);
end;

procedure Extract_all_vertices_from_edges;
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

procedure Draw_Vertices;
begin
  Extract_all_vertices_from_edges;

  var redBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);
  redBrush.Thickness:=3;

  const circle_radius = 3;

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

function Find_attached_edge(last_endpoint: TPoint; polyEdges: TObjectList<TEdge>): TEdge;
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

procedure Create_polygons_from_edges;
var new_endpoint: TPoint;
    polygon: TPolygon;

  procedure Add_point_to_polygon(point: TPoint);
  begin
    setLength(polygon,length(polygon)+1);
    polygon[length(polygon)-1]:= point;
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

procedure Draw_Polygons;
begin
  Create_polygons_from_edges;

  var purpleBrush:= TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Purple);
  form1.Viewport3D1.Canvas.Fill:= purpleBrush;

  for var polygon in polygons do
    form1.Viewport3D1.Canvas.FillPolygon(polygon,0.5);
end;

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

procedure Draw_Edges;
begin
  Delete_all_known_edges;
  //Place_edges_on_screen_borders;

  for var x := 0 to Grid_size-1 do
  for var y := 0 to Grid_size-1 do
    begin
      var tile:= tiles[X,Y];
      if tile=nil then continue;
      tile.Get_or_create_edges;
    end;

  var whiteBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.White);
  whiteBrush.Thickness:=1;

  for var edge in edges do
    form1.Viewport3D1.Canvas.DrawLine(edge.starts,edge.ends,1,whiteBrush);
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
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  form1.Text1.text:=
    'Statistics: '+ sLineBreak+
    'Tiles: '+      Tile_count.ToString +sLineBreak+
    'Edges: '+      Edges.count.ToString +sLineBreak+
    'Vertices: '+   Vertices.count.ToString +sLineBreak+
    'Polygons: '+   Polygons.count.ToString +sLineBreak+
    'Created Rays: '+created_rays.ToString +sLineBreak+
    'Useful Rays: '+Rays.count.ToString;

  form1.Text1.Repaint;
end;

procedure TForm1.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not (Button=TMouseButton.mbLeft) then exit;

  var mouse_over_tile:= Mouse_coords_to_tile_pos(X, Y);
  var tile:= tiles[mouse_over_tile.X,mouse_over_tile.Y];
  var tile_already_exists:= tile<>nil;

  painting_tiles:= not tile_already_exists;
end;

procedure TForm1.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  var mouse_over_tile:= Mouse_coords_to_tile_pos(X, Y);
  //Text1.Text:= 'Tile X:'+mouse_over_tile.X.ToString+', Y:'+mouse_over_tile.Y.ToString;

  if not (ssLeft in Shift) then exit;
  Work_with_tile_under_cursor(mouse_over_tile);
end;

procedure TForm1.Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not (Button=TMouseButton.mbLeft) then exit;

  var mouse_over_tile:= Mouse_coords_to_tile_pos(X, Y);
  Work_with_tile_under_cursor(mouse_over_tile);
end;

{ TTile }

procedure Draw_Tiles;
begin
  form1.Viewport3D1.Canvas.Clear(TAlphaColorRec.Teal);

  var blueBrush:= TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Blue);

  for var x := 0 to Grid_size-1 do
  for var y := 0 to Grid_size-1 do
    begin
      var tile:= tiles[X,Y];
      if tile=nil then continue;
      
      var rect:= TRectF.Create(tile.origin,tile_size,tile_size);
      form1.Viewport3D1.Canvas.FillRect(rect,1,blueBrush);
    end;
end;

procedure Update_all_tiles;
begin
  form1.Viewport3D1.Canvas.BeginScene;
  Draw_Tiles;
  Draw_Edges;
  Draw_Vertices;
  Draw_Polygons;
  Draw_Rays;
  form1.Viewport3D1.Canvas.EndScene;
end;

constructor TTile.Create(position: TPoint);
begin
  self.X:= position.X;
  self.Y:= position.Y;
  self.origin:= TPoint.Create(X*tile_size,Y*tile_size);
  tiles[position.X,position.Y]:= self;
  Update_all_tiles;
  inc(tile_count);
end;

procedure TTile.Remove(position: TPoint);
begin
  tiles[position.X,position.Y].Free;
  tiles[position.X,position.Y]:= nil;
  Update_all_tiles;
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

end.
