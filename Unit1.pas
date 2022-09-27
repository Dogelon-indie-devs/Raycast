unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Generics.Collections,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Viewport3D,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Objects;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Button1: TButton;
    Panel1: TPanel;
    Text1: TText;
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
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

  TEdge = class
    index: integer;
    starts, ends: TPoint;
    procedure Extend(direction: TDirection);
  end;

  TTile = class
    X, Y: integer;
    origin: TPoint;
    touches_edges: array[dNorth..dEast] of TEdge;
    function Get_neighbor(direction: TDirection): TTile;
    function Has_edge(direction: TDirection): TEdge;
    procedure Get_or_create_edges;
    procedure Ask_neighbor_for_edges(direction: TDirection; neighbor:TTile);
    procedure Make_an_edge(location: TDirection);
    constructor Create(position: TPoint);
  end;

const
  Grid_size = 20;

var
  Form1: TForm1;
  Tile_size: integer;
  tiles: array of array of TTile;
  edges: TObjectList<TEdge>;

implementation

{$R *.fmx}

// loosely following tutorial from https://www.youtube.com/watch?v=fc3nnG2CG8U

procedure TForm1.FormCreate(Sender: TObject);
begin
  setLength(tiles, 20, 20);
  tile_size:= round(Form1.Viewport3D1.Width / Grid_size); // 40x40;
  edges:= TObjectList<TEdge>.Create;
end;

procedure Draw_edges;
begin
  var redBrush:= TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);
  redBrush.Thickness:=3;

  for var edge in edges do
    with form1.Viewport3D1.Canvas do
      begin
        BeginScene;
        DrawLine(edge.starts,edge.ends,1,redBrush);
        EndScene;
      end;
end;

procedure Analyze_tiles_for_edges;
begin
  edges.Clear;

  for var x := 0 to Grid_size-1 do
  for var y := 0 to Grid_size-1 do
    begin
      var tile:= tiles[X,Y];
      if tile=nil then continue;
      tile.Get_or_create_edges;
    end;

  Draw_edges;
  form1.Text1.text:= 'Edge count: '+edges.Count.ToString;
end;

function Mouse_coords_to_tile_pos(mouseX, mouseY: Single): TPoint;
begin
  result.X := round(mouseX) div Tile_size;
  result.Y := round(mouseY) div Tile_size;
end;

procedure Create_tile_under_cursor(mouse_over_tile: TPoint);
begin
  if mouse_over_tile.X>Grid_size-1 then exit;
  if mouse_over_tile.Y>Grid_size-1 then exit;

  var tile_already_exists:= tiles[mouse_over_tile.X,mouse_over_tile.Y]<>nil;
  if tile_already_exists then
    exit
  else
    TTile.Create(mouse_over_tile);
end;

procedure TForm1.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  var mouse_over_tile:= Mouse_coords_to_tile_pos(X, Y);
  //Text1.Text:= 'Tile X:'+mouse_over_tile.X.ToString+', Y:'+mouse_over_tile.Y.ToString;

  if not (ssLeft in Shift) then exit;
  Create_tile_under_cursor(mouse_over_tile);
end;

procedure TForm1.Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not (Button=TMouseButton.mbLeft) then exit;

  var mouse_over_tile:= Mouse_coords_to_tile_pos(X, Y);
  Create_tile_under_cursor(mouse_over_tile);
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

constructor TTile.Create(position: TPoint);
begin
  self.X:= position.X;
  self.Y:= position.Y;
  self.origin:= TPoint.Create(X*tile_size,Y*tile_size);
  tiles[position.X,position.Y]:= self;

  form1.Viewport3D1.Canvas.BeginScene;
  Draw_Tiles;
  Analyze_tiles_for_edges;
  form1.Viewport3D1.Canvas.EndScene;
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

function TTile.Has_edge(direction: TDirection): TEdge;
begin
  result:= touches_edges[direction];
end;

procedure TTile.Make_an_edge(location: TDirection);
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

procedure TTile.Get_or_create_edges;
begin
  for var direction := TDirection.dNorth to TDirection.dEast do
    begin
      var neighbor:= Get_neighbor(direction);
      if neighbor<>nil then
        begin
          if (direction=dNorth) or (direction=dWest) then
            Ask_neighbor_for_edges(direction,neighbor);
        end
      else
        Make_an_edge(direction);
    end;
end;

procedure TTile.Ask_neighbor_for_edges(direction: TDirection; neighbor: TTile);
var edge: TEdge;
begin
  case direction of
    dNorth:
      begin
        edge:= neighbor.Has_edge(TDirection.dWest);
        if edge<>nil then
          begin
            self.touches_edges[TDirection.dWest]:= edge;
            edge.Extend(TDirection.dSouth);
          end
        else
          self.Make_an_edge(TDirection.dWest);

        edge:= neighbor.Has_edge(TDirection.dEast);
        if edge<>nil then
          begin
            self.touches_edges[TDirection.dEast]:= edge;
            edge.Extend(TDirection.dSouth);
          end
        else
          self.Make_an_edge(TDirection.dEast);
      end;

    dWest:
      begin
        edge:= neighbor.Has_edge(TDirection.dNorth);
        if edge<>nil then
          begin
            self.touches_edges[TDirection.dNorth]:= edge;
            edge.Extend(TDirection.dEast);
          end
        else
          self.Make_an_edge(TDirection.dNorth);

        edge:= neighbor.Has_edge(TDirection.dSouth);
        if edge<>nil then
          begin
            self.touches_edges[TDirection.dSouth]:= edge;
            edge.Extend(TDirection.dEast);
          end
        else
          self.Make_an_edge(TDirection.dSouth);
      end;
  end;
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
