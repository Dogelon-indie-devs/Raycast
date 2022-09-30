unit IntersectMath;

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  System.Math;

type ELinesParallel = class(Exception);
type ELineSegmentDontIntersect = class(Exception);

type TBasicLine = class
  starts, ends: TPointF;
end;

function Find_intersection_point(line_edge,line_ray: TBasicLine): TPointF;
function Point_in_bounding_rectangles(point: TPointF; line1,line2: TBasicLine): boolean;

implementation

function Line_Line_Intersection(A,B,C,D: TPointF): TPointF;
// https://www.geeksforgeeks.org/program-for-point-of-intersection-of-two-lines
begin
  // Line AB represented as a1x + b1y = c1
  var a1 := B.Y - A.Y;
  var b1 := A.X - B.X;
  var c1 := a1*(A.X) + b1*(A.Y);

  // Line CD represented as a2x + b2y = c2
  var a2 := D.Y - C.Y;
  var b2 := C.X - D.X;
  var c2 := a2*(C.X)+ b2*(C.Y);

  var determinant := a1*b2 - a2*b1;

  if (determinant <> 0) then
    begin
      var x := (b2*c1 - b1*c2) / determinant;
      var y := (a1*c2 - a2*c1) / determinant;
      result:= TPointF.Create(x, y);
    end
  else
    raise ELinesParallel.Create('The given lines are parallel');
end;

function Bounding_rect_from_line(line: TBasicLine): TRectF;
var top,left,right,bottom: single;
begin
  top:=   IfThen(line.starts.Y < line.ends.Y,line.starts.Y,line.ends.Y);
  left:=  IfThen(line.starts.X < line.ends.X,line.starts.X,line.ends.X);
  right:= IfThen(line.starts.X > line.ends.X,line.starts.X,line.ends.X);
  bottom:=IfThen(line.starts.Y > line.ends.Y,line.starts.Y,line.ends.Y);

  var topleft:=     TPointF.Create(left,top);
  var bottomright:= TPointF.Create(right,bottom);

  result:= TRectF.Create(topleft,bottomright);
end;

function Combine_bounding_rects(rect1,rect2: TRectF): TRectF;
var top,left,right,bottom: single;
begin
  top:=   IfThen(rect1.Top    < rect2.Top,    rect1.Top,    rect2.Top);
  left:=  IfThen(rect1.left   < rect2.left,   rect1.left,   rect2.left);
  right:= IfThen(rect1.right  > rect2.right,  rect1.right,  rect2.right);
  bottom:=IfThen(rect1.bottom > rect2.bottom, rect1.bottom, rect2.bottom);

  var topleft:=     TPointF.Create(left,top);
  var bottomright:= TPointF.Create(right,bottom);

  result:= TRectF.Create(topleft,bottomright);
end;

function Point_in_bounding_rectangle(point: TPointF; line: TBasicLine): boolean; overload;
begin
  var bounding_rect:= Bounding_rect_from_line(line);

  result:=(point.Y >= bounding_rect.Top)    AND
          (point.X >= bounding_rect.Left)   AND
          (point.X <= bounding_rect.Right)  AND
          (point.Y <= bounding_rect.Bottom);
end;

function Point_in_bounding_rectangle(point: TPointF; bounding_rect: TRectF): boolean; overload;
begin
  result:=(point.Y >= bounding_rect.Top)    AND
          (point.X >= bounding_rect.Left)   AND
          (point.X <= bounding_rect.Right)  AND
          (point.Y <= bounding_rect.Bottom);
end;

function Point_in_bounding_rectangles(point: TPointF; line1,line2: TBasicLine): boolean;
begin
  var rect1:= Bounding_rect_from_line(line1);
  if not Point_in_bounding_rectangle(point,rect1) then exit(false);
  var rect2:= Bounding_rect_from_line(line2);
  if not Point_in_bounding_rectangle(point,rect2) then exit(false);

  var bounding_rect:= Combine_bounding_rects(rect1,rect2);

  result:=(point.Y >= bounding_rect.Top)   AND
          (point.X >= bounding_rect.Left)  AND
          (point.X <= bounding_rect.Right) AND
          (point.Y <= bounding_rect.Bottom);
end;

function Find_intersection_point(line_edge,line_ray: TBasicLine): TPointF;
begin
  var intersection := Line_Line_Intersection(
    line_edge.starts,
    line_edge.ends,
    line_ray.starts,
    line_ray.ends
    );

  if not Point_in_bounding_rectangle(intersection,line_edge) then
    raise ELineSegmentDontIntersect.Create('Intersect outside of the lines');

  result:= intersection;
end;


end.
