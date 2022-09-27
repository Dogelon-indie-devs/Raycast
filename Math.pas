unit Math;

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils;

type ELinesParallel = class(Exception);
type ELineSegmentDontIntersect = class(Exception);

type TBasicLine = class
  starts, ends: TPointF;
end;

function Find_intersection_point(line1,line2: TBasicLine): TPointF;

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

function Find_intersection_point(line1,line2: TBasicLine): TPointF;
begin
  var intersection :=
    Line_Line_Intersection(line1.starts,line1.ends,line2.starts,line2.ends);

  if  (intersection.X < line1.starts.X) OR
      (intersection.X > line1.ends.X) OR
      (intersection.X < line2.starts.X) OR
      (intersection.X > line2.ends.X) OR

      (intersection.Y < line1.starts.Y) OR
      (intersection.Y > line1.ends.Y) OR
      (intersection.Y < line2.starts.Y) OR
      (intersection.Y > line2.ends.Y) then
  raise ELineSegmentDontIntersect.Create('Intersect outside of the lines');

  result:= intersection;

  // NOTE: Further check can be applied in case of line segments. Here, we have considered AB and CD as lines
  //Log('The intersection of the given lines AB and CD is: '+intersection.X.ToString+','+intersection.Y.ToString);
end;


end.
