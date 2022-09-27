unit Math;

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils;

function Find_intersection_point: TPointF;

implementation


type TBasicLine = record
  p1,p2:TPointF;
end;

const FLT_MAX = 99999;

function Line_Line_Intersection(A,B,C,D: TPointF) :TPointF;
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
    raise Exception.Create('The given lines are parallel');
end;

function Find_intersection_point: TPointF;    // (line1,line2: TLine)
begin
  var A := TPoint.Create(1, 1);
  var B := TPoint.Create(4, 4);
  var C := TPoint.Create(1, 8);
  var D := TPoint.Create(2, 4);

  var intersection := Line_Line_Intersection(A, B, C, D);

  // NOTE: Further check can be applied in case of line segments. Here, we have considered AB and CD as lines
  raise Exception.Create('The intersection of the given lines AB and CD is: '+
    intersection.X.ToString+','+intersection.Y.ToString);
end;


end.
