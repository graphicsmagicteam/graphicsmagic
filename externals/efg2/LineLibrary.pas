// efg, August 1998
// Break this out as a separate library since it's needed elsewhere.

UNIT LineLibrary;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

INTERFACE
  USES
    Windows,   // TPoint, ClipCursor
    ExtCtrls;  // TImage

  TYPE
    TLineSelected = (lsNotSelected, lsPoint1, lsPoint2, lsLine);

    // TLineOrientation is used as part of the heuristic algorithm that decides
    // if a line is selected and if lines intersect}
    TLineOrientation  = (loPoint, loHorizontal, loVertical);

    TVector = record
      StartPoint: TPoint;
      EndPoint: TPoint;
    end;

    TFigure = (TRectangle, TTriangle, TCircle);
    TPolygon = record
      X, Y: Double;                               // replace Real with Double
      case Tipo: TFigure of
        TRectangle: (Height, Width: Double);      // replace Real with Double
        TTriangle: (Side1, Side2, Angle: Double); // replace Real with Double
        TCircle: (Radius: Double);                // replace Real with Double
    end;

    TFigura = (toLine, toPolygon);

    TObjeto = record
      Numero   : integer;
      case Tipo: TFigura of
         toLine: (linha : TVector);
         toPolygon: (polygon : TPolygon);
    end;    


  FUNCTION AddPoints(CONST PointA, PointB:  TPoint):  TPoint;
  FUNCTION SubtractPoints(CONST PointA, PointB:  TPoint):  TPoint;

  PROCEDURE CalcLineParameters (CONST PointA, PointB  :  TPoint;
                                 VAR   Slope, Intercept:  DOUBLE;
                                 VAR   LineOrientation :  TLineOrientation);
  FUNCTION NearLine(CONST Target, Point1, Point2:  TPoint):  BOOLEAN;


  PROCEDURE RestrictCursorToDrawingArea (CONST Image: TImage);
  PROCEDURE RemoveCursorRestrictions;


  FUNCTION SquareContainsPoint (CONST SquareCenter   :  TPoint;
                                CONST SquareHalfSize :  INTEGER; {pixels}
                                CONST TestPoint      :  TPoint):  BOOLEAN;

IMPLEMENTATION

  USES
    Math,     // MinIntValue, MaxIntValue
    Classes;  // Bounds

  FUNCTION AddPoints(CONST PointA, PointB:  TPoint):  TPoint;
  BEGIN
    WITH RESULT DO
    BEGIN
      X := PointA.X + PointB.X;
      Y := PointA.Y + PointB.Y
    END
  END {AddPoints};


  FUNCTION SubtractPoints(CONST PointA, PointB:  TPoint):  TPoint;
  BEGIN
    WITH RESULT DO
    BEGIN
      X := PointA.X - PointB.X;
      Y := PointA.Y - PointB.Y
    END
  END {SubtractPoints};


  // Determine whether a line is ltHorizonal or ltVertical,  along with the
  // appropriate slope and intercept FOR point-slope line  equations.  These
  // parameters are used to determine if a line is selected.
  PROCEDURE CalcLineParameters (CONST PointA, PointB  :  TPoint;
                                VAR   Slope, Intercept:  DOUBLE;
                                VAR   LineOrientation :  TLineOrientation);
    VAR
      Delta:  TPoint;
  BEGIN
    Delta := SubtractPoints(PointB, PointA);

    IF  (Delta.X = 0) AND (Delta.Y = 0)
    THEN BEGIN
      // This special CASE should never happen if iMinPixels > 0
      LineOrientation := loPoint;
      Slope     := 0.0;
      Intercept := 0.0
    END
    ELSE BEGIN

      IF   ABS(Delta.X) >= ABS(Delta.Y)
      THEN BEGIN
        // The line is more horizontal than vertical.  Determine values FOR
        // equation:  Y = slope*X + intercept
        LineOrientation := loHorizontal;
        TRY
          Slope := Delta.Y / Delta.X   {conventional slope in geometry}
        EXCEPT
          Slope := 0.0
        END;
        Intercept := PointA.Y - PointA.X*Slope
      END
      ELSE BEGIN
        // The line is more vertical than horizontal.  Determine values FOR
        // equation:  X = slope*Y + intercept
        LineOrientation := loVertical;
        TRY
          Slope := Delta.X / Delta.Y  {reciprocal of conventional slope}
        EXCEPT
          Slope := 0.0
        END;
        Intercept := PointA.X - PointA.Y*Slope;
      END

    END
  END {CalcLineParameters};


  // Determine if Target1 is "near" line segment between Point1 and Point2
  FUNCTION NearLine(CONST Target, Point1, Point2:  TPoint):  BOOLEAN;
    CONST
      LineSelectFuzz =  4;  // Pixel "fuzz" used in line selection
    VAR
      Intercept      :  DOUBLE;
      LineOrientation:  TLineOrientation;
      maxX           :  INTEGER;
      maxY           :  INTEGER;
      minX           :  INTEGER;
      minY           :  INTEGER;
      Slope          :  DOUBLE;
      xCalculated    :  INTEGER;
      yCalculated    :  INTEGER;
  BEGIN
    RESULT := FALSE;

    // If an Endpoint is not selected, was part of line selected?
    CalcLineParameters (Point1, Point2, Slope, Intercept, LineOrientation);

    CASE LineOrientation OF
      loHorizontal:
        BEGIN
          minX := MinIntValue([Point1.X, Point2.X]);
          maxX := MaxIntValue([Point1.X, Point2.X]);
          // first check if selection within horizontal range of line
          IF (Target.X >= minX) and (Target.X <= maxX)
          THEN BEGIN
            // Since X is within range of line, now see if Y value is close
            // enough to the calculated Y value FOR the line to be selected.
             yCalculated := ROUND( Slope*Target.X + Intercept );
             IF   ABS(yCalculated - Target.Y) <= LineSelectFuzz
             THEN RESULT := TRUE
          END
        END;

      loVertical:
        BEGIN
          minY := MinIntValue([Point1.Y, Point2.Y]);
          maxY := MaxIntValue([Point1.Y, Point2.Y]);
          // first check if selection within vertical range of line
          IF   (Target.Y >= minY) AND (Target.Y <= maxY)
          THEN BEGIN
            // Since Y is within range of line, now see if X value is close
            // enough to the calculated X value FOR the line to be selected.
            xCalculated := ROUND( Slope*Target.Y + Intercept );
            IF   ABS(xCalculated - Target.X) <= LineSelectFuzz
            THEN RESULT := TRUE
          END
        END;

      loPoint:
        // Do nothing -- should not occur
    END
  END {NearLine};


  ///////////////////////////////////////////////////////////////////////

  PROCEDURE RestrictCursorToDrawingArea (CONST Image:  TImage);
    VAR
      CursorClipArea:  TRect;
  BEGIN
    CursorClipArea := Bounds(Image.ClientOrigin.X,
                             Image.ClientOrigin.Y,
                             Image.Width, Image.Height);
    Windows.ClipCursor(@CursorClipArea)
  END {RestrictCursorToDrawingArea};


  PROCEDURE RemoveCursorRestrictions;
  BEGIN
    Windows.ClipCursor(NIL)
  END {RemoveCursorRestrictions};


  ///////////////////////////////////////////////////////////////////////

  // Could use Windows PtInRect API call instead (but it excludes the
  // bottom and right edges of the rectangle while including the left
  // and top edges.
  FUNCTION SquareContainsPoint (CONST SquareCenter   :  TPoint;
                                CONST SquareHalfSize :  INTEGER; {pixels}
                                CONST TestPoint      :  TPoint):  BOOLEAN;
  BEGIN
     RESULT := (TestPoint.X >= SquareCenter.X - SquareHalfSize) AND
               (TestPoint.X <= SquareCenter.X + SquareHalfSize) AND
               (TestPoint.Y >= SquareCenter.Y - SquareHalfSize) AND
               (TestPoint.Y <= SquareCenter.Y + SquareHalfSize)
  END {SquareContainsPoint};


END.
