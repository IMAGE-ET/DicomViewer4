unit ruler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Math,
  Agg2d,
  images,
  vectors,
  basiccell;

type

  { TRuler }

  TRuler = class(TVector)
  private
    FSelected: Boolean;
    FLastPoint: TPoint;
    FFirstPoint: TPoint;
  public
    constructor Create(AOwner: TObject; AFirstPoint: TPoint; AXOffset, AYOFFset: Integer; AZoom: double); override;
    procedure Drag(AX, AY: Integer); override;
    procedure Draw(Agg2D: TAgg2D; AZoom: double; AXOffset, AYOffset: Integer; AVPRect: TBasicCellRect); override;
    function Selected(APoint: TPoint): boolean; override;
    property FirstPoint: TPoint read FFirstPoint write FFirstPoint;
    property LastPoint: TPoint read FLastPoint write FLastPoint;
  end;

implementation

{ TRuler }

constructor TRuler.Create(AOwner: TObject; AFirstPoint: TPoint; AXOffset, AYOFFset: Integer; AZoom: double);
begin
  inherited Create(AOwner, AFirstPoint, AXOffset, AYOFFset, AZoom);
  (* Al momento de la creación ambos puntos (FirstPoint y LastPoint) son iguales *)
  FX := AFirstPoint.X;
  FY := AFirstPoint.Y;
  FFirstPoint := ScreenToImage(AFirstPoint.X, AFirstPoint.Y);
  FLastPoint := FFirstPoint;
end;

procedure TRuler.Drag(AX, AY: Integer);
var
  lOriginalPoint: TPoint;
begin
  lOriginalPoint := ScreenToImage(FX, FY);
  if ComparePoints(lOriginalPoint, FLastPoint) then
    FLastPoint := ScreenToImage(AX, AY)
  else
  if ComparePoints(lOriginalPoint, FFirstPoint) then
    FFirstPoint := ScreenToImage(AX, AY);

  FX := AX;
  FY := AY;
end;

procedure TRuler.Draw(Agg2D: TAgg2D; AZoom: double; AXOffset, AYOffset: Integer; AVPRect: TBasicCellRect);
var
  lTextPoint: TPoint;
  lAngle: double;
  lDistance: double;
  lDistanceStr: string;
  lFirst: TPoint;
  lLast: TPoint;

begin
  inherited;
  lFirst := ImageToScreen(FirstPoint);
  lLast := ImageToScreen(LastPoint);

  (* Dibujamos linea *)
  DrawLine(lFirst, lLast, FColor);
  (* Dibujamos manejador 1 *)
  DrawHandler(lFirst, FColor);
  (* Dibujamos manejador 2 *)
  DrawHandler(lLast, FColor);

  lDistance :=
    hypot((LastPoint.X - FirstPoint.X) *  (Owner as TImage).ColSpacing,
    (LastPoint.Y - FirstPoint.Y) * (Owner as TImage).RowSpacing);
  if (FOwner as TImage).Modality = 'XA' then
    lDistanceStr := Format('%f* mm', [lDistance])
  else
    lDistanceStr := Format('%f mm', [lDistance]);

  (* Ahora se dibuja el texto *)
  lAngle := arctan2(LastPoint.Y-FirstPoint.Y, LastPoint.X-FirstPoint.X) * 180 / PI;

  if(lAngle <= 0) and (lAngle > -90) then
  begin
    lTextPoint.X := lFirst.X + 10;
    lTextPoint.Y := round(lFirst.Y + FAgg2d.FontHeight);
  end
  else
  if(lAngle < -90) and (lAngle >= -180) then
  begin
    lTextPoint.X := round(lFirst.X - FAgg2d.TextWidth(lDistanceStr) - 10);
    lTextPoint.Y := round(lFirst.Y + FAgg2d.FontHeight);
  end
  else
  if(lAngle > 90) and (lAngle <= 180) then
  begin
    lTextPoint.X := round(lLast.X + 10);
    lTextPoint.Y := round(lLast.Y + FAgg2d.FontHeight);
  end
  else
  if(lAngle > 0) and (lAngle < 90) then
  begin
    lTextPoint.X := round(lLast.X - FAgg2d.TextWidth(lDistanceStr) - 10);
    lTextPoint.Y := round(lLast.Y + FAgg2d.FontHeight);
  end;

  DrawText(lTextPoint, lDistanceStr, FColor, False);
end;

function TRuler.Selected(APoint: TPoint): boolean;
var
  lPoint: TPoint;
begin
  Result := False;
  FX := APoint.X;
  FY := APoint.Y;
  lPoint := ScreenToImage(FX, FY);
  if ComparePoints(lPoint, FFirstPoint, 10) then
  begin
    FFirstPoint := lPoint;
    Result := True;
  end
  else
  if ComparePoints(lPoint, FLastPoint, 10) then
  begin
    FLastPoint := lPoint;
    Result := True;
  end;
end;

end.

