unit vectors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fgl, Agg2D, agg_color, utils,
  basiccell;

type

  { TVector }

  TVector = class
  protected
    FOwner: TObject;
    FAgg2d: TAgg2d;
    FZoom: double;
    FXOffset: Integer;
    FYOffset: Integer;
    FX: Integer;
    FY: Integer;
    FColor: TAggColor;
    FVPRect: TBasicCellRect;
  public
    constructor Create(AOwner: TObject; AFirstPoint: TPoint; AXOffset, AYOFFset: Integer; AZoom: double); virtual;
    function Selected(AX, AY: Integer): Boolean; virtual; abstract;
    function ScreenToImage(X, Y: Integer): TPoint;
    function ImageToScreen(ImagePoint: TPoint): TPoint;
    function ComparePoints(APoint1, APoint2: TPoint; Athreshold: Integer = 1): Boolean;
    function Selected(APoint: TPoint): boolean; virtual; abstract;
    procedure DrawHandler(APoint: TPoint; AColor: TAggColor);
    procedure DrawLine(AFirst, ALast: TPoint; AColor: TAggColor);
    procedure DrawText(APoint: TPoint; AText: string; AColor: TAggColor; AVCenter: boolean = false);
    procedure Drag(X, Y: Integer); virtual; abstract;
    procedure Draw(Agg2D: TAgg2D; AZoom: double; AXOffset, AYOffset: Integer; AVPRect: TBasicCellRect); virtual;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Owner: TObject read FOwner;
    property Color: TAggColor read FColor write FColor;
  end;

  TVectorClass = class of TVector;

  TVectorsSpecialization = specialize TFPGList<TVector>;

  { TVectors }

  TVectors = class(TVectorsSpecialization)
  public
    destructor Destroy; override;
  end;

implementation
  
uses
  images;

{ TVectors }

destructor TVectors.Destroy;
var
  lVector: TVector;
begin
  for lVector in Self do
  begin
    lVector.free;
  end;
  inherited Destroy;
end;

{ TVector }

constructor TVector.Create(AOwner: TObject; AFirstPoint: TPoint; AXOffset, AYOFFset: Integer; AZoom: double);
begin
  // AOwner must be TImage
  assert(AOwner is TImage, 'AOwner must be TImage');
  FOwner := AOwner;
  FZoom := AZoom;
  FXOffset := AXOffset;
  FYOffset := AYOFFset;
  FColor.Construct($ff, $ff, $00);
end;

function TVector.ScreenToImage(X, Y: Integer): TPoint;
begin
  Result.X := Round(X / FZoom - (FXOffset / FZoom));
  Result.Y := Round(Y/ FZoom - (FYOffset / FZoom));
end;

function TVector.ImageToScreen(ImagePoint: TPoint): TPoint;
begin
  Result.X := FVPRect.Left + Round((ImagePoint.X + (FXOffset / FZoom) ) * FZoom);
  Result.Y := FVPRect.Top + Round((ImagePoint.Y + (FYOffset / FZoom) ) * FZoom);
end;

procedure TVector.Draw(Agg2D: TAgg2D; AZoom: double;
    AXOffset, AYOffset: Integer; AVPRect: TBasicCellRect);
begin
  FAgg2d := Agg2D;
  FZoom := AZoom;
  FXOffset := AXOffset;
  FYOffset := AYOffset;
  FVPRect := AVPRect;
end;

function TVector.ComparePoints(APoint1, APoint2: TPoint; Athreshold: Integer = 1): Boolean;
var
  lThreshold: double;
begin
  lThreshold:= Athreshold / FZoom;
  Result := (APoint1.X >= APoint2.X - lthreshold) and  (APoint1.X <= APoint2.X + lthreshold) and
    (APoint1.Y >= APoint2.Y - lthreshold) and  (APoint1.Y <= APoint2.Y + lthreshold);
end;

procedure TVector.DrawHandler(APoint: TPoint; AColor: TAggColor);
begin
  FAgg2d.NoLine;
  FAgg2d.FillColor(AColor);
  FAgg2d.Rectangle(APoint.X - 2, APoint.Y - 2, APoint.X + 2, APoint.Y + 2, true);
end;

procedure TVector.DrawLine(AFirst, ALast: TPoint; AColor: TAggColor);
begin
  FAgg2d.NoFill;
  FAgg2d.LineWidth(1);
  FAgg2d.LineColor($00, $00, $00);
  FAgg2d.Line(AFirst.X + 1, AFirst.Y + 1, ALast.X + 1, ALast.Y + 1, true);
  FAgg2d.LineColor(AColor);
  FAgg2d.Line(AFirst.X, AFirst.Y, ALast.X, ALast.Y, true);
end;

procedure TVector.DrawText(APoint: TPoint; AText: string; AColor: TAggColor; AVCenter: boolean);
var
  lHeight: double;
  lStr: TStringList;
  lText: string;
  I: Integer;
  lX: Integer;
  lY: Integer;
begin
  lX := APoint.X;
  lY := APoint.Y;
  lHeight := FAgg2d.FontHeight;
  if AVCenter then
  begin
    lY := lY - Round(lHeight / 2);
  end;
  lStr := TStringList.create;
  try
    lStr.Text := AText;
    for I := 0 to lStr.Count - 1 do
    begin
      lText := lStr[I];
      lHeight := FAgg2d.FontHeight;
      FAgg2d.NoLine;
      FAgg2d.FillColor($00, $00, $00);
      FAgg2d.Text(lX + 1, (lY + 1) + (I * lHeight), lText, False, 0.0, 0.0);
      FAgg2d.FillColor(AColor);
      FAgg2d.Text(lX, lY + (I * lHeight), lText, False, 0.0, 0.0);
    end;
  finally
    lStr.Free;
  end;
end;

end.

