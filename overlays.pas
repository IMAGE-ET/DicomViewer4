unit overlays;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_main,
  fpg_base,
  fpg_imgfmt_png,
  Agg2D,
  images,
  series,
  basiccell,
  textoverlays;

type

  { TOverlays }

  TOverlayPosition = (opTopLeft, opTopRight, opBottomLeft, opBottomRight);

  TOverlays = class
  private
    FAgg2D: TAgg2D;
    FImage: TImage;
    FVPRect: TBasicCellRect;
    FXOffset: double;
    FYOffset: double;
    FZoom: double;
    function GetOverlayValue(AValue: string): string;
    procedure WriteText(ADisplacement: double; APosition: TOverlayPosition);
  public
    constructor Create(ACanvas: TAgg2D; AImage: TImage; ACellRect: TBasicCellRect; AZoom: double);
    destructor destroy; override;
    procedure DrawOverlays(APosition: TOverlayPosition);
    property XOffset: double read FXOffset write FXOffset;
    property YOffset: double read FYOffset write FYOffset;
  end;


implementation

{ TOverlays }

function TOverlays.GetOverlayValue(AValue: string): string;
begin
  if AValue = 'ANGLE' then
    Result := Format('%dº', [FImage.DicomUtils.GetAngle])
  else
  if AValue = 'ZOOM' then
    Result := Format('%f %%', [FZoom * 100])
  else
  if AValue = 'WW' then
    Result := Format('%f', [FImage.DicomUtils.WindowWidth])
  else
  if AValue = 'WC' then
    Result := Format('%f', [FImage.DicomUtils.WindowCenter])
  else
  if AValue = 'IMAGEOFSERIES' then
    Result := Format('%d de %d', [(FImage.Serie as TSerie).Images.IndexOf(FImage) + 1, (FImage.Serie as TSerie).Images.Count])
  else
  if AValue = 'LEFT' then
    Result := Format('%f', [FXOffset])
  else
  if AValue = 'TOP' then
    Result := Format('%f', [FYOffset])
  else
  if AValue = 'FRAMES' then
    Result := Format('%d/%d', [FImage.DicomUtils.CurrentFrame + 1, FImage.DicomUtils.FrameCount])
  else
    Result := AValue;
end;

procedure TOverlays.WriteText(ADisplacement: double; APosition: TOverlayPosition);
var
  lTextOverlays: TTextOverlays;
  lTextOverlay: TTextOverlay;
  lValue: string;
  lTop: double;
  lLeft: double;
  lStr: string;

const
  cInterLine = 3;

begin
  FAgg2D.TextAlignment(AGG_AlignLeft, AGG_AlignBottom);
  case APosition of
    opTopLeft: begin
      lTextOverlays := FImage.TopLeftOverlays;
      lLeft := 5 + ADisplacement;
      lTop := 5 + FAgg2D.FontHeight + ADisplacement;
    end;
    opBottomLeft: begin
      lTextOverlays := FImage.BottomLeftOverlays;
      lLeft := 5 + ADisplacement;
      lTop := ((FVPRect.Bottom - FVPRect.Top) - (5 + ((FAgg2D.FontHeight + cInterLine) * (lTextOverlays.Count - 1)))) + ADisplacement;
    end;
    opTopRight: begin
      lTextOverlays := FImage.TopRightOverlays;
      lLeft := ((FVPRect.Right - FVPRect.Left) - 5) + ADisplacement;
      lTop := 5 + FAgg2D.FontHeight + ADisplacement;
    end;
    opBottomRight: begin
      lTextOverlays := FImage.BottomRightOverlays;
      lLeft := ((FVPRect.Right - FVPRect.Left) - 5) + ADisplacement;
      lTop := ((FVPRect.Bottom - FVPRect.Top) - (5 + ((FAgg2D.FontHeight + cInterLine) * (lTextOverlays.Count - 1)))) + ADisplacement;
    end;
  end;

  for lTextOverlay in lTextOverlays do
  begin
    if lTextOverlay.TextLabel <> '' then
      lStr := Format('%s %s', [lTextOverlay.TextLabel, GetOverlayValue(lTextOverlay.Value)])
    else
      lStr := Format('%s', [GetOverlayValue(lTextOverlay.Value)]);

    case APosition of
      opTopRight: FAgg2D.Text(FVPRect.Left + (lLeft - FAgg2D.TextWidth(lStr)), FVPRect.Top + lTop, lStr, true, 0.0, 0.0);
      opBottomRight: FAgg2D.Text(FVPRect.Left + (lLeft - FAgg2D.TextWidth(lStr)), FVPRect.Top + lTop, lStr, true, 0.0, 0.0);
    else
      FAgg2D.Text(FVPRect.Left + lLeft, FVPRect.Top + lTop, lStr, true, 0.0, 0.0);
    end;

    lTop := lTop + Round(FAgg2D.FontHeight) + cInterLine;
  end;
end;

constructor TOverlays.Create(ACanvas: TAgg2D; AImage: TImage; ACellRect: TBasicCellRect; AZoom: double);
begin
  FImage := AImage;
  FAgg2D := ACanvas;
  FVPRect := ACellRect;
  FZoom := AZoom;
end;

destructor TOverlays.destroy;
begin
  inherited destroy;
end;

procedure TOverlays.DrawOverlays(APosition: TOverlayPosition);
begin
  FAgg2D.NoLine;
  FAgg2D.FillColor($00, $00, $00);
  WriteText(1, APosition);
  FAgg2D.FillColor($ff, $ff, $ff);
  WriteText(0, APosition);
end;

end.

