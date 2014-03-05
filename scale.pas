unit scale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_main,
  fpg_base,
  Agg2D,
  images,
  basiccell;

type

  { TScale }

  TScale = class
  private
    FImage: TImage;
    FCanvas: TAgg2D;
    FVPRect: TBasicCellRect;
    FCanvasWidth: Integer;
  public
    constructor Create(ACanvas: TAgg2D; AImage: TImage; AVPRect: TBasicCellRect; ACanvasWidth: Integer);
    destructor destroy; override;
    procedure DrawVScale(AZoom: double);
  end;

implementation

{ TScale }

constructor TScale.Create(ACanvas: TAgg2D; AImage: TImage;
   AVPRect: TBasicCellRect; ACanvasWidth: Integer);
begin
  FImage := AImage;
  FCanvas := ACanvas;
  FVPRect := AVPRect;
  FCanvasWidth := ACanvasWidth;
end;

destructor TScale.destroy;
begin
  inherited destroy;
end;

procedure TScale.DrawVScale(AZoom: double);
var
  lText: string;
  lFontHeight: Integer;
  lTextWidth: Integer;
  lX: Integer;
  lY: Integer;
  lCentimeters: double;
  lLineWidth: Integer;
  I: Integer;
  A: Integer;
  lWidth: Integer;
  lAlign: TAlign;
  lCanvasHeight: Double;
  lCanvasWidth: Double;
  lScaleWidth: double;

begin
  (* Escala Vertical *)
  if FImage.RowSpacing = 0 then
    exit;

  lScaleWidth := FCanvasWidth * 0.01;
  lCanvasWidth := FVPRect.Right - FVPRect.Left;
  lCanvasHeight := FVPRect.Bottom - FVPRect.Top;
  lLineWidth:= Round(lCanvasHeight / 3);
  lCentimeters := Round((lLineWidth * FImage.RowSpacing / AZoom) / 10);

  lAlign := alRight;
  if (FImage.Modality = 'MG') and (FImage.Laterality = 'R') then
    lAlign := alLeft;

  lY := Round((lCanvasHeight / 2) - (lLineWidth / 2));

  if lAlign = alRight then
  begin
    if (FImage.Modality = 'CT') or (FImage.Modality = 'MR') then
      lX := Round(lCanvasWidth - 15)
    else
      lX := Round(lCanvasWidth - 15)
  end
  else
    lX := Round(5);

  (* Nombre *)
  lText := Format('%.0fcm', [lCentimeters]);
  lFontHeight := Round(FCanvas.FontHeight);
  lTextWidth := Round(FCanvas.TextWidth(lText));
  FCanvas.LineWidth(0.1);

  for I := 1 downto 0 do
  begin
    FCanvas.NoLine;
    if I = 1 then
    begin
      FCanvas.FillColor($00, $00, $00);
    end
    else
    begin
      FCanvas.FillColor(fpgColor2AggColor(clYellow));
    end;

    if I = 1 then
    begin
      lX := lX + I;
      lY := lY + I;
    end
    else
    begin
      lX := lX - 1;
      lY := lY - 1;
    end;

    (* Texto *)
    if lAlign = alLeft then
      FCanvas.Text(FVPRect.Left + lX, FVPRect.Top + (lY + lLineWidth + lFontHeight + 2), lText, true, 0.0, 0.0)
    else
      FCanvas.Text(FVPRect.Left + (lX - lTextWidth), FVPRect.Top + (lY + lLineWidth + lFontHeight + 2), lText, true, 0.0, 0.0);

    FCanvas.NoFill;
    if I = 1 then
    begin
      FCanvas.LineColor($00, $00, $00);
    end
    else
    begin
      FCanvas.LineColor($ff, $ff, $00);
    end;

    (* Linea Vertical *)
    FCanvas.LineWidth(1);
    FCanvas.Line(FVPRect.Left + lX, FVPRect.Top + lY, FVPRect.Left + lX, FVPRect.Top + (lY + lLineWidth - 1), true);

    for A := 0 to 15 do
    begin
      if lAlign = alLeft then
      begin
        if (A mod 5) = 0 then
          lWidth := Round(lX + lScaleWidth)
        else
          lWidth := Round(lX + (lScaleWidth / 2));
      end
      else
      begin
        if (A mod 5) = 0 then
          lWidth := Round(lX - lScaleWidth)
        else
          lWidth := Round(lX - (lScaleWidth / 2));
      end;

      FCanvas.Line(
        FVPRect.Left + lX,
        FVPRect.Top + Round(lY + (A * ((lLineWidth - 1) / 15))),
        FVPRect.Left + (lWidth + 0.5),
        FVPRect.Top + Round(lY + (A * ((lLineWidth - 1) / 15))), true);
    end;
  end;
end;


end.


