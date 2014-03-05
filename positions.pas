unit positions;

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
  utils,
  basiccell;

type
  { TPositions }

  TPositions = class
  private
    FAgg2D: TAgg2D;
    FImage: TImage;
    FVPRect: TBasicCellRect;
    procedure WriteText(ADisplacement: double);
  public
    constructor Create(ACanvas: TAgg2D; AImage: TImage; ACellRect: TBasicCellRect);
    destructor destroy; override;
    procedure Draw;
  end;


implementation

type
  TLetterPos = record
    left: double;
    top: double;
  end;

{ TPositions }

procedure TPositions.WriteText(ADisplacement: double);
var
  lOldFontHeight: double;
  lValue: string;
  lTextWidth: double;
  lTextHeight: double;

  lLeft: TLetterPos;
  lTop: TLetterPos;
  lRight: TLetterPos;
  lBottom: TLetterPos;

  lStr: string;
  lPositions: string;

const
  cInterLine = 3;

begin
  lPositions :=
    FImage.Orientation[2][1] +
    FImage.Orientation[1][2] +
    FImage.Orientation[2][3] +
    FImage.Orientation[3][2];

  lOldFontHeight:= FAgg2D.FontHeight;

  //FAgg2D.Font(utils.GetFont, FAgg2D.FontHeight * 1.5);

  FAgg2D.TextAlignment(AGG_AlignLeft, AGG_AlignBottom);

  // posición de letra izquierda
  lTextWidth := FAgg2D.TextWidth(lPositions[1]);
  lTextHeight := FAgg2D.TextWidth(lPositions[1]);
  lLeft.left := FVPRect.Left + 5;
  lLeft.top := FVPRect.Top + (lTextHeight / 2) + ((FVPRect.Bottom - FVPRect.Top) / 2);
  FAgg2D.Text(lLeft.left, lLeft.top, lPositions[1], true, 0.0, 0.0);

  // posición de letra superior
  lTextWidth := FAgg2D.TextWidth(lPositions[2]);
  lTextHeight := FAgg2D.TextWidth(lPositions[2]);
  lTop.left := FVPRect.Left + ((FVPRect.Right - FVPRect.Left) / 2) + (lTextWidth / 2);
  lTop.top := FVPRect.Top + lTextHeight + 5;
  FAgg2D.Text(lTop.left, lTop.top, lPositions[2], true, 0.0, 0.0);

  // posición de letra derecha
  lTextWidth := FAgg2D.TextWidth(lPositions[3]);
  lTextHeight := FAgg2D.TextWidth(lPositions[3]);
  lRight.left := FVPRect.Right - (lTextWidth + 5);
  lRight.top := FVPRect.Top + (lTextHeight / 2) + ((FVPRect.Bottom - FVPRect.Top) / 2);
  FAgg2D.Text(lRight.left, lRight.top, lPositions[3], true, 0.0, 0.0);

  // posición de letra inferior
  lTextWidth := FAgg2D.TextWidth(lPositions[4]);
  lTextHeight := FAgg2D.TextWidth(lPositions[4]);
  lBottom.left := FVPRect.Left + ((FVPRect.Right - FVPRect.Left) / 2) + (lTextWidth / 2);
  lBottom.top := FVPRect.Bottom - 5;
  FAgg2D.Text(lBottom.left, lBottom.top, lPositions[4], true, 0.0, 0.0);

  //FAgg2D.Font(utils.GetFont, lOldFontHeight);
end;

constructor TPositions.Create(ACanvas: TAgg2D; AImage: TImage; ACellRect: TBasicCellRect);
begin
  FImage := AImage;
  FAgg2D := ACanvas;
  FVPRect := ACellRect;
end;

destructor TPositions.destroy;
begin
  inherited destroy;
end;

procedure TPositions.Draw;
begin
  FAgg2D.NoLine;
  FAgg2D.FillColor($00, $00, $00);
  WriteText(1);
  FAgg2D.FillColor($ff, $ff, $ff);
  WriteText(0);
end;

end.

