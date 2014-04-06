unit imagecell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  libDicomUtilsWrapperDyn,
  math,
  fgl,
  fpg_main,
  fpg_widget,
  fpg_base,
  FPimage,
  FPCanvas,
  ViewerCanvas,
  Agg2D,
  scale,
  overlays,
  positions,
  images,
  series,
  vectors,
  basiccell;

type
  { TImageCell }


  TImageCell = class;

  TOnNeedImage = procedure(AImageCell: TImageCell) of object;

  TImageCell = class(TBasicCell)
  private
    FChanged: Boolean;
    FImage: TImage;
    FMagnifying: boolean;
    FMagnifyPoint: TPoint;
    FOnNeedImage: TOnNeedImage;
    FRightScale: Boolean;
    FSerieCell: TBasicCell;
    FWindowCenter: double;
    FWindowWidth: double;
    FWLX: Integer;
    FWLY: Integer;
    FX: Integer;
    FXOffset: Integer;
    FY: Integer;
    FYOffset: Integer;
    FZoom: double;
    FZoomX: Integer;
    FZoomY: Integer;
    FEmpty: boolean;
    FVPRect: TBasicCellRect;
    FImgRect: TBasicCellRect;
    FCanvasImage: TfpgImage;
    FAgg2D: TAgg2D;
    FIsSimpleLayout: boolean;
    FCurrentVector: TVector;
    FViewerCanvas: TViewerCanvas;
    FCanvasWidth: Integer;
    FCanvasHeight: Integer;
    FAngle: Integer;
    FVFlip: Boolean;
    FHFlip: Boolean;
    FInverted: Boolean;
    FResampledBuf: Pointer;
    FResampledBufSize: TLongOrdinal;
    FResampledWidth: Integer;
    FResampledHeight: Integer;
    FLut: TLut;
    function GetImgRect: TBasicCellRect;
    function GetOverlappingRect(ARect1, ARect2: TBasicCellRect): TBasicCellRect;
    procedure SetRightScale(AValue: Boolean);
    function Touch(ARect1, ARect2: TBasicCellRect): Boolean;
    function ScreenToImage(X, Y: Integer): TPoint;
    function ImageToScreen(ImagePoint: TPoint): TPoint;
    procedure SetZoom(AFactor: double);
    procedure SetImage(AValue: TImage);
    procedure DrawVectors;
    procedure DrawScale;
    procedure DrawOverlays;
    procedure DrawScoutLine;
    procedure DrawNoImage;
    procedure DrawLayoutType;
    procedure DrawMagnifier;
    procedure DrawPositions;
    procedure DrawImagePoint(ASamples: Integer; ABuf: PByte; AScanLine: PLongWord);
    procedure DrawImagePointLut(ASamples: Integer; var ABuf: Pointer;
        var AScanLine: PLongWord; ALut: TLut);
  public
    constructor Create(ASerieCell: TBasicCell);
    destructor Destroy; override;
    function GetViewPortRect: TBasicCellRect;
    function VectorAtPoint(AMousePos: TPoint): boolean;
    procedure MakeWindowLut;
    procedure Resample;
    procedure Rotate90CW;
    procedure Rotate90CCW;
    procedure HFlip;
    procedure VFlip;
    procedure Invert;
    procedure AlignByModality;
    procedure DeleteCurrentVector;
    procedure CreateVectorAtPoint(AVectorClass: TVectorClass; AMousePos: TPoint);
    procedure Draw(AViewerCanvas: TViewerCanvas; IsCurrentCell, IsSameSerie: Boolean);
    procedure DrawImageOld;
    procedure DrawImage;
    procedure Align(Valign: TValign; HAlign: THalign);
    procedure ZoomToFit;
    property DrawingRect: TBasicCellRect read GetViewPortRect;
    property Image: TImage read FImage write SetImage;
    property XOffset: Integer read FXOffset write FXOffset;
    property YOffset: Integer read FYOffset write FYOffset;
    property WLX: Integer read FWLX write FWLX;
    property WLY: Integer read FWLY write FWLY;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property ZoomX: Integer read FZoomX write FZoomX;
    property ZoomY: Integer read FZoomY write FZoomY;
    property Zoom: double read FZoom write SetZoom;
    property WindowCenter: double read FWindowCenter write FWindowCenter;
    property WindowWidth: double read FWindowWidth write FWindowWidth;
    property OnNeedImage: TOnNeedImage read FOnNeedImage write FOnNeedImage;
    property Changed: Boolean read FChanged write FChanged;
    property VPRect: TBasicCellRect read FVPRect;
    property Empty: boolean read FEmpty write FEmpty;
    property CurrentVector: TVector read FCurrentVector write FCurrentVector;
    property Magnifying: boolean read FMagnifying write FMagnifying;
    property MagnifyPoint: TPoint read FMagnifyPoint write FMagnifyPoint;
    property RightScale: Boolean read FRightScale write SetRightScale;
    property CanvasImage: TfpgImage read FCanvasImage;
  end;

  TImageCellsSpecialization = specialize TFPGList<TImageCell>;

  { TImageCells }

  TImageCells = class(TImageCellsSpecialization)
  public
    destructor Destroy; override;
  end;


implementation

uses
  SerieCell;

{ TImageCells }

destructor TImageCells.Destroy;
var
  lImageCell: TImageCell;
begin
  for lImageCell in Self do
  begin
    lImageCell.free;
  end;
  inherited Destroy;
end;


{ TImageCell }

function TImageCell.GetViewPortRect: TBasicCellRect;
var
  lFontSize: double;
  lHeight: Integer;
  lSerieCell: TSerieCell;
  lA4Height: Integer;
begin
  lSerieCell := (FSerieCell as TSerieCell);
  Result.Left   := Round((FSerieCell.Left * lSerieCell.PixelWidth) + (Left * FSerieCell.Width * lSerieCell.PixelWidth));
  Result.Top    := Round((FSerieCell.Top * lSerieCell.PixelHeight) + (Top * FSerieCell.Height * lSerieCell.PixelHeight));
  Result.Right  := Round(Result.Left + (Width * FSerieCell.Width * lSerieCell.PixelWidth));
  Result.Bottom := Round(Result.Top + (Height * FSerieCell.Height * lSerieCell.PixelHeight));

  lHeight := Result.Bottom - Result.Top;
  // para un alto de tamaño A4, la fuente es
  // igual a mFontSize
  //
  // Alto A4 = 279mm
  // Convertido a pulgadas = 279 * 1/25.4 = 10.98
  // 300 dpi

  lA4Height := Round(FViewerCanvas.DPI * 10.98);

  // por regla de 3 simple
  // lA4Height ---> mFontSize
  // lHeight   ----> X

  //lFontSize := lHeight * 12 / lA4Height;

  //FAgg2D.Font(utils.GetFont, lFontSize);
end;

function TImageCell.VectorAtPoint(AMousePos: TPoint): boolean;
var
  lVector: TVector;
begin
  Result := False;
  if FImage = nil then
    exit;

  for lVector in FImage.Vectors do
  begin
    if lVector.Selected(AMousePos) then
    begin
      FCurrentVector := lVector;
      Result := True;
      Break;
    end;
  end;
end;

procedure TImageCell.MakeWindowLut;
begin
  FLut := FImage.MakeWindowLut(FWindowCenter, FWindowWidth);
end;

procedure TImageCell.Resample;
var
  lRect: TRect; // area en imagen original
begin
  // se determina el área en la imágen original
  lRect.Left := Round(FXOffset * -1 / (FZoom));
  lRect.Top := Round(FYOffset * -1/ (FZoom));
  lRect.Right:= Round(lRect.Left + (FImage.DicomUtils.ImgWidth / FZoom));
  lRect.Bottom:= Round(lRect.Top + (FImage.DicomUtils.ImgHeight / FZoom));

  // extrae una porción de la imágen original
  // y le aplica zoom.
  FImage.Resample(
    FZoom,
    FResampledWidth,
    FResampledHeight,
    FResampledBuf,
    FResampledBufSize);
end;

procedure TImageCell.Rotate90CW;
begin
  FAngle := FAngle + 90;
  if FAngle = 360 then
    FAngle := 0;
  FChanged := True;
end;

procedure TImageCell.Rotate90CCW;
begin
  FAngle := FAngle - 90;
  if FAngle = -360 then
    FAngle := 0;
  FChanged := True;
end;

procedure TImageCell.HFlip;
begin
  FHFlip:= not FHFlip;
end;

procedure TImageCell.VFlip;
begin
  FVFlip := not FVFlip;
end;

procedure TImageCell.Invert;
begin
  FInverted:= not FInverted;
end;

procedure TImageCell.AlignByModality;
begin
  (* Si es una MG, se alinea según el tag 0020,0062,
     en los demás casos siempre van centradas. *)
  if FImage.Modality <> 'MG' then
    Self.Align(vaCenter, haCenter)
  else
    if FImage.DicomUtils.GetTagValue('0020,0062') = 'R' then
      Self.Align(vaTop, haRight)
    else
      Self.Align(vaTop, haLeft);
end;

procedure TImageCell.DeleteCurrentVector;
begin
  if FCurrentVector <> nil then
  begin
    FImage.Vectors.Remove(FCurrentVector);
    FCurrentVector.Free;
    FCurrentVector := nil;
    Changed := True;
  end;
end;

procedure TImageCell.CreateVectorAtPoint(AVectorClass: TVectorClass;
  AMousePos: TPoint);
begin
  FCurrentVector := AVectorClass.Create(FImage, AMousePos, FXOffset, FYOffset, FZoom);
  FImage.Vectors.Add(FCurrentVector);
end;

function TImageCell.GetImgRect: TBasicCellRect;
begin
  Result.Left := FVPrect.Left + FXOffset;
  Result.Top := FVPrect.Top + FYOffset;
  Result.Right := Result.Left + Trunc(FResampledWidth);
  Result.Bottom := Result.Top + Trunc(FResampledHeight);
end;

function TImageCell.ScreenToImage(X, Y: Integer): TPoint;
begin
  Result.X := Round(X / FZoom - (FXOffset / FZoom));
  Result.Y := Round(Y/ FZoom - (FYOffset / FZoom));
end;

function TImageCell.ImageToScreen(ImagePoint: TPoint): TPoint;
begin
  Result.X := Round((ImagePoint.X + (FXOffset / FZoom) ) * FZoom);
  Result.Y := Round((ImagePoint.Y + (FYOffset / FZoom) ) * FZoom);
end;


procedure TImageCell.SetZoom(AFactor: double);
var
  lOriginalPoint: TPoint;
  lFinalPoint: TPoint;
  lZoomX: Integer;
  lZoomY: Integer;
  lHZoom: double;
  lVZoom: double;

begin
  // el mayor zoom posible es 3
  if (FZoom * AFactor) > 3 then
    exit;

  // el menor zoom es el que permite ver toda la
  // imágen en pantalla con hasta una reducción del 50%
  lHZoom := (FImage.DicomUtils.ImgWidth) * (FZoom * AFactor);
  if lHZoom < (FVPRect.Right - FVPRect.Left) * 0.50 then
    lHZoom := (FVPRect.Right - FVPRect.Left) * 0.50;

  lVZoom := (FImage.DicomUtils.ImgHeight) * (FZoom * AFactor);
  if lVZoom < (FVPRect.Bottom - FVPRect.Top) * 0.50 then
    lVZoom := (FVPRect.Bottom - FVPRect.Top) * 0.50;

  lZoomX := FZoomX - FVPRect.Left;
  lZoomY := FZoomY - FVPRect.Top;

  // se calcula la posición del mouse en la imágen
  lOriginalPoint := ScreenToImage(lZoomX, lZoomY);

  // ejecutamos el zoom
  FZoom := FZoom * AFactor;
  Resample;

  // una vez aplicado el zoom, se calcula la
  // nueva posición del punto en la pantalla
  lFinalPoint := ImageToScreen(lOriginalPoint);

  // obtenemos el offset final
  // TODO: este cálculo es incorrecto si hacemos zoomIn / zoomOut
  // el punto de ancla se desplaza, y no debería hacerlo.
  FXOffset := Round(FXOffset + (lZoomX - lFinalPoint.X) );
  FYOffset := Round(FYOffset + (lZoomY - lFinalPoint.Y) );
end;

procedure TImageCell.Align(Valign: TValign; HAlign: THalign);
var
  vpWidth: Integer;
  vpHeight: Integer;
  lTop: Integer;
  lLeft: Integer;
  imgWidth: double;
  imgHeight: double;
  lVPRect: TBasicCellRect;
  lIMGRect: TBasicCellRect;
begin
  lVPRect := GetViewPortRect;
  lIMGRect := GetImgRect;
  vpWidth := lVPRect.Right - lVPRect.Left;
  vpHeight:= lVPRect.Bottom - lVPRect.Top;

  if FImage.Modality = 'MG' then
  begin
    lTop := -Round(FImage.MGY1 * FZoom);
    lLeft := Round(FImage.MGX1 * FZoom);
    imgWidth := (((FImage.MGX2) * FZoom) - lIMGRect.Left);
    imgHeight := (FImage.MGY2 - FImage.MGY1) * FZoom;
  end
  else
  begin
    lTop := 0;
    lLeft := 0;
    imgWidth:= lIMGRect.Right - lIMGRect.Left;
    imgHeight:= lIMGRect.Bottom - lIMGRect.Top;
  end;

  case Valign of
    vaTop: FYOffset := lTop;
    vaCenter: FYOffset := Round((vpHeight / 2) - (imgHeight / 2));
    vaBottom: FYOffset := Round(vpHeight - imgHeight);
  end;

  case HAlign of
    haLeft: FXOffset := lLeft;
    haCenter: FXOffset := Round((vpWidth / 2) - (imgWidth / 2));
    haRight: FXOffset := Round(vpWidth - imgWidth);
  end;
end;

procedure TImageCell.DrawScoutLine;
var
  lSerie: TSerie;
  lPt1: TPoint;
  lPt2: TPoint;

begin
  lSerie := FImage.Serie as TSerie;
  FAgg2D.NoFill;
  FAgg2D.MasterAlpha(0.3);
  FAgg2D.LineColor($ff, $FF, $FF);

  lPt1 := ImageToScreen(Point(lSerie.ScoutColPixel[0], lSerie.ScoutRowPixel[0]));
  lPt2 := ImageToScreen(Point(lSerie.ScoutColPixel[1], lSerie.ScoutRowPixel[1]));
  if lPt1.Y < 0 then
    lPt1.Y := 0;
  if lPt2.Y < 0 then
    lPt2.Y := 0;
  FAgg2D.Line(FVPRect.Left + lPt1.x, FVPRect.Top + lPt1.y, FVPRect.Left + lPt2.x , FVPRect.Top + lPt2.y, true);

  lPt1 := ImageToScreen(Point(lSerie.ScoutColPixel[1], lSerie.ScoutRowPixel[1]));
  lPt2 := ImageToScreen(Point(lSerie.ScoutColPixel[2], lSerie.ScoutRowPixel[2]));
  if lPt1.Y < 0 then
    lPt1.Y := 0;
  if lPt2.Y < 0 then
    lPt2.Y := 0;  FAgg2D.Line(FVPRect.Left + lPt1.x, FVPRect.Top + lPt1.y, FVPRect.Left + lPt2.x , FVPRect.Top + lPt2.y, true);

  lPt1 := ImageToScreen(Point(lSerie.ScoutColPixel[2], lSerie.ScoutRowPixel[2]));
  lPt2 := ImageToScreen(Point(lSerie.ScoutColPixel[3], lSerie.ScoutRowPixel[3]));
  if lPt1.Y < 0 then
    lPt1.Y := 0;
  if lPt2.Y < 0 then
    lPt2.Y := 0;  FAgg2D.Line(FVPRect.Left + lPt1.x, FVPRect.Top + lPt1.y, FVPRect.Left + lPt2.x , FVPRect.Top + lPt2.y, true);

  lPt1 := ImageToScreen(Point(lSerie.ScoutColPixel[3], lSerie.ScoutRowPixel[3]));
  lPt2 := ImageToScreen(Point(lSerie.ScoutColPixel[0], lSerie.ScoutRowPixel[0]));
  if lPt1.Y < 0 then
    lPt1.Y := 0;
  if lPt2.Y < 0 then
    lPt2.Y := 0;
  FAgg2D.Line(FVPRect.Left + lPt1.x, FVPRect.Top + lPt1.y, FVPRect.Left + lPt2.x , FVPRect.Top + lPt2.y, true);
  FAgg2D.MasterAlpha(1);
end;

procedure TImageCell.DrawNoImage;
var
  lLeft: Integer;
  lTop: Integer;
  lWidth: Integer;
  lHeight: Integer;
  lTextHeight: double;

const
  cText1 = 'No images for this cell.';
  cText2 = 'Please drag an image from the Series Panel.';

begin
  lWidth := (FVPRect.Right - FVPRect.Left);
  lHeight := (FVPRect.Bottom - FVPRect.Top);
  FAgg2D.NoLine;
  FAgg2D.FillColor($AA, $AA, $AA);
  lTextHeight := FAgg2D.FontHeight + 2;

  (* Linea 1 *)
  lLeft := FVPRect.Left + Round((lWidth / 2) - (FAgg2D.TextWidth(cText1) / 2));
  lTop := FVPRect.Top + Round((lHeight / 2) - ((lTextHeight * 2) / 2));
  FAgg2D.Text(lLeft, lTop, cText1, false, 0.0, 0.0);

  (* Linea 2 *)
  lLeft := FVPRect.Left + Round((lWidth / 2) - (FAgg2D.TextWidth(cText2) / 2));
  lTop := FVPRect.Top + Round((lHeight / 2) - ((lTextHeight * 2) / 2));
  FAgg2D.Text(lLeft, lTop + lTextHeight, cText2, false, 0.0, 0.0);
end;

procedure TImageCell.DrawLayoutType;
var
  lText: string;
  lTop: double;
  lLeft: double;
  lWidth: double;
  lHeight: double;
  lTextWidth: double;
begin
  if FIsSimpleLayout then
  begin
    lText := 'Full screen';
    FAgg2D.NoLine;
    FAgg2D.FillColor($00, $55, $00);
    FAgg2D.TextAlignment(AGG_AlignLeft, AGG_AlignCenter);
    lWidth := 200;
    lHeight := FAgg2D.FontHeight + 4;
    lLeft := -98;
    lTop := FVPRect.Top + 30;

    FAgg2D.Rotate(Deg2Rad(-45));
    FAgg2D.Rectangle(
      FVPRect.Left + lLeft,
      FVPRect.Top + lTop,
      FVPRect.Left + (lLeft + lWidth),
      FVPRect.Top + (lTop + lHeight)
      );
    FAgg2D.FillColor($ff, $ff, $ff);
    FAgg2D.Text(
      FVPRect.Left + (lLeft + ((lWidth / 2) - (FAgg2D.TextWidth(lText) / 2))),
      FVPRect.Top + (lTop + ((lHeight / 2))),
      lText, true, 0.0, 0.0);
    FAgg2D.ResetTransformations;
  end;
end;

procedure TImageCell.DrawMagnifier;
var
  lBuf: PByte;
  lTmpBuf: PByte;
  lBufSize: TLongOrdinal;
  lImageBase: TfpgImage;
  lWidth: double;
  lHeight: double;
  lPixel: PLongWord;
  lRed: Byte;
  lGreen: Byte;
  lBlue: Byte;
  lSamples: Byte;
  x1: double;
  y1: double;
  x2: double;
  y2: double;
  llX: Integer;
  llY: Integer;
  lScanLine: PLongWord;
  lOldZoom: double;
  lMagnifyImage: TfpgImage;
  lPoint: TPoint;
  lMagWidth: Integer;


begin
  // Version modificada de ScreenToImage
  lPoint.X := Round(FMagnifyPoint.X / FZoom - (FXOffset / FZoom));
  lPoint.Y := Round(FMagnifyPoint.Y/ FZoom - (FYOffset / FZoom));

  lMagWidth:= (FVPRect.Right - FVPRect.Left) div 5;
  x1 := lPoint.X - (lMagWidth / FZoom / 2);
  x2 := lPoint.X + (lMagWidth / FZoom / 2);
  y1 := lPoint.Y - (lMagWidth / FZoom / 2);
  y2 := lPoint.Y + (lMagWidth / FZoom / 2);
  lWidth := x2 - x1;
  lHeight := y2 - y1;

  (* x1,y1,lWidth,lHeight conforman el rectánculo
     a mostrar de la imágen a escala 1:1 *)
  lBuf := nil;
  lBufSize:= 0;
  lOldZoom:= FZoom;
  FImage.DicomUtils.LoadDicomImageToBufferEx(lBuf, lBufSize, x1, y1, lWidth, lHeight, FZoom * 1.5);
  // se obtiene el area superpuesta,
  // es decir, el rectángulo contenedor de la imágen
  lMagnifyImage := TfpgImage.Create;

  try
    (* lWidth y lHeight es el ancho y alto de la imágen
       resultante, o sea, escalada *)
    lTmpBuf := lBuf;
    lSamples := FImage.DicomUtils.SamplesPerPixel;
    lMagnifyImage.AllocateImage(32, trunc(lWidth), trunc(lHeight));

    for llY := 0 to trunc(lHeight) - 1 do
    begin
      lScanLine := lMagnifyImage.ScanLine[llY];

      //inc(lScanLine);

      for llX := 0 to Trunc(lWidth) - 1 do
      begin
        if lSamples = 1 then
        begin
          lRed := lTmpBuf^;
          lGreen := lTmpBuf^;
          lBlue := lTmpBuf^;
        end
        else
        begin
          // es Blue-Green-Red (BGR)
          lRed := lTmpBuf[2];
          lGreen := lTmpBuf[1];
          lBlue := lTmpBuf[0];
        end;
        lScanLine^ :=  lRed or (lGreen shl 8) or (lBlue shl 16) or (alphaOpaque shl 24);
        inc(lTmpBuf, lSamples);
        inc(lScanLine);
      end;
    end;
    FAgg2D.ResetPath;
    FAgg2D.NoFill;
    FAgg2D.LineWidth(4);
    FAgg2D.LineColor($ff, $ff, $ff);
    FAgg2D.Ellipse(
        FMagnifyPoint.X,
        FMagnifyPoint.Y,
        lMagnifyImage.Width / 2,
        lMagnifyImage.Height / 2);
    FAgg2D.ClosePolygon;
    FAgg2D.TransformImagePath(lMagnifyImage,
      FMagnifyPoint.X - (lMagnifyImage.Width / 2),
      FMagnifyPoint.Y - (lMagnifyImage.Height / 2),
      FMagnifyPoint.X + (lMagnifyImage.Width / 2),
      FMagnifyPoint.Y + (lMagnifyImage.Height / 2));
  finally
    FImage.DicomUtils.DeleteBuffer(lBuf);
    lMagnifyImage.Free;
  end;
end;

procedure TImageCell.DrawPositions;
var
  lPatientPosition: string;
begin
  if (FImage.Modality = 'CT') or (FImage.Modality = 'MR') then
  begin
    (* Patient position *)
    lPatientPosition := FImage.DicomUtils.GetTagValue('0018,5100');
    with TPositions.Create(FAgg2D, FImage, FVPRect) do
    begin
      Draw;
      Free;
    end;
    //SetBigFontSize;
    //RotatePositions(Image.Angle);
    //SetFontSize;
  end;
end;

procedure TImageCell.DrawImagePoint(ASamples: Integer; ABuf: PByte; AScanLine: PLongWord);
var
  lRed: Byte;
  lGreen: Byte;
  lBlue: Byte;

begin
  if abs(ASamples) = 1 then
  begin
    lRed := ABuf^;
    lGreen := ABuf^;
    lBlue := ABuf^;
  end
  else
  begin
    // es Blue-Green-Red (BGR)
    lRed := ABuf[2];
    lGreen := ABuf[1];
    lBlue := ABuf[0];
  end;

  AScanLine^ := lRed or (lGreen shl 8) or (lBlue shl 16) or (alphaOpaque shl 24);
end;

procedure TImageCell.DrawImagePointLut(ASamples: Integer; var ABuf: Pointer;
  var AScanLine: PLongWord; ALut: TLut);
var
  lRed: Byte;
  lGreen: Byte;
  lBlue: Byte;

begin
  if abs(ASamples) = 1 then
  begin
    lRed := ALut[Word(ABuf^)];
    lGreen := lRed; // los tres canales son iguales
    lBlue := lRed;
  end
  else
  begin
    // es Blue-Green-Red (BGR)
    // https://www.dabsoft.ch/dicom/3/C.7.6.3.1.3/
    if FImage.PlanarConfiguration = 0 then
    begin
      lRed := PByte(ABuf)[2];
      lGreen := PByte(ABuf)[1];
      lBlue := PByte(ABuf)[0];
    end;
  end;

  AScanLine^ := lRed or (lGreen shl 8) or (lBlue shl 16) or (alphaOpaque shl 24);
end;

procedure TImageCell.ZoomToFit;
var
  lImgRect: TBasicCellRect;
  vpWidth: Integer;
  vpHeight: Integer;
  imgWidth: double;
  imgHeight: double;
  vZoom: double;
  hZoom: double;

begin
  FVPRect := GetViewPortRect;
  vpWidth := FVPRect.Right - FVPRect.Left;
  vpHeight:= FVPRect.Bottom - FVPRect.Top;

  if FImage.Modality = 'MG' then
  begin
    imgWidth := (FImage.MGX2 - FImage.MGX1);
    imgHeight := (FImage.MGY2 - FImage.MGY1);
    if (imgWidth = 0) or (imgHeight = 0) then
    begin
      exit;
    end;
  end
  else
  begin
    imgWidth := FImage.DicomUtils.ImgWidth;
    imgHeight := FImage.DicomUtils.ImgHeight;
  end;

  vZoom := vpWidth / imgWidth;
  hZoom := vpHeight / imgHeight;

  // se usa el zoom menor entre vZoom y hZoom
  FZoom := 1;
  SetZoom(Min(vZoom, hZoom));
end;

constructor TImageCell.Create(ASerieCell: TBasicCell);
var
  lSerieCell: TSerieCell;
  lNeedImage: TOnNeedImage;

begin
  inherited Create;
  FAngle := 0;
  FVFlip := false;
  FHFlip := false;
  FInverted := false;
  FChanged := True;
  FSerieCell := ASerieCell;
  FZoom := 1;
  FWindowCenter := -1;
  FWindowWidth := -1;
  FImage := nil;
  FCurrentVector := nil;
  FMagnifying:= False;
  lSerieCell := FSerieCell as TSerieCell;
  lNeedImage:= lSerieCell.OnNeedImage;
  FOnNeedImage := lNeedImage;
  FEmpty := True;
  FIsSimpleLayout:= ASerieCell.Owner.ClassName = 'TSingleImageLayout';
  SetLength(FLut, 0);
end;

destructor TImageCell.Destroy;
begin
  FreeMem(FResampledBuf);
  inherited Destroy;
end;

procedure TImageCell.Draw(AViewerCanvas: TViewerCanvas; IsCurrentCell, IsSameSerie: Boolean);
var
  lx: Integer;
  ly: Integer;
  lw: Integer;
  lh: Integer;
begin
  if FChanged then
  begin
    FCanvasWidth:= AViewerCanvas.CanvasImage.Width;
    FCanvasHeight:= AViewerCanvas.CanvasImage.Height;

    if Assigned(FOnNeedImage) then
      FOnNeedImage(Self);

    FViewerCanvas := AViewerCanvas;
    FAgg2D := AViewerCanvas.AggCanvas2D;
    FCanvasImage := AViewerCanvas.CanvasImage;

    FVPRect := GetViewPortRect;
    AViewerCanvas.AggCanvas2D.LineColor($00, $00, $00);
    AViewerCanvas.AggCanvas2D.FillColor($00, $00, $00);
    AViewerCanvas.AggCanvas2D.Rectangle(
      FVPRect.Left,
      FVPRect.Top,
      FVPRect.Right - 1,
      FVPRect.Bottom - 1);
    AViewerCanvas.AggCanvas2D.MasterAlpha(1);

    // Se dibuja todo sobre FCanvasImage, y al final
    // se copia el contenido de esa imágen en
    //  ACanvas
    if FImage <> nil then
    begin

      if FEmpty then
      begin
        Self.ZoomToFit;
        Self.AlignByModality;
        FEmpty := False;
      end;

      // si se cambió de imágen, el vector seleccionado pasa
      // a ser nil.
      if FImage.Vectors.IndexOf(FCurrentVector) = -1 then
        FCurrentVector := nil;

      DrawImage;
      DrawVectors;

      if FMagnifying then
        DrawMagnifier;

      if (not IsCurrentCell) and (not IsSameSerie) then
        (* El scout se dibuja sólo en las celdas que no están seleccionadas *)
        DrawScoutLine;

      DrawScale;
      DrawOverlays;
      DrawPositions;
      DrawLayoutType;
      FAgg2d.NoFill;
      FAgg2d.LineWidth(0.5);
      FAgg2D.LineColor($AA, $ff, $00);
      lX := ImageToScreen(Point(FImage.MGX1, FImage.MGY1)).X;
      lY := ImageToScreen(Point(FImage.MGX1, FImage.MGY1)).Y;
      lW := ImageToScreen(Point(FImage.MGX2, FImage.MGY2)).X;
      lH := ImageToScreen(Point(FImage.MGX2, FImage.MGY2)).Y;
      //FAgg2D.Rectangle(lX, lY, lW, lH);
      FChanged := False;
    end
    else
    begin
      DrawNoImage;
    end;
  end;

  AViewerCanvas.AggCanvas2D.NoFill;
  AViewerCanvas.AggCanvas2D.LineWidth(1);

  if IsCurrentCell then
    AViewerCanvas.AggCanvas2D.LineColor($FF, $FF, $FF)
  else
    AViewerCanvas.AggCanvas2D.LineColor($11, $11, $11);

  AViewerCanvas.AggCanvas2D.Rectangle(
    FVPRect.Left + 0.5,
    FVPRect.Top + 0.5,
    FVPRect.Right - 1,
    FVPRect.Bottom - 1,
    True);
end;

function TImageCell.GetOverlappingRect(ARect1, ARect2: TBasicCellRect): TBasicCellRect;
begin
  Result.Left   := Max(ARect1.Left, ARect2.Left);
  Result.Top    := Max(ARect1.Top, ARect2.Top);
  Result.Right  := Min(ARect1.Right, ARect2.Right);
  Result.Bottom := Min(ARect1.Bottom, ARect2.Bottom);
end;

procedure TImageCell.SetRightScale(AValue: Boolean);
begin
  if FRightScale=AValue then Exit;
  FRightScale:=AValue;
end;

procedure TImageCell.SetImage(AValue: TImage);
begin
  FImage:=AValue;

  if (FImage = nil) then
    exit;

  if not FImage.DicomUtils.Loaded then
  begin
    FImage.DicomUtils.Release;
    FImage.OpenFile(FImage.FileName);
  end;

  if (FWindowCenter = -1) and (FWindowWidth = -1) then
  begin
    FWindowCenter := FImage.DicomUtils.WindowCenter;
    FWindowWidth := FImage.DicomUtils.WindowWidth;
    MakeWindowLut;
  end;
end;

procedure TImageCell.DrawVectors;
var
  lVector: TVector;
begin
  if FImage.Vectors.Count > 0 then
  begin
    for lVector in FImage.Vectors do
    begin
      lVector.Color.Construct($ff, $ff, $ff);
      if lVector = FCurrentVector then
        lVector.Color.Construct($ff, $ff, $00);
      lVector.Draw(FAgg2D, FZoom, FXOffset, FYOffset, FVPRect);
    end;
  end;
end;

procedure TImageCell.DrawScale;
var
  lScale: TScale;
begin
  lScale := TScale.Create(FAgg2D, FImage, FVPRect, FCanvasWidth);
  try
    if FRightScale then
      lScale.DrawVScale(FZoom);
  finally
    lScale.Free;
  end;
end;

procedure TImageCell.DrawOverlays;
var
  lOverlays: TOverlays;
begin
  lOverlays := TOverlays.Create(FAgg2D, FImage, FVPRect, FZoom);
  try
    // se setean valores que no se obtienen desde DicomTags.
    lOverlays.XOffset := FXOffset;
    lOverlays.YOffset := FYOffset;
    // se "dibuja" el texto
    lOverlays.DrawOverlays(opTopLeft);
    lOverlays.DrawOverlays(opBottomLeft);
    lOverlays.DrawOverlays(opTopRight);
    lOverlays.DrawOverlays(opBottomRight);
  finally
    lOverlays.Free;
  end;
end;

function TImageCell.Touch(ARect1, ARect2: TBasicCellRect): Boolean;
begin
  Result := (ARect1.Left <= ARect2.Right) and
    (ARect2.Left <= ARect1.Right) and
    (ARect1.Top <= ARect2.Bottom) and
    (ARect2.Top <= ARect1.Bottom);
end;

procedure TImageCell.DrawImageOld;
var
  lBuf: PByte;
  lTmpBuf: PByte;
  lBufSize: TLongOrdinal;
  lWidth: double;
  lHeight: double;
  lSamples: Integer;
  x1: double;
  y1: double;
  x2: double;
  y2: double;
  llX: Integer;
  llY: Integer;
  lRect: TBasicCellRect;
  lScanLine: PLongWord;
  lOverlappingRect: TBasicCellRect;
  lAngle: Integer;
  lOrientation: Integer;

begin
  // Si la imágen está dentro del viewport entonces se mostrará parte de ella
  lAngle := FImage.DicomUtils.Angle;
  if lAngle <> FAngle then
  begin
    FImage.DicomUtils.Angle := -lAngle;
    FImage.DicomUtils.Angle := FAngle;
  end;

  if FInverted then
    FImage.DicomUtils.Polarity := 1
  else
    FImage.DicomUtils.Polarity := 0;

  FImgRect := GetImgRect;
  lRect.Left:= 0;
  lRect.Top := 0;
  lRect.Right:= FVPRect.Right;
  lRect.Bottom:= FVPRect.Bottom;
  if Touch(lRect, FImgRect) then
  begin
    // se obtiene el area superpuesta,
    // es decir, el rectángulo contenedor de la imágen
    lOverlappingRect := GetOverlappingRect(lRect, FImgRect);

    lOrientation := 1;
    if FHFlip then
      lOrientation := 1;

    if FImage.DicomUtils.ImgWidth * FImage.DicomUtils.ImgHeight >= 800*800 then
    begin
      // Para las imágenes grandes se usa el método de zoom que
      // mantiene copia de la imageen con el zoom aplicado.
      // Esto se hace para mejorar la performance.
      x1 := (lOverlappingRect.Left - FXOffset * lOrientation);
      x2 := (lOverlappingRect.Right - FXOffset * lOrientation);
      y1 := (lOverlappingRect.Top - FYOffset);
      y2 := (lOverlappingRect.Bottom - FYOffset);

      lWidth := x2 - x1;
      lHeight := y2 - y1;

      (* x1,y1,lWidth,lHeight conforman el rectánculo
         a mostrar de la imágen a escala 1:1 *)
      lBuf := nil;
      lBufSize:= 0;
      FImage.DicomUtils.LoadDicomImageToBufferEx(lBuf, lBufSize, x1, y1, lWidth, lHeight);
    end
    else
    begin
      // Para las imágenes chicas no es necesario
      // almacenar en memoria una copia de la imágen
      // original, por lo que se trabaja diréctamente sobre ella.
      x1 := (lOverlappingRect.Left - FXOffset * lOrientation) / FZoom;
      x2 := (lOverlappingRect.Right - FXOffset * lOrientation) / FZoom;
      y1 := (lOverlappingRect.Top - FYOffset) / FZoom;
      y2 := (lOverlappingRect.Bottom - FYOffset) / FZoom;

      lWidth := x2 - x1;
      lHeight := y2 - y1;

      (* x1,y1,lWidth,lHeight conforman el rectánculo
         a mostrar de la imágen a escala 1:1 *)
      lBuf := nil;
      lBufSize:= 0;
      FImage.DicomUtils.LoadDicomImageToBufferEx(lBuf, lBufSize, x1, y1, lWidth, lHeight, FZoom);
    end;

    try
      (* lWidth y lHeight es el ancho y alto de la imágen
         resultante, o sea, escalada *)
      lTmpBuf := lBuf;
      lSamples := FImage.DicomUtils.SamplesPerPixel;
      for llY := FVPRect.Top + lOverlappingRect.Top to FVPRect.Top + (lOverlappingRect.Top + trunc(lHeight) - 1) do
      begin
        lScanLine := FCanvasImage.ScanLine[llY];

        if not FHFlip then
        begin
          inc(lScanLine, FVPRect.Left + lOverlappingRect.Left);
          for llX := lOverlappingRect.Left to lOverlappingRect.Left + Trunc(lWidth) - 1 do
          begin
            DrawImagePoint(lSamples, lTmpBuf, lScanLine);
            inc(lTmpBuf, lSamples);
            inc(lScanLine);
          end;
        end
        else
        begin
          inc(lScanLine, FVPRect.Left + lOverlappingRect.Right);
          for llX := 0 to Trunc(lWidth) - 1 do
          begin
            DrawImagePoint(lSamples, lTmpBuf, lScanLine);
            inc(lTmpBuf, lSamples);
            dec(lScanLine);
          end;
        end;
      end;
    finally
      FImage.DicomUtils.DeleteBuffer(lBuf);
    end;
    //FAgg2D.LineColor($AA, $BB, $CC);
    //FAgg2D.NoFill;
    //FAgg2D.Rectangle(lXFrom, lYFrom, lXTo, lYTo);
  end;
end;

procedure TImageCell.DrawImage;
var
  lBuf: Pointer;
  lTmpBuf: Pointer;
  lBufSize: TLongOrdinal;
  lWidth: double;
  lSamples: Integer;
  x1: double;
  y1: double;
  x2: double;
  y2: double;
  llX: Integer;
  llY: Integer;
  lRect: TBasicCellRect;
  lScanLine: PLongWord;
  lOverlappingRect: TBasicCellRect;
  lAngle: Integer;
  lOrientation: Integer;
  lSize: Integer;
  lXPos: Integer;
  lBufWidth: Integer;

begin
  if FImage.ImgBuf = nil then
  begin
    lBuf := nil;
    FImage.DicomUtils.LoadDicomImageToBuffer(lBuf, lBufSize, True);
    FImage.ImgBuf := lBuf;
    FImage.ImgBufLen := lBufSize;
    Resample;
  end;

  // Si la imágen está dentro del viewport entonces se mostrará parte de ella
  lAngle := FImage.DicomUtils.Angle;
  if lAngle <> FAngle then
  begin
    FImage.DicomUtils.Angle := -lAngle;
    FImage.DicomUtils.Angle := FAngle;
  end;

  if FInverted then
    FImage.DicomUtils.Polarity := 1
  else
    FImage.DicomUtils.Polarity := 0;

  FImgRect := GetImgRect;
  lRect.Left:= FVPRect.Left;
  lRect.Top := FVPRect.Top;
  lRect.Right:= FVPRect.Right;
  lRect.Bottom:= FVPRect.Bottom;
  if Touch(lRect, FImgRect) then
  begin
    // se obtiene el area superpuesta,
    // es decir, el rectángulo contenedor de la imágen
    lOverlappingRect := GetOverlappingRect(lRect, FImgRect);

    lOrientation := 1;
    if FHFlip then
      lOrientation := 1;

    (* lWidth y lHeight es el ancho y alto de la imágen
       resultante, o sea, escalada *)
    lTmpBuf := FResampledBuf;
    lSamples := FImage.DicomUtils.SamplesPerPixel;

    x1 := (lOverlappingRect.Left - 0 * lOrientation);
    x2 := (lOverlappingRect.Right - 0 * lOrientation);
    y1 := (lOverlappingRect.Top - FYOffset);
    y2 := (lOverlappingRect.Bottom - FYOffset);

    lWidth := x2 - x1;

    if lSamples = 1 then
      lSize := lSamples * SizeOf(Word)
    else
      lSize := lSamples * SizeOf(Byte);

    if (FYOffset < 0) then
      inc(lTmpBuf, lSize * FResampledWidth * Abs(FYOffset));

    // Todo: como idea armar una matriz que acomode los
    // punteros a las posiciones dentro del buffer armando
    // una grilla que contemple HFlip, VFlip, etc.
    for llY := lOverlappingRect.Top to lOverlappingRect.Bottom - 1 do
    begin
      lScanLine := FCanvasImage.ScanLine[llY];

      // se saltean los pixels desde la Izq hasta el borde
      // del overlapping rect.
      inc(lScanLine, lOverlappingRect.Left);

      // lXPos es la posición actual en
      // la linea actual del buffer
      lXPos := 0;
      // se dibujan los pixels del Scanline
      // durante el ancho de la celda,
      // desde el offset hasta el borde.

      if FHFlip then
      begin
        // Si se flipeó, el lXpos primero se
        // posiciona al final, y luego se decrementa
        if FXOffset > 0 then
          lBufWidth:= FResampledWidth
        else
          lBufWidth := FResampledWidth - (FResampledWidth - Round(lWidth));

        lXPos := lSize * lBufWidth;
        inc(lTmpBuf, lXPos);

        for llX := 0 to Round(lWidth) - 1 do
        begin
          DrawImagePointLut(lSamples, lTmpBuf, lScanLine, FLut);
          inc(lScanLine);
          dec(lTmpBuf, lSize );
          dec(lXPos, lSize);
        end;
        inc(lTmpBuf, (FResampledWidth * lSize) - lXPos);
      end
      else
      begin
        if (FXOffset < 0) then
        begin
          lXPos := lSize * Abs(FXOffset);
          inc(lTmpBuf, lXPos);
        end;
        for llX := 0 to Round(lWidth) - 1 do
        begin
          DrawImagePointLut(lSamples, lTmpBuf, lScanLine, FLut);
          inc(lScanLine);
          inc(lTmpBuf, lSize );
          inc(lXPos, lSize);
        end;
        // el largo de una línea en el buffer
        // es: lSize * FResampledWidth;
        // si lXPos es menor al largo de la línea
        // entonces movemos el puntero del buffer hasta el final
        if (lXPos < lSize * FResampledWidth) then
          inc(lTmpBuf, (lSize * FResampledWidth) - lXPos);
      end;
    end;

    {FAgg2d.NoFill;
    FAgg2d.LineWidth(0.5);
    FAgg2D.LineColor($AA, $ff, $00);
    FAgg2D.Rectangle(
      lOverlappingRect.Left,
      lOverlappingRect.Top,
      lOverlappingRect.Right - 1,
      lOverlappingRect.Bottom - 1);}
  end;
end;


end.

