unit seriespanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_img_utils,
  Series,
  contnrs,
  images,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_panel,
  Agg2D,
  Utils,
  FPimage,
  fpTimer,
  fgl,
  libDicomUtilsWrapperDyn,
  commonmessages;

type

  { TThumbNails }

  { TSeriePosition }

  TSeriePosition = class
  private
    FSerie: TSerie;
    FThumbNail: TfpgImage;
    FTop: Integer;
    procedure CopyBufferToFpgImage(
      ABmp: TfpgImage;
      ABuffer: Pointer;
      ABufSize: TLongOrdinal;
      AWidth, AHeight: Integer;
      ASamples: Integer;
      ALut: TLut);
    procedure DrawText(AText: string; FillBackGround: Boolean);
    procedure SaveToFile(ABmp: TfpgImage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw(AImage: TImage);
    property Serie: TSerie read FSerie write FSerie;
    property Top: Integer read FTop write FTop;
    property ThumbNail: TfpgImage read FThumbNail;
  end;

  TSeriesPositions = specialize TFPGList<TSeriePosition>;

  TThumbNails = class(TfpgPanel)
  private
    FLastClickedPanel: TfpgWidget;
    FInternalImage: TfpgImage;
    FAgg2D: TAgg2D;
    FOffset: Integer;
    FY: Integer;
    FX: Integer;
    FCurrentSerie: Integer;
    FFPTimer: TFPTimer;
    FSeriesPositions: TSeriesPositions;
    procedure DoDraw(ASeriePosition: TSeriePosition);
    procedure DragStartDetected(Sender: TObject);
    procedure CleanUpInternalImage;
    procedure Paint(Sender: TObject);
    procedure ExecuteTimer(Sender: TObject);
    procedure CreateNewPanel(ASerie: TSerie);
    procedure MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearInternalImage;
    procedure CreateThumbnail(ASerie: TSerie);
    procedure SelectSerie(ASerie: TSerie);
  end;

  { TSeriesCaption }

  TSeriesCaption = class(TfpgPanel)
  private
    FSeriesPanelImage: TfpgImage;
    FSeriesPanelAgg2d: TAgg2D;
    procedure DrawSeriesPanel;
    procedure SeriesPanelPaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TSeriesPanel }

  TSeriesPanel = class(TfpgPanel)
  private
    FThumbNails: TThumbNails;
    FSeriesCaption: TSeriesCaption;
    procedure Paint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    procedure CreateThumbNails(ASerie: TSerie);
    property ThumbNails: TThumbNails read FThumbNails;
  end;

implementation

const
  mWidth = 100;
  mHeight = 120;
  mTextFrameHeight = 20;


{ TSeriePosition }

procedure TSeriePosition.CopyBufferToFpgImage(
  ABmp: TfpgImage;
  ABuffer: Pointer;
  ABufSize: TLongOrdinal;
  AWidth, AHeight: Integer;
  ASamples: Integer;
  ALut: TLut);
var
  lScanLine: PLongWord;
  lRed: byte;
  lGreen: byte;
  lBlue: byte;
  X: Integer;
  Y: Integer;
  lSize: Integer;
  lTmpBuf: Pointer;

begin
  lTmpBuf := ABuffer;

  if ASamples = 1 then
    lSize := ASamples * SizeOf(Word)
  else
    lSize := ASamples * SizeOf(Byte);

  for Y := 0 to AHeight - 1 do
  begin
    lScanLine := ABmp.ScanLine[Y];
    for X := 0 to AWidth - 1 do
    begin
      if ASamples = 1 then
      begin
        lRed := ALut[Word(lTmpBuf^)];
        lGreen := lRed; // los tres canales son iguales
        lBlue := lRed;
      end
      else
      begin
        // es Blue-Green-Red (BGR)
        lRed := PByte(lTmpBuf)[2];
        lGreen := PByte(lTmpBuf)[1];
        lBlue := PByte(lTmpBuf)[0];
      end;
      lScanLine^ :=  lRed or (lGreen shl 8) or (lBlue shl 16) or (alphaOpaque shl 24);
      inc(lTmpBuf, lSize);
      inc(lScanLine);
    end;
  end;
end;

procedure TSeriePosition.DrawText(AText: string; FillBackGround: Boolean);
var
  lAgg2d: TAgg2D;
  lBmp: TfpgImage;
  lTop: Integer;
  lLeft: Integer;

begin
  lAgg2D := TAgg2D.Create(nil);
  lBmp := TfpgImage.Create;
  lBmp.AllocateImage(32, mWidth - 2, mTextFrameHeight);
  try
    lAgg2D.Attach(lBmp);
    // se dibuja el recuadro para la descripción
    lAgg2D.NoLine;
    lAgg2D.FillColor($10, $10, $10);
    lAgg2D.ClipBox(0, 0, lBmp.Width, lBmp.Height);
    lAgg2D.Rectangle(0, 0, lBmp.Width, lBmp.Height, False);

    // se escribe el texto
    lAgg2D.NoLine;
    lAgg2D.FillColor($ff, $ff, $ff);
    lAgg2D.Font(utils.GetFont, utils.mFontSize);
    lTop := lBmp.Height - Trunc(mTextFrameHeight/2 - lAgg2D.FontHeight/2) - 2;
    lLeft := Trunc((lBmp.Width / 2) - (lAgg2D.TextWidth(AText) / 2));
    lAgg2D.Text(lLeft, lTop, AText, true, 0.0, 0.0);
    lAgg2d.DrawImage(0, 0, lBmp);
    lBmp.UpdateImage;

    lAgg2d.Attach(FThumbNail);

    if FillBackGround then
    begin
      lAgg2D.NoLine;
      lAgg2D.FillColor($0, $0, $0);
      lAgg2D.ClipBox(0, 0, FThumbNail.Width, FThumbNail.Height);
      lAgg2D.Rectangle(0, 0, FThumbNail.Width, FThumbNail.Height, False);
    end;

    lAgg2d.DrawImage(2, FThumbNail.Height - mTextFrameHeight, lBmp);
    FThumbNail.UpdateImage;
  finally
    lBmp.Free;
    lAgg2d.Free;
  end;
end;

procedure TSeriePosition.SaveToFile(ABmp: TfpgImage);
var
  lBmp: TFPMemoryImage;
begin
  lBmp := TFPMemoryImage.create(ABmp.Width, ABmp.Height);
  try
    FpgImageToFPImage(ABmp, lBmp);
    lBmp.SaveToFile('/home/leonardo/salida.bmp');
  finally
    lBmp.Free;
  end;
end;

constructor TSeriePosition.Create;
begin
  FThumbNail := TfpgImage.Create;
  FThumbNail.AllocateImage(32, mWidth, mHeight);
end;

destructor TSeriePosition.Destroy;
begin
  FThumbNail.Free;
  inherited Destroy;
end;

procedure TSeriePosition.Draw(AImage: TImage);
var
  lBmp: TfpgImage;
  lFactor: double;
  lVFactor: double;
  lHFactor: double;
  lBevelWidth: Integer;
  lLeft: double;
  lTop: double;
  lBoxW: double;
  lBoxH: double;
  lDescription: string;
  lSeriePosition: TSeriePosition;
  lAgg2d: TAgg2D;
  lResampledWidth: Integer;
  lResampledHeight: Integer;
  lBuf: Pointer;
  lBufSize: TLongOrdinal;
  lResampledBuf: Pointer;
  lResampledBufSize: TLongOrdinal;
  lLut: TLut;

begin
  lBevelWidth := 2;
  lHFactor := (mWidth - (lBevelWidth * 4)) / AImage.DicomUtils.ImgWidth;
  lVFactor := ((mHeight - mTextFrameHeight) - (lBevelWidth * 4)) / AImage.DicomUtils.ImgHeight;
  if lVFactor < lHFactor then
    lFactor:= lVFactor
  else
    lFactor:= lHFactor;

  lBuf := nil;
  AImage.DicomUtils.LoadDicomImageToBuffer(lBuf, lBufSize, true);
  AImage.ImgBuf:= lBuf;
  AImage.ImgBufLen:= lBufSize;
  lLut := AImage.MakeWindowLut(AImage.WC, AImage.WW);
  lResampledBuf := nil;
  lResampledBufSize := 0;
  AImage.Resample(lFactor, lResampledWidth, lResampledHeight, lResampledBuf, lResampledBufSize);

  lBmp := TfpgImage.Create;
  lBmp.AllocateImage(32, lResampledWidth, lResampledHeight);
  lAgg2d := TAgg2D.Create(nil);
  try
    lAgg2d.Attach(FThumbNail);

    CopyBufferToFpgImage(lBmp, lResampledBuf, lResampledBufSize, lResampledWidth, lResampledHeight, AImage.DicomUtils.SamplesPerPixel, lLut);
    lBmp.UpdateImage;
    //SaveToFile(lBmp);

    lAgg2D.FillColor($FA, $AF, $FA);
    lAgg2D.FillRectangle(0, FThumbNail.Height - mHeight, mWidth, FThumbNail.Height);

    lLeft := Trunc((mWidth / 2) - (lResampledWidth / 2));
    lTop := FThumbNail.Height - (mHeight - Round(((mHeight - mTextFrameHeight) / 2) - (lResampledHeight / 2)));
    lAgg2D.DrawImage(Round(lLeft), Round(lTop), lBmp);

    lDescription := AImage.SeriesDescription;
    if (lDescription = '') or (lDescription = 'No series description found.') then
      lDescription := AImage.SeriesNumber;

    // quantity box
    if FSerie.Images.Count > 1 then
    begin
      lAgg2D.Font(utils.GetFont, utils.mFontSize);
      lBoxW := lAgg2D.TextWidth(IntToStr(FSerie.Images.Count));
      lBoxH := lAgg2D.FontHeight;
      lTop := 4;
      lLeft := FThumbNail.Width - (lBoxW + 4);

      lAgg2D.NoLine;
      lAgg2D.FillColor($77, $88, $99);
      lAgg2D.Rectangle(lLeft - 2, lTop - 2, lLeft + lBoxW + 4, lTop + lBoxH + 2);

      lAgg2D.NoLine;
      lAgg2D.FillColor($00, $00, $00);
      lAgg2D.Text(lLeft + 1, lTop + lBoxH, IntToStr(FSerie.Images.Count), false, 0.0, 0.0);
    end;

    FThumbNail.UpdateImage;

    DrawText(lDescription, False);

  finally
    Freemem(lResampledBuf);
    //AImage.Free;
    lBmp.Free;
    lAgg2d.Free;
  end;
end;

{ TSeriesCaption }

procedure TSeriesCaption.DrawSeriesPanel;
var
  lLeft: Integer;
  lTop: Integer;
begin
  FSeriesPanelImage.AllocateImage(32, width, height);
  FSeriesPanelAgg2d.Attach(FSeriesPanelImage);
  FSeriesPanelAgg2d.NoLine;
  FSeriesPanelAgg2d.FillColor($30,$30,$30);
  FSeriesPanelAgg2d.Rectangle(0,0,Width, Height);
  FSeriesPanelAgg2d.NoLine;
  FSeriesPanelAgg2d.FillColor($80,$80,$80);
  FSeriesPanelAgg2d.Font(utils.GetFont, Utils.mFontSize);
  lLeft:= Round(Width / 2 - FSeriesPanelAgg2d.TextWidth('Series') / 2);
  lTop:= Round((Height / 2) - (FSeriesPanelAgg2d.FontHeight / 2));
  FSeriesPanelAgg2d.TextAlignment(0, 1);
  FSeriesPanelAgg2d.Font(Utils.GetFont, Utils.mFontSize);
  FSeriesPanelAgg2d.Text(lLeft, lTop, 'Series');
  FSeriesPanelImage.UpdateImage;
end;

procedure TSeriesCaption.SeriesPanelPaint(Sender: TObject);
begin
  DrawSeriesPanel;
  Canvas.DrawImage(0, 0, FSeriesPanelImage);
end;

constructor TSeriesCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '';
  Align:= alTop;
  Style:=bsFlat;
  Height := 30;
  FSeriesPanelAgg2d := TAgg2D.Create(nil);
  FSeriesPanelImage := TfpgImage.Create;
  DrawSeriesPanel;
  OnPaint:=@SeriesPanelPaint;
end;

destructor TSeriesCaption.Destroy;
begin
  FSeriesPanelImage.Free;
  FSeriesPanelAgg2d.Free;
  inherited Destroy;
end;

{ TSeriesPanel }

procedure TSeriesPanel.Paint(Sender: TObject);
begin
  if Assigned(FThumbNails) then
    FThumbNails.RePaint;
end;

constructor TSeriesPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '';
  Width:= mWidth + 4;
  Style:=bsFlat;

  FSeriesCaption := TSeriesCaption.Create(Self);
  FSeriesCaption.Align:= alTop;

  FThumbNails := TThumbNails.Create(Self);
  FThumbNails.Align:= alClient;

  OnPaint := @Paint;
end;

destructor TSeriesPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TSeriesPanel.Reset;
begin
  FThumbNails.ClearInternalImage;
end;

procedure TSeriesPanel.CreateThumbNails(ASerie: TSerie);
begin
  FThumbNails.CreateThumbnail(ASerie);
  fpgPostMessage(Self, Owner, MSG_SERIESPANELUPDATE);
end;

{ TSeriePanel }

procedure TThumbNails.MouseMove(Sender: TObject; AShift: TShiftState;
  const AMousePos: TPoint);
begin
  if FInternalImage.Height = 0 then
    exit;

  if ssLeft in AShift then
  begin
    if AMousePos.X > Self.Width then
      exit;
    if FY < AMousePos.Y then
    begin
      begin
        if FOffset + Abs(FY - AMousePos.Y) <= 0 then
          Inc(FOffset, Abs(FY - AMousePos.Y));
      end;
    end
    else
    if FY > AMousePos.Y then
    begin
        if FInternalImage.Height + (FOffset - Abs(FY - AMousePos.Y)) > Height then
          Dec(FOffset, Abs(FY - AMousePos.Y));
    end;
    invalidate;
    FX := AMousePos.X;
    FY := AMousePos.Y;
  end;
end;

procedure TThumbNails.MouseUp(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if FInternalImage.Height = 0 then
    exit;

  if mbLeft = AButton then
  begin
    FY := AMousePos.Y;
    FX := AMousePos.X;
    FFPTimer.Enabled:= False;
    OnDragStartDetected := nil;
    MouseCursor := mcDefault;
  end;
end;

procedure TThumbNails.MouseDown(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if mbLeft = AButton then
  begin
    if FInternalImage.Height = 0 then
      exit;
    FX := AMousePos.X;
    FY := AMousePos.Y;
    FFPTimer.Enabled:= True;
    FCurrentSerie := Trunc((FY - FOffset) / mHeight);
    if FCurrentSerie > FSeriesPositions.Count - 1 then
      FCurrentSerie:= FSeriesPositions.Count - 1;
    Invalidate;
  end;
end;

{ TThumbNails }

procedure TThumbNails.DoDraw(ASeriePosition: TSeriePosition);
var
  lImages: TImages;
begin
  lImages := ASeriePosition.Serie.Images;
  if (lImages.Count > 0) and (lImages[0].DicomUtils.Loaded) then
    ASeriePosition.Draw(lImages[0])
  else
    ASeriePosition.DrawText(Format('%.2f Kb', [ASeriePosition.Serie.DownloadedSize / 1024]), True);
end;

procedure TThumbNails.DragStartDetected(Sender: TObject);
var
  d: TfpgDrag;
  m: TfpgMimeData;
begin
  m := TfpgMimeData.Create;
  m.SetData('SerieIndex', FSeriesPositions[FCurrentSerie].Serie.Index);
  d := TfpgDrag.Create(Sender as TfpgWindow);
  d.MimeData := m;
  d.Execute([daCopy]);
end;

procedure TThumbNails.CleanUpInternalImage;
begin
  // Se pinta de negro el espacio que ocupará la nueva imaǵen
  FAgg2D.FillColor($00, $00, $00);
  FAgg2D.FillRectangle(0, FInternalImage.Height - mHeight, mWidth, FInternalImage.Height);
  FInternalImage.UpdateImage;
end;

procedure TThumbNails.Paint(Sender: TObject);
var
  lLeft: Integer;
  lSeriePosition: TSeriePosition;
begin
  Canvas.DrawImage(0, FOffset, FInternalImage);

  if FCurrentSerie > -1 then
  begin
    // Se dibuja la selección actual
    lLeft := Trunc(Width/2-FInternalImage.Width/2);
    Canvas.Color:= clLime;
    Canvas.DrawRectangle(lLeft, (FSeriesPositions[FCurrentSerie].Top) + (FOffset), mWidth, mHeight);
  end;
end;

procedure TThumbNails.ExecuteTimer(Sender: TObject);
begin
  OnDragStartDetected:=@DragStartDetected;
  MouseCursor := mcHand;
end;

procedure TThumbNails.CreateNewPanel(ASerie: TSerie);
(* Se crea una imágen del tamaño mWidth * mHeight *)
var
  lImage: TImage;
  lSeriePosition: TSeriePosition;
begin
  // se crea la nueva serieposition
  lSeriePosition := TSeriePosition.Create;
  lSeriePosition.Serie := ASerie;
  FSeriesPositions.Add(lSeriePosition);

  DoDraw(lSeriePosition);
end;

constructor TThumbNails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := '';
  Width:= mWidth;
  Style:=bsFlat;
  BackgroundColor := $050505;
  FLastClickedPanel := nil;
  FInternalImage := TfpgImage.Create;
  FAgg2D := TAgg2D.Create(nil);
  FOffset := 0;
  FCurrentSerie:= -1;
  AcceptDrops:= False;
  FSeriesPositions := TSeriesPositions.Create;
  FFPTimer := TFPTimer.Create(nil);
  FFPTimer.Interval := 50;
  FFPTimer.OnTimer := @ExecuteTimer;
  FFPTimer.Enabled:= False;
  OnPaint:=@Paint;
  OnMouseDown:=@MouseDown;
  OnMouseMove:=@MouseMove;
  OnMouseUp:=@MouseUp;
end;

destructor TThumbNails.Destroy;
var
  lSeriePosition: TSeriePosition;
begin
  FInternalImage.Free;
  FAgg2D.Free;
  FFPTimer.Free;
  for lSeriePosition in FSeriesPositions do
  begin
    FreeAndNil(lSeriePosition);
  end;
  FSeriesPositions.Free;
  inherited Destroy;
end;

procedure TThumbNails.ClearInternalImage;
begin
  FAgg2D.ClearAll($00, $00, $00);
  FInternalImage.AllocateImage(32, mWidth, 0);
  FInternalImage.UpdateImage;
  Parent.BackgroundColor:=clWhite;
  FInternalImage.Free;
  FInternalImage := TfpgImage.Create;
  FOffset:= 0;
  FCurrentSerie:=-1;
end;

procedure TThumbNails.CreateThumbnail(ASerie: TSerie);
var
  I: Integer;
  lSeriePosition: TSeriePosition;

begin
  (* Si ya existe, entonces sale *)
  for I := 0 to FSeriesPositions.Count - 1 do
  begin
    if FSeriesPositions[I].Serie = ASerie then
    begin
      lSeriePosition := FSeriesPositions[I];
      DoDraw(lSeriePosition);
      //lSeriePosition.DrawText(Format('%d', [ASerie.DownloadedSize]));
      FAgg2D.DrawImage(0, I * mHeight, lSeriePosition.ThumbNail);
      exit;
    end;
  end;

  CreateNewPanel(ASerie);
  FInternalImage.AllocateImage(32, mWidth, FSeriesPositions.Count * mHeight);
  FAgg2D.Attach(FInternalImage);
  CleanUpInternalImage;
  for I := 0 to FSeriesPositions.Count - 1 do
  begin
    lSeriePosition := FSeriesPositions[I];
    lSeriePosition.Top := I * mHeight;
    FAgg2D.DrawImage(0, I * mHeight, lSeriePosition.FThumbNail);
  end;
  FInternalImage.UpdateImage;
end;

procedure TThumbNails.SelectSerie(ASerie: TSerie);
var
  I: Integer;
begin
  for I := 0 to FSeriesPositions.Count - 1 do
    if FSeriesPositions[I].Serie = ASerie then
    begin
      FCurrentSerie := I;
      Invalidate;
    end;
end;

end.

