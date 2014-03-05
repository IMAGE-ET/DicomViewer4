unit viewerpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  libDicomUtilsWrapperDyn,
  fptimer,
  fpg_main,
  fpg_widget,
  fpg_base,
  fpg_menu,
  utils,
  dicomheader,
  series,
  images,
  basiccell,
  seriecell,
  imagecell,
  ruler,
  layout,
  viewercanvas,
  viewertypes;

type

  { TViewerPanel }

  (* El layout es el contenedor de series,
     cada serie contiene un sub-layout con las
     imágenes.
     Cada serie es contenida en un TLayoutCell, donde
     se define la posición y tamaño de la celda contenedora
     de imágenes.
  *)
  TOnModeChange = procedure(AMode: TViewerMode) of object;

  TViewerPanel = class(TfpgWidget)
  private
    FOnModeChange: TOnModeChange;
    FOnSelect: TNotifyEvent;
    FPopUp: TfpgPopupMenu;
  protected
    FLayout: TLayout;
    FOldLayout: TLayout;
    FViewerMode: TViewerMode;
    FMiddleButtonDragging: Boolean;
    FMouseDown: Boolean;
    FMouseX: Integer;
    FMouseY: Integer;
    FViewerCanvas: TViewerCanvas;
    FMimeChoice: string;
    FDragging: Boolean;
    procedure ShowDicomHeader(Sender: TObject);
    procedure Print(Sender: TObject);
    procedure Export(Sender: TObject);
    procedure Paint(Sender: TObject);
    procedure Resize(Sender: TObject);
    procedure MouseWheel(Sender: TObject; AShift: TShiftState; AWheelDelta: Single; const AMousePos: TPoint);
    procedure MouseDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure DragEnter(Sender, Source: TObject; AMimeList: TStringList;
      var AMimeChoice: TfpgString; var ADropAction: TfpgDropAction; var Accept: Boolean);
    procedure DragDrop(Sender, Source: TObject; X, Y: integer; AData: variant);
    procedure SetViewerMode(AValue: TViewerMode);
    procedure DragStart(Sender: TObject);
    procedure Rotate90CW(Sender: TObject);
    procedure Rotate90CCW(Sender: TObject);
    procedure HFlip(Sender: TObject);
    procedure VFlip(Sender: TObject);
    procedure Invert(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ToBmp;
    procedure ResetLayoutDimensions;
    procedure NextImage(AIncrement: Integer);
    procedure PriorImage(AIncrement: Integer);
    property Layout: TLayout read FLayout write FLayout;
    property ViewerMode: TViewerMode write SetViewerMode;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnAfterModeChange: TOnModeChange read FOnModeChange write FOnModeChange;
  end;

implementation


{ TViewerPanel }

procedure TViewerPanel.ShowDicomHeader(Sender: TObject);
begin
  if (FLayout.CurrentImageCell <> nil) and
    ((FLayout.CurrentImageCell as TImageCell).Image <> nil) then
    TdlgDicomHeader.Execute((FLayout.CurrentImageCell as TImageCell).Image);
end;

procedure TViewerPanel.Print(Sender: TObject);
begin
  FLayout.ToPrinter;
end;

procedure TViewerPanel.Export(Sender: TObject);
begin
  ToBmp;
end;

procedure TViewerPanel.Paint(Sender: TObject);
begin
  FLayout.Draw(FViewerCanvas);
  FViewerCanvas.CanvasImage.UpdateImage;
  Canvas.DrawImage(0, 0, FViewerCanvas.CanvasImage);
end;

procedure TViewerPanel.Resize(Sender: TObject);
var
  lA4Height: Integer;
  lFontSize: double;
begin
  ResetLayoutDimensions;
  FViewerCanvas.Allocate(Width, Height);

  lA4Height := Round(FViewerCanvas.DPI * 10.98);
  lFontSize := Height * 12 / lA4Height;

  FViewerCanvas.AggCanvas2D.Font(utils.GetFont, lFontSize);
end;

procedure TViewerPanel.MouseWheel(Sender: TObject; AShift: TShiftState;
  AWheelDelta: Single; const AMousePos: TPoint);
begin
  if ssCtrl in AShift then
  begin
    if FLayout.CurrentCell = nil then
      exit;
    if AWheelDelta > 0 then
      (FLayout.CurrentCell as TSerieCell).AdjustImageCells(1)
    else
    if AWheelDelta < 0 then
      (FLayout.CurrentCell as TSerieCell).AdjustImageCells(-1);
    FLayout.CurrentImageCell := (FLayout.CurrentCell as TSerieCell).ImageCells[0];
    Invalidate;
  end
  else
  begin
    if AWheelDelta > 0 then
      NextImage(Trunc(AWheelDelta))
    else
    if AWheelDelta < 0 then
      PriorImage(abs(Trunc(AWheelDelta)));
  end;
end;

procedure TViewerPanel.MouseDoubleClick(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
var
  lCurrentImageCell: TImageCell;
begin
  lCurrentImageCell := FLayout.CurrentImageCell as TImageCell;
  if (lCurrentImageCell = nil) or (lCurrentImageCell.Image = nil) then
    exit;

  if mbLeft = AButton then
  begin
    // reemplazamos el layout actual por TSingleImageLayout
    if FOldLayout = nil then
    begin
      FOldLayout := FLayout;
      FLayout := TSingleImageLayout.Create;
      (* Se llama a Resize para actualizar el tamaño del layout *)
      lCurrentImageCell := FLayout.Cells[0].ImageCells[0];
      Resize(Self);
      FLayout.Cells[0].Serie := TImageCell(FOldLayout.CurrentImageCell).Image.Serie as TSerie;
      FLayout.Cells[0].FirstImageIndex := (FOldLayout.CurrentCell as TSerieCell).FirstImageIndex;
      lCurrentImageCell.WindowWidth:= TImageCell(FOldLayout.CurrentImageCell).WindowWidth;
      lCurrentImageCell.WindowCenter:= TImageCell(FOldLayout.CurrentImageCell).WindowCenter;
      lCurrentImageCell.RightScale:= TImageCell(FOldLayout.CurrentImageCell).RightScale;
      FLayout.CurrentImageCell := lCurrentImageCell;
      FLayout.CurrentCell := FLayout.Cells[0];
    end
    else
    begin
      (* Se elimina el layout de 1x1 *)
      (FOldLayout.CurrentCell as TSerieCell).FirstImageIndex := (FLayout.CurrentCell as TSerieCell).FirstImageIndex;
      (FOldLayout.CurrentImageCell as TImageCell).WindowCenter := (FLayout.CurrentImageCell as TImageCell).WindowCenter;
      (FOldLayout.CurrentImageCell as TImageCell).WindowWidth := (FLayout.CurrentImageCell as TImageCell).WindowWidth;
      FreeAndNil(FLayout);
      (* Se reasigna el layout original *)
      FLayout := FOldLayout;
      (* Se llama a Resize para actualizar el tamaño del layout *)
      Resize(Self);
      FOldLayout := nil;
    end;
    FLayout.SetAllChanged;
    Invalidate;
  end;
end;

procedure TViewerPanel.MouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  lCurrentImageCell: TImageCell;
begin
  if mbMiddle = AButton then
  begin
    FMiddleButtonDragging := False;
    FLayout.SetAllChanged;
    Invalidate;
  end
  else
  if mbRight = AButton then
  begin
    FPopup.ShowAt(Self, AMousePos.X, AMousePos.Y);
  end
  else
  if mbLeft = AButton then
  begin
    OnDragStartDetected := nil;
    FDragging := False;
    FMouseDown:= False;
    lCurrentImageCell := nil;
    if FLayout.CurrentCell <> nil  then
      lCurrentImageCell := FLayout.CurrentImageCell as TImageCell;

    if lCurrentImageCell = nil then
      exit;

    if FViewerMode = vmMagnify then
    begin
      lCurrentImageCell.Magnifying := False;
      lCurrentImageCell.Changed := True;
      Invalidate;
    end;
    if FViewerMode = vmRuler then
    begin
      lCurrentImageCell.CurrentVector := nil;
      ViewerMode:= vmSelect;
    end
    else
    if FViewerMode = vmWL then
    begin
      lCurrentImageCell.WLX := AMousePos.X;
      lCurrentImageCell.WLY := AMousePos.Y;
    end
    else
    if FViewerMode = vmZoom then
    begin
      lCurrentImageCell.ZoomX := AMousePos.X;
      lCurrentImageCell.ZoomY := AMousePos.Y;
    end
    else
    if FViewerMode = vmPan then
    begin
      lCurrentImageCell.X := AMousePos.X;
      lCurrentImageCell.Y := AMousePos.Y;
    end;
  end;
end;

procedure TViewerPanel.MouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  lCurrentImageCell: TImageCell;
  lOldImageCell: TImageCell;
  lPoint: TPoint;
begin
  if mbMiddle = AButton then
  begin
    FLayout.SetCurrentCell(AMousePos.X, AMousePos.Y);
    lCurrentImageCell := FLayout.CurrentImageCell as TImageCell;
    if lCurrentImageCell = nil then
      exit;

    FMiddleButtonDragging := True;
    lCurrentImageCell.X := AMousePos.X;
    lCurrentImageCell.Y := AMousePos.Y;
  end
  else
  if mbLeft = AButton then
  begin
    FMouseDown := True;
    lOldImageCell := FLayout.CurrentImageCell as TImageCell;
    FLayout.SetCurrentCell(AMousePos.X, AMousePos.Y);
    lCurrentImageCell := FLayout.CurrentImageCell as TImageCell;

    if lCurrentImageCell = nil then
      exit;

    if Assigned(FOnSelect) then
      FOnSelect(Self);

    lPoint.X := Round(AMousePos.X - lCurrentImageCell.VPRect.Left);
    lPoint.Y := Round(AMousePos.Y - lCurrentImageCell.VPRect.Top);

    if ssCtrl in AShift then
    begin
      OnDragStartDetected:=@DragStart;
      FDragging := True;
      MouseCursor := mcHand;
    end;

    if FViewerMode = vmMagnify then
    begin
      lCurrentImageCell.Magnifying := True;
      lCurrentImageCell.MagnifyPoint := lPoint;
    end
    else
    if FViewerMode = vmZoom then
    begin
      lCurrentImageCell.ZoomX := AMousePos.X;
      lCurrentImageCell.ZoomY := AMousePos.Y;
    end
    else
    if FViewerMode = vmWL then
    begin
      lCurrentImageCell.WLX := AMousePos.X;
      lCurrentImageCell.WLY := AMousePos.Y;
    end
    else
    if FViewerMode = vmSelect then
    begin
      lCurrentImageCell.VectorAtPoint(lPoint);
    end
    else
    if FViewerMode = vmRuler then
    begin
      if not lCurrentImageCell.VectorAtPoint(lPoint) then
        lCurrentImageCell.CreateVectorAtPoint(TRuler, lPoint);
    end
    else
    if FViewerMode = vmPan then
    begin
      lCurrentImageCell.X := AMousePos.X;
      lCurrentImageCell.Y := AMousePos.Y;
    end;
    lCurrentImageCell.Changed := True;
    if lOldImageCell <> lCurrentImageCell then
      FLayout.SetAllChanged;
    invalidate;
    fpgApplication.ProcessMessages;
  end;
end;

procedure TViewerPanel.MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
var
  lCurrentImageCell: TImageCell;
  lWW: double;
  lWC: double;
  lZoomFactor: double;
  lDistance: Integer;
  lPoint: TPoint;
begin
  lCurrentImageCell := FLayout.CurrentImageCell as TImageCell;
  if lCurrentImageCell = nil then
  begin
    exit;
  end;

  if ssMiddle in AShift then
  begin
    if FMiddleButtonDragging then
    begin
      if AMousePos.Y < lCurrentImageCell.Y then
        FLayout.CurrentCell.PriorImage(Abs(lCurrentImageCell.Y - AMousePos.Y) div 4)
      else
      if AMousePos.Y > lCurrentImageCell.Y then
        FLayout.CurrentCell.NextImage(Abs(lCurrentImageCell.Y - AMousePos.Y) div 4)
      else
        exit;

      lCurrentImageCell.X := AMousePos.X;
      lCurrentImageCell.Y := AMousePos.Y;
      lCurrentImageCell.Changed := True;

      invalidate;
    end;
  end
  else
  if ssLeft in AShift then
  begin
    if FDragging then
      exit;
    if not FMouseDown then
      exit; // se usa para impedir que se mueva la imágen al arrastrar thumbnails sobre TViewerPanel.

    lPoint.X := Round(AMousePos.X - lCurrentImageCell.VPRect.Left);
    lPoint.Y := Round(AMousePos.Y - lCurrentImageCell.VPRect.Top);

    if FViewerMode = vmMagnify then
    begin
      lCurrentImageCell.MagnifyPoint := lPoint;
      lCurrentImageCell.Changed := True;
      Invalidate;
    end;
    if FViewerMode = vmSelect then
    begin
      if lCurrentImageCell.CurrentVector <> nil then
      begin
        lCurrentImageCell.CurrentVector.Drag(lPoint.X, lPoint.Y);
        lCurrentImageCell.Changed := True;
        Invalidate;
      end;
    end
    else
    if FViewerMode = vmRuler then
    begin
      if lPoint.X <> lCurrentImageCell.CurrentVector.X then
        lCurrentImageCell.CurrentVector.Drag(lPoint.X, lPoint.Y);
      lCurrentImageCell.Changed := True;
      Invalidate;
    end
    else
    if FViewerMode = vmZoom then
    begin
      lCurrentImageCell.Image.DicomUtils.GetWindowLevel;
      if AMousePos.Y > lCurrentImageCell.ZoomY then
        lZoomFactor := 0.95
      else
        lZoomFactor := 1.05;

      if (lCurrentImageCell.Zoom * lZoomFactor >= 3) or (lCurrentImageCell.Zoom * lZoomFactor <= 0.01) then
        exit;

      lCurrentImageCell.ZoomX := AMousePos.X;
      lCurrentImageCell.ZoomY := AMousePos.Y;
      lCurrentImageCell.Zoom := lZoomFactor;
      lCurrentImageCell.Changed := True;

      Invalidate;
    end
    else
    if FViewerMode = vmWL then
    begin
      if lCurrentImageCell.Image = nil then
        exit;

      lWW := lCurrentImageCell.WindowWidth;
      lWC := lCurrentImageCell.WindowCenter;

      lDistance := Abs(AMousePos.X - lCurrentImageCell.WLX);
      lDistance := Trunc(lDistance * lWW / 100);
      if AMousePos.X > lCurrentImageCell.WLX then
        lWW := lWW + lDistance
      else
        lWW := lWW - lDistance;

      lDistance := Abs(AMousePos.Y - lCurrentImageCell.WLY);
      lDistance := Trunc(lDistance * lWC / 100);
      if AMousePos.Y > lCurrentImageCell.WLY then
        lWC := lWC + lDistance
      else
        lWC := lWC - lDistance;

      TImageCell(lCurrentImageCell).WindowCenter:= lWC;
      TImageCell(lCurrentImageCell).WindowWidth:= lWW;
      TImageCell(lCurrentImageCell).MakeWindowLut;

      lCurrentImageCell.WLX := AMousePos.X;
      lCurrentImageCell.WLY := AMousePos.Y;
      lCurrentImageCell.Changed := True;

      Invalidate;
      fpgApplication.ProcessMessages;
    end
    else
    if FViewerMode = vmPan then
    begin
      if AMousePos.X < lCurrentImageCell.X then
        lCurrentImageCell.XOffset := lCurrentImageCell.XOffset - Abs(AMousePos.X - lCurrentImageCell.X)
      else
        lCurrentImageCell.XOffset := lCurrentImageCell.XOffset + Abs(AMousePos.X - lCurrentImageCell.X);

      if AMousePos.Y < lCurrentImageCell.Y then
        lCurrentImageCell.YOffset := lCurrentImageCell.YOffset - Abs(AMousePos.Y - lCurrentImageCell.Y)
      else
        lCurrentImageCell.YOffset := lCurrentImageCell.YOffset + Abs(AMousePos.Y - lCurrentImageCell.Y);

      lCurrentImageCell.X := AMousePos.X;
      lCurrentImageCell.Y := AMousePos.Y;
      lCurrentImageCell.Changed := True;

      Invalidate;
    end;
  end;
end;

procedure TViewerPanel.DragEnter(Sender, Source: TObject;
  AMimeList: TStringList; var AMimeChoice: TfpgString;
  var ADropAction: TfpgDropAction; var Accept: Boolean);
begin
  FMimeChoice := '';
  if FLayout is TSingleImageLayout then
  begin
    Accept := False;
    exit;
  end;
  if AMimeList.Count > 0 then
  begin
    AMimeChoice:= AMimeList[0];
    FMimeChoice := AMimeChoice;
    Accept := True;
  end;
end;

procedure TViewerPanel.DragDrop(Sender, Source: TObject; X, Y: integer;
  AData: variant);
var
  lSeries: TSeries;
  lSerie: TSerie;
  lImage: TImage;
begin
  FLayout.SetCurrentCell(X, Y);
  if FLayout.CurrentCell <> nil then
  begin
    // los datos dropeados pueden ser una serie, o bien
    // una imágen.
    if FMimeChoice = 'ImageFile' then
    begin
      lSeries := FLayout.Series;
      lSerie := TSerie.Create(lSeries);
      lSeries.Add(lSerie);
      lImage := TImage.Create(lSerie);
      lImage.FileName:= AData;
      lSerie.Images.Add(lImage);
      (FLayout.CurrentCell as TSerieCell).Serie := lSerie;
    end
    else
    if FMimeChoice = 'SerieIndex' then
    begin
      (FLayout.CurrentCell as TSerieCell).Serie := FLayout.Series[AData];
    end
    else
      exit;
    (FLayout.CurrentCell as TSerieCell).FirstImageIndex := 0;
    (FLayout.CurrentImageCell as TImageCell).WindowCenter := -1;
    (FLayout.CurrentImageCell as TImageCell).WindowWidth := -1;
    (FLayout.CurrentImageCell as TImageCell).Changed := True;
    (FLayout.CurrentImageCell as TImageCell).Empty:= True;
    Invalidate;
  end;
end;

procedure TViewerPanel.SetViewerMode(AValue: TViewerMode);
begin
  if (not (AValue in [vmSelect])) and (AValue = FViewerMode) then
    AValue := vmNone;
  FViewerMode := AValue;
  case AValue of
    vmNone: MouseCursor:= mcDefault;
    vmZoom: MouseCursor:= mcArrow;
    vmPan: MouseCursor:= mcMove;
    vmWL: MouseCursor:= mcCross;
    vmRuler: MouseCursor:= mcArrow;
    vmSelect: MouseCursor:= mcArrow;
    vmMagnify: MouseCursor:= mcCross;
  end;

  if Assigned(FOnModeChange) then
    FOnModeChange(AValue);
end;

procedure TViewerPanel.DragStart(Sender: TObject);
var
  d: TfpgDrag;
  m: TfpgMimeData;
  lImageFile: string;
begin
  lImageFile := (FLayout.CurrentImageCell as TImageCell).Image.FileName;
  m := TfpgMimeData.Create;
  m.SetData('ImageFile', lImageFile);
  d := TfpgDrag.Create(Sender as TfpgWindow);
  d.MimeData := m;
  d.Execute([daCopy]);
end;

procedure TViewerPanel.Rotate90CW(Sender: TObject);
begin
  (FLayout.CurrentImageCell as TImageCell).Rotate90CW;
  Invalidate;
end;

procedure TViewerPanel.Rotate90CCW(Sender: TObject);
begin
  (FLayout.CurrentImageCell as TImageCell).Rotate90CCW;
  Invalidate;
end;

procedure TViewerPanel.HFlip(Sender: TObject);
begin
  (FLayout.CurrentImageCell as TImageCell).HFlip;
  (FLayout.CurrentImageCell as TImageCell).Changed:= True;
  if Sender is TfpgMenuItem then
    (Sender as TfpgMenuItem).Checked := not (Sender as TfpgMenuItem).Checked;
  Invalidate;
end;

procedure TViewerPanel.VFlip(Sender: TObject);
begin
  (FLayout.CurrentImageCell as TImageCell).VFlip;
  (FLayout.CurrentImageCell as TImageCell).Changed:= True;
  if Sender is TfpgMenuItem then
    (Sender as TfpgMenuItem).Checked := not (Sender as TfpgMenuItem).Checked;
  Invalidate;
end;

procedure TViewerPanel.Invert(Sender: TObject);
begin
  (FLayout.CurrentImageCell as TImageCell).Invert;
  if Sender is TfpgMenuItem then
    (Sender as TfpgMenuItem).Checked := not (Sender as TfpgMenuItem).Checked;
  Invalidate;
end;

constructor TViewerPanel.Create(AOwner: TComponent);
begin
  inherited;
  FLayout := TLayout.Create(Self);
  FOldLayout := nil;
  FViewerMode:= vmPan;
  FMiddleButtonDragging := False;
  FMouseDown := False;
  AcceptDrops:= True;
  FViewerCanvas := TViewerCanvas.Create(72);
  FDragging := False;
  OnResize:= @Resize;
  OnPaint := @Paint;
  OnMouseDown:= @MouseDown;
  OnMouseMove:= @MouseMove;
  OnMouseUp:= @MouseUp;
  OnMouseScroll:=@MouseWheel;
  OnDoubleClick:= @MouseDoubleClick;
  OnDragDrop:=@DragDrop;
  OnDragEnter:=@DragEnter;

  FPopUp := TfpgPopupMenu.Create(Self);
  with FPopUp do
  begin
    AddMenuItem('Dicom Header', '', @ShowDicomHeader);
    AddMenuItem('-', '', nil);
    AddMenuItem('Print...', '', @Print);
    AddMenuItem('Export...', '', @Export);
    AddMenuItem('-', '', nil);
    AddMenuItem('Invert', '', @Invert);
    AddMenuItem('-', '', nil);
    AddMenuItem('Rotate 90º CW', '', @Rotate90CW);
    AddMenuItem('Rotate 90º CCW', '', @Rotate90CCW);
    AddMenuItem('H. Flip', '', @HFlip);
    AddMenuItem('V. Flip', '', @VFlip);
  end;

  Self.Width := 200;
  Self.Height := 200;
  Resize(Self);
end;

destructor TViewerPanel.Destroy;
begin
  FViewerCanvas.Free;
  FLayout.Free;
  inherited Destroy;
end;

procedure TViewerPanel.ToBmp;
begin
  FLayout.ToBmp(Width, Height);
end;

procedure TViewerPanel.ResetLayoutDimensions;
var
  lCell: TSerieCell;
  lImageCell: TImageCell;

begin
  FLayout.Width := Self.Width;
  FLayout.Height := Self.Height;
  for lCell in FLayout.Cells do
    for lImageCell in lCell.ImageCells do
    begin
      if lImageCell.Image <> nil then
      begin
        lImageCell.ZoomToFit;
        lImageCell.AlignByModality;
      end;
    end;
  FLayout.SetAllChanged;
end;

procedure TViewerPanel.NextImage(AIncrement: Integer);
begin
  if FLayout.CurrentCell = nil then
    Exit;
  FLayout.CurrentCell.NextImage(AIncrement);
  FLayout.SetAllChanged;
  Invalidate;
end;

procedure TViewerPanel.PriorImage(AIncrement: Integer);
begin
  if FLayout.CurrentCell = nil then
    Exit;
  FLayout.CurrentCell.PriorImage(AIncrement);
  FLayout.SetAllChanged;
  Invalidate;
end;

end.

