unit layout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_main,
  fpg_base,
  fpg_img_utils,
  utils,
  FPimage,
  graphics,
  viewercanvas,
  seriecell,
  imagecell,
  basiccell,
  baselayout,
  Scout,
  series,
  hangingprotocol,
  layoutdefinition,
  paperprint,
  bitmapprint,
  printers,
  images;

type
  (* TLayoutCells container *)
  TLayout = class(TBaseLayout)
  private
    FOwner: TObject;
    FCells: TSerieCells;
    FOnRepaint: TNotifyEvent;
    FSeries: TSeries;
    procedure SetSeries(AValue: TSeries);
    procedure ToPrinter(AImage: TfpgImage);
    procedure DrawToCanvas(ACanvas: TViewerCanvas);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure CreateLayout(ALayoutDefinition: TLayoutDefinition);
    procedure Draw(AViewerCanvas: TViewerCanvas);
    procedure SetCurrentCell(AX, AY: Integer);
    procedure SetAllChanged;
    procedure DeleteSelectedVector;
    procedure ApplyHangingProtocol(AHangingProtocols: THangingProtocols);
    procedure ToBmp(AWidth, AHeight: Integer);
    procedure ToPrinter;
    property Series: TSeries read FSeries write SetSeries;
    property Cells: TSerieCells read FCells;
    property OnRepaint: TNotifyEvent read FOnRepaint write FOnRepaint;
  end;

  { TSingleImageLayout }

  TSingleImageLayout = class(TLayout)
  public
    constructor Create;
  end;


implementation

{ TSingleImageLayout }

constructor TSingleImageLayout.Create;
var
  lCell: TSerieCell;
  lImageCell: TImageCell;
begin
  (* Este layout se crea cuando el usuario
     hace doble click sobre una celda,
     muestra una imágen a pantalla completa.
  *)
  CurrentCell := nil;
  FCells := TSerieCells.Create;
  (* Celda 0,0 *)
  lCell := TSerieCell.Create(Self);
  lCell.Left := 0;
  lCell.Top := 0;
  lCell.Width:= 1;
  lCell.Height:= 1;
  lImageCell := TImageCell.Create(lCell);
  lImageCell.Left := 0;
  lImageCell.Top := 0;
  lImageCell.Width := 1;
  lImageCell.Height := 1;
  lCell.ImageCells.Add(lImageCell);
  FCells.Add(lCell);
end;

{ TLayout }

constructor TLayout.Create(AOwner: TObject);
begin
  FOwner := AOwner;
  CurrentCell := nil;
  FCells := TSerieCells.Create;
end;

destructor TLayout.Destroy;
var
  lCell: TSerieCell;
begin
  for lCell in FCells do
  begin
    FreeAndNil(lCell);
  end;
  FCells.Free;
  inherited Destroy;
end;

procedure TLayout.CreateLayout(ALayoutDefinition: TLayoutDefinition);
var
  lX: Integer;
  lY: Integer;
  lCell: TSerieCell;
  lImageCell: TImageCell;
  I: Integer;

begin
  FCells.Clear;

  for I := 0 to Length(ALayoutDefinition) - 1 do
  begin
    lCell := TSerieCell.Create(Self);
    lCell.Left :=  ALayoutDefinition[I].Left;
    lCell.Top := ALayoutDefinition[I].Top;
    lCell.Width:= ALayoutDefinition[I].width;
    lCell.Height:= ALayoutDefinition[I].height;
    lImageCell := TImageCell.Create(lCell);
    lImageCell.Left := 0;
    lImageCell.Top := 0;
    lImageCell.Width := 1;
    lImageCell.Height := 1;
    lCell.ImageCells.Add(lImageCell);
    FCells.Add(lCell);
  end;
end;

procedure TLayout.Draw(AViewerCanvas: TViewerCanvas);
var
  lCell: TSerieCell;
  lImageType: string;
  lSerie: TSerie;
  lScout: TScout;
  lCurrentImage: TImage;
  lRect: TBasicCellRect;

begin
  (* Se calcula el localizer *)
  for lCell in FCells do
  begin
    (* En el caso de CT, buscamos el Localizer *)
    if lCell.ImageCells.Count = 0 then
      continue;

    if lCell.ImageCells[0].Image = nil then
      continue;

    if CurrentCell = nil then
      continue;

    lCurrentImage := (CurrentCell as TSerieCell).ImageCells[0].Image;
    if lCurrentImage = nil then
      continue;

    if (lCell.ImageCells[0].Image.Modality = 'CT') or (lCell.ImageCells[0].Image.Modality = 'MR') then
    begin
      lImageType := lCell.ImageCells[0].Image.ImageType;
      if lImageType = '' then
        continue;
      lSerie := (lCell.ImageCells[0].Image.Serie as TSerie);
      if (lSerie.Images.Count > 0) then //and ((lImageType = 'LOCALIZER') or (lImageType = 'MPR')) then
      begin
        (* Encontrado el Localizer, se dibuja el SCOUT LINE *)
        lScout := TScout.Create(lCurrentImage, lCell.ImageCells[0].Image);
        try
          if lScout.DoCalcs then
          begin
            lSerie.ScoutColPixel := lScout.ColPixel;
            lSerie.ScoutRowPixel := lScout.RowPixel;
          end;
        finally
          lScout.Free;
        end;
      end;
    end;
  end;

  // se dibujan celdas de series
  if FCells.Count = 0 then
  begin
    AViewerCanvas.AggCanvas2D.FillColor($AA, $BB, $CC);
    AViewerCanvas.AggCanvas2D.Rectangle(0, 0, Width, Height);
  end
  else
  for lCell in FCells do
  begin
    if lCell = CurrentCell then
      lCell.Draw(AViewerCanvas, True)
    else
      lCell.Draw(AViewerCanvas, False);

    //lRect := lCell.GetRect;

    // se dibuja el recuadro del layout
    //AViewerCanvas.AggCanvas2D.LineColor($00, $FF, $FF);
    //AViewerCanvas.AggCanvas2D.Rectangle(lRect.left, lRect.Top, lRect.Right, lRect.Bottom);
  end;
  AViewerCanvas.CanvasImage.UpdateImage;
end;

procedure TLayout.SetCurrentCell(AX, AY: Integer);
var
  lCell: TSerieCell;
  lRect: TBasicCellRect;
begin
  for lCell in FCells do
  begin
    // se dibuja el recuadro de la celda
    lRect := lCell.GetRect;
    if (AX >= lRect.Left) and (AX <= lRect.Left + (lRect.Right - lRect.Left)) then
      if (AY >= lRect.Top) and (AY <= lRect.Top + (lRect.Bottom - lRect.Top)) then
      begin
        CurrentCell := lCell;
        lCell.SetCurrentImageCell(AX, AY);
        if Assigned(FOnRepaint) then
          FOnRepaint(Self);
        Break;
      end;
  end;
end;

procedure TLayout.SetAllChanged;
var
  lCell: TSerieCell;
begin
  for lCell in FCells do
    lCell.SetAllChanged;
end;

procedure TLayout.DeleteSelectedVector;
begin
  if CurrentImageCell <> nil then
  begin
    (CurrentImageCell as TImageCell).DeleteCurrentVector;
  end;
end;

procedure TLayout.ApplyHangingProtocol(AHangingProtocols: THangingProtocols);
var
  lHp: THangingProtocol;
  lHPCell: THangingProtocolCell;
  lSeriesFound: Boolean;

  procedure CreateOneCell(AHPCell: THangingProtocolCell);
  var
    lCell: TSerieCell;
    lImageCell: TImageCell;

  begin
    lCell := TSerieCell.Create(Self);
    lCell.Left:= AHPCell.Left;
    lCell.Top:= AHPCell.Top;
    lCell.Width:= AHPCell.Width;
    lCell.Height:= AHPCell.Height;
    lCell.Serie := AHPCell.FindSerie(FSeries);
    lImageCell := TImageCell.Create(lCell);
    lImageCell.Left := 0;
    lImageCell.Top := 0;
    lImageCell.Width := 1;
    lImageCell.Height := 1;
    lImageCell.RightScale := AHPCell.HangingProtocol.RightScale;
    lCell.ImageCells.Add(lImageCell);
    FCells.Add(lCell);
  end;

begin
  FCells.Clear;
  CurrentCell := nil;
  CurrentImageCell := nil;

  lSeriesFound:= False;
  lHp := AHangingProtocols.Current;
  if lHp = nil then
  begin
    lHp := THangingProtocol.Create(AHangingProtocols);
    lHPCell := THangingProtocolCell.Create;
    lHPCell.Left:= 0;
    lHPCell.Top:= 0;
    lHPCell.Width:= 1;
    lHPCell.Height:= 1;
    lHPCell.HangingProtocol := lHp;
    lHP.Cells.Add(lHPCell);
    CreateOneCell(lHpCell);
    AHangingProtocols.Current := lHp;
  end
  else
  begin
    for lHPCell in lHp.Cells do
    begin
      lHPCell.HangingProtocol := lHp;
      CreateOneCell(lHPCell);
      lSeriesFound := lSeriesFound or FCells.HasImages;
    end;
    if not lSeriesFound then
    begin
      (* Si no se encontraron imagenes y se fue cambiando de h.p. hasta
         que se llegó al último, entonces se setea el 1er H.P. *)
      if AHangingProtocols.Current <> AHangingProtocols.Items[AHangingProtocols.Count - 1] then
      begin
        AHangingProtocols.Next;
        ApplyHangingProtocol(AHangingProtocols);
      end
      else
        AHangingProtocols.Current := AHangingProtocols.Items[0];
    end;
  end;
end;

procedure TLayout.ToBmp(AWidth, AHeight: Integer);
var
  lCanvas: TViewerCanvas;
  lBmp: TFPMemoryImage;
  lWidth: Integer;
  lHeight: Integer;
  lFile: string;
begin
  lWidth := AWidth;
  lHeight := AHeight;
  if TdlgBitmapPrint.Execute(lWidth, lHeight, lFile) then
  begin
    lBmp := TFPMemoryImage.create(lWidth, lHeight);
    lCanvas := TViewerCanvas.Create(72);
    try
      lCanvas.Allocate(lBmp.Width, lBmp.Height);
      DrawToCanvas(lCanvas);
      FpgImageToFPImage(lCanvas.CanvasImage, lBmp);
      lBmp.SaveToFile(lFile);
    finally
      lCanvas.Free;
      lBmp.Free;
    end;
  end;
end;

procedure TLayout.ToPrinter;
var
  lCanvas: TViewerCanvas;

begin
  if TdlgPaperPrint.Execute then
  begin
    lCanvas := TViewerCanvas.Create(Printer.XDPI);
    try
      lCanvas.Allocate(Printer.PageWidth, Printer.PageHeight);
      DrawToCanvas(lCanvas);
      ToPrinter(lCanvas.CanvasImage);

      //lBmp := TFPMemoryImage.create(lCanvas.CanvasImage.Width, lCanvas.CanvasImage.Height);
      //FpgImageToFPImage(lCanvas.CanvasImage, lBmp);

      //lBmp.SaveToFile(ExtractFilePath(ParamStr(0)) + 'salida.jpeg');
      //lBmp.Free;

    finally
      lCanvas.Free;
    end;
  end;
end;

procedure TLayout.ToPrinter(AImage: TfpgImage);
var
  Y: Integer;
  X: Integer;
  lScanLine: PByte;
  lBitmap: TBitmap;

  procedure SetScanLinePixel(x,y:integer; c:longword; var surface:TBitmap);
  begin
    { this typecast makes X add by multiples of 3 }
    PLongWord(NativeInt(surface.Scanline[y]) + x * 4)^:= c;
  end;

begin
  lBitmap := TBitmap.Create;
  try
    lBitmap.Width := AImage.Width;
    lBitmap.Height := AImage.Height;
    for Y := 0 to lBitmap.Height - 1 do
    begin
      for X := 0 to lBitmap.Width - 1 do
      begin
        SetScanLinePixel(X, Y, AImage.Colors[X, Y], lBitmap);
      end;
    end;
    Printer.BeginDoc;
    Printer.Canvas.Draw(0, 0, lBitmap);
    Printer.EndDoc;
  finally
    lBitmap.Free;
  end;
end;

procedure TLayout.DrawToCanvas(ACanvas: TViewerCanvas);
var
  lFontSize: double;
  lCell: TSerieCell;
  lOrigPixelWidth: Integer;
  lOrigPixelHeight: Integer;
  lImageCell: TImageCell;

begin
  // para hoja A4 se usa fuente 24
  lFontSize := ACanvas.CanvasImage.Height * 24/3300;

  ACanvas.AggCanvas2D.Font(utils.GetFont, lFontSize);

  // se setean los nuevos valores
  for lCell in FCells do
  begin
    lOrigPixelWidth := lCell.PixelWidth;
    lOrigPixelHeight := lCell.PixelHeight;
    lCell.PixelWidth := ACanvas.CanvasImage.Width;
    lCell.PixelHeight := ACanvas.CanvasImage.Height;
    for lImageCell in lCell.ImageCells do
    begin
      if lImageCell.Image <> nil then
      begin
        lImageCell.ZoomToFit;
        lImageCell.AlignByModality;
      end;
    end;
  end;
  SetAllChanged;

  Draw(ACanvas);

  // se vuelve todo a la normalidad
  for lCell in FCells do
  begin
    lCell.PixelWidth := lOrigPixelWidth;
    lCell.PixelHeight := lOrigPixelHeight;
    for lImageCell in lCell.ImageCells do
    begin
      if lImageCell.Image <> nil then
      begin
        lImageCell.ZoomToFit;
        lImageCell.AlignByModality;
      end;
    end;
  end;
  SetAllChanged;
end;

procedure TLayout.SetSeries(AValue: TSeries);
var
  I: Integer;
  lCell: TSerieCell;
begin
  FSeries := AValue;
  for I := 0 to FCells.Count - 1 do
  begin
    lCell := FCells[I];
    if FSeries.Count > I then
      lCell.Serie := FSeries[I];
  end;
  SetAllChanged;
end;

end.

