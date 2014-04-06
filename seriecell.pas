unit seriecell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  libDicomUtilsWrapperDyn,
  math,
  fgl,
  fpg_main,
  fpg_base,
  ViewerCanvas,
  Agg2D,
  images,
  basiccell,
  imagecell,
  series,
  baselayout;

type

  { TSerieCell }

  TSerieCell = class(TBasicCell)
  private
    FImageCells: TImageCells;
    FOnNeedImage: TOnNeedImage;
    FSerie: TSerie;
    FFirstImageIndex: Integer;
    FPixelWidth: Integer;
    FPixelHeight: Integer;
    procedure SetSerie(AValue: TSerie);
    procedure NeedImage(AImageCell: TImageCell);
  public
    constructor Create(AOwner: TBaseLayout);
    destructor Destroy; override;
    function GetRect: TBasicCellRect;
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetAllChanged;
    procedure SetCurrentImageCell(AX, AY: Integer);
    procedure Draw(AViewerCanvas: TViewerCanvas; IsCurrentCell: boolean);
    procedure NextImage(AIncrement: Integer); override;
    procedure PriorImage(AIncrement: Integer); override;
    procedure AdjustImageCells(AIncrement: Integer);
    property PixelWidth: Integer read FPixelWidth write FPixelWidth;
    property PixelHeight: Integer read FPixelHeight write FPixelHeight;
    property DrawingRect: TBasicCellRect read GetRect;
    property ImageCells: TImageCells read FImageCells;
    property Serie: TSerie read FSerie write SetSerie;
    property FirstImageIndex: Integer read FFirstImageIndex write FFirstImageIndex;
    property OnNeedImage: TOnNeedImage read FOnNeedImage write FOnNeedImage;
  end;

  TSerieCellsSpecialization = specialize TFPGList<TSerieCell>;

  { TSerieCells }

  TSerieCells = class(TSerieCellsSpecialization)
  private
  public
    function HasImages: Boolean;
    procedure Clear;
  end;

implementation

{ TSerieCells }

function TSerieCells.HasImages: Boolean;
var
  lCell: TSerieCell;
begin
  Result := False;
  for lCell in Self do
  begin
    if (lCell.Serie <> nil) and (lCell.Serie.Images.Count > 0) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TSerieCells.Clear;
var
  lSerieCell: TSerieCell;
begin
  for lSerieCell in Self do
  begin
    lSerieCell.free;
  end;
  inherited Clear;
end;

{ TSerieCell }

function TSerieCell.GetRect: TBasicCellRect;
begin
  Result.Left   := Round(Left * FPixelWidth);
  Result.Top    := Round(Top * FPixelHeight);
  Result.Right  := Result.Left + Round(Width * FPixelWidth);
  Result.Bottom := Result.Top + Round(Height * FPixelHeight);
end;

procedure TSerieCell.SetSerie(AValue: TSerie);
begin
  if FSerie=AValue then Exit;
  FSerie:=AValue;
  FFirstImageIndex:= 0;
end;

procedure TSerieCell.NeedImage(AImageCell: TImageCell);
var
  lIndex: Integer;
  lWW: double;
  lWC: double;
  lZoom: double;
  lLut: TLut;

begin
  if FSerie = nil then
    exit;
  // obtenemos el index de AImageCell
  lIndex := Self.ImageCells.IndexOf(AImageCell);
  lIndex := lIndex + FFirstImageIndex;
  if FSerie.Images.Count > lIndex then
  begin
    AImageCell.Image := FSerie.Images[lIndex];
    AImageCell.Resample;
  end
  else
    AImageCell.Image := nil;
end;

constructor TSerieCell.Create(AOwner: TBaseLayout);
begin
  inherited Create;
  FImageCells := TImageCells.Create;
  FSerie := nil;
  FOnNeedImage := @NeedImage;
  FFirstImageIndex := 0;
  Owner := AOwner;
end;

destructor TSerieCell.Destroy;
begin
  FImageCells.Free;
  inherited Destroy;
end;

function TSerieCell.GetWidth: Integer;
var
  lRect: TBasicCellRect;
begin
  lRect := GetRect;
  Result := Round(lRect.Right - lRect.Left);
end;

function TSerieCell.GetHeight: Integer;
var
  lRect: TBasicCellRect;
begin
  lRect := GetRect;
  Result := Round(lRect.Bottom - lRect.Top);
end;

procedure TSerieCell.SetAllChanged;
var
  lCell: TImageCell;
begin
  for lCell in FImageCells do
    lCell.Changed:= True;
end;

procedure TSerieCell.SetCurrentImageCell(AX, AY: Integer);
var
  lImageCell: TImageCell;
  lRect: TBasicCellRect;
begin
  CurrentCell := nil;
  for lImageCell in FImageCells do
  begin
    // se dibuja el recuadro de la celda
    lRect := lImageCell.GetViewPortRect;
    if (AX >= lRect.Left) and (AX <= lRect.Left + (lRect.Right - lRect.Left)) then
      if (AY >= lRect.Top) and (AY <= lRect.Top + (lRect.Bottom - lRect.Top)) then
      begin
        (Self.Owner as TBaseLayout).CurrentImageCell := lImageCell;
        Break;
      end;
  end;
end;

procedure TSerieCell.Draw(AViewerCanvas: TViewerCanvas; IsCurrentCell: boolean);
var
  lCell: TImageCell;
  lLeft: Integer;
  lTop: Integer;
  lWidth: Integer;
  lHeight: Integer;
  lRect: TBasicCellRect;
  lCurrentImageCell: TBasicCell;
  lSameSerie: boolean;


begin
  FPixelWidth:= AViewerCanvas.CanvasImage.Width;
  FPixelHeight:= AViewerCanvas.CanvasImage.Height;
  lCurrentImageCell := (Owner as TBaseLayout).CurrentImageCell;
  lSameSerie := (Owner as TBaseLayout).CurrentCell = Self;

  // se dibujan las celdas que contienen las imágenes
  // para esta serie
  for lCell in FImageCells do
  begin
    if lCell <> lCurrentImageCell then
      lCell.Draw(AViewerCanvas, False, lSameSerie)
    else
      lCell.Draw(AViewerCanvas, True, lSameSerie);
  end;
end;

procedure TSerieCell.NextImage(AIncrement: Integer);
var
  lMultiplier: Integer;
begin
  if FSerie = nil then
    exit;

  lMultiplier := Self.ImageCells.Count;
  if lMultiplier = 1 then
    lMultiplier := AIncrement;
  if FSerie.Images.Count > FFirstImageIndex + lMultiplier then
  begin
    Inc(FFirstImageIndex, lMultiplier);
    SetAllChanged;
  end;
end;

procedure TSerieCell.PriorImage(AIncrement: Integer);
var
  lMultiplier: Integer;
begin
  if FSerie = nil then
    exit;

  lMultiplier := Self.ImageCells.Count;
  if lMultiplier = 1 then
    lMultiplier := AIncrement;
  if FFirstImageIndex - lMultiplier >= 0 then
  begin
    Dec(FFirstImageIndex, lMultiplier);
    SetAllChanged;
  end;
end;

procedure TSerieCell.AdjustImageCells(AIncrement: Integer);
var
  lActualValue: Integer;
  lImageCell: TImageCell;
  lRows: Integer;
  lCols: Integer;
  I: Integer;
  A: Integer;
  lLeft: double;
  lTop: double;
  lWidth: double;
  lHeight: double;
  lCell: TImageCell;
begin
  if (AIncrement = -1) and (ImageCells.Count = 1) then
    exit;

  if (AIncrement = 1) and (Round(sqrt(ImageCells.Count)) = 5) then
    exit;

  lActualValue := ImageCells.Count;
  for lCell in ImageCells do
    lCell.free;

  ImageCells.Clear;

  lRows := Round(sqrt(lActualValue)) + AIncrement;
  lCols := lRows;
  lWidth := 1 / lCols;
  lHeight := 1 / lRows;

  lTop := 0;
  for I := 0 to lRows - 1 do
  begin
    lLeft := 0;
    for A := 0 to lCols - 1 do
    begin
      lImageCell := TImageCell.Create(Self);
      lImageCell.Left := lLeft;
      lImageCell.Top := lTop;
      lImageCell.Width := lWidth;
      lImageCell.Height := lHeight;
      ImageCells.Add(lImageCell);
      lLeft := lLeft + lWidth;
    end;
    lTop:= lTop + lHeight;
  end;
end;

{ TSerieCell }

end.

