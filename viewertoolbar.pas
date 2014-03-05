unit viewertoolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_panel, fpg_button, fpg_widget,
  Agg2D,
  fpg_imgfmt_png,
  FPimage,
  FPReadPNG,
  utils,
  viewertypes;

type

  { TViewerToolBar }
  TOnModeChange = procedure(AOldMode, ANewMode: TViewerMode) of object;

  TViewerButton = class;

  TViewerToolBar = class(TfpgBevel)
  private
    FbtnWindowLevel: TViewerButton;
    FbtnZoom: TViewerButton;
    FbtnPan: TViewerButton;
    FbtnOpenFile: TViewerButton;
    FbtnLayout: TViewerButton;
    FbtnRuler: TViewerButton;
    FbtnSelect: TViewerButton;
    FbtnMagnify: TViewerButton;
    FbtnHPDesigner: TViewerButton;
    FOnAfterModeChange: TOnModeChange;
    FAgg2d: TAgg2D;
    FInternalImage: TfpgImage;
    procedure BtnClick(Sender: TObject);
    procedure Draw;
  protected
    procedure Paint(Sender: TObject);
    procedure AfterButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetMode(AMode: TViewerMode);
    procedure EnableImageButtons;
    property btnWindowLevel: TViewerButton read FbtnWindowLevel write FbtnWindowLevel;
    property OnAfterModeChange: TOnModeChange read FOnAfterModeChange write FOnAfterModeChange;
  end;

  { TViewerButton }

  TViewerButton = class(TfpgButton)
  private
    FImageFile: string;
    FMouseIn: Boolean;
    FOnAfterClick: TNotifyEvent;
    FSelected: Boolean;
    FMode: TViewerMode;
    FAgg2d: TAgg2D;
    FInternalImage: TfpgImage;
    FPGImage: TfpgImage;
    procedure SetImageFile(AValue: string);
    procedure Draw;
  protected
    procedure Paint(Sender: TObject);
    procedure MouseEnter(Sender: TObject);
    procedure MouseLeave(Sender: TObject);
    procedure MouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure AfterClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ImageFile: string read FImageFile write SetImageFile;
    property Selected: Boolean read FSelected write FSelected;
    property Mode: TViewerMode read FMode write FMode;
    property OnAfterClick: TNotifyEvent read FOnAfterClick write FOnAfterClick;
  end;

implementation

{ TViewerButton }

constructor TViewerButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnPaint:=@Paint;
  OnMouseEnter:= @MouseEnter;
  OnMouseExit:= @MouseLeave;
  OnMouseDown:= @MouseDown;
  FAgg2d := TAgg2D.Create(nil);
  FInternalImage := TfpgImage.Create;
  FMouseIn := False;
  FSelected := False;
  Enabled := True;
  ShowHint:= True;
  Width := 60;
end;

destructor TViewerButton.Destroy;
begin
  FAgg2d.Free;
  FInternalImage.Free;
  FPGImage.Free;
  inherited Destroy;
end;

procedure TViewerButton.SetImageFile(AValue: string);
begin
  if FImageFile=AValue then Exit;
  FImageFile:=AValue;
  FPGImage := LoadImage_PNG(FImageFile);
end;

procedure TViewerButton.Draw;
var
  lTextWidth: Integer;
  lTextHeight: Integer;
  lTop: Integer;
  lLeft: Integer;
  c1: TAggColor;
  c2: TAggColor;

begin
  if (FInternalImage.Width <> Width) or (FInternalImage.Height <> Height) then
  begin
    FInternalImage.AllocateImage(32, Width, height);
    FInternalImage.UpdateImage;
    FAgg2d.Attach(FInternalImage);
  end;
  FAgg2d.NoLine;


  FAgg2d.MasterAlpha(1);
  FAgg2d.ClearAll($ff, $ff, $ff);

  c1.Construct(62 , 62, 62);
  c2.Construct(28 , 28, 28);
  FAgg2d.NoLine;
  FAgg2d.FillLinearGradient(0, 0, 0, Height, c1, c2, 1);
  FAgg2d.Rectangle(0, 0, Width, Height);

  lLeft:= Round((Width / 2) - (FPGImage.Width / 2));
  lTop := 4;

  if FSelected then
  begin
    lLeft := lLeft + 2;
    lTop := lTop + 2;
    c1.Construct(22 , 22, 22);
    c2.Construct(58 , 58, 58);
    FAgg2d.FillLinearGradient(0, 0, 0, Height, c1, c2, 1);
    FAgg2d.Rectangle(0, 0, Width, Height);
  end;

  if Enabled then
  begin
    FAgg2d.MasterAlpha(0.5);
    if FMouseIn then
      FAgg2d.MasterAlpha(1)
    else
      FAgg2d.MasterAlpha(0.5);
  end
  else
    FAgg2d.MasterAlpha(0.2);

  FAgg2d.DrawImage(lLeft, lTop, FPGImage);

  FAgg2d.Font(utils.GetFont, utils.mFontSize);
  lTextWidth := Round(FAgg2d.TextWidth(Self.Text));
  lTextHeight := Round(FAgg2d.FontHeight);

  lLeft := Round((Width / 2) - (lTextWidth / 2));
  lTop := Height - lTextHeight + 2;

  if FSelected then
  begin
    lLeft := lLeft + 2;
    lTop := lTop + 2;
  end;

  FAgg2d.FillColor($5E, $5E, $5E);
  FAgg2d.Text(lLeft + 0.1, lTop + 0.1, Self.Text);
  FAgg2d.FillColor($CD, $CD, $CD);
  FAgg2d.Text(lLeft, lTop, Self.Text);
  FInternalImage.UpdateImage;
end;

procedure TViewerButton.Paint(Sender: TObject);
begin
  Draw;
  Canvas.DrawImage(0, 0, FInternalImage);
end;

procedure TViewerButton.MouseEnter(Sender: TObject);
begin
  FMouseIn := True;
  invalidate;
end;

procedure TViewerButton.MouseLeave(Sender: TObject);
begin
  FMouseIn := False;
  invalidate;
end;

procedure TViewerButton.MouseDown(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if AButton = mbLeft then
  begin
    AfterClick;
  end;
end;

procedure TViewerButton.AfterClick;
begin
  if Assigned(FOnAfterClick) then
    FOnAfterClick(Self);
end;

{ TViewerToolBar }

procedure TViewerToolBar.BtnClick(Sender: TObject);
begin
  if (Sender as TViewerButton).Selected then
  begin
    (Sender as TViewerButton).Selected := False;
    (Sender as TViewerButton).Invalidate;
    if Assigned(FOnAfterModeChange) then
      FOnAfterModeChange( (Sender as TViewerButton).Mode, vmNone );
  end
  else
  begin
    (Sender as TViewerButton).Selected := True;
    (Sender as TViewerButton).Invalidate;
    if Assigned(FOnAfterModeChange) then
      FOnAfterModeChange( vmNone, (Sender as TViewerButton).Mode );
  end;

  (Sender as TViewerButton).AfterClick;
end;

procedure TViewerToolBar.Draw;
var
  c1: TAggColor;
  c2: TAggColor;

begin
  if (FInternalImage.Width <> Width) or (FInternalImage.Height <> Height) then
  begin
    FInternalImage.AllocateImage(32, Width, height);
    FInternalImage.UpdateImage;
    FAgg2d.Attach(FInternalImage);
  end;

  FAgg2d.ClearAll($FF, $FF, $FF);
  c1.Construct(62 , 62, 62);
  c2.Construct(28 , 28, 28);
  FAgg2D.NoLine;
  FAgg2D.FillLinearGradient(0, 0, 0, Height, c1, c2, 1);
  FAgg2D.Rectangle(0, 0, Width, Height);
  FInternalImage.UpdateImage;
end;

procedure TViewerToolBar.Paint(Sender: TObject);
begin
  Draw;
  Canvas.DrawImage(0, 0, FInternalImage);
end;

procedure TViewerToolBar.AfterButtonClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
  begin
    if (Components[I] is TViewerButton) and (Components[I] <> Sender) then
    begin
      TViewerButton(Components[I]).Selected := False;
      TViewerButton(Components[I]).Invalidate;
    end;
  end;
end;

constructor TViewerToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnPaint:=@Paint;
  FInternalImage := TfpgImage.Create;
  FAgg2D := TAgg2D.Create(nil);
  Align := alTop;
  Self.Height := 60;

  // open file button
  FbtnOpenFile := TViewerButton.Create(Self);
  FbtnOpenFile.Align:= alLeft;
  FbtnOpenFile.ImageLayout := ilImageTop;
  FbtnOpenFile.ImageFile:= 'icons' + DirectorySeparator + 'openfile.png';
  FbtnOpenFile.Flat:= True;
  FbtnOpenFile.Embedded:= True;
  FbtnOpenFile.Text:= 'Open';
  FbtnOpenFile.OnAfterClick:= @AfterButtonClick;
  FbtnOpenFile.OnClick:= @BtnClick;
  FbtnOpenFile.Mode:= vmOpenFile;
  FbtnOpenFile.Enabled:= True;
  FbtnOpenFile.Hint:= 'Open File (ctrl + o)';

  // window level button
  FbtnWindowLevel := TViewerButton.Create(Self);
  FbtnWindowLevel.Align:= alLeft;
  FbtnWindowLevel.ImageLayout := ilImageTop;
  FbtnWindowLevel.ImageFile:= 'icons' + DirectorySeparator + 'wl.png';
  FbtnWindowLevel.Flat:= True;
  FbtnWindowLevel.Embedded:= True;
  FbtnWindowLevel.Text:= 'W/L';
  FbtnWindowLevel.OnAfterClick:= @AfterButtonClick;
  FbtnWindowLevel.OnClick:= @BtnClick;
  FbtnWindowLevel.Mode:= vmWL;
  FbtnWindowLevel.Enabled:= False;
  FbtnWindowLevel.Hint := 'Window / Level (w)';

  // zoom button
  FbtnZoom := TViewerButton.Create(Self);
  FbtnZoom.Align:= alLeft;
  FbtnZoom.ImageLayout := ilImageTop;
  FbtnZoom.ImageFile:= 'icons' + DirectorySeparator + 'zoom.png';
  FbtnZoom.Flat:= True;
  FbtnZoom.Embedded:= True;
  FbtnZoom.Text:= 'Zoom';
  FbtnZoom.OnAfterClick:= @AfterButtonClick;
  FbtnZoom.OnClick:= @BtnClick;
  FbtnZoom.Mode:= vmZoom;
  FbtnZoom.Enabled:= False;
  FbtnZoom.Hint := 'Zoom (z)';

  // pan button
  FbtnPan := TViewerButton.Create(Self);
  FbtnPan.Align:= alLeft;
  FbtnPan.ImageLayout := ilImageTop;
  FbtnPan.ImageFile:= 'icons' + DirectorySeparator + 'pan.png';
  FbtnPan.Flat:= True;
  FbtnPan.Embedded:= True;
  FbtnPan.Text:= 'Pan';
  FbtnPan.OnAfterClick:= @AfterButtonClick;
  FbtnPan.OnClick:= @BtnClick;
  FbtnPan.Mode:= vmPan;
  FbtnPan.Enabled:= False;
  FbtnPan.Hint := 'Pan (p)';

  // Select button
  FbtnSelect := TViewerButton.Create(Self);
  FbtnSelect.Align:= alLeft;
  FbtnSelect.ImageLayout := ilImageTop;
  FbtnSelect.ImageFile:= 'icons' + DirectorySeparator + 'select.png';
  FbtnSelect.Flat:= True;
  FbtnSelect.Embedded:= True;
  FbtnSelect.Text:= 'Select';
  FbtnSelect.OnAfterClick:= @AfterButtonClick;
  FbtnSelect.OnClick:= @BtnClick;
  FbtnSelect.Mode:= vmSelect;
  FbtnSelect.Enabled:= False;
  FbtnSelect.Hint := 'Select';


  // Ruler button
  FbtnRuler := TViewerButton.Create(Self);
  FbtnRuler.Align:= alLeft;
  FbtnRuler.ImageLayout := ilImageTop;
  FbtnRuler.ImageFile:= 'icons' + DirectorySeparator + 'ruler.png';
  FbtnRuler.Flat:= True;
  FbtnRuler.Embedded:= True;
  FbtnRuler.Text:= 'Measure';
  FbtnRuler.OnAfterClick:= @AfterButtonClick;
  FbtnRuler.OnClick:= @BtnClick;
  FbtnRuler.Mode:= vmRuler;
  FbtnRuler.Enabled:= False;
  FbtnRuler.Hint := 'Measure';

  // Magnify button
  FbtnMagnify := TViewerButton.Create(Self);
  FbtnMagnify.Align:= alLeft;
  FbtnMagnify.ImageLayout := ilImageTop;
  FbtnMagnify.ImageFile:= 'icons' + DirectorySeparator + 'magnify.png';
  FbtnMagnify.Flat:= True;
  FbtnMagnify.Embedded:= True;
  FbtnMagnify.Text:= 'Magnify';
  FbtnMagnify.OnAfterClick:= @AfterButtonClick;
  FbtnMagnify.OnClick:= @BtnClick;
  FbtnMagnify.Mode:= vmMagnify;
  FbtnMagnify.Enabled:= False;
  FbtnMagnify.Hint := 'Magnify';

  // layout button
  FbtnLayout := TViewerButton.Create(Self);
  FbtnLayout.Align:= alLeft;
  FbtnLayout.ImageLayout := ilImageTop;
  FbtnLayout.ImageFile:= 'icons' + DirectorySeparator + 'layout.png';
  FbtnLayout.Flat:= True;
  FbtnLayout.Embedded:= True;
  FbtnLayout.Text:= 'Layout';
  FbtnLayout.OnAfterClick:= @AfterButtonClick;
  FbtnLayout.OnClick:= @BtnClick;
  FbtnLayout.Mode:= vmLayout;
  FbtnLayout.Enabled:= False;
  FbtnLayout.Hint := 'Layout';

  // Hanging Protocol Designer
  FbtnHPDesigner := TViewerButton.Create(Self);
  FbtnHPDesigner.Align:= alLeft;
  FbtnHPDesigner.ImageLayout := ilImageTop;
  FbtnHPDesigner.ImageFile:= 'icons' + DirectorySeparator + 'hpdesigner.png';
  FbtnHPDesigner.Flat:= True;
  FbtnHPDesigner.Embedded:= True;
  FbtnHPDesigner.Text:= 'H.P. Designer';
  FbtnHPDesigner.OnAfterClick:= @AfterButtonClick;
  FbtnHPDesigner.OnClick:= @BtnClick;
  FbtnHPDesigner.Mode:= vmHPDesigner;
  FbtnHPDesigner.Enabled:= False;
  FbtnHPDesigner.Width:= 80;
  FbtnHPDesigner.Hint := 'Hanging Protocol Designer';
end;

destructor TViewerToolBar.Destroy;
begin
  FAgg2d.Free;
  FInternalImage.Free;
  inherited Destroy;
end;

procedure TViewerToolBar.SetMode(AMode: TViewerMode);
begin
  case AMode of
     vmNone: AfterButtonClick(nil);
     vmHPDesigner: FbtnHPDesigner.OnClick(FbtnHPDesigner);
     vmZoom: FbtnZoom.OnClick(FbtnZoom);
     vmPan: FbtnPan.OnClick(FbtnPan);
     vmWL: FbtnWindowLevel.OnClick(FbtnWindowLevel);
     vmLayout: FbtnLayout.OnClick(FbtnLayout);
     vmRuler: FbtnRuler.OnClick(FbtnRuler);
     vmSelect: begin
       if not FbtnSelect.Selected then
         FbtnSelect.OnClick(FbtnSelect);
     end;
     vmMagnify: FbtnMagnify.OnClick(FbtnMagnify);
  end;
end;

procedure TViewerToolBar.EnableImageButtons;
begin
  FbtnPan.Enabled := True;
  FbtnZoom.Enabled := True;
  FbtnWindowLevel.Enabled := True;
  FbtnLayout.Enabled := True;
  FbtnRuler.Enabled:= True;
  FbtnSelect.Enabled:= True;
  FbtnMagnify.Enabled:= True;
  FbtnHPDesigner.Enabled:= True;
end;

end.

