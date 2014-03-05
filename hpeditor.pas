unit hpeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base,
  fpg_main,
  fpg_label,
  fpg_form,
  fpg_edit,
  fpg_checkbox,
  fpg_panel,
  fpg_memo,
  fpg_button,
  seriecell,
  hangingprotocol;

type

  { TgprLayout }

  { TPanelLayout }

  TPanelLayout = class(TfpgPanel)
  private
    FOnSelectCell: TNotifyEvent;
    FSelectedCell: THangingProtocolCell;
    function PointInLayout(APoint: TPoint): THangingProtocolCell;
    procedure Paint(Sender: TObject);
    procedure MouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure SetSelectedCell(AValue: THangingProtocolCell);
  public
    constructor Create(AOwner: TComponent); override;
    property SelectedCell: THangingProtocolCell read FSelectedCell write SetSelectedCell;
    property OnSelectCell: TNotifyEvent read FOnSelectCell write FOnSelectCell;
  end;

  { THPEditor }

  THPEditor = class(TfpgForm)
  private
    FlblModality: TfpgLabel;
    FedtProtocolName: TfpgEdit;
    FHangingProtocol: THangingProtocol;
    FgrpLayout: TfpgGroupBox;
    FgrpOverlays: TfpgGroupBox;
    FPanelLayout: TPanelLayout;
    FchkRightScale: TfpgCheckBox;
    FMemo: TfpgMemo;
    FGrpConditions: TfpgGroupBox;
    FPanelOverlays: TfpgPanel;
    FbtnPanel: TfpgPanel;
    FbtnOk: TfpgButton;
    FbtnCancel: TfpgButton;
    procedure Initialize;
    procedure SaveData;
    procedure SelectCell(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure edtProtocolNameChange(Sender: TObject);
    procedure SaveCellConditions(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute(AHangingProtocol: THangingProtocol): boolean;
  end;

implementation

{ TgprLayout }

function TPanelLayout.PointInLayout(APoint: TPoint): THangingProtocolCell;
var
  lCell: THangingProtocolCell;
  lForm: THPEditor;
  lWidth: Integer;
  lHeight: Integer;
  lLeft: Integer;
  lTop: Integer;
  lRect: TfpgRect;

begin
  Result := nil;
  lForm := Owner.Owner as THPEditor;
  if Assigned(lForm.FHangingProtocol) then
  begin
    lWidth := round(Width * 0.8);
    lHeight := round(Height * 0.8);
    lLeft := round((Width - lWidth) / 2);
    lTop := round((Height - lHeight) / 2);
    for lCell in lForm.FHangingProtocol.Cells do
    begin
      lRect.Left := (round(lCell.Left * lWidth) + 2) + lLeft;
      lRect.Top := (round(lCell.Top * lHeight) + 2) + lTop;
      lRect.Width := round(lCell.Width * lWidth) - 2;
      lRect.Height:= round(lCell.Height * lHeight) - 2;
      if (APoint.X > lRect.Left) and
       (APoint.Y > lRect.Top) and
       (APoint.X < lRect.Left + lRect.Width) and
       (APoint.Y < lRect.Top + lRect.Height) then
       begin
         Result := lCell;
         Break;
       end;
    end;
  end;
end;

procedure TPanelLayout.Paint(Sender: TObject);
var
  lCell: TSerieCell;
  lForm: THPEditor;
  lWidth: Integer;
  lHeight: Integer;
  lLeft: Integer;
  lTop: Integer;
  lPos: Integer;
  lTextLeft: Integer;
  lTextTop: Integer;
  lTextWidth: Integer;
begin
  lForm := Owner.Owner as THPEditor;
  if Assigned(lForm.FHangingProtocol) then
  begin
    lWidth := round(Width * 0.8);
    lHeight := round(Height * 0.8);
    lLeft := round((Width - lWidth) / 2);
    lTop := round((Height - lHeight) / 2);
    lPos := 1;
    for lCell in lForm.FHangingProtocol.Cells do
    begin
      if FSelectedCell = lCell then
        Canvas.SetColor($666666)
      else
        Canvas.SetColor($333333);
      Canvas.FillRectangle(
        (round(lCell.Left * lWidth) + 2) + lLeft,
        (round(lCell.Top * lHeight) + 2) + lTop,
        round(lCell.Width * lWidth) - 2,
        round(lCell.Height * lHeight) - 2
        );
      lTextWidth := Canvas.Font.TextWidth(IntToStr(lPos));
      lTextLeft := (round(lCell.Left * lWidth) + 2) + lLeft;
      lTextLeft := round((((lCell.Width * lWidth) - 2) / 2) - (lTextWidth / 2)) + lTextLeft;
      lTextTop := (round(lCell.Top * lHeight) + 2) + lTop;
      lTextTop := round((((lCell.Height * lHeight) - 2) / 2) - (Canvas.Font.Height / 2)) + lTextTop;
      Canvas.TextColor:= clWhite;
      Canvas.DrawText(lTextLeft, lTextTop, Format('%d', [lPos]));
      inc(lPos);
    end;
  end;
end;

procedure TPanelLayout.MouseDown(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if AButton = mbLeft then
  begin
    SelectedCell := PointInLayout(AMousePos);
    invalidate;
  end;
end;

procedure TPanelLayout.SetSelectedCell(AValue: THangingProtocolCell);
begin
  FSelectedCell:=AValue;
  if Assigned(FOnSelectCell) then
    FOnSelectCell(Self);
end;

constructor TPanelLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnPaint := @Paint;
  OnMouseDown:= @MouseDown;
  FSelectedCell := nil;
end;

{ THPEditor }

procedure THPEditor.Initialize;
begin
  FlblModality.Text := FHangingProtocol.Owner.Modality;
  FedtProtocolName.Text:= FHangingProtocol.Name;
  FPanelLayout.SelectedCell := FHangingProtocol.Cells[0];
  FPanelLayout.Invalidate;
  FchkRightScale.Checked:= FHangingProtocol.RightScale;
end;

procedure THPEditor.SaveData;
begin
  FHangingProtocol.RightScale:=FchkRightScale.Checked;
end;

procedure THPEditor.SelectCell(Sender: TObject);
var
  lCellCondition: TCellCondition;
begin
  if FPanelLayout.SelectedCell <> nil then
  begin
    // se muestran las condiciones que debe
    // cumplir la imágen para ubicarse en esta
    // celda.
    FMemo.Lines.Clear;
    for lCellCondition in FPanelLayout.SelectedCell.CellConditions do
    begin
      FMemo.Lines.Add(lCellCondition.TagPath + '|' + lCellCondition.Value);
    end;
  end;
end;

procedure THPEditor.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure THPEditor.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure THPEditor.edtProtocolNameChange(Sender: TObject);
begin
  FHangingProtocol.Name:= FedtProtocolName.Text;
end;

procedure THPEditor.SaveCellConditions(Sender: TObject);
begin
  // Primero que nada se guarda el contenido actual del memo
  FPanelLayout.SelectedCell.CellConditions.SaveConditions(FMemo.Lines);
end;

constructor THPEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'MainForm';
  FWidth:= 440;
  FHeight:= 430;
  Sizeable := False;
  WindowPosition := wpScreenCenter;
  WindowTitle := 'Hanging Protocol Editor';

  FbtnPanel := TfpgPanel.Create(Self);
  with FbtnPanel do
  begin
    Align:= alBottom;
    Anchors := [anRight,anBottom];
    Text:= '';
    Height:= 30;
    Style:=bsFlat;
  end;

  FbtnOk := TfpgButton.Create(FbtnPanel);
  with FbtnOk do
  begin
    Name := 'btnOk';
    Parent := FbtnPanel;
    Align := alRight;
    Anchors := [anRight,anBottom];
    Text := 'Ok';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.Ok';
    TabOrder := 0;
    OnClick := @btnOkClick;
  end;

  FbtnCancel := TfpgButton.Create(FbtnPanel);
  with FbtnCancel do
  begin
    Parent := FbtnPanel;
    Name := 'btnCancel';
    Align := alRight;
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.Cancel';
    TabOrder := 0;
    OnClick := @btnCancelClick;
  end;

  with TfpgLabel.Create(Self) do
  begin
    text := 'Modality:';
    Alignment := taRightJustify;
    width := 100;
    top := 10;
    left := 20;
  end;

  FlblModality := TfpgLabel.Create(Self);
  with FlblModality do
  begin
    Alignment := taLeftJustify;
    width := 100;
    top := 10;
    left := 130;
  end;

  with TfpgLabel.Create(Self) do
  begin
    text := 'Protocol Name:';
    Alignment := taRightJustify;
    width := 100;
    top := 30;
    left := 20;
  end;

  FedtProtocolName := TfpgEdit.Create(Self);
  with FedtProtocolName do
  begin
    Top := 30;
    Left := 130;
    Width := 290;
    Text := '';
    OnChange:= @edtProtocolNameChange;
  end;

  FGrpConditions := TfpgGroupBox.Create(Self);
  with FGrpConditions do
  begin
    Align := alBottom;
    Height := 100;
    Text := 'Conditions';
  end;

  FMemo := TfpgMemo.Create(FGrpConditions);
  with FMemo do
  begin
    Align := alClient;
    Lines.Clear;
    OnExit:=@SaveCellConditions;
  end;

  FgrpOverlays := TfpgGroupBox.Create(Self);
  with FgrpOverlays do
  begin
    Align := alBottom;
    Height := 60;
    Text := 'Overlays';
  end;

  FchkRightScale := TfpgCheckBox.Create(FgrpOverlays);
  with FchkRightScale do
  begin
    Align := alClient;
    Height := 30;
    Text:= 'Show right scale';
    Checked := True;
  end;

  FgrpLayout := TfpgGroupBox.Create(Self);
  with FgrpLayout do
  begin
    Align := alBottom;
    Height := 180;
    Text := 'Layout';
  end;

  FPanelLayout := TPanelLayout.Create(FgrpLayout);
  with FPanelLayout do
  begin
    align := alClient;
    Style:= bsLowered;
    Text:= '';
    BackgroundColor:=clDarkGray;
    OnSelectCell:= @SelectCell;
  end;
end;

destructor THPEditor.Destroy;
begin
  inherited Destroy;
end;

class function THPEditor.Execute(AHangingProtocol: THangingProtocol): boolean;
begin
  with THPEditor.Create(nil) do
  begin
    FHangingProtocol := AHangingProtocol;
    Initialize;
    Result := ShowModal = mrOK;
    if Result then
    begin
      SaveData;
    end;
    Free;
  end;
end;

end.

