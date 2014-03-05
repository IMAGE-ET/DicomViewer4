unit hpdesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_main,
  fpg_base,
  fpg_panel,
  fpg_label,
  fpg_listbox,
  fpg_button,
  fpg_dialogs,
  commonmessages,
  Series,
  hangingprotocol,
  hpeditor,
  layout;

type

  { TfrmHangingProtocol }

  TfrmHangingProtocol = class(TfpgPanel)
  private
    FgrpProtocols: TfpgGroupBox;
    FgrpDetails: TfpgGroupBox;
    FlstProtocols: TfpgListBox;
    FpnlButtons: TfpgPanel;
    FHangingProtocols: THangingProtocols;
    FbtnEdit: TfpgButton;
    FbtnDelete: TfpgButton;
    FLayout: TLayout;
    procedure NewHP(Sender: TObject);
    procedure EditHP(Sender: TObject);
    procedure DeleteHP(Sender: TObject);
    procedure ListBoxSelect(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadHangingProtocols(ASeries: TSeries; ALayout: TLayout);
  end;

implementation

{ TfrmHangingProtocol }

procedure TfrmHangingProtocol.NewHP(Sender: TObject);
var
  lHP: THangingProtocol;
begin
  lHP := THangingProtocol.Create(FHangingProtocols);
  lHP.SetCellsConditions(FLayout.Cells);
  if THPEditor.Execute(lHP) then
  begin
    // Guardar cambios
    FlstProtocols.Items.AddObject(lHP.Name, lHp);
    FHangingProtocols.Add(lHP);
    FHangingProtocols.Save;
  end;
end;

procedure TfrmHangingProtocol.EditHP(Sender: TObject);
var
  lHP: THangingProtocol;
begin
  lHP := FlstProtocols.Items.Objects[FlstProtocols.FocusItem] as THangingProtocol;
  if THPEditor.Execute(lHP) then
  begin
    // Guardar cambios
    FlstProtocols.Items[FlstProtocols.FocusItem] := lHp.Name;
    FlstProtocols.Invalidate;
    FHangingProtocols.Save;
    fpgPostMessage(lHp, fpgApplication.MainForm, MSG_AFTERHPCHANGED);
  end;
end;

procedure TfrmHangingProtocol.DeleteHP(Sender: TObject);
var
  lHP: THangingProtocol;

begin
  if TfpgMessageDialog.Question('Delete reading protocol',
     'Delete selected reading protocol?', mbYesNo, mbNoButton) = mbYes then
  begin
    lHP := FlstProtocols.Items.Objects[FlstProtocols.FocusItem] as THangingProtocol;
    FHangingProtocols.Remove(lHp);
    FlstProtocols.Items.Delete(FlstProtocols.FocusItem);
    FlstProtocols.Invalidate;
    FHangingProtocols.Save;
  end;
end;

procedure TfrmHangingProtocol.ListBoxSelect(Sender: TObject);
begin
  FbtnEdit.Enabled:= True;
  FbtnDelete.Enabled:= True;
end;

constructor TfrmHangingProtocol.Create(AOwner: TComponent);
begin
  inherited;
  Name := 'HPDesigner';
  Width := 200;
  Height := 300;
  Style := bsFlat;
  Hint := '';
  Text := '';

  with TfpgLabel.Create(Self) do
  begin
    Text := 'Hanging Protocols Designer';
    BackgroundColor:=fpgColor($80,$80,$80);
    Align:= alTop;
    Layout:=tlCenter;
    Alignment:=taCenter;
    Height:= 24;
  end;

  FpnlButtons := TfpgPanel.Create(Self);
  with FpnlButtons do
  begin
    Align:=alTop;
    Text:= '';
    Height:= 24;
    Style := bsFlat;
  end;

  with TfpgButton.Create(FpnlButtons) do
  begin
    Align:= alLeft;
    Text:= 'New';
    Hint := 'New hanging protocol based on current layout.';
    Width:= 50;
    ShowHint:= True;
    OnClick:=@NewHP;
  end;

  FbtnEdit := TfpgButton.Create(FpnlButtons);
  with FbtnEdit do
  begin
    Align:= alLeft;
    Text:= 'Edit';
    Enabled := False;
    Width:= 50;
    Hint := 'Edit selected hanging protocol.';
    ShowHint:= True;
    OnClick:=@EditHP;
  end;

  FbtnDelete := TfpgButton.Create(FpnlButtons);
  with FbtnDelete do
  begin
    Align:= alLeft;
    Text:= 'Delete';
    Enabled := False;
    Width:= 50;
    Hint := 'Delete selected hanging protocol.';
    ShowHint:= True;
    OnClick:=@DeleteHP;
  end;

  FgrpProtocols := TfpgGroupBox.Create(Self);
  with FgrpProtocols do
  begin
    Text := 'Protocols for ... ';
    Align := alClient;
  end;

  FlstProtocols := TfpgListBox.Create(FgrpProtocols);
  with FlstProtocols do
  begin
    Parent := FgrpProtocols;
    Align := alClient;
    OnSelect := @ListBoxSelect;
    OnChange := @ListBoxSelect;
  end;

  FHangingProtocols := THangingProtocols.Create;
end;

destructor TfrmHangingProtocol.Destroy;
begin
  FHangingProtocols.Free;
  inherited Destroy;
end;

procedure TfrmHangingProtocol.LoadHangingProtocols(ASeries: TSeries; ALayout: TLayout);
var
  lModality: string;
  lHP: THangingProtocol;
begin
  FLayout := ALayout;
  lModality := ASeries[0].Images[0].Modality;
  FgrpProtocols.Text:= 'Protocols for (' + lModality + ')';
  FHangingProtocols.Modality := lModality;
  FHangingProtocols.Load;
  FlstProtocols.Items.Clear;
  for lHp in FHangingProtocols do
  begin
    FlstProtocols.Items.AddObject(lHP.Name, lHp);
  end;
end;

end.

