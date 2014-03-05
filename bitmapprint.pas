unit bitmapprint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_dialogs, fpg_button, fpg_panel,
  fpg_combobox, fpg_label, fpg_radiobutton,
  fpg_edit;

type

  { TdlgBitmapPrint }

  TdlgBitmapPrint = class(TfpgForm)
  private
    FPanel: TfpgPanel;
    FbtnOk: TfpgButton;
    FbtnCancel: TfpgButton;
    FlblWidth: TfpgLabel;
    FlblHeight: TfpgLabel;
    FlblFile: TfpgLabel;
    FedtWidth: TfpgEdit;
    FedtHeight: TfpgEdit;
    FedtFile: TfpgEdit;
    FbtnOpen: TfpgButton;
    FBmpWidth: Integer;
    FBmpHeight: Integer;
    FFile: string;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure Initialize;
    procedure Browse(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute(var AWidth, AHeight: Integer; out AFile: string): boolean;
  end;

implementation

{ TdlgBitmapPrint }

procedure TdlgBitmapPrint.BtnOkClick(Sender: TObject);
begin
  FBmpWidth := StrToIntDef(FedtWidth.Text, 100);
  FBmpHeight := StrToIntDef(FedtHeight.Text, 100);
  FFile:= FedtFile.Text;
  ModalResult := mrOK;
end;

procedure TdlgBitmapPrint.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TdlgBitmapPrint.Initialize;
begin
  FedtWidth.Text:= IntToStr(FBmpWidth);
  FedtHeight.Text:= IntToStr(FBmpHeight);
end;

procedure TdlgBitmapPrint.Browse(Sender: TObject);
var
  lDlfFile: TfpgFileDialog;
begin
  lDlfFile := TfpgFileDialog.Create(nil);
  try
    lDlfFile.Filter := 'JPeg Files (*.jpg)|*.jpeg;*.jpg';
    if lDlfFile.RunSaveFile then
    begin
      FedtFile.Text:= lDlfFile.FileName;
    end;
    lDlfFile.Close;
  finally
    lDlfFile.Free;
  end;
end;

constructor TdlgBitmapPrint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'dlgBitmapPrint';
  FWidth := 400;
  FHeight := 300;
  Sizeable := False;
  WindowAttributes := [];
  WindowPosition := wpAuto;
  WindowTitle := 'Bitmap Print';

  FlblWidth := TfpgLabel.Create(Self);
  with FlblWidth do
  begin
    Left := 10;
    Top := 10;
    Text:= 'Width:';
  end;

  FedtWidth := TfpgEdit.Create(Self);
  with FedtWidth do
  begin
    Left := 100;
    Top := 10;
    Width := 50;
    Text:= '';
  end;

  FlblHeight := TfpgLabel.Create(Self);
  with FlblHeight do
  begin
    Left := 10;
    Top := 40;
    Text:= 'Height:';
  end;

  FedtHeight := TfpgEdit.Create(Self);
  with FedtHeight do
  begin
    Left := 100;
    Top := 40;
    Width := 50;
    Text:= '';
  end;

  FlblFile := TfpgLabel.Create(Self);
  with FlblFile do
  begin
    Left := 10;
    Top := 70;
    Text:= 'File:';
  end;

  FedtFile := TfpgEdit.Create(Self);
  with FedtFile do
  begin
    Left := 100;
    Top := 70;
    Width := 200;
    Text:= '';
  end;

  FbtnOpen := TfpgButton.Create(Self);
  with FbtnOpen do
  begin
    Left := 310;
    Top := 70;
    Width := 70;
    Text:= 'Browse...';
    OnClick:= @Browse;
  end;

  FPanel := TfpgPanel.Create(Self);
  with FPanel do
  begin
    Align:= alBottom;
    Anchors := [anRight,anBottom];
    Text:= '';
    Height:= 30;
    Style:=bsFlat;
  end;

  FbtnOk := TfpgButton.Create(FPanel);
  with FbtnOk do
  begin
    Name := 'btnOk';
    Parent := FPanel;
    Align := alRight;
    Anchors := [anRight,anBottom];
    Text := 'Ok';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.Ok';
    TabOrder := 0;
    ModalResult:= mrOK;
    OnClick := @btnOkClick;
  end;

  FbtnCancel := TfpgButton.Create(FPanel);
  with FbtnCancel do
  begin
    ModalResult:= mrCancel;
    Parent := FPanel;
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
end;

destructor TdlgBitmapPrint.Destroy;
begin
  inherited Destroy;
end;

class function TdlgBitmapPrint.Execute(var AWidth, AHeight: Integer;
  out AFile: string): boolean;
begin
  with TdlgBitmapPrint.Create(nil) do
  begin
    FBmpWidth := AWidth;
    FBmpHeight := AHeight;
    initialize;
    Result := ShowModal = mrOK;
    if Result then
    begin
      AWidth := FBmpWidth;
      AHeight := FBmpHeight;
      AFile := FFile;
    end;
    Free;
  end;
end;

end.

