unit paperprint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_dialogs, fpg_button, fpg_panel,
  fpg_combobox, fpg_label, fpg_radiobutton,
  Printers;

type

  { TdlgPaperPrint }

  TdlgPaperPrint = class(TfpgForm)
  private
    FPanel: TfpgPanel;
    FbtnOk: TfpgButton;
    FlblPrinter: TfpgLabel;
    FlblBin: TfpgLabel;
    FlblPaperSize: TfpgLabel;
    FgrpOrientation: TfpgGroupBox;
    FcmbPrinters: TfpgComboBox;
    FcmbBins: TfpgComboBox;
    FcmbPaperSize: TfpgComboBox;
    FbtnCancel: TfpgButton;
    FrbPortrait: TfpgRadioButton;
    FrbLandscape: TfpgRadioButton;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure Initialize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute: boolean;
  end;

implementation

{ TdlgPaperPrint }

procedure TdlgPaperPrint.BtnOkClick(Sender: TObject);
begin
  Printer.SetPrinter(FcmbPrinters.Text);
  Printer.PrinterIndex:= FcmbPrinters.FocusItem;
  Printer.BinName := FcmbBins.Text;
  Printer.PaperSize.PaperName:=FcmbPaperSize.Text;
  if FrbPortrait.Checked then
    Printer.Orientation := poPortrait
  else
    Printer.Orientation := poLandscape;

  ModalResult := mrOK;
end;

procedure TdlgPaperPrint.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TdlgPaperPrint.Initialize;
var
  lIdx: Integer;
begin
  // impresoras
  FcmbPrinters.Items.Clear;
  FcmbPrinters.Items.AddStrings(Printers.Printer.Printers);
  lIdx := Printers.Printer.PrinterIndex;
  if lIdx > -1 then
    FcmbPrinters.FocusItem := lIdx;

  // bandejas
  FcmbBins.Items.Clear;
  FcmbBins.Items.AddStrings(Printers.Printer.SupportedBins);
  lIdx :=  Printers.Printer.SupportedBins.IndexOf(Printers.Printer.BinName);
  if lIdx > -1 then
    FcmbBins.FocusItem := lIdx;

  // paper size
  FcmbPaperSize.Items.Clear;
  FcmbPaperSize.Items.AddStrings(Printers.Printer.PaperSize.SupportedPapers);
  lIdx :=  Printers.Printer.PaperSize.SupportedPapers.IndexOf(Printers.Printer.PaperSize.DefaultPaperName);
  if lIdx > -1 then
    FcmbPaperSize.FocusItem := lIdx;

  // orientation
  if Printers.Printer.Orientation = poLandscape then
    FrbLandscape.Checked := True
  else
    FrbPortrait.Checked := True
end;

constructor TdlgPaperPrint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'dlgPaperPrint';
  FWidth := 310;
  FHeight := 300;
  Sizeable := False;
  WindowAttributes := [];
  WindowPosition := wpAuto;
  WindowTitle := 'Paper Print';

  FlblPrinter := TfpgLabel.Create(Self);
  with FlblPrinter do
  begin
    Left := 10;
    Top := 10;
    Text:= 'Printer:';
  end;

  FlblBin := TfpgLabel.Create(Self);
  with FlblBin do
  begin
    Left := 10;
    Top := 40;
    Text:= 'Bin:';
  end;

  FlblPaperSize := TfpgLabel.Create(Self);
  with FlblPaperSize do
  begin
    Left := 10;
    Top := 70;
    Text:= 'Paper Size:';
  end;

  FcmbPrinters := TfpgComboBox.Create(Self);
  with FcmbPrinters do
  begin
    Left := 100;
    Top := 10;
    Width := 200;
  end;

  FcmbBins := TfpgComboBox.Create(Self);
  with FcmbBins do
  begin
    Left := 100;
    Top := 40;
    Width := 200;
  end;

  FcmbPaperSize := TfpgComboBox.Create(Self);
  with FcmbPaperSize do
  begin
    Left := 100;
    Top := 70;
    Width := 200;
  end;

  FgrpOrientation := TfpgGroupBox.Create(Self);
  with FgrpOrientation do
  begin
    Left := 10;
    Top := 110;
    Width:= 290;
    Text:= 'Orientation';
    Height:= 80;
  end;

  FrbPortrait := TfpgRadioButton.Create(FgrpOrientation);
  with FrbPortrait do
  begin
    Left:= 10;
    Top:= 25;
    GroupIndex:=0;
    Checked:= True;
    Text:= 'Portrait';
  end;

  FrbLandscape := TfpgRadioButton.Create(FgrpOrientation);
  with FrbLandscape do
  begin
    Left:= 10;
    Top:= 50;
    GroupIndex:=0;
    Checked:= False;
    Text:= 'Landscape';
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
  Initialize;
end;

destructor TdlgPaperPrint.Destroy;
begin
  inherited Destroy;
end;

class function TdlgPaperPrint.Execute: boolean;
begin
  with TdlgPaperPrint.Create(nil) do
  begin
    Result := ShowModal = mrOK;
    Free;
  end;
end;

end.

