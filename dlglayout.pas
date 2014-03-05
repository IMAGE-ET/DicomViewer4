unit dlgLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_dialogs, fpg_button, fpg_panel,
  layoutdefinition;

type

  { TCustomLayoutButton }

  TCustomLayoutButton = class(TfpgButton)
  private
    FChecked: Boolean;
    FOnChecked: TNotifyEvent;
    FLayout: TLayoutDefinition;
    procedure Paint(Sender: TObject);
    procedure MouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  public
    constructor Create(AOwner: TComponent; ALayout: TLayoutDefinition);
    property OnChecked: TNotifyEvent read FOnChecked write FOnChecked;
    property Checked: Boolean read FChecked write FChecked;
    property Layout: TLayoutDefinition read FLayout;
  end;

  TdlgLayout = class(TfpgForm)
  private
    FLayoutDefinition: TLayoutDefinition;
    FbtnOk: TfpgButton;
    FbtnCancel: TfpgButton;
    FbtnPanel: TfpgPanel;
    FButtonChecked: TCustomLayoutButton;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure ButtonChecked(Sender: TObject);
    function CreateSymmetricLayout(AX, AY: Integer): TLayoutDefinition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute(out ALayout: TLayoutDefinition): boolean;
  end;

implementation

{ TCustomLayoutButton }

procedure TCustomLayoutButton.Paint(Sender: TObject);
var
  lDimensions: TfpgRect;
  lX: Integer;
  lY: Integer;
  I: Integer;
begin
  lDimensions.Left := 2;
  lDimensions.Top := 2;
  lDimensions.Width := Width - 4;
  lDimensions.Height := Height - 4;
  if FChecked then
    Canvas.Clear($111111)
  else
    Canvas.Clear(clWhite);
  Canvas.Color:= clBlack;
  Canvas.DrawRectangle(lDimensions);

  for I := 0 to Length(FLayout) - 1 do
  begin
    lDimensions.Width:= Round((Width - 8) * FLayout[I].width);
    lDimensions.Height:= Round((Height - 8) * FLayout[I].height);
    lDimensions.Left := Round((Width - 8) * FLayout[I].left) + 4;
    lDimensions.Top:= Round((Height - 8) * FLayout[I].top) + 4;
    Canvas.Color:= clDarkGray;
    Canvas.DrawRectangle(lDimensions);
  end;
end;

procedure TCustomLayoutButton.MouseDown(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  if AButton = mbLeft then
  begin
    if Assigned(OnChecked) then
      FOnChecked(Self);
  end;
end;

constructor TCustomLayoutButton.Create(AOwner: TComponent;
  ALayout: TLayoutDefinition);
begin
  inherited Create(AOwner);
  FLayout := ALayout;
  OnPaint:=@Paint;
  OnMouseDown:=@MouseDown;
  Width := 80;
  Height := 80;
  Flat:= True;
end;

{ TdlgLayout }

procedure TdlgLayout.BtnOkClick(Sender: TObject);
begin
  FLayoutDefinition := FButtonChecked.Layout;
  ModalResult := mrOK;
end;

procedure TdlgLayout.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TdlgLayout.ButtonChecked(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TCustomLayoutButton then
    begin
      TCustomLayoutButton(Components[I]).Checked:= False;
      TCustomLayoutButton(Components[I]).Invalidate;
    end;
  (Sender as TCustomLayoutButton).Checked := True;
  FButtonChecked := (Sender as TCustomLayoutButton);
end;

function TdlgLayout.CreateSymmetricLayout(AX, AY: Integer): TLayoutDefinition;
var
  X: Integer;
  Y: Integer;
  I: Integer;
  lButtonCell: TLayoutDefinitionCell;
begin
  I := 0;
  SetLength(Result, AX * AY);
  for X := 0 to AX - 1 do
    for Y := 0 to AY - 1 do
    begin
      lButtonCell.left:= X / AX;
      lButtonCell.top:= Y / AY;
      lButtonCell.width:= 1 / AX;
      lButtonCell.height:= 1 / AY;
      Result[I] := lButtonCell;
      inc(I);
    end;
end;

constructor TdlgLayout.Create(AOwner: TComponent);
var
  X: Integer;
  Y: Integer;
  lLayoutButton: TCustomLayoutButton;
  lCustButton: TCustomLayoutButton;
  lButtonCell: TLayoutDefinitionCell;
  lLayout: TLayoutDefinition;
begin
  inherited Create(AOwner);
  FButtonChecked := nil;
  Name := 'dlgLayout';
  FWidth := 270;
  FHeight := 400;
  Sizeable := False;
  WindowAttributes := [];
  WindowPosition := wpAuto;
  WindowTitle := 'Change layout';

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

  // se dibujan los botones standard
  for X := 0 to 2 do
    for Y := 0 to 2 do
    begin
      lLayout := CreateSymmetricLayout(X + 1, Y + 1);
      lLayoutButton := TCustomLayoutButton.Create(Self, lLayout);
      with lLayoutButton do
      begin
        Left := ((Width + 4) * X) + 10;
        Top := ((Height + 4) * Y) + 10;
        OnChecked:=@ButtonChecked;
        // se selecciona el 1er boton
        if (X = 0) and (Y = 0) then
        begin
          FButtonChecked := lLayoutButton;
          FButtonChecked.Checked := True;
        end;
      end;
    end;

  // botones a medida
  SetLength(lLayout, 3);
  lButtonCell.left:= 0;
  lButtonCell.top:= 0;
  lButtonCell.width:= 0.5;
  lButtonCell.height:= 1;
  lLayout[0] := lButtonCell;
  lButtonCell.left:= 0.5;
  lButtonCell.top:= 0;
  lButtonCell.width:= 0.5;
  lButtonCell.height:= 0.5;
  lLayout[1] := lButtonCell;
  lButtonCell.left:= 0.5;
  lButtonCell.top:= 0.5;
  lButtonCell.width:= 0.5;
  lButtonCell.height:= 0.5;
  lLayout[2] := lButtonCell;

  lCustButton := TCustomLayoutButton.Create(Self, lLayout);
  with lCustButton do
  begin
    Left := ((Width + 4) * 0) + 10;
    Top := ((Height + 4) * 3) + 10;
    OnChecked:=@ButtonChecked;
  end;
end;

destructor TdlgLayout.Destroy;
begin
  inherited Destroy;
end;

class function TdlgLayout.Execute(out ALayout: TLayoutDefinition): boolean;
begin
  with TdlgLayout.Create(nil) do
  begin
    result := ShowModal = mrOK;
    if Result then
    begin
      ALayout := FLayoutDefinition;
    end;
    Free;
  end;
end;

end.

