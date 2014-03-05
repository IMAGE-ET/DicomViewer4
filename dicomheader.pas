unit dicomheader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_main,
  fpg_form,
  fpg_base,
  fpg_basegrid,
  fpg_grid,
  images;

type

  { TdlgDicomHeader }

  TdlgDicomHeader = class(TfpgForm)
  private
    FImage: TImage;
    FGrid: TfpgStringGrid;
    procedure stringgridDrawCell(Sender: TObject; const ARow,
      ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
      var ADefaultDrawing: boolean);
    procedure Initialize;
    procedure Close(Sender: TObject; var CloseAction: TCloseAction);
  public
    procedure AfterCreate; override;
    class procedure Execute(AImage: TImage);
  end;

implementation

{ TdlgDicomHeader }

procedure TdlgDicomHeader.stringgridDrawCell(Sender: TObject; const ARow,
  ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
  var ADefaultDrawing: boolean);

var
  lStr: string;
  lRect: TRect;
begin
  if (aRow > 0) then
  begin
    lStr := FGrid.Cells[0, ARow];
    if Copy(lStr, 1, 3) = '   ' then
    begin
      FGrid.Canvas.Color := $00D8D8D8;
    end
    else
    if Copy(lStr, 1, 1) = ' ' then
    begin
      FGrid.Canvas.Color := $00E7EFEF;
    end;
    FGrid.Canvas.FillRectangle(ARect);
  end;
end;

procedure TdlgDicomHeader.Initialize;
var
  I: Integer;
  lTag: string;

begin
  FGrid.RowCount := FImage.DicomUtils.DicomTags.Count - 3;
  for I := 3 to FImage.DicomUtils.DicomTags.Count - 1 do
  begin
    lTag := FImage.DicomUtils.DicomTags[I];
    FGrid.Cells[0, I - 3] := FImage.DicomUtils.GetTagGroupElement(lTag);
    FGrid.Cells[1, I - 3] := FImage.DicomUtils.GetTagName(lTag);
    FGrid.Cells[2, I - 3] := FImage.DicomUtils.GetTagVR(lTag);
    FGrid.Cells[3, I - 3] := FImage.DicomUtils.GetTagValue(lTag);
  end;
end;

procedure TdlgDicomHeader.Close(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TdlgDicomHeader.AfterCreate;
begin
  Name := 'DicomHeader';
  FWidth:= 600;
  FHeight:= 500;
  WindowPosition := wpScreenCenter;
  WindowTitle := 'Dicom Header';

  FGrid := TfpgStringGrid.Create(self);
  with FGrid do
  begin
    Name := 'FGrid';
    Align := alClient;
    Options := Options + [go_SmoothScroll, go_AlternativeColor];
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('Group/Element', 100, taCenter);
    AddColumn('Name', 200, taLeftJustify);
    AddColumn('VR', 50, taCenter);
    AddColumn('Value', 400, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowSelect := True;
    TabOrder := 1;
    TextColor:= clBlack;
    Visible := True;
    // Add custom painting
    OnDrawCell := @StringGridDrawCell;
    OnClose:= @Close;
  end;
end;

class procedure TdlgDicomHeader.Execute(AImage: TImage);
begin
  with TdlgDicomHeader.Create(nil) do
  begin
    FImage := AImage;
    Initialize;
    Show;
  end;
end;

end.

