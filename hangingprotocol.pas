unit hangingprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  fpjson,
  jsonparser,
  seriecell,
  series,
  images;

type
  THangingProtocol = class;

  { TCellCondition }

  TCellCondition = class
  private
    FTagPath: string;
    FValue: string;
  public
    property TagPath: string read FTagPath write FTagPath;
    property Value: string read FValue write FValue;
  end;

  TCellConditionsSpecialization = specialize TFPGList<TCellCondition>;

  { TCellConditions }

  TCellConditions = class(TCellConditionsSpecialization)
  private
    FValidTagPaths: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function ImageMatches(AImage: TImage): boolean;
    procedure Clear;
    procedure SetFromImages(AImages: TImages);
    procedure SaveConditions(AConditions: TStrings);
  end;

  { THangingProtocolCell }

  THangingProtocolCell = class(TSerieCell)
  private
    FCellConditions: TCellConditions;
    FHangingProtocol: THangingProtocol;
  public
    constructor Create;
    destructor Destroy; override;
    function FindSerie(ASeries: TSeries): TSerie;
    property CellConditions: TCellConditions read FCellConditions write FCellConditions;
    property HangingProtocol: THangingProtocol read FHangingProtocol write FHangingProtocol;
  end;

  THangingProtocolCellsSpecialization = specialize TFPGList<THangingProtocolCell>;

  { THangingProtocolCells }

  THangingProtocolCells = class(THangingProtocolCellsSpecialization)
  private
  public
    destructor Destroy; override;
  end;

  THangingProtocols = class;

  { THangingProtocol }

  THangingProtocol = class
  private
    FName: string;
    FOwner: THangingProtocols;
    FHPCells: THangingProtocolCells;
    FRightScale: Boolean;
  public
    constructor Create(AOwner: THangingProtocols);
    destructor Destroy; override;
    procedure SetCellsConditions(ACells: TSerieCells);
    property Name: string read FName write FName;
    property Owner: THangingProtocols read FOwner;
    property Cells: THangingProtocolCells read FHPCells;
    property RightScale: Boolean read FRightScale write FRightScale;
  end;

  THangingProtocolSpecialization = specialize TFPGList<THangingProtocol>;

  { THangingProtocols }

  THangingProtocols = class(THangingProtocolSpecialization)
  private
    FCurrent: THangingProtocol;
    FModality: string;
    FMsgDest: TObject;
    procedure SetModality(AValue: string);
  public
    destructor Destroy; override;
    procedure Save;
    procedure Load;
    procedure Clear;
    procedure Next;
    property Modality: string read FModality write SetModality;
    property Current: THangingProtocol read FCurrent write FCurrent;
    property MsgDest: TObject read FMsgDest write FMsgDest;
  end;

implementation

{ THangingProtocolCell }

constructor THangingProtocolCell.Create;
begin
  inherited;
  FCellConditions := TCellConditions.Create;
end;

destructor THangingProtocolCell.Destroy;
begin
  FCellConditions.Free;
  inherited Destroy;
end;

{ THangingProtocolCells }

destructor THangingProtocolCells.Destroy;
var
  lHpCell: THangingProtocolCell;
begin
  for lHpCell in Self do
  begin
    FreeAndNil(lHpCell);
  end;
  inherited Destroy;
end;

{ TCellConditions }

procedure TCellConditions.SetFromImages(AImages: TImages);
var
  lValidTagPath: string;
  lImage: TImage;
  lCellCondition: TCellCondition;
  lValue: string;
begin
  if AImages.Count > 0 then
    lImage := AImages[0]
  else
    exit;

  for lValidTagPath in FValidTagPaths do
  begin
    lValue := lImage.DicomUtils.GetTagValue(lValidTagPath);
    if lValue <> '' then
    begin
      lCellCondition := TCellCondition.Create;
      lCellCondition.TagPath := lValidTagPath;
      lCellCondition.Value := lValue;
      inherited Add(lCellCondition);
    end;
  end;
end;

procedure TCellConditions.SaveConditions(AConditions: TStrings);
// convierte una serie de lineas de texto
// en condiciones
var
  I: Integer;
  lTagPath: string;
  lTagValue: string;
  lCondition: TCellCondition;
begin
  Clear;
  for I := 0 to AConditions.Count - 1 do
  begin
    lTagPath := Copy(AConditions[I], 0, Pos('|', AConditions[I]) - 1);
    lTagValue := Copy(AConditions[I], Pos('|', AConditions[I]) + 1, Length(AConditions[I]));
    lCondition := TCellCondition.Create;
    lCondition.TagPath:= lTagPath;
    lCondition.Value:= lTagValue;
    Add(lCondition);
  end;
end;

constructor TCellConditions.Create;
begin
  inherited;
  FValidTagPaths := TStringList.Create;
  FValidTagPaths.Add('0054,0220/fffe,e000/0008,0100');
  FValidTagPaths.Add('0020,0062');
  FValidTagPaths.Add('0018,1400');
  FValidTagPaths.Add('0018,1401');
end;

destructor TCellConditions.Destroy;
var
  lCellCondition: TCellCondition;
begin
  for lCellCondition in Self do
  begin
    FreeAndNil(lCellCondition);
  end;
  FValidTagPaths.Free;
  inherited Destroy;
end;

function TCellConditions.ImageMatches(AImage: TImage): boolean;
var
  lCondition: TCellCondition;
  lRes: boolean;
begin
  Result := True;
  for lCondition in Self do
  begin
    // Primero se evalúan las condiciones internas
    if UpperCase(lCondition.TagPath) = 'SERIESPOSITION' then
    begin
      if (AImage.Serie <> nil) and (StrToIntDef(lCondition.Value, 0) = (AImage.Serie as TSerie).Index) then
      begin
        lRes := True;
      end;
    end
    else
    begin
      // Luego los tags
      lRes := AImage.DicomUtils.GetTagValue(lCondition.TagPath) = lCondition.Value;
    end;
    Result := Result and lRes;
  end;
end;

procedure TCellConditions.Clear;
var
  lCondition: TCellCondition;
begin
  for lCondition in Self do
  begin
    FreeAndNil(lCondition);
  end;
  inherited Clear;
end;

{ THangingProtocols }

procedure THangingProtocols.SetModality(AValue: string);
begin
  if FModality=AValue then Exit;
  FModality:=AValue;

  // cuando se asigna un nuevo H.P. deben borrarse todos los otros
  Clear;
end;

destructor THangingProtocols.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure THangingProtocols.Save;
(* Saves hanging protocol for current modality *)
var
  lJsonItem: TJSONObject;
  lJsonHPs: TJSONArray;
  lJsonCells: TJSONArray;
  lJsonConditions: TJSONArray;
  lHp: THangingProtocol;
  lCell: THangingProtocolCell;
  lCondition: TCellCondition;
  lStr: TStringList;
begin
  lJsonHPs := TJSONArray.Create;
  try
    for lHp in Self do
    begin
      lJsonCells := TJSONArray.Create;
      for lCell in lHp.Cells do
      begin
        lJsonConditions := TJSONArray.Create;
        for lCondition in lCell.CellConditions do
        begin
          lJsonItem := TJSONObject.Create;
          lJsonItem.Add('tagpath', lCondition.TagPath);
          lJsonItem.Add('value', lCondition.Value);
          lJsonConditions.Add(lJsonItem);
        end;
        lJsonItem := TJSONObject.Create;
        lJsonItem.Add('conditions', lJsonConditions);
        lJsonItem.Add('width', lCell.Width);
        lJsonItem.Add('height', lCell.Height);
        lJsonItem.Add('left', lCell.Left);
        lJsonItem.Add('top', lCell.Top);
        lJsonCells.Add(lJsonItem);
      end;

      lJsonItem := TJSONObject.Create;
      lJsonItem.Add('protocol', lHp.Name);
      lJsonItem.Add('rightscale', lHp.RightScale);
      lJsonItem.Add('cells', lJsonCells);
      lJsonHPs.Add(lJsonItem);
    end;
    lStr := TStringList.Create;
    lStr.Text := lJsonHPs.AsJSON;
    lStr.SaveToFile(ExtractFilePath(ParamStr(0)) + FModality + '.hangingprotocol');
  finally
    lStr.Free;
    lJsonHPs.Free;
  end;
end;

procedure THangingProtocols.Load;
var
  lStr: TStringList;
  I: Integer;
  A: Integer;
  B: Integer;
  lJson: TJSONObject;
  lProtocols: TJSONArray;
  lCells: TJsonArray;
  lConditions: TJsonArray;
  lParser: TJSONParser;
  lHP: THangingProtocol;
  lCell: THangingProtocolCell;
  lCondition: TCellCondition;
  lFile: string;
begin
  lFile := ExtractFilePath(ParamStr(0)) + FModality + '.hangingprotocol';
  if not FileExists(lFile) then
    exit;
  Clear;
  FCurrent := nil;
  lStr := TStringList.Create;
  try
    lStr.LoadFromFile(lFile);
    lParser := TJSONParser.Create(lStr.Text);
    lProtocols := lParser.Parse as TJSONArray;
    for I := 0 to lProtocols.Count - 1 do
    begin
      lJson := lProtocols[I] as TJSONObject;
      lHP := THangingProtocol.Create(Self);
      lHP.Name := lJson.Strings['protocol'];
      if lJson.IndexOfName('rightscale') <> - 1 then
        lHP.RightScale := lJson.Booleans['rightscale'];
      lCells := lJson.Arrays['cells'];
      for A := 0 to lCells.count - 1 do
      begin
        lCell := THangingProtocolCell.Create;
        lCell.Left:= TJsonObject(lCells[A]).Floats['left'];
        lCell.Top:= TJsonObject(lCells[A]).Floats['top'];
        lCell.Width:= TJsonObject(lCells[A]).Floats['width'];
        lCell.Height:= TJsonObject(lCells[A]).Floats['height'];
        lConditions := TJsonObject(lCells[A]).Arrays['conditions'];
        for B := 0 to lConditions.Count - 1 do
        begin
          lCondition := TCellCondition.Create;
          lCondition.TagPath:= TJsonObject(lConditions[B]).Strings['tagpath'];
          lCondition.Value:= TJsonObject(lConditions[B]).Strings['value'];
          lCell.CellConditions.Add(lCondition);
        end;
        lHP.Cells.Add(lCell);
      end;
      Add(lHP);
      if FCurrent = nil then
        FCurrent := Items[0];
    end;
  finally
    lStr.Free;
    lParser.Free;
    lProtocols.Free;
  end;
end;

procedure THangingProtocols.Clear;
var
  lHP: THangingProtocol;
begin
  for lHP in Self do
  begin
    FreeAndNil(lHP);
  end;
end;

procedure THangingProtocols.Next;
var
  lIdx: Integer;
begin
  if Self.Count = 0 then
  begin
    FCurrent := nil;
    exit;
  end;

  lIdx := Self.IndexOf(Current);
  if lIdx + 1 <= Count - 1 then
    FCurrent := Self.Items[lIdx + 1]
  else
    FCurrent := Self.Items[0];
end;

{ THangingProtocol }

constructor THangingProtocol.Create(AOwner: THangingProtocols);
begin
  inherited Create;
  FOwner := AOwner;
  FHPCells := THangingProtocolCells.Create;
end;

destructor THangingProtocol.Destroy;
begin
  FHPCells.Free;
  inherited Destroy;
end;

procedure THangingProtocol.SetCellsConditions(ACells: TSerieCells);
var
  lHpCell: THangingProtocolCell;
  lCell: TSerieCell;
begin
  for lCell in ACells do
  begin
    lHpCell := THangingProtocolCell.Create;
    lHpCell.Left:= lCell.Left;
    lHpCell.Top:= lCell.Top;
    lHpCell.Width:= lCell.Width;
    lHpCell.Height:= lCell.Height;
    lHpCell.CellConditions.SetFromImages(lCell.Serie.Images);
    FHPCells.Add(lHpCell);
  end;
end;

function THangingProtocolCell.FindSerie(ASeries: TSeries): TSerie;
var
  lSerie: TSerie;
begin
  Result := nil;
  if ASeries = nil then
    exit;

  for lSerie in ASeries do
  begin
    if (lSerie.Images.Count > 0) and (FCellConditions.ImageMatches(lSerie.Images[0])) then
    begin
      Result := lSerie;
      exit;
    end;
  end;
end;

end.
