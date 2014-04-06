unit images;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fgl,
  fpjson, jsonparser,
  vectors,
  config,
  textoverlays,
  libDicomUtilsWrapperDyn,
  fpg_base,
  fpg_main,
  fpg_form,
  Agg2D,
  fpg_img_utils,
  FPimage,
  MammoBoundaries;

type

  TOrientationMatrix = array[1..3] of array[1..3] of char;

  TLut = array of byte;

  { TImage }

  TImage = class
  private
    FImgBuf: Pointer;
    FImgBufLen: TLongOrdinal;
    FCols: Integer;
    FColSpacing: double;
    FDicomUtils: TLibDicomUtilsWrapperDyn;
    FFileName: string;
    FImageOrientationPatient: TStringList;
    FImagePositionPatient: TStringList;
    FImageType: string;
    FInstanceNumber: Integer;
    FModality: string;
    FPatientName: string;
    FRows: Integer;
    FRowSpacing: double;
    FLaterality: string;
    FSerie: TObject; // usamos TObject en lugar de TSerie para evitar referencias circulares
    FSeriesDescription: string;
    FSeriesInstanceUID: string;
    FSeriesNumber: string;
    FSOPInstanceUID: string;
    FStudyInstanceUID: string;
    FVectors: TVectors;
    FOrientation: TOrientationMatrix;
    FTopLeftOverlay: TTextOverlays;
    FTopRightOverlay: TTextOverlays;
    FBottomLeftOverlay: TTextOverlays;
    FBottomRightOverlay: TTextOverlays;
    FWC: double;
    FWW: double;
    FX1: Integer;
    FX2: Integer;
    FY1: Integer;
    FY2: Integer;
    FBitsStored: byte;
    FPixelRepresentation: Integer;
    FRescaleSlope: Double;
    FRescaleIntercept: Double;
    FPhotometricInterpretation: string;
    FPlanarConfiguration: byte;
    function GetOrientation: TOrientationMatrix;
    procedure SetOverlaysForModality(AModality: string);
    procedure LoadModalityOverlays(AModality: TJSONObject);
    procedure AddOverlays(AOverlays: TTextOverlays; AJSONArray: TJSONArray);
  public
    constructor Create(ASerie: TObject);
    destructor Destroy; override;
    function MakeWindowLut(AWC, AWW: double): TLut;
    procedure Resample(AZoom: double;
      out AResampledWidth, AResampledHeight: integer;
      var AResampledBuf: Pointer;
      out AResampledBufSize: TLongOrdinal);
    procedure OpenFile(AFile: string);
    procedure OpenStream(AStream: TMemoryStream);
    procedure Reset;
    procedure CalculateRealWidthHeight;
    property DicomUtils: TLibDicomUtilsWrapperDyn read FDicomUtils;
    property FileName: string read FFileName write FFileName;
    property InstanceNumber: Integer read FInstanceNumber write FInstanceNumber;
    property ImageType: string read FImageType write FImageType;
    property Serie: TObject read FSerie write FSerie;
    property ImageOrientationPatient: TStringList read FImageOrientationPatient;
    property ImagePositionPatient: TStringList read FImagePositionPatient;
    property RowSpacing: double read FRowSpacing;
    property ColSpacing: double read FColSpacing;
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
    property Modality: string read FModality;
    property Laterality: string read FLaterality;
    property PatientName: string read FPatientName;
    property SeriesNumber: string read FSeriesNumber;
    property Vectors: TVectors read FVectors;
    property TopLeftOverlays: TTextOverlays read FTopLeftOverlay;
    property TopRightOverlays: TTextOverlays read FTopRightOverlay;
    property BottomLeftOverlays: TTextOverlays read FBottomLeftOverlay;
    property BottomRightOverlays: TTextOverlays read FBottomRightOverlay;
    property MGX1: Integer read FX1;
    property MGX2: Integer read FX2;
    property MGY1: Integer read FY1;
    property MGY2: Integer read FY2;
    property StudyInstanceUID: string read FStudyInstanceUID write FStudyInstanceUID;
    property SeriesInstanceUID: string read FSeriesInstanceUID write FSeriesInstanceUID;
    property SOPInstanceUID: string read FSOPInstanceUID write FSOPInstanceUID;
    property SeriesDescription: string read FSeriesDescription write FSeriesDescription;
    property Orientation: TOrientationMatrix read FOrientation;
    property ImgBuf: Pointer read FImgBuf write FImgBuf;
    property ImgBufLen: TLongOrdinal read FImgBufLen write FImgBufLen;
    property PlanarConfiguration: Byte read FPlanarConfiguration write FPlanarConfiguration;
    property WW: double read FWW write FWW;
    property WC: double read FWC write FWC;
  end;

  TImagesSpecialization = specialize TFPGList<TImage>;

  { TImages }

  TImages = class(TImagesSpecialization)
  public
    destructor Destroy; override;
  end;

implementation

{ TImages }

destructor TImages.Destroy;
var
  lImage: TImage;
begin
  for lImage in Self do
  begin
    lImage.free;
  end;
  inherited Destroy;
end;

{ TImage }

function TImage.GetOrientation: TOrientationMatrix;
var
  lOrientationX: char;
  lOrientationY: char;
  lOrientationZ: char;
  lFloatOrient: array[0..2] of double;
  absX: double;
  absY: double;
  absZ: double;
  I: Integer;
  A: Integer;
  lFormat: TFormatSettings;
  lRes: string;
begin

  (*

  Trabajamos con una matriz de 3x3 similar a esta

   |A|
  -+-+-
  L| |R
  -+-+-
   |P|

   Luego, para mostrar los "refinements" (ver C.7.6.1.1.1) usamos
   esa matríz. En caso de rotar la imagen, simplemente
   rotamos la matriz en la misma proporción.

  *)

  for I := 1 to 3 do
    for A := 1 to 3 do
      Result[I][A] := #32;


  lRes := '';
  lFormat.DecimalSeparator:= '.';

  for A := 0 to 1 do
  begin
    if FImageOrientationPatient.Count < (A * 3 + 2) then
      break;
    lFloatOrient[0] := StrToFloatDef(FImageOrientationPatient[A * 3 + 0], 0, lFormat);
    lFloatOrient[1] := StrToFloatDef(FImageOrientationPatient[A * 3 + 1], 0, lFormat);
    lFloatOrient[2] := StrToFloatDef(FImageOrientationPatient[A * 3 + 2], 0, lFormat);
    if lFloatOrient[0] < 0 then
      lOrientationX := 'L'
    else
      lOrientationX := 'R';

    if lFloatOrient[1] < 0 then
      lOrientationY := 'P'
    else
      lOrientationY := 'A';

    if lFloatOrient[2] < 0 then
      lOrientationZ := 'H'
    else
      lOrientationZ := 'F';

    absX := lFloatOrient[0];
    if (absX < 0) then
      absX := -absX;

    absY := lFloatOrient[1];
    if (absY < 0) then
      absY := -absY;

    absZ := lFloatOrient[2];
    if (absZ < 0) then
      absZ := -absZ;

    if (absX > 0.0001) and (absX > absY) and (absX > absZ) then
    begin
      lRes := lRes + lOrientationX;
      absX := 0;
    end
    else
    if (absY > 0.0001) and (absY > absX) and (absY > absZ) then
    begin
      lRes := lRes + lOrientationY;
      absY := 0;
    end
    else
    if (absZ > 0.0001) and (absZ > absX) and (absZ > absY) then
    begin
      lRes := lRes + lOrientationZ;
      absZ := 0;
    end;
  end;

  if Length(lRes) > 0 then
  begin
    if lRes[1] = 'A' then
    begin
      Result[2][1] := 'A';
      Result[2][3] := 'P';
    end
    else
    if lRes[1] = 'P' then
    begin
      Result[2][1] := 'P';
      Result[2][3] := 'A';
    end
    else
    if lRes[1] = 'L' then
    begin
      Result[2][1] := 'L';
      Result[2][3] := 'R';
    end
    else
    if lRes[1] = 'R' then
    begin
      Result[2][1] := 'R';
      Result[2][3] := 'L';
    end
  end;

  if Length(lRes) > 1 then
  begin
    if lRes[2] = 'A' then
    begin
      Result[1][2] := 'A';
      Result[3][2] := 'P';
    end
    else
    if lRes[2] = 'P' then
    begin
      Result[1][2] := 'P';
      Result[3][2] := 'A';
    end
    else
    if lRes[2] = 'L' then
    begin
      Result[1][2] := 'L';
      Result[3][2] := 'R';
    end
    else
    if lRes[2] = 'R' then
    begin
      Result[1][2] := 'R';
      Result[3][2] := 'L';
    end
    else
    if lRes[2] = 'H' then
    begin
      Result[1][2] := 'H';
      Result[3][2] := 'F';
    end
    else
    if lRes[2] = 'F' then
    begin
      Result[1][2] := 'F';
      Result[3][2] := 'H';
    end
  end;
end;

procedure TImage.SetOverlaysForModality(AModality: string);
(* Se cargan los Overlays por modalidad *)
var
  lParser: TJSONParser;
  lJSONData: TJSONData;
  lModality: TJSONObject;
  lJSONArray: TJSONArray;
  I: Integer;
  lFound: Boolean;

begin
  FTopLeftOverlay.Clear;
  FTopRightOverlay.Clear;
  FBottomLeftOverlay.Clear;
  FBottomRightOverlay.Clear;
  lFound := False;
  lParser := TJSONParser.Create(gConfig.Overlays);
  try
    lJSONData := lParser.Parse;
    lJSONArray := TJSONObject(lJSONData).Arrays['modalities'];
    for I := 0 to lJSONArray.Count - 1 do
    begin
      if TJSONObject(lJSONArray[I]).Names[0] = AModality then
      begin
        lModality := TJSONObject(lJSONArray[I]).Objects[AModality];
        LoadModalityOverlays(lModality);
        lFound := True;
        Break;
      end;
    end;
    if not lFound then
    begin
      for I := 0 to lJSONArray.Count - 1 do
      begin
        if TJSONObject(lJSONArray[I]).Names[0] = 'DEFAULT' then
        begin
          lModality := TJSONObject(lJSONArray[I]).Objects['DEFAULT'];
          LoadModalityOverlays(lModality);
          Break;
        end;
      end;
    end;
  finally
    lJSONData.Free;
    lParser.Free;
  end;
end;

procedure TImage.LoadModalityOverlays(AModality: TJSONObject);
var
  I: Integer;
  lModalityItem: TJSONObject;
  lConditions: TJSONArray;
  lCondition: TJSONObject;
  lOverlayArray: TJSONArray;
  lOverlayTypes: TStringList;
  lTag: string;
  lValue: string;
  lOverlayType: string;

begin
  lOverlayTypes := TStringList.Create;
  try
    lOverlayTypes.AddObject('TOPLEFT', FTopLeftOverlay);
    lOverlayTypes.AddObject('TOPRIGHT', FTopRightOverlay);
    lOverlayTypes.AddObject('BOTTOMLEFT', FBottomLeftOverlay);
    lOverlayTypes.AddObject('BOTTOMRIGHT', FBottomRightOverlay);

    lModalityItem := AModality;
    if lModalityItem.IndexOfName('CONDITIONS') <> - 1 then
    begin
      lConditions := lModalityItem.Arrays['CONDITIONS'];
      for I := 0 to lConditions.Count - 1 do
      begin
        lCondition := TJSONObject(lConditions[I]);
        lTag := lCondition.Strings['tag'];
        lValue := lCondition.Strings['value'];
        if FDicomUtils.GetTagValue(lTag) = lValue then
        begin
          lModalityItem := lCondition.Objects['OVERLAYS'];
          break;
        end;
      end;
    end;

    for I := 0 to lOverlayTypes.Count - 1 do
    begin
      lOverlayType:= lOverlayTypes[I];
      if lModalityItem.IndexOfName(lOverlayType) > -1 then
      begin
        lOverlayArray := lModalityItem.Arrays[lOverlayType];
        AddOverlays(TTextOverlays(lOverlayTypes.Objects[I]), lOverlayArray);
      end;
    end;
  finally
    lOverlayTypes.Free;
  end;
end;

procedure TImage.Resample(AZoom: double;
  out AResampledWidth, AResampledHeight: integer;
  var AResampledBuf: Pointer;
  out AResampledBufSize: TLongOrdinal);
var
  I: longword;
  P: longword;
  X: Integer;
  Y: Integer;
  lSamples: Integer;
  lIn: PByte;
  lOut: PByte;

begin
  AResampledWidth := Trunc(DicomUtils.ImgWidth * AZoom);
  AResampledHeight := Trunc(DicomUtils.ImgHeight * AZoom);

  if FImgBuf = nil then
    exit;

  lSamples := FDicomUtils.SamplesPerPixel;

  if lSamples = 1 then
    AResampledBufSize := AResampledWidth * AResampledHeight * SizeOf(Word)
  else
    AResampledBufSize := AResampledWidth * AResampledHeight * SizeOf(Byte) * lSamples;

  ReAllocMem(AResampledBuf, AResampledBufSize);

  if lSamples = 1 then
  begin
    // Monochrome1 and Monochrome2
    for X := 0 to AResampledWidth - 1 do
      for Y := 0 to AResampledHeight - 1 do
      begin
        // i es el pixel x,y en AResampledBuf
        I := x * lSamples + y * AResampledWidth * lSamples;
        // p es el pixel x,y extrapolado a FImgBuf
        P := round(x / AZoom) * lSamples + round(y / AZoom) * FDicomUtils.ImgWidth * lSamples;
        PWord(AResampledBuf)[I] := PWord(FImgBuf)[P]
      end;
  end
  else
  begin
    // RGB
    for X := 0 to AResampledWidth - 1 do
      for Y := 0 to AResampledHeight - 1 do
      begin
        // i es el pixel x,y en AResampledBuf
        I := x * lSamples + y * AResampledWidth * lSamples;
        // p es el pixel x,y extrapolado a FImgBuf
        P := round(x / AZoom) * lSamples + round(y / AZoom) * FDicomUtils.ImgWidth * lSamples;
        lIn := PByte(FImgBuf);
        inc(lIn, P);
        lOut := PByte(AResampledBuf);
        inc(lOut, I);
        lOut[0] := lIn[0];
        lOut[1] := lIn[1];
        lOut[2] := lIn[2];
      end;
  end;
end;

procedure TImage.AddOverlays(AOverlays: TTextOverlays;
  AJSONArray: TJSONArray);
var
  I: Integer;
  lIndex: Integer;
  lOverlay: TTextOverlay;

begin
  for I := 0 to AJSONArray.Count - 1 do
  begin
    lOverlay := TTextOverlay.Create;
    with lOverlay do
    begin
      if TJSONObject(AJSONArray[I]).IndexOfName('textlabel') > -1 then
        TextLabel := TJSONObject(AJSONArray[I]).Strings['textlabel'];
      if TJSONObject(AJSONArray[I]).IndexOfName('tag') > -1 then
        Tag := TJSONObject(AJSONArray[I]).Strings['tag'];
      if TJSONObject(AJSONArray[I]).IndexOfName('value') > -1 then
        Value := TJSONObject(AJSONArray[I]).Strings['value']
      else
      if  TJSONObject(AJSONArray[I]).IndexOfName('removecircumflex') > -1 then
      begin
        if UpperCase(TJSONObject(AJSONArray[I]).Strings['removecircumflex']) = 'TRUE' then
        begin
          Value := FDicomUtils.GetTagValue(Tag);
          Value := StringReplace(Value, '^', ', ', []);
          Value := StringReplace(Value, '^', ' ', [rfReplaceAll]);
        end;
      end
      else
      if  TJSONObject(AJSONArray[I]).IndexOfName('index') > -1 then
      begin
        lIndex := StrToIntDef(TJSONObject(AJSONArray[I]).Strings['index'], 0);
        Value := FDicomUtils.GetTagValue(Tag);
        Value := FDicomUtils.GetTagValueFromIndex(Value, lIndex);
      end
      else
        Value := FDicomUtils.GetTagValue(Tag);
    end;
    AOverlays.Add(lOverlay);
  end;
end;

constructor TImage.Create(ASerie: TObject);
begin
  inherited Create;
  FDicomUtils := TLibDicomUtilsWrapperDyn.Create;
  FSerie := ASerie;
  FImageOrientationPatient := TStringList.Create;
  FImagePositionPatient := TStringList.Create;
  FVectors := TVectors.Create;
  FTopLeftOverlay := TTextOverlays.Create;
  FTopRightOverlay := TTextOverlays.Create;
  FBottomLeftOverlay := TTextOverlays.Create;
  FBottomRightOverlay := TTextOverlays.Create;
  FImgBuf:= nil;
  FImgBufLen:=0;
end;

destructor TImage.Destroy;
begin
  FImageOrientationPatient.Free;
  FImagePositionPatient.Free;
  FDicomUtils.Free;
  FVectors.Free;
  FTopLeftOverlay.Free;
  FTopRightOverlay.Free;
  FBottomLeftOverlay.Free;
  FBottomRightOverlay.Free;
  inherited Destroy;
end;

procedure TImage.CalculateRealWidthHeight;
const
  (* Es el tamaño al cual se calculan los límites de la mamografía,
     esto permite que los cálculos se hagan más rápido. *)
  cRatioMiniatura = 0.10;

var
  lBuffer: PByte;
  lTmpBuff: PByte;
  lBufSize: TLongOrdinal;
  lResult: integer;
  lWidth: double;
  lHeight: double;
  lIntWidth: Integer;
  lIntHeight: Integer;
  x: Integer;
  y: Integer;
  lCol: Integer;
  lOldZoom: double;
  lScanLine: PLongWord;
  lScanLine2: PLongWord;
  lImage: TfpgImage;
  lMemoryImage: TFPCustomImage;
  lByteColor: TfpgColor;
  lColor: TfpgColor;

begin
  lBuffer := nil;
  lBufSize:= 0;

  lImage := TfpgImage.Create;
  try
    DicomUtils.SetZoom(cRatioMiniatura);
    lResult := DicomUtils.LoadDicomImageToBufferEx(lBuffer, lBufSize, 0, 0, lWidth, lHeight);
    if lResult = 1 then
    begin
      lImage.AllocateImage(32, Trunc(lWidth), Trunc(lHeight));
      lTmpBuff := lBuffer;
      // se obtiene una imagen pixelada
      for y := 0 to trunc(lHeight) - 1 do
      begin
        lScanLine := lImage.ScanLine[y];
        for x := 0 to Trunc(lWidth) - 1 do
        begin
          if lTmpBuff^ > 25 then
            lByteColor := clWhite
          else
            lByteColor := clBlack;
          if (x mod 4 = 0) then
          begin
            lColor := lByteColor;
          end;
          lScanLine^ := lColor;
          inc(lTmpBuff);
          inc(lScanLine);
        end;
      end;
    end;

    lImage.UpdateImage;

    TMammoBoundaries.Execute(lImage, FX1, FY1, FX2, FY2);

    FX1 := Round(FX1 / cRatioMiniatura);
    FX2 := Round(FX2 / cRatioMiniatura);
    FY1 := Round(FY1 / cRatioMiniatura);
    FY2 := Round(FY2 / cRatioMiniatura);
  finally
    lImage.Free;
    DicomUtils.DeleteBuffer(lBuffer);
    lBuffer := nil;
  end;
end;


procedure TImage.OpenFile(AFile: string);
begin
  TLibDicomUtilsWrapperDyn.RegisterCodecs;
  if FDicomUtils.OpenFile(AFile) then
  begin
    Reset;
    FDicomUtils.Loaded := True;
  end;
  TLibDicomUtilsWrapperDyn.UnRegisterCodecs;
end;

procedure TImage.OpenStream(AStream: TMemoryStream);
begin
  if FDicomUtils.OpenStream(AStream) then
  begin
    FDicomUtils.Loaded := True;
    Reset;
  end;
end;

procedure TImage.Reset;
(* Aplica toda la información relativa a la imágen *)
var
  lFormat: TFormatSettings;

begin
  lFormat.DecimalSeparator:= '.';
  FDicomUtils.GetDimensions;
  FDicomUtils.GetWindowLevel;
  FWW:= FDicomUtils.WindowWidth;
  FWC:= FDicomUtils.WindowCenter;
  FDicomUtils.CurrentFrame:=0;
  FDicomUtils.LoadDicomTags;
  FBitsStored := StrToIntDef(FDicomUtils.GetTagValue('0028,0101'), 0);
  FPlanarConfiguration := StrToIntDef(FDicomUtils.GetTagValue('0028,0006'), 0);
  FPixelRepresentation := StrToIntDef(FDicomUtils.GetTagValue('0028,0103'), 0);
  FRescaleSlope := StrToFloatDef(FDicomUtils.GetTagValue('0028,1053'), 0);
  FRescaleIntercept := StrToFloatDef(FDicomUtils.GetTagValue('0028,1052'), 0);
  FPhotometricInterpretation := FDicomUtils.GetTagValue('0028,0004');
  FImageType := FDicomUtils.GetTagValue('0008,0008');
  FImageType := FDicomUtils.GetTagValueFromIndex(FImageType, 2);
  FImageOrientationPatient.Delimiter := '\';
  FImageOrientationPatient.Duplicates := dupAccept;
  FImageOrientationPatient.DelimitedText := FDicomUtils.GetTagValue('0020,0037');
  FImagePositionPatient.Delimiter := '\';
  FImagePositionPatient.Duplicates := dupAccept;
  FImagePositionPatient.DelimitedText := FDicomUtils.GetTagValue('0020,0032');
  FOrientation := GetOrientation;
  FPatientName:= FDicomUtils.GetTagValue('0010,0010');
  FLaterality := FDicomUtils.GetTagValue('0020,0060');
  FSeriesNumber:= FDicomUtils.GetTagValue('0020,0011');
  FStudyInstanceUID := FDicomUtils.GetTagValue('0020,000D');
  FSeriesInstanceUID := FDicomUtils.GetTagValue('0020,000E');
  FSOPInstanceUID := FDicomUtils.GetTagValue('0008,0018');
  FSeriesDescription:= FDicomUtils.GetTagValue('0008,103E');
  MakeWindowLut(FWC, FWW);

  FModality := FDicomUtils.Modality;
  if (FModality = 'CR') or (FModality = 'XA') then
  begin
    FRowSpacing := StrToFloatDef(FDicomUtils.GetTagValue('0018,1164', 0), 0, lFormat);
    FColSpacing := StrToFloatDef(FDicomUtils.GetTagValue('0018,1164', 1), 0, lFormat);
  end
  else
  if FModality = 'US' then
  begin
    FRowSpacing := StrToFloatDef(FDicomUtils.GetTagValue('0028,0034', 0), 0, lFormat);
    FColSpacing := StrToFloatDef(FDicomUtils.GetTagValue('0028,0034', 1), 0, lFormat);
    (* Pasamos a 1/1 *)
    if (FRowSpacing > 0) then
    begin
      FColSpacing := FColSpacing / FRowSpacing;
      FRowSpacing := FRowSpacing / FRowSpacing;
    end;
  end
  else
  begin
    FRowSpacing := StrToFloatDef(FDicomUtils.GetTagValue('0028,0030', 0), 0, lFormat);
    FColSpacing := StrToFloatDef(FDicomUtils.GetTagValue('0028,0030', 1), 0, lFormat);
  end;
  FRows := Round(StrToFloatDef(FDicomUtils.GetTagValue('0028,0010', 0), 0, lFormat));
  FCols := Round(StrToFloatDef(FDicomUtils.GetTagValue('0028,0011', 0), 0, lFormat));

  if Modality = 'MG' then
    CalculateRealWidthHeight;

  (* Dependiendo de la modalidad se generan diferentes text overlays y presets *)
  SetOverlaysForModality(FModality);
end;

function TImage.MakeWindowLut(AWC, AWW: double): TLut;
var
  ymin: Integer;
  ymax: Integer;
  yrange: Integer;
  cmp5: double;
  wm1: double;
  halfwm1: double;
  lbottom: double;
  ltop: double;
  startx: Integer;
  endx: Integer;
  xi: Integer;
  x: double;
  y: double;

begin
  ymin := 0;
  ymax := 255;
  yrange := ymax - ymin;

  cmp5 := AWC - 0.5; // center
  wm1 := AWW - 1.0; // width

  // si wm1 es 0 se corre riesgo de hacer una división por cero
  // más abajo.
  //if wm1 = 0 then
  //  exit;

  halfwm1 := wm1/2.0;
  lbottom := cmp5 - halfwm1;
  ltop := cmp5 + halfwm1;

  startx := 0;
  endx := 0;


  if (FbitsStored > 8) then
  begin
    // Averiguar si signed es true/false
    if (FPixelRepresentation = 1) then
    begin
      startx := -32768; //pixelDataInformation.signed ? -32768 : 0;
      endx   := 32768; //pixelDataInformation.signed ?  32768 : 65536;
    end
    else
    begin
      startx := 0; //pixelDataInformation.signed ? -32768 : 0;
      endx   := 65536; //pixelDataInformation.signed ?  32768 : 65536;
    end;
    SetLength(Result, 65536);
  end
  else
  begin
    if (FPixelRepresentation = 1) then
    begin
      startx := -128; //pixelDataInformation.signed ? -128 : 0;
      endx   := 128; //pixelDataInformation.signed ?  128 : 256;
    end
    else
    begin
      startx := 0; //pixelDataInformation.signed ? -128 : 0;
      endx   := 256; //pixelDataInformation.signed ?  128 : 256;
    end;
    SetLength(Result, 256);
  end;

  for xi := startx to endx - 1 do
  begin
    x := xi * FrescaleSlope + FrescaleIntercept;
//    y := yrange / (1 + exp(-4*(x - FDicomUtils.WindowCenter) / FDicomUtils.WindowWidth)) + ymin + 0.5;
    y := ((x-cmp5)/wm1 + 0.5)*yrange+ymin;
    if(y < ymin) then
      y := ymin
    else
    if(y > ymax) then
      y := ymax;
    // si es MONOCHROME1 es invertido
    if FPhotometricInterpretation = 'MONOCHROME1' then
      y := ymax - y;
    // los tres valores (r,g,b) son iguales
    Result[xi and $ffff] := trunc(y);
  end;
end;

end.

