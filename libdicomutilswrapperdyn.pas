unit libDicomUtilsWrapperDyn;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  dynlibs;

type
  TLongOrdinal = longword;

  { TLibDicomUtilsWrapperDyn }
  TOnSerie = procedure(const ASeriesDescription: PAnsiChar; const ASeriesNumber: PAnsiChar; const AModality: PAnsiChar; ACaller: Pointer); cdecl;
  TOnStudy = procedure (const AStudyDescription: PAnsiChar; const AStudyInstanceUID: PAnsiChar; const AAccession: PAnsiChar; const ADate: PAnsiChar; ACaller: Pointer); cdecl;
  TOnPatient = procedure (const APatient: PAnsiChar; const APatientId: PAnsiChar; ACaller: Pointer); cdecl;
  TOnImage = procedure (const AImage: PAnsiChar; const AInstanceNumber: PAnsiChar; ACaller: Pointer); cdecl;

  TLibDicomUtilsWrapperDyn = class
  private
    FAngle: Integer;
    FCurrentFrame: Integer;
    FDicomImage: Pointer;
    FOrigWindowWidth: Double;
    FOrigWindowCenter: Double;
    FWindowCenter: double;
    FWindowWidth: double;
    FImgWidth: Integer;
    FImgHeight: Integer;
    FLoaded: Boolean;
    FDicomTags: TStringList;
    FHFlipped: boolean;
    FVFlipped: boolean;
    function GetFrameCount: Integer;
    function GetImageDepth: Integer;
    function GetModality: string;
    function GetPolarity: Integer;
    procedure SetAngle(AValue: Integer);
    procedure SetCurrentFrame(AValue: Integer);
    procedure SetDicomImage(const AValue: Pointer);
    procedure SetPolarity(const AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function LoadDicomImageToBuffer(var ABuffer: Pointer; out ABufSize: TLongOrdinal; onlyPixelData: boolean): Integer;
    function LoadDicomImageToBufferEx(var ABuffer: Pointer; var ABufSize: TLongOrdinal; left, top: double; var width, height: double; zoom: double = -1; onlyPixelData: boolean = false): Integer;
    function FileIsDicom(AFile: AnsiString): Boolean;
    function GetTagIndex(ATag: string; AStartFrom: Integer = 0): Integer;
    function GetTagValueFromIndex(AValue: string; AIndex: Integer): string;
    function GetTagName(ATag: string): string;
    function GetTagVR(ATag: string): string;
    function GetTagGroupElement(ATag: string): string;
    function GetTagValue(ATag: string; AIndex: Integer = -1; AStartFrom: Integer = 0): string;
    function GetAngle: Integer;
    procedure ReadDicomDir(AFile: string; AOnStudy: TOnStudy; AOnPatient: TOnPatient;
      AOnSerie: TOnSerie; AOnImage: TOnImage; ACaller: Pointer);
    procedure SaveToFile(AFile: string);
    procedure DeleteBuffer(var ABuffer: Pointer);
    procedure GetWindowLevel;
    procedure GetDimensions;
    procedure Release;
    procedure LoadDicomTags;
    procedure ReplacePixelData(ABuffer: Pointer; ABufSize: longint; ATmpFile: string);
    procedure RectRoi(X, Y, W, H: Integer; out APromedio, AStdDev: double);
    procedure CopyDicomImage(const AImage: Pointer; var ANewImage: Pointer);
    procedure BmpToDicom(
      const bmpBuffer: Pointer;
      bmpBufSize, aRows, aCols, aBitsAllocated, aBitsStored, aHighBit, aSamplesPerPixel: LongInt;
      APhotometricInterpretation, AFile: string);
    procedure HFlip(AFlip: boolean);
    procedure VFlip(AFlip: boolean);
    procedure SetZoom(AZoom: double);
    function GetDensity(X, Y: Integer): Integer;
    function OpenFile(AFile: AnsiString): Boolean;
    function OpenHeaderFromFile(AFile: AnsiString): Boolean;
    function OpenDicomDir(AFile: AnsiString): Boolean;
    function OpenStream(AStream: TMemoryStream): Boolean;
    function OpenHeaderFromStream(AStream: TMemoryStream): Boolean;
    function SamplesPerPixel: Integer;
    class procedure RegisterCodecs;
    class procedure UnRegisterCodecs;
    property WindowWidth: double read FWindowWidth write FWindowWidth;
    property WindowCenter: double read FWindowCenter write FWindowCenter;
    property ImgWidth: Integer read FImgWidth;
    property ImgHeight: Integer read FImgHeight;
    property Loaded: Boolean read FLoaded write FLoaded;
    property OrigWindowWidth: Double read FOrigWindowWidth;
    property OrigWindowCenter: Double read FOrigWindowCenter;
    property ImageDepth: Integer read GetImageDepth;
    property DicomImage: Pointer read FDicomImage write SetDicomImage;
    property Polarity: Integer read GetPolarity write SetPolarity;
    property Modality: string read GetModality;
    property DicomTags: TStringList read FDicomTags;
    property FrameCount: Integer read GetFrameCount;
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
    property HFlipped: boolean read FHFlipped;
    property VFlipped: boolean read FVFlipped;
    property Angle: Integer read FAngle write SetAngle;
  end;

type
  TCFindCallBack = procedure(const AText: PAnsiChar); cdecl;
  TRegisterCodecs = procedure(); cdecl;
  TUnRegisterCodecs = procedure(); cdecl;
  TcopyDicomImage = function(const ADicomImage: Pointer; var ACopy: Pointer): boolean; cdecl;
  TopenDicomBuffer = function(const ABuffer: PAnsiChar; const ABufSize: LongInt; var ADicomImage: Pointer): boolean; cdecl;
  TopenDicomHeaderFromBuffer = function(const ABuffer: Pointer; const ABufSize: LongInt; var output: PAnsiChar): boolean; cdecl;
  TReadDicomDir = procedure(const AFile: PAnsiChar; AOnStudy: TOnStudy; AOnPatient: TOnPatient; AOnSerie: TOnSerie; AOnImage: TOnImage; ACaller: Pointer); cdecl;
  TopenDicomFile = function(const AFile: PAnsiChar; var ADicomImage: Pointer): boolean; cdecl;
  TopenDicomHeaderFromFile = function(const AFile: PAnsiChar; var output: PAnsiChar): boolean; cdecl;
  TopenDicomDir = function(const AFile: PAnsiChar; var output: PAnsiChar): boolean; cdecl;
  TreplacePixelData = procedure(const ADicomImage: Pointer; ABuffer: Pointer; ABufSize: longint; ATmpFile: PAnsiChar); cdecl;
  TgetDicomImage = function(const ADicomImage: Pointer; var buffer: Pointer;
    var bufSize: longint; ALeft, ATop: double; var AWidth, AHeight: double): integer; cdecl;
  TapplyWindowLevel = procedure(const ADicomImage: Pointer; ACenter: double; AWidth: double); cdecl;
  THFlip = procedure(const ADicomImage: Pointer); cdecl;
  TVFlip = procedure(const ADicomImage: Pointer); cdecl;
  TgetWindowLevel = procedure(const ADicomImage: Pointer; var ACenter: double; var AWidth: double); cdecl;
  TgetPolarity = function(const ADicomImage: Pointer): Integer; cdecl;
  TsetPolarity = procedure(const ADicomImage: Pointer; APolarity: Integer); cdecl;
  TgetDimensions = procedure(const ADicomImage: Pointer; var Width: integer; var Height: integer); cdecl;
  TgetDicomImageDepth = function(const ADicomImage: Pointer): integer; cdecl;
  TgetDicomSamplesPerPixel = function(const ADicomImage: Pointer): integer; cdecl;
  TsetDicomZoom = procedure(const ADicomImage: Pointer; AZoom: double); cdecl;
  TgetDicomZoom = function(const ADicomImage: Pointer): double; cdecl;
  TgetDicomDensity = procedure(const ADicomImage: Pointer; x, y: Integer; var density: Integer); cdecl;
  TgetDicomTags = procedure(const ADicomImage: Pointer; var output: PAnsiChar); cdecl;
  TgetDicomImageBuffer = function(const ADicomImage: Pointer; var buffer: Pointer; var bufSize: TLongOrdinal; onlyPixelData: boolean): Integer; cdecl;
  TgetDicomImageBufferEx = function(const ADicomImage: Pointer; var buffer: pointer; var bufSize: TLongOrdinal; left, top: double; var width, height: double; zoom: double; onlyPixelData: boolean = false): Integer; cdecl;
  TgetDicomRectRoi = procedure(const ADicomImage: Pointer; x, y, w, h: Integer; var APromedio: double; var AStdDev: double); cdecl;
  TgetAngle = function(const ADicomImage: Pointer): Integer; cdecl;
  TDicomDisposeStr = procedure(var AString: PAnsiChar); cdecl;
  TinitDicomCFind = procedure(var CFindObj: Pointer); cdecl;
  TDicomCFindPerformQuery = procedure(const CFindObj: Pointer; const CFindCallback: TCFindCallBack; APeer: PAnsiChar; APort: Integer; AOurAETitle, APeerAETitle, AAbstractSyntax, AOverrideKeys: PAnsiChar); cdecl;
  TdeleteDicomCFInd = procedure(const CFIndObj: Pointer); cdecl;
  TdeleteBuffer = procedure(const ABuffer: Pointer); cdecl;
  TfileIsDicom = function(const AFile: PAnsiChar): boolean; cdecl;
  TreleaseDicomImage = procedure(const ADicomImage: Pointer); cdecl;
  TsetDicomAngle = procedure(const ADicomImage: Pointer; AAngle: Integer); cdecl;
  TDicomSaveToFile = procedure(const ADicomImage: Pointer; const AFile: PAnsiChar); cdecl;
  TgetFrameCount = function(const ADicomImage: Pointer): Integer; cdecl;
  TsetCurrentFrame = procedure(const ADicomImage: Pointer; AFrame: Integer); cdecl;
  TbmpToDicom = procedure (const bmpBuffer: Pointer; bmpBufSize, aRows, aCols, aBitsAllocated, aBitsStored, aHighBit, aSamplesPerPixel: Integer; aFotometricInterpretation, AFile: PAnsiChar); cdecl;


var
  gLibHandle: TLibHandle;

implementation

{ TLibDicomUtilsWrapperDyn }

constructor TLibDicomUtilsWrapperDyn.Create;
begin
  FDicomImage := nil;
  FDicomTags := TStringList.Create;
  FHFlipped:= False;
  FVFlipped:= False;
end;

destructor TLibDicomUtilsWrapperDyn.Destroy;
begin
  FDicomTags.Free;
  inherited;
end;

class procedure TLibDicomUtilsWrapperDyn.RegisterCodecs;
var
  lRegisterCodecs: TRegisterCodecs;
begin
  lRegisterCodecs := nil;
  Pointer(lRegisterCodecs) := GetProcAddress(gLibHandle, 'registerCodecs');
  if @lRegisterCodecs <> nil then
  begin
    lRegisterCodecs;
  end;
end;

class procedure TLibDicomUtilsWrapperDyn.UnRegisterCodecs;
var
  lUnRegisterCodecs: TUnRegisterCodecs;
begin
  lUnRegisterCodecs := nil;
  Pointer(lUnRegisterCodecs) := GetProcAddress(gLibHandle, 'unRegisterCodecs');
  if @lUnRegisterCodecs <> nil then
  begin
    lUnRegisterCodecs;
  end;
end;

function TLibDicomUtilsWrapperDyn.FileIsDicom(AFile: AnsiString): Boolean;
var
  lFileIsDicom: TfileIsDicom;

begin
  lFileIsDicom := nil;
  Pointer(lFileIsDicom) := GetProcAddress(gLibHandle, 'fileIsDicom');
  if @lFileIsDicom <> nil then
  begin
    Result := lFileIsDicom(PAnsiChar(AFile));
  end;
end;

function TLibDicomUtilsWrapperDyn.GetDensity(X, Y: Integer): Integer;
var
  lDensity: TgetDicomDensity;
begin
  lDensity := nil;
  Result := 0;
  Pointer(lDensity) := GetProcAddress(gLibHandle, 'getDicomDensity');
  if @lDensity <> nil then
  begin
    lDensity(FDicomImage, x, y, result);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.LoadDicomTags;
var
  lTags: PAnsiChar;
  lgetTags: TgetDicomTags;
  ldisposeStr: TDicomDisposeStr;

begin
  lgetTags:= nil;
  ldisposeStr:= nil;
  lTags := '';
  Pointer(lgetTags) := GetProcAddress(gLibHandle, 'getDicomTags');
  if lgetTags <> nil then
  begin
    lgetTags(FDicomImage, lTags);
  end;
  FDicomTags.clear;
  FDicomTags.Text := string(lTags);

  Pointer(ldisposeStr) := GetProcAddress(gLibHandle, 'disposeStr');
  if ldisposeStr <> nil then
  begin
    ldisposeStr(lTags);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.ReplacePixelData(ABuffer: Pointer;
  ABufSize: longint; ATmpFile: string);
var
  lReplacePixelData: TreplacePixelData;
begin
  lReplacePixelData := nil;
  Pointer(lReplacePixelData) := GetProcAddress(gLibHandle, 'replacePixelData');
  if lReplacePixelData <> nil then
  begin
    lReplacePixelData(FDicomImage, ABuffer, ABufSize, PAnsiChar(ATmpFile));
  end;
end;

procedure TLibDicomUtilsWrapperDyn.RectRoi(X, Y, W, H: Integer; out APromedio,
  AStdDev: double);
var
  lGetRectRoi: TgetDicomRectRoi;

begin
  lGetRectRoi := nil;
  Pointer(lGetRectRoi) := GetProcAddress(gLibHandle, 'getDicomRectRoi');
  if @lGetRectRoi <> nil then
  begin
    lGetRectRoi(FDicomImage, X, Y, W, H, APromedio, AStdDev);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.CopyDicomImage(const AImage: Pointer;
  var ANewImage: Pointer);
var
  lCopyDicomImage: TcopyDicomImage;
begin
  lCopyDicomImage := nil;
  Pointer(lCopyDicomImage) := GetProcAddress(gLibHandle, 'copyDicomImage');
  if lCopyDicomImage <> nil then
  begin
    lCopyDicomImage(AImage, ANewImage);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.BmpToDicom(
  const bmpBuffer: Pointer;
  bmpBufSize, aRows, aCols, aBitsAllocated, aBitsStored, aHighBit, aSamplesPerPixel: LongInt;
  APhotometricInterpretation, AFile: string);
var
  lBmpToDicom: TbmpToDicom;
begin
  lBmpToDicom := nil;
  Pointer(lBmpToDicom) := GetProcAddress(gLibHandle, 'bmpToDicom');
  if lBmpToDicom <> nil then
  begin
    lBmpToDicom(
      bmpBuffer, bmpBufSize,
      aRows, aCols, aBitsAllocated, aBitsStored, aHighBit, aSamplesPerPixel,
      PAnsiChar(APhotometricInterpretation),
      PAnsiChar(AFile));
  end;
end;

procedure TLibDicomUtilsWrapperDyn.HFlip(AFlip: boolean);
var
  lHFlip: THFlip;
begin
  lHFlip := nil;
  Pointer(lHFlip) := GetProcAddress(gLibHandle, 'HFlip');
  if lHFlip <> nil then
  begin
    lHFlip(FDicomImage);
    FHFlipped := AFlip;
  end;
end;

procedure TLibDicomUtilsWrapperDyn.VFlip(AFlip: boolean);
var
  lVFlip: TVFlip;
begin
  lVFlip := nil;
  Pointer(lVFlip) := GetProcAddress(gLibHandle, 'VFlip');
  if lVFlip <> nil then
  begin
    lVFlip(FDicomImage);
    FVFlipped := AFlip;
  end;
end;

procedure TLibDicomUtilsWrapperDyn.SetZoom(AZoom: double);
var
  lSetDicomZoom: TsetDicomZoom;

begin
  lSetDicomZoom:= nil;
  Pointer(lSetDicomZoom) := GetProcAddress(gLibHandle, 'setDicomZoom');
  if lSetDicomZoom <> nil then
  begin
    lSetDicomZoom(FDicomImage, AZoom);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.DeleteBuffer(var ABuffer: Pointer);
var
  lDeleteBuffer: TdeleteBuffer;
begin
  lDeleteBuffer := nil;
  Pointer(lDeleteBuffer) := GetProcAddress(gLibHandle, 'deleteBuffer');
  if lDeleteBuffer <> nil then
  begin
    lDeleteBuffer(ABuffer);
    ABuffer := nil;
  end;
end;

procedure TLibDicomUtilsWrapperDyn.GetDimensions;
var
  lgetDimensions: TgetDimensions;

begin
  lgetDimensions := nil;
  Pointer(lgetDimensions) := GetProcAddress(gLibHandle, 'getDicomDimensions');
  if @lgetDimensions <> nil then
  begin
    lgetDimensions(FDicomImage, FImgWidth, FImgHeight);
  end;
end;

function TLibDicomUtilsWrapperDyn.GetImageDepth: Integer;
var
  lgetDepth: TgetDicomImageDepth;

begin
  lgetDepth := nil;
  Pointer(lgetDepth) := GetProcAddress(gLibHandle, 'getDicomImageDepth');
  if @lgetDepth <> nil then
  begin
    Result := lgetDepth(FDicomImage);
  end;
end;

function TLibDicomUtilsWrapperDyn.GetModality: string;
begin
  Result := GetTagValue('0008,0060');
end;

function TLibDicomUtilsWrapperDyn.GetPolarity: Integer;
var
  lgetPolarity: TgetPolarity;

begin
  lgetPolarity := nil;
  Pointer(lgetPolarity) := GetProcAddress(gLibHandle, 'getDicomPolarity');
  if @lgetPolarity <> nil then
  begin
    Result := lgetPolarity(FDicomImage);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.SetAngle(AValue: Integer);
var
  lsetDicomAngle: TsetDicomAngle;
begin
  lsetDicomAngle := nil;
  Pointer(lsetDicomAngle) := GetProcAddress(gLibHandle, 'setDicomAngle');
  if lsetDicomAngle <> nil then
  begin
    lsetDicomAngle(FDicomImage, AValue);
    FAngle := AValue;
  end;
end;

procedure TLibDicomUtilsWrapperDyn.SetCurrentFrame(AValue: Integer);
var
  lSetCurrentFrame: TSetCurrentFrame;

begin
  if AValue >= GetFrameCount then
    exit;
  FCurrentFrame := AValue;

  lSetCurrentFrame := nil;
  Pointer(lSetCurrentFrame) := GetProcAddress(gLibHandle, 'setCurrentFrame');
  if @lSetCurrentFrame <> nil then
  begin
    lSetCurrentFrame(FDicomImage, FCurrentFrame);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.SetDicomImage(const AValue: Pointer);
begin
  FDicomImage:=AValue;
end;

function TLibDicomUtilsWrapperDyn.GetFrameCount: Integer;
var
  lgetFrameCount: TgetFrameCount;

begin
  lgetFrameCount := nil;
  Pointer(lgetFrameCount) := GetProcAddress(gLibHandle, 'getFrameCount');
  if @lgetFrameCount <> nil then
  begin
    Result := lgetFrameCount(FDicomImage);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.SetPolarity(const AValue: Integer);
var
  lsetPolarity: TsetPolarity;

begin
  lsetPolarity := nil;
  Pointer(lsetPolarity) := GetProcAddress(gLibHandle, 'setDicomPolarity');
  if @lsetPolarity <> nil then
  begin
    lsetPolarity(FDicomImage, AValue);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.GetWindowLevel;
var
  lgetWindowLevel: TgetWindowLevel;

begin
  lgetWindowLevel := nil;
  Pointer(lgetWindowLevel) := GetProcAddress(gLibHandle, 'getDicomWindowLevel');
  if @getWindowLevel <> nil then
  begin
    lgetWindowLevel(FDicomImage, FWindowCenter, FWindowWidth);
  end;
end;

function TLibDicomUtilsWrapperDyn.OpenFile(AFile: AnsiString): Boolean;
var
  lOpenFile: TopenDicomFile;
begin
  lOpenFile := nil;
  Result := False;
  Pointer(lOpenFile) := GetProcAddress(gLibHandle, 'openDicomFile');
  if lOpenFile <> nil then
  begin
    Result := lOpenFile(PAnsiChar(AFile), FDicomImage);
  end;
end;

function TLibDicomUtilsWrapperDyn.OpenHeaderFromFile(AFile: AnsiString): Boolean;
var
  lOpenFile: TopenDicomHeaderFromFile;
  lTags: PAnsiChar;
begin
  lOpenFile := nil;
  Result := False;
  Pointer(lOpenFile) := GetProcAddress(gLibHandle, 'openDicomHeaderFromFile');
  if lOpenFile <> nil then
  begin
    lTags := '';
    Result := lOpenFile(PAnsiChar(AFile), lTags);
    if Result then
      DicomTags.Text:= string(lTags);
  end;
end;

function TLibDicomUtilsWrapperDyn.OpenDicomDir(AFile: AnsiString): Boolean;
var
  lOpenDicomDir: TopenDicomDir;
  lDicomTags: PAnsiChar;
  ldisposeStr: TDicomDisposeStr;
begin
  lOpenDicomDir := nil;
  ldisposeStr := nil;
  Result := False;
  Pointer(lOpenDicomDir) := GetProcAddress(gLibHandle, 'openDicomDir');
  lDicomTags := nil;
  if lOpenDicomDir <> nil then
  begin
    Result := lOpenDicomDir(PAnsiChar(AFile), lDicomTags);
    FDicomTags.Clear;
    FDicomTags.Text := string(lDicomTags);

    Pointer(ldisposeStr) := GetProcAddress(gLibHandle, 'disposeStr');
    if ldisposeStr <> nil then
    begin
      ldisposeStr(lDicomTags);
    end;
  end;
end;

function TLibDicomUtilsWrapperDyn.OpenStream(AStream: TMemoryStream): Boolean;
var
  lOpenDicomBuffer: TopenDicomBuffer;

begin
  Result := False;
  lOpenDicomBuffer := nil;
  AStream.Position := 0;

  Pointer(lOpenDicomBuffer) := GetProcAddress(gLibHandle, 'openDicomBuffer');
  if lOpenDicomBuffer <> nil then
  begin
    Result := lOpenDicomBuffer(AStream.Memory, AStream.Size, FDicomImage);
  end;
end;


function TLibDicomUtilsWrapperDyn.OpenHeaderFromStream(AStream: TMemoryStream
  ): Boolean;
var
  lOpenDicomHeaderFromBuffer: TopenDicomHeaderFromBuffer;
  lTags: PAnsiChar;

begin
  Result := False;
  lOpenDicomHeaderFromBuffer := nil;
  AStream.Position := 0;

  Pointer(lOpenDicomHeaderFromBuffer) := GetProcAddress(gLibHandle, 'openDicomHeaderFromBuffer');
  if lOpenDicomHeaderFromBuffer <> nil then
  begin
    lTags := '';
    Result := lOpenDicomHeaderFromBuffer(AStream.Memory, AStream.Size, lTags);
    if Result then
      DicomTags.Text:= string(lTags);
  end;
end;

function TLibDicomUtilsWrapperDyn.SamplesPerPixel: Integer;
var
  lSamplesPerPixel: TgetDicomSamplesPerPixel;
begin
  lSamplesPerPixel := nil;
  Result := 0;
  Pointer(lSamplesPerPixel) := GetProcAddress(gLibHandle, 'getDicomSamplesPerPixel');
  if lSamplesPerPixel <> nil then
  begin
    Result := lSamplesPerPixel(FDicomImage);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.Release;
var
  lReleaseDicomImage: TreleaseDicomImage;
begin
  lReleaseDicomImage := nil;
  Pointer(lReleaseDicomImage) := GetProcAddress(gLibHandle, 'releaseDicomImage');
  if lReleaseDicomImage <> nil then
  begin
    lReleaseDicomImage(FDicomImage);
    FDicomImage:= nil;
    FLoaded:= False;
  end;
end;

function TLibDicomUtilsWrapperDyn.LoadDicomImageToBuffer(var ABuffer: Pointer; out ABufSize: TLongOrdinal; onlyPixelData: boolean): Integer;
var
  lgetDicomImageBuffer: TgetDicomImageBuffer;
  lBufSize: TLongOrdinal;

begin
  ABuffer := nil;
  lBufSize := 0;
  lgetDicomImageBuffer := nil;
  Pointer(lgetDicomImageBuffer) := GetProcAddress(gLibHandle, 'getDicomImageBuffer');
  if lgetDicomImageBuffer <> nil then
  begin
    Result := lgetDicomImageBuffer(FDicomImage, ABuffer, lBufSize, onlyPixelData);
    ABufSize:= lBufSize;
  end;
end;

function TLibDicomUtilsWrapperDyn.LoadDicomImageToBufferEx(
  var ABuffer: Pointer; var ABufSize: TLongOrdinal; left, top: double; var width,
  height: double; zoom: double = -1; onlyPixelData: boolean = false): Integer;
var
  lgetDicomImageBufferEx: TgetDicomImageBufferEx;

begin
  ABuffer := nil;
  ABufSize := 0;
  lgetDicomImageBufferEx := nil;
  Pointer(lgetDicomImageBufferEx) := GetProcAddress(gLibHandle, 'getDicomImageBufferEx');
  if lgetDicomImageBufferEx <> nil then
  begin
    Result := lgetDicomImageBufferEx(FDicomImage, ABuffer, ABufSize, left, top, width, height, zoom, onlyPixelData);
  end;
end;

function TLibDicomUtilsWrapperDyn.GetTagValue(ATag: string; AIndex: Integer = -1; AStartFrom: Integer = 0): string;
var
  I: Integer;
  Y,M,D: LongInt;
  lTag: string;
  lFrom: Integer;
begin
  Result := '';
  if Pos('/', ATag) > 0 then
  begin
    (* Busca el primer elemento en strings del tipo
       0020,0010/0052,0001 o sea debe encontrar 0020,0010 *)
    lTag := Copy(ATag, 1, Pos('/', ATag) - 1);
    lFrom := GetTagIndex(lTag, AStartFrom);
    (* Ahora hace una búsqueda a partir del siguiente elemento,
       o sea 0052,0001 *)
    lTag := Copy(ATag, Pos('/', ATag) + 1, Length(ATag));
    Result := GetTagValue(lTag, -1, lFrom);
    Exit;
  end;

  for I := AStartFrom to FDicomTags.Count - 1 do
  begin
    if I < 0 then
      continue;
    lTag := FDicomTags[I];
    if Pos(UpperCase(ATag), UpperCase(lTag)) > 0 then
    begin
      Result := Trim(lTag);
      Result := Copy(Result, 15, Pos('#', Result) - 16);
      Result := AnsiReplaceStr(Result, '[', '');
      Result := AnsiReplaceStr(Result, ']', '');
      Result := Trim(Result);
      if GetTagVR(ATag) = 'DA' then
      begin
        if TryStrToInt(Copy(Result, 0, 4), Y) and TryStrToInt(Copy(Result, 5, 2), M) and
          TryStrToInt(Copy(Result, 7, 2), D) then
          Result := DateToStr(EncodeDate(Y, M, D));
      end;
      if AIndex > -1 then
        Result := GetTagValueFromIndex(Result, AIndex);
      Break;
    end;
  end;
end;

function TLibDicomUtilsWrapperDyn.GetAngle: Integer;
var
  lgetAngle: TgetAngle;

begin
  lgetAngle := nil;
  Result := 0;
  Pointer(lgetAngle) := GetProcAddress(gLibHandle, 'getDicomAngle');
  if @getWindowLevel <> nil then
  begin
    Result := lgetAngle(FDicomImage);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.ReadDicomDir(AFile: string; AOnStudy: TOnStudy; AOnPatient: TOnPatient;
      AOnSerie: TOnSerie; AOnImage: TOnImage; ACaller: Pointer);
var
  lReadDicomDir: TReadDicomDir;

begin
  lReadDicomDir := nil;
  Pointer(lReadDicomDir) := GetProcAddress(gLibHandle, 'ReadDicomDir');
  if lReadDicomDir <> nil then
  begin
    lReadDicomDir(PAnsiChar(AFile), AOnStudy, AOnPatient, AOnSerie, AOnImage, ACaller);
  end;
end;

procedure TLibDicomUtilsWrapperDyn.SaveToFile(AFile: string);
var
  lDicomSaveToFile: TDicomSaveToFile;

begin
  lDicomSaveToFile := nil;
  Pointer(lDicomSaveToFile) := GetProcAddress(gLibHandle, 'DicomSaveToFile');
  if lDicomSaveToFile <> nil then
  begin
    lDicomSaveToFile(FDicomImage, PAnsiChar(AFile));
  end;
end;


function TLibDicomUtilsWrapperDyn.GetTagGroupElement(ATag: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FDicomTags.Count - 1 do
  begin
    if Pos(ATag, FDicomTags[I]) > 0 then
    begin
      Result := Copy(FDicomTags[I], 0, Pos(')', FDicomTags[I]) - 1);
      Result := AnsiReplaceStr(REsult, '(', '');
      Break;
    end;
  end;
end;

function TLibDicomUtilsWrapperDyn.GetTagVR(ATag: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FDicomTags.Count - 1 do
  begin
    if Pos(ATag, FDicomTags[I]) > 0 then
    begin
      Result := TrimLeft(FDicomTags[I]);
      Result := Copy(Result, 13, 2);
      Result := Trim(Result);
      Break;
    end;
  end;
end;

function TLibDicomUtilsWrapperDyn.GetTagName(ATag: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FDicomTags.Count - 1 do
  begin
    if Pos(ATag, FDicomTags[I]) > 0 then
    begin
      if Pos('#', FDicomTags[I]) > 0 then
      begin
        Result := TrimLeft(FDicomTags[I]);
        Result := Copy(Result, RPos('#', Result) + 1, Length(Result));
        Result := Trim(Result);
        Result := Copy(Result, Pos(',', Result) + 1, Length(Result));
        Result := Trim(Result);
        Result := Copy(Result, Pos(' ', Result) + 1, Length(Result));
        Result := Trim(Result);
      end;
      Break;
    end;
  end;
end;

function TLibDicomUtilsWrapperDyn.GetTagValueFromIndex(AValue: string; AIndex: Integer): string;
var
  lStr: TStringList;
begin
  Result := '';
  lStr := TStringList.Create;
  try
    lStr.Delimiter:= '\';
    lStr.DelimitedText:= AValue;
    if lStr.Count - 1 >= AIndex then
      Result := lStr[AIndex];
  finally
    lStr.Free;
  end;
end;

function TLibDicomUtilsWrapperDyn.GetTagIndex(ATag: string; AStartFrom: Integer = 0): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AStartFrom to FDicomTags.Count - 1 do
  begin
    if I < 0 then
      continue;
    if Pos(ATag, FDicomTags[I]) > 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

end.
