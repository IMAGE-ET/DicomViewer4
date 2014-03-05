{

Carga de estudios desde url usando threads para no bloquear la pantalla
mientras se realiza la carga de imagenes del tipo:

http://192.168.0.168/cgi-bin/gpacs/seriesImages?AccessionNumber=62996

}

unit RemoteLoader;

interface

uses
  fileutil,
  series,
  images,
  SysUtils,
  blcksock,
  httpsend,
  fpjson,
  jsonparser,
  fpg_main,
  Classes,
  commonmessages,
  strutils;

type

  TOnAddImage = procedure(ASerie: TSerie; AMemoryStream: TMemoryStream) of object;
  TOnProgress = procedure(ASerie: TSerie) of object;
  TOnException = procedure(ACode, ADescription: string) of object;
  TOnImageLoaded = procedure(AJSonObject: TJSONObject) of object;

  { TRemoteLoader }

  TRemoteLoader = class
  private
    FFinished: boolean;
    FJSon: TJSONObject;
    FOnAddStreamImageToSerie: TOnAddImage;
    FOnException: TOnException;
    FExceptionCode: string;
    FExceptionDesc: string;
    FOnLoadDicomHeader: TOnAddImage;
    FOnProgress: TOnProgress;
    FOnImageLoaded: TOnProgress;
    FSeries: TSeries;
    FSeriesCount: Integer;
    FImageCount: Integer;
    FIP: AnsiString;
    FUrl: AnsiString;
    FIdentifier: string;
    FMsgDest: TObject;
    FStudyInstanceUID: string;
    FThreadList: TThreadList;
    procedure SetIP(AUrl: AnsiString);
    procedure LoadImages(AJsonImages: TJSONStringType; AIP: string; ASerie: TSerie; AIdentifier: string);
  public
    constructor Create(AMsgDest: TObject; ASeries: TSeries; AUrl: AnsiString);
    destructor Destroy; override;
    procedure LoadFromServer;
    procedure LoadFromServerOriginal;
    procedure Execute;
    property SeriesCount: Integer read FSeriesCount;
    property ImageCount: Integer read FImageCount;
    property Finished: boolean read FFinished;
    property OnAddStreamImageToSerie: TOnAddImage read FOnAddStreamImageToSerie write FOnAddStreamImageToSerie;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnImageLoaded: TOnProgress read FOnImageLoaded write FOnImageLoaded;
    property OnLoadDicomHeader: TOnAddImage read FOnLoadDicomHeader write FOnLoadDicomHeader;
    procedure ThreadException;
    property OnException: TOnException read FOnException write FOnException;
  end;

  { TImagesLoader }

  TImagesLoader = class(TThread)
  private
    FOnException: TOnException;
    FFinished: Boolean;
    FOnAddStreamImageToSerie: TOnAddImage;
    FOnLoadDicomHeader: TOnAddImage;
    FOnProgress: TOnProgress;
    FJSONImages: TJSONArray;
    FCurrentImage: Integer;
    FIP: AnsiString;
    FImageCount: Integer;
    FJSONParser: TJSONParser;
    FOnImageLoaded: TOnProgress;
    FSerie: TSerie;
    FImgStream: TMemoryStream;
    FHttp: THTTPSend;
    FIdentifier: string;
    FImageFile: string;
    FExceptionCode: string;
    FExceptionDesc: string;
    FStudyInstanceUID: string;
    FMsgDest: TObject;
    procedure HeartBeat(Sender: TObject);
    procedure ThreadException;
    procedure DoImageToSerie;
    procedure AddStreamImageToSerie(ASerie: TSerie; AImgStream: TMemoryStream);
    procedure DoUpdateStatus;
    procedure ImageLoaded(AJSONObject: TJSONObject);
    procedure LoadImage(AUrl: string; ADownloadSize: Integer; AObject: TJSONObject);
    procedure Execute; override;
  public
    constructor Create(AMsgDest: TObject; AJSonImages: TJSONStringType; AIP: string; ASerie: TSerie; AIdentifier, AStudyInstanceUID: string);
    destructor Destroy; override;
    property Finished: Boolean read FFinished write FFinished;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnAddStreamImageToSerie: TOnAddImage read FOnAddStreamImageToSerie write FOnAddStreamImageToSerie;
    property OnImageLoaded: TOnProgress read FOnImageLoaded write FOnImageLoaded;
    property OnLoadDicomHeader: TOnAddImage read FOnLoadDicomHeader write FOnLoadDicomHeader;
    property OnException: TOnException read FOnException write FOnException;
  end;

  { TImagesLoaderList }

  TImagesLoaderList = class(TThreadList)
  public
    destructor destroy; override;
  end;

var
  gCGI: string;


implementation

{ TImagesLoaderList }

destructor TImagesLoaderList.destroy;
var
  I: Integer;
  lList: TList;
  lImagesLoader: TImagesLoader;
begin
  lList := LockList;
  try
    for I := lList.Count - 1 downto  0 do
    begin
      lImagesLoader := TImagesLoader(lList[I]);
      while not lImagesLoader.Finished do
      begin
        CheckSynchronize(100);
        lImagesLoader.Finished := True;
      end;
      lImagesLoader.Free;
    end;
  finally
    UnlockList;
  end;
  inherited destroy;
end;

{ TImagesLoader }

procedure TImagesLoader.HeartBeat(Sender: TObject);
var
  lOrigPosition: Int64;
  lSize: Int64;
begin
  if not FFinished then
  begin
    FSerie.DownloadedSize := (Sender as TBlockSocket).RecvCounter;
    fpgPostMessage(FSerie, FMsgDest, MSG_HEARTBEAT);
  end;
end;

procedure TRemoteLoader.ThreadException;
begin
  if Assigned(FOnException) then
    FOnException(FExceptionCode, FExceptionDesc);
end;
procedure TImagesLoader.ThreadException;
begin
  if Assigned(FOnException) then
    FOnException(FExceptionCode, FExceptionDesc);
end;

procedure TImagesLoader.DoImageToSerie;
begin
  if Assigned(FOnAddStreamImageToSerie) then
   FOnAddStreamImageToSerie(FSerie, FImgStream);
end;

procedure TImagesLoader.AddStreamImageToSerie(ASerie: TSerie;
  AImgStream: TMemoryStream);
var
  lPath: string;
  lImage: TImage;

begin
  if not FFinished then
  begin
    AImgStream.Position := 0;
    lImage := TImage.Create(ASerie);
    lImage.OpenStream(AImgStream);
    lPath := ExtractFilePath(ParamStr(0)) + 'tmp';
    (* Se crea el directorio tmp si no existe *)
    if not DirectoryExists(lPath) then
      FileUtil.ForceDirectory(lPath);
    (* Se crea el directorio <FIdentifier> si no existe *)
    lPath := lPath + DirectorySeparator + lImage.StudyInstanceUID;
    if not DirectoryExists(lPath) then
      FileUtil.ForceDirectory(lPath);
    (* Se crea el directorio <SeriesInstanceUID> si no existe *)
    lPath := lPath + DirectorySeparator + lImage.SeriesInstanceUID;
    if not DirectoryExists(lPath) then
      FileUtil.ForceDirectory(lPath);
    (* Se guarda la imágen *)
    lPath := lPath + DirectorySeparator + lImage.SOPInstanceUID;
    lImage.FileName := lPath;
    ASerie.Images.Add(lImage);

    AImgStream.Position := 0;
    AImgStream.SaveToFile(lImage.FileName);
    fpgPostMessage(lImage, FMsgDest, MSG_IMAGELOADED);
  end;
end;

procedure TImagesLoader.DoUpdateStatus;
begin
  if not FFinished then
  begin
    if Assigned(FOnImageLoaded) then
      FOnImageLoaded(FSerie);
  end;
end;

function SortItems(const Item1: TImage; const Item2: TImage):Integer;
var
  lNum1: Integer;
  lNum2: Integer;
begin
  lNum1 := TImage(Item1).InstanceNumber;
  lNum2 := TImage(Item2).InstanceNumber;
  if lNum1 > lNum2 then
    result := 1
  else
  if lNum1 < lNum2 then
    result := -1
  else
    result := 0;
end;

procedure TImagesLoader.ImageLoaded(AJSONObject: TJSONObject);
begin
  if not FFinished then
  begin
    (* Este método se ejecuta cuando termina de cargarse una imagen *)
    FSerie.Images.Sort(@SortItems);

    if Assigned(FOnImageLoaded) then
      FOnImageLoaded(FSerie);
  end;
end;

procedure TImagesLoader.LoadImage(AUrl: string; ADownloadSize: Integer; AObject: TJSONObject);
var
  lHttp: THTTPSend;
begin
 if FFinished then
  begin
    exit;
  end;

  lHttp := THTTPSend.Create;
  try
    (* Heartbeat sólo cuando se está cargando la 1era imagen de cada serie *)
    FHttp := lHttp;
    if FSerie.Images.Count = 0 then
    begin
      lHttp.Sock.OnHeartbeat := @HeartBeat;
      lHttp.Sock.HeartbeatRate := 5000;
    end
    else
      lHttp.Sock.OnHeartbeat := nil;

    gCommonMsg.Text := 'Loading image ' + AUrl;
    fpgPostMessage(gCommonMsg, FMsgDest, MSG_UPDATESTATUS);

    if (lHttp.HTTPMethod('GET', AUrl) and (lHttp.ResultCode = 200)) then
    begin
      if not FFinished then
      begin
        if lHttp.ResultCode = 200 then
        begin
          lHttp.Document.Position:= 0;
          AddStreamImageToSerie(FSerie, lHttp.Document);
          gCommonMsg.Text := 'Done.';
          fpgPostMessage(gCommonMsg, FMsgDest, MSG_UPDATESTATUS);
        end;
      end;
    end
    else
    begin
      FExceptionCode := IntToStr(lHttp.ResultCode);
      FExceptionDesc := 'Cannot load image: ' + AUrl;
      Terminate;
    end;
  finally
    FreeAndNil(lHttp);
  end;
end;

constructor TImagesLoader.Create(AMsgDest: TObject; AJSonImages: TJSONStringType; AIP: string; ASerie: TSerie; AIdentifier, AStudyInstanceUID: string);
begin
  FJSONParser := TJSONParser.Create(AJSonImages);
  FJSONImages := TJSONArray(FJSONParser.Parse);
  FSerie := ASerie;
  FStudyInstanceUID := AStudyInstanceUID;
  FSerie.Loading:= True;

  Priority:= tpIdle;
  FIdentifier := AIdentifier;
  FMsgDest := AMsgDest;
  FIP := AIP;
  FImageCount := FJSONImages.Count;
  FFinished := False;
  FreeOnTerminate := False;
  inherited Create(True);
end;

destructor TImagesLoader.Destroy;
begin
  FJSONImages.Free;
  FJSONParser.Free;
  FSerie.Loading := False;
  inherited Destroy;
end;

procedure TImagesLoader.Execute;
var
  A: Integer;
  lImage: string;
  lSOPInstanceUID: string;
  lUrl: string;
  lSize: LongInt;
  lFile: string;
  lDicomImage: TImage;
begin
  FFinished:= False;
  try
    for A := 0 to FJsonImages.Count - 1 do
    begin
      FCurrentImage := A + 1;
      if FFinished then
      begin
        Break;
      end;
      lImage := FJsonImages.Objects[A].Strings['IdImage'];
      lSOPInstanceUID := '';
      if FJsonImages.Objects[A].Find('SOPInstanceUID') <> nil then
        lSOPInstanceUID := FJsonImages.Objects[A].Strings['SOPInstanceUID'];
      //FSerie.DownloadSize := FJsonImages.Objects[A].Integers['FileSizeBytes'];
      lSize := FJsonImages.Objects[A].Integers['FileSizeBytes'];
      lUrl := 'http://' + string(FIP) + gCGI + '/getDicomImage?IdImage=' + lImage;

      if lSOPInstanceUID <> '' then
      begin
        lFile := ExtractFilePath(ParamStr(0));
        lFile := lFile + 'tmp' + DirectorySeparator;
        lFile := lFile + FStudyInstanceUID + DirectorySeparator;
        lFile := lFile + FSerie.SeriesInstanceUID + DirectorySeparator;
        lFile := lFile + lSOPInstanceUID;
        if FileExists(lFile) then
        begin
          (* se carga la imágen por archivo *)
          gCommonMsg.Text := 'Opening file ' + lFile;
          fpgPostMessage(gCommonMsg, FMsgDest, MSG_UPDATESTATUS);

          lDicomImage := TImage.Create(FSerie);
          lDicomImage.FileName:= lFile;

          if A = 0 then
          begin
            // sólo se abre la 1era imágen (el thumbnail)
            lDicomImage.OpenFile(lFile);
            fpgPostMessage(lDicomImage, FMsgDest, MSG_IMAGELOADED);
          end;
          FSerie.Images.Add(lDicomImage);

          gCommonMsg.Text := 'Done.';
          fpgPostMessage(gCommonMsg, FMsgDest, MSG_UPDATESTATUS);
        end
        else
        begin
          (* se carga la imágen por HTTP *)
          LoadImage(lUrl, lSize, FJsonImages.Objects[A]);
        end;
      end
      else
      begin
        (* se carga la imágen por HTTP *)
        LoadImage(lUrl, lSize, FJsonImages.Objects[A]);
      end;
    end;
  finally
    FFinished := True;
    Terminate;
  end;
end;

{ TRemoteLoader }

constructor TRemoteLoader.Create(AMsgDest: TObject; ASeries: TSeries; AUrl: AnsiString);
const
  lSearchFor = 'SERIESIMAGES?';
var
  lPos: Integer;
begin
  FSeries := ASeries;
  FUrl := AUrl;

  (* Se extrae un identificador del URL.

     Ej.: http://192.168.0.101/cgi-bin/gpacs.exe/seriesImages?AccessionNumber=24355
     El Identificador es: 24355

  *)

  lPos := Pos(lSearchFor, UpperCase(AUrl));
  if lPos > 0 then
  begin
    FIdentifier := Copy(AUrl, lPos + Length(lSearchFor), Length(AUrl));
    FIdentifier := Copy(FIdentifier, Pos('=', FIdentifier) + 1, Length(FIdentifier));
  end;

  FFinished:= False;
  FMsgDest := AMsgDest;

  FThreadList := TThreadList.Create;

  gCommonMsg.Text := 'Initializing Remote Loader...';
  fpgPostMessage(gCommonMsg, FMsgDest, MSG_UPDATESTATUS);
end;

destructor TRemoteLoader.Destroy;
var
  I: Integer;
  lList: TList;
begin
  lList := FThreadList.LockList;
  for I := lList.Count - 1 downto 0 do
  begin
    TImagesLoader(lList.Items[I]).Finished := True;
    TImagesLoader(lList.Items[I]).WaitFor;
    TImagesLoader(lList.Items[I]).Free;
  end;
  FThreadList.Free;
  FJSon.Free;
  inherited;
end;

procedure TRemoteLoader.LoadFromServer;
var
  P: TJSONParser;
  lHttp: THTTPSend;
  lJson: string;
  lResponse: TStringList;

begin
  gCommonMsg.Text := 'Getting series list...';
  fpgPostMessage(gCommonMsg, FMsgDest, MSG_UPDATESTATUS);

  SetIP(FUrl);
  lResponse := TStringList.Create;
  lHttp := THTTPSend.Create;
  try
    if (lHttp.HTTPMethod('GET', FURL)) then // and (lHttp.ResultCode = 307) then
    begin
       if lHttp.ResultCode = 307 then
       begin
        FUrl := Copy(lHttp.Headers[3], 11, Length(lHttp.Headers[3]));
        LoadFromServer;
       end;
       if lHttp.ResultCode = 200 then
       begin
         lHttp.Document.Position:= 0;
         lResponse.LoadFromStream(lHttp.Document);
         lJson := lResponse.Text;
         if AnsiContainsText(lJson,'encontrado') then
         begin
           FExceptionCode := IntToStr(lHttp.ResultCode);
           FExceptionDesc := 'Estudio no econtrado en PACS : ' + FUrl;
         end
         else
         begin
           if lJSon <> '' then
           begin
             P := TJSONParser.Create(lJson);
             try
               FJSon := TJSONObject(P.Parse);
             finally
               P.Free;
             end;
           end;
         end;
       end
       else
       begin
         FExceptionCode := IntToStr(lHttp.ResultCode);
         FExceptionDesc := 'Cannot load JSON data from : ' + FUrl;
       end;
    end
    else
    begin
      FExceptionCode := IntToStr(lHttp.ResultCode);
      FExceptionDesc := 'Cannot load JSON data from : ' + FUrl;
    end;
  finally
    lHttp.Free;
    lResponse.Free;
    gCommonMsg.Text := 'Ready.';
    fpgPostMessage(gCommonMsg, FMsgDest, MSG_UPDATESTATUS);
  end;

  if FExceptionDesc <> '' then
    Raise Exception.Create(FExceptionDesc);
end;

procedure TRemoteLoader.LoadFromServerOriginal;
var
  P: TJSONParser;
  lHttp: THTTPSend;
  lJson: string;
  lResponse: TStringList;

begin

  SetIP(FUrl);
  lResponse := TStringList.Create;
  lHttp := THTTPSend.Create;
  try
    if (lHttp.HTTPMethod('GET', FURL)) then // and (lHttp.ResultCode = 307) then
    begin
      if lHttp.ResultCode = 307 then
      begin
        FUrl := Copy(lHttp.Headers[3], 11, Length(lHttp.Headers[3]));
        LoadFromServer;
       END;
      lHttp.Document.Position:= 0;
      lResponse.LoadFromStream(lHttp.Document);
      lJson := lResponse.Text;
      if lJSon <> '' then
      begin
        P := TJSONParser.Create(lJson);
        try
          FJSon := TJSONObject(P.Parse);
        finally
          P.Free;
        end;
      end;
    end
    else
    begin
      FExceptionCode := IntToStr(lHttp.ResultCode);
      FExceptionDesc := '2 Cannot get JSON data from : ' + FUrl;
    end;
  finally
    lHttp.Free;
    lResponse.Free;
  end;
end;

procedure TRemoteLoader.Execute;
var
  lJsonSeries: TJSONArray;
  lJsonImages: TJSONArray;
  I: Integer;
  lSerie: TSerie;

begin
  (* Recorre la lista de series *)
  LoadFromServer;
  lJsonSeries := FJSon.Arrays['rows'];
  FStudyInstanceUID := FJSon.Strings['StudyInstanceUID'];
  FSeriesCount := lJsonSeries.Count;
  if FSeriesCount > 0 then
  begin
    for I := 0 to lJsonSeries.Count - 1 do
    begin
      (* Se crea la serie y se asigna el SeriesInstanceUID *)
      lSerie := TSerie.Create(FSeries);
      lSerie.SeriesInstanceUID := lJsonSeries.Objects[I].strings['seriesinstanceuid'];
      FSeries.Add(lSerie);
    end;

    for I := 0 to FSeries.Count - 1 do
    begin
      lJsonImages := lJsonSeries.Objects[I].Arrays['images'];

      (* Se cargan las imagenes *)
      LoadImages(lJsonImages.AsJSON, FIP, FSeries[I], FIdentifier);
    end;
  end;
  FFinished := True;
end;

procedure TRemoteLoader.SetIP(AUrl: AnsiString);
begin
  FIP := '';
  gCGI := Copy(AUrl, AnsiPos('/GPACS', UpperCase(AUrl)) + 1, Length(AUrl));
  gCGI := '/cgi-bin/' + Copy(gCGI, 0, AnsiPos('/', gCGI) - 1);

  AUrl := AnsiString(Trim(AnsiUpperCase(string(AUrl))));
  if AnsiPos('HTTP://', string(AUrl)) >0 then
  begin
    FIP := Copy(AUrl, 8, Length(AUrl));
    FIP := Copy(FIP, 0, AnsiPos('/', string(FIP)) - 1);
  end;
end;

procedure TRemoteLoader.LoadImages(AJsonImages: TJSONStringType; AIP: string; ASerie: TSerie; AIdentifier: string);
var
  lImagesLoader: TImagesLoader;
begin
  lImagesLoader := TImagesLoader.Create(FMsgDest, AJsonImages, AIP, ASerie, AIdentifier, FStudyInstanceUID);
  lImagesLoader.OnProgress := FOnProgress;
  lImagesLoader.OnAddStreamImageToSerie:= FOnAddStreamImageToSerie;
  lImagesLoader.OnImageLoaded:= FOnImageLoaded;
  lImagesLoader.OnLoadDicomHeader:= FOnLoadDicomHeader;
  lImagesLoader.OnException := FOnException;
  FThreadList.Add(lImagesLoader);
  lImagesLoader.Start;
end;

end.

