unit dicomdirloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  commonmessages,
  libDicomUtilsWrapperDyn,
  fgl,
  strutils,
  Series,
  Images,
  fpg_main,
  fpg_base;

type

  TOnNewImage = procedure(AImage: TImage) of object;

  { TDicomDirLoader }

  TDicomDirLoader = class(TThread)
  private
    FFinished: boolean;
    FFile: string;
    FOnImage: TOnNewImage;
    FRootPath: string;
    FSeries: TSeries;
    FDicomUtils: TLibDicomUtilsWrapperDyn;
    FPatientName: string;
    FStudyDescription: string;
    FMsgDest: TObject;
  public
    constructor Create(ASeries: TSeries; AFile: string; AMsgDest: TObject);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Sync;
    property Finished: boolean read FFinished;
    property StudyDescription: string read FStudyDescription write FStudyDescription;
    property PatientName: string read FPatientName write FPatientName;
    property RootPath: string read FRootPath write FRootPath;
    property OnNewImage: TOnNewImage read FOnImage write FOnImage;
  end;


implementation

{ TDicomDirLoader }

function SortItems(const Item1: TImage; const Item2: TImage):Integer;
var
  lNum1: Integer;
  lNum2: Integer;
begin
  lNum1 := Item1.InstanceNumber;
  lNum2 := Item2.InstanceNumber;
  if lNum1 > lNum2 then
    result := 1
  else
  if lNum1 < lNum2 then
    result := -1
  else
    result := 0;
end;

{ TDicomDirSerie }


constructor TDicomDirLoader.Create(ASeries: TSeries; AFile: string; AMsgDest: TObject);
begin
  FDicomUtils := TLibDicomUtilsWrapperDyn.Create;
  FRootPath := ExtractFilePath(AFile);

  FFile := AFile;
  Priority:= tpIdle;
  FSeries := ASeries;
  FFinished := False;
  FMsgDest := AMsgDest;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TDicomDirLoader.Destroy;
begin
  FDicomUtils.Free;
  inherited Destroy;
end;

procedure OnStudy(const AStudyDescription: PAnsiChar; const AStudyInstanceUID: PAnsiChar; const AAccession: PAnsiChar; const ADate: PAnsiChar; ACaller: Pointer); cdecl;
var
  lCaller: TDicomDirLoader;
  lStr: string;
begin
   (* Obtenemos los datos del Paciente *)
   lCaller := TDicomDirLoader(ACaller);
   lStr := AStudyDescription;
   lCaller.StudyDescription := string(AStudyDescription);
   //lCaller.Sync;
end;

procedure OnPatient(const APatient: PAnsiChar; const APatientId: PAnsiChar; ACaller: Pointer); cdecl;
var
  lCaller: TDicomDirLoader;
begin
   (* Obtenemos los datos del Paciente *)
   lCaller := TDicomDirLoader(ACaller);
   lCaller.PatientName := APatient;
   lCaller.PatientName := StringReplace(lCaller.PatientName, '^', ', ', []);
   lCaller.PatientName := StringReplace(lCaller.PatientName, '^', ' ', [rfReplaceAll]);
   //lCaller.Sync;
end;

procedure OnSerie(const ASeriesDescription: PAnsiChar; const ASeriesNumber: PAnsiChar; const AModality: PAnsiChar; ACaller: Pointer); cdecl;
var
  lSerie: TSerie;
  lCaller: TDicomDirLoader;
begin
   (* Agregamos la serie *)
   lCaller := TDicomDirLoader(ACaller);
   if ASeriesNumber <> '0' then
   begin
     lSerie := TSerie.Create(lCaller.FSeries);
     lSerie.SeriesDescription := ASeriesDescription;
     lSerie.SeriesNumber := ASeriesNumber;
     lCaller.FSeries.Add(lSerie);
   end;
end;

procedure OnImage(const AImage: PAnsiChar; const AInstanceNumber: PAnsiChar; ACaller: Pointer); cdecl;
var
  lFileName: string;
  lCaller: TDicomDirLoader;
  lImage: TImage;
  lImages: TImages;
  lSerie: TSerie;
begin
   (* Agregamos la Imagen *)
   lFileName := AImage;
   lFileName := AnsiReplaceStr(lFileName, '\', DirectorySeparator);
   lFileName := AnsiReplaceStr(lFileName, '/', DirectorySeparator);
   lCaller := TDicomDirLoader(ACaller);
   lCaller.RootPath := ExcludeTrailingPathDelimiter(lCaller.RootPath) + DirectorySeparator;
   lFileName := lCaller.RootPath + lFileName;

   lSerie := lCaller.FSeries[lCaller.FSeries.Count - 1];
   lImage := TImage.Create(lSerie);
   lImage.FileName := lFileName;
   lImage.InstanceNumber:= StrToIntDef(AInstanceNumber, 0);
   lImages := lSerie.Images;
   lImages.Add(lImage);
   lImages.Sort(@SortItems);

   if (lSerie.Images.Count = 1) then
     lCaller.Sync;
end;

procedure TDicomDirLoader.Execute;
var
  I: Integer;
  lValue: string;
  lInstanceNumber: string;
  lSeriesNumber: string;
  lFileName: string;
  lSerie: TSerie;

begin
  FDicomUtils.ReadDicomDir(FFile, @OnStudy, @OnPatient, @OnSerie, @OnImage, Self);
  FFinished := True;
  DoTerminate;
end;

procedure TDicomDirLoader.Sync;
var
  lImages: TImages;
  lImage: TImage;
begin
  lImages := FSeries[FSeries.Count - 1].Images;
  lImage := lImages[lImages.Count - 1];
  fpgPostMessage(lImage, FMsgDest, MSG_BEFOREIMAGELOAD);
end;

end.


