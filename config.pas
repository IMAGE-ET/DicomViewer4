unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { TConfig }

  TConfig = class
  private
    FMammoMargin: integer;
    FOverlays: string;
    FPresets: string;
    FReadingProtocols: string;
    procedure LoadJSon(AFile: string);
  public
    constructor Create;
    property ReadingProtocols: string read FReadingProtocols write FReadingProtocols;
    property Presets: string read FPresets write FPresets;
    property Overlays: string read FOverlays write FOverlays;
    property MammoMargin: integer read FMammoMargin write FMammoMargin;
  end;

var
  gConfig: TConfig;

implementation

{ TConfig }

procedure TConfig.LoadJSon(AFile: string);
var
  lStr: TStringList;

begin
  if not FileExists(AFile) then
    raise Exception.Create('File ' + AFile + ' not found. Cannot continue.');

  if not FileExists(AFile) then
    exit;
  lStr := TStringList.Create;
  try
    lStr.LoadFromFile(AFile);
  finally
    lStr.Free;
  end;
end;

constructor TConfig.Create;
begin
  FReadingProtocols:= ExtractFilePath(ParamStr(0)) + 'readingprotocols.json';
  LoadJSon(FReadingProtocols);

  FOverlays:= ExtractFilePath(ParamStr(0)) + 'overlays.json';
  LoadJSon(FOverlays);

  FPresets := ExtractFilePath(ParamStr(0)) + 'presets.json';
  LoadJSon(FPresets);

  FMammoMargin := 0;
end;

end.


