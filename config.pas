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
    procedure LoadJSon(var AJson: string);
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

procedure TConfig.LoadJSon(var AJson: string);
var
  lStr: TStringList;

begin
  if not FileExists(AJson) then
    raise Exception.Create('File ' + AJson + ' not found. Cannot continue.');

  if not FileExists(AJson) then
    exit;

  lStr := TStringList.Create;
  try
    lStr.LoadFromFile(AJson);
    AJson := lStr.Text;
  finally
    lStr.Free;
  end;
end;

constructor TConfig.Create;
begin
  //FReadingProtocols:= ExtractFilePath(ParamStr(0)) + 'readingprotocols.json';
  //LoadJSon(FReadingProtocols);

  FOverlays:= ExtractFilePath(ParamStr(0)) + 'overlays.json';
  LoadJSon(FOverlays);

  //FPresets := ExtractFilePath(ParamStr(0)) + 'presets.json';
  //LoadJSon(FPresets);

  FMammoMargin := 0;
end;

end.


