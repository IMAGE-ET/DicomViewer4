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
    procedure LoadJSon(AFile: string; var AValue: string);
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

procedure TConfig.LoadJSon(AFile: string; var AValue: string);
var
  lStr: TStringList;

begin
  lStr := TStringList.Create;
  try
    lStr.LoadFromFile(AFile);
    AValue := lStr.Text;
  finally
    lStr.Free;
  end;
end;

constructor TConfig.Create;
var
  lFile: string;
begin
  lFile := ExtractFilePath(ParamStr(0)) + 'readingprotocols.json';
  if FileExists(lFile) then
    LoadJSon(lFile, FReadingProtocols);

  lFile := ExtractFilePath(ParamStr(0)) + 'overlays.json';
  if FileExists(lFile) then
    LoadJSon(lFile, FOverlays);

  lFile := ExtractFilePath(ParamStr(0)) + 'presets.json';
  if FileExists(lFile) then
    LoadJSon(lFile, FPresets);

  FMammoMargin := 0;
end;

end.


