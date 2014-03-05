unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main, FPimage;

function GetFont: string;

const
  {$ifdef windows}
  mFontSize = 11;
  {$else}
  mFontSize = 9;
  {$endif}

implementation

function GetFont: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'DejaVuSans.ttf';
end;


end.

