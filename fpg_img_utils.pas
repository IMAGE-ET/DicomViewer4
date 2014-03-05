unit fpg_img_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FPimage,
  fpg_main,
  fpg_base;

procedure FpgImageToFPImage(ASource: TfpgImage; ATarget: TFPMemoryImage);
function ColorToFPColor(const AColor: TfpgColor): FPImage.TFPColor;

implementation

function ColorToFPColor(const AColor: TfpgColor): FPImage.TFPColor;
begin
  with Result do
  begin
     { this could probably be optimised a bit }
    Red   := fpgGetRed(AColor) shl 8 or fpgGetRed(AColor);
    Green := fpgGetGreen(AColor) shl 8 or fpgGetGreen(AColor);
    Blue  := fpgGetBlue(AColor) shl 8 or fpgGetBlue(AColor);
    Alpha := fpgGetAlpha(AColor) shl 8 or fpgGetAlpha(AColor);
  end
end;

procedure FpgImageToFPImage(ASource: TfpgImage; ATarget: TFPMemoryImage);
var
  i, j: integer;
  lColor: FPimage.TFPColor;
begin
  if ASource=nil then Exit;

  ATarget.UsePalette:= False;
  for i := 0 to ATarget.Height - 1 do
    for j := 0 to ATarget.Width - 1 do
    begin
      ATarget.Colors[j, i] := ColorToFPColor(ASource.Colors[j, i]);
    end;
end;


end.

