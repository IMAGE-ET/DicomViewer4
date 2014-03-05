unit textoverlays;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  { TTextOverlay }

  TTextOverlay = class
  private
    FLabel: string;
    FTag: string;
    FValue: string;
  public
    property Tag: string read FTag write FTag;
    property Value: string read FValue write FValue;
    property TextLabel: string read FLabel write FLabel;
  end;

  TTextOverlaysSpecialization = specialize TFPGList<TTextOverlay>;

  { TTextOverlays }

  TTextOverlays = class(TTextOverlaysSpecialization)
  private
  public
    procedure Clear;
    Destructor Destroy; override;
  end;

implementation

{ TTextOverlays }

procedure TTextOverlays.Clear;
var
  lOverlay: TTextOverlay;
begin
  for lOverlay in Self do
  begin
    FreeAndNil(lOverlay);
  end;
end;

destructor TTextOverlays.Destroy;
begin
  clear;
  inherited Destroy;
end;

end.

