unit layoutdefinition;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TLayoutDefinitionCell = record
    left: double;
    top: double;
    width: double;
    height: double;
  end;

  TLayoutDefinition = array of TLayoutDefinitionCell;

function GetSimpleLayoutDefinition: TLayoutDefinition;

implementation

function GetSimpleLayoutDefinition: TLayoutDefinition;
var
  lCell: TLayoutDefinitionCell;
begin
  lCell.left:= 0;
  lCell.Top:= 0;
  lCell.width:= 1;
  lCell.height:= 1;
  SetLength(Result, 1);
  Result[0] := lCell;
end;

end.

