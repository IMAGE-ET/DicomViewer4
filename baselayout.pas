unit baselayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  basiccell;

type

  { TBaseLayout }

  TBaseLayout = class(TBasicObject)
  private
    FCurrentImageCell: TBasicCell;
  public
    property CurrentImageCell: TBasicCell read FCurrentImageCell write FCurrentImageCell;
  end;

implementation

{ TBaseLayout }

end.

