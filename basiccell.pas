unit basiccell;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TValign = (vaTop, vaCenter, vaBottom);
  THalign = (haLeft, haCenter, haRight);

  { TBasicCell }

  TBasicCellRect = record
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
  end;

  TBasicCell = class; // forward

  TBasicObject = class
  private
    FWidth: double;
    FHeight: double;
    FCurrentCell: TBasicCell;
  public
    property Width: double read FWidth write FWidth;
    property Height: double read FHeight write FHeight;
    property CurrentCell: TBasicCell read FCurrentCell write FCurrentCell;
  end;

  TBasicCell = class(TBasicObject)
  private
    FLeft: double;
    FTop: double;
    FOwner: TBasicObject;
  public
    procedure NextImage(AIncrement: Integer); virtual; abstract;
    procedure PriorImage(AIncrement: Integer); virtual; abstract;
    property Left: double read FLeft write FLeft;
    property Top: double read FTop write FTop;
    property Owner: TBasicObject read FOwner write FOwner;
  end;

implementation

end.

