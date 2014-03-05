unit MammoBoundaries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  contnrs,
  Config,
  fpg_main,
  fpg_base;

type

  { TBlob }
  TBlob = class
  private
    FxMin: Integer;
    FxMax: Integer;
    FyMin: Integer;
    FyMax: Integer;
    FMass: Integer;
  public
    constructor create( xMin, xMax, yMin, yMax, Mass: Integer);
    property xMin: Integer read FxMin;
    property yMin: Integer read FyMin;
    property xMax: Integer read FxMax;
    property yMax: Integer read FyMax;
    property Mass: Integer read FMass;
  end;

  { TMammoBoundaries }
  TMammoBoundaries = class
  private
    FBmp: TfpgImage;
    FX1: Integer;
    FX2: Integer;
    FY1: Integer;
    FY2: Integer;
    FBlobList: TObjectList;
    procedure DoCalcs;
    procedure NormalizarGrises;
  public
    destructor Destroy; override;
    class procedure Execute(ABmp: TfpgImage; var AX1, AY1, AX2, AY2: Integer);
  end;


implementation

const
  cInitialColor = 0;

class procedure TMammoBoundaries.Execute(ABmp: TfpgImage; var AX1, AY1, AX2, AY2: Integer);
var
  lMammoBoundaries: TMammoBoundaries;
  lBlob: TBlob;
  I: Integer;
begin
  lMammoBoundaries := TMammoBoundaries.Create;
  try
    with lMammoBoundaries do
    begin
      FBlobList := TObjectList.create(True);
      FBmp := ABmp;
      //NormalizarGrises;
      FX1:= AX1;
      FX2:= AX2;
      FY1:= AY1;
      FY2:= AY2;
      DoCalcs;
      for I := 0 to FBlobList.Count - 1 do
      begin
        if I = 0 then
        begin
          lBlob := TBlob(FBlobList[0]);
        end
        else
        if TBlob(FBlobList[I]).Mass > lBlob.Mass then
          lBlob := TBlob(FBlobList[I]);
      end;
      AX1:= lBlob.xMin;
      AX2:= Round(lBlob.xMax + (lBlob.xMax * gConfig.MammoMargin / 100));
      AY1:= lBlob.yMin;
      AY2:= Round(lBlob.yMax + (lBlob.yMax * gConfig.MammoMargin / 100));
//      ABmp.Assign(FBmp);
    end;
  finally
    lMammoBoundaries.Free;
  end;
end;

{ TBlob }

constructor TBlob.create(xMin, xMax, yMin, yMax, Mass: Integer);
begin
  FxMin := xMin;
  FxMax := xMax;
  FyMin := yMin;
  FyMax := yMax;
  FMass := Mass;
end;

procedure TMammoBoundaries.DoCalcs;
var
  X, Y: Integer;
  p: PLongWord;
  tableSize: Integer;
  srcPtr: Integer;
  lLabel: Integer;
  aLabel: Integer;
  bLabel: Integer;
  cLabel: Integer;
  dLabel: Integer;
  aPtr: Integer;
  bPtr: Integer;
  cPtr: Integer;
  dPtr: Integer;
  lMin: Integer;
  I: Integer;
  L: Integer;
  LabelBuffer: array of Integer;
  labelTable: array of Integer;
  xMinTable: array of Integer;
  xMaxTable: array of Integer;
  yMinTable: array of Integer;
  yMaxTable: array of Integer;
  massTable: array of Integer;
  lBlob: TBlob;

begin
  SetLength(LabelBuffer, FBmp.Width * FBmp.Height);
  tableSize := FBmp.Width * FBmp.Height div 4;

  SetLength(labelTable, tableSize);
  SetLength(xMinTable, tableSize);
  SetLength(xMaxTable, tableSize);
  SetLength(yMinTable, tableSize);
  SetLength(yMaxTable, tableSize);
  SetLength(massTable, tableSize);

  srcPtr := 0;
  aPtr := -FBmp.Width - 1;
  bPtr := -FBmp.Width;
  cPtr := -FBmp.Width + 1;
  dPtr := -1;
  lLabel := 1;

  for Y := 0 to FBmp.Height - 1 do
  begin
    p := FBmp.ScanLine[Y];
    for X := 0 to FBmp.Width - 1 do
    begin
      LabelBuffer[srcPtr] := 0;
      if p^ = clWhite then
      begin
	// Find label for neighbours (0 if out of range)
        aLabel := 0;
        bLabel := 0;
        cLabel := 0;
        dLabel := 0;

        if (x > 0) and (y > 0) then
          aLabel := labelTable[LabelBuffer[aPtr]];

        if (y > 0) then
          bLabel := labelTable[LabelBuffer[bPtr]];

        if (x < FBmp.Width - 1) and (y > 0) then
          cLabel := labelTable[LabelBuffer[cPtr]];

        if (x > 0) then
          dLabel := labelTable[LabelBuffer[dPtr]];

	// Look for label with least value
	lMin := MaxInt;
	if (aLabel <> 0) and (aLabel < lmin) then
           lMin := aLabel;

	if (bLabel <> 0) and (bLabel < lmin) then
           lMin := bLabel;

	if (cLabel <> 0) and (cLabel < lmin) then
           lMin := cLabel;

	if (dLabel <> 0) and (dLabel < lmin) then
           lMin := dLabel;

        // If no neighbours in foreground
        if (lMin = MaxInt) then
        begin
          labelBuffer[srcPtr] := lLabel;
          labelTable[lLabel] := lLabel;

          // Initialise min/max x,y for label
          yMinTable[lLabel] := y;
          yMaxTable[lLabel] := y;
          xMinTable[lLabel] := x;
          xMaxTable[lLabel] := x;
          massTable[lLabel] := 1;

          inc(lLabel);
        end
        else
	// Neighbour found
	begin
	  // Label pixel with lowest label from neighbours
	  labelBuffer[srcPtr] := lMin;

	  // Update min/max x,y for label
	  yMaxTable[lMin] := y;
	  inc(massTable[lMin]);

	  if (x < xMinTable[lMin]) then
             xMinTable[lMin] := x;

	  if (x > xMaxTable[lMin]) then
             xMaxTable[lMin] := x;

	  if (aLabel <> 0) then
             labelTable[aLabel] := lMin;

	  if (bLabel <> 0) then
             labelTable[bLabel] := lMin;

	  if (cLabel <> 0) then
             labelTable[cLabel] := lMin;

	  if (dLabel <> 0) then
             labelTable[dLabel] := lMin;
	end;
      end;
      inc(srcPtr);
      inc(aPtr);
      inc(bPtr);
      inc(cPtr);
      inc(dPtr);
      inc(p);
    end;
  end;

  for I := lLabel - 1 downto 0 do
  begin
    if (labelTable[i] <> i) then
    begin
      if (xMaxTable[i] > xMaxTable[labelTable[i]]) then
         xMaxTable[labelTable[i]] := xMaxTable[i];

      if (xMinTable[i] < xMinTable[labelTable[i]]) then
         xMinTable[labelTable[i]] := xMinTable[i];

      if (yMaxTable[i] > yMaxTable[labelTable[i]]) then
         yMaxTable[labelTable[i]] := yMaxTable[i];

      if (yMinTable[i] < yMinTable[labelTable[i]]) then
         yMinTable[labelTable[i]] := yMinTable[i];

      massTable[labelTable[i]] := massTable[labelTable[i]] + massTable[i];

      L := I;
      while (l <> labelTable[l]) do
        L := labelTable[l];

      labelTable[i] := L;
    end
    else
    begin
      lBlob := TBlob.create(xMinTable[i], xMaxTable[i], yMinTable[i], yMaxTable[i], massTable[i]);
      FBlobList.Add(lBlob);
    end;
  end;
end;

procedure TMammoBoundaries.NormalizarGrises;
var
  x: Integer;
  y: Integer;
  psrc: PLongWord;

begin
  for y := 0 to FBmp.Height - 1 do
  begin
    psrc:= FBmp.ScanLine[y];
    for x := 0 to FBmp.Width - 1 do
    begin
      if psrc^ > 25 then
        psrc^ := 255
      else
        psrc^ := 0;
      inc(psrc);
    end;
  end;
end;

destructor TMammoBoundaries.Destroy;
begin
  FBlobList.Free;
  inherited Destroy;
end;

end.

