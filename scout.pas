unit scout;

{

Esta unidad contiene la clase TScout, que permite obtener los puntos
necesarios para dibujar la linea de Scout.
Traducción a Object Pascal de http://www.dclunie.com/dicom3tools/workinprogress/dcpost.cc

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Images;

type
  { TScout }
  TRowPixel = array[0..3] of Integer;
  TColPixel = array[0..3] of Integer;

  TScoutData = class
  private
    FRow_dircos_x: double;
    FRow_dircos_y: double;
    FRow_dircos_z: double;
    FCol_dircos_x: double;
    FCol_dircos_y: double;
    FCol_dircos_z: double;
    FNrm_dircos_x: double;
    FNrm_dircos_y: double;
    FNrm_dircos_z: double;
    FPos_x: double;
    FPos_y: double;
    FPos_z: double;
    FRow_spacing: double;
    FCol_spacing: double;
    FRow_length: double;
    FCol_length: double;
    FRows: Integer;
    FCols: Integer;
    FImage: TImage;
  public
    procedure getPositionOrientationSpacingAndSize;
    constructor Create(AImage: TImage);
    property Row_dircos_x: double read FRow_dircos_x;
    property Row_dircos_y: double read FRow_dircos_y;
    property Row_dircos_z: double read FRow_dircos_z;
    property Col_dircos_x: double read FCol_dircos_x;
    property Col_dircos_y: double read FCol_dircos_y;
    property Col_dircos_z: double read FCol_dircos_z;
    property Nrm_dircos_x: double read FNrm_dircos_x;
    property Nrm_dircos_y: double read FNrm_dircos_y;
    property Nrm_dircos_z: double read FNrm_dircos_z;
    property Pos_x: double read FPos_x;
    property Pos_y: double read FPos_y;
    property Pos_z: double read FPos_z;
    property Row_spacing: double read FRow_spacing;
    property Col_spacing: double read FCol_spacing;
    property Row_length: double read FRow_length;
    property Col_length: double read FCol_length;
    property Rows: Integer read FRows;
    property Cols: Integer read FCols;
    property Image: TImage read FImage;
  end;

  TScout = class
  private
    FColPixel: TColPixel;
    FOriginal: TScoutData;
    FLocalizer: TScoutData;
    FRowPixel: TRowPixel;
    procedure Rotate(AScoutData: TScoutData;
      Src_Pos_x, Src_Pos_y, Src_Pos_z: double;
      out Dst_Pos_x, Dst_Pos_y, Dst_Pos_z: double);
  public
    constructor Create(AImage, ALocalizer: TImage);
    destructor Destroy; override;
    function DoCalcs: Boolean;
    property RowPixel: TRowPixel read FRowPixel write FRowPixel;
    property ColPixel: TColPixel read FColPixel write FColPixel;
  end;

implementation

{ TScout }

procedure TScoutData.getPositionOrientationSpacingAndSize;
var
  lFormat: TFormatSettings;
begin
  lFormat.DecimalSeparator:= '.';
  if (FImage.ImageOrientationPatient.Count < 5) or (FImage.ImagePositionPatient.Count < 3) then
    exit;

  FRow_dircos_x := StrToFloatDef(FImage.ImageOrientationPatient[0], 0, lFormat);
  FRow_dircos_y := StrToFloatDef(FImage.ImageOrientationPatient[1], 0, lFormat);
  FRow_dircos_z := StrToFloatDef(FImage.ImageOrientationPatient[2], 0, lFormat);
  FCol_dircos_x := StrToFloatDef(FImage.ImageOrientationPatient[3], 0, lFormat);
  FCol_dircos_y := StrToFloatDef(FImage.ImageOrientationPatient[4], 0, lFormat);
  FCol_dircos_z := StrToFloatDef(FImage.ImageOrientationPatient[5], 0, lFormat);

  // compute nrm to row and col (i.e. cross product of row and col unit vectors)

  FNrm_dircos_x := FRow_dircos_y * FCol_dircos_z - FRow_dircos_z * FCol_dircos_y;
  FNrm_dircos_y := FRow_dircos_z * FCol_dircos_x - FRow_dircos_x * FCol_dircos_z;
  FNrm_dircos_z := FRow_dircos_x * FCol_dircos_y - FRow_dircos_y * FCol_dircos_x;

  (* Por ahora se omiten estos dos cálculos *)
  // check are unit vectors ...
  // check are orthogonal (dot product is zero, i.e. cos 90)

  FPos_x := StrToFloatDef(FImage.ImagePositionPatient[0], 0, lFormat);
  FPos_y := StrToFloatDef(FImage.ImagePositionPatient[1], 0, lFormat);
  FPos_z := StrToFloatDef(FImage.ImagePositionPatient[2], 0, lFormat);

  FRow_spacing := FImage.RowSpacing;
  FCol_spacing := FImage.ColSpacing;

  FRows := FImage.Rows;
  FCols := FImage.Cols;

  FRow_length := FRows * FRow_spacing;
  FCol_length := FCols * FCol_spacing;
end;

constructor TScoutData.Create(AImage: TImage);
begin
  FImage := AImage;
end;

procedure TScout.Rotate(AScoutData: TScoutData; Src_Pos_x, Src_Pos_y,
  Src_Pos_z: double; out Dst_Pos_x, Dst_Pos_y, Dst_Pos_z: double);
begin
  dst_pos_x := AScoutData.Row_dircos_x * src_pos_x
  	  + AScoutData.Row_dircos_y * src_pos_y
  	  + AScoutData.Row_dircos_z * src_pos_z;

  dst_pos_y := AScoutData.Col_dircos_x * src_pos_x
  	  + AScoutData.Col_dircos_y * src_pos_y
  	  + AScoutData.Col_dircos_z * src_pos_z;

  dst_pos_z := AScoutData.Nrm_dircos_x * src_pos_x
  	  + AScoutData.Nrm_dircos_y * src_pos_y
  	  + AScoutData.Nrm_dircos_z * src_pos_z;
end;

constructor TScout.Create(AImage, ALocalizer: TImage);
begin
  FOriginal := TScoutData.Create(AImage);
  FLocalizer := TScoutData.Create(ALocalizer);
end;

destructor TScout.Destroy;
begin
  FOriginal.Free;
  FLocalizer.Free;
  inherited Destroy;
end;

function TScout.DoCalcs: Boolean;
{

TODO: por ahora sólo funciona para LOCALIZERs originales,
no se está aplicando la rotación.

}

var
  pos_x: array[0..3] of double;
  pos_y: array[0..3] of double;
  pos_z: array[0..3] of double;
  I: Integer;

begin
  Result := True;
  // Open the Localizer image
  FLocalizer.getPositionOrientationSpacingAndSize;
  if (FLocalizer.Col_spacing=0) or (FLocalizer.Row_spacing=0) then
  begin
    Result := False;
    exit;
  end;
  // Open Current image
  FOriginal.getPositionOrientationSpacingAndSize;

  // Build a square to project with 4 corners TLHC, TRHC, BRHC, BLHC ...

  // TLHC is what is in ImagePositionPatient
  pos_x[0] := FOriginal.Pos_x;
  pos_y[0] := FOriginal.Pos_y;
  pos_z[0] := FOriginal.Pos_z;

  // TRHC
  pos_x[1] := FOriginal.Pos_x + FOriginal.Row_dircos_x * FOriginal.Row_length;
  pos_y[1] := FOriginal.Pos_y + FOriginal.Row_dircos_y * FOriginal.Row_length;
  pos_z[1] := FOriginal.Pos_z + FOriginal.Row_dircos_z * FOriginal.Row_length;

  // BRHC
  pos_x[2] := FOriginal.Pos_x + FOriginal.Row_dircos_x * FOriginal.Row_length + FOriginal.Col_dircos_x * FOriginal.Col_length;
  pos_y[2] := FOriginal.Pos_y + FOriginal.Row_dircos_y * FOriginal.Row_length + FOriginal.Col_dircos_y * FOriginal.Col_length;
  pos_z[2] := FOriginal.Pos_z + FOriginal.Row_dircos_z * FOriginal.Row_length + FOriginal.Col_dircos_z * FOriginal.Col_length;

  // BLHC
  pos_x[3] := FOriginal.Pos_x + FOriginal.Col_dircos_x * FOriginal.Col_length;
  pos_y[3] := FOriginal.Pos_y + FOriginal.Col_dircos_y * FOriginal.Col_length;
  pos_z[3] := FOriginal.Pos_z + FOriginal.Col_dircos_z * FOriginal.Col_length;

  for I := 0 to 3 do
  begin
    // we want to view the source slice from the "point of view" of
    // the target localizer, i.e. a parallel projection of the source
    // onto the target

    // do this by imaging that the target localizer is a view port
    // into a relocated and rotated co-ordinate space, where the
    // viewport has a row vector of +X, col vector of +Y and normal +Z,
    // then the X and Y values of the projected target correspond to
    // row and col offsets in mm from the TLHC of the localizer image !

    // move everything to origin of target
    pos_x[i] := pos_x[i] - FLocalizer.Pos_x;
    pos_y[i] := pos_y[i] - FLocalizer.Pos_y;
    pos_z[i] := pos_z[i] - FLocalizer.Pos_z;

    // The rotation is easy ... just rotate by the row, col and normal
    // vectors ...
    rotate(FLocalizer,
      pos_x[i], pos_y[i], pos_z[i],
      pos_x[i], pos_y[i], pos_z[i]
      );

    // DICOM coordinates are center of pixel 1\1
    FColPixel[i] := Round(pos_x[i] / FLocalizer.Col_spacing + 0.5);
    FRowPixel[i] := Round(pos_y[i] / FLocalizer.Row_spacing + 0.5);
  end;
end;

end.

