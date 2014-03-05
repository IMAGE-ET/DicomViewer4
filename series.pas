unit series;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fgl,
  images,
  scout;

type

  { TSeries }

  TSerie = class;
  TSeriesSpecialization = specialize TFPGList<TSerie>;

  TSeries = class(TSeriesSpecialization)
    destructor Destroy; override;
  end;

  TSerie = class
  private
    FDownloadedSize: LongInt;
    FImages: TImages;
    FIndex: Integer;
    FLoading: Boolean;
    FScoutColPixel: TColPixel;
    FScoutRowPixel: TRowPixel;
    FSeries: TSeries;
    FSeriesDescription: string;
    FSeriesInstanceUID: string;
    FSeriesNumber: string;
  public
    constructor Create(ASeries: TSeries);
    destructor Destroy; override;
    property Images: TImages read FImages;
    property SeriesDescription: string read FSeriesDescription write FSeriesDescription;
    property SeriesNumber: string read FSeriesNumber write FSeriesNumber;
    property Series: TSeries read FSeries;
    property Index: Integer read FIndex;
    property ScoutRowPixel: TRowPixel read FScoutRowPixel write FScoutRowPixel;
    property ScoutColPixel: TColPixel read FScoutColPixel write FScoutColPixel;
    property SeriesInstanceUID: string read FSeriesInstanceUID write FSeriesInstanceUID;
    property DownloadedSize: LongInt read FDownloadedSize write FDownloadedSize;
    property Loading: Boolean read FLoading write FLoading;
  end;


implementation

{ TSeries }

destructor TSeries.Destroy;
var
  lSerie: TSerie;
  I: Integer;
begin
  for lSerie in Self do
  begin
    FreeAndNil(lSerie);
  end;
  inherited Destroy;
end;

{ TSeries }

constructor TSerie.Create(ASeries: TSeries);
begin
  inherited Create;
  FImages := TImages.Create;
  FSeries := ASeries;
  FIndex := FSeries.Count;
end;

destructor TSerie.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

end.

