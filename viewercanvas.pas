unit viewercanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_main,
  fpg_base,
  utils,
  Agg2D;

type

  { TViewerCanvas }

  TViewerCanvas = class
  private
    FCanvasImage: TfpgImage;
    FAgg2D: TAgg2D;
    FDPI: Integer;
  public
    constructor Create(ADPI: Integer);
    destructor Destroy; override;
    procedure Allocate(AWidth, AHeight: Integer);
    property AggCanvas2D: TAgg2d read FAgg2D;
    property CanvasImage: TfpgImage read FCanvasImage;
    property DPI: Integer read FDPI;
  end;

implementation

{ TViewerCanvas }

constructor TViewerCanvas.Create(ADPI: Integer);
begin
  FDPI := ADPI;
  FCanvasImage := TfpgImage.Create;
  FAgg2D := TAgg2D.Create(nil);
  FAgg2D.SetLineStyle(1, lsSolid);
end;

destructor TViewerCanvas.Destroy;
begin
  FAgg2D.Free;
  FCanvasImage.Free;
  inherited Destroy;
end;

procedure TViewerCanvas.Allocate(AWidth, AHeight: Integer);
begin
  FCanvasImage.AllocateImage(32, AWidth, AHeight);
  FAgg2D.Attach(FCanvasImage);
end;

end.

