unit newseriespanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_panel,
  Series;

type

  { TSeriesPanel }

  TSeriesPanel = class(TfpgPanel)
  private
    FSeries: TSeries;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateThumbNail(ASerie: TSerie);
    procedure SetSeries(ASeries: TSeries);
  end;

implementation

{ TSeriesPanel }

constructor TSeriesPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TSeriesPanel.CreateThumbNail(ASerie: TSerie);
var
  lPanel: TfpgPanel;
begin
  lPanel := TfpgPanel.Create(self);
  lPanel.Style:= bsFlat;
  lPanel.Top := Self.ComponentCount * 40;
  lPanel.Height:= 40;
  lPanel.Text := Format('%d',[Self.ComponentCount]);
  lPanel.UpdateWindowPosition;
end;

procedure TSeriesPanel.SetSeries(ASeries: TSeries);
begin
  FSeries := ASeries;
end;

end.

