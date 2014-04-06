unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_dialogs, fpg_panel, fpg_label,
  layoutdefinition,
  commonmessages,
  Series,
  Images,
  dicomdirloader,
  remoteloader,
  viewertoolbar,
  viewerpanel,
  seriecell,
  Layout,
  dlgLayout,
  viewertypes,
  config,
  hpdesigner,
  hangingprotocol,
  threadedloader,
  seriespanel,
  libDicomUtilsWrapperDyn,
  dynlibs;

type
  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    {@VFD_HEAD_END: MainForm}
    FToolBar: TViewerToolBar;
    FViewer: TViewerPanel;
    FSeries: TSeries;
    FDicomDirLoader: TDicomDirLoader;
    FRemoteLoader: TRemoteLoader;
    FSeriesPanel: TSeriesPanel;
    FHpDesigner: TfrmHangingProtocol;
    FHangingProtocols: THangingProtocols;
    FTimer: TfpgTimer;
    FStatusBar: TfpgPanel;
    FStatusText: TfpgLabel;
    procedure Timer(Sender: TObject);
    procedure KeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure SetZoomMode;
    procedure SetPanMode;
    procedure SetWLMode;
    procedure SetRulerMode;
    procedure SetSelectMode;
    procedure SetMagnifyMode;
    procedure ShowHPDesigner;
    procedure OpenFile;
    procedure OpenDicomFile(AFile: string);
    procedure DicomDirTerminate(Sender: TObject);
    procedure NextImage;
    procedure PriorImage;
    procedure ChangeLayout;
    procedure ViewerAfterModeChange(AMode: TViewerMode);
    procedure AfterModeChange(AOldMode, ANewMode: TViewerMode);
    procedure ImageSelected(Sender: TObject);
    procedure NewLayout(ALayoutDefinition: TLayoutDefinition);
    procedure BeforeImageLoad(var msg: TfpgMessageRec); message MSG_BEFOREIMAGELOAD;
    procedure AfterImageLoaded(var msg: TfpgMessageRec); message MSG_IMAGELOADED;
    procedure AfterThumbnailsCreated(var msg: TfpgMessageRec); message MSG_SERIESPANELUPDATE;
    procedure AfterHPChanged(var msg: TfpgMessageRec); message MSG_AFTERHPCHANGED;
    procedure RemoteLoading(var msg: TfpgMessageRec); message MSG_HEARTBEAT;
    procedure UpdateStatus(var msg: TfpgMessageRec); message MSG_UPDATESTATUS;
    procedure OpenFromParams;
    procedure Close(Sender: TObject; var CloseAction: TCloseAction);
    procedure Print;
    procedure PrintToBMP;
    procedure LoadLibDicomUtils;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterCreate; override;
  end;

implementation

procedure TMainForm.Timer(Sender: TObject);
var
  lSerie: TSerie;
  lLoading: Boolean;
begin
  FStatusText.Text := gCommonMsg.Text;
  fpgApplication.ProcessMessages;

  {if (FHangingProtocols.Current = nil) then
  begin
    FHangingProtocols.Modality := lImage.Modality;
    FHangingProtocols.Load;
    FViewer.Layout.ApplyHangingProtocol(FHangingProtocols);
  end;}
  FViewer.Layout.SetAllChanged;
  FViewer.Invalidate;

  lLoading := false;
  // Se crean los thumbnails del SeriesPanel
  for lSerie in FSeries do
  begin
    if lSerie.Loading then
      lLoading := true;
    FSeriesPanel.CreateThumbNails(lSerie);
  end;

  if not lLoading then
    FTimer.Enabled:= False;
end;

procedure TMainForm.KeyPress(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  if ssCtrl in ShiftState then
  begin
    case KeyCode of
      79: // ctrl + o
        begin
          OpenFile;
          Consumed:= True;
        end;
      80: // ctrl + p
        begin
          Print;
          Consumed:= True;
        end;
    end;
  end
  else
  case KeyCode of
    78: // n
      begin
        if FViewer.Layout.ClassType <> TSingleImageLayout then
        begin
          FHangingProtocols.Next;
          FViewer.Layout.ApplyHangingProtocol(FHangingProtocols);
          FViewer.Invalidate;
        end;
        Consumed := True;
      end;
    127: // Del
      begin
        FViewer.Layout.DeleteSelectedVector;
        FViewer.Invalidate;
        Consumed :=True;
      end;
    90: // Z
      begin
        FToolBar.SetMode(vmZoom);
        Consumed:= True;
      end;
    80: // P
      begin
        FToolBar.SetMode(vmPan);
        Consumed:= True;
      end;
    87: // W
      begin
        FToolBar.SetMode(vmWL);
        Consumed:= True;
      end;
    57397: // right arrow
      begin
        NextImage;
        Consumed:= True;
      end;
    57396: // left arrow
      begin
        PriorImage;
        Consumed:= True;
      end;
  end;
end;

procedure TMainForm.SetZoomMode;
begin
  FViewer.ViewerMode:= vmZoom;
end;

procedure TMainForm.SetPanMode;
begin
  FViewer.ViewerMode:= vmPan;
end;

procedure TMainForm.SetWLMode;
begin
  FViewer.ViewerMode:= vmWL;
end;

procedure TMainForm.SetRulerMode;
begin
  FViewer.ViewerMode:= vmRuler;
end;

procedure TMainForm.SetSelectMode;
begin
  FViewer.ViewerMode:=vmSelect;
end;

procedure TMainForm.SetMagnifyMode;
begin
  FViewer.ViewerMode:=vmMagnify;
end;

procedure TMainForm.ShowHPDesigner;
begin
  if not FHpDesigner.Visible then
  begin
    FHpDesigner.Visible:=True;
    FHpDesigner.LoadHangingProtocols(FSeries, FViewer.Layout);
  end
  else
  begin
    FHpDesigner.Visible:= False;
  end;
  Realign;
end;

procedure TMainForm.OpenFile;
var
  dlg: TfpgFileDialog;
  lSerie: TSerie;

begin
{  FRemoteLoader := TRemoteLoader.Create(Self, FSeries, 'http://127.0.0.1:8080/cgi-bin/gpacs.new/seriesImages?AccessionNumber=39647');
  FTimer.Enabled := True;
  FRemoteLoader.Execute;
exit;}
  dlg := TfpgFileDialog.Create(nil);
  try
    dlg.Filter := 'All Files (*.*)|*|DicomDir (DICOMDIR)|DICOMDIR';
    if dlg.RunOpenFile then
    begin
      dlg.Close;
      FSeriesPanel.Visible := False;
      Self.Realign;
      FViewer.ViewerMode := vmNone;
      FSeriesPanel.Reset;
      for lSerie in FSeries do
        lSerie.free;
      FSeries.Clear;
      NewLayout(GetSimpleLayoutDefinition);

      if UpperCase(ExtractFileName(dlg.FileName)) = 'DICOMDIR' then
      begin
        // abre un DicomDir
        FDicomDirLoader := TDicomDirLoader.Create(FSeries, dlg.FileName, Self);
        FDicomDirLoader.OnTerminate := @DicomDirTerminate;
        FDicomDirLoader.Start;
      end
      else
      begin
        // abre un sólo archivos
        OpenDicomFile(dlg.FileName);
      end;
    end;
  finally
    fpgApplication.ProcessMessages;
    dlg.Free;
    FToolBar.SetMode(vmNone);
  end;
end;

procedure TMainForm.OpenDicomFile(AFile: string);
var
  lSerie: TSerie;
  lImage: TImage;
begin
  lSerie := TSerie.Create(FSeries);
  lImage := TImage.Create(lSerie);
  lImage.OpenFile(AFile);
  lSerie.Images.Add(lImage);
  FSeries.Add(lSerie);

  FViewer.Layout.Cells[lSerie.Index].Serie := lSerie;
  FViewer.Layout.Cells[lSerie.Index].SetAllChanged;
  FViewer.Invalidate;
end;

procedure TMainForm.DicomDirTerminate(Sender: TObject);
begin
  // Una vez terminado de cargar el dicomdir refrescamos la pantalla
  {FViewer.Invalidate;}
  FViewer.ViewerMode := vmNone;
end;

procedure TMainForm.BeforeImageLoad(var msg: TfpgMessageRec);
(* Este mensaje avisa que se ha identificado el
   archivo correspondiente a una imagen, pero
   aún no ha sido cargada con Open(). *)
var
  lImage: TImage;

begin
  lImage := msg.Sender as TImage;
  FSeriesPanel.CreateThumbNails(lImage.Serie as TSerie);

  with TThreadedLoader.Create(Self, lImage) do
  begin
    Start;
  end;
end;

procedure TMainForm.NextImage;
var
  lStart: cardinal;
  lEnd: cardinal;
  lTotal: cardinal;
begin
  lStart := GetTickCount;
  FViewer.NextImage(1);
  lEnd := GetTickCount;
  lTotal := lEnd - lStart;
  WindowTitle:= Format('%d', [lTotal]);
end;

procedure TMainForm.PriorImage;
begin
  FViewer.PriorImage(1);
end;

procedure TMainForm.ChangeLayout;
var
  lLayout: TLayoutDefinition;

begin
  if TdlgLayout.Execute(lLayout) then
  begin
    NewLayout(lLayout);
    FToolBar.SetMode(vmNone);
  end;
end;

procedure TMainForm.ViewerAfterModeChange(AMode: TViewerMode);
begin
  if AMode = vmSelect then
    FToolBar.SetMode(AMode);
end;

procedure TMainForm.AfterModeChange(AOldMode, ANewMode: TViewerMode);
begin
  case AOldMode of
    vmHPDesigner: ShowHPDesigner;
  end;

  case ANewMode of
    vmRuler: SetRulerMode;
    vmZoom: SetZoomMode;
    vmPan: SetPanMode;
    vmWL: SetWLMode;
    vmOpenFile: OpenFile;
    vmLayout: ChangeLayout;
    vmSelect: SetSelectMode;
    vmMagnify: SetMagnifyMode;
    vmHPDesigner: ShowHPDesigner;
    vmNone: FViewer.ViewerMode:= vmNone;
  end;
end;

procedure TMainForm.ImageSelected(Sender: TObject);
var
  lSerie: TSerie;

begin
  FToolBar.EnableImageButtons;
  lSerie := (FViewer.Layout.CurrentCell as TSerieCell).Serie;
  if lSerie <> nil then
    FSeriesPanel.ThumbNails.SelectSerie(lSerie);
end;

procedure TMainForm.NewLayout(ALayoutDefinition: TLayoutDefinition);
begin
  FViewer.Layout.Free;
  FViewer.Layout := TLayout.Create(Self);
  FViewer.Layout.CreateLayout(ALayoutDefinition);
  FViewer.Layout.Series := FSeries;
  FViewer.ResetLayoutDimensions;
  FViewer.Invalidate;
end;

procedure TMainForm.AfterImageLoaded(var msg: TfpgMessageRec);
(* El mensaje MSG_IMAGELOADED llega cuando ya se
   ha cargado una imágen en memoria. *)
var
  lImage: TImage;

begin
  lImage := msg.Sender as TImage;

  if (FHangingProtocols.Current = nil) then
  begin
    FHangingProtocols.Modality := lImage.Modality;
    FHangingProtocols.Load;
    FViewer.Layout.ApplyHangingProtocol(FHangingProtocols);
  end;
  FViewer.Layout.SetAllChanged;
  FViewer.Invalidate;

  // Se crean los thumbnails del SeriesPanel
  FSeriesPanel.CreateThumbNails(lImage.Serie as TSerie);
end;

procedure TMainForm.AfterThumbnailsCreated(var msg: TfpgMessageRec);
begin
  FSeriesPanel.Invalidate;
  if FSeriesPanel.Visible = false then
  begin
    FSeriesPanel.Visible:= True;
    Self.Realign;
  end;
end;

procedure TMainForm.AfterHPChanged(var msg: TfpgMessageRec);
var
  lHP: THangingProtocol;
begin
  lHP := msg.Sender as THangingProtocol;
  FViewer.Layout.ApplyHangingProtocol(lHP.Owner);
  FViewer.Invalidate;
end;

procedure TMainForm.RemoteLoading(var msg: TfpgMessageRec);
var
  lSerie: TSerie;
begin
  lSerie := (msg.Sender as TSerie);
  // Se crean los thumbnails del SeriesPanel
  FSeriesPanel.CreateThumbNails(lSerie as TSerie);
end;

procedure TMainForm.UpdateStatus(var msg: TfpgMessageRec);
begin
  FStatusText.Text := (msg.Sender as TCommonMsg).Text;
  fpgApplication.ProcessMessages;
end;

procedure TMainForm.OpenFromParams;
begin
  if FileExists(ParamStr(1)) then
  begin
    // abre un DicomDir
    if UpperCase(ExtractFileName(ParamStr(1))) = 'DICOMDIR' then
    begin
      FDicomDirLoader := TDicomDirLoader.Create(FSeries, ParamStr(1), Self);
      FDicomDirLoader.OnTerminate := @DicomDirTerminate;
      FDicomDirLoader.Start;
    end
    else
      OpenDicomFile(ParamStr(1));
    //'/home/leonardo/Documentos/Griensu/daa/dicom/mamo/oulton/2/DICOMDIR'
    {FDicomDirLoader := TDicomDirLoader.Create(FSeries, ParamStr(1), Self);
    FDicomDirLoader.OnTerminate := @DicomDirTerminate;
    FDicomDirLoader.Start;}
  end
  else
  if Pos(UpperCase(ParamStr(1)), 'HTTP') = 0 then
  begin
    //'http://127.0.0.1:8080/cgi-bin/gpacs.new/seriesImages?AccessionNumber=39647'
    FRemoteLoader := TRemoteLoader.Create(Self, FSeries, ParamStr(1));
    FTimer.Enabled := True;
    FRemoteLoader.Execute;
  end;
end;

procedure TMainForm.Close(Sender: TObject; var CloseAction: TCloseAction);
begin
  FStatusText.Text := 'Closing...';
  fpgApplication.ProcessMessages;
end;

procedure TMainForm.Print;
begin
  FViewer.Layout.ToPrinter;
end;

procedure TMainForm.PrintToBMP;
begin
  FViewer.ToBMP;
end;

procedure TMainForm.LoadLibDicomUtils;
var
  lFile: string;
begin
  {$ifdef unix}
  lFile := ExtractFilePath(ParamStr(0)) + 'libdicomutils-2.0.so';
  {$else}
  lFile := ExtractFilePath(ParamStr(0)) + 'libdicomutils-2.0.dll';
  {$endif}
  if FileExists(lFile) then
  begin
    gLibHandle := LoadLibrary(lFile);
    if gLibHandle <> NilHandle then
      TLibDicomUtilsWrapperDyn.RegisterCodecs;
  end
  else
    raise Exception.Create('File ' + lFile + ' not found.');
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Focusable:= True;
  OnKeyPress := @KeyPress;
  OnClose:= @Close;

  FTimer := TfpgTimer.Create(1000);
  FTimer.OnTimer:= @Timer;
  FTimer.Enabled:= False;

  FHangingProtocols := THangingProtocols.Create;
  FHangingProtocols.MsgDest := Self;
  FSeries := TSeries.Create;

  FToolBar := TViewerToolBar.Create(Self);
  FToolBar.OnAfterModeChange:= @AfterModeChange;

  FStatusBar := TfpgPanel.Create(self);
  with FStatusBar do
  begin
    Name := 'bevStatusBar';
    SetPosition(1, 387, 715, 22);
    BackgroundColor:= fpgColor($30,$30,$30);
    Align:= alBottom;
    Anchors := [anLeft,anRight,anBottom];
    Hint := '';
    BorderStyle:= bsSingle;
    Style := bsFlat;
  end;

  FStatusText := TfpgLabel.Create(FStatusBar);
  with FStatusText do
  begin
    Name := 'lblStatusText';
    SetPosition(4, 4, 704, 15);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    TextColor:= fpgColor($80,$80,$80);
    Hint := '';
    Text := '';
  end;

  FSeriesPanel := TSeriesPanel.Create(Self);
  FSeriesPanel.Align := alLeft;
  FSeriesPanel.Visible:= False;
  FSeriesPanel.Reset;

  FHpDesigner := TfrmHangingProtocol.Create(Self);
  FHpDesigner.Align:= alRight;
  FHpDesigner.Visible:= False;

  FViewer := TViewerPanel.Create(Self);
  FViewer.Align:= alClient;
  FViewer.OnSelect:=@ImageSelected;
  FViewer.OnAfterModeChange:=@ViewerAfterModeChange;

  DNDEnabled:=True;
end;

destructor TMainForm.Destroy;
begin
  FTimer.Free;
  FreeAndNil(FRemoteLoader);
  FreeAndNil(FToolBar);
  FSeries.Free;
  FHangingProtocols.Free;
  gConfig.Free;
  if gLibHandle <> NilHandle then
  begin
    if gLibHandle <> NilHandle then
      TLibDicomUtilsWrapperDyn.UnRegisterCodecs;
    FreeLibrary(gLibHandle);
  end;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
var
  lFile: string;
  lCloseAction: TCloseAction;
begin
  try
    gConfig := TConfig.Create;
    LoadLibDicomUtils;
    NewLayout(GetSimpleLayoutDefinition);
    if Paramcount = 1 then
      OpenFromParams;
  except
    on E: Exception do
    begin
      fpgApplication.ShowException(E);
      fpgApplication.Terminate;
    end;
  end;

  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  FWidth:= 1024;
  FHeight:= 768;
  WindowPosition := wpScreenCenter;
  WindowTitle := 'DicomViewer IV';
  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.

