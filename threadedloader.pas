unit threadedloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  commonmessages,
  Series,
  Images,
  fpg_base,
  fpg_main;

type
  { TThreadedLoader }

  TThreadedLoader = class(TThread)
  private
    FImage: TImage;
    FCaller: TObject;
  public
    constructor Create(ACaller: TObject; AImage: TImage);
    procedure Execute; override;
    property Image: TImage read FImage;
  end;

implementation

{ TThreadedLoader }

constructor TThreadedLoader.Create(ACaller: TObject; AImage: TImage);
begin
  FCaller := ACaller;
  FImage := AImage;
  Priority:= tpIdle;
  FreeOnTerminate:= True;
  inherited Create(True);
end;

procedure TThreadedLoader.Execute;
begin
  if (FImage <> nil) and (not FImage.DicomUtils.Loaded) then
  begin
    FImage.OpenFile(FImage.FileName);
    fpgPostMessage(FImage, FCaller, MSG_IMAGELOADED);
  end;
  DoTerminate;
end;

end.

