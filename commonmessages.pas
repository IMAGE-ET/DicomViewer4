unit commonmessages;

{$mode objfpc}{$H+}

interface

uses
  fpg_base;

const
  MSG_IMAGELOADED = FPGM_USER + 100;
  MSG_BEFOREIMAGELOAD = FPGM_USER + 101;
  MSG_SERIESPANELUPDATE = FPGM_USER + 102;
  MSG_AFTERHPCHANGED = FPGM_USER + 103;
  MSG_HEARTBEAT = FPGM_USER + 104;
  MSG_UPDATESTATUS = FPGM_USER + 105;

type

  { TCommonMsg }

  TCommonMsg = class
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

var
  gCommonMsg: TCommonMsg;

implementation

initialization
  gCommonMsg := TCommonMsg.Create;

finalization
  gCommonMsg.Free;

end.

