unit uMeEventFeatureTest;

{$I MeSetting.inc}

{.$DEFINE Debug_WriteToConsole_Support}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, //QueryPerformanceCounter
  {$ENDIF}
  {$IFDEF DEBUG}
  DbugIntf,
  {$ENDIF}
  Classes,
  SysUtils,
  TypInfo,
  IniFiles,
  Messages,
  TestFramework
  , uMeObject
  , uMeSystem
  , uMeEventFeature
  ;

type
  TTestPublisher = class
  protected
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  public
    CharCode: Word;
    //fake dispatch
    procedure WndProc(var Message: TMessage);
  end;

  TWinMessageEvent = procedure(const Sender: TObject; const Message: TMessage) of object;
  TTestListener = class
  protected
    FOnChar: TWinMessageEvent;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  public
    Id: Integer;
    CharCode: Word;
    property OnChar: TWinMessageEvent read FOnChar write FOnChar;
  end;

  TTest_MeEventFeature = class(TTestCase)
  protected
    FTriggeredCount: Integer;
    
    procedure DoMessage(const Sender: TObject; const Message: TMessage);

    procedure Setup;override;
    procedure TearDown;override;

  public
  published
    procedure Test_Event;
  end;

implementation


var
  AppPath: string;

{ TTestPublisher }
procedure TTestPublisher.WMChar(var Message: TWMChar);
begin
  CharCode := Message.CharCode;
end;

procedure TTestPublisher.WndProc(var Message: TMessage);
begin
  CharCode := 0;
  Dispatch(Message);
end;

{ TTestListener }
procedure TTestListener.WMChar(var Message: TWMChar);
begin
  CharCode := Message.CharCode;
end;

{ TTest_MeEventFeature }
procedure TTest_MeEventFeature.Setup;
begin
  //New(FScript, Create);
end;

procedure TTest_MeEventFeature.TearDown;
begin
  //MeFreeAndNil(FScript);
end;

procedure TTest_MeEventFeature.DoMessage(const Sender: TObject; const Message: TMessage);
begin
  Inc(FTriggeredCount);
end;

procedure TTest_MeEventFeature.Test_Event();
var
  vPublisher: TTestPublisher;
  vListeners: array[0..1] of TTestListener;
  i: Integer;
begin
  vPublisher := TTestPublisher.Create;
  for i := 0 to High(vListeners) do
  begin
    vListeners[i] := TTestListener.Create;
    vListeners[i].Id := i;
    vListeners[i].OnChar := DoMessage;
  end;
  try
    GMeWindowMessageFeature.RegisterEvent(vPublisher, WM_CHAR);
  finally
    vPublisher.Free;
    for i := 0 to High(vListeners) do
    begin
      vListeners[i].Free;
    end;
  end;
  //CheckEquals(Ord(opPush), Ord(c^), ' the OpCode opPush error.');
  //Inc(c);
  //CheckEquals(0, PInteger(c)^, ' the ParamCount error.');
end;

Initialization

  AppPath := ExtractFilePath(ParamStr(0));
  RegisterTests('MeEvent suites',
                [
                 TTest_MeEventFeature.Suite
                 //, TTest_MeEventFeature.Suite
                 //, TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
