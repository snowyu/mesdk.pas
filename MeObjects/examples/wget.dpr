program wget;
 
{Author: Riceball LEE}
 
{$AppType Console}
uses
  Windows,
  SysUtils, Classes
  , uMeObject
  , uMeThread
  , uMeIndyTask
  , uMeLog
  , uMeLoggerEx
  , IdComponent
  ;
 
const
  cCopyright = 'WGet Indy Version by Riceball LEE  - CopyRight(C) 2008';

function ToMethod(const aProc: Pointer; const aData: Pointer = nil): TMethod;
begin
  with Result do
  begin
    Data := aData;
    Code := aProc;
  end;
end;

var
  HasError: Boolean = False;
procedure DoException(const Self: TObject; const aThread: PMeCustomThread; const aException: Exception);
begin
  with aException do GLogger.error('Exception: '+ ClassName+ ' Error:'+ Message);
  HasError := True;
  {EnterMainThread;
  try
    with aException do writeln('Exception: ', ClassName, ' Error:', Message);
  finally
    LeaveMainThread;
  end;//}
end;

procedure DoStatus(const Self: TObject; ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  GLogger.info(AStatusText);
end;

const
  cWorkModeStr : array[TWorkMode] of string =
  (
    'Read'
    , 'Write'
  );
procedure DoWorkBegin(const Self: TObject; ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  //writeln('DoWorkBegin');
  //writeln(Format(cWorkModeStr[AWorkMode]+ ' WorkBegin: %d bytes', [AWorkCountMax]));
  GLogger.info(cWorkModeStr[AWorkMode]+ ' WorkBegin: %d bytes', [AWorkCountMax]);
end;

procedure DoWorkEnd(const Self: TObject; ASender: TObject; AWorkMode: TWorkMode);
begin
  //writeln((cWorkModeStr[AWorkMode]+ ' WorkEnd'));
  GLogger.info(cWorkModeStr[AWorkMode]+ ' WorkEnd');
end;

procedure DoWork(const Self: TObject; ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  //writeln(Format(cWorkModeStr[AWorkMode]+ ' Current: %d', [AWorkCount]));
  GLogger.info(cWorkModeStr[AWorkMode]+ ' Current: %d', [AWorkCount]);
end;

var
 vBegin, vEnd: Longword;
procedure GrabUrl(const aUrl: string; aStream: TStream);
var
 vThread: PMeThread;
 vTask: PMeHttpDownloadSimpleTask;
begin
  New(vTask, Create(aURL, aStream));
  vThread := NewThreadTask(vTask);
  try
    vThread.OnException := TMeExceptionThreadEvent(ToMethod(@DoException));
    vTask.OnStatus := TIdStatusEvent(ToMethod(@DoStatus));
    vTask.OnWorkBegin := TWorkBeginEvent(ToMethod(@DoWorkBegin));
    vTask.OnWork := TWorkEvent(ToMethod(@DoWork));
    vTask.OnWorkEnd := TWorkEndEvent(ToMethod(@DoWorkEnd));
    vBegin := GetTickCount;
    //vHttp.Get(vURL, vStream);
    vThread.Start;
    while not vThread.Terminated do
    begin
      Sleep(100);
    end;
    vEnd := GetTickCount;
  finally
    vThread.Free;
  end;
end;

{procedure DoRedirect(const Self: TObject; Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: boolean; var VMethod: TIdHTTPMethod);
begin
  handled := true;
end;
 }
var
  i: Integer;
  FLogData : TStrings;
  FVerbose: Boolean;
 vFileName: string;
 vURL : string;
 vStream: TMemoryStream;
 vStrs: PMeStrings;
begin
 try
  Writeln(cCopyright);
  GLogger.AddLogger(New(PMeDebugLogger, Create));
  vStrs := New(PMeStrings, Create);
  GLogger.AddLogger(New(PMeStringsLogger, Create(vStrs)));
  GLogger.Level := vlAll;
  GLogger.Open;
  vURL := ParamStr(1);
  vFileName := '';
  if ParamCount >= 2 then
    vFileName := ParamStr(2);
  FVerbose := ParamCount >= 3;
  vStream := TMemoryStream.Create;
  FLogData := TStringList.Create;
  writeln(vURL);
  try
    GrabUrl(vURL, vStream);
    //vStream.Size := 0;
    //GrabUrl(vURL, vStream);
    if (vStrs.count > 0) then
    begin
      WriteLn('-------------------------');
      writeln(Trim(vStrs.Text));
      WriteLn('-------------------------');
    end;
    if vFileName = '' then
    begin
    end;
    if vFileName = '' then vFileName := 'index.htm';
    if not HasError and (vStream.Size > 0) then
    begin
      vStream.SaveToFile(vFileName);
      Writeln('Get ',vURL ,' Done. save to ',vFileName ,', Total Time:', vEnd - vBegin);
    end;
    //Sleep(500);
  finally
    vStream.Free;
    vStrs.Free;
    FreeAndNil(FLogData);
  end;
 except
   On E: Exception Do
     writeln('Exception Occur: ', E.ClassName, ' Error:', E.Message);
 end;
end.

