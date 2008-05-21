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
 vThread: PMeThread;
 vTask: PMeHttpDownloadSimpleTask;
 vURL : string;
 vStream: TMemoryStream;
 vBegin, vEnd: Longword;
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
  New(vTask, Create(vURL, vStream));
  vThread := NewThreadTask(vTask);
  try
    vThread.OnException := TMeExceptionThreadEvent(ToMethod(@DoException));
    vTask.OnStatus := TIdStatusEvent(ToMethod(@DoStatus));
    vBegin := GetTickCount;
    //vHttp.Get(vURL, vStream);
    vThread.Start;
    while not vThread.Terminated do
    begin
      Sleep(100);
    end;
    vEnd := GetTickCount;
    {
    for i := 0 to vHTTP.Response.RawHeaders.Count -1 do
    begin  
        FLogData.Add(vHTTP.Response.RawHeaders[i]);       
        if FVerbose then
        begin
          WriteLn(vHTTP.Response.RawHeaders[i]);
        end;
    end;}
 
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
    vStrs.Free;
    FreeAndNil(FLogData);
    vStream.Free;
    vThread.Free;
  end;
 except
   On E: Exception Do
     writeln('Exception Occur: ', E.ClassName, ' Error:', E.Message);
 end;
end.

