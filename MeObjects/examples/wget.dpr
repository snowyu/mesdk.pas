program wget;
 
{Author: Riceball LEE}
 
{$AppType Console}
uses
  Windows,
  SysUtils, Classes
  , uMeThread
  , uMeIndyTask
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
begin
 try
  Writeln(cCopyright);
  vURL := ParamStr(1);
  vFileName := '';
  if ParamCount >= 2 then
    vFileName := ParamStr(2);
  FVerbose := ParamCount >= 3;
  vStream := TMemoryStream.Create;
  FLogData := TStringList.Create;
  New(vTask, Create(vURL));
  vThread := NewThreadTask(vTask);
  try
    vBegin := GetTickCount;
    //vHttp.Get(vURL, vStream);
    vThread.Start;
    while not vThread.Stopped do
    begin
      Sleep(100);
    end;
    vEnd := GetTickCount;
    if FVerbose then
        WriteLn('-------------------------');
    {
    for i := 0 to vHTTP.Response.RawHeaders.Count -1 do
    begin  
        FLogData.Add(vHTTP.Response.RawHeaders[i]);       
        if FVerbose then
        begin
          WriteLn(vHTTP.Response.RawHeaders[i]);
        end;
    end;}
 
    if vFileName = '' then
    begin
    end;
    if vFileName = '' then vFileName := 'index.htm';
    vTask.Stream.SaveToFile(vFileName);
    Writeln('Get ',vURL ,' Done. save to ',vFileName ,', Total Time:', vEnd - vBegin);
  finally
    FreeAndNil(FLogData);
    vStream.Free;
    vThread.Free;
  end;
 except
   On E: Exception Do
     writeln('Exception: ', E.ClassName, ' Error:', E.Message);
 end;
end.

