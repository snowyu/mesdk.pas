program client;

{$I MeSetting.inc}

{$AppType Console}
uses
  Windows, SysUtils
  , uMeSystem
  , uMeObject
  , uMeTypes
  , uMeProcType
  , uMeRemoteUtils
  , uMeRemoteFuncFeature
  , uMeIndyClientTransport
  ;

type
  TAdd = function (const a, b: Integer): Integer of object;
  TRemoteAdd = class
    class function Add(const a, b: Integer): Integer;virtual; abstract;
  end;

function Add(const a, b: Integer): Integer;
begin
  {$I uMeMakeHole.inc}
  Result := -1;
end;

var
  vTransport: TMeIndyClientTransport;
  vA,vB, vC: Integer;
  vBegin, vEnd: Int64;
begin
  vA := 1;
  vB := 2;
  if ParamCount >= 1 then
    vA := StrToIntDef(ParamStr(1), 1);
  if ParamCount >= 2 then
    vB := StrToIntDef(ParamStr(2), 2);
  vTransport := TMeIndyClientTransport.Create();
  with vTransport.Client do
  try
   try
    Host := 'localhost';
    Port := 1111;
    vTransport.KeepAlive := True;
    TMeRemoteFuncFeature.AddTo(@Add, 'add', TypeInfo(TAdd), vTransport);
    //TMeRemoteFuncFeature.AddTo(@TRemoteAdd.Add, 'add', TypeInfo(TAdd), vTransport);

    QueryPerformanceCounter(vBegin);
    vC := Add(vA,vB);
    QueryPerformanceCounter(vEnd);
    writeln(vA,'+',vB,'=',vC);
    writeln('RunTime(QueryPerformanceCount):',vEnd-vBegin);
    //exit;

    //TMeRemoteFuncFeature.AddTo(@Add, 'add', TypeInfo(TAdd), vTransport);
    vA := 33;
    vB := 1234;
    QueryPerformanceCounter(vBegin);
    vC := Add(vA,vB);
    QueryPerformanceCounter(vEnd);
    writeln(vA,'+',vB,'=',vC);
    writeln('RunTime(QueryPerformanceCount):',vEnd-vBegin);
    except
      On E: Exception do
        Writeln('Exception(', E.ClassName, '):', E.Message);
    end;
  finally
    vTransport.Free;
  end;
end.