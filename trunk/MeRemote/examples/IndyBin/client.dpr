program client;
{$AppType Console}
uses
  Windows, SysUtils
  , uMeObject
  , uMeTypes
  , uMeProcType
  , uMeRemoteUtils
  , uMeRemoteFuncFeature
  , uMeIndyClientTransport
  ;

type
  TAdd = function (const a, b: Integer): Integer of object;

function Add(const a, b: Integer): Integer;
begin
  asm
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
  end;
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
    //vTransport.KeepAlive := True; //todo: not impl yet.
    TMeRemoteFuncFeature.AddTo(@Add, 'add', TypeInfo(TAdd), vTransport);
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