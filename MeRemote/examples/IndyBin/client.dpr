program client;
{$AppType Console}
uses
  SysUtils
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
begin
  vTransport := TMeIndyClientTransport.Create();
  with vTransport.Client do
  try
   try
    Host := 'localhost';
    Port := 1111;
    TMeRemoteFuncFeature.AddTo(@Add, 'add', TypeInfo(TAdd), vTransport);
    writeln('1+2=',Add(1,2));
    except
      On E: Exception do
        Writeln('Exception(', E.ClassName, '):', E.Message);
    end;
  finally
    vTransport.Free;
  end;
end.