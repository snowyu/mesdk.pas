program server;
{$AppType Console}
uses
  SysUtils
  , uMeObject
  , uMeSystem
  , uMeTypes
  , uMeProcType
  , uMeRemoteUtils
  , uMeIndyServerTransport
  ;

type
  TAdd = function (const a, b: Integer): Integer of object;

function Add(const a, b: Integer): Integer;
begin
  Result := a + b;
end;

var
  vServer: TMeIndyRemoteFunctionServer;
begin
  vServer := TMeIndyRemoteFunctionServer.Create(nil);
  with vServer do
  try
   try
      with Bindings.Add do
      begin
       IP := '127.0.0.1';
       Port := 1111;
      end;
      RemoteFunctions.Register(ToMethod(@Add), 'add', TypeInfo(TAdd));
      Active := True;
      ReadLn;
      Active := False;
    except
      On E: Exception do
        Writeln('Exception(', E.ClassName, '):', E.Message);
    end;
  finally
    vServer.Free;
  end;
end.