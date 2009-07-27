library plugintest;

uses
  uMeServiceTypes
  , uMePlugin
  ;

type
  PMeTestPlugin =^ TMeTestPlugin;
  TMeTestPlugin = object(TMePluginService)
  protected
    procedure Init; virtual; {override}
    function DoInitialize: Integer; virtual; //override
  public
  end;
procedure TMeTestPlugin.Init;
begin
  inherited;
end;

function TMeTestPlugin.DoInitialize: Integer;
begin
  Result := inherited DoInitialize();
  if Result = MEAPI_OK then
  begin
  end;  
end;

var
  //GTestPlugin: PMeTestPlugin;
  SaveDllProc: Pointer;

procedure LibExit(Reason: Integer);
begin
  if Reason = DLL_PROCESS_DETACH then
  begin
    // library exit code
    SetPlugin(nil);
  end;
  SaveDllProc(Reason);  // call saved entry point procedure
end;

begin
  SetPlugin(New(PMeTestPlugin, Create));
  SaveDllProc := DllProc;  // save exit procedure chain
  DllProc := @LibExit;  // install LibExit exit procedure
end.
