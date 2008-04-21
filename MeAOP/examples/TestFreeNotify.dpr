program TestFreeNotify;

{$APPTYPE Console}
	
uses
  Windows, SysUtils
  , uMeSystem
  , uMeFeature
  ;

var
  v: TObject;

procedure NotifyFree(Self: TObject; Instance : TObject);
begin
  writeln('Instance=',Integer(Instance));
  writeln('v=',Integer(v));
  writeln('it should instance = v. :', Instance=v);
end;

begin
  try
    v := TObject.Create;
    AddFreeNotification(v, TFreeNotifyProc(ToMethod(@NotifyFree)));
  finally
    writeln('try free object..');
    v.Free;
  end;
end.
