program TestFreeNotify;

{$APPTYPE Console}
	
uses
  Windows, SysUtils
  , uMeSystem
  , uMeObject
  , uMeSysUtils
  ;

var
  v: Pointer;
  v2: Pointer;

procedure NotifyFree(const Self: Pointer; const Instance : Pointer);
begin
  writeln('Instance=',Integer(Instance));
  writeln('v=',Integer(v));
  writeln('v2=',Integer(v2));
  writeln('it should instance = v. :', Instance=v);
end;

begin
  try
    v := TObject.Create;
    v2 := New(PMeDynamicObject, Create);
    //PMeDynamicObject(v2).destroy;
    AddFreeNotification(v, TFreeNotifyProc(ToMethod(@NotifyFree)));
    writeln('hell1');
    AddFreeNotification(v2, TFreeNotifyProc(ToMethod(@NotifyFree)));
    writeln('hell2');
  finally
    writeln('try free object..');
    TObject(v).Free;
    PMeDynamicObject(v2).Free;
  end;

  try
    v := New(PMeDynamicObject, Create);
    writeln('h3');
    AddFreeNotification(v, TFreeNotifyProc(ToMethod(@NotifyFree)));
  finally
    writeln('try free Meobject..');
    PMeDynamicObject(v).Free;
  end;
end.
