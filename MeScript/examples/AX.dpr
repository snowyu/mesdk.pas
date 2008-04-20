//{$IMAGEBASE $008000000}
program AX;

{$AppType Console}

uses
  windows, SysUtils, Classes,
  ComObj,ObjComAuto,
  ActiveX
  , uMeSystem
  , uAXScript
  , uAXScriptInf
  , uAXScriptObj
  ;


//function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult;
type
  TDllGetClassObjectProc = function (const CLSID, IID: TGUID; var Obj): HResult;stdcall;

function MyCoCreateInstance(const aDLLName: string; const aGuid: TGUID; unkOuter: IUnknown; dwClsContext: Longint; const iid: TIID; out pv: IUnknown): HModule;
var
  vGetClassObject: TDllGetClassObjectProc;
  vFactory: IClassFactory;
  hr :HRESULT;
begin
  Result := LoadLibrary(PChar(aDLLName));
  if Result = 0 then 
  begin
    raise Exception.Create('Load Lib DLL failed  '+aDLLName);
  end;
  try
    vGetClassObject := GetProcAddress(Result, 'DllGetClassObject');
    if not Assigned(vGetClassObject) then
    begin
      if Result <> 0 then
        FreeLibrary(Result);
      Result := 0;
      raise Exception.Create('no DllGetClassObject function found in '+aDLLName);
    end;
    hr := vGetClassObject(aGuid, IClassFactory, vFactory);
    OleCheck(hr);
    Assert(Assigned(vFactory));
    hr := vFactory.CreateInstance(unkOuter, iid, Result);
    OleCheck(hr);
  except
    if Result <> 0 then
    begin
      FreeLibrary(Result);
      Result := 0;
    end;
  end;
end;

//function SomeFunction(var p: pointer): HRESULT; stdcall;external 'te' name 'SomeFunction@4';

{function CoCreateInstance(const clsid: TCLSID; unkOuter: IUnknown;
  dwClsContext: Longint; const iid: TIID; out pv): HResult; stdcall;external 'ole32' name 'CoCreateInstance';



//}

procedure DoScriptError(Self: TObject; Sender : TObject; Line, Pos : integer; ASrc : string; ADescription : string);
begin
  Writeln('Script(', Line, ',', Pos, ') Error:', ADescription);
  if ASrc <> '' then
    Writeln( ' in  ', ASrc);
end;

const
  NULL_GUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
  IID_IUnknown: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
var
  ScriptCLSID : TGUID;
  vIntf: IInterface;
  hr: HRESULT;
  a: IInterface;
  vObj: IDispatch;
  //FEngine: IActiveScript;
  //FParser: IActiveScriptParse;
  FSite: TAXScriptSite;//IActiveScriptSite;
  oldCW: Word;
  vDLL: HModule = 0;
  vScript: string;
begin
  if System.ParamCount < 2 then exit;
  oldCW := Default8087CW;
  Default8087CW := Default8087CW or $3F;
  //disable the FPU Exception.
  Set8087CW(Default8087CW);

  //try
  CoInitialize(nil);
  FSite := TAXScriptSite.Create;
  try
    FSite.ScriptLanguage := ParamStr(1);
    writeln('Lang=',FSite.ScriptLanguage);
    vObj := TActiveAppHelper.Create;
    FSite.AddNamedItem('WScript', vObj);
    FSite.OnError := TAXScriptErrorEvent(ToMethod(@DoScriptError));

    with TStringList.Create do
    try
      try
        LoadFromFile(ParamStr(2));
        writeln('loading file:', ParamStr(2));
        vScript := Text;
      except
        vScript := ParamStr(2);
      end;
    finally
      free;
    end;
    //Writeln(vScript);
    try
    FSite.Execute(vScript);
    except
      on e: exception do
        Writeln(e.className, ' ', e.message);
    end;
  finally
    FSite.Release;//it will raise error for RefCount <> 0 if it is TInterfacedObject 
  end;


(*
  vIntf := NIL;
  {hr := CoCreateInstance(ScriptCLSID, Nil, CLSCTX_INPROC_SERVER, IUnknown, vIntf);
  OLECHECK(hr);
  //}
  vDLL := MyCoCreateInstance('E:\Dev\TamarinScriptEngine\axtam.dll', ScriptCLSID, NIL, CLSCTX_INPROC_SERVER, IUnknown, vIntf);
  Assert(vDLL <> 0);

  hr := vIntf.QueryInterface(IActiveScript, FEngine);
  OLECHECK(hr);
  
  FSite := TpsvActiveScriptSite.Create();
  hr := FEngine.QueryInterface(IActiveScriptParse, FParser);
  OLECHECK(hr);

  hr := FEngine.SetScriptSite(FSite);
  OLECHECK(hr);

  hr := FEngine.GetScriptSite(IActiveScriptSite, Pointer(FOSite));
  OLECHECK(hr);
  F2 := FSite;
  Assert(F2=FOSite);

  hr := FParser.InitNew();
  OLECHECK(hr);
*)
  
  {Except
    on e: Exception do
    begin
      writeln('Exception ', E.ClassName, ' with ', E.Message);
    end;
  end;//}
end.
