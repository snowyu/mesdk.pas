

{ Summary: the AX Script Interface Declaration

  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uAXScript.pas,v $.
    * The Initial Developers of the Original Code are Serhiy Perevoznyk.
    * Portions created by Serhiy Perevoznyk is Copyright (C) 2001-2004
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.
    * Contributor(s):
    *  Serhiy Perevoznyk
    *  Riceball LEE
}

unit uAXScript;

interface

uses
  Windows, SysUtils, ActiveX, ComObj, Contnrs, Classes
  //, Forms
  {$IFDEF COMPILER6_UP}
  , Variants
  {$ENDIF}
  , uAXScriptInf
  ;


ResourceString
  SBreakPointManager='Breakpoint manager';

  SErrDuplicateBreakpoints='Duplicated breakpoint';
  SErrInvalidItemSize='Incorrect item size (%d)';
  SErrReferencedObject='Object has references';
  SErrEvaluateTimeOut='Exoression time is expired. Answer is not available';
  SErrBreakPointNotSet='Can not set breakpoint on this line';
  SErrProcessNotAccesible='process is not accessible';
  SErrBreakpointBegin = 'Incorrect breakpoint condition: ';
  SErrBreakpointEnd = '. Error message: ';

type
  TAXScriptErrorEvent = procedure(Sender : TObject; Line, Pos, Cookie : integer; ASrc : string; ADescription : string) of object;

  { TUnknownObject }

  TUnknownObject = class(TObject, IUnknown)
  protected
    FDestroying:Boolean;
    FRefCount: Integer;
  public
    destructor Destroy;override;

    function _AddRef:Integer;stdcall;
    function _Release:Integer;stdcall;
    function QueryInterface(Const IID:TGUID;out Obj):HResult;virtual;stdcall;

    property Destroying:Boolean read FDestroying write FDestroying;
    property RefCount: Integer read FRefCount;
  end;

  //TAxScriptProject = Class;

  TAXScriptGlobalObjects = class(TObject)
  private
    FIntfList: IInterfaceList;
    FNamedList: TStrings;
  public
    constructor Create;
    function GetNamedItemCount: Integer;
    function GetNamedItemName(I: Integer): string;
    procedure AddNamedIntf(const AName: string; AIntf: IUnknown);
    function FindNamedItemIntf(const AName: string): IUnknown;
    destructor Destroy; override;
    property NamedItemCount: Integer read GetNamedItemCount;
    property NamedItemName[I: Integer]: string read GetNamedItemName;
  end;

  TAXScriptSite = class({$IFDEF UseComp}TComponent{$ELSE}TInterfacedObject{$ENDIF}, IActiveScriptSite)
  protected
    FUseSafeSubset : boolean;
    FGlobalObjects : TAXScriptGlobalObjects;
    FOnError : TAXScriptErrorEvent;
    FEngine: IActiveScript;
    FParser: IActiveScriptParse;
    FScriptLanguage : string;
    procedure CreateScriptEngine(const Language: string);virtual;
    procedure SetScriptLanguage(const Value: string);
    function GetScriptState: SCRIPTSTATE;
    procedure SetScriptState(const Value: SCRIPTSTATE);
    procedure InitGlobalObjects(const aEngine: IActiveScript);
  protected
    procedure iExecute(const aCode: WideString);virtual;
    { IActiveScriptSite }
    function  GetLCID(out plcid: LongWord): HResult; stdcall;
    function GetItemInfo(
      pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
    function  GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    function  OnScriptTerminate(var pvarResult: OleVariant; var pexcepinfo: EXCEPINFO): HResult; stdcall;
    function  OnStateChange(ssScriptState: tagSCRIPTSTATE): HResult; stdcall;
    function  OnScriptError(const pScriptError: IActiveScriptError): HResult; stdcall;
    function  OnEnterScript: HResult; stdcall;
    function  OnLeaveScript: HResult; stdcall;
  public
    constructor Create({$IFDEF UseComp}aOwner: TComponent = nil{$ENDIF}); {$IFDEF UseComp}override;{$ELSE}virtual;{$ENDIF}
    destructor Destroy; override;
    function Release: Integer;
    function Compile(const aCode: WideString): HResult;
    function RunExpression(const ACode : Widestring) : string;
    procedure  Execute(const aCode : WideString);
    procedure CloseScriptEngine;
    procedure AddNamedItem(AName : string; AIntf : IUnknown);

    property ScriptState: SCRIPTSTATE read GetScriptState write SetScriptState;
  {$IFDEF UseComp}
  published
  {$ENDIF}
    property ScriptLanguage : string read FScriptLanguage write SetScriptLanguage;
    property OnError : TAXScriptErrorEvent read FOnError write FOnError;
    property UseSafeSubset : boolean read FUseSafeSubset write FUseSafeSubset default false;
  end;

  TAXScriptSiteWindow = class(TAXScriptSite, IActiveScriptSiteWindow)
  protected
    FWindowHandle: HWND;
    {IActiveSriptSiteWindow}
    function GetWindow(out pHwnd: HWND): HResult; stdcall;
    function EnableModeless(aEnable: BOOL): HResult; stdcall;
  public
    property WindowHandle: HWND read FWindowHandle write FWindowHandle;
  end;

procedure GetActiveScriptParse(List: TStrings);


implementation

procedure GetActiveScriptParse(List: TStrings);
var
  ProgID: string;

  function ValidProgID: Boolean;
  var
    PID: string;
  begin
     if Length(ProgID) > 7 then
       Result := AnsiCompareStr('.Encode', Copy(ProgID, Length(ProgID)-6, 7)) <> 0
     else
       Result := True;
     // Exclude XML script engine
     if CompareText(Copy(ProgID, 1, 3), 'XML') = 0 then
       Result := False;
     // Exclude "signed" script engines
     PID := UpperCase(ProgID);
     if Pos('SIGNED', PID) <> 0 then
       Result := False;
  end;
var
  EnumGUID: IEnumGUID;
  Fetched: Cardinal;
  Guid: TGUID;
  Rslt: HResult;
  CatInfo: ICatInformation;
  I, BufSize: Integer;
  ClassIDKey: HKey;
  S: string;
  Buffer: array[0..255] of Char;
begin
  List.Clear;
  Rslt := CoCreateInstance(CLSID_StdComponentCategoryMgr, nil,
    CLSCTX_INPROC_SERVER, ICatInformation, CatInfo);
  if Succeeded(Rslt) then
  begin
    OleCheck(CatInfo.EnumClassesOfCategories(1, @CATID_ActiveScriptParse, 0, nil, EnumGUID));
    while EnumGUID.Next(1, Guid, Fetched) = S_OK do
    begin
      try
        ProgID := ClassIDToProgID(Guid);
        if ValidProgID then
          List.Add(ProgID);
      except
        ProgID := ClassIDToProgID(StringToGUID(Buffer));
        List.Add('Invalid Entry In Categories');
      end;
    end;
  end else
  begin
    if RegOpenKey(HKEY_CLASSES_ROOT, 'CLSID', ClassIDKey) <> 0 then
      try
        I := 0;
        while RegEnumKey(ClassIDKey, I, Buffer, SizeOf(Buffer)) = 0 do
        begin
          S := Format('%s\Implemented Categories\%s',[Buffer,  { do not localize }
            GUIDToString(CATID_ActiveScriptParse)]);
          if RegQueryValue(ClassIDKey, PChar(S), nil, BufSize) = 0 then
          begin
            ProgID := ClassIDToProgID(StringToGUID(Buffer));
            if ValidProgID then
              List.Add(ProgID);
          end;
          Inc(I);
        end;
      finally
        RegCloseKey(ClassIDKey);
      end;
  end;
end;

{ TUnknownObject }

destructor TUnknownObject.Destroy;
begin
  if (RefCount <> 0) and not FDestroying then
    Raise Exception.CreateRes(@SErrReferenceDobject);
  Inherited;
end;

function TUnknownObject.QueryInterface(Const IID:TGUID;out Obj):HResult;
begin
  If GetInterface(IID,Obj) Then
    Result:=S_OK
  Else
    Result:=E_NOINTERFACE;
end;

function TUnknownObject._AddRef:Integer;
begin
  Result:=InterlockedIncrement(FRefCount);
end;

function TUnknownObject._Release: Integer;
begin
  Result:=InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    if not FDestroying then
    begin
      FDestroying := True;
      Destroy;
    end;
    Result:= 0;
  end;
end;


{ TAXScriptSite }

constructor TAXScriptSite.Create;
begin
  inherited;
  FScriptLanguage := 'VBScript';
  FGlobalObjects := TAXScriptGlobalObjects.Create;
  FUseSafeSubset := false;
end;

destructor TAXScriptSite.Destroy;
begin
  //CloseScriptEngine;
  FParser := nil;
  FEngine := nil;
  FGlobalObjects.Free;
  inherited;
end;

function TAXScriptSite.Release: Integer;
begin
  Result := _Release;
  {$IFDEF UseComp} 
  Free;
  {$ENDIF}
end;

procedure TAXScriptSite.AddNamedItem(AName: string;
  AIntf: IUnknown);
begin
  FGlobalObjects.AddNamedIntf(AName, AIntf);
end;



procedure TAXScriptSite.CreateScriptEngine(const Language: string);
const
  NULL_GUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
var
  ScriptCLSID : TGUID;
  LanguageW : WideString;
  hr : HRESULT;
  i : integer;
  pOs : IObjectSafety;
  dwSupported : DWORD;
  dwEnabled : DWORD;
  vIntf: IInterface;
begin
  CloseScriptEngine;
  LanguageW := Language;

  if CLSIDFromProgID(PWideChar(LanguageW), ScriptCLSID) <> S_OK then 
  begin
    ScriptCLSID := NULL_GUID;
    Raise Exception.Create('no such lang');
  end;

  hr := ActiveX.CoCreateInstance(ScriptCLSID, nil, CLSCTX_SERVER, IUnknown, vIntf);
  OLECHECK(hr);
  //vIntf  := CreateComObject(ScriptCLSID);
  //vIntf :=GetActiveOleObject(Language);
  //FEngine := CreateComObject(ScriptCLSID) as IActiveScript;
  hr := vIntf.QueryInterface(IActiveScript, FEngine);
  OLECHECK(hr);

  if FUseSafeSubset then
   begin
     dwSupported := 0;
     dwEnabled := 0;
     FEngine.QueryInterface(IObjectSafety, pOS);
     if Assigned(pOS) then
      begin
        pOS.GetInterfaceSafetyOptions(IDispatch, @dwSupported, @dwEnabled);
          if (INTERFACE_USES_SECURITY_MANAGER and dwSupported) = INTERFACE_USES_SECURITY_MANAGER then
           begin
             dwEnabled := dwEnabled or INTERFACE_USES_SECURITY_MANAGER;
           end;
         pOS.SetInterfaceSafetyOptions(IDispatch, INTERFACE_USES_SECURITY_MANAGER, dwEnabled);
      end;
    end;

  hr := FEngine.QueryInterface(IActiveScriptParse, FParser);
  OLECHECK(hr);

  hr := FEngine.SetScriptSite(Self);
  OLECHECK(hr);

  hr := FParser.InitNew();
  OLECHECK(hr);

  InitGlobalObjects(FEngine);

  {
  //!! the tamarin egine has no GetScriptDispatch interface!
  FEngine.GetScriptDispatch('', Disp);
  OLECHECK(hr);
  writeln(3);
  FDisp := Disp; //}
end;

procedure TAXScriptSite.InitGlobalObjects(const aEngine: IActiveScript);
var
  I: Integer;
begin
  for I := 0 to FGlobalObjects.NamedItemCount - 1 do
    aEngine.AddNamedItem(PWideChar(WideString(FGlobalObjects.NamedItemName[I])), SCRIPTITEM_ISVISIBLE or SCRIPTITEM_GLOBALMEMBERS);
end;

procedure TAXScriptSite.CloseScriptEngine;
begin
  FParser := nil;
  //FEngine.Close: it will call TAXScriptSite._Release
  //if FEngine <> nil then FEngine.Close;
  FEngine := nil;
end;

function TAXScriptSite.RunExpression(const ACode: WideString): string;
var
  AResult: OleVariant;
  ExcepInfo: TExcepInfo;
begin
  if not Assigned(FEngine) then
    CreateScriptEngine(FScriptLanguage);
  if FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0,
    SCRIPTTEXT_ISEXPRESSION, AResult, ExcepInfo) = S_OK
    then
      Result := AResult
    else
      Result := '';
end;

function TAXScriptSite.Compile(const aCode: WideString): HResult;
var
  vResult: OleVariant;
  ExcepInfo: TExcepInfo;
begin
  if not Assigned(FEngine) then
    CreateScriptEngine(FScriptLanguage);
  Result := FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0, SCRIPTTEXT_DELAYEXECUTION, vResult, ExcepInfo);
end;

procedure TAXScriptSite.iExecute(const aCode: WideString);
var
  vName: Widestring;
  vResult: OleVariant;
  ExcepInfo: TExcepInfo;
  vScript: IActiveScript;
  Disp: IDispatch;
begin
  FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0, SCRIPTTEXT_ISPERSISTENT or SCRIPTTEXT_ISVISIBLE or SCRIPTTEXT_DELAYEXECUTION, vResult, ExcepInfo);
end;

procedure TAXScriptSite.Execute(const aCode: WideString);
begin
  if not Assigned(FEngine) then
    CreateScriptEngine(FScriptLanguage);
  iExecute(aCode);
  OleCheck(FEngine.SetScriptState(SCRIPTSTATE_CONNECTED));
end;

function TAXScriptSite.GetScriptState: SCRIPTSTATE;
begin
  if Assigned(FEngine) then
  begin
    if FEngine.GetScriptState(Result) <> S_OK then
      Result := LongWord(-1);
  end
  else
    Result := LongWord(-1);
end;

procedure TAXScriptSite.SetScriptState(const Value: SCRIPTSTATE);
begin
  if Assigned(FEngine) then
    FEngine.SetScriptState(Value)
end;

function TAXScriptSite.GetDocVersionString(
  out pbstrVersion: WideString): HResult;
begin
  pbstrVersion := 'AX Script host 1.0';
  Result := S_OK;
  //Result := E_NOTIMPL;
end;

function TAXScriptSite.GetItemInfo(pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
var
  s: string;
begin
  if @ppiunkItem <> nil then Pointer(ppiunkItem) := nil;
  if @ppti <> nil then Pointer(ppti) := nil;
  if (dwReturnMask and SCRIPTINFO_IUNKNOWN) <> 0
    then ppiunkItem := FGlobalObjects.FindNamedItemIntf(pstrName);
  s := pstrName;
  //writeln(s, ' Get Item ppiunkItem=', Assigned(ppiunkItem));
  Result := S_OK;
end;

function TAXScriptSite.GetLCID(out plcid: LongWord): HResult;
begin
  plcid := GetSystemDefaultLCID;
  Result := S_OK;
end;

function TAXScriptSite.OnEnterScript: HResult;
begin
  result := S_OK;
end;

function TAXScriptSite.OnLeaveScript: HResult;
begin
  result := S_OK;
end;

function TAXScriptSite.OnScriptError(
  const pScriptError: IActiveScriptError): HResult;
var
  wCookie   : DWord;
  ExcepInfo : TExcepInfo;
  CharNo    : Integer;
  LineNo    : DWORD;
  SourceLineW : WideString;
  SourceLine : string;
  Desc : string;
begin
  Result := S_OK;
  wCookie := 0;
  LineNo  := 0;
  CharNo  := 0;
  if Assigned(pScriptError) then
    begin
      Desc := ExcepInfo.bstrDescription;
      pScriptError.GetSourcePosition(wCookie, LineNo, CharNo);
      pScriptError.GetSourceLineText(SourceLineW);
      SourceLine := SourceLineW;
      if Assigned(FOnError) then
        FOnError(Self, LineNo, CharNo, wCookie, SourceLine, Desc);
    end;
end;

function TAXScriptSite.OnScriptTerminate(var pvarResult: OleVariant;
  var pExcepInfo: EXCEPINFO): HResult;
begin
  Result := S_OK;
end;

function TAXScriptSite.OnStateChange(
  ssScriptState: tagSCRIPTSTATE): HResult;
begin
   case ssScriptState of
     SCRIPTSTATE_UNINITIALIZED:;
     SCRIPTSTATE_INITIALIZED:;
     SCRIPTSTATE_STARTED:;
     SCRIPTSTATE_CONNECTED:;
     SCRIPTSTATE_DISCONNECTED:;
     SCRIPTSTATE_CLOSED:;
   end;
  //writeln('ssScriptState:', ssScriptState);
   Result := S_OK;

end;

procedure TAXScriptSite.SetScriptLanguage(const Value: string);
begin
  if FScriptLanguage <> Value then
  begin
    FScriptLanguage := Value;
  end;
end;

{ TAXScriptSiteWindow }

function TAXScriptSiteWindow.EnableModeless(aEnable: BOOL): HResult;
begin
  Result := S_OK;
end;

function TAXScriptSiteWindow.GetWindow(out pHwnd: HWND): HResult;
begin
    pHwnd := FWindowHandle;
    Result := S_OK;
    //Result := S_FALSE;
end;


{ TAXScriptGlobalObjects }

procedure TAXScriptGlobalObjects.AddNamedIntf(const AName: string; AIntf: IUnknown);
begin
  FNamedList.Add(AName);
  FIntfList.Add(AIntf);
end;

constructor TAXScriptGlobalObjects.Create;
begin
  inherited Create;
  FNamedList := TStringList.Create;
  FIntfList := TInterfaceList.Create;
end;

destructor TAXScriptGlobalObjects.Destroy;
begin
  FNamedList.Free;
  inherited;
end;

function TAXScriptGlobalObjects.FindNamedItemIntf(const AName: string): IUnknown;
var
  I: Integer;
begin
  I := FNamedList.IndexOf(AName);
  if I >= 0 then
    Result := FIntfList[I]
  else
    Result := nil;
end;

function TAXScriptGlobalObjects.GetNamedItemCount: Integer;
begin
  Result := FNamedList.Count;
end;

function TAXScriptGlobalObjects.GetNamedItemName(I: Integer): string;
begin
  Result := FNamedList[I];
end;

end.
