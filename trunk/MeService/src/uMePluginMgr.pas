
{Summary The MePluginMgr is the plugin host for DLL files.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.1 $

Description

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
    * The Original Code is $RCSfile: uMePluginMgr.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):

}
unit uMePluginMgr;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeProcType
  , uMeServiceTypes
  , uMeService
  , uMeServiceMgr
  ;


type
  PMePluginInfo = ^TMePluginInfo;
  PMePluginManager = ^ TMePluginManager;

  TGetInfo = function(nInfoNo: Integer; lpBuffer: LPTSTR; nSize: Integer): Integer; stdcall;
  TInitialize = function(lpInitInfo: PMeServiceInitInfo): Integer; stdcall;
  TTerminate = function: Integer; stdcall;

  TMePluginInfo = object(TMeCustomService)
  protected
    FDllHandle: THandle;
    FFileName:  TFileName;

    FGetInfo: TGetInfo;
    FInitialize: TInitialize;
    FTerminate: TTerminate;
  public
    destructor Destroy; virtual; //override
    function LoadFromFile(const aFileName: TFileName): Boolean;
    function GetInfo(InfoNo: Integer; lpBuffer: LPTSTR; nSize: Integer): Integer;
    function GetInfoEx(InfoNo: Integer): AnsiString;
    function Initialize(lpInitInfo: PMeServiceInitInfo): Integer;
    function Terminate: Integer;


    property DllHandle: THandle read FDllHandle;
    property FileName: TFileName read FFileName;
  end;

  TMePluginManager = object(TMeCustomServiceManager)
  protected
    FMainThreadID: Cardinal;
    FMainThreadHandle: THandle;

    procedure Init; virtual; {override}
    class function ServiceClass: TMeClass; virtual; {override}
  public
    destructor Destroy; virtual; {override}
    function AddPlugin(const aFileName: TFileName): Boolean;
    //load the files in the path
    procedure LoadFromPath(const aPath: TFileName);
  public
  end;


implementation

{ TMePluginInfo }
destructor TMePluginInfo.Destroy; 
begin
  FFileName := '';
  inherited;
end;

function TMePluginInfo.GetInfo(InfoNo: Integer; lpBuffer: LPTSTR; nSize: Integer): Integer;
begin
  if @FGetInfo <> nil then
    Result := FGetInfo(InfoNo, lpBuffer, nSize)
  else
    Result := 0;
end;

function TMePluginInfo.GetInfoEx(InfoNo: Integer): AnsiString;
var
  vBuf: array[0..255] of Char;
begin
  if GetInfo(InfoNo, vBuf, SizeOf(vBuf)) > 0 then
    Result := vBuf
  else
    Result := '';
end;

function TMePluginInfo.Initialize(lpInitInfo: PMeServiceInitInfo): Integer;
begin
  if @FInitialize <> nil then
    Result := FInitialize(lpInitInfo)
  else
    Result := MEAPI_ERR_NOTIMPLEMENT;
end;

function TMePluginInfo.Terminate: Integer;
begin
  if @FTerminate <> nil then
    Result := FTerminate
  else
    Result := MEAPI_ERR_NOTIMPLEMENT;
end;

function TMePluginInfo.LoadFromFile(const aFileName: TFileName): Boolean;
var
  buf: array[0..255] of Char;
  i: Integer;
begin
  FDllHandle := LoadLibrary(PChar(AFileName));

  Result := FDllHandle <> 0;
  if Result then
  begin
    if GetModuleFileName(FDllHandle, buf, SizeOf(buf)-1) > 0 then
      FFileName := buf
    else
      FFileName := AFileName;

    FGetInfo := GetProcAddress(FDllHandle,  EXPORT_FUNC_GETINFO);
    FInitialize := GetProcAddress(FDllHandle, EXPORT_FUNC_INITIALIZE);
    FTerminate := GetProcAddress(FDllHandle, EXPORT_FUNC_TTERMINATE);
    Result := Assigned(FGetInfo) and Assigned(FInitialize) and Assigned(FTerminate);
  end;

  if Result then 
  begin
    Result := Initialize(Addr(PMePluginManager(GMeServiceManager).FServiceInitInfo)) = MEAPI_OK;
	if result then for i := MEAPI_INFO_FIRST to MEAPI_INFO_LAST do
      FInfo[i] := GetInfoEx(i);
  end;
end;

{ TMePluginManager }
procedure TMePluginManager.Init;
begin
  inherited;
  FMainThreadID := GetCurrentThreadId;
  DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(),
    @FMainThreadHandle, 0, FALSE, DUPLICATE_SAME_ACCESS);
end;

destructor TMePluginManager.Destroy;
begin
  CloseHandle(FMainThreadHandle);
  inherited;
end;

class function TMePluginManager.ServiceClass: TMeClass;
begin
  Result := TypeOf(TMePluginInfo);
end;

function TMePluginManager.AddPlugin(const aFileName: TFileName): Boolean;
var
  vPlugin: PMePluginInfo;
begin
  New(vPlugin, Create);
  Result := vPlugin.LoadFromFile(aFileName);
  if Result
     //and (vPlugin.ProtocolVersion = MEAPI_CURRENT_VER)
     and (vPlugin.Name <> MESRV_SYSTEM) 
  then
  begin
    Result := AddService(vPlugin) >= 0;
  end 
  else
  begin
    vPlugin.Free;
    Result := False;
  end;
end;

procedure TMePluginManager.LoadFromPath(const aPath: TFileName);
var
  sr: TSearchRec;
begin
  if FindFirst(aPath, faAnyFile, sr) = 0 then 
  try
    repeat
      AddPlugin(ExtractFilePath(aPath) + sr.Name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

initialization
  SetMeVirtualMethod(TypeOf(TMePluginInfo), ovtVmtParent, TypeOf(TMeCustomService));
  SetMeVirtualMethod(TypeOf(TMePluginManager), ovtVmtParent, TypeOf(TMeCustomServiceManager));


  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMePluginInfo), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMePluginManager), ovtVmtClassName, nil);
  {$ENDIF}
end.
