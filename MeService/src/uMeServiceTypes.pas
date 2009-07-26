
{Summary The general types and constants for SOA(service-oriented architecture) system framework .}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.2 $

Description
经过测试在DLL中使用FastMM或D2007以上版本可以互相访问代码段以及公用一个heap.
the service function name and event name is full URI(includes the ServiceName):
  ServiceName.FuncName

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
    * The Original Code is $RCSfile: uMeServiceTypes.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeServiceTypes;

interface

{$I MeService.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeProcType
  ;

const
  EXPORT_FUNC_GETINFO    = 'ServiceInfo';
  EXPORT_FUNC_INITIALIZE = 'InitializeService';
  EXPORT_FUNC_TTERMINATE = 'TerminateService';

  MEAPI_ERR_NOTIMPLEMENT        = -1; 
  MEAPI_OK                      = 0;  
  MEAPI_ERR_INTERNAL            = 100;
  MEAPI_ERR_UNINITIALIZED       = 101;
  MEAPI_ERR_ALREADY_INITIALIZED = 102;

const
  {$IFNDEF SUPPORTS_MESERVICE_CALLEX}
  MEAPI_CURRENT_VER        = '0.1';
  {$ELSE}
  MEAPI_CURRENT_VER        = '0.2';
  {$ENDIF}

  //MEAPI Info constants:
  MEAPI_VER                = 0;        // the supports api protocal version
  MEAPI_NAME               = 1;        // Service name
  MEAPI_TITLE              = 2;        // Service Title
  MEAPI_DESCRIPTION        = 3;        // Service Description
  MEAPI_AUTHOR             = 4;        // Service Author
  MEAPI_COPYRIGHT          = 5;        // Copyright
  MEAPI_SERVICE_VER        = 6;        // Service(Plugin)-version
  MEAPI_INFO_FIRST         = MEAPI_VER;
  MEAPI_INFO_LAST          = MEAPI_SERVICE_VER;

  MEID_SPLITER             = '.';
  MEID_PATH                = '/';

  MESRV_SYSTEM         = 'System';
  //All Modules loaded notification event.
  MESRV_SYSTEM_MODULESLOADED    = MESRV_SYSTEM + '/OnModulesLoaded';



type
  LPCTSTR = PAnsiChar;
  TMeIdentity = AnsiString;

  { handle 0 means null. }
  HMeService = Cardinal;
  HMeServiceFunction = Cardinal;
  HMeServiceEvent    = Cardinal;

  TMeServiceFunctionProc = function (const wParam, lParam: Cardinal): Integer; stdcall;
  TMeServiceMethodProc = function (const Self:Pointer; const wParam, lParam: Cardinal): Integer; stdcall;
  TMeServiceMethod = function (const wParam, lParam: Cardinal): Integer of object; stdcall;

  TMeServiceEventProc = function (const wParam, lParam, lResult: Cardinal): Integer; stdcall;
  TMeServiceEventMethod = function (const wParam, lParam, lResult: Cardinal): Integer of object; stdcall;

  TMeCreateServiceProc = function (const lpServiceName: LPCTSTR): HMeService; stdcall;
  TMeCreateServiceFunctionProc = function (const lpServiceFunctionName: LPCTSTR; const lpProcAddr: Pointer
    ; const lpProcParams: Pointer = nil; const lpInstance: Pointer = nil): HMeServiceFunction; stdcall;
  TMeGetServiceProc = function (const lpName: LPCTSTR): HMeService; stdcall;
  TMeGetServiceFunctionProc = function (const lpFunctionName: LPCTSTR; const aProcParams: PTypeInfo): HMeServiceFunction; stdcall;
  TMeCallServiceFunctionProc = function (const hFuncHandle: HMeServiceFunction; const wParam, lParam: Cardinal): Integer; stdcall;
  TMeCallServiceFunctionExProc = function (const hFuncHandle: HMeServiceFunction; const Params: variant): Integer; stdcall;
  TMeCreateServiceEventProc = function (const lpEventName: LPCTSTR): HMeServiceEvent; stdcall;
  TMeGetServiceEventProc = function (const lpEventName: LPCTSTR): HMeServiceEvent; stdcall;
  TMeNotifyServiceEventProc = function (const hEventHandle: HMeServiceEvent; wParam, lParam: Cardinal): Integer; stdcall;
  TMeHookServiceEventProc = function (const lpEventName: LPCTSTR; const lpProcAddr: Pointer; const lpInstance: Pointer = nil): Integer; stdcall;
  TMeUnhookServiceEventProc = function (const lpEventName: LPCTSTR; const lpProcAddr: Pointer; const lpInstance: Pointer = nil): Integer; stdcall;

  PMeServiceInitInfo = ^ TMeServiceInitInfo;
  TMeServiceInitInfo = record
    cbSize: Integer;
    RegisterService: TMeCreateServiceProc;
    RegisterFunction: TMeCreateServiceFunctionProc;
    GetService: TMeGetServiceProc;
    GetFunction: TMeGetServiceFunctionProc;
    CallFunction: TMeCallServiceFunctionProc;
    CallFunctionEx: TMeCallServiceFunctionExProc;
    CreateEvent: TMeCreateServiceEventProc;
    GetEvent: TMeGetServiceEventProc;
    NotifyEvent: TMeNotifyServiceEventProc;
    HookEvent: TMeHookServiceEventProc;
    UnhookEvent: TMeUnhookServiceEventProc;
  end;

implementation


initialization
end.
