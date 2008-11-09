//AppFramework.txt

{Summary The MeService Library is a mini general SOA(service-oriented architecture) system framework.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.7 $

Description
经过测试在DLL中使用FastMM或D2007以上版本可以互相访问代码段以及公用一个heap.

uMeServiceTypes
  uMeService
    uMeServiceMgr
      uMePluginMgr
    uMePlugin : the plugin features implement in it(for Delphi).
      DLL 插件必须export 3 个函数: ServiceInfo, InitializeService,  TerminateService
      One DLL means one plugin only.

Service URN declaration samples:
  Function: System/Connection/UIService/Member.Enum
  Event: System/Connection.OnConnect

System Service:
  Functions:
  Events:
    System.OnModulesLoaded
  
Local:
  PluginMgr load plugin from DLL.
  PluginMgr ---> SerivceMgr
    manage the ServiceInfo --> CustomService 
  plugin features implementation in:  CustomPlugin --> CustomService --> AbstractService

Remote:
  1. Client: Crete RPC Plugin to do so.
       RemoteClientTransportPlugin
       RemoteClientPlugin
  2. Server: 
       RemoteServiceMgr --> PluginMgr
       RemoteServerTransportPlugin --> Plugin

过程调用支持两种模式:
  1. 内置的固定参数模式: wParam, lParam
  2. 增强模式：任意参数模式，参数传递通过 VariantArray. 暂未实现（通过编译开关打开: SUPPORTS_MESERVICE_CALLEX）

注：事件只支持固定参数模式。

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
    * The Original Code is $RCSfile: uMeService.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeService;

interface

{$I MeService.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeSysUtils
  , uMeProcType
  , uMeServiceTypes
  ;

type
  PMeAbstractService = ^ TMeAbstractService;

  TMeAbstractService = object(TMeDynamicObject)
  protected
    //collects the API Version, service name, etc info.
    FInfo: array[MEAPI_INFO_FIRST..MEAPI_INFO_LAST] of TMeIdentity;

    //function GetName: TMeIdentity;
    //procedure SetName(const Value: TMeIdentity);

    function GetInfo(const Index: Integer): TMeIdentity;
    procedure SetInfo(const Index: Integer; const Value: TMeIdentity);

    procedure Init; virtual; {override}
  public
    destructor Destroy; virtual; {override}

  public
    property Info[const Index: Integer]: TMeIdentity read GetInfo write SetInfo;
    property Name: TMeIdentity index MEAPI_NAME read GetInfo write SetInfo;
    property ProtocolVersion: TMeIdentity index MEAPI_VER read GetInfo write SetInfo;
    property Version: TMeIdentity index MEAPI_SERVICE_VER read GetInfo write SetInfo;
  end;

implementation

{ TMeAbstractService }
procedure TMeAbstractService.Init;
begin
  inherited;
end;

destructor TMeAbstractService.Destroy;
var
  i: Integer;
begin
  for i := Low(FInfo) to High(FInfo) do
   FInfo[i] := '';
  inherited;
end;

function TMeAbstractService.GetInfo(const Index: Integer): TMeIdentity;
begin
  if Index in [Low(FInfo)..High(FInfo)] then
    Result := FInfo[Index]
  else
    Result := '';
end;

procedure TMeAbstractService.SetInfo(const Index: Integer; const Value: TMeIdentity);
begin
  if Index in [Low(FInfo)..High(FInfo)] then
    FInfo[Index] := Value;
end;

{
function TMeAbstractService.GetName: TMeIdentity;
begin
  Result := FInfo[MEAPI_NAME];
end;

procedure TMeAbstractService.SetName(const Value: TMeIdentity);
begin
  FInfo[MEAPI_NAME] := Value;
end;
}

{$IFDEF MeRTTI_SUPPORT}
const
  cMeAbstractServiceClassName: PChar = 'TMeAbstractService';
{$ENDIF}

initialization
  SetMeVirtualMethod(TypeOf(TMeAbstractService), ovtVmtParent, TypeOf(TMeDynamicObject));


  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeAbstractService), ovtVmtClassName, cMeAbstractServiceClassName);
  {$ENDIF}
end.
