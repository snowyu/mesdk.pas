//AppFramework.txt

{Summary The MeService Library is a mini general SOA(service-oriented architecture) system framework..}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.2 $

Description
经过测试在DLL中使用FastMM或D2007以上版本可以互相访问代码段以及公用一个heap.

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

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeProcType
  ;

type
  PMeCustomService = ^ TMeCustomService;
  PMeServiceFunction = ^ TMeServiceFunction;
  PMeServiceFunctionList = ^ TMeServiceFunctionList;
  PMeServiceEventList = ^ TMeServiceEventList;
  PMeServiceList ＝ ^ TMeServiceList;

  TMeCustomService = object(TMeDynamicObject)
  protected
    FFunctions: PMeServiceFunctionList;
    FEvents: PMeServiceEventList;

    function GetFunctions: PMeServiceFunctionList;
    function GetEvents: PMeServiceEventList;

    procedure Init; virtual; {override}
  public
    destructor Destroy; virtual; {override}

  public
    property Functions: PMeServiceFunctionList read GetFunctions;
    property Events: PMeServiceEventList read GetEvents;
  end;

  TMeServiceFunction = object(TMeProcParams)
  protected
    FFuncEntry: Pointer;
  public
    procedure Execute(const aProc: Pointer = nil);virtual;//override
  end;
  
  TMeServiceFunctionList = object(TMeList)
  protected
  public
    //if success return the index in the list else return -1.
    function Register(const aProc: PTypeInfo; const aFuncEntry: Pointer; const aObj: TObject = nil): Integer;
  end;

  TMeServiceEventList = object(TMeList)
  protected
  public
    //if success return the index in the list else return -1.
    function Register(const aProc: PTypeInfo; const aFuncEntry: Pointer): Integer;
  end;

  TMeServiceList = object(TMeList)
  end;

  TMeCustomServiceMgr = object(TMeCustomService)
  protected
    FServices: PMeServiceList;
  end;

implementation

{ TMeCustomService }
procedure TMeCustomService.Init;
begin
  inherited;
end;

destructor TMeCustomService.Destroy;
begin
  MeFreeAndNil(FFunctions);
  MeFreeAndNil(FEvents);
  inherited;
end;

function TMeCustomService.GetFunctions: PMeServiceFunctionList;
begin
  if not Assgined(FFunctions) then New(FFunctions, Create);
  Result := FFunctions;
end;

function TMeCustomService.GetEvents: PMeServiceEventList;
begin
  if not Assgined(FEvents) then New(FEvents, Create);
  Result := FEvents;
end;

{$IFDEF MeRTTI_SUPPORT}
const
  cMeCustomServiceClassName: PChar = 'TMeCustomService';
{$ENDIF}

initialization
  SetMeVirtualMethod(TypeOf(TMeCustomService), ovtVmtParent, TMeDynamicObject);
  SetMeVirtualMethod(TypeOf(TMeServiceFunction), ovtVmtParent, TypeOf(TMeProcParams));
  SetMeVirtualMethod(TypeOf(TMeServiceFunctionList), ovtVmtParent, TypeOf(TMeList));
  SetMeVirtualMethod(TypeOf(TMeServiceEventList), ovtVmtParent, TypeOf(TMeList));
  SetMeVirtualMethod(TypeOf(TMeServiceList), ovtVmtParent, TypeOf(TMeList));


  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeCustomService), ovtVmtClassName, cMeCustomServiceClassName);
  SetMeVirtualMethod(TypeOf(TMeServiceFunction), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeServiceFunctionList), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeServiceEventList), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeServiceList), ovtVmtClassName, nil);
  {$ENDIF}
end.
