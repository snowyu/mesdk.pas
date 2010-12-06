

{Summary MeRemote Server Function Invoker.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision$


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
    * The Original Code is $RCSfile: uMeRemoteServerFunc.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeRemoteServerFunc;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , TypInfo
  , uMeObject
  , uMeStream
  , uMeSysUtils
  , uMeTypInfo
  , uMeTypes
  , uMeProcType
  , uMeRemoteUtils
  ;

type
  PMeRemoteFunction = ^ TMeRemoteFunction;

   {
      New(vParams, Create);
      vParams.InitFromType(); 
   }
  TMeRemoteFunction = object(TMeProcParams)
  protected
    //
    FName: string;
    FInstance: Pointer;
    FProc: Pointer;
    procedure Init;virtual; //override;
  public
    destructor Destroy;virtual; //override
    procedure rExecute();overload;
    procedure rExecute(const aIn, aOut: PMeStream);overload;

    property Instance: Pointer read FInstance write FInstance;
    property Name: string read FName write FName;
    property Proc: Pointer read FProc;
  end;

  PMeRemoteFunctions = ^ TMeRemoteFunctions;
  TMeRemoteFunctions = object(TMeThreadSafeList)
  protected
    function GetItem(const Index: Integer): PMeRemoteFunction;
  public
    destructor Destroy;virtual; //override
    procedure Register(const aMethod: TMethod; const aName: string; aProcParams: PTypeInfo = nil);
    function IndexOf(const aMethod: TMethod): Integer;
    function IndexOfName(const aName: string): Integer;
    function IsValid(const aName: string; const aMethod: TMethod): Boolean;
    function Execute(const aName: string; const aIn, aOut: PMeStream): Integer; overload;
    procedure Execute(const aIndex: Integer; const aIn, aOut: PMeStream); overload;

    property Items[const Index: Integer]: PMeRemoteFunction read GetItem;
  end;

implementation

{ TMeRemoteFunction }
procedure TMeRemoteFunction.Init;
begin
  inherited;
end;

destructor TMeRemoteFunction.Destroy;
begin
  FName := '';
  inherited;
end;

procedure TMeRemoteFunction.rExecute();
begin
  inherited Execute(FProc);
end;

procedure TMeRemoteFunction.rExecute(const aIn, aOut: PMeStream);
begin
  LoadParamsFromStream(@Self, aIn);
  rExecute;
  if Assigned(ResultParam) then
    writeln(ResultParam.AsString)
  else
    writeln('err');
  SaveParamsToStream(@Self, aOut);
end;

{ TMeRemoteFunctions }
destructor TMeRemoteFunctions.Destroy;
begin
  with LockList^ do
  try
    FreeMeObjects;
  finally
    UnlockList;
  end;
  inherited;
end;

procedure TMeRemoteFunctions.Execute(const aIndex: Integer; const aIn, aOut: PMeStream);
var
  vItem: PMeRemoteFunction;
begin
  vItem := Get(aIndex);
  vItem.rExecute(aIn, aOut);
end;

function TMeRemoteFunctions.Execute(const aName: string; const aIn, aOut: PMeStream): Integer;
begin
  Result := IndexOfName(aName);
  if Result >= 0 then
  begin
    Execute(Result, aIn, aOut);
  end;
end;

function TMeRemoteFunctions.GetItem(const Index: Integer): PMeRemoteFunction;
begin
  Result := inherited Get(Index);
end;

function TMeRemoteFunctions.IndexOf(const aMethod: TMethod): Integer;
var
  vItem: PMeRemoteFunction;
begin
  with LockList^ do
  try
  for Result := 0 to Count - 1 do
  begin
    vItem := List[Result];
    if (vItem.FInstance = aMethod.Data) and (vItem.FProc = aMethod.Code) then
      exit;
  end;
  finally
    UnlockList;
  end;
  Result := -1;
end;

function TMeRemoteFunctions.IndexOfName(const aName: string): Integer;
var
  vItem: PMeRemoteFunction;
begin
  with LockList^ do
  try
  for Result := 0 to Count - 1 do
  begin
    vItem := Items[Result];
    if (vItem.FName = aName) then
      exit;
  end;
  finally
    UnlockList;
  end;
  Result := -1;
end;

function TMeRemoteFunctions.IsValid(const aName: string; const aMethod: TMethod): Boolean;
var
  vItem: PMeRemoteFunction;
  i: Integer;
begin
  Result :=  (aName <> '') and Assigned(aMethod.Code);
  if Result then with LockList^ do
  try
    for i := 0 to Count - 1 do
    begin
      vItem := Items[i];
      if (vItem.FName = aName) or ((vItem.FInstance = aMethod.Data) and (vItem.FProc = aMethod.Code)) then
      begin
        Result := False;
        exit;
      end;
    end;
  finally
    UnlockList;
  end;
  Result := True;
end;

procedure TMeRemoteFunctions.Register(const aMethod: TMethod; const aName: string; aProcParams: PTypeInfo);
var
  vFunc: PMeRemoteFunction;
  vClass: TClass;
begin
  if IsValid(aName, aMethod) then
  begin
    New(vFunc, Create);
    if Assigned(aMethod.Data) and not Assigned(aProcParams) then
      aProcParams := TypeInfo(TMeObjectMethod);
    vClass := nil;
    if Assigned(aMethod.Data) and IsObject(aMethod.Data) then
      vClass := TObject(aMethod.Data).ClassType;
    vFunc.InitFromType(RegisterProcTypeInfo(aProcParams, vClass));
    vFunc.FInstance := aMethod.Data;
    if Assigned(vFunc.SelfParam) then
      vFunc.SelfParam.AsPointer := aMethod.Data;
    vFunc.FProc := aMethod.Code;
    vFunc.FName := aName;
    Add(vFunc);
  end;
end;

initialization
  SetMeVirtualMethod(TypeOf(TMeRemoteFunction), ovtVmtParent, TypeOf(TMeProcParams));
  SetMeVirtualMethod(TypeOf(TMeRemoteFunctions), ovtVmtParent, TypeOf(TMeThreadSafeList));
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeRemoteFunction), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeRemoteFunctions), ovtVmtClassName, nil);
  {$ENDIF}

finalization
end.
