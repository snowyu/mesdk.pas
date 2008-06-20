

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
  , uMeObject
  , uMeStream
  , uMeSysUtils
  , uMeTypInfo
  , uMeProcType
  , uMeRemoteUtils
  ;

type
  PMeRemmoteFunction = ^ TMeRemmoteFunction;

   {
      New(vParams, Create);
      vParams.InitFromType(); 
   }
  TMeRemmoteFunction = object(TMeProcParams)
  protected
    //
    FName: string;
    FInstance: Pointer;
    FProc: Pointer;
    procedure Init;virtual; //override;
  public
    destructor Destroy;virtual; //override
    procedure Execute();overload;
    procedure Execute(const aIn, aOut: PMeStream);overload;

    property Instance: Pointer read FInstance write FInstance;
    property Name: string read FName write FName;
    property Proc: Pointer read FProc;
  end;

  PMeRemmoteFunctions = ^ TMeRemmoteFunctions;
  TMeRemmoteFunctions = object(TMeThreadSafeList)
  protected
    function GetItem(const Index: Integer): PMeRemmoteFunction;
  public
    destructor Destroy;virtual; //override
    procedure Register(const aMethod: TMethod; const aName: string; aProcParams: PTypeInfo = nil);
    function IndexOf(const aMethod: TMethod): Integer;
    function IndexOfName(const aName: string): Integer;
    function IsValid(const aName: string; const aMethod: TMethod): Boolean;
    function Execute(const aName: string; const aIn, aOut: PMeStream): Integer; overload;
    procedure Execute(const aIndex: Integer; const aIn, aOut: PMeStream); overload;

    property Items[const Index: Integer]: PMeRemmoteFunction read GetItem;
  end;

implementation

{ TMeRemmoteFunction }
procedure TMeRemmoteFunction.Init;
begin
  inherited;
end;

destructor TMeRemmoteFunction.Destroy;
begin
  FName := '';
  inherited;
end;

procedure TMeRemmoteFunction.Execute();
begin
  inherited Execute(FProc);
end;

procedure TMeRemmoteFunction.Execute(const aIn, aOut: PMeStream);
begin
  LoadParamsFromStream(@Self, aIn);
  Execute;
  SaveParamsToStream(@Self, aOut);
end;

{ TMeRemmoteFunctions }
destructor TMeRemmoteFunctions.Destroy;
begin
  FreeMeObjects;
  inherited;
end;

procedure TMeRemmoteFunctions.Execute(const aIndex: Integer; const aIn, aOut: PMeStream);
var
  vItem: PMeRemmoteFunction;
begin
  vItem := Get(aIndex);
  vItem.Execute(aIn, aOut);
end;

function TMeRemmoteFunctions.Execute(const aName: string; const aIn, aOut: PMeStream): Integer;
begin
  Result := IndexOfName(aName);
  if Result >= 0 then
  begin
    Execute(Result, aIn, aOut);
  end;
end;

function TMeRemmoteFunctions.GetItem(const Index: Integer): PMeRemmoteFunction;
begin
  Result := inherited Get(Index);
end;

function TMeRemmoteFunctions.IndexOf(const aMethod: TMethod): Integer;
var
  vItem: PMeRemmoteFunction;
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

function TMeRemmoteFunctions.IndexOfName(const aName: string): Integer;
var
  vItem: PMeRemmoteFunction;
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

function TMeRemmoteFunctions.IsValid(const aName: string; const aMethod: TMethod): Boolean;
var
  vItem: PMeRemmoteFunction;
begin
  Result :=  (aName <> '') and Assigned(aMethod.Code);
  if Result then with LockList^ do
  try
    for Integer(Result) := 0 to Count - 1 do
    begin
      vItem := Items[Integer(Result)];
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

procedure TMeRemmoteFunctions.Register(const aMethod: TMethod; const aName: string; aProcParams: PTypeInfo);
var
  vFunc: PMeRemmoteFunction;  
begin
  if IsValid(aName, aMethod) then
  begin
    New(vFunc, Create);
    if Assigned(aMethod.Data) and not Assigned(aProcParams) then
      aProcParams := TypeInfo(TMeObjectMethod);
    vFunc.InitFromType(aProcParams);
    vFunc.FInstance := aMethod.Data;
    vFunc.FProc := aMethod.Code;
    vFunc.FName := aName;
    Add(vFunc);
  end;
end;

initialization
  SetMeVirtualMethod(TypeOf(TMeRemmoteFunction), ovtVmtParent, TypeOf(TMeProcParams));
  SetMeVirtualMethod(TypeOf(TMeRemmoteFunctions), ovtVmtParent, TypeOf(TMeThreadSafeList));
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeRemmoteFunction), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeRemmoteFunctions), ovtVmtClassName, nil);
  {$ENDIF}

finalization
end.
