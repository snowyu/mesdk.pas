{Summary The MeService Function wrap the TMeProcParams to execute the function.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.2 $

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
    * The Original Code is $RCSfile: uMeServiceFunction.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeServiceFunction;

interface

{$I MeService.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  {$IFDEF SUPPORTS_MESERVICE_CALLEX}
  , Variants
  {$ENDIF}
  , TypInfo
  , uMeObject
  , uMeSysUtils
  , uMeProcType
  , uMeServiceTypes
  , uMeService
  ;

type
  PMeServiceFunction = ^ TMeServiceFunction;

  TMeServiceFunction = object(TMeProcParams)
  public
    Name: TMeIdentity;
    FuncEntry: Pointer;
    Instance: TObject;
    ProcParams: PTypeInfo;
  public
    destructor Destroy; virtual; //override;
    procedure Execute(const aProc: Pointer = nil); overload; virtual; //override
    function ExecuteByParam(const wParam, lParam: Cardinal): Integer; 
    function ExecuteByArray(const aParams : array of Variant): Variant;
    {$IFDEF SUPPORTS_MESERVICE_CALLEX}
    function ExecuteByVariant(const aParams : Variant): Variant;
    {$ENDIF}
  end;


implementation

{ TMeServiceFunction }
destructor TMeServiceFunction.Destroy;
begin
  Name := '';
  Inherited;
end;

procedure TMeServiceFunction.Execute(const aProc: Pointer = nil);
begin
  if Assigned(aProc) then
    inherited Execute(aProc)
  else
    inherited Execute(FuncEntry);
end;

function TMeServiceFunction.ExecuteByParam(const wParam, lParam: Cardinal): Integer;
begin
  if Assigned(FuncEntry) then
  begin
    if Assigned(Instance) then
      Result := TMeServiceMethodProc(FuncEntry)(Pointer(Instance), wParam, lParam)
    else
      Result := TMeServiceFunctionProc(FuncEntry)(wParam, lParam);
  end
  else
    Result := 0;
end;

function TMeServiceFunction.ExecuteByArray(const aParams : array of variant): variant;
var
  i: Integer;
begin
  if Assigned(SelfParam) then
    SelfParam.AsPointer := Instance;
  Assert(Count = High(aParams)+1, 'Parameter count error');
  for i := 0 to High(aParams) do
  begin
    Items[i].Value := aParams[i];
  end;
  inherited Execute(FuncEntry);
  if Assigned(ResultParam) then
    Result := ResultParam.Value
  else
    Result := 0;
end;

{$IFDEF SUPPORTS_MESERVICE_CALLEX}
function TMeServiceFunction.ExecuteByVariant(const aParams : Variant): Variant;
var
  i: Integer;
begin
  if Assigned(SelfParam) then
    SelfParam.AsPointer := Instance;
  Assert(1=VarArrayDimCount(aParams), 'aParams must be one dimension array');
  Assert(Count = VarArrayHighBound(aParams, 1) - VarArrayLowBound(aParams, 1)+1, 'Parameter count error');
  for i := VarArrayLowBound(aParams, 1) to VarArrayHighBound(aParams, 1) do
  begin
    Items[i].Value := aParams[i];
  end;
  inherited Execute(FuncEntry);
  if Assigned(ResultParam) then
    Result := ResultParam.Value
  else
    Result := 0;
end;
{$ENDIF}

{$IFDEF MeRTTI_SUPPORT}
const
  cMeCustomServiceClassName: PChar = 'TMeAbstractService';
{$ENDIF}

initialization
  SetMeVirtualMethod(TypeOf(TMeServiceFunction), ovtVmtParent, TypeOf(TMeProcParams));

  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeServiceFunction), ovtVmtClassName, nil);
  {$ENDIF}
end.
