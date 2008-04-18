(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is psvActiveScript Library
 *
 * The Initial Developer of the Original Code is
 * Serhiy Perevoznyk
 *
 *
 * ***** END LICENSE BLOCK ***** *)

unit uAXScriptObj;

//demo how to write the ScriptObj 

interface

uses
  Classes, ObjComAuto, Forms, Dialogs;

type
  TActiveAppInfo = class(TInterfacedObject, IDispatch)
  protected
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

  {$METHODINFO ON}
  TActiveAppHelper = class(TObjectDispatch, IDispatch)
  public
    constructor Create;
    function GetExeName : WideString; stdcall;
    function GetTitle   : WideString; stdcall;
    function GetHint    : WideString; stdcall;
    procedure ECho(const s: string);
    function Version(): string;
  end;
  {$METHODINFO OFF}

implementation

uses
  ActiveX, Windows, SysUtils;


const
  DISPID_GETEXENAME  = 1;
  DISPID_SHOWMESSAGE = 2;


function TActiveAppInfo.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HResult;
type
  TDispIDsArray = array[0..0] of TDISPID;
  PDispIDsArray = ^TDispIDsArray;
var
  IDs: PDispIDsArray absolute DispIDs;
  i: integer;
  Name: WideString;
begin
  if NameCount > 1 then Result := DISP_E_UNKNOWNNAME
  else
    if NameCount < 1 then Result := E_INVALIDARG
    else Result := S_OK;
  for i := 0 to NameCount - 1 do
    IDs[i] := DISPID_UNKNOWN;
  if NameCount = 1 then
    begin
      Name := PWideChar(Names^);
      if UpperCase(Name) = 'GETEXENAME' then IDs[0] := DISPID_GETEXENAME else
      if UpperCase(Name) = 'SHOWMESSAGE' then IDs[0] := DISPID_SHOWMESSAGE else
       Result := DISP_E_UNKNOWNNAME;
    end;
end;

function TActiveAppInfo.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TActiveAppInfo.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TActiveAppInfo.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;

type
  PVariantArray = ^TVariantArray;
  TVariantArray = array[0..65535] of Variant;
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..65535] of Integer;
var
  Parms: PDispParams;
  FRetValue : Variant;
begin
  if (DispID = DISPID_GETEXENAME) then
    begin
      if VarResult = nil then
        VarResult := @FRetValue;
      POleVariant(VarResult)^ := Application.ExeName;
      Result := S_OK;
    end
  else
  if (DispID = DISPID_SHOWMESSAGE) then
    begin
      Parms := @Params;
      ShowMessage(PVariantArray(Parms.rgvarg)^[0]);
      Result := S_OK;
    end
  else 

Result := DISP_E_MEMBERNOTFOUND;
end;


{ TActiveAppHelper }

constructor TActiveAppHelper.Create;
begin
  inherited Create(Self, false);
end;

function TActiveAppHelper.GetExeName: WideString;
begin
  Result := Application.ExeName;
end;

function TActiveAppHelper.GetHint: WideString;
begin
  Result := Application.Hint;
end;

function TActiveAppHelper.GetTitle: WideString;
begin
  Result := Application.Title;
end;

procedure TActiveAppHelper.ECho(const s: string);
begin
  writeln(s);
end;

function TActiveAppHelper.Version(): string;
begin
  Result := '1.0.0.0';
end;
end.
