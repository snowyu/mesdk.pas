
{Summary the RTTI types info objects.}
{
   @author  Riceball LEE<riceballl@hotmail.com>
   @version $Revision: 1.40 $


  Usaage:

    Type
      TNumber = (one, two, three);

    PEnumerationType(TypeInfo(TNumber));

}
(*
 * The contents of this file are released under a dual license, and
 * you may choose to use it under either the Mozilla Public License 
 * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html) 
 * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
 * http://www.opensource.org/licenses/lgpl-license.php).
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is $RCSfile: TypInfoEx.pas,v $.
 *
 * The Initial Developers of the Original Code are Riceball LEE<riceballl@hotmail.com>.
 * Portions created by Riceball LEE<riceballl@hotmail.com> is Copyright (C) 2006-2007
 * All rights reserved.
 *
 * Contributor(s):
 *  hallvards
 *
 *)

unit TypInfoEx;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, TypInfo
  ;

type
  TCallingConvention = (ccUnknown=-1, ccRegister, ccCdecl, ccPascal, ccStdCall, ccSafeCall, ccFastCall, ccForth);

  PPPTypeEx = ^PPTypeEx;
  PPTypeEx = ^PTypeEx;
  PTypeEx = ^TTypeEx;
  PSetType = ^TSetType;
  PCustomOrdinalType = ^TCustomOrdinalType;
  POrdinalType = ^TOrdinalType;
  PEnumerationType = ^TEnumerationType;
  PInterfaceType = ^TInterfaceType;

  TTypeEx = object
  protected //Fields
    FKind: TTypeKind;
    FName: ShortString;
  public
    function TypeData: PTypeData;
  public
    Property Kind: TTypeKind read FKind;
    Property Name: ShortString read FName;
  end;

  TCustomOrdinalType = Object(TTypeEx)
  protected
    function GetOrdType: TOrdType;
  public
    {
     @param otSByte: Signed byte
     @param otUByte: Unsigned byte
     @param otSWord: Signed word
     @param otUWord: Unsigned word
     @param otSLong: Signed longword
     @param otULong: Unsigned longword
    }
    Property OrdType: TOrdType read GetOrdType;
  end;

  TSetType = Object(TCustomOrdinalType)
  protected
    //FCompType: PPTypeEx;
  protected
    function GetCompType: PTypeEx;
  public
    Property CompType: PTypeEx read GetCompType;
  end;

  {Ordinal: tkInteger, tkChar, tkEnumeration, tkWChar}
  TOrdinalType = Object(TCustomOrdinalType)
  protected
    //FMinValue: Longint;
    //FMaxValue: Longint;
  protected
    function GetMinValue: Longint;
    function GetMaxValue: Longint;
  public
    Property MinValue: Longint read GetMinValue;
    Property MaxValue: Longint read GetMaxValue;
  end;

  TEnumerationType = Object(TOrdinalType)
  protected
    //FBaseType: PPTypeEx;
    //FNameList: ShortString;
    {NameList: array[0..Count-1] of packed ShortString;
    EnumUnitName: packed ShortString));}
  protected
    function GetBaseType: TTypeEx;
    function GetNameList: PShortString;
    function GetEnumValue(const aName: string): Integer;
    function GetEnumName(Value: Integer): ShortString;
    function GetEnumUnitName: ShortString;
  public
    function Count: Integer; //=FMaxValue - FMinValue + 1
  public
    Property BaseType: TTypeEx read GetBaseType;
    Property EnumName[Index: Longint]: ShortString read GetEnumName;
    Property EnumValue[const Name: string]: Integer read GetEnumValue;
    Property EnumUnitName: ShortString read GetEnumUnitName;
  end;

  //TODO: not finished yet.
  TProcedureType = Object(TTypeEx)
  protected
    function GetMethodKind: TMethodKind;
    function GetParamCount: Byte;
  public
    Property MethodKind: TMethodKind read GetMethodKind;
    Property ParamCount: Byte read GetParamCount;
  end;

  PIntfMethodResult = ^TIntfMethodResult;
  TIntfMethodResult = packed record
    Name: ShortStringBase; //packed ShortString
    TypeInfo: PPTypeInfo;
  end;  
  PIntfParameter = ^TIntfParameter;
  TIntfParameter = packed record
    Flags: TParamFlags;
    ParamName: ShortStringBase; //packed ShortString; 
    TypeName: ShortStringBase; //packed ShortString; 
    TypeInfo: PPTypeInfo;
  end;

  PIntfMethodEntry = ^TIntfMethodEntry;
  TIntfMethodEntry = packed record
    Name: ShortStringBase; //packed ShortString; 
    Kind: TMethodKind; // mkProcedure or mkFunction
    CallConv: TCallingConvention;
    ParamCount: byte;  // including Self
    Parameters: packed array[0..High(byte)-1] of TIntfParameter;
    case TMethodKind of
      mkFunction: 
        (Result: TIntfMethodResult);
  end;
  PIntfEntry = ^TIntfEntry;
  TIntfEntry = packed record
    MethodCount: Word;   // #methods 
    MethodRTTICount: Word; // $FFFF if no method RTTI, 
    // #methods again if has RTTI 
    //Methods: packed array[0..$FFFF-1] of TIntfMethodEntry;
  end;

  PInterfaceMethod = ^TInterfaceMethod;
  TInterfaceMethod = Object
  protected
    function GetName: ShortStringBase; //packed ShortString; 
    function GetKind: TMethodKind; // mkProcedure or mkFunction
    function GetCallConv: TCallingConvention;
    function GetParamCount: byte;  // including Self
    function Parameters(const Index: Byte): PIntfParameter;
    function GetResult: TIntfMethodResult;
  public
    Property Name: ShortStringBase read GetName;
  end;
  (*
        IntfParent : PPTypeInfo; { ancestor }
        IntfFlags : TIntfFlagsBase;
        Guid : TGUID;
        IntfUnit : ShortStringBase;
        IntfEntry: TIntfEntry;
          MethodCount: Word;   // #methods 
          MethodRTTICount: Word; // $FFFF if no method RTTI, 
          // #methods again if has RTTI 
          Methods: packed array[0..High(Word)-1] of TIntfMethodEntry;

  *)
  TInterfaceType = Object(TTypeEx)
  protected
    function GetIntfEntry: PIntfEntry;

    function GetIntfParent: PTypeEx;
    function GetIntfFlags: TIntfFlagsBase;
    function GetGuid: TGUID;
    function GetIntfUnit: ShortStringBase;
    function GetMethodCount: Word;
    function GetMethodHasRTTI: Boolean;
  public
    Property ParentInterface: PTypeEx read GetIntfParent;
    Property Flags: TIntfFlagsBase read GetIntfFlags;
    Property Guid: TGUID read GetGuid;
    Property UnitName: ShortStringBase read GetIntfUnit;
    Property MethodCount: Word read GetMethodCount;
    Property HasRTTI: Boolean read GetMethodHasRTTI;
  end;

//writen by hallvards
function GetImplementorOfInterface(const I: IInterface): TObject; 

implementation

{## helper function ###}
function GetImplementorOfInterface(const I: IInterface): TObject; 
const 
  AddByte = $04244483;  
  AddLong = $04244481;  
type 
  PAdjustSelfThunk = ^TAdjustSelfThunk; 
  TAdjustSelfThunk = packed record 
    case AddInstruction: longint of 
      AddByte : (AdjustmentByte: shortint); 
      AddLong : (AdjustmentLong: longint); 
  end; 
  PInterfaceMT = ^TInterfaceMT; 
  TInterfaceMT = packed record 
    QueryInterfaceThunk: PAdjustSelfThunk; 
  end; 
  TInterfaceRef = ^PInterfaceMT; 
var 
  QueryInterfaceThunk: PAdjustSelfThunk; 
begin 
  Result := Pointer(I); 
  if Assigned(Result) then 
    try 
      QueryInterfaceThunk := TInterfaceRef(I)^. QueryInterfaceThunk; 
      case QueryInterfaceThunk.AddInstruction of 
        AddByte: Inc(PChar(Result), QueryInterfaceThunk.AdjustmentByte); 
        AddLong: Inc(PChar(Result), QueryInterfaceThunk.AdjustmentLong); 
        else     Result := nil; 
      end; 
    except 
      Result := nil; 
    end; 
end; 

//read packed ShortString.
function ReadPackedShortString(var P: Pointer): String;
var
  B: Byte;
begin
  B := Byte(P^);
  SetLength(Result, B);
  P := Pointer( Integer(P) + 1);
  Move(P^, Result[1], Integer(B));
  P := Pointer( Integer(P) + B );
end;

function ReadByte(var P: Pointer): Byte;
begin
  Result := Byte(P^);
  P := Pointer( Integer(P) + 1);
end;

function ReadWord(var P: Pointer): Word;
begin
  Result := Word(P^);
  P := Pointer( Integer(P) + 2);
end;

function ReadLong(var P: Pointer): Integer;
begin
  Result := Integer(P^);
  P := Pointer( Integer(P) + 4);
end;

function TTypeEx.TypeData: PTypeData;
begin
  Result := PTypeData(Integer(@FName[1]) + Length(FName));
end;

function TCustomOrdinalType.GetOrdType: TOrdType;
begin
  Result := TOrdType(Pointer(TypeData)^);
end;

function TSetType.GetCompType: PTypeEx;
begin
  //Result := FCompType^^;
  Result := PPPTypeEx(Integer(TypeData)+SizeOf(TOrdType))^^;
  //Result := PPTypeEx(Result)^;
end;

function TOrdinalType.GetMinValue: Longint;
begin
  Result := PLongint(Integer(TypeData)+SizeOf(TOrdType))^;
end;

function TOrdinalType.GetMaxValue: Longint;
begin
  Result := PLongint(Integer(TypeData)+SizeOf(TOrdType)+SizeOf(Longint))^;
end;

function TEnumerationType.GetBaseType: TTypeEx;
begin
  Result := PPTypeEx(Integer(TypeData)+SizeOf(TOrdType)+SizeOf(Longint)+SizeOf(Longint))^^;
end;

function TEnumerationType.Count: Integer;
begin
  Result := GetMaxValue - GetMinValue + 1;
end;

function TEnumerationType.GetNameList: PShortString;
begin
  Result := PShortString(Integer(TypeData)+SizeOf(TOrdType)+SizeOf(Longint)+SizeOf(Longint)+SizeOf(Pointer));
end;

function TEnumerationType.GetEnumValue(const aName: string): Integer;
var
  P: PShortString;
begin
  P := GetNameList;
  For Result := MinValue to MaxValue do
  begin
    if aName = P^ then
      exit;
    Inc(Integer(P), Length(P^) + 1);
  end;
  Result := -1;
end;

function TEnumerationType.GetEnumName(Value: Integer): ShortString;
var
  P: PShortString;
begin
  P := GetNameList;
  while Value <> 0 do
  begin
    Inc(Integer(P), Length(P^) + 1);
    Dec(Value);
  end;
  Result := P^;
end;

function TEnumerationType.GetEnumUnitName: ShortString;
var
  P: PShortString;
  i: Integer;
begin
  P := GetNameList;
  for i := MinValue to MaxValue do Inc(Integer(P), Length(P^) + 1);
  Result := P^;
end;

function TProcedureType.GetMethodKind: TMethodKind;
begin
  Result := TypeData.MethodKind;
end;

function TProcedureType.GetParamCount: Byte;
begin
  Result := TypeData.ParamCount;
end;

{ TInterfaceType }
function TInterfaceType.GetIntfParent: PTypeEx;
begin
  Result := PPPTypeEx(Integer(TypeData))^^;
end;

function TInterfaceType.GetIntfFlags: TIntfFlagsBase;
Type
  PIntfFlagsBase = ^ TIntfFlagsBase;
begin
  Result := PIntfFlagsBase(Integer(TypeData) + SizeOf(Pointer))^;
end;

function TInterfaceType.GetGuid: TGUID;
begin
  Result := PGuid(Integer(TypeData) + SizeOf(Pointer)+ SizeOf(TIntfFlagsBase))^;
end;

function TInterfaceType.GetIntfUnit: ShortStringBase;
begin
  Result := PShortString(Integer(TypeData) + SizeOf(Pointer)+ SizeOf(TIntfFlagsBase) + SizeOf(TGUID))^;
end;

function TInterfaceType.GetIntfEntry: PIntfEntry;
var
  vP: PChar;
begin
  vP := PChar(Integer(TypeData) + SizeOf(Pointer)+ SizeOf(TIntfFlagsBase) + SizeOf(TGUID));
  Result := PIntfEntry(Integer(vP) + SizeOf(Byte) + Ord(vP[0]));
end;

function TInterfaceType.GetMethodCount: Word;
begin
  Result := GetIntfEntry.MethodCount;
end;

function TInterfaceType.GetMethodHasRTTI: Boolean;
begin
  Result := GetIntfEntry.MethodRTTICount <> $FFFF;
end;

{ TInterfaceMethod }
function TInterfaceMethod.GetName: ShortStringBase; //packed ShortString; 
begin
end;

function TInterfaceMethod.GetKind: TMethodKind; // mkProcedure or mkFunction
begin
end;

function TInterfaceMethod.GetCallConv: TCallingConvention;
begin
end;

function TInterfaceMethod.GetParamCount: byte;  // including Self
begin
end;

function TInterfaceMethod.Parameters(const Index: Byte): PIntfParameter;
begin
end;

function TInterfaceMethod.GetResult: TIntfMethodResult;
begin
end;

initialization
finalization
end.
