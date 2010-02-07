
{Summary: the Mini Run-time Type Infomation of Object. the MeType is stream-able.. it's the basic types info object.}
{

example: 
How to Register a New type:
<code>
uses
  TypInfo, uMeTypes;

type
  TMyInt = Longword;
  TMyRecord = packed record
    aField1: Integer;
    aField2: Integer;
  end;
  ...

Procedure RegisterMyTypes;
var
  FMyRecordType: PMeRecordType;
  FMyIntType: PMeCustomIntegerType;
begin
  FMyRecordType := New(PMeRecordType, Create);
  FMyRecordType.Name := 'TMyRecord'; //Must as same as your decalare name(include case)
  FMyRecordType.IsPacked := True;
  FMyRecordType.Add('aField1', 'Integer');
  FMyRecordType.Add('aField2', 'Integer');

  FMyIntType := New(PMeCustomIntegerType, Create);
  FMyIntType.Name := 'MyInt'; //Must as same as your decalare name(include case)
  FMyIntType.OrdType := otULong;
  FMyIntType.MinValue := 0;
  FMyIntType.MaxValue := Longint(High(LongWord));

  RegisterType(FMyRecordType);
  RegisterType(FMyIntType);
end;

initilization

finalization
end;
</code>

  License:
    * The contents of this file are released under the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeTypes.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2006-2010
    * All rights reserved.

    * Contributor(s):
}
unit uMeTypes;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  {$IFDEF DEBUG}
  , DbugIntf
  {$ENDIF}
  , Variants
  , TypInfo
  , uMeConsts
  , uMeSystem
  , uMeObject
  , uMeTypInfo //the TCallingConvention
  ;

const
  cMaxIdentityNameLen = 31;

type
  EMeTypeError = class(EMeError);

  TMePropertySpecifier = (mpsGet, mpsSet, mpsStored);
  TMePropertySpecifiers = set of TMePropertySpecifier;
  {: the most basis types 最基本的类型}
  {
    @param mtkLString Long AnsiString
    @param mtkString  Short AnsiString
    @param mtkQWord   8 bytes
    @param mtkClass   the class type
    @param mtkObject  the object type
    @param mtkMethod  the procedure or method type
    @param mtkParam   the prcocedural parameter type(internal)
    Note: (mtkUnknown..mtkDynArray) = TypInfo.TTypeKind
  }
  TMeTypeKind = (
    mtkUnknown,  mtkInteger,  mtkChar,     mtkEnumeration, mtkFloat,
    mtkString,   mtkSet,      mtkClass,    mtkMethod,      mtkWChar, 
    mtkLString,  mtkWString,  mtkVariant,  mtkArray,       mtkRecord, 
    mtkInterface,mtkInt64,    mtkDynArray, mtkQWord,       mtkObject,
    mtkParam,    mtkPointer,  mtkProcedure
    );

  {: the Calling Convention }
  { 
   @param ccFastCall the micrsoft fastcall calling convention. use ECX,EDX first, then push Right-to-left, Routine cleanup, 
   @param ccForth is the TurboScript calling convention!
  }
  //TCallingConvention = (ccUnknown=-1, ccRegister, ccCdecl, ccPascal, ccStdCall, ccSafeCall, ccFastCall, ccForth);
  {
  @param pdtSelf the self parameter if the procedure is method.
  }
  //TMeParamDataType = (pdtUnknown, pdtInput, pdtOutput, pdtInputOutput, pdtResult, pdtSelf);

  PMeVarRec = ^TMeVarRec;
  //occupies 16 bytes
  TMeVarRec = record
    case Integer of
      0: (
        case TMeTypeKind of
          mtkInteger:    (VInteger: Longint);
          //mtkSet:        (VSet: Byte);
          mtkLString:    (VAnsiString: Pointer);
          mtkChar:       (VChar: AnsiChar);
          mtkWString:    (VWideString: Pointer);
          mtkString:     (VString: PShortString);
          mtkPointer:    (VPointer: Pointer);
          mtkObject:     (VMeObject: Pointer);
          mtkClass:      (VObject: TObject);
          mtkWChar:      (VWideChar: WideChar);
          mtkVariant:    (VVariant: TVarData);
          mtkInterface:  (VInterface: Pointer);
          mtkInt64:      (VInt64: Int64);
          mtkDynArray:   (VDynBound: Integer; VDynArray: Pointer);
          mtkMethod:     (VCode: Pointer; VData: Pointer);
          //mtkProcedure:  (VCode: Pointer);
          mtkFloat:   (
           case TFloatType of
             ftSingle: (VSingle: Single);
             ftDouble: (VDouble: Double);
             ftExtended: (VExtended: Extended);
             ftComp: (VComp: Comp);
             ftCurr: (VCurr: Currency);
          );
      );
      1: (VBytes: array [0..15] of byte);
      2: (VWords: array [0..7] of word);
      3: (VDWords: array [0..3] of LongWord);
      4: (VInt64s: array [0..1] of Int64);
      5: (VByte: byte);
      6: (VWord: word);
      7: (VLongword: Longword);
  end;


  PMeIdentityString = ^TMeIdentityString;
  TMeIdentityString = AnsiString;//AnsiString[cMaxIdentityNameLen];

  PMeRegisteredTypes = ^TMeRegisteredTypes;
  PPMeType = ^PMeType;
  PMeType = ^TMeType;
  PMeClassType = ^TMeClassType;
  PMeCustomOrdinalType = ^TMeCustomOrdinalType;
  PMeShortStringType = ^TMeShortStringType;
  PMeFloatType = ^TMeFloatType;
  PMeInt64Type = ^TMeInt64Type;
  PMeSetType = ^TMeSetType;
  PMeOrdinalType = ^TMeOrdinalType;
  PMeEnumerationType = ^TMeEnumerationType;
  PMeCustomIntegerType = ^TMeCustomIntegerType;
  PMeStructuredType = ^TMeStructuredType;
  PMeTypeField = ^TMeTypeField;
  PMeRecordType = ^TMeRecordType;
  PMeArrayType = ^TMeArrayType;
  //PMeIntegerType = ^TMeIntegerType;
  //PMeCardinalType = ^TMeCardinalType;

  //处理则返回真
  TMeTypeLoadFromStreamEvent = procedure(const aSender: PMeType; const aStream: TStream; var aType: PMeType) of object;
  TMeTypeSaveToStreamEvent = procedure(const aSender: PMeType; const aStream: TStream; const aType: PMeType) of object;

  {: the abstract type info object. }
  TMeType = object(TMeDynamicObject)
  protected //Fields
    FKind: TMeTypeKind;
    FName: TMeIdentityString;
    FOwner: PMeRegisteredTypes;
  protected
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
  public
    destructor Destroy; virtual;
    procedure Assign(aTypeInfo: PTypeInfo);//overload;
    //procedure Assign(aType: PMeType);virtual;overload;
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    //do not compare the name!!
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    Property Kind: TMeTypeKind read FKind write FKind;
    Property Name: TMeIdentityString read FName write FName;
    Property Owner: PMeRegisteredTypes read FOwner write FOwner;
  end;

  {: the Class type info object. }
  {
    Note: if u wanna load from to stream u the FSelfClass MUST be registered by RegisterClass 
  }
  TMeClassType = Object(TMeType)
  protected
    FSelfClass: TClass;
    //FParentInfo: PTypeInfo;
  protected
    procedure Init;virtual;
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
  public
    //FSelfClass := Classes.FindClass(Name)
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    Property SelfClass: TClass read FSelfClass write FSelfClass;
    //Property ParentInfo: PTypeInfo read FParentInfo write FParentInfo;
  end;

  {: the abstract float type info object: Real, Single, Double, Extended, Comp, Currency. }
  TMeFloatType = Object(TMeType)
  protected
    FFloatType: TFloatType;
  protected
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
  public
    procedure Init;virtual;
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    Property FloatType: TFloatType read FFloatType write FFloatType;
  end;

  {: the Int64 type info object. }
  TMeInt64Type = Object(TMeType)
  protected
    FMinInt64Value: Int64;
    FMaxInt64Value: Int64;
  protected
    procedure Init;virtual;
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    Property MinInt64Value: Int64 read FMinInt64Value write FMinInt64Value;
    Property MaxInt64Value: Int64 read FMaxInt64Value write FMaxInt64Value;
  end;

  {: the ShortString type info object. }
  TMeShortStringType = Object(TMeType)
  protected
    FMaxLength: Byte;
  protected
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    Property MaxLength: Byte read FMaxLength write FMaxLength;
  end;

  {: the abstract Ordinal type info object. }
  TMeCustomOrdinalType = Object(TMeType)
  protected
    FOrdType: TOrdType;
  protected
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
  public
    {
     @param otSByte: Signed byte
     @param otUByte: Unsigned byte
     @param otSWord: Signed word
     @param otUWord: Unsigned word
     @param otSLong: Signed longword
     @param otULong: Unsigned longword
    }
    Property OrdType: TOrdType read FOrdType write FOrdType;
  end;

  {: the abstract common Ordinal type info object. }
  {Ordinal: tkInteger, tkChar, tkEnumeration, tkWChar}
  TMeOrdinalType = Object(TMeCustomOrdinalType)
  protected
    FMinValue: Longint;
    FMaxValue: Longint;
  protected
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    Property MinValue: Longint read FMinValue write FMinValue;
    Property MaxValue: Longint read FMaxValue write FMaxValue;
  end;

  {: the Enumeration type info object. }
  TMeEnumerationType = Object(TMeOrdinalType)
  protected
    FBaseType: PMeType;
    FNameList: PMeStrings;
    FIsCustomValue: Boolean;
    //FNameList: PShortString;
    {NameList: array[0..Count-1] of packed ShortString;
    EnumUnitName: packed ShortString));}
  protected
    procedure Init;virtual;
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
    function GetEnumValue(const aName: AnsiString): Integer;
    function GetEnumName(Value: Integer): AnsiString;
    //function GetEnumUnitName: ShortString;
  public
    destructor Destroy; virtual;
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
    function Count: Integer; //=FMaxValue - FMinValue + 1
  public
    Property BaseType: PMeType read FBaseType;
    Property NameList: PMeStrings read FNameList;
    Property EnumName[Index: Longint]: AnsiString read GetEnumName;
    Property EnumValue[const Name: AnsiString]: Integer read GetEnumValue;
    //Property EnumUnitName: ShortString read GetEnumUnitName;
  end;

  { Summary the set types define RTTI object }
  TMeSetType = Object(TMeCustomOrdinalType)
  protected
    FCompType: PMeType;
  protected
    procedure Init;virtual;
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    Property CompType: PMeType read FCompType write FCompType;
  end;

  TMeCustomIntegerType = Object(TMeOrdinalType)
  protected
    procedure Init;virtual;
  end;

  { Summary the structured types define RTTI object }
  TMeStructuredType = object(TMeType)
  protected
    FIsPacked: Boolean;
  protected
    //to generate new VMT for it.
    //class function ParentClassAddress: TMeClass;virtual;abstract;
    //get the structured type real size(bytes)
    //function GetSize: Cardinal; virtual; abstract;
  protected
    property IsPacked: Boolean read FIsPacked;
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  end;

  {: the record element type. }
  TMeTypeField = record
    Name: TMeIdentityString;
    FieldType: PMeType;
  end;

  { Summary the record types define RTTI object }
  TMeRecordType = object(TMeStructuredType)
  protected
    //FSize: Cardinal; //the record size
    FFields: PMeList; //manage the PMeTypeField
  protected
    procedure Init;virtual;
    destructor Destroy; virtual;
    function GetItem(Index: Integer): PMeTypeField;
    function GetSize: Cardinal; //virtual;
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
    function Count: Integer;
    procedure Clear;
    function Add(const aFieldName: TMeIdentityString; const aFieldType: PMeType): Boolean;overload;
    function Add(const aFieldName: TMeIdentityString; const aFieldTypeName: TMeIdentityString): Boolean;overload;
    function Add(const aFieldName: TMeIdentityString; const aFieldTypeInfo: PTypeInfo): Boolean;overload;
    function FieldByName(const aFieldName: TMeIdentityString): PMeTypeField;
  public
    property IsPacked;
    //the type real size(bytes)
    property Size: Cardinal read GetSize;
    property Items[Index: Integer]: PMeTypeField read GetItem; default;
  end;

  { Summary the static array type info object }
  TMeArrayType = object(TMeStructuredType)
  protected
    FBaseType: PMeType;
    FLowBound: Integer; //Low bounds
    FHighBound: Integer; //high bounds
  protected
    procedure Init;virtual;
    function GetSize: Cardinal; //virtual;
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;
    function IsSameAs(const aType: PMeType): Boolean;virtual;
  public
    property IsPacked;
    //real size (bytes)
    property Size: Cardinal read GetSize;
    property BaseType: PMeType read FBaseType;
    property LowBound: Integer read FLowBound;
    property HighBound: Integer read FHighBound;
  end;

  {TMeIntegerType = Object(TMeCustomIntegerType)
  protected
    procedure Init;virtual;
  end;

  TMeCardinalType = Object(TMeCustomIntegerType)
  protected
    procedure Init;virtual;
  end;
  //}

  {: manage the PMeTypes in the list.}
  {
    TypeId: Int64 for the type in stream!
      TypeId = 0                                        means nill.
      TypeId >= 1 and TypeId <= GRegisteredTypes.Count  means the Index(+1) of the GRegisteredTypes. [not impl in it]
      TypeId > GRegisteredTypes.Count                   means the offset of the stream.
      TypeId < -1                                       means this external reference type!  [not impl in it]
      TypeId = -1                                       means error!
      
  }
  TMeRegisteredTypes = object(TMeList)
  protected
    FOwner: Pointer; //for PMeDynamicObject or TObject
    //容器，来保存类型与对应于在流中的偏移量 (PMeType(Items[i]) : FStreamOffsetList[i])
    {
    change StreamOffset from Int64 to LongInt
     因为我始终将类型保存在流的前面，而且也不会出现>4G的文件！
    }
    FStreamOffsetList: PMeList;
    //保存的是暂时不能解决的项
    //目的：解决子类型引用了后面的暂时还没有存入流中的项的情况。
    FUnResovledTypes: PMeList;
  protected
    procedure DoOnSaveType(const aSender: PMeType; const aStream: TStream; const aType: PMeType);
    procedure DoOnLoadType(const aSender: PMeType; const aStream: TStream; var aType: PMeType);
  
    {: return the PMeType of the stream "offset" when LoadFromStream. }
    { 
      @param aTypeId the type's offset in stream. the Offset is 0, means nil! 
    }
    function GetTypeByTypeId(const aTypeId: LongInt): PMeType; virtual;
    {: return the offset of the type in the stream  when SaveToStream. }
    { 
      @param aType the type to get.
    }
    function GetTypeIdByType(const aType: PMeType): LongInt; virtual;
    //New PMeType by TypeKind
    function CreateMeTypeBy(const aKind: TMeTypeKind): PMeType;virtual;
  public
    destructor Destroy; virtual;
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);
    procedure Clear;
    function Add(Value: PMeType): Integer;
    function IndexOfSameAs(aType: PMeType): Integer;
    function GetRegisteredTypeByClass(const aClass: TClass): PMeClassType;
    //i used this in Turbo Script Type System, override this method to find type in external types.
    function GetRegisteredTypeByName(const aName: TMeIdentityString): PMeType; virtual;
    function GetRegisteredTypeByTypeInfo(const aTypeInfo: PTypeInfo): PMeType;
    function RegisterType(const aType: PMeType): Boolean;
    function RegisterTypeInfo(const aTypeInfo: PTypeInfo): Boolean;
    function RegisterClassType(const aClass: TClass): PMeClassType;
    function IsRegisteredType(const aType: PMeType): Boolean;
    function IsRegisteredTypeInfo(const aTypeInfo: PTypeInfo): Boolean;
  end;

  PMeVariable = ^ TMeVariable;
  TMeVariable = Object(TMeDynamicObject)
  protected
    FDataType: PMeType;
    FValue: TMeVarRec;
  protected
  public
    property Value: TMeVarRec read FValue write FValue;
    property DataType: PMeType read FDataType write FDataType;
  end;


function GetRegisteredTypeByClass(const aClass: TClass): PMeClassType;
function GetRegisteredTypeByName(const aName: TMeIdentityString): PMeType;
function GetRegisteredTypeByTypeInfo(const aTypeInfo: PTypeInfo): PMeType;
function RegisterType(const aType: PMeType): Boolean;
function RegisterTypeInfo(const aTypeInfo: PTypeInfo): Boolean;
function RegisterClassType(const aClass: TClass): PMeClassType;
function IsRegisteredType(const aType: PMeType): Boolean;
function IsRegisteredTypeInfo(const aTypeInfo: PTypeInfo): Boolean;
function GRegisteredTypes: PMeRegisteredTypes;

function RetOnStack(Info: PMeType): Boolean;
function RetInFPU(Info: PMeType): Boolean;
function GetTypeSize(P: PMeType): Cardinal;
function GetStackTypeSize(P: PMeType;  CC: TCallingConvention): Integer;
function IsParamByRef(Flags: TParamFlags; ParamInfo: PMeType; CC: TCallingConvention): Boolean;
function IsRetInAXDX(Info: PMeType): Boolean;
function IsRequiredAlignMem(const aType: PMeType): Boolean;


procedure RegisterClass(const aClass: TClass);
function FindClass(const aClassName: AnsiString): TClass;

const
  cEmptyMeVar: TMeVarRec = (VInt64s:(0,0));
  cAllPropertySpecifiers = [mpsGet, mpsSet, mpsStored];

var
  GRegisteredMeTypesClass : TMeClass = TypeOf(TMeRegisteredTypes);

implementation

var
  FRegisteredTypes: PMeRegisteredTypes;
  FRegisteredClasses: PMeList;


procedure RegisterTypes;forward;

function GRegisteredClasses: PMeList;
begin
  if FRegisteredClasses = nil then
  begin
    New(FRegisteredClasses, Create);
  end;
  Result := FRegisteredClasses;
end;

function FindClass(const aClassName: AnsiString): TClass;
var
  i: Integer;
begin
  with GRegisteredClasses^ do
  begin
    for i := 0 to Count -1 do
    begin
      Result := TClass(Items[i]);
      if aClassName = Result.ClassName then
        exit;
    end;
  end;
  Result := nil;
end;

procedure RegisterClass(const aClass: TClass);
begin
  if not Assigned(FindClass(aClass.ClassName)) then
    GRegisteredClasses.Add(aClass);
end;

function GRegisteredTypes: PMeRegisteredTypes;
begin
  if FRegisteredTypes = nil then
  begin
    FRegisteredTypes := PMeRegisteredTypes(NewMeObject(GRegisteredMeTypesClass));
    //register the common types.
    RegisterTypes;
  end;
  Result := FRegisteredTypes;
end;

function RegisterType(const aType: PMeType): Boolean;
begin
  Result := GRegisteredTypes.RegisterType(aType);
end;

function IsRegisteredType(const aType: PMeType): Boolean;
begin
  Result := GRegisteredTypes.IsRegisteredType(aType);
end;

function RegisterClassType(const aClass: TClass): PMeClassType;
begin
  Result := GRegisteredTypes.RegisterClassType(aClass);
end;

function GetRegisteredTypeByClass(const aClass: TClass): PMeClassType;
begin
  Result := GRegisteredTypes.GetRegisteredTypeByClass(aClass);
end;

function GetRegisteredTypeByTypeInfo(const aTypeInfo: PTypeInfo): PMeType;
begin
  Result := GRegisteredTypes.GetRegisteredTypeByTypeInfo(aTypeInfo);
end;

function IsRegisteredTypeInfo(const aTypeInfo: PTypeInfo): Boolean;
begin
  Result := GRegisteredTypes.IsRegisteredTypeInfo(aTypeInfo);
end;

function RegisterTypeInfo(const aTypeInfo: PTypeInfo): Boolean;
begin
  Result := GRegisteredTypes.RegisterTypeInfo(aTypeInfo);
end;

function GetRegisteredTypeByName(const aName: TMeIdentityString): PMeType;
var
  i: integer;
begin
  Result := GRegisteredTypes.GetRegisteredTypeByName(aName);
end;

function IsParamByRef(Flags: TParamFlags; ParamInfo: PMeType; CC: TCallingConvention): Boolean;
begin
  Result := (pfVar in Flags) or (pfOut in Flags);

  if (not Result) then
  begin
    case ParamInfo.Kind of
      mtkVariant:
        Result := (pfConst in Flags) or (CC = ccPascal);
      mtkString:
        Result := True;
    end;//case
  end;
end;


function GetTypeSize(P: PMeType): Cardinal;
begin
  Result := 4;
  case P.Kind of
    mtkInteger:
      case PMeCustomIntegerType(P).OrdType of
        otSByte,
        otUByte:
            Result := sizeof(Byte);
        otSWord,
        otUWord:
          begin
            Result := sizeof(Word);
          end;
        {otSLong,
        otULong:
           Result := SizeOf(Integer);
          ;//}
      end;
    mtkFloat:
      case PMeFloatType(P).FloatType of
        ftSingle:
          Result := Sizeof(Single);
        ftDouble:
          Result := Sizeof(Double);
        ftComp:
          Result := Sizeof(Comp);
        ftCurr:
          Result := Sizeof(Currency);
        ftExtended:
          Result := Sizeof(Extended);
      end;
    mtkChar:
      Result := 1;
    mtkWChar:
      Result := 2;
    mtkInt64:
      Result := sizeof(Int64);
    mtkVariant:
      Result := sizeof(TVarData);
    mtkEnumeration:
    begin
      Result := PMeEnumerationType(P).Count;
      {$IFDEF ByteEnum}
      if Result <= High(Byte)+1 then
        Result := 1
      else 
      {$ENDIF}
      {$IFDEF WordEnum}
      if Result <= High(Word)+1 then
        Result := 2
      else //if Result <= High(LongWord)+1 then
      {$ENDIF}
        //Result := 4; //the default value
    end;
    mtkSet:
    begin
      Result := 1;
      P := PMeSetType(P).FCompType;
      if Assigned(P) then
        case P.Kind of
          mtkEnumeration: 
          begin
            Result := PMeEnumerationType(P).Count;
            {$IFDEF BORLAND}
            if Result <=8 then
              Result := 1
            else if Result <=16 then
              Result := 2
            else
            {$ENDIF}
            if Result <= 32 then
              Result := 4
            {$IFDEF FPC}
            else
              Result := 32
            {$ENDIF}
          end;
        end;
    end;
    mtkString: //ShortString
      Result := 1 + PMeShortStringType(P).MaxLength;
    mtkRecord:
      Result := PMeRecordType(P).Size;
    mtkArray:
      Result := PMeArrayType(P).Size;
  end;
end;

function IsRetInAXDX(Info: PMeType): Boolean;
begin
  Result := False;
   if Info <> nil then
    case Info.Kind of
      mtkInt64:
        Result := True;
    end;
end;

function RetOnStack(Info: PMeType): Boolean;
begin
  Result := False;
  if Info <> nil then
    case Info.Kind of
      mtkLString,
      mtkString,
      mtkWString:
          Result := True;
      mtkVariant:
          Result := True;
      mtkDynArray:
         Result := True;
    end;
end;

function RetInFPU(Info: PMeType): Boolean;
begin
  Result := False;
  if Info <> nil then
    case Info.Kind of
      mtkFloat: Result := True;
    end;
end;

{
  GetStackTypeSize

  Returns the size that is actually allocated on the stack for a given
  type.  This is frequently different than the heap allocation for
  an object, because all stack pointers are allocated on 4 byte boundaries.
  So for example, the Extended type might occupy 10 bytes, but we will
  always allocate 12 bytes on the stack for it.
}
function GetStackTypeSize(P: PMeType; CC: TCallingConvention): Integer;
begin
  Result := 4;
  case P.Kind of
    mtkFloat:
      case PMeFloatType(P).FloatType of
        //ftSingle:          ;
        ftDouble,
        ftComp,
        ftCurr:
          Result := 8;
        ftExtended:
          Result := 10;
      end;
    mtkInt64, mtkDynArray, mtkMethod:
      Result := 8;
    mtkVariant:
      if CC in [ccCdecl, ccStdCall, ccSafeCall] then
        Result := sizeof(TVarData);
  end;

  // Make sure we're aligned on a 4 byte boundary
  Result := (Result + 3) and $FFFFFFFC;
end;

Const
  //Note: the mtkInteger must be check
  cReqAlignMemTypeKinds = [mtkInteger, mtkFloat, mtkClass, mtkPointer, mtkString, mtkLString, mtkInt64, mtkQWord];

function IsRequiredAlignMem(const aType: PMeType): Boolean;
begin
  Result := Assigned(aType);
  if Result then
  begin
    Result := aType.Kind in cReqAlignMemTypeKinds;
    if Result and (aType.Kind = mtkInteger) then
    begin
      Result := not (PMeCustomOrdinalType(aType).OrdType in [otSByte, otUByte]);
    end;
  end;
end;

{ #### TMeRegisteredTypes #### }

destructor TMeRegisteredTypes.Destroy;
begin
  if FRegisteredTypes = @Self then FRegisteredTypes := nil;
  //SetLength(FStreamOffsetList, 0);
  FreeMeObjects;
  inherited;
end;

procedure TMeRegisteredTypes.Clear;
begin
  FreeMeObjects;
  inherited;
end;

function TMeRegisteredTypes.CreateMeTypeBy(const aKind: TMeTypeKind): PMeType;
begin
    case aKind of
      mtkLString, mtkWString, mtkVariant:
      begin
        Result := New(PMeType, Create);
        Result.FKind := aKind;
      end;
      mtkInteger, mtkChar, mtkWChar: 
      begin
        Result := New(PMeCustomIntegerType, Create);
        Result.FKind := aKind;
      end;
      mtkSet: 
      begin
        Result := New(PMeSetType, Create);
      end;
      mtkEnumeration: Result := New(PMeEnumerationType, Create);
      mtkFloat: Result := New(PMeFloatType, Create);
      mtkString: Result := New(PMeShortStringType, Create);
      mtkInt64: Result := New(PMeInt64Type, Create);
      mtkClass: Result := New(PMeClassType, Create);
    else
      Result := nil;
    end;// case
end;

type
  PUnResovledTypeRec = ^ TUnResovledTypeRec;
  TUnResovledTypeRec = packed record
    Pos: LongInt; //the position in the stream.
    case Boolean of
      True: (TypeIndex: Integer);
      False: (pType: PPMeType);
  end;
procedure TMeRegisteredTypes.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
var
  i: Integer;
  v: LongInt;
begin
  New(FStreamOffsetList, Create);
  New(FUnResovledTypes, Create);
  try
    i := Count;
    FStreamOffsetList.Count := i;
    FillListIn(FStreamOffsetList^, 0, i, Integer(nil));
    //for i := 0 to Count - 1 do FStreamOffsetList.Items[i] := nil;
    aStream.WriteBuffer(i, SizeOf(i));
    for i := 0 to Count - 1 do
    begin
      FStreamOffsetList.Items[i] := Pointer(aStream.Position);
      //if PMeType(Items[i]).Kind = mtkUnknown then
        //raise EMeTypeError.Create(PMeType(Items[i]).Name+' mtkUnknown Error');
      PMeType(Items[i]).SaveToStream(aStream, DoOnSaveType);
    end;
    if FUnResovledTypes.Count > 0 then
    begin
      for i := 0 to FUnResovledTypes.Count - 1 do
      with PUnResovledTypeRec(FUnResovledTypes.Items[i])^ do
      begin
        if LongInt(FStreamOffsetList.Items[TypeIndex]) <> 0 then
        begin
          aStream.Position := Pos;
          v := LongInt(FStreamOffsetList.Items[TypeIndex]);
          //writeln(PMeType(Self.Items[TypeIndex]).Name,' saveto:', v);
          aStream.WriteBuffer(v, SizeOf(v));
        end
        else
          raise EMeTypeError.Create('Types SaveToStream Error: the TypeIndex:' + IntToStr(TypeIndex) + ' can not be resovled!');
      end;
    end;
  finally
    FUnResovledTypes.FreePointers;
    MeFreeAndNil(FUnResovledTypes);
    MeFreeAndNil(FStreamOffsetList);
    //SetLength(FStreamOffsetList, 0);
  end;
end;

procedure TMeRegisteredTypes.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
var
  i: Integer;
  vKind: TMeTypeKind;
  vType: PMeType;
begin
  New(FStreamOffsetList, Create);
  New(FUnResovledTypes, Create);
  try
    aStream.ReadBuffer(i, SizeOf(i));
    FStreamOffsetList.Count := i;
    FillListIn(FStreamOffsetList^, 0, i, Integer(nil));
    for i := 0 to FStreamOffsetList.Count - 1 do
    begin
      FStreamOffsetList.Items[i]:= Pointer(aStream.Position);
      aStream.ReadBuffer(vKind, SizeOf(vKind));
      vType := CreateMeTypeBy(vKind);
      if Assigned(vType) then
      begin
        vType.LoadFromStream(aStream, DoOnLoadType);
        vType.FKind := vKind;
        Add(vType);
      end
      else
        Raise EMeTypeError.Create('Types Load from stream error: can not create type from such type kind('+IntToStr(i)+'):'+ GetEnumName(TypeInfo(TMeTypeKind), Integer(vKind)));
    end;
    if FUnResovledTypes.Count > 0 then
    begin
      for i := 0 to FUnResovledTypes.Count - 1 do
      with PUnResovledTypeRec(FUnResovledTypes.Items[i])^ do
      begin
        vType := GetTypeByTypeId(Pos);
        if Assigned(vType) then
        begin
          pType^ := vType;
          //writeln('UNResolvedType:', vType.Name);
        end
        else
          raise EMeTypeError.Create('Types LoadFromStream Error: the TypeIndex:' + IntToStr(TypeIndex) + ' can not be resovled!');
      end;
    end;
  finally
    FUnResovledTypes.FreePointers;
    MeFreeAndNil(FUnResovledTypes);
    MeFreeAndNil(FStreamOffsetList);
  end;
end;

function TMeRegisteredTypes.GetTypeByTypeId(const aTypeId: LongInt): PMeType;
var
  i: Integer;
begin
  if aTypeId <> 0 then
  begin
    for i := 0 to Count -1 do
    begin
      if LongInt(FStreamOffsetList.Items[i]) = aTypeId then
      begin
        Result := Items[i];
        exit;
      end;
    end;
    Integer(Result) := -1;
    //Raise EMeTypeError.Create('Load From Stream Error: not found such MeType!');
  end
  else
    Result := nil;
end;

function TMeRegisteredTypes.GetTypeIdByType(const aType: PMeType): LongInt;
var
  i:Integer;
begin
  if not Assigned(aType) then
  begin
    Result := 0;
    Exit;
  end;

  i := IndexOf(aType);
  if i >=0 then // it's the internal List types!
  begin
    Result := LongInt(FStreamOffsetList.Items[i]);
    if Result = 0 then Result := -1;
  end
  else
  begin
    //Result := -1;
    Raise EMeTypeError.Create('save to stream error: not found such MeType in list!');
  end;
end;

procedure TMeRegisteredTypes.DoOnSaveType(const aSender: PMeType; const aStream: TStream; const aType: PMeType);
var
  i: LongInt;
  v: PUnResovledTypeRec;
begin
  i := GetTypeIdByType(aType);
  //if Assigned(aType) then writeln(aSender.Name, ' DoOnSave:', aType.Name, ' i=', i);
  if i = -1 then
  begin
    New(v);
    v.Pos := aStream.Position;
    v.TypeIndex := IndexOf(aType);
    if v.TypeIndex < 0 then
    begin
      Dispose(v);
      Raise EMeTypeError.Create('TMeRegisteredTypes.DoOnSaveType: can not find the sender:'+ aSender.Name);
    end
    else
      FUnResovledTypes.Add(v);
  end;
  aStream.WriteBuffer(i, SizeOf(i))
end;

procedure TMeRegisteredTypes.DoOnLoadType(const aSender: PMeType; const aStream: TStream; var aType: PMeType);
var
  i: LongInt;
  v: PUnResovledTypeRec;
begin
  aStream.ReadBuffer(i, SizeOf(i));
  aType := GetTypeByTypeId(i);
  if Integer(aType) = -1 then
  begin
    aType := nil;
    if i > 0 then
    begin
        New(v);
        v.Pos := i;
        v.pType := @aType;
        FUnResovledTypes.Add(v);
    end
    else
      Raise EMeTypeError.Create('Load From Stream Error(DoOnLoadType): not found such MeType!');
  end;
end;

function TMeRegisteredTypes.Add(Value: PMeType): Integer;
begin
  Result := inherited Add(Value);
  if Result >= 0 then
  begin
    Value.FOwner := @Self;
  end;
end;

function TMeRegisteredTypes.IndexOfSameAs(aType: PMeType): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    with PMeType(Items[Result])^ do
      if IsSameAs(aType) then  exit;
  end;
  Result := -1;
end;

function TMeRegisteredTypes.RegisterType(const aType: PMeType): Boolean;
begin
  Result := not IsRegisteredType(aType);
  if Result then //not registered
    Result := Add(aType) >= 0;
end;

function TMeRegisteredTypes.IsRegisteredType(const aType: PMeType): Boolean;
begin
  Result := IndexOf(aType) >= 0;
end;

function TMeRegisteredTypes.RegisterClassType(const aClass: TClass): PMeClassType;
begin
  if Assigned(aClass) then
  begin
    Result := GetRegisteredTypeByClass(aClass);
    if Result = nil then
    begin
      New(Result, Create);
      Result.FSelfClass := aClass;
      Result.FName := aClass.ClassName;
      if Add(Result) < 0 then
        MeFreeAndNil(Result);;
    end;
  end
  else 
    Result := nil;
end;

function TMeRegisteredTypes.GetRegisteredTypeByClass(const aClass: TClass): PMeClassType;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := List^[i];
    if (Result.ClassType = TypeOf(TMeClassType)) and (Result.SelfClass = aClass) then
      exit;
  end;
  Result := nil;
end;

function TMeRegisteredTypes.GetRegisteredTypeByTypeInfo(const aTypeInfo: PTypeInfo): PMeType;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := List^[i];
    if (Ord(Result.Kind) = Ord(aTypeInfo.Kind)) and (Result.Name = aTypeInfo.Name) then
      exit;
  end;
  Result := nil;
end;

function TMeRegisteredTypes.IsRegisteredTypeInfo(const aTypeInfo: PTypeInfo): Boolean;
begin
  Result := Assigned(GetRegisteredTypeByTypeInfo(aTypeInfo));
end;

function TMeRegisteredTypes.RegisterTypeInfo(const aTypeInfo: PTypeInfo): Boolean;
var
  vType: PMeType;
  //vTypeData: PTypeData;
begin
  Result := Assigned(aTypeInfo) and not IsRegisteredTypeInfo(aTypeInfo);
  if Result then //not registered
  begin
    {$IFDEF BORLAND}
    vType := CreateMeTypeBy(TMeTypeKind(aTypeInfo.Kind));
    {$ELSE FPC}
      TODO: Check the TMeTypeKind = TTypeKind
    {$ENDIF}
    Result := Assigned(vType);
    if Result then 
    begin
      vType.FOwner := @Self;
      vType.Assign(aTypeInfo);
      Result := Add(vType) >= 0;
    end;
  end;
end;

function TMeRegisteredTypes.GetRegisteredTypeByName(const aName: TMeIdentityString): PMeType;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := PMeType(List^[i]);
    if AnsiSameText(Result.Name, aName) then
    begin
      Exit;
    end;
  end;
  Result := nil;
end;

{ #### TMeType #### }
destructor TMeType.Destroy;
begin
  FName := '';
  inherited;
end;

procedure TMeType.Assign(aTypeInfo: PTypeInfo);
begin
  Byte(FKind) := Byte(aTypeInfo.Kind);
  FName := aTypeInfo.Name;
  //writeln('metypw.ass=', fname);
  AssignFromTypeData(GetTypeData(aTypeInfo));
end;

procedure TMeType.AssignFromTypeData(aTypeData: PTypeData);
begin
end;

procedure TMeType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);
var
  i: integer;
begin
  with aStream do
  begin
    WriteBuffer(FKind, SizeOf(FKind));
    i := Length(FName);
    WriteBuffer(i, SizeOf(i));
    if i > 0 then
      WriteBuffer(FName[1], Length(FName));
  end;
end;

procedure TMeType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);
var
  i: integer;
begin
  with aStream do
  begin
    //ReadBuffer(FKind, SizeOf(FKind)); //see TMeRegisteredTypes.LoadFromStream
    ReadBuffer(i, SizeOf(i));
    SetLength(FName, i);
    if i > 0 then
      ReadBuffer(FName[1], Length(FName));
  end;
end;

function TMeType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := FKind = aType.FKind;
end;

{ #### TMeInt64Type #### }
procedure TMeInt64Type.Init;
begin
  inherited;
  FKind := mtkInt64;
end;

procedure TMeInt64Type.AssignFromTypeData(aTypeData: PTypeData);
begin
  FMinInt64Value := aTypeData.MinInt64Value;
  FMaxInt64Value := aTypeData.MaxInt64Value;
end;

procedure TMeInt64Type.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
begin
  inherited;
  with aStream do
  begin
    WriteBuffer(FMinInt64Value, SizeOf(FMinInt64Value));
    WriteBuffer(FMaxInt64Value, SizeOf(FMaxInt64Value));
  end;
end;

procedure TMeInt64Type.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
begin
  inherited;
  with aStream do
  begin
    ReadBuffer(FMinInt64Value, SizeOf(FMinInt64Value));
    ReadBuffer(FMaxInt64Value, SizeOf(FMaxInt64Value));
  end;
end;

function TMeInt64Type.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FMinInt64Value = PMeInt64Type(aType).FMinInt64Value) and (FMaxInt64Value = PMeInt64Type(aType).FMaxInt64Value);
end;

{ #### TMeFloatType #### }
procedure TMeFloatType.Init;
begin
  inherited;
  FKind := mtkFloat;
end;

procedure TMeFloatType.AssignFromTypeData(aTypeData: PTypeData);
begin
  FFloatType := aTypeData.FloatType;
end;

procedure TMeFloatType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);
begin
  inherited SaveToStream(aStream, aOnSaveType);
  with aStream do
  begin
    WriteBuffer(FFloatType, SizeOf(FFloatType));
  end;
end;

procedure TMeFloatType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  with aStream do
  begin
    ReadBuffer(FFloatType, SizeOf(FFloatType));
  end;
end;

function TMeFloatType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FFloatType = PMeFloatType(aType).FFloatType);
end;

{ #### TMeShortStringType #### }
procedure TMeShortStringType.AssignFromTypeData(aTypeData: PTypeData);
begin
  FMaxLength := aTypeData.MaxLength;
end;

procedure TMeShortStringType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);
begin
  inherited SaveToStream(aStream, aOnSaveType);
  with aStream do
  begin
    WriteBuffer(FMaxLength, SizeOf(FMaxLength));
  end;
end;

procedure TMeShortStringType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  with aStream do
  begin
    ReadBuffer(FMaxLength, SizeOf(FMaxLength));
  end;
end;

function TMeShortStringType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FMaxLength = PMeShortStringType(aType).FMaxLength);
end;

{ #### TMeCustomOrdinalType #### }
procedure TMeCustomOrdinalType.AssignFromTypeData(aTypeData: PTypeData);
begin
  //inherited;
  FOrdType := aTypeData.OrdType;
end;

procedure TMeCustomOrdinalType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);
begin
  inherited SaveToStream(aStream);
  with aStream do
  begin
    WriteBuffer(FOrdType, SizeOf(FOrdType));
  end;
end;

procedure TMeCustomOrdinalType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);
begin
  inherited LoadFromStream(aStream);
  with aStream do
  begin
    ReadBuffer(FOrdType, SizeOf(FOrdType));
  end;
end;

function TMeCustomOrdinalType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FOrdType = PMeCustomOrdinalType(aType).FOrdType);
end;

{ #### TMeSetType #### }
procedure TMeSetType.Init;
begin
  inherited;
  FKind := mtkSet;
end;

procedure TMeSetType.AssignFromTypeData(aTypeData: PTypeData);
begin
  inherited;
  FCompType := nil;
  if Assigned(FOwner) then
    FCompType := FOwner.GetRegisteredTypeByTypeInfo(aTypeData.CompType^);
  if not Assigned(FCompType) then
    FCompType := GetRegisteredTypeByTypeInfo(aTypeData.CompType^);
  if FCompType = nil then
  begin
    raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, [aTypeData.CompType^.Name]);
  end;
end;

procedure TMeSetType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
begin
  inherited SaveToStream(aStream, aOnSaveType);
  if Assigned(aOnSaveType) then
    aOnSaveType(@Self, aStream, FCompType);
end;

procedure TMeSetType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  if Assigned(aOnLoadType) then
  begin
    aOnLoadType(@Self, aStream, FCompType);
  end;
end;

function TMeSetType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
  begin
    if Assigned(FCompType) then
    begin
      //writeln(Integer(PMeSetType(aType).FCompType), ' ', Integer(FCompType));
      Result := FCompType.IsSameAs(PMeSetType(aType).FCompType);
    end
    else
      Result := (FCompType = PMeSetType(aType).FCompType);
  end;
end;

{ #### TMeOrdinalType #### }
procedure TMeOrdinalType.AssignFromTypeData(aTypeData: PTypeData);
begin
  inherited;
  FMinValue := aTypeData.MinValue;
  FMaxValue := aTypeData.MaxValue;
end;

procedure TMeOrdinalType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
begin
  inherited SaveToStream(aStream, aOnSaveType);
  with aStream do
  begin
    WriteBuffer(FMinValue, SizeOf(FMinValue));
    WriteBuffer(FMaxValue, SizeOf(FMaxValue));
  end;
end;

procedure TMeOrdinalType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  with aStream do
  begin
    ReadBuffer(FMinValue, SizeOf(FMinValue));
    ReadBuffer(FMaxValue, SizeOf(FMaxValue));
  end;
end;

function TMeOrdinalType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FMinValue = PMeOrdinalType(aType).FMinValue) and (FMaxValue = PMeOrdinalType(aType).FMaxValue);
end;

{ #### TMeEnumerationType #### }
procedure TMeEnumerationType.Init;
begin
  inherited;
  FKind := mtkEnumeration;
  New(FNameList, Create);
end;

destructor TMeEnumerationType.Destroy;
begin
  MeFreeAndNil(FNameList);
  inherited;
end;

procedure TMeEnumerationType.AssignFromTypeData(aTypeData: PTypeData);
var
  I: Integer;
  P: PShortString;
begin
  inherited;
  if aTypeData.BaseType^.Name = Name then
    FBaseType := nil
  else
  begin
    if Assigned(FOwner) then
      FBaseType := FOwner.GetRegisteredTypeByTypeInfo(aTypeData.BaseType^)
    else
      FBaseType := GetRegisteredTypeByTypeInfo(aTypeData.BaseType^);
    if FBaseType = nil then
      raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, [aTypeData.BaseType^.Name]);
  end;
  FIsCustomValue := False;
  P := @aTypeData.NameList;
  //writeln('Name:', Name, ' MinValue:',MinValue, ' MaxValue :', MaxValue );
  For I := MinValue to MaxValue do
  begin
    if Length(P^) > 0 then
    begin
      FNameList.AddObject(P^, I);
      Inc(Integer(P), Length(P^) + 1);
    end
    else
      Break;
  end;//}
end;

procedure TMeEnumerationType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
var
  i: Integer;
  Len: Integer;
begin
  inherited SaveToStream(aStream, aOnSaveType);
  if Assigned(aOnSaveType) then
    aOnSaveType(@Self, aStream, FBaseType);
  With aStream do
  begin
    WriteBuffer(FIsCustomValue, 1);
    i := FNameList.Count;
    WriteBuffer(i, SizeOf(i));
    if i > 0 then
    begin
      for i := 0 to FNameList.Count - 1 do
      begin
        if FIsCustomValue then
        begin
          LongWord(Len) := FNameList.Objects[i];
          WriteBuffer(Len, SizeOf(Len));
        end;

        Len := FNameList.ItemLen[i];
        WriteBuffer(Len, SizeOf(Len));

        if Len > 0 then
          WriteBuffer(FNameList.ItemPtrs[i]^, Len)
        else
          raise EMeTypeError.Create('TMeEnumerationType.SaveToStream: no enum name error!!');
      end;
    end;
  end;
end;

procedure TMeEnumerationType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
var
  i, j: Integer;
  vCount, Len: Integer;
  vValue: LongWord;
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  if Assigned(aOnLoadType) then
    aOnLoadType(@Self, aStream, FBaseType);
  With aStream do
  begin
    ReadBuffer(FIsCustomValue, 1);
    ReadBuffer(vCount, SizeOf(vCount));
    FNameList.Clear;
    if vCount > 0 then
    begin
      vValue := 0;
      for i := 0 to vCount - 1 do
      begin
        if FIsCustomValue then
        begin
          WriteBuffer(vValue, SizeOf(vValue));
        end;

        ReadBuffer(Len, SizeOf(Len));
        if Len > 0 then
        begin
          j := FNameList.AddObjectLen(nil, Len, vValue);
          ReadBuffer(FNameList.ItemPtrs[j]^, Len);
        end
        else
          raise EMeTypeError.Create('TMeEnumerationType.LoadFromStream: the name length is 0. no enum name error!!');
      end;
    end;
  end;
end;

function TMeEnumerationType.IsSameAs(const aType: PMeType): Boolean;
var
  i: Integer;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FIsCustomValue = PMeEnumerationType(aType).FIsCustomValue) and (Count = PMeEnumerationType(aType).Count);
  if Result then
  begin
    for i := 0 to FNameList.Count -1  do
    begin
      Result := (FNameList.ItemPtrs[i]^ = PMeEnumerationType(aType).FNameList.ItemPtrs[i]^);
      if FIsCustomValue and Result then
        Result := (FNameList.Objects[i] = PMeEnumerationType(aType).FNameList.Objects[i]);
      if not Result then exit;
    end;
  end;
end;

function TMeEnumerationType.Count: Integer;
begin
  Result := FNameList.Count; //FMaxValue - FMinValue + 1;
end;

function TMeEnumerationType.GetEnumValue(const aName: AnsiString): Integer;
begin
  Result := FNameList.IndexOf(aName);
  if (Result <> -1) then Result := FNameList.Objects[Result];
end;

function TMeEnumerationType.GetEnumName(Value: Integer): AnsiString;
begin
  if not FIsCustomValue then
    Value := Value - FMinValue
  else
    Value := FNameList.IndexOfObject(Value);
  Result := FNameList.Items[Value];
end;

{
function TMeEnumerationType.GetEnumUnitName: ShortString;
var
  P: PShortString;
  i: Integer;
begin
  P := FNameList;
  for i := MinValue to MaxValue do Inc(Integer(P), Length(P^) + 1);
  Result := P^;
end;
}

{ #### TMeCustomIntegerType #### }
procedure TMeCustomIntegerType.Init;
begin
  inherited;
  FKind := mtkInteger;
end;

(*
{ #### TMeIntegerType #### }
procedure TMeIntegerType.Init;
begin
  inherited;
  FName := 'Integer';
  FOrdType := otSLong;
  FMinValue := Low(Integer);
  FMaxValue := High(Integer);
end;

{ #### TMeCardinalType #### }
procedure TMeCardinalType.Init;
begin
  inherited;
  FName := 'Cardinal';
  FOrdType := otULong;
  FMinValue := 0;
  Cardinal(FMaxValue) := High(Cardinal);
end;
*)

{ #### TMeStructuredType #### }
procedure TMeStructuredType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
begin
  inherited SaveToStream(aStream, aOnSaveType);
  with aStream do
  begin
    WriteBuffer(FIsPacked, SizeOf(FIsPacked));
  end;
end;

procedure TMeStructuredType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  with aStream do
  begin
    ReadBuffer(FIsPacked, SizeOf(FIsPacked));
  end;
end;

function TMeStructuredType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FIsPacked = PMeStructuredType(aType).FIsPacked);
end;

{ #### TMeRecordType #### }
procedure TMeRecordType.Init;
begin
  inherited;
  FKind := mtkRecord;
  New(FFields, Create);
end;

destructor TMeRecordType.Destroy; 
begin
  Clear;
  MeFreeAndNil(FFields);
  inherited;
end;

procedure TMeRecordType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
var
  i: Integer;
  Len: Integer;
  vItem: PMeTypeField;
begin
  inherited SaveToStream(aStream, aOnSaveType);
  Len := FFields.Count;
  aStream.WriteBuffer(len, SizeOf(Len));
  for i := 0 to FFields.Count -1 do
  begin
    vItem := PMeTypeField(FFields.Items[i]);
    if Assigned(vItem) then
    begin
      Len := Length(vItem.Name);
      aStream.WriteBuffer(len, SizeOf(Len));
      if Len > 0 then
        aStream.WriteBuffer(vItem.Name[1], Len);
      if Assigned(aOnSaveType) then
        aOnSaveType(@Self, aStream, vItem.FieldType);
    end;
  end;
end;

procedure TMeRecordType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
var
  i: Integer;
  Len, L: Integer;
  vFieldName: AnsiString;
  vFieldType: PMeType;
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  aStream.ReadBuffer(len, SizeOf(Len));
  for i := 0 to Len -1 do
  begin
    aStream.ReadBuffer(L, SizeOf(L));
    SetLength(vFieldName, L);
    if L > 0 then
      aStream.WriteBuffer(vFieldName[1], L);
    if Assigned(aOnLoadType) then
      aOnLoadType(@Self, aStream, vFieldType);
    Add(vFieldName, vFieldType);
  end;
end;

function TMeRecordType.IsSameAs(const aType: PMeType): Boolean;
var 
  i: Integer;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FFields.Count = PMeRecordType(aType).FFields.Count);
  if Result then
    for i := 0 to FFields.Count - 1 do
    begin
      Result := PMeTypeField(FFields.Items[i]).Name = PMeTypeField(PMeRecordType(aType).FFields.Items[i]).Name;
      if Result and (PMeTypeField(FFields.Items[i]).FieldType <> PMeTypeField(PMeRecordType(aType).FFields.Items[i]).FieldType) then
        Result := PMeTypeField(FFields.Items[i]).FieldType.IsSameAs(PMeTypeField(PMeRecordType(aType).FFields.Items[i]).FieldType);
      if not Result then exit;
    end;
end;

procedure TMeRecordType.Clear;
var
  i: Integer;
begin
  for i := 0 to FFields.Count -1 do
  begin
    if Assigned(FFields.Items[i]) then
      PMeTypeField(FFields.Items[i]).Name := '';
  end;
  FFields.Clear;
end;

function TMeRecordType.Add(const aFieldName: TMeIdentityString; const aFieldType: PMeType): Boolean;
var
  vField: PMeTypeField;
begin
  if FieldByName(aFieldName) = nil then
  begin
    New(vField);
    vField.Name := aFieldName;
    vField.FieldType := aFieldType;
    Result := FFields.Add(vField) >= 0;
    if not result then Dispose(vField);
  end
  else 
    Raise EMeTypeError.CreateResFmt(@rsDuplicateIdentityNameError, [aFieldName]);
end;

function TMeRecordType.Add(const aFieldName: TMeIdentityString; const aFieldTypeName: TMeIdentityString): Boolean;
var
  vFieldType: PMeType;
begin
  vFieldType := nil;
  if Assigned(FOwner) then
    vFieldType := FOwner.GetRegisteredTypeByName(aFieldTypeName);
  if vFieldType = nil then vFieldType := GetRegisteredTypeByName(aFieldTypeName);
  Result := Assigned(vFieldType);
  if Result then
    Result := Add(aFieldName, vFieldType);
end;

function TMeRecordType.Add(const aFieldName: TMeIdentityString; const aFieldTypeInfo: PTypeInfo): Boolean;
var
  vFieldType: PMeType;
begin
  vFieldType := GetRegisteredTypeByTypeInfo(aFieldTypeInfo);
  Result := vFieldType <> nil;
  if Result then
    Result := Add(aFieldName, vFieldType);
end;

function TMeRecordType.FieldByName(const aFieldName: TMeIdentityString): PMeTypeField;
var
  i: Integer;
begin
  with FFields^ do
  for i := 0 to Count -1 do
  begin
    Result := Items[i];
    if Result.Name = aFieldName then exit;
  end;
  Result := nil;
end;

function TMeRecordType.Count: Integer;
begin
  Result := FFields.Count;
end;

function TMeRecordType.GetSize: Cardinal;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count -1 do
  begin
    Result := Result + GetTypeSize(Items[i].FieldType);
  end;
end;

function TMeRecordType.GetItem(Index: Integer): PMeTypeField;
begin
  Result := FFields.Items[Index];
end;

{ #### TMeClassType #### }

procedure TMeClassType.AssignFromTypeData(aTypeData: PTypeData);
begin
  FSelfClass := aTypeData.ClassType;
  //FParentInfo := aTypeData.ParentInfo^;
end;

procedure TMeClassType.Init;
begin
  inherited;
  FKind := mtkClass;
end;

procedure TMeClassType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
begin
  inherited;
  FSelfClass := FindClass(FName);
  if not Assigned(FSelfClass) then
    Raise EMeTypeError.Create('Type Load From Stream error: the SelfClass not found!');
end;

function TMeClassType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FSelfClass = PMeClassType(aType).FSelfClass);
end;

{ #### TMeArrayType #### }
procedure TMeArrayType.Init;
begin
  inherited;
  FKind := mtkArray;
end;

procedure TMeArrayType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
begin
  inherited SaveToStream(aStream, aOnSaveType);
  aStream.WriteBuffer(FLowBound, SizeOf(FLowBound));
  aStream.WriteBuffer(FHighBound, SizeOf(FHighBound));
  if Assigned(aOnSaveType) then
    aOnSaveType(@Self, aStream, FBaseType);
end;

procedure TMeArrayType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  aStream.ReadBuffer(FLowBound, SizeOf(FLowBound));
  aStream.ReadBuffer(FHighBound, SizeOf(FHighBound));
  if Assigned(aOnLoadType) then
    aOnLoadType(@Self, aStream, FBaseType);
end;

function TMeArrayType.IsSameAs(const aType: PMeType): Boolean;
begin
  Result := inherited IsSameAs(aType);
  if Result then
    Result := (FLowBound = PMeArrayType(aType).FLowBound) and (FHighBound = PMeArrayType(aType).FHighBound);
  if Result and (FBaseType <> PMeArrayType(aType).FBaseType) then
    Result := FBaseType.IsSameAs(PMeArrayType(aType).FBaseType);
end;

function TMeArrayType.GetSize: Cardinal;
begin
  Result := GetTypeSize(FBaseType) * (Cardinal(FHighBound - FLowBound) + 1);
end;

{$IFDEF Delphi_BoolTypeInfo_BUG}
procedure FixBoolTypInfo(aTypeData: PTypeData);
var
  PatchAddress: PPointer;
const
  aV : array[0..1] of Longint = (0,1);
begin
  PatchAddress := @aTypeData.MinValue;
  WriteMem(PatchAddress, @aV, SizeOf(aV))
end;
{$ENDIF}

procedure RegisterTypes;
begin
  //Register simple types first.
  with FRegisteredClasses^ do
  begin
    RegisterTypeInfo(Typeinfo(Integer));
    RegisterTypeInfo(Typeinfo(Shortint));
    RegisterTypeInfo(Typeinfo(Smallint));
    RegisterTypeInfo(Typeinfo(Cardinal));
    RegisterTypeInfo(Typeinfo(Byte));
    RegisterTypeInfo(Typeinfo(Word));
    RegisterTypeInfo(Typeinfo(Int64));
    RegisterTypeInfo(Typeinfo(AnsiString));
    RegisterTypeInfo(Typeinfo(ShortString));
    RegisterTypeInfo(Typeinfo(WideString));
    RegisterTypeInfo(Typeinfo(WideChar));
    RegisterTypeInfo(Typeinfo(AnsiChar));
  
    RegisterTypeInfo(Typeinfo(Real));
    RegisterTypeInfo(Typeinfo(Single));
    RegisterTypeInfo(Typeinfo(Double));
    RegisterTypeInfo(Typeinfo(Extended));
    RegisterTypeInfo(Typeinfo(Comp));
    RegisterTypeInfo(Typeinfo(Currency));
    RegisterTypeInfo(Typeinfo(TDateTime));
  
    RegisterTypeInfo(Typeinfo(Boolean));
    {$IFDEF Delphi_BoolTypeInfo_BUG}
      //the ByteBool..LongBool is NOT Set the MinValue and MaxValue to 0, 1
      FixBoolTypInfo(GetTypeData(Typeinfo(ByteBool)));
      FixBoolTypInfo(GetTypeData(Typeinfo(WordBool)));
      FixBoolTypInfo(GetTypeData(Typeinfo(LongBool)));
    {$ENDIF}
    
    RegisterTypeInfo(Typeinfo(ByteBool));
    RegisterTypeInfo(Typeinfo(WordBool));
    RegisterTypeInfo(Typeinfo(LongBool));
  
    RegisterTypeInfo(Typeinfo(TObject));
  end;
end;

{$IFDEF MeRTTI_SUPPORT}
const
  cMeTypeClassName: PChar = 'TMeType';
  cMeClassTypeClassName: PChar = 'TMeClassType';
  cMeFloatTypeClassName: PChar = 'TMeFloatType';
  cMeInt64TypeClassName: PChar = 'TMeInt64Type';
  //cMeStringTypeClassName: PChar = 'TMeStringType';
  cMeShortStringTypeClassName: PChar = 'TMeShortStringType';
{$ENDIF}

(*
{$IFDEF DEBUG}
var i: integer;
{$ENDIF}
*)
initialization
  SetMeVirtualMethod(TypeOf(TMeType), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeClassType), ovtVmtParent, TypeOf(TMeType));
  SetMeVirtualMethod(TypeOf(TMeFloatType), ovtVmtParent, TypeOf(TMeType));
  SetMeVirtualMethod(TypeOf(TMeInt64Type), ovtVmtParent, TypeOf(TMeType));
  SetMeVirtualMethod(TypeOf(TMeShortStringType), ovtVmtParent, TypeOf(TMeType));

  SetMeVirtualMethod(TypeOf(TMeCustomOrdinalType), ovtVmtParent, TypeOf(TMeType));
  SetMeVirtualMethod(TypeOf(TMeSetType), ovtVmtParent, TypeOf(TMeCustomOrdinalType));
  SetMeVirtualMethod(TypeOf(TMeOrdinalType), ovtVmtParent, TypeOf(TMeCustomOrdinalType));
  SetMeVirtualMethod(TypeOf(TMeEnumerationType), ovtVmtParent, TypeOf(TMeOrdinalType));
  SetMeVirtualMethod(TypeOf(TMeCustomIntegerType), ovtVmtParent, TypeOf(TMeOrdinalType));

  SetMeVirtualMethod(TypeOf(TMeStructuredType), ovtVmtParent, TypeOf(TMeType));
  SetMeVirtualMethod(TypeOf(TMeRecordType), ovtVmtParent, TypeOf(TMeStructuredType));
  SetMeVirtualMethod(TypeOf(TMeArrayType), ovtVmtParent, TypeOf(TMeStructuredType));

  SetMeVirtualMethod(TypeOf(TMeRegisteredTypes), ovtVmtParent, TypeOf(TMeList));


  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeClassType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeFloatType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeInt64Type), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeShortStringType), ovtVmtClassName, nil);

  SetMeVirtualMethod(TypeOf(TMeCustomOrdinalType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeSetType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeOrdinalType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeEnumerationType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeCustomIntegerType), ovtVmtClassName, nil);

  SetMeVirtualMethod(TypeOf(TMeStructuredType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeRecordType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeArrayType), ovtVmtClassName, nil);

  SetMeVirtualMethod(TypeOf(TMeRegisteredTypes), ovtVmtClassName, nil);
  {$ENDIF}

  //RegisterTypes; //move the GRegisteredTypes

  //for MeClassType streamable  
  RegisterClass(TObject);

finalization
(*
  {$IFDEF DEBUG}
  for i := FRegisteredTypes.Count -1 downto 0 do
  begin
    SendDebug(IntToStr(i)+':'+PMeType(FRegisteredTypes.Items[i]).Name);
    PMeType(FRegisteredTypes.Items[i]).Free;
    FRegisteredTypes.Items[i] := nil;
  end;
  FRegisteredTypes.FreeMeObjects();
  {$ENDIF}
*)
  MeFreeAndNil(FRegisteredTypes);
  MeFreeAndNil(FRegisteredClasses);
end.
