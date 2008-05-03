
{Summary: the Mini Run-time ProcType Infomation of Object. .}
{
  License:
    * The contents of this file are released under the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeProcType.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2006-2008
    * All rights reserved.

    * Contributor(s):
}
unit uMeProcType;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, TypInfo
  , Variants
  , uMeConsts
  , uMeSystem
  , uMeObject
  , uMeTypInfo //the TCallingConvention
  , uMeTypes
  ;


type
  PMeParamType        = ^ TMeParamType;
  PMeProcType         = ^ TMeProcType;
  PMeParam            = ^ TMeParam;
  PMeParams           = ^ TMeParams;
  PMeCustomProcParams = ^ TMeCustomProcParams;
  PMeProcParams       = ^ TMeProcParams;
  PMeTypes            = ^ TMeTypes;

  { Summary the parameter type info of the procedure or method }
  TMeParamType = object//(TMeType)
  protected
    //the param type info object.
    FParamType: PMeType;
    //pfVar in Flags passed by reference, as 32-bit pointers that point to the actual storage location.
    FFlags: TParamFlags;  //##byte
    FProcType: PMeProcType;
    {$IFDEF MeRTTI_EXT_SUPPORT}
    FParamName: ShortString; //TMeIdentityString;
    {$ENDIF}
  public
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);
  protected
    //procedure Init;virtual;
    function GetTypeSize: Integer;
    function GetStackTypeSize: Integer;
    function GetParamTypeName: TMeIdentityString;
    function GetKind: TMeTypeKind;
    {$IFDEF MeRTTI_EXT_SUPPORT}
    //function GetName: ShortString;
    {$ENDIF}
  public
    {: the param is by reference. means pointer.}
    function IsByRef: Boolean;
    //function ParamResultType: TMeParamDataType;
  public
    {: the param type info object.}
    property ParamType: PMeType read FParamType write FParamType;
    {: the param flags. }
    {
     set of [pfVar, pfConst, pfArray, pfAddress, pfReference, pfOut]
    }
    property Flags: TParamFlags read FFlags write FFlags;
    {: the procedure type own the param. }
    property ProcType: PMeProcType read FProcType;
    property ParamTypeName: TMeIdentityString read GetParamTypeName;
    {: the param type size (bytes).}
    property TypeSize: Integer read GetTypeSize;
    {: the size that is actually allocated on the stack for a given type.}
    {
    This is frequently different than the heap allocation for
    an object, because all stack pointers are allocated on 4 byte boundaries.
    So for example, the Extended type might occupy 10 bytes, but we will
    always allocate 12 bytes on the stack for it.
    }
    property StackTypeSize: Integer read GetStackTypeSize;
    property Kind: TMeTypeKind read GetKind;

    {$IFDEF MeRTTI_EXT_SUPPORT}
    {: the parameter name.}
    property Name: ShortString read FParamName write FParamName;
    {$ENDIF}
  end;

  PMeParamData = ^TMeParamData; //For RTTI Cast
  TMeParamData = object       // 函数参数的数据结构
    Flags: TParamFlags;     // 参数传递规则
    ParamName: ShortString; // 参数的名称
    TypeName: ShortString;  // 参数的类型名称
  end;

  TMeProcType = object(TMeType)
  protected
    FSelfType: PMeType;
    FMethodKind: TMethodKind;
    FCallingConvention: TCallingConvention;
    FResultType: PMeType;
    //pointer to PMeParamType 
    //manage the ParamType, 参数类型列表
    FParamList: TMeList;
    FTypeInfo: PTypeInfo;

    function GetIsMethod: Boolean;
    function GetIsClassMethod: Boolean;
    function GetIsFunction: Boolean;
    procedure SetTypeInfo(const aTypeInfo: PTypeInfo);
    procedure SetSelfType(const aValue: PMeType);
    function SetProcParamInfo(var aProcParamInfo: TMeParamType; const aParamData: TMeParamData): PMeParamData;
    procedure SetResultType(const aValue: PMeType);
  protected
    procedure AssignFromTypeData(aTypeData: PTypeData);virtual;
  public  //methods
    procedure Init;virtual;
    destructor Destroy;virtual;
    procedure SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent = nil);virtual;
    procedure LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent = nil);virtual;

    {: create a new Param for the procedure.}
    function NewParam: PMeParamType;
    function RetOnStack: Boolean;
    function RetInFPU: Boolean;
    //whether the size include the registers.. 
    {
    -1: means 根据调用约定决定
    0: means 没有使用寄存器
    > 0 means 计算的时候跳过指定个数的寄存器大小
    }
    function GetStackTypeSizeIn(vRegCount: Integer = -1): Integer; 
    function GetStackTypeSize: Integer;
    // check the proc type is same as aProcType(the same result type, the same paramters type...).
    function IsSameAs(const aProcType: PMeType): Boolean;virtual;
  public  //properties
    //Note: only the method type can have the RTTI TypeInfo !! So we have to declare the procedure type as method type!!
    property TypeInfo: PTypeInfo read FTypeInfo write SetTypeInfo;
    //the class(object) owns the method, if nil means it is procedure.
    property SelfType: PMeType read FSelfType write SetSelfType;
    property IsMethod: Boolean read GetIsMethod;
    property IsClassMethod: Boolean read GetIsClassMethod;
    property IsFunction: Boolean read GetIsFunction;
    property MethodKind: TMethodKind read FMethodKind write FMethodKind;
    property CallingConvention: TCallingConvention read FCallingConvention;
    //Just be careful, if it is a string then just set once, or it will add more parameters.
    property ResultType: PMeType read FResultType write SetResultType;
    //the ParamType List, if it's a function and RetOnStack then the last ParamType is the ResultType..
    //NOTE: SelfType 没有在参数类型表中，而如果函数的返回值不是RetOnStack的话，也不会在参数类型表中！
    property ParamList: TMeList read FParamList;
    property StackTypeSize: Integer read GetStackTypeSize;
  end;

  {: represents a parameter for a procedure.}
  TMeParam = Object(TMeDynamicObject)
  protected
    //pointer to param value. assign by TMeParams
    FParamValue: TMeVarRec;
    FDataType: TMeParamType;
    //FParamType: TMeParamDataType;
  protected
    function GetAsVariant: Variant;
    procedure SetAsVariant(const aValue: Variant);
    function GetAsPointer: Pointer;
    procedure SetAsPointer(const aValue: Pointer);
    function GetAsInteger: Integer;
    procedure SetAsInteger(const aValue: Integer);
    function GetAsInt64: Int64;
    procedure SetAsInt64(const aValue: Int64);
    function GetAsString: String;
    procedure SetAsString(const aValue: String);

    {$IFDEF MeRTTI_EXT_SUPPORT}
    function GetName: ShortString;
    {$ENDIF}
  public
    destructor Destroy;virtual;
    procedure Assign(const aParam: PMeParam);
    //free FParamValue memory if neccesary
    procedure ClearValue;
    //you must clearValue first!!
    procedure CopyValue(const src: TMeVarRec);
    function IsByRef: Boolean;
    procedure LoadFromStream(const aStream: PMeStream);
    procedure SaveToStream(const aStream: PMeStream; const aStoredParamName: Boolean = False; const aStoredParamType: Boolean = False);
    //Dec the aStackPt. and push the param value to the stack.
    {Note: you MUSY be sure enough space in it.
    但是对于字符串的内存分配如何处理？算了不放在这里！
    }
    //procedure PushToStack(var aStackPt: Pointer);

  public
    Property ParamValue: TMeVarRec read FParamValue write FParamValue;
    //Represents the value of the parameter as a Variant.
    Property Value: Variant read GetAsVariant write SetAsVariant;
    Property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    Property AsInteger: Integer read GetAsInteger write SetAsInteger;
    Property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    Property AsString: String read GetAsString write SetAsString;
    //Property AsXXX: AsString, AsInteger, AsBoolean, AsDateTime....
    //Indicates the type of parameter.
    Property DataType: TMeParamType read FDataType;
    //Indicates the input/output type of the parameter
    //Property ParamType: TMeParamDataType read FParamType;
    {$IFDEF MeRTTI_EXT_SUPPORT}
    {: the Param Name }
    property Name: ShortString read GetName;
    {$ENDIF}
  end;

  {: manages a list of parameters include the Self and Result parameters if any.}
  {
    Note:if it's method then the first param is Self param(the param name is always "Self"), 
         if it's function then the last param is the result(the param name is always "Result").
  }
  TMeParams = object(TMeList)
  protected
    //FParamData: Pointer; //the param value memory
    function GetParamValue(const ParamName: String): Variant;
  public
    procedure Clear; virtual;
    destructor Destroy;virtual; //override
    {$IFDEF MeRTTI_EXT_SUPPORT}
    function ParamByName(const Value: String): PMeParam;
    property ParamValues[const ParamName: String]: Variant read GetParamValue;
    {$ENDIF}
    //Property Items[Index: Integer]: TMeParam;
  end;

  TMeCustomProcParams = object(TMeParams)
  protected
    FProcType: PMeProcType;
    FSelfParam: PMeParam;
    FResultParam: PMeParam;
    function Get(Index: Integer): PMeParam;
  public
    procedure Clear; virtual; //override
    function Add(aParam: PMeParam): integer;
    {: this count is exclude the Self param and Result param from List}
    function Count: integer;
    procedure Execute(const aProc: Pointer); virtual;abstract;
  public
    //ignore the self param from 0. if it's method.
    Property Items[Index: Integer]: PMeParam read Get;
    //the Self Param if it's method;
    Property SelfParam: PMeParam read FSelfParam;
    //the result Param if it's function;
    Property ResultParam: PMeParam read FResultParam;
  end;
  
  PMeCommonRegs = ^TMeCommonRegs;
  TMeCommonRegs = packed record
    case Integer of
      0: (EAX: Integer; EDX: Integer;  ECX: Integer);
      1: (Raw: array [0..2] of Integer);
      2: (aInt64: Int64);
  end;
  {: manages a list of procedural parameters.}
  TMeProcParams = object(TMeCustomProcParams)
  protected
  public
    //Init and realloc param mem from aProcType.
    procedure InitFromType(const aProcType: PMeProcType);
    {: assign the parameters from stack.}
    {
    @param aLastParamPtr pointer to the last param address in the stack
    @param aRegisters aRegisters[0] = EAX; aRegisters[1] := EDX; aRegisters[2] := ECX

    Note: you must InitFromType first before call it
    }
    procedure AssignFromStack(aLastParamPtr: Pointer; aRegisters: PInteger; vRegCount: Integer = -1);
    {: assign the function result after execute.}
    {
    @param aRegisters aRegisters[0] = EAX; aRegisters[1] := EDX; aRegisters[2] := ECX
    }
    procedure AssignResult(aRegisters: PMeCommonRegs);
    {: save the parameters to the stack before execute}
    procedure SaveToStack(aLastParamPtr: Pointer; aRegisters: PInteger);
    {: save the Result after execute}
    procedure SaveResult(aRegisters: PInteger);
    procedure Execute(const aProc: Pointer);virtual;//override
  public
    Property ProcType: PMeProcType read FProcType write InitFromType;
  end;

  {: extent(add) the methods to manage the ProcTypes }
  TMeTypes = Object(TMeRegisteredTypes)
  protected
    function CreateMeTypeBy(const aKind: TMeTypeKind): PMeType;virtual; //override
  public
    function GetRegisteredProcType(const aTypeInfo: PTypeInfo; const aIsMethod: Boolean): PMeProcType;overload;
    function GetRegisteredProcType(const aProcType: PMeProcType): PMeProcType;overload;
    {: register the ProcType if exists , return the PMeProcType.}
    function RegisterProcTypeInfo(const aTypeInfo: PTypeInfo; const aClassType: PMeType): PMeProcType;overload;
    function RegisterProcTypeInfo(const aTypeInfo: PTypeInfo; const aClass: TClass): PMeProcType;overload;
    function RegisterProcType(const aProcType: PMeProcType): PMeProcType;
  end;

function GetRegisteredProcType(const aTypeInfo: PTypeInfo; const aIsMethod: Boolean): PMeProcType;overload;
function GetRegisteredProcType(const aProcType: PMeProcType): PMeProcType;overload;
{: register the ProcType if exists , return the PMeProcType.}
function RegisterProcTypeInfo(const aTypeInfo: PTypeInfo; const aClassType: PMeType): PMeProcType;overload;
function RegisterProcTypeInfo(const aTypeInfo: PTypeInfo; const aClass: TClass): PMeProcType;overload;
function RegisterProcType(const aProcType: PMeProcType): PMeProcType;

implementation

function RegisterProcType(const aProcType: PMeProcType): PMeProcType;
begin
  Result := PMeTypes(GRegisteredTypes).RegisterProcType(aProcType);
end;


function RegisterProcTypeInfo(const aTypeInfo: PTypeInfo; const aClass: TClass): PMeProcType;
begin
  Result := PMeTypes(GRegisteredTypes).RegisterProcTypeInfo(aTypeInfo, aClass)
end;

function RegisterProcTypeInfo(const aTypeInfo: PTypeInfo; const aClassType: PMeType): PMeProcType;
begin
  Result := PMeTypes(GRegisteredTypes).RegisterProcTypeInfo(aTypeInfo, aClassType);
end;

function GetRegisteredProcType(const aProcType: PMeProcType): PMeProcType;
begin
  Result := PMeTypes(GRegisteredTypes).GetRegisteredProcType(aProcType);
end;

function GetRegisteredProcType(const aTypeInfo: PTypeInfo; const aIsMethod: Boolean): PMeProcType;
begin
  Result := PMeTypes(GRegisteredTypes).GetRegisteredProcType(aTypeInfo, aIsMethod);
end;

{ #### TMeTypes #### }
function TMeTypes.CreateMeTypeBy(const aKind: TMeTypeKind): PMeType;
begin
  if aKind = mtkMethod then
    Result := New(PMeProcType, Create)
  else 
    Result := Inherited CreateMeTypeBy(aKind);
end;

function TMeTypes.RegisterProcType(const aProcType: PMeProcType): PMeProcType;
begin
  if aProcType <> nil then
  begin
    Result := GetRegisteredProcType(aProcType);
    if Result = nil then
    begin
      Result := aProcType;
      if Add(Result) < 0 then Result := nil;
    end;
  end
  else
    Result := nil;
end;


function TMeTypes.RegisterProcTypeInfo(const aTypeInfo: PTypeInfo; const aClass: TClass): PMeProcType;
begin
  if aTypeInfo <> nil then
    Result := RegisterProcTypeInfo(aTypeInfo, RegisterClassType(aClass))
  else
    Result := nil;
end;

function TMeTypes.RegisterProcTypeInfo(const aTypeInfo: PTypeInfo; const aClassType: PMeType): PMeProcType;
begin
  //if aTypeInfo <> nil then
  //begin
    Result := GetRegisteredProcType(aTypeInfo, aClassType <> nil);
    if Result = nil then
    begin
      New(Result, Create);
      Result.Assign(aTypeInfo);
      Result.SelfType := aClassType;
      Add(Result);
    end;
  //end
  //else
    //Result := nil;
end;

function TMeTypes.GetRegisteredProcType(const aProcType: PMeProcType): PMeProcType;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := List^[i];
    if (Result.ClassType = TypeOf(TMeProcType)) and (Result.IsSameAs(aProcType)) then
      exit;
  end;
  Result := nil;
end;

function TMeTypes.GetRegisteredProcType(const aTypeInfo: PTypeInfo; const aIsMethod: Boolean): PMeProcType;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := List^[i];
    if aIsMethod then
    begin
      if (Result.ClassType = TypeOf(TMeProcType)) and (Result.TypeInfo = aTypeInfo) and (Result.IsMethod) then
        exit;
    end
    else 
      if (Result.ClassType = TypeOf(TMeProcType)) and (Result.TypeInfo = aTypeInfo) and (not Result.IsMethod) then
        exit;
  end;
  Result := nil;
end;

{ #### TMeParamType #### }
{procedure TMeParamType.Init;
begin
  inherited;
  FKind := mtkParam;
end;
//}

procedure TMeParamType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
var
  i: integer;
begin
  with aStream do
  begin
    {$IFDEF FPC}
    WriteBuffer(FFlags, 1);  //##byte
    {$ELSE BORLAND}
    WriteBuffer(FFlags, SizeOf(FFlags));  //##byte
    {$ENDIF}
    
    if Assigned(aOnSaveType) then
      aOnSaveType(@Self, aStream, FParamType);
    {$IFDEF MeRTTI_EXT_SUPPORT}
    i := Length(FParamName);
    WriteBuffer(i, SizeOf(i));
    if i > 0 then
      WriteBuffer(FParamName[1], i);
    {$ENDIF}
  end;
end;

procedure TMeParamType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
var
  i: integer;
begin
  with aStream do
  begin
    {$IFDEF FPC}
    i := 0;
    ReadBuffer(i, 1);  //##byte
    Integer(FFlags) := i;
    {$ELSE BORLAND}
    ReadBuffer(FFlags, SizeOf(FFlags));  //##byte
    {$ENDIF}

    if Assigned(aOnLoadType) then
      aOnLoadType(@Self, aStream, FParamType);
    {$IFDEF MeRTTI_EXT_SUPPORT}
    ReadBuffer(i, SizeOf(i));
    SetLength(FParamName, i);
    if i > 0 then
      ReadBuffer(FParamName[1], i);
    {$ENDIF}
  end;
end;

function TMeParamType.GetParamTypeName: TMeIdentityString;
begin
  Result := FParamType.Name;
end;

{$IFDEF MeRTTI_EXT_SUPPORT}
{function TMeParamType.GetName: ShortString;
begin
  Result := FParamName;
end;}
{$ENDIF}

function TMeParamType.GetTypeSize: Integer;
begin
  if IsByRef then
    Result := SizeOf(Pointer)
  else
    Result := uMeTypes.GetTypeSize(FParamType);
end;

function TMeParamType.GetKind: TMeTypeKind;
begin
  Result := mtkUnknown;
  if Assigned(FParamType) then
    Result := FParamType.Kind;
end;

function TMeParamType.GetStackTypeSize: Integer;
begin
  if IsByRef then
    Result := SizeOf(Pointer)
  else
    Result := uMeTypes.GetStackTypeSize(FParamType, FProcType.FCallingConvention);
end;

function TMeParamType.IsByRef: Boolean;
begin
  Result := IsParamByRef(FFlags, FParamType, FProcType.FCallingConvention);
end;

{ ####TMeProcType#### }

procedure TMeProcType.Init;
begin
  inherited;
  FKind := mtkMethod;
end;

destructor TMeProcType.Destroy;
begin
  FParamList.FreePointers; //the FParamList is paramType object not a dynamic obj. 
  FParamList.Clear;
  inherited;
end;

procedure TMeProcType.SaveToStream(const aStream: TStream; aOnSaveType: TMeTypeSaveToStreamEvent);
var
  i: integer;
begin
  inherited SaveToStream(aStream, aOnSaveType);
  with aStream do
  begin
    WriteBuffer(FMethodKind, SizeOf(FMethodKind));
    WriteBuffer(FCallingConvention, SizeOf(FCallingConvention));
    if Assigned(aOnSaveType) then
    begin
      aOnSaveType(@Self, aStream, FSelfType);
      aOnSaveType(@Self, aStream, FResultType);
    end;
    i := FParamList.Count;
    WriteBuffer(i, SizeOf(i));
    for i := 0 to FParamList.Count - 1 do
    begin
      PMeParamType(FParamList.items[i]).SaveToStream(aStream, aOnSaveType);
    end;
  end;
end;

procedure TMeProcType.LoadFromStream(const aStream: TStream; aOnLoadType: TMeTypeLoadFromStreamEvent);
var
  i: integer;
  vParamType: PMeParamType;
begin
  inherited LoadFromStream(aStream, aOnLoadType);
  with aStream do
  begin
    ReadBuffer(FMethodKind, SizeOf(FMethodKind));
    ReadBuffer(FCallingConvention, SizeOf(FCallingConvention));
    if Assigned(aOnLoadType) then
    begin
      aOnLoadType(@Self, aStream, FSelfType);
      aOnLoadType(@Self, aStream, FResultType);
    end;
    //i := FParamList.Count;
    ReadBuffer(i, SizeOf(i));
    FParamList.FreePointers;
    FParamList.Clear;
    FParamList.Count := i;
    for i := 0 to FParamList.Count - 1 do
    begin
      vParamType := New(PMeParamType);
      vParamType.LoadFromStream(aStream, aOnloadType);
      FParamList.List^[i] := vParamType;
      vParamType.FProcType := @Self;
    end;
  end;
end;

function TMeProcType.IsSameAs(const aProcType: PMeType): Boolean;
var
  i: integer;
begin
  Result := inherited IsSameAs(aProcType);
  if Result then
    Result := Assigned(FTypeInfo) and (FTypeInfo = PMeProcType(aProcType).FTypeInfo);
  if not Result then
  begin
    Result := (FSelfType = PMeProcType(aProcType).FSelfType) and (FMethodKind = PMeProcType(aProcType).FMethodKind) and (FCallingConvention = PMeProcType(aProcType).FCallingConvention) 
      and (FResultType = PMeProcType(aProcType).FResultType) and (FParamList.Count = PMeProcType(aProcType).FParamList.Count);
    if Result then
      for i := 0 to FParamList.Count - 1 do
      begin
        with PMeParamType(PMeProcType(aProcType).FParamList.List^[i])^ do
          Result := (PMeParamType(FParamList.List^[i]).FFlags = FFlags) and (PMeParamType(FParamList.List^[i]).FParamType = FParamType);
        if not Result then Exit;
      end;
  end;
end;
procedure TMeProcType.SetResultType(const aValue: PMeType);
var
  vParamType: PMeParamType;
begin
  if FResultType <> aValue then
  begin
    FResultType := aValue;
    if RetOnStack then
    begin //the caller passes an additional 32-bit pointer that points to a variable in which to return the function result.
      vParamType := New(PMeParamType);
      if FParamList.Add(vParamType) >= 0 then
      begin
        vParamType.FProcType := @Self;
        vParamType.FFlags := [pfVar];
        vParamType.FParamType := FResultType;
      end;
    end;
  end;
end;

procedure TMeProcType.SetSelfType(const aValue: PMeType);
begin
  //if FSelfType <> aValue then
  //begin
    FSelfType := aValue;
    if Assigned(FSelfType) then
      FKind := mtkMethod
    else
      FKind := mtkProcedure;
  //end;
end;

//return the next paramData address
function TMeProcType.SetProcParamInfo(var aProcParamInfo: TMeParamType; const aParamData: TMeParamData): PMeParamData;
var
  vTypeStr: PShortString;
begin
  with aProcParamInfo do
  begin
    FFlags := aParamData.Flags;
    vTypeStr := Pointer(Integer(@aParamData.ParamName) +
      Length(aParamData.ParamName) + 1);
    {$IFDEF MeRTTI_EXT_SUPPORT}
    FParamName := aParamData.ParamName;
    {$ENDIF}
    FParamType := nil; //issue #7
    if Assigned(Self.Owner) then
    begin
      FParamType := Self.Owner.GetRegisteredTypeByName(vTypeStr^);
    end;
    if FParamType = nil then
    begin
      FParamType := GetRegisteredTypeByName(vTypeStr^);
    end;
    if FParamType = nil then
    begin
      raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, [vTypeStr^]);
    end;
    //FParamType := StrToVarType(vTypeStr^);
    //if FParamType = vtEmpty then
      //raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, [vTypeStr^]);
  end;
  Result := PMeParamData(Integer(@aParamData) + SizeOf(TParamFlags) +
    Length(aParamData.ParamName) + Length(vTypeStr^) + 2);
end;

function TMeProcType.NewParam: PMeParamType;
begin
  Result := New(PMeParamType);
  Result.FProcType := @Self;
  FParamList.Add(Result);
end;

procedure TMeProcType.AssignFromTypeData(aTypeData: PTypeData);
var
  I: Integer;
  vParamData: PMeParamData;
  vStr: PShortString;
  vParamType: PMeParamType;
begin
  FMethodKind := aTypeData^.MethodKind;
  FParamList.Count := aTypeData^.ParamCount;
  vParamData := PMeParamData(@aTypeData^.ParamList);
  for I := 0 to FParamList.Count - 1 do
  begin
    vParamType := New(PMeParamType);
    FParamList.List^[I] := vParamType;
    vParamType.FProcType := @Self;
    vParamData := SetProcParamInfo(vParamType^, vParamData^);
  end;

  FResultType := nil;
  if (FMethodKind = mkFunction) or (FMethodKind = mkClassFunction) then
  begin
    vStr := PShortString(vParamData);
    if Assigned(FOwner) then
      FResultType := FOwner.GetRegisteredTypeByName(vStr^);
    if not Assigned(FResultType) then
      FResultType:= GetRegisteredTypeByName(vStr^);
    if FResultType = nil then
      Raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, [vStr^]);
    if RetOnStack then
    begin //the caller passes an additional 32-bit pointer that points to a variable in which to return the function result.
      vParamType := New(PMeParamType);
      if FParamList.Add(vParamType) >= 0 then
      begin
        vParamType.FProcType := @Self;
        vParamType.FFlags := [pfVar];
        vParamType.FParamType := FResultType;
      end
      //else 
        //Raise EMe
      ;
      
    end;
    vParamData := PMeParamData(Integer(vParamData) 
      + Length(vStr^) + 1 + 4);
  end;
  FCallingConvention := TCallingConvention(PByte(vParamData)^);
end;

procedure TMeProcType.SetTypeInfo(const aTypeInfo: PTypeInfo);
begin
  FTypeInfo := aTypeInfo;
  if aTypeInfo = nil then
  begin
    //FParamCount := 0;
    FMethodKind := mkProcedure;
    FParamList.FreePointers;
    FParamList.Clear;
    FName := '';
  end
  else
    Assign(aTypeInfo)
end;

function TMeProcType.GetIsFunction: Boolean;
begin
  Result := FMethodKind in [mkFunction, mkClassFunction];
end;

function TMeProcType.GetIsClassMethod: Boolean;
begin
  Result := FMethodKind in [mkClassProcedure, mkClassFunction
    {$IFDEF COMPILER8_UP}
    , mkClassConstructor
    {$ENDIF}
  ];
end;

function TMeProcType.GetIsMethod: Boolean;
begin
  Result := FSelfType <> nil;
end;

function TMeProcType.GetStackTypeSize: Integer;
begin
  Result := GetStackTypeSizeIn(-1);
end;

function TMeProcType.GetStackTypeSizeIn(vRegCount: Integer): Integer;
var
  i: integer;
  //vRegCount: integer;
  vSize: integer;
begin
  Result := 0;
  //vRegCount := 0;
  //根据register调用约定，int64,将直接压入堆栈，而不是保存到寄存器！！
  //而开放数组，将传两个参数!!
  //Sets, records, and static arrays of 1, 2, or 4 bytes are passed as 8-bit, 16-bit, and 32-bit values. 
  //Larger sets, records, and static arrays are passed as 32-bit pointers 
  if vRegCount < 0 then
    Case FCallingConvention of
      ccRegister: vRegCount := 3;
      ccFastCall: vRegCount := 2;
    End;
  if IsMethod then Dec(vRegCount);
  for i := 0 to FParamList.Count - 1 do
  with PMeParamType(FParamList.List^[i])^ do 
  begin
    vSize := GetStackTypeSize;
    if (vRegCount > 0) and not (ParamType.Kind in [mtkFloat, mtkMethod, mtkVariant, mtkInt64]) then
    begin
      Dec(vRegCount);
      if (ParamType.Kind = mtkDynArray) then
      begin
        if vRegCount > 0 then 
          Dec(vRegCount) 
        else 
          Result := Result + SizeOf(Pointer);
      end;
    end
    else
      Result := Result + vSize;
  end;
//  if (c <> 0) and (FCallingConvention <> ccRegister) then Result := Result + SizeOf(Pointer);
end;

function TMeProcType.RetInFPU: Boolean;
begin
  Result := Assigned(FResultType) and (FResultType.Kind = mtkFloat);
end;

function TMeProcType.RetOnStack: Boolean;
begin
  Result := Assigned(FResultType) and (FResultType.Kind in [mtkLString, mtkWString, mtkString, mtkDynArray, mtkMethod, mtkVariant])
end;

{ #### TMeParam #### }
destructor TMeParam.Destroy;
begin
  ClearValue;
  inherited;
end;

procedure TMeParam.Assign(const aParam: PMeParam);
begin
  ClearValue;
  if Assigned(aParam) then
  begin
    FDataType := aParam.DataType;
    CopyValue(aParam.FParamValue);
  end;
end;

procedure TMeParam.ClearValue;
begin
  if Assigned(FDataType.ParamType) and not FDataType.IsByRef then
  begin
    case FDataType.ParamType.Kind of
      mtkLString: Finalize(AnsiString(FParamValue.VAnsiString));
      mtkWString: Finalize(WideString(FParamValue.VWideString));
      mtkString:  if Assigned(FParamValue.vString) then FreeMem(FParamValue.vString);
      //mtkVariant: Finalize(FParamValue.VVariant^);
    end;
  end;
  FParamValue := cEmptyMeVar;
end;

procedure TMeParam.CopyValue(const src: TMeVarRec);
begin
  if Assigned(FDataType.ParamType) then
    if FDataType.IsByRef then
      FParamValue := src
    else
      case FDataType.ParamType.Kind of
        mtkLString: AnsiString(FParamValue.VAnsiString) := AnsiString(src.VAnsiString);
        mtkWString: WideString(FParamValue.VWideString) := WideString(src.VWideString);
        mtkString:
          begin
            if not Assigned(FParamValue.vString) then
              New(FParamValue.VString);
            FParamValue.VString^ := src.VString^;
          end;
        {mtkVariant:
        begin
          //if Assigned(FParamValue.VVariant) then
          Finalize(FParamValue.VVariant^);
          FParamValue.VVariant := System.New(PVariant);
          FParamValue.VVariant^ := src.VVariant^;
        end; //}
      else
        begin
          FParamValue := src;
        end;
      end;
end;

{$IFDEF MeRTTI_EXT_SUPPORT}
function TMeParam.GetName: ShortString;
begin
  Result := FDataType.FParamName;
end;
{$ENDIF}

function TMeParam.GetAsVariant: Variant;
begin
  Result := Null;
  if Assigned(FDataType.ParamType) then
    if FDataType.IsByRef then
      case FDataType.ParamType.Kind of
        mtkLString: Result := PAnsiString(FParamValue.VPointer)^;
        mtkWString: Result := PWideString(FParamValue.VPointer)^;
        mtkString:  Result := PShortString(FParamValue.VPointer)^;
        mtkSet, mtkEnumeration, mtkInteger: if FDataType.ParamType.InheritsFrom(TypeOf(TMeCustomOrdinalType)) then
          case PMeCustomOrdinalType(FDataType.ParamType).OrdType of
            otSByte:
                Result := PShortInt(FParamValue.VPointer)^;
            otUByte:
                Result := PByte(FParamValue.VPointer)^;
            otSWord:
                Result := PSmallInt(FParamValue.VPointer)^;
            otUWord:
                Result := PWord(FParamValue.VPointer)^;
            otSLong:
                Result := PInteger(FParamValue.VPointer)^;
            otULong:
                Result := PLongWord(FParamValue.VPointer)^;
          end;
        mtkChar:    Result := PByte(FParamValue.VPointer)^;
        //mtkPointer: Result := FParamValue.VPointer;
        //mtkObject:     (VMeObject: Pointer);
        //mtkClass:      (VObject: TObject);
        mtkWChar:   Result := PWideChar(FParamValue.VPointer)^;
        mtkVariant: Result := PVariant(FParamValue.VPointer)^;
        //mtkInterface:  (VInterface: Pointer);
        mtkInt64:   Result := PInt64(FParamValue.VPointer)^;
        //mtkDynArray:   (VDynBound: Integer; VDynArray: Pointer);
        //mtkMethod:     (VCode: Pointer; VData: Pointer);
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
        begin
         case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: Result := PSingle(FParamValue.VPointer)^;
            ftDouble: Result := PDouble(FParamValue.VPointer)^;
            ftExtended: Result := PExtended(FParamValue.VPointer)^;
            ftComp: Result := PComp(FParamValue.VPointer)^;
            ftCurr: Result := PCurrency(FParamValue.VPointer)^;
         end;
       end;
      end// case
    else
      case FDataType.ParamType.Kind of
        mtkLString: Result := AnsiString(FParamValue.VAnsiString);
        mtkWString: Result := WideString(FParamValue.VWideString);
        mtkString:  Result := FParamValue.VString^;
        mtkInteger: 
        begin
          //Result := FParamValue.VInteger;
          if FDataType.ParamType.ClassType = TypeOf(TMeCustomOrdinalType) then
            case PMeCustomOrdinalType(FDataType.ParamType).OrdType of
              otSByte: Result := ShortInt(FParamValue.VInteger);
              otUByte: Result := Byte(FParamValue.VInteger);
              otSWord: Result := SmallInt(FParamValue.VInteger);
              otUWord: Result := Word(FParamValue.VInteger);
              otSLong: Result := FParamValue.VInteger;
              otULong: Result := LongWord(FParamValue.VInteger);
            end;//case
        end;
        mtkSet:
        begin
          case GetTypeSize(FDataType.ParamType) of
            2: Result := FParamValue.VWord;
            4: Result := FParamValue.VLongWord;
            else
              Result := FParamValue.VByte;
          end;
        end;
        mtkChar:    Result := FParamValue.VChar;
        //mtkPointer: Result := FParamValue.VPointer;
        //mtkObject:     (VMeObject: Pointer);
        //mtkClass:      (VObject: TObject);
        mtkWChar:   Result := FParamValue.VWideChar;
        mtkVariant: Result := Variant(FParamValue.VVariant);
        //mtkInterface:  (VInterface: Pointer);
        mtkInt64:   Result := FParamValue.VInt64;
        //mtkDynArray:   (VDynBound: Integer; VDynArray: Pointer);
        //mtkMethod:     (VCode: Pointer; VData: Pointer);
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
        begin
         case PMeFloatType(FDataType.ParamType).FloatType of
           ftSingle: Result := FParamValue.VSingle;
           ftDouble: Result := FParamValue.VDouble;
           ftExtended: Result := FParamValue.VExtended;
           ftComp: Result := FParamValue.VComp;
           ftCurr: Result := FParamValue.VCurr;
         end;
       end;
      end;// case
end;

procedure TMeParam.SetAsVariant(const aValue: Variant);
begin
  if Assigned(FDataType.ParamType) then
    if FDataType.IsByRef then
      case FDataType.ParamType.Kind of
        //mtkPointer, mtkObject, mtkClass, mtkInterface: FParamValue.VPointer := aValue;
        mtkLString: PAnsiString(FParamValue.VPointer)^ := aValue;
        mtkWString: PWideString(FParamValue.VPointer)^ := aValue;
        mtkString:  
        begin
          //if not Assigned(FParamValue.VString) then FreeMem(FParamValue.VString);
          //New(FParamValue.VString);
          FParamValue.VString^ := aValue;
        end;
        mtkInteger:
        begin
          PInteger(FParamValue.VPointer)^ := aValue;
        end;
        mtkSet:
        begin
          case GetTypeSize(FDataType.ParamType) of
            2: PWord(FParamValue.VPointer)^ := aValue;
            4: PLongWord(FParamValue.VPointer)^ := aValue;
            else
              PByte(FParamValue.VPointer)^ := aValue;
          end;
        end;
        mtkChar:     PByte(FParamValue.VPointer)^ := aValue;
        //mtkChar:    FParamValue.VChar := aValue;
        //mtkWChar: FParamValue.VWideChar := aValue;
        mtkVariant: 
        begin
          //Finalize(FParamValue.VVariant^);
          //New(FParamValue.VVariant);
          PVariant(FParamValue.VPointer)^ := aValue;
        end;
        mtkInt64:   PInt64(FParamValue.VPointer)^ := aValue;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        //mtkMethod:     aValue := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
        begin
         case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: PSingle(FParamValue.VPointer)^ := aValue;
            ftDouble: PDouble(FParamValue.VPointer)^ := aValue;
            ftExtended: PExtended(FParamValue.VPointer)^ := aValue;
            ftComp: PComp(FParamValue.VPointer)^ := aValue;
            ftCurr: PCurrency(FParamValue.VPointer)^ := aValue;
         end;
       end;
      end// case
    else
      case FDataType.ParamType.Kind of
        //mtkPointer, mtkObject, mtkClass, mtkInterface: FParamValue.VPointer := aValue;
        mtkLString: AnsiString(FParamValue.VPointer) := aValue;
        mtkWString: WideString(FParamValue.VPointer) := aValue;
        mtkString:  
        begin
          if not Assigned(FParamValue.VString) then 
            New(FParamValue.VString);
          FParamValue.VString^ := aValue;
        end;
        mtkInteger:
        begin
          FParamValue.VInteger := aValue;
        end;
        mtkSet:
        begin
          case GetTypeSize(FDataType.ParamType) of
            2: FParamValue.VWord := aValue;
            4: FParamValue.VLongWord := aValue;
            else
              FParamValue.VByte := aValue;
          end;
        end;
        //mtkChar:    FParamValue.VChar := aValue;
        //mtkWChar: FParamValue.VWideChar := aValue;
        mtkVariant: 
        begin
          //Finalize(FParamValue.VVariant^);
          //New(FParamValue.VVariant);
          FParamValue.VVariant := TVarData(aValue);
        end;
        mtkInt64:   FParamValue.VInt64 := aValue;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        //mtkMethod:     aValue := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
        begin
         case PMeFloatType(FDataType.ParamType).FloatType of
           ftSingle: FParamValue.VSingle := aValue;
           ftDouble: FParamValue.VDouble := aValue;
           ftExtended: FParamValue.VExtended := aValue;
           ftComp: FParamValue.VComp := aValue;
           ftCurr: FParamValue.VCurr := aValue;
         end;
       end;
      end;// case
end;

function TMeParam.GetAsPointer: Pointer;
begin
  Result := nil;
  if Assigned(FDataType.FParamType) then
    if FDataType.IsByRef then
      Result := FParamValue.VPointer
    else
      case FDataType.ParamType.Kind of
        mtkPointer, mtkObject, mtkClass, mtkInterface: Result := FParamValue.VPointer;
        mtkLString: Result := FParamValue.VAnsiString;
        mtkWString: Result := FParamValue.VWideString;
        mtkString:  Result := FParamValue.VString;
        mtkInteger:
        begin
          Result := Pointer(FParamValue.VInteger);
        end;
        mtkSet:     
        begin
          case GetTypeSize(FDataType.ParamType) of
            2: Result := Pointer(FParamValue.VWord);
            4: Result := Pointer(FParamValue.VLongWord);
            else
              Result := Pointer(FParamValue.VByte);
          end;
        end;
        mtkChar:    Result := Pointer(FParamValue.VChar);
        mtkWChar:Result := Pointer(FParamValue.VWideChar);
        mtkVariant: Result := @FParamValue.VVariant;
        mtkInt64:   Result := @FParamValue.VInt64;
        mtkDynArray:   Result := FParamValue.VDynArray;
        //mtkMethod:     Result := FParamValue.VCode;
        mtkProcedure:    Result := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
        begin
         case PMeFloatType(FDataType.ParamType).FloatType of
           ftSingle: Result := @FParamValue.VSingle;
           ftDouble: Result := @FParamValue.VDouble;
           ftExtended: Result := @FParamValue.VExtended;
           ftComp: Result := @FParamValue.VComp;
           ftCurr: Result := @FParamValue.VCurr;
         end;
       end;
      end;// case
end;

procedure TMeParam.SetAsPointer(const aValue: Pointer);
begin
  if Assigned(FDataType.ParamType) then
    if FDataType.IsByRef then
      FParamValue.VPointer := aValue
    else
      case FDataType.ParamType.Kind of
        mtkPointer, mtkProcedure, mtkObject, mtkClass, mtkInterface: FParamValue.VPointer := aValue;
        mtkLString: AnsiString(FParamValue.VPointer) := AnsiString(aValue);
        mtkWString: WideString(FParamValue.VPointer) := WideString(aValue);
        mtkString:
        begin
          if not Assigned(FParamValue.VString) then //FreeMem(FParamValue.VString);
          New(FParamValue.VString);
          FParamValue.VString^ := PShortString(aValue)^;
        end;
        mtkInteger:
        begin
          Pointer(FParamValue.VInteger) := aValue;
        end;
        //mtkSet:     aValue := Pointer(FParamValue.VSet);
        //mtkChar:    aValue := Pointer(FParamValue.VChar);
        mtkWChar: FParamValue.VWideChar := WideChar(aValue);
        mtkVariant:
        begin
          //Finalize(FParamValue.VVariant^);
          FParamValue.VVariant := PVarData(aValue)^;
        end;
        mtkInt64:   FParamValue.VInt64 := PInt64(aValue)^;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        //mtkMethod:     aValue := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
        begin
         case PMeFloatType(FDataType.ParamType).FloatType of
           ftSingle: FParamValue.VSingle := PSingle(aValue)^;
           ftDouble: FParamValue.VDouble := PDouble(aValue)^;
           ftExtended: FParamValue.VExtended := PExtended(aValue)^;
           ftComp: FParamValue.VComp := PComp(aValue)^;
           ftCurr: FParamValue.VCurr := PCurrency(aValue)^;
         end;
       end;
      end;// case
end;

function TMeParam.GetAsInteger: Integer;
begin
  Result := 0;
  if Assigned(FDataType.ParamType) then
    if FDataType.IsByRef then
      case FDataType.ParamType.Kind of
        mtkSet, mtkEnumeration, mtkInteger: if FDataType.ParamType.InheritsFrom(TypeOf(TMeCustomOrdinalType)) then
          case PMeCustomOrdinalType(FDataType.ParamType).OrdType of
            otSByte:
                Result := PShortInt(FParamValue.VPointer)^;
            otUByte:
                Result := PByte(FParamValue.VPointer)^;
            otSWord:
                Result := PSmallInt(FParamValue.VPointer)^;
            otUWord:
                Result := PWord(FParamValue.VPointer)^;
            otSLong:
                Result := PInteger(FParamValue.VPointer)^;
            otULong:
                Result := PLongWord(FParamValue.VPointer)^;
          end;

        mtkPointer, mtkProcedure, mtkObject, mtkClass, mtkInterface, mtkWChar:
          Result := PInteger(FParamValue.VPointer)^;
        mtkChar:
          Result := PByte(FParamValue.VPointer)^;
        mtkVariant:
        begin
          Result := PVariant(FParamValue.VPointer)^;
        end;
        mtkInt64:   Result := PInt64(FParamValue.VPointer)^;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        //mtkMethod:     aValue := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: Result := Trunc(PSingle(FParamValue.VPointer)^);
            ftDouble: Result := Trunc(PDouble(FParamValue.VPointer)^);
            ftExtended: Result := Trunc(PExtended(FParamValue.VPointer)^);
            ftComp: Result := Trunc(PComp(FParamValue.VPointer)^);
            ftCurr: Result := Trunc(PCurrency(FParamValue.VPointer)^);
          end;
        mtkLString: Result := StrToInt(PAnsiString(FParamValue.VPointer)^);
        mtkWString: Result := StrToInt(PWideString(FParamValue.VPointer)^);
        mtkString:  Result := StrToInt(FParamValue.VString^);
      end// case
    else
      case FDataType.ParamType.Kind of
        mtkSet, mtkEnumeration, mtkInteger: if FDataType.ParamType.InheritsFrom(TypeOf(TMeCustomOrdinalType)) then
          case PMeCustomOrdinalType(FDataType.ParamType).OrdType of
            otSByte:
                Result := ShortInt(FParamValue.VByte);
            otUByte:
                Result := FParamValue.VByte;
            otSWord:
                Result := SmallInt(FParamValue.VWord);
            otUWord:
                Result := FParamValue.VWord;
            otSLong:
                Result := FParamValue.VInteger;
            otULong:
                Result := FParamValue.VLongWord;
          end;

        mtkPointer, mtkProcedure, mtkObject, mtkClass, mtkInterface, mtkWChar:
          Result := FParamValue.VInteger;
        mtkChar:
          Result := FParamValue.VByte;
        mtkVariant:
        begin
          Result := Variant(FParamValue.VVariant);
        end;
        mtkInt64:   Result := FParamValue.VInt64;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        //mtkMethod:     aValue := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: Result := Trunc(FParamValue.VSingle);
            ftDouble: Result := Trunc(FParamValue.VDouble);
            ftExtended: Result := Trunc(FParamValue.VExtended);
            ftComp: Result := Trunc(FParamValue.VComp);
            ftCurr: Result := Trunc(FParamValue.VCurr);
          end;
        mtkLString: Result := StrToInt(AnsiString(FParamValue.VAnsiString));
        mtkWString: Result := StrToInt(WideString(FParamValue.VWideString));
        mtkString:  Result := StrToInt(FParamValue.VString^);
      end;// case
end;

procedure TMeParam.SetAsInteger(const aValue: Integer);
begin
  if Assigned(FDataType.ParamType) then
    if FDataType.IsByRef then
      case FDataType.ParamType.Kind of
        mtkInteger, mtkEnumeration, mtkPointer, mtkObject, mtkClass, mtkInterface, mtkWChar, mtkSet, mtkChar:
              PInteger(FParamValue.VPointer)^ := aValue;
        mtkVariant:
        begin
          PVariant(FParamValue.VPointer)^ := aValue;
        end;
        mtkInt64:   PInt64(FParamValue.VPointer)^ := aValue;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        mtkProcedure:     PPointer(FParamValue.VPointer)^ := Pointer(aValue);
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: PSingle(FParamValue.VPointer)^ := aValue;
            ftDouble: PDouble(FParamValue.VPointer)^ := aValue;
            ftExtended: PExtended(FParamValue.VPointer)^ := aValue;
            ftComp: PComp(FParamValue.VPointer)^ := aValue;
            ftCurr: PCurrency(FParamValue.VPointer)^ := aValue;
          end;
        mtkLString: PAnsiString(FParamValue.VAnsiString)^ := IntToStr(aValue);
        mtkWString: PWideString(FParamValue.VWideString)^ := IntToStr(aValue);
        mtkString:  
          begin 
            if not Assigned(FParamValue.VString) then New(FParamValue.VString); 
            FParamValue.VString^ := IntToStr(aValue); 
          end;
      end// case
    else
      case FDataType.ParamType.Kind of
        mtkInteger, mtkEnumeration, mtkPointer, mtkObject, mtkClass, mtkInterface, mtkWChar, mtkSet, mtkChar:
              FParamValue.VInteger := aValue;
        mtkVariant:
        begin
          Variant(FParamValue.VVariant) := aValue;
        end;
        mtkInt64:   FParamValue.VInt64 := aValue;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        mtkProcedure:     FParamValue.VCode := Pointer(aValue);
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: FParamValue.VSingle := aValue;
            ftDouble: FParamValue.VDouble := aValue;
            ftExtended: FParamValue.VExtended := aValue;
            ftComp: FParamValue.VComp := aValue;
            ftCurr: FParamValue.VCurr := aValue;
          end;
        mtkLString: AnsiString(FParamValue.VAnsiString) := IntToStr(aValue); 
        mtkWString: WideString(FParamValue.VWideString) := IntToStr(aValue);
        mtkString:  
          begin 
            if Assigned(FParamValue.VString) then
              FParamValue.VString^ := IntToStr(aValue); 
          end;
      end;// case
end;

function TMeParam.GetAsInt64: Int64;
begin
  Result := 0;
  if Assigned(FDataType.ParamType) then
    if FDataType.IsByRef then
      case FDataType.ParamType.Kind of
        mtkSet, mtkEnumeration, mtkInteger: if FDataType.ParamType.ClassType = TypeOf(TMeCustomOrdinalType) then
          case PMeCustomOrdinalType(FDataType.ParamType).OrdType of
            otSByte:
                Result := PShortInt(FParamValue.VPointer)^;
            otUByte:
                Result := PByte(FParamValue.VPointer)^;
            otSWord:
                Result := PSmallInt(FParamValue.VPointer)^;
            otUWord:
                Result := PWord(FParamValue.VPointer)^;
            otSLong:
                Result := PInteger(FParamValue.VPointer)^;
            otULong:
                Result := PLongWord(FParamValue.VPointer)^;
          end;

        mtkPointer, mtkProcedure, mtkObject, mtkClass, mtkInterface, mtkWChar:
          Result := PInteger(FParamValue.VPointer)^;
        mtkChar:
          Result := PByte(FParamValue.VPointer)^;
        mtkVariant:
        begin
          Result := PVariant(FParamValue.VPointer)^;
        end;
        mtkInt64:   Result := PInt64(FParamValue.VPointer)^;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        //mtkMethod:     aValue := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: Result := Trunc(PSingle(FParamValue.VPointer)^);
            ftDouble: Result := Trunc(PDouble(FParamValue.VPointer)^);
            ftExtended: Result := Trunc(PExtended(FParamValue.VPointer)^);
            ftComp: Result := Trunc(PComp(FParamValue.VPointer)^);
            ftCurr: Result := Trunc(PCurrency(FParamValue.VPointer)^);
          end;
        mtkLString: Result := StrToInt(PAnsiString(FParamValue.VPointer)^);
        mtkWString: Result := StrToInt(PWideString(FParamValue.VPointer)^);
        mtkString:  Result := StrToInt(FParamValue.VString^);
      end// case
    else
      case FDataType.ParamType.Kind of
        mtkSet, mtkEnumeration, mtkInteger: if FDataType.ParamType.ClassType = TypeOf(TMeCustomOrdinalType) then
          case PMeCustomOrdinalType(FDataType.ParamType).OrdType of
            otSByte:
                Result := ShortInt(FParamValue.VByte);
            otUByte:
                Result := FParamValue.VByte;
            otSWord:
                Result := SmallInt(FParamValue.VWord);
            otUWord:
                Result := FParamValue.VWord;
            otSLong:
                Result := FParamValue.VInteger;
            otULong:
                Result := FParamValue.VLongWord;
          end;

        mtkPointer, mtkProcedure, mtkObject, mtkClass, mtkInterface, mtkWChar:
          Result := FParamValue.VInteger;
        mtkChar:
          Result := FParamValue.VByte;
        mtkVariant:
        begin
          Result := Variant(FParamValue.VVariant);
        end;
        mtkInt64:   Result := FParamValue.VInt64;
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        //mtkMethod:     aValue := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: Result := Trunc(FParamValue.VSingle);
            ftDouble: Result := Trunc(FParamValue.VDouble);
            ftExtended: Result := Trunc(FParamValue.VExtended);
            ftComp: Result := Trunc(FParamValue.VComp);
            ftCurr: Result := Trunc(FParamValue.VCurr);
          end;
        mtkLString: Result := StrToInt(AnsiString(FParamValue.VAnsiString));
        mtkWString: Result := StrToInt(WideString(FParamValue.VWideString));
        mtkString:  Result := StrToInt(FParamValue.VString^);
      end;// case
end;

procedure TMeParam.SetAsInt64(const aValue: Int64);
begin
  if Assigned(FDataType.ParamType) then
  begin
    if FDataType.IsByRef then
      case FDataType.ParamType.Kind of
        mtkVariant:
        begin
          PVariant(FParamValue.VPointer)^ := aValue;
        end;
        mtkInt64:   PInt64(FParamValue.VPointer)^ := aValue;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: PSingle(FParamValue.VPointer)^ := aValue;
            ftDouble: PDouble(FParamValue.VPointer)^ := aValue;
            ftExtended: PExtended(FParamValue.VPointer)^ := aValue;
            ftComp: PComp(FParamValue.VPointer)^ := aValue;
            ftCurr: PCurrency(FParamValue.VPointer)^ := aValue;
          end;
        mtkLString: PAnsiString(FParamValue.VAnsiString)^ := IntToStr(aValue);
        mtkWString: PWideString(FParamValue.VWideString)^ := IntToStr(aValue);
        mtkString:  
          begin 
            if not Assigned(FParamValue.VString) then
              New(FParamValue.VString);
            FParamValue.VString^ := IntToStr(aValue); 
          end;
      end// case
    else
      case FDataType.ParamType.Kind of
        mtkVariant:
        begin
          Variant(FParamValue.VVariant) := aValue;
        end;
        mtkInt64:   FParamValue.VInt64 := aValue;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: FParamValue.VSingle := aValue;
            ftDouble: FParamValue.VDouble := aValue;
            ftExtended: FParamValue.VExtended := aValue;
            ftComp: FParamValue.VComp := aValue;
            ftCurr: FParamValue.VCurr := aValue;
          end;
        mtkLString: AnsiString(FParamValue.VAnsiString) := IntToStr(aValue); 
        mtkWString: WideString(FParamValue.VWideString) := IntToStr(aValue); 
        mtkString:  
          begin 
            if Assigned(FParamValue.VString) then
              FParamValue.VString^ := IntToStr(aValue); 
          end;
      end;// case
  end;
end;

function TMeParam.GetAsString: String;
begin
  Result := '';
  if Assigned(FDataType.FParamType) then
    if FDataType.IsByRef then
      case FDataType.ParamType.Kind of
        mtkSet, mtkEnumeration, mtkInteger: if FDataType.ParamType.ClassType = TypeOf(TMeCustomOrdinalType) then
          case PMeCustomOrdinalType(FDataType.ParamType).OrdType of
            otSByte:
                Result := IntToStr(PShortInt(FParamValue.VPointer)^);
            otUByte:
                Result := IntToStr(PByte(FParamValue.VPointer)^);
            otSWord:
                Result := IntToStr(PSmallInt(FParamValue.VPointer)^);
            otUWord:
                Result := IntToStr(PWord(FParamValue.VPointer)^);
            otSLong:
                Result := IntToStr(PInteger(FParamValue.VPointer)^);
            otULong:
                Result := IntToStr(PLongWord(FParamValue.VPointer)^);
          end;

        mtkPointer, mtkProcedure, mtkObject, mtkClass, mtkInterface:
          Result := IntToStr(PInteger(FParamValue.VPointer)^);
        mtkChar:
          Result := PChar(FParamValue.VPointer)^;
        mtkWChar:
          Result := PWideChar(FParamValue.VPointer)^;
        mtkVariant:
        begin
          Result := PVariant(FParamValue.VPointer)^;
        end;
        mtkInt64:   Result := IntToStr(PInt64(FParamValue.VPointer)^);
        //mtkDynArray:   aValue := FParamValue.VDynArray;
        //mtkMethod:     aValue := FParamValue.VCode;
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: Result := FloatToStr(PSingle(FParamValue.VPointer)^);
            ftDouble: Result := FloatToStr(PDouble(FParamValue.VPointer)^);
            ftExtended: Result := FloatToStr(PExtended(FParamValue.VPointer)^);
            ftComp: Result := FloatToStr(PComp(FParamValue.VPointer)^);
            ftCurr: Result := FloatToStr(PCurrency(FParamValue.VPointer)^);
          end;
        mtkLString: Result := (PAnsiString(FParamValue.VPointer)^);
        mtkWString: Result := (PWideString(FParamValue.VPointer)^);
        mtkString:  Result := (FParamValue.VString^);
      end// case
    else
      case FDataType.ParamType.Kind of
        mtkPointer, mtkObject, mtkClass, mtkInterface: Result := IntToStr(Integer(FParamValue.VPointer));
        mtkLString: Result := PAnsiString(FParamValue.VAnsiString)^;
        mtkWString: Result := PWideString(FParamValue.VWideString)^;
        mtkString:  Result := FParamValue.VString^;
        mtkInteger:
        begin
          Result := IntToStr(FParamValue.VInteger);
        end;
        mtkSet:     
        begin
          case GetTypeSize(FDataType.ParamType) of
            2: Result := IntToStr(FParamValue.VWord);
            4: Result := IntToStr(FParamValue.VLongWord);
            else
              Result := IntToStr(FParamValue.VByte);
          end;
        end;
        mtkChar:    Result := FParamValue.VChar;
        mtkWChar:   Result := FParamValue.VWideChar;
        mtkVariant: Result := Variant(FParamValue.VVariant);
        mtkInt64:   Result := IntToStr(FParamValue.VInt64);
        //mtkDynArray:   Result := FParamValue.VDynArray;
        //mtkMethod:     Result := FParamValue.VCode;
        mtkProcedure:    Result := IntToStr(Integer(FParamValue.VCode));
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
        begin
         case PMeFloatType(FDataType.ParamType).FloatType of
           ftSingle: Result := FloatToStr(FParamValue.VSingle);
           ftDouble: Result := FloatToStr(FParamValue.VDouble);
           ftExtended: Result := FloatToStr(FParamValue.VExtended);
           ftComp: Result := FloatToStr(FParamValue.VComp);
           ftCurr: Result := FloatToStr(FParamValue.VCurr);
         end;
       end;
      end;// case
end;

procedure TMeParam.SetAsString(const aValue: String);
begin
  if Assigned(FDataType.ParamType) then
  begin
    if FDataType.IsByRef then
      case FDataType.ParamType.Kind of
        mtkVariant:
        begin
          PVariant(FParamValue.VPointer)^ := aValue;
        end;
        mtkInt64:   PInt64(FParamValue.VPointer)^ := StrToInt(aValue);
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: PSingle(FParamValue.VPointer)^ := StrToFloat(aValue);
            ftDouble: PDouble(FParamValue.VPointer)^ := StrToFloat(aValue);
            ftExtended: PExtended(FParamValue.VPointer)^ := StrToFloat(aValue);
            ftComp: PComp(FParamValue.VPointer)^ := StrToFloat(aValue);
            ftCurr: PCurrency(FParamValue.VPointer)^ := StrToFloat(aValue);
          end;
        mtkLString: PAnsiString(FParamValue.VAnsiString)^ := aValue;
        mtkWString: PWideString(FParamValue.VWideString)^ := aValue;
        mtkString:  
          begin 
            if not Assigned(FParamValue.VString) then
              New(FParamValue.VString);
            FParamValue.VString^ := aValue; 
          end;
      end// case
    else
      case FDataType.ParamType.Kind of
        mtkVariant:
        begin
          Variant(FParamValue.VVariant) := aValue;
        end;
        mtkInt64:   FParamValue.VInt64 := StrToInt(aValue);
        mtkFloat: if FDataType.ParamType.ClassType = TypeOf(TMeFloatType) then
          case PMeFloatType(FDataType.ParamType).FloatType of
            ftSingle: FParamValue.VSingle := StrToFloat(aValue);
            ftDouble: FParamValue.VDouble := StrToFloat(aValue);
            ftExtended: FParamValue.VExtended := StrToFloat(aValue);
            ftComp: FParamValue.VComp := StrToFloat(aValue);
            ftCurr: FParamValue.VCurr := StrToFloat(aValue);
          end;
        mtkLString: AnsiString(FParamValue.VAnsiString) := aValue; 
        mtkWString: WideString(FParamValue.VWideString) := aValue; 
        mtkString:  
          begin 
            if Assigned(FParamValue.VString) then
              FParamValue.VString^ := aValue; 
          end;
      end;// case
  end;
end;

function TMeParam.IsByRef: Boolean;
begin
  Result := FDataType.IsByRef;
end;

procedure TMeParam.LoadFromStream(const aStream: PMeStream);
var
  s: string;
begin
  With aStream^ do
  begin
    s := ReadString();
    AsString := s;
    s := ReadString();
    s := ReadString();
  end;
end;

procedure TMeParam.SaveToStream(const aStream: PMeStream; const aStoredParamName: Boolean; const aStoredParamType: Boolean);
begin
  With aStream^ do
  begin
    WriteString(AsString);
    if aStoredParamName then
    begin
      WriteString(Name);
    end
    else
      WriteString('');
    if aStoredParamType and Assigned(DataType.ParamType) then
    begin
      WriteString(DataType.GetParamTypeName);
    end
    else
      WriteString('');
  end;
end;

{ #### TMeParams #### }
destructor TMeParams.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMeParams.Clear;
begin
  FreeMeObjects;
  inherited Clear;
end;

{$IFDEF MeRTTI_EXT_SUPPORT}
function TMeParams.ParamByName(const Value: String): PMeParam;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := List^[i];
    if CompareText(Result.FDataType.FParamName, Value) = 0 then
      exit;
  end;
  Result := nil;
end;

function TMeParams.GetParamValue(const ParamName: String): Variant;
var
  vParam: PMeParam;
begin
  vParam := ParamByName(ParamName);
  if Assigned(vParam) then
    Result := vParam.Value
  else
    Result := NULL;
end;
{$ENDIF}

{ #### TMeCustomProcParams #### }
function TMeCustomProcParams.Add(aParam: PMeParam): integer;
begin
  Result := inherited Add(aParam);
  aParam.FDataType.FProcType :=  FProcType;
end;

function TMeCustomProcParams.Count: integer;
begin
  Result := inherited Count;
  if FProcType.IsMethod then Dec(Result);
  if FProcType.IsFunction then Dec(Result);
end;

function TMeCustomProcParams.Get(Index: Integer): PMeParam;
begin
  if FProcType.IsMethod then Inc(Index);
  Result := inherited Get(Index);
end;

procedure TMeCustomProcParams.Clear;
begin
  //FreeMeObjects;
  inherited Clear;
  FResultParam := nil;
  FSelfParam := nil;
end;

{ #### TMeProcParams #### }
procedure TMeProcParams.InitFromType(const aProcType: PMeProcType);
var
  i: Integer;
  vParam: PMeParam;
begin
  Clear;
  FProcType := aProcType;
  if Assigned(FProcType) then with aProcType^ do
  begin
    vParam := nil;
    if IsMethod then
    begin
      New(vParam, Create);
      Add(vParam);
      vParam.FDataType.FParamType := FSelfType;
      //vParam.FDataType.FProcType := aProcType;
      FSelfParam := vParam;
    end
    else
      FSelfParam := nil;
    for i := 0 to FParamList.Count - 1 do
    begin
      New(vParam, Create);
      Add(vParam);
      vParam.FDataType := PMeParamType(FParamList.List^[i])^;
    end;
    if IsFunction then
    begin
      if not RetOnStack then
      begin
        New(vParam, Create);
        Add(vParam);
        //vParam.FDataType.FProcType := aProcType;
        vParam.FDataType.FParamType := FResultType;
      end;
      FResultParam := vParam;
    end
    else
      FResultParam := nil;
  end;
end;

procedure TMeProcParams.AssignResult(aRegisters: PMeCommonRegs);
var
  vParam: PMeParam;
begin
  vParam := FResultParam;
  if Assigned(vParam) then
  begin
    if FProcType.RetInFPU then
    begin
      case PMeFloatType(FProcType.ResultType).FloatType of
        ftSingle:
         asm
           PUSH ECX
           MOV  ECX, vParam
           LEA  ECX, [ECX].TMeParam.FParamValue

           FSTP [ECX].TMeVarRec.VSingle //fetch from ST(0)
           POP  ECX
         end;
        ftDouble:
          asm
           PUSH ECX
           MOV  ECX, vParam
           LEA  ECX, [ECX].TMeParam.FParamValue

           FSTP [ECX].TMeVarRec.VDouble //fetch from ST(0)
           POP  ECX
          end;
        ftComp:
          asm
           PUSH ECX
           MOV  ECX, vParam
           LEA  ECX, [ECX].TMeParam.FParamValue

           FSTP [ECX].TMeVarRec.VComp //fetch from ST(0)
           POP  ECX
          end;
        ftCurr:
          asm
           PUSH ECX
           MOV  ECX, vParam
           LEA  ECX, [ECX].TMeParam.FParamValue

           FSTP [ECX].TMeVarRec.VCurr //fetch from ST(0)
           POP  ECX
          end;
        ftExtended: //(VExtended: Extended);
          asm
           PUSH ECX
           MOV  ECX, vParam
           LEA  ECX, [ECX].TMeParam.FParamValue

           FSTP [ECX].TMeVarRec.VExtended //fetch from ST(0)
           POP  ECX
          end;
      end; //case
    end
    else if not FProcType.RetOnStack then
    begin
      if GetStackTypeSize(FProcType.ResultType, FProcType.CallingConvention) <= SizeOf(Pointer) then
        FResultParam.FParamValue.VInteger := aRegisters.EAX
      else if FProcType.ResultType.Kind = mtkInt64 then
        begin
        //case FProcType.ResultType.Kind of
          //mtkInt64:
            FResultParam.FParamValue.VInt64 := aRegisters.aInt64;
          //mtkArray: ;//TODO: larger than Pointer size of the mtkRecord, mtkArray the additional var parameter be push to stack
        end; //case
    end;
  end
end;

function ReadPointerFromStack(var aLastParamPtr: Pointer; var aRegisters: PInteger; var vRegCount: Integer
  ; const cc: TCallingConvention
  ; const CanInReg: Boolean = True): Pointer;
begin
  case cc of
    ccRegister:
    begin
      if CanInReg and (vRegCount > 0) then
      begin
        Result := PPointer(aRegisters)^;
        Inc(Integer(aRegisters), SizeOf(Pointer));
        Dec(vRegCount);
      end
      else begin
        Result := PPointer(aLastParamPtr)^;
        Dec(Integer(aLastParamPtr), SizeOf(Pointer));
      end;
    end;
    ccPascal:
    begin
      Result := PPointer(aLastParamPtr)^;
      Dec(Integer(aLastParamPtr), SizeOf(Pointer));
    end;
    ccCdecl, ccStdCall, ccSafeCall:
    begin
      Result := PPointer(aLastParamPtr)^;
      Inc(Integer(aLastParamPtr), SizeOf(Pointer));
    end;
    ccFastCall:
    begin
      if CanInReg and (vRegCount > 0) then
      begin
        Result := PPointer(aRegisters)^;
        Dec(Integer(aRegisters), SizeOf(Pointer));
        Dec(vRegCount);
      end
      else begin
        Result := PPointer(aLastParamPtr)^;
        Inc(Integer(aLastParamPtr), SizeOf(Pointer));
      end;
    end;
  else
    Result := nil;
  end;// case
end;

procedure TMeProcParams.AssignFromStack(aLastParamPtr: Pointer; aRegisters: PInteger; vRegCount: Integer);
var
  i: integer;
  //for ccRegister calling convention
  //vRegCount: Integer;
begin
  if Assigned(FProcType) then with FProcType^ do
  begin
    if Assigned(aRegisters) and (vRegCount < 0) then
      vRegCount := 3;
    if (CallingConvention = ccFastCall) and Assigned(aRegisters) and (vRegCount < 0) then
    begin
      vRegCount := 2;
      //pointer to the ECX
      Inc(Integer(aRegisters), SizeOf(Pointer)*2);
    end;
    if CallingConvention in [ccRegister, ccPascal] then
      //move the aLastParamPtr to the first param address
      Integer(aLastParamPtr) := Integer(aLastParamPtr) + Max(0,GetStackTypeSizeIn(vRegCount)-SizeOf(Pointer));
    if IsMethod then
    begin
      PMeParam(List^[0]).AsPointer := ReadPointerFromStack(aLastParamPtr, aRegisters, vRegCount, CallingConvention);
    end; //if IsMethod
    for i := 0 to FParamList.Count - 1 do
    begin
      with PMeParamType(FParamList.List^[i])^ do
      if IsByRef or (StackTypeSize <= SizeOf(Pointer)) then
      begin
        Items[i].FParamValue.VPointer := ReadPointerFromStack(aLastParamPtr, aRegisters, vRegCount, CallingConvention);
      end
      else 
        case Items[i].FDataType.Kind of
          mtkDynArray: 
            begin
              Items[i].FParamValue.VDynBound := Integer(ReadPointerFromStack(aLastParamPtr, aRegisters, vRegCount, CallingConvention));
              Items[i].FParamValue.VDynArray := ReadPointerFromStack(aLastParamPtr, aRegisters, vRegCount, CallingConvention);
            end;
        end;//case
    end; //for
    {if Assigned(FResultParam) and not RetOnStack then
    begin
      Assert((FParamList.Count=0) or (FParamList.Count = (Self.Count -2)), 'AssignFromStack.ResultType Count Mismatch.');
      if ResultType.Kind = mtkInt64 then
      begin
        FResultParam.FParamValue.VInt64 := PMeCommonRegs(aRegisters).aInt64;
      end
      else if (uMeTypes.GetStackTypeSize(ResultType, CallingConvention) <= SizeOf(Pointer)) then
      begin
        FResultParam.FParamValue.VInteger := PMeCommonRegs(aRegisters).EAX;
      end;
    end; //if IsFunction}
  end; //if Assigned(FProcType)
end;

procedure WritePointerToStack(var aLastParamPtr: Pointer; var aRegisters: PInteger; const aValue: Pointer
  ; var vRegCount: Integer
  ; const cc: TCallingConvention
  ; const CanInReg: Boolean = True);
begin
  case cc of
    ccRegister:
    begin
      if CanInReg and (vRegCount > 0) then
      begin
        PPointer(aRegisters)^ := aValue;
        Inc(Integer(aRegisters), SizeOf(Pointer));
        Dec(vRegCount);
      end
      else begin
        PPointer(aLastParamPtr)^ := aValue;
        Dec(Integer(aLastParamPtr), SizeOf(Pointer));
      end;
    end;
    ccPascal:
    begin
      PPointer(aLastParamPtr)^ := aValue;
      Dec(Integer(aLastParamPtr), SizeOf(Pointer));
    end;
    ccCdecl, ccStdCall, ccSafeCall:
    begin
      PPointer(aLastParamPtr)^ := aValue;
      Inc(Integer(aLastParamPtr), SizeOf(Pointer));
    end;
  end;// case
end;

procedure TMeProcParams.SaveResult(aRegisters: PInteger);
var
  vParam: PMeParam;
begin
    if Assigned(FResultParam) then with FProcType^ do
    begin
      //Assert((FParamList.Count = 0) or (FParamList.Count = (Self.Count -2)), 'SaveToStack.ResultType Count Mismatch.');
      vParam := FResultParam;
      if RetInFPU then
      begin
        case PMeFloatType(ResultType).FloatType of
          ftSingle:
           asm
             PUSH EDX
             MOV  EDX, vParam
             LEA  EDX, [EDX].TMeParam.FParamValue

             FLD  [EDX].TMeVarRec.VSingle //push to ST(0)
             POP  EDX
           end;
          ftDouble:
            asm
             PUSH EDX
             MOV  EDX, vParam
             LEA  EDX, [EDX].TMeParam.FParamValue

             FLD  [EDX].TMeVarRec.VDouble //push to ST(0)
             POP  EDX
            end;
          ftComp:
            asm
             PUSH EDX
             MOV  EDX, vParam
             LEA  EDX, [EDX].TMeParam.FParamValue

             FLD  [EDX].TMeVarRec.VComp //push to ST(0)
             POP  EDX
            end;
          ftCurr:
            asm
             PUSH EDX
             MOV  EDX, vParam
             LEA  EDX, [EDX].TMeParam.FParamValue

             FLD  [EDX].TMeVarRec.VCurr //push to ST(0)
             POP  EDX
            end;
          ftExtended: //(VExtended: Extended);
            asm
             PUSH EDX
             MOV  EDX, vParam
             LEA  EDX, [EDX].TMeParam.FParamValue

             FLD  [EDX].TMeVarRec.VExtended //push to ST(0)
             POP  EDX
            end;
        end; //case
      end
      else if not RetOnStack then
      begin
        if ResultType.Kind = mtkInt64 then
        begin
          PMeCommonRegs(aRegisters).aInt64 := vParam.AsInt64;
        end
        else if (uMeTypes.GetStackTypeSize(ResultType, CallingConvention) <= SizeOf(Pointer)) then
          PMeCommonRegs(aRegisters).EAX := vParam.FParamValue.VInteger;
      end;
    end; //if IsFunction
end;

procedure TMeProcParams.SaveToStack(aLastParamPtr: Pointer; aRegisters: PInteger);
var
  i: integer;
  //for ccRegister calling convention
  vRegCount: Integer;
  //vParam: PMeParam;
begin
  if Assigned(FProcType) then with FProcType^ do
  begin
    vRegCount := 3;

    if CallingConvention = ccFastCall then
    begin
      vRegCount := 2;
      //pointer to the ECX
      Inc(Integer(aRegisters), SizeOf(Pointer)*2);
    end;
    if CallingConvention in [ccRegister, ccPascal] then
      //move the aLastParamPtr to the first param address
      Integer(aLastParamPtr) := Integer(aLastParamPtr) + Max(0, GetStackTypeSize-SizeOf(Pointer));
    if IsMethod then
    begin
      WritePointerToStack(aLastParamPtr, aRegisters, PMeParam(List^[0]).FParamValue.VPointer, vRegCount, CallingConvention);
    end; //if IsMethod
    for i := 0 to FParamList.Count - 1 do
    begin
      with PMeParamType(FParamList.List^[i])^ do
      if IsByRef or (StackTypeSize <= SizeOf(Pointer)) then
      begin
        WritePointerToStack(aLastParamPtr, aRegisters, Items[i].FParamValue.VPointer, vRegCount, CallingConvention);
      end
      else 
        case Items[i].FDataType.Kind of
          mtkDynArray: 
            begin
              WritePointerToStack(aLastParamPtr, aRegisters, Pointer(Items[i].FParamValue.VDynBound), vRegCount, CallingConvention);
              WritePointerToStack(aLastParamPtr, aRegisters, Items[i].FParamValue.VDynArray, vRegCount, CallingConvention);
            end;
        end;//case
    end; //for
  end; //if Assigned(FProcType)
end;

procedure TMeProcParams.Execute(const aProc: Pointer);
var
  { vRegisters^ = EAX; PInteger(vRegisters + 4)^ = EDX;  PInteger(vRegisters + 8)^ = ECX}
  vRegisters: TMeCommonRegs;
  vLastParamPtr: Pointer; //the parameter address in the stack.
  vSize: Integer;
  vCallingConvention: TCallingConvention;
begin
  vCallingConvention := FProcType.CallingConvention;
  vSize := FProcType.GetStackTypeSize;
  asm
    //alloc parameter stack space
    SUB  ESP, vSize
    MOV  vLastParamPtr, ESP
  end;
  SaveToStack(vLastParamPtr, @vRegisters);
  asm
    CMP   vCallingConvention, ccRegister
    JNZ   @DoFastCall
  @DoRegisterCall:
    MOV   EAX, vRegisters.&EAX
    MOV   EDX, vRegisters.&EDX
    MOV   ECX, vRegisters.&ECX
    JMP   @DoCallProc
  @DoFastCall:
    CMP   vCallingConvention, ccFastCall
    JNZ   @DoCallProc
    MOV   EDX, vRegisters.&EDX
    MOV   ECX, vRegisters.&ECX
  @DoCallProc:
    CALL  aProc
    MOV   vRegisters.&EAX, EAX
    MOV   vRegisters.&EDX, EDX
    MOV   vRegisters.&ECX, ECX
  end;
  AssignResult(@vRegisters);
end;

{$IFDEF MeRTTI_SUPPORT}
const
  cMeProcTypeClassName: PChar = 'TMeProcType';
{$ENDIF}

initialization
  SetMeVirtualMethod(TypeOf(TMeProcType), ovtVmtParent, TypeOf(TMeType));
  SetMeVirtualMethod(TypeOf(TMeTypes), ovtVmtParent, TypeOf(TMeRegisteredTypes));
  SetMeVirtualMethod(TypeOf(TMeParams), ovtVmtParent, TypeOf(TMeList));
  SetMeVirtualMethod(TypeOf(TMeCustomProcParams), ovtVmtParent, TypeOf(TMeParams));
  SetMeVirtualMethod(TypeOf(TMeProcParams), ovtVmtParent, TypeOf(TMeCustomProcParams));


  {$IFDEF MeRTTI_SUPPORT}
  //Make the ovtVmtClassName point to PShortString class name
  SetMeVirtualMethod(TypeOf(TMeProcType), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeTypes), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeParams), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeCustomProcParams), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeProcParams), ovtVmtClassName, nil);
  {$ENDIF}

  GRegisteredMeTypesClass := TypeOf(TMeTypes);
finalization
end.
