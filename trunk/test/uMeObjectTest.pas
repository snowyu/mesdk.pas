unit uMeObjectTest;

{$I MeSetting.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, //QueryPerformanceCounter
  {$ENDIF}
  {$IFDEF DEBUG}
  DbugIntf,
  {$ENDIF}
  Classes,
  SysUtils,
  Math,
  TypInfo
  {$IFDEF FPC}
  , fpcunit
  , testregistry
  {$ENDIF}
  {$IFDEF Borland}
  , TestFramework
  {$ENDIF}
  , uMeObject
  , uInternalTestObj
  ;


type
  TTest_MeHelperFuncs = class (TTestCase)
  protected
    procedure CheckClassName(const aObj: PMeDynamicObject; const aClassName: string);
  published
    procedure Test_MeTypeOf;
    procedure Test_MeSizeOf;
  end;

  TTest_MeObject = class (TTestCase)
  protected
    FMeObject: PMeDynamicObject;
    FObj: PMeDynamicObject;
  protected
    procedure CreateMeObject;virtual;
    {$IFDEF MeRTTI_SUPPORT}
    procedure CheckObjectClassName;virtual;
    {$ENDIF}
    procedure Setup;override;
    procedure TearDown;override;
  public
  published
    procedure Test_VMT;
    procedure Test_InstanceSize;
    procedure Test_IsObject;
    procedure Test_InheritsFrom;virtual;
    {$IFDEF MeRTTI_EXT_SUPPORT}
    //procedure Test_Name;
    {$ENDIF}
  end;

  TTest_MeList = class (TTest_MeObject)
  protected
    procedure CreateMeObject;override;
    {$IFDEF MeRTTI_SUPPORT}
    procedure CheckObjectClassName;override;
    {$ENDIF}
    procedure TestEmptyList;
    procedure TestClear;
    procedure TestAdd;
  published
    procedure Test_Add;
    procedure Test_Popup;
    procedure Test_InheritsFrom;override;
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    procedure Test_AddItems;
    {$ENDIF}
  end;

  TTest_MeStrings = class (TTest_MeObject)
  protected
    procedure CreateMeObject;override;
    {$IFDEF MeRTTI_SUPPORT}
    procedure CheckObjectClassName;override;
    {$ENDIF}
    procedure TestEmptyList;
    procedure TestClear;
    procedure TestAdd;
    procedure TestAddNamedPair;
  published
    procedure Test_Add;
    procedure Test_ValueNamePair;
    procedure Test_InheritsFrom;override;
  end;

  TTest_MeDynamicMemory = class (TTest_MeObject)
  protected
    procedure CreateMeObject;override;
    {$IFDEF MeRTTI_SUPPORT}
    procedure CheckObjectClassName;override;
    {$ENDIF}
    procedure TestEmptyList;
    procedure TestClear;
    procedure TestAdd;
  published
    procedure Test_Add;
    procedure Test_Assign;
    procedure Test_InheritsFrom;override;
  end;

const
  cTestSize = 100;

var
  aIntList: array [0..cTestSize - 1] of Integer;
  aDoubleList: array [0..cTestSize - 1] of Double;
  aPtrList: array [0..cTestSize - 1] of Pointer;
  aRandomIntList: array [0..cTestSize - 1] of Integer;
  aStrList: array [0..cTestSize - 1] of string;
  aNamedStrList: array [0..cTestSize - 1] of string;
  aRandomStrList: array [0..cTestSize - 1] of string;

implementation

Procedure ShuffleIntArray(var aAry: array of Integer);
var
  v: integer;
  i,j,
  L, iBegin, iEnd: integer;
Begin
  iBegin := Low(aAry);
  iEnd := High(aAry);
  i := iBegin;
  L := Length(aAry);
  While (i <= iEnd) Do
  Begin
    j := iBegin + Random(L);
    v := aAry[i];
    aAry[i] := aAry[j];
    aAry[j] := v;
    inc(i);
  End; // While
End;

Procedure ShuffleStrArray(var aAry: array of string);
var
  v: string;
  i,j,
  L, iBegin, iEnd: integer;
Begin
  iBegin := Low(aAry);
  iEnd := High(aAry);
  i := iBegin;
  L := Length(aAry);
  While (i <= iEnd) Do
  Begin
    j := iBegin + Random(L);
    v := aAry[i];
    aAry[i] := aAry[j];
    aAry[j] := v;
    inc(i);
  End; // While
End;

procedure TTest_MeObject.CreateMeObject;
begin
  FMeObject := New(PMeDynamicObject, Create);
{$IFDEF MeRTTI_SUPPORT}
  CheckObjectClassName;
{$ENDIF}
end;

{$IFDEF MeRTTI_SUPPORT}
procedure TTest_MeObject.CheckObjectClassName;
begin
  CheckEquals('TMeDynamicObject', FMeObject.ClassName, 'the MeObject classname is mismatch.');
  
  //CheckEquals(Integer(FMeObject), Integer(FindMeObject('TMeDynamicObject')), 'the FindMeObject is Error.');
end;
{$ENDIF}

procedure TTest_MeObject.Setup;
begin
  if not Assigned(FMeObject) then
  begin
    CreateMeObject;
    if Assigned(FMeObject) then
    begin
      FObj:= NewMeObject(FMeObject.ClassType);
      try
        CheckEquals(FMeObject.ClassName, FObj.ClassName, 'NewMeObject: the MeObject classname is mismatch.');
        CheckEquals(Integer(FMeObject.ClassType), Integer(FObj.ClassType), 'NewMeObject: the MeObject ClassType is mismatch.');
      finally
        MeFreeAndNil(FObj);
        CheckEquals(0, Integer(FObj), 'the FObj can not be free.');
      end;
    end;
  end;
end;

procedure TTest_MeObject.TearDown;
{$IFDEF MeRTTI_EXT_SUPPORT}
var
  vCount: Integer;
{$ENDIF}
begin
{$IFDEF MeRTTI_SUPPORT}
  CheckObjectClassName;
  //vCount := GObjectNameList.Count;
  //Check(vCount > 0, 'the GObjectNameList.Count should greater than 0.');
{$ENDIF}
  MeFreeAndNil(FMeObject);
  CheckEquals(0, Integer(FMeObject), 'the MeObject can not be free.');
{$IFDEF MeRTTI_EXT_SUPPORT}
  //CheckEquals(vCount-1, GObjectNameList.Count, 'the GObjectNameList.Count decrease 1.');
{$ENDIF}
end;

procedure TTest_MeObject.Test_VMT;
var
  i: Integer;
begin
  CheckEquals(Integer(TypeOf(FMeObject^)), Integer(FMeObject.ClassType), 'the VMT Address is mismatch.');
  i := PInteger(Integer(FMeObject.ClassType) + ovtVMTAddress)^;
  CheckEquals(Integer(FMeObject.ClassType), i, 'the VMT Offset address is mismatch.');
  i := PInteger(Integer(FMeObject.ClassType) + ovtInstanceSize)^;
  CheckEquals(SizeOf(FMeObject^), i, 'the VMT instance Size address is mismatch.');
end;

procedure TTest_MeObject.Test_InheritsFrom;
begin
  if FMeObject.ClassType <> TypeOf(TMeDynamicObject) then
  begin
    CheckEquals(True, FMeObject.InheritsFrom(TypeOf(TMeDynamicObject)), 'the object should Inherit From TMeDynamicObject.');
    CheckEquals(True, MeInheritsFrom(FMeObject.ClassType, TypeOf(TMeDynamicObject)), 'MeInheritsFrom: the object should Inherit From TMeDynamicObject.');
  end;
end;

procedure TTest_MeObject.Test_InstanceSize;
begin
  CheckEquals(SizeOf(FMeObject^), Integer(FMeObject.InstanceSize), 'the InstanceSize is mismatch.');
end;

{procedure TTest_MeObject.Test_Name;
begin
  CheckObjectClassName;
end;
}
procedure TTest_MeObject.Test_IsObject;
begin
  CheckEquals(True, FMeObject.IsObject(FMeObject), 'the FMeObject.IsObject(FMeObject) should be true.');
  CheckEquals(False, FMeObject.IsObject(nil), 'the FMeObject.IsObject(FMeObject) should be false.');
end;

procedure TTest_MeList.CreateMeObject;
begin
  //FMeObject := New(PMeList, Create);
  FMeObject := NewList;
  TestEmptyList;
{$IFDEF MeRTTI_SUPPORT}
  //PMeList(FMeObject)^.Name := 'TMeList';
  CheckObjectClassName;
{$ENDIF}
end;

{$IFDEF MeRTTI_SUPPORT}
procedure TTest_MeList.CheckObjectClassName;
begin
  CheckEquals('TMeList', FMeObject.ClassName, 'the TMeList ClassName is mismatch.');
  
  //CheckEquals(Integer(FMeObject), Integer(FindMeObject('TMeList')), 'the FindMeObject(TMeList) is Error.');
end;
{$ENDIF}

procedure TTest_MeList.TestAdd;
var 
  i: Integer;
  j,k: Integer;
begin
  with PMeList(FMeObject)^ do
  begin
    k := Count;
    for i := Low(aIntList) to High(aIntList) do
    begin
      j := Add(Pointer(aIntList[i]));
      CheckEquals(k, j, 'the Added MeList.index is error.');
      Inc(k);
    end;
    CheckEquals(k, Count, 'the Added MeList.Count is error.');
    k := k - (High(aIntList) - Low(aIntList) + 1);
    for i := Low(aIntList) to High(aIntList) do
    begin
      CheckEquals(aIntList[i], Integer(Items[k+i]), IntToStr(k)+' the MeList['+IntToStr(k+i)+'].value is error.');
    end;
  end;
end;

procedure TTest_MeList.Test_Add;
begin
  TestClear;
  TestAdd;
  TestClear;
end;

procedure TTest_MeList.Test_Popup;
var
  i, j: Integer;
  v: Integer;
begin
  TestClear;
  TestAdd;
  with PMeList(FMeObject)^ do
  begin
    j := Count;
    for i := 0 to Count - 1 do
    begin
      v := Integer(Items[Count-1]);
      CheckEquals(v, Integer(Popup()), 'the MeList.Popup() value mismatch.');
      CheckEquals(j-1, Count, 'the MeList.Popup() should decrease the count.');
      Dec(j);
    end;
    CheckEquals(0, Count, 'the MeList.Popup() Count should empty now.');
  end;
  TestClear;
end;

procedure TTest_MeList.TestEmptyList;
begin
  with PMeList(FMeObject)^ do
  begin
    CheckEquals(0, Integer(List), 'the list should be nil.');
    CheckEquals(0, Count, 'the list count should be 0.');
    CheckEquals(0, Capacity, 'the list Capacity should be 0.');
  end;
end;

procedure TTest_MeList.Test_InheritsFrom;
begin
  Inherited;
  CheckEquals(True, FMeObject.InheritsFrom(TypeOf(TMeContainer)), 'the object should Inherit From TMeContainer.');
  CheckEquals(True, MeInheritsFrom(FMeObject.ClassType, TypeOf(TMeContainer)), 'MeInheritsFrom: the object should Inherit From TMeContainer.');
end;


procedure TTest_MeList.TestClear;
begin
  with PMeList(FMeObject)^ do
  begin
    Clear;
  end;
  TestEmptyList;
end;

{$IFDEF SUPPORTS_DYNAMICARRAYS}
procedure TTest_MeList.Test_AddItems;
var 
  i: Integer;
  j: Integer;
begin
  TestClear;
  with PMeList(FMeObject)^ do
  begin
    AddItems(aPtrList);
    CheckEquals(Length(aPtrList), Count, 'the Added items MeList.Count is error.');
    for i := 0 to High(aPtrList) do
    begin
      CheckEquals(Integer(aPtrList[i]), Integer(Items[i]), 'the MeList['+IntToStr(i)+'].value is error.');
    end;
  end;
  TestClear;
end;
{$ENDIF}

procedure TTest_MeStrings.CreateMeObject;
begin
  //FMeObject := New(PMeList, Create);
  FMeObject := NewStrings;
  TestEmptyList;
{$IFDEF MeRTTI_SUPPORT}
  //PMeList(FMeObject)^.Name := 'TMeList';
  CheckObjectClassName;
{$ENDIF}
end;

{$IFDEF MeRTTI_SUPPORT}
procedure TTest_MeStrings.CheckObjectClassName;
begin
  CheckEquals('TMeStrings', FMeObject.ClassName, 'the TMeStrings ClassName is mismatch.');
end;
{$ENDIF}

procedure TTest_MeStrings.TestEmptyList;
begin
  with PMeStrings(FMeObject)^ do
  begin
    CheckEquals(0, Count, 'the list count should be 0.');
    CheckEquals('', Last, 'the list last property should be "".');
    CheckEquals('', Text, 'the list Text  property should be "".');
  end;
end;

procedure TTest_MeStrings.TestClear;
begin
  with PMeStrings(FMeObject)^ do
  begin
    Clear;
  end;
  TestEmptyList;
end;

procedure TTest_MeStrings.TestAddNamedPair;
var 
  i: Integer;
  j,k: Integer;
begin
  with PMeStrings(FMeObject)^ do
  begin
    k := Count;
    for i := Low(aNamedStrList) to High(aNamedStrList) do
    begin
      j := Add(aNamedStrList[i]);
      CheckEquals(k, j, 'the Added MeStrings.index is error.');
      Objects[i] := i;
      Inc(k);
    end;
    CheckEquals(k, Count, 'the Added MeStrings.Count is error.');
    k := k - (High(aNamedStrList) - Low(aNamedStrList) + 1);
    for i := Low(aNamedStrList) to High(aNamedStrList) do
    begin
      CheckEquals(aNamedStrList[i], Items[k+i], 'the MeStrings['+IntToStr(k+i)+'].value is error.');
      CheckEquals(i, Objects[k+i], 'the MeStrings.Objects['+IntToStr(k+i)+'].value is error.');
      CheckEquals(StrToInt(GetValueByIndex(k+i)), Objects[k+i], 'the MeStrings.Objects['+IntToStr(k+i)+'].GetValueByIndex() is error.');
      CheckEquals(aStrList[i], Names[k+i], 'the MeStrings['+IntToStr(k+i)+'].Names[] is error.');
      CheckEquals(StrToInt(Values[PChar(Names[k+i])]), Objects[k+i], 'the MeStrings.Objects['+IntToStr(k+i)+'].Values[] is error.');
    end;
  end;
end;

procedure TTest_MeStrings.TestAdd;
var 
  i: Integer;
  j,k: Integer;
begin
  with PMeStrings(FMeObject)^ do
  begin
    k := Count;
    for i := Low(aStrList) to High(aStrList) do
    begin
      j := Add(aStrList[i]);
      CheckEquals(k, j, 'the Added MeStrings.index is error.');
      Objects[i] := i;
      Inc(k);
    end;
    CheckEquals(k, Count, 'the Added MeStrings.Count is error.');
    k := k - (High(aStrList) - Low(aStrList) + 1);
    for i := Low(aStrList) to High(aStrList) do
    begin
      CheckEquals(aStrList[i], Items[k+i], 'the MeStrings['+IntToStr(k+i)+'].value is error.');
      CheckEquals(i, Objects[k+i], 'the MeStrings.Objects['+IntToStr(k+i)+'].value is error.');
    end;
  end;
end;

procedure TTest_MeStrings.Test_Add;
begin
  TestClear;
  TestAdd;
  TestClear;
end;

procedure TTest_MeStrings.Test_InheritsFrom;
begin
  Inherited;
  CheckEquals(True, FMeObject.InheritsFrom(TypeOf(TMeContainer)), 'the object should Inherit From TMeContainer.');
end;

procedure TTest_MeStrings.Test_ValueNamePair;
begin
  TestClear;
  TestAddNamedPair;
end;

{ TTest_MeDynamicMemory }
procedure TTest_MeDynamicMemory.CreateMeObject;
begin
  //FMeObject := New(PMeList, Create);
  FMeObject := NewMeObject(TypeOf(TMeDynamicMemory));
  TestEmptyList;
{$IFDEF MeRTTI_SUPPORT}
  //PMeList(FMeObject)^.Name := 'TMeList';
  CheckObjectClassName;
{$ENDIF}
end;

{$IFDEF MeRTTI_SUPPORT}
procedure TTest_MeDynamicMemory.CheckObjectClassName;
begin
  CheckEquals('TMeDynamicMemory', FMeObject.ClassName, 'the TMeDynamicMemory ClassName is mismatch.');
end;
{$ENDIF}

procedure TTest_MeDynamicMemory.Test_InheritsFrom;
begin
  Inherited;
  CheckEquals(True, FMeObject.InheritsFrom(TypeOf(TMeDynamicMemory)), 'the object should Inherit From TMeDynamicMemory.');
end;

procedure TTest_MeDynamicMemory.TestEmptyList;
begin
  with PMeDynamicMemory(FMeObject)^ do
  begin
    CheckEquals(0, UsedSize, 'the list UsedSize should be 0.');
  end;
end;

procedure TTest_MeDynamicMemory.TestClear;
begin
  with PMeDynamicMemory(FMeObject)^ do
  begin
    Clear;
  end;
  TestEmptyList;
end;

procedure TTest_MeDynamicMemory.TestAdd;
var
 p: pointer;
 i: integer;
 d: Double;
 vSize: integer;
 s: string;
begin
  with PMeDynamicMemory(FMeObject)^ do
  begin
    i := Random(MaxInt);
    p := Pointer(UsedSize);
    AddInt(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := SizeOf(Integer);
    CheckEquals(vSize, UsedSize, 'the addint UsedSize is mismatch.');
    CheckEquals(i, PInteger(p)^, 'the addint value is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    i := Random($FF);
    p := Pointer(UsedSize);
    AddByte(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Byte);
    CheckEquals(vSize, UsedSize, 'the addbyte UsedSize is mismatch.');
    CheckEquals(i, PByte(p)^, 'the addbyte value is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    Align; //align by dword border.
    vSize := vSize + SizeOf(Integer) - SizeOf(Byte);
    CheckEquals(vSize, UsedSize, 'the Align is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    i := Random($FFFF);
    p := Pointer(UsedSize);
    AddWord(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Word);
    CheckEquals(vSize, UsedSize, 'the addword UsedSize is mismatch.');
    CheckEquals(i, PWord(p)^, 'the addword value is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    i := Random($FF);
    p := Pointer(UsedSize);
    AddByte(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Byte);
    CheckEquals(vSize, UsedSize, 'the addbyte UsedSize is mismatch.');
    CheckEquals(i, PByte(p)^, 'the addbyte value is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    d := Random;
    p := Pointer(UsedSize);
    AddDouble(d);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Double);
    CheckEquals(vSize, UsedSize, 'the addDouble UsedSize is mismatch.');
    CheckEquals(d, PDouble(p)^, 'the addDouble value is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    d := Random;
    p := Pointer(UsedSize);
    AddDouble(d);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Double);
    CheckEquals(vSize, UsedSize, 'the addDouble UsedSize is mismatch.');
    CheckEquals(d, PDouble(p)^, 'the addDouble value is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    d := Random;
    p := Pointer(UsedSize);
    AddDouble(d);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + SizeOf(Double);
    CheckEquals(vSize, UsedSize, 'the addDouble UsedSize is mismatch.');
    CheckEquals(d, PDouble(p)^, 'the addDouble value is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    s := 'Hello ²âÊÔ×Ö·û´®PCharÄÚ´æ·ÖÅä Tao Is Nothing!!!';
    p := Pointer(UsedSize);
    AddPChar(s);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + Length(s) + 1;
    CheckEquals(vSize, UsedSize, 'the addPChar UsedSize is mismatch.');
    CheckEquals(s, PChar(p), 'the addPchar value is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    s := 'Hello ²âÊÔBuffer ÄÚ´æ·ÖÅä Tao Is Buffer Nothing!!!';
    p := Pointer(UsedSize);
    AddBuffer(s[1], Length(s));
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + Length(s);
    CheckEquals(vSize, UsedSize, 'the addbuffer UsedSize is mismatch.');
    {$IFDEF FPC}
    Check(CompareMem(@s[1], p, Length(s)), 'the addbuffer value is mismatch.');
    {$ELSE}
    CheckEqualsMem(@s[1], p, Length(s), 'the addbuffer value is mismatch.');
    {$ENDIF}
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

    i := Random($FFF) + 10;
    p := Pointer(UsedSize);
    AllocSpace(i);
    p := Pointer(Integer(Memory) + Integer(p));
    vSize := vSize + i;
    CheckEquals(vSize, UsedSize, 'the AllocSpace UsedSize is mismatch.');
    Check(Size>=UsedSize, 'the Size should greater or equ UsedSize.');

  end;
end;

procedure TTest_MeDynamicMemory.Test_Add;
begin
  TestClear;
  TestAdd;
  TestClear;
end;

procedure TTest_MeDynamicMemory.Test_Assign;
var
  vObj: PMeDynamicMemory;
begin
  TestClear;
  TestAdd;
  New(vObj, Create);
  try
    vObj.Assign(PMeDynamicMemory(FMeObject));
    with PMeDynamicMemory(FMeObject)^ do
    begin
      CheckEquals(UsedSize, vObj.UsedSize, 'the UsedSize is mismatch.');
      CheckEquals(Size, vObj.Size, 'the Size is mismatch.');
      {$IFDEF FPC}
      Check(CompareMem(Memory, vObj.Memory, UsedSize), 'the Memory content is mismatch.');
      {$ELSE}
      CheckEqualsMem(Memory, vObj.Memory, UsedSize, 'the Memory content is mismatch.');
      {$ENDIF}
    end;
    
  finally
    vObj.Free;
  end;
  TestClear;
end;

{ TTest_MeHelperFuncs }
procedure TTest_MeHelperFuncs.CheckClassName(const aObj: PMeDynamicObject; const aClassName: string);
var
  vC: TMeClass;
begin
  {$IFDEF MeRTTI_SUPPORT}
  vC := MeTypeOf(aObj^);
  CheckEquals(aClassName, vC.ClassName, 'the ClassName is error.');
  {$ENDIF}
end;

procedure TTest_MeHelperFuncs.Test_MeTypeOf;
var
  vObj: PMeDynamicObject;
begin
  vObj := New(PMeDynamicObject, Create);
  try
    CheckEquals(Integer(TypeOf(TMeDynamicObject)), Integer(MeTypeOf(vObj^)), 'the TMeDynamicObject MeTypeOf is error.');
    CheckEquals(Integer(TypeOf(TMeDynamicObject)), Integer(vObj^), 'the TMeDynamicObject TypeOf is error.');
  finally
    MeFreeAndNil(vObj);
  end;

  vObj := New(PMeList, Create);
  try
    CheckEquals(Integer(TypeOf(TMeList)), Integer(MeTypeOf(vObj^)), 'the TMeList MeTypeOf is error.');
    CheckEquals(Integer(TypeOf(TMeList)), Integer(vObj^), 'the TMeList TypeOf is error.');
  finally
    MeFreeAndNil(vObj);
  end;
end;

procedure TTest_MeHelperFuncs.Test_MeSizeOf;
var
  vObj: PMeDynamicObject;
begin
   New(vObj, Create);
  try
    CheckEquals(SizeOf(TMeDynamicObject), MeSizeOf(vObj^), 'the TMeDynamicObject MeSizeOf is error.');
    CheckClassName(vObj, 'TMeDynamicObject');
  finally
    MeFreeAndNil(vObj);
  end;

  vObj := New(PMeList, Create);
  try
    CheckEquals(SizeOf(TMeList), MeSizeOf(vObj^), 'the TMeList MeSizeOf is error.');
    CheckClassName(vObj, 'TMeList');
  finally
    MeFreeAndNil(vObj);
  end;

  vObj := New(PMeStrings, Create);
  try
    CheckEquals(SizeOf(TMeStrings), MeSizeOf(vObj^), 'the TMeStrings MeSizeOf is error.');
    CheckClassName(vObj, 'TMeStrings');
  finally
    MeFreeAndNil(vObj);
  end;
end;


procedure InitList;
var
  i: integer;
begin
  for i := 0 to cTestSize -1 do
  begin
    aIntList[i] := i;
    aRandomIntList[i] := i;
    aDoubleList[i] := i;
    aPtrList[i] := Pointer(i);
    aStrList[i] := 'Item '+ Format('%4d', [i]);
    aNamedStrList[i] :=aStrList[i] + '=' + IntToStr(i);
    aRandomStrList[i] := aStrList[i];
  end;

  ShuffleIntArray(aRandomIntList);
  ShuffleStrArray(aRandomStrList);
end;

Initialization
  Randomize;
  InitList;
  RegisterTests({$IFDEF Borland}'MeObjects suites',{$ENDIF}
                [
                 TTest_MeObject{$IFDEF Borland}.Suite{$ENDIF}
                 , TTest_MeHelperFuncs{$IFDEF Borland}.Suite{$ENDIF}
                 , TTest_MeList{$IFDEF Borland}.Suite{$ENDIF}
                 , TTest_MeStrings{$IFDEF Borland}.Suite{$ENDIF}
                 , TTest_MeDynamicMemory{$IFDEF Borland}.Suite{$ENDIF}
                ]);//}
finalization
end.
