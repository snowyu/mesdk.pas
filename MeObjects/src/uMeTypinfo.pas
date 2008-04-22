
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.32 $

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
    * The Original Code is $RCSfile: uMeTypinfo.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Andreas Hausladen is Copyright (C) 2005
    * Portions created by Riceball LEE is Copyright (C) 2006-2008
    * All rights reserved.
    * Contributor(s):
    *  Hallvard Vassbotn
    *  Jcl(JclSysUtils) http://jvcl.sourceforge.net
    *  FastCode Project
}
unit uMeTypInfo;

{$I MeSetting.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, TypInfo
  , uMeConsts
  //, uMeDisAsmEngine
  , uMeSystem
  ;


type
  //for property read/write/stored method
  TBooleanFuncType = function():Boolean of object;
  TBooleanIndexFuncType = function(Index: Integer):Boolean of object;
  TBooleanProcType = procedure(Value: Boolean) of object;
  TBooleanIndexProcType = procedure(Index: Integer; Value: Boolean) of object;

  TIntegerFuncType = function():Integer of object;
  TIntegerIndexFuncType = function(Index: Integer):Integer of object;
  TIntegerProcType = procedure(Value: Integer) of object;
  TIntegerIndexProcType = procedure(Index: Integer; Value: Integer) of object;

  TInt64FuncType = function():Int64 of object;
  TInt64IndexFuncType = function(Index: Integer):Int64 of object;
  TInt64ProcType = procedure(Value: Int64) of object;
  TInt64IndexProcType = procedure(Index: Integer; Value: Int64) of object;

  TStringFuncType = function():String of object;
  TStringIndexFuncType = function(Index: Integer):String of object;
  TStringProcType = procedure(Value: String) of object;
  TStringIndexProcType = procedure(Index: Integer; Value: String) of object;

  TByteFuncType = function():Byte of object;
  TByteIndexFuncType = function(Index: Integer):Byte of object;
  TByteProcType = procedure(Value: Byte) of object;
  TByteIndexProcType = procedure(Index: Integer; Value: Byte) of object;

  TWordFuncType = function():Word of object;
  TWordIndexFuncType = function(Index: Integer):Word of object;
  TWordProcType = procedure(Value: Word) of object;
  TWordIndexProcType = procedure(Index: Integer; Value: Word) of object;

  TLongWordFuncType = function():LongWord of object;
  TLongWordIndexFuncType = function(Index: Integer):LongWord of object;
  TLongWordProcType = procedure(Value: LongWord) of object;
  TLongWordIndexProcType = procedure(Index: Integer; Value: LongWord) of object;

  TObjectFuncType = function():TObject of object;
  TObjectIndexFuncType = function(Index: Integer):TObject of object;
  TObjectProcType = procedure(Value: TObject) of object;
  TObjectIndexProcType = procedure(Index: Integer; Value: TObject) of object;


type
  {: the Calling Convention }
  { 
   @param ccFastCall the micrsoft fastcall calling convention. use ECX,EDX first, then push Right-to-left, Routine cleanup, 
   @param ccForth is the TurboScript calling convention!
  }
  TCallingConvention = (ccUnknown=-1, ccRegister, ccCdecl, ccPascal, ccStdCall, ccSafeCall, ccFastCall, ccForth);

  PClass = ^TClass;

  //procedure for method.
  PProcedure         = procedure (Self: TObject);
  //Note: the PAbstractError is no param, but the method require the Self param.
  //PAbstractError     = TProcedure;
  PAbstractError     = PProcedure;

  {
  the mtStatic and mtProcedure is static too. but mtStatic is a method of a class.
  }
  TMethodType = (mtUnknown, mtProcedure, mtStatic, mtVirtual, mtDynamic, mtPublished);

  PFieldEntry = ^TFieldEntry;
  //TFieldEntry contains information about a field of a class.
  TFieldEntry = packed record
    //OffSet of the field. TODO relative to what?
    OffSet: Integer;
    //Index for the classtype in the class list.
    IDX: Word;
    //Name of the field.
    Name: ShortString;
  end;

  PFieldClassTable = ^TFieldClassTable;
  //List of classes used by the fields entries.
  TFieldClassTable = packed record
    Count: Smallint;
    //array of classes. The highest valid index in this array is Count - 1.
    Classes: array [0..8191] of ^TPersistentClass;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    //Number of entries in the field table.
    EntryCount: Word;
    //Table with classes, used by the entries IDX property.
    FieldClassTable: PFieldClassTable;
    //First entry (if it exists), this is a variable length structure.
    FirstEntry: TFieldEntry;
   {Entries: array [1..65534] of TFieldEntry;}
  end;

  //Delphi (since version 7) supports extended RTTI on the methods of a class - by compiling the class with $METHODINFO ON defined. This RTTI includes the signature information of the public and published methods. Delphi uses this to implement scripting of Delphi code in the WebSnap framework - see ObjAuto and friends for the details.
  TParamLocation = (plUnknown=-1, plEAX=0, plEDX=1, plECX=2, plStack1=3, plStackN=$FFFF);
  TMethodParamFlag =  (mpfVar, mpfConst, mpfArray, mpfAddress, mpfReference, mpfOut, mpfResult);
  TMethodParamFlags = set of TMethodParamFlag;
  PMethodParam = ^TMethodParam;
  TMethodParam = record
    Flags: TMethodParamFlags;
    ParamName: string;
    TypeName: string;
    TypeInfo: PTypeInfo;
    Location: TParamLocation; 
  end;
  TMethodParamList = array of TMethodParam;
  PMethodSignature = ^TMethodSignature;
  TMethodSignature = record
    Name: string;
    MethodKind: TMethodKind;
    CallConv: TCallingConvention;
    HasSignatureRTTI: boolean;
    Address: Pointer;
    ParamCount: Byte;
    Parameters: TMethodParamList;
    ResultTypeName: string;
    ResultTypeInfo: PTypeInfo;
  end;  
  PPackedShortString = ^TPackedShortString;
  TPackedShortString = string[1];
  PClassInfo = ^TClassInfo;
  TClassInfo = record
    UnitName: string; 
    Name: string;
    ClassType: TClass;
    ParentClass: TClass;
    MethodCount: Word;
    Methods: array of TMethodSignature;  
  end;

  //the published method table structure:
  PPublishedMethodEntry = ^TPublishedMethodEntry;
  TPublishedMethodEntry = packed record
    {Notice 
      if no parameter info(D6 above) the the value of Size has always been equal to 
      the expression:

      Size :=  SizeOf(Size) + SizeOf(Address) + SizeOf(Name[0]) + Length(Name);
    }
    Size: word;  
    Address: Pointer;
    Name: {packed} Shortstring; // really string[Length(Name)]
  end;
  TPublishedMethodEntries = packed array[0..High(Word)-1] of TPublishedMethodEntry;

  PPublishedMethodTable = ^TPublishedMethodTable;
  TPublishedMethodTable = packed record
    Count: Word;
    Methods: TPublishedMethodEntries; // really [0..Count-1]
  end;

  //the Dynamic method table structure
  TDMTIndex   = Smallint;
  PDynamicMethodIndexList = ^TDynamicMethodIndexList;
  TDynamicMethodIndexList = array[0..High(Word)-1] of TDMTIndex;
  PDynamicMethodAddressList = ^TDynamicMethodAddressList;
  TDynamicMethodAddressList = array[0..High(Word)-1] of Pointer;
  PDynamicMethodTable = ^TDynamicMethodTable;
  TDynamicMethodTable = packed record
    Count: word;
    Indicies: TDynamicMethodIndexList; // really [0..Count-1]
    //Methods : TDynamicMethodAddressList; // really [0..Count-1]
  end;

  //the VMT methods table structure
  PSafeCallException = function  (Self: TObject; ExceptObject:
    TObject; ExceptAddr: Pointer): HResult;
  PAfterConstruction = PProcedure;
  PBeforeDestruction = PProcedure;
  PDispatch          = procedure (Self: TObject; var Message);
  PDefaultHandler    = procedure (Self: TObject; var Message);
  PNewInstance       = function  (Self: TClass) : TObject;
  PFreeInstance      = PProcedure;
  PDestroy           = procedure (Self: TObject; OuterMost: ShortInt);

  PVirtualMethodTable = ^TVirtualMethodTable;
  TVirtualMethodTable = packed record
    SelfPtr           : TClass;
    IntfTable         : PInterfaceTable;
    AutoTable         : Pointer;
    {The initialization table is a record type info, 
    from which the size is set to zero, but still has 
    info about the fields which needs to be initialized, 
    but only for the fields not already existing in an 
    inherited class. It is used when a new object is 
    created to initialize its fields.
    
    每个类都有一张InitTable，记录着初始化对象时的内存分配信息。
    从当前类向上追溯，根据每一级类的InitTable进行清理。
    从 _FinalizeRecord的名字可以看出它清理的是复杂数据类型，
    如记录、数组等。当然，对象初始化时也会初始化复杂数据类型成员，
    只是我们在代码中没有看到，应该是一个幕后操作
    }
    InitTable         : Pointer;
    TypeInfo          : PTypeInfo;
    //published field?
    FieldTable        : PFieldTable;
    MethodTable       : PPublishedMethodTable;
    DynamicTable      : PDynamicMethodTable;
    ClassName         : PShortString;
    InstanceSize      : PLongint;
    Parent            : PClass;
    //TODO:  the following method tag as deprecated. use VMTOFFSET in asm code instead
    SafeCallException : PSafeCallException;
    AfterConstruction : PAfterConstruction;
    BeforeDestruction : PBeforeDestruction;
    Dispatch          : PDispatch;
    DefaultHandler    : PDefaultHandler;
    NewInstance       : PNewInstance;
    FreeInstance      : PFreeInstance;
    Destroy           : PDestroy;
   {UserDefinedVirtuals: array[0..999] of procedure;}
  end;

// Virtual Methods
{ Summary Return the VMT record pointer }
function GetVMT(aClass: TClass): PVirtualMethodTable; overload;
function GetVMT(Instance: TObject): PVirtualMethodTable; overload;

//##Come From jclSysUtils
{$IFNDEF FPC}
{ Summary Returns whether a pointer points to a class.}
{ Description
IsClass tests whether the specified pointer points to a class. 
Currently the implementation only test whether the Self 
pointer actually points to the class itself.

Parameters

  @param Address The address to test. Note that this should be the equivalent of Obj.ClassType where Obj is an instance of TObject or descendant class.

Return Value

  The result is True if the Address is a class otherwise it is False.
}
function IsClass(Address: Pointer): Boolean;
{ Summary Returns whether a pointer points to an object.}
{ Description
IsObject tests whether the specified pointer points to an object. 
Currently the implementation only test whether the Self pointer 
actually points to the object itself.

Parameters

 @param Address The address to test. Note that this should be a TObject or TObject descendant.

Return Value

  The result is True if the Address is an object otherwise it is False.
}
function IsObject(Address: Pointer): Boolean;
{ Summary Returns the number of virtual methods of a class.}
{ Description
GetVirtualMethodCount returns the number of virtual methods of the specified class. This includes the methods declared in the class itself and all its parent classes. However, the result does not include the virtual methods declared in TObject. The result includes all abstract methods.

Parameters

  @param aClass The class for which to get the number of virtual methods.

Return Value

  The number of virtual methods of the given class.

}
function GetVirtualMethodCount(aClass: TClass): Integer;
{$ENDIF ~FPC}
{ Summary Returns a pointer to the virtual method from the specified class with the given index.}
{ Descritption
Returns the virtual method from the specified class with the given index. There is no checking performed that the given index actually is a virtual method, and the return value is undefined for invalid indices.

Parameters

  @param aClass The class for which to get a virtual method.
  @param Index  The index of the virtual method to be retrieved. If the index is invalid the result is undefined. Valid indices are 0 to GetVirtualMethodCount - 1.

Return Value

  The result is a pointer to the virtual method. If the method is an abstract method the value returned is a pointer to System._AbstractError.

}
function GetVirtualMethod(aClass: TClass; const Index: Integer): Pointer;
{ Summary Changes a virtual method to the given method.}
{ Description
Changes a virtual method to the given method.

Do NOT use this function.There should be no need to use this function unless your implementing a very dirty hack. 

There is no checking performed that the Index is a valid virtual method index. The function only changes the pointer in the VMT of the specified class, not the static references to this method, which are made when an anchestor uses inherited to call the virtual method. This procedure changes the executable code (with the risk of completly destroying your application) and uses WriteProcessMemory to accomplish that. The function is multiprocessor safe.

Parameters

 @param aClass The class for which to set a virtual method.
 @param Index  The index of the virtual method to be set.
 @param Method The new address of the virtual method.

Return Value
  if success return the OldMethod address else return the nil. 
  set the virtual method only when the Method <> OldMethod(VMT[Index])
}
function SetVirtualMethod(aClass: TClass; const Index: Integer; const Method: Pointer): Pointer;
{ Summary Return the index of the virtual method from the method address.}
{ Description
  Note: the MethodAddr CAN NOT BE abstract method!!!
  if no such method address then return -1.
}
function FindVirtualMethodIndex(aClass: TClass; MethodAddr: Pointer): Integer;

{ Summary Return the Dynamic method table address.}
function GetDMT(aClass: TClass): PDynamicMethodTable;
{ Summary Returns the number of dynamic methods of a class.}
{ Description
GetDynamicMethodCount returns the number of dynamic methods 
for the specified class (including abstract methods). This 
only includes dynamic methods declared in the class itself, 
not the methods delcared in ancestor classes.

Parameters

 @param aClass The class for which to get the number of dynamic methods.

Return Value

 The number of dynamic methods of the specified class.

}
function GetDynamicMethodCount(aClass: TClass): integer;
function GetDynamicMethodIndexBySlot(aClass: TClass; Slot: integer): integer;
//function GetDynamicMethodBySlot(aClass: TClass; Slot: integer): Pointer;
{Summary Returns a pointer to a specific dynamic method.}
{ Description

Returns (the most recent version of) the dynamic method with the given index. 
If there is no dynamic method with the given index an abstract error will be raised, however, 
If the dynamic method is an abstract method, System._AbstractError is returned.

Parameters

  @param aClass The class for which to get the dynamic method.
  @param DMTIndex  The index for which to get.

Return Value

  Returns a pointer to the dynamic method, or System._AbstractError if the dynamic method is an abstract method.


等价于 System.GetDynaMethod }
function GetDynamicMethod(aClass: TClass; DMTIndex: Integer): Pointer;
{ Summary Return a pointer to the dynamic method by the Slot }
{ Only get the dynamic method(new and override) on the class.
}
function GetDynamicMethodBySlot(aClass: TClass; Slot: integer): Pointer;
//through out the parent class.
function SetDynamicMethodByDMTIndex(aClass: TClass; const Index: TDMTIndex; const Method: Pointer): Pointer;
{
Return Value
  return the old method address if success, or return nil.
}
function SetDynamicMethod(aClass: TClass; const Slot: Integer; const Method: Pointer): Pointer;
function HasDynamicMethod(AClass: TClass; DMTIndex: TDMTIndex): Boolean;
//helper functions for DynamicMethod
function GetDynamicMethodAddressList(AClass: TClass): PDynamicMethodAddressList; 
function GetDynamicMethodIndexList(AClass: TClass): PDynamicMethodIndexList;
{ Summary Return the message method address, }
{ 
if fact the message id(WM_DRAWITEM) is the DMTIndex.
so this method call GetDynamicMethod(aClass, aMessageId))
}
function GetMessageMethodAddress(aMessageId: Word; aClass: TClass): Pointer;
{ Summary Return the Slot of the dynamic method from the method address.}
{ Description
  Note: the MethodAddr CAN NOT BE abstract method!!!
  if no such method address then return -1.
}
function FindDynamicMethodIndex(aClass: TClass; MethodAddr: Pointer): Integer;


//###### Published Methods:
{ Summary Returns the method table of a class.}
{
GetPMT returns a pointer to the method table of the specified class. 
The method table contains information about the published methods declared in the class.

Parameters

  @param AClass The class for which to retrieve the method table.

Return Value

  Pointer to the method table. Do not modify this structure!

}
function GetPMT(aClass: TClass): PPublishedMethodTable;
{ Summary Returns the number of published methods of a class.}
function GetPublishedMethodCount(aClass: TClass): integer;
function GetPublishedMethodEntry(aClass: TClass; Index: integer): PPublishedMethodEntry;overload;
{
@param aName the method name must be declared on the aClass!!
@param Index return the index(slot) number for the virtual method or dyanmic method if found.
return mtUnknown if not found
}
function GetPublishedMethodType(aClass: TClass; const aName: ShortString; out Index: Integer): TMethodType;
{
@param Index return the  index(slot) number for the virtual method or dyanmic method if found. 
             if it is published method return index is PPublishedMethodEntry. 
             Note: if you set the index to -1, means you wanna find the method whether virtual or dynamic or static 
             and do not test published method.

}
function GetMethodType(aClass: TClass; aMethod: Pointer; var Index: Integer): TMethodType;
{ Summary Get the aClass's Published method address(only the aClass not search the parent class)}
function GetPublishedMethod(aClass: TClass; const aName: ShortString): Pointer;
{ Summary Changes a published method to the given method on the PMT.}
{ Description
Changes a published method to the given method.

Do NOT use this function.There should be no need to use this function unless your implementing a very dirty hack. 

The function only changes the pointer in the PMT of the specified class. This procedure changes the executable code (with the risk of completly destroying your application) and uses WriteProcessMemory to accomplish that. The function is multiprocessor safe.

Parameters

 @param aClass The class for which to set a published method.
 @param aName  The name of the published method to be set.
 @param Method The new address of the published method.

Return Value
  if success return the OldMethod address else return the nil. 
}
function SetPublishedMethod(aClass: TClass; const aName: ShortString; const aMethod: Pointer): Pointer;

{ Summary Returns an entry from the method table.}
{
GetPublishedMethodEntry returns the entry with the given index from the method table. 
There is no checking performed that the given Index is valid.

Parameters

  @param MethodTable The method table to get the entry from
  @param Index The index of the entry to retrieve.

Return Value

  Pointer to the method table entry from the method table with the given index. Do not modify this structure!

}
function GetPublishedMethodEntry(MethodTable: PPublishedMethodTable; Index: Integer): PPublishedMethodEntry;overload;

function GetFirstPublishedMethodEntry(aClass: TClass): PPublishedMethodEntry;
function GetNextPublishedMethodEntry(aClass: TClass; PublishedMethodEntry: PPublishedMethodEntry): PPublishedMethodEntry;

type
  //Return true to stop Enumerating.
  TGetPublishedMethodItemProc = function (const aItem: PPublishedMethodEntry): Boolean;
procedure EnumeratePublishedMethods(const aClass: TClass; const DoItemProc: TGetPublishedMethodItemProc);

{
Note:
TObject already contains methods to perform published method lookups using MethodAddress and MethodName. it will search on the parant class too.
these function only search on the current class!!
}
function FindPublishedMethodEntryName(aClass: TClass; AAddr: Pointer): Shortstring;
function FindPublishedMethodEntryAddr(aClass: TClass; const aName: ShortString): Pointer;
function FindPublishedMethodEntryByAddr(aClass: TClass; AAddr: Pointer): PPublishedMethodEntry;
function FindPublishedMethodEntryByName(aClass: TClass; const aName: ShortString): PPublishedMethodEntry;


{ Summary Returns the initialization table of a class.}
{
GetInitTable returns the initialization table of the specified class. 

TODO This is abracadabra to me :-) Need to contact author for further explanation about this (eg what does the init table look like). 

The initialization table is a record type info, from which the size is set to zero, 
but still has info about the fields which needs to be initialized, 
but only for the fields not already existing in an inherited class. 
It is used when a new object is created to initialize its fields.

Parameters

  @param AClass The class for which to get the initialization table.

Return Value

  The result is a pointer to the initialization table. Do not modify this structure!

}
function GetInitTable(AClass: TClass): PTypeInfo;


{ field table methods }


{ Summary Returns the field table of a class.}
{
GetFieldTable returns a pointer to the field table of the specified class. A field table contains information about the published fields in a class.

Parameters

  @param AClass The class for which to retrieve the field table.

Return Value

  Pointer to the field table. Do not modify this structure!


}
function GetFieldTable(AClass: TClass): PFieldTable;


// Class Parent
{ Summary Modifies the class parent for the given class.}
{
Changes the parent of AClass to become NewClassParent. 

 Do NOT use this function.There should be no need to 
 use this function unless you are implementing a very 
 dirty hack.

This procedure changes the executable code (with the risk 
of completly destroying your application) and uses WriteProcessMemory 
for this. The function is multiprocessor safe.

Parameters

  @param AClass The class whose parent to change.
  @param NewClassParent The new parent for AClass.


}
procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
{ Summary Returns the parent of the class.}
{
Returns the parent class of the given class. 
That is, it returns the type of the immediate 
ancestor of the specified class. 
This is the same as TObject.ClassParent.

Parameters

  @param AClass The class for which to retrieve the parent class.

Return Value

  The parent class (type of the immediate ancestor) of the specified class or nil of the specified class is TObject.

}
function GetClassParent(AClass: TClass): TClass;

// Interface information
function GetImplementorOfInterface(const I: IInterface): TObject;

{ Summary Return the AbstractError procedure address.}
function GetAbstractErrorProc: PAbstractError;

{ Summary Return true if the method is abstract method.}
{
check the method whether it is the abstract method.
}
function IsAbstractMethod(aMethod: Pointer): Boolean;


//NOTE: the TypeInfo MUST be the Enum type info!!
function SetToString(TheEnumTypeInfo: PTypeInfo; Value: Integer; Brackets: Boolean = False): string;
function StringToSet(TheEnumTypeInfo: PTypeInfo; const Value: string): Integer;
{$IFNDEF COMPILER9_UP}
function GetSetElementName(TypeInfo: PTypeInfo; Value: Integer): string;
function GetSetElementValue(TypeInfo: PTypeInfo; const Name: string): Integer;
{$ENDIF}

procedure SetInstanceSize(const aClass: TClass; const aNewSize: Integer);

//procedure GetInterfaceInfo(InterfaceTypeInfo: PTypeInfo; 
  //var InterfaceInfo: TInterfaceInfo);

var
  AbstractErrorProc: PAbstractError;

implementation

uses
  RTLConsts;


//=== Interface Table ========================================================

function GetInitTable(AClass: TClass): PTypeInfo; assembler;
asm
        MOV     EAX, [EAX].vmtInitTable
end;

function GetFieldTable(AClass: TClass): PFieldTable; assembler;
asm
        MOV     EAX, [EAX].vmtFieldTable
end;

//=== Class Parent methods ===================================================

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + vmtParent)^;
  WriteMem(PatchAddress, @NewClassParent, SizeOf(Pointer));
end;

function GetClassParent(AClass: TClass): TClass; assembler;
asm
        MOV     EAX, [EAX].vmtParent
        TEST    EAX, EAX
        JE      @@Exit
        MOV     EAX, [EAX]
@@Exit:
end;

//=== Interface information ==================================================

function GetImplementorOfInterface(const I: IInterface): TObject;
{ TODO -cDOC : Original code by Hallvard Vassbotn }
{ TODO -cTesting : Check the implemetation for any further version of compiler }
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: Longint of
      AddByte: (AdjustmentByte: ShortInt);
      AddLong: (AdjustmentLong: Longint);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  try
    Result := Pointer(I);
    if Assigned(Result) then
    begin
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte:
          Inc(PChar(Result), QueryInterfaceThunk.AdjustmentByte);
        AddLong:
          Inc(PChar(Result), QueryInterfaceThunk.AdjustmentLong);
      else
        Result := nil;
      end;
    end;
  except
    Result := nil;
  end;
end;

{$IFNDEF FPC}
{
普通对象的第一个双字就是指向其类的VMT的指针，以此将对象、类和元类关联起来。
这个VMT表是Delphi中类的核心所在，通过它可以在运行时获取类的绝大部分信息。
例如在VMT中有一个vmtSelfPtr指针又回指到VMT表头，我们可以利用这个特性判断一个
指针指向的是否是有效的对象或类。
}
function IsClass(Address: Pointer): Boolean; assembler;
asm
        CMP     Address, Address.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;
{$ENDIF ~FPC}

{$IFNDEF FPC}
function IsObject(Address: Pointer): Boolean; assembler;
asm
// or IsClass(Pointer(Address^));
        MOV     EAX, [Address]
        CMP     EAX, EAX.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;
{$ENDIF ~FPC}

// Virtual Methods
// Helper method

function SetVMTPointer(aClass: TClass; Offset: Integer; Value: Pointer): Pointer;
var
  PatchAddress: PPointer;
begin
  PatchAddress := Pointer(Integer(aClass) + Offset);
  ReadMem(PatchAddress, @Result, SizeOf(Result));
  if Result <> Value then
    WriteMem(PatchAddress, @Value, SizeOf(Value))
  else
    Result := nil;
end;

function GetVMT(aClass: TClass): PVirtualMethodTable;
begin
  Result := PVirtualMethodTable(aClass);
  Dec(Result);
end;

function GetVMT(Instance: TObject): PVirtualMethodTable;
begin
  Result := GetVMT(Instance.ClassType);
end;

{$IFNDEF FPC}
function GetVirtualMethodCount(aClass: TClass): Integer;
var
  BeginVMT: Longint;
  EndVMT: Longint;
  TablePointer: Longint;
  I: Integer;
begin
  BeginVMT := Longint(aClass);

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.

  EndVMT := PLongint(Longint(aClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    TablePointer := PLongint(Longint(aClass) + I)^;
    if (TablePointer <> 0) and (TablePointer >= BeginVMT) and
       (TablePointer < EndVMT) then
      EndVMT := Longint(TablePointer);
    Inc(I, SizeOf(Pointer));
  until I >= vmtClassName;

  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;
{$ENDIF ~FPC}

function GetVirtualMethod(aClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(Integer(aClass) + Index * SizeOf(Pointer))^;
end;

function SetVirtualMethod(aClass: TClass; const Index: Integer; const Method: Pointer): Pointer;
begin
  Result := SetVMTPointer(aClass, Index * SizeOf(Pointer), Method);
end;

function FindVirtualMethodIndex(aClass: TClass; MethodAddr: Pointer): Integer;
var
  LVMethodAddr: Pointer;
begin
  Result := 0;
  for Result := 0 to GetVirtualMethodCount(aClass) - 1 do
    if MethodAddr = GetVirtualMethod(aClass, Result) then
      exit;
  Result := -1;
end;

//################### Published Method ####################
function GetPMT(aClass: TClass): PPublishedMethodTable;
{$IFDEF PUREPASCAL}
var
  Vmt: PVirtualMethodTable;
begin
  Vmt := GetVMT(aClass);
  if Assigned(Vmt)
  then Result := Vmt.MethodTable
  else Result := nil;
end;
{$ELSE}
assembler;
asm
        MOV     EAX, [EAX].vmtMethodTable
end;
{$ENDIF}

function GetPublishedMethodEntry(aClass: TClass; Index: integer): PPublishedMethodEntry;
var
  Pmt: PPublishedMethodTable;
begin
  Pmt := GetPMT(aClass);
  Result := GetPublishedMethodEntry(Pmt, Index);
{  if Assigned(Pmt) and (Index < Pmt.Count) then
  begin
    Result := @Pmt.Methods[0];
    while Index > 0 do
    begin
      Inc(PChar(Result), Result.Size);
      Dec(Index);
    end;
  end
  else
    Result := nil;
}
end;

function GetPublishedMethodEntry(MethodTable: PPublishedMethodTable; Index: Integer): PPublishedMethodEntry;
begin
  Result := Pointer(Cardinal(MethodTable) + 2);
  for Index := Index downto 1 do
    Inc(Cardinal(Result), Result^.Size);
end;

function GetPublishedMethodCount(aClass: TClass): integer;
var
  Pmt: PPublishedMethodTable;
begin
  Pmt := GetPMT(aClass);
  if Assigned(Pmt)
  then Result := Pmt.Count
  else Result := 0;
end;

procedure EnumeratePublishedMethods(const aClass: TClass; const DoItemProc: TGetPublishedMethodItemProc);
var
  i: integer;
  vItem: PPublishedMethodEntry;
begin
  if Assigned(aClass) and Assigned(DoItemProc) then
  begin
    vItem := GetFirstPublishedMethodEntry(aClass);
    for i := 0 to GetPublishedMethodCount(aClass)-1 do
    begin
      if DoItemProc(vItem) then exit;
      vItem := GetNextPublishedMethodEntry(aClass, vItem);
    end;
  end;
end;

function GetFirstPublishedMethodEntry(aClass: TClass): PPublishedMethodEntry;
begin
  Result := GetPublishedMethodEntry(aClass, 0);
end;
 
{Detecting extra published method data
I’ve added some DEBUG code to GetNextPublishedMethodEntry that tries to detect and raise an exception of it encounters a TPublishedMethodEntry record where the Size field indicates that the record contains additional data after the packed Name string.

Delphi 5 (and earlier versions) would encode the parameters of some published methods – more specifically stdcall methods with RTTI enabled parameter and return types. This half-hearted parameter encoding was probably the remains of some experimental RTTI generation code in the compiler that seems to have been removed from Delphi 7 and 2006.
}
function GetNextPublishedMethodEntry(aClass: TClass; 
  PublishedMethodEntry: PPublishedMethodEntry): PPublishedMethodEntry;
{$IFDEF DEBUG}
var
  ExpectedSize: integer;
{$ENDIF}
begin
  Result := PublishedMethodEntry;
{$IFDEF DEBUG}
  ExpectedSize :=   SizeOf(Result.Size) 
                  + SizeOf(Result.Address) 
                  + SizeOf(Result.Name[0]) 
                  + Length(Result.Name);
  if Result.Size <> ExpectedSize then
    raise Exception.CreateFmt(
'RTTI for the published method "%s" of class "%s" has %d extra bytes of unknown data!', 
[Result.Name, aClass.ClassName, Result.Size-ExpectedSize]);
{$ENDIF}
  if Assigned(Result) then
    Inc(PChar(Result), Result.Size);
end;

function FindPublishedMethodEntryByName(aClass: TClass; 
  const aName: ShortString): PPublishedMethodEntry;
var
  i : integer;
begin
  if Assigned(aClass) then
  //while Assigned(aClass) do
  begin
    Result := GetFirstPublishedMethodEntry(aClass);
    for i := 0 to GetPublishedMethodCount(aClass)-1 do
    begin
      // Note: Length(ShortString) expands to efficient inline code
      if (Length(Result.Name) = Length(aName)) and
         (StrLIComp(@Result.Name[1], @aName[1], Length(aName)) = 0) then
        Exit;
      Result := GetNextPublishedMethodEntry(aClass, Result);
    end;
    //aClass := aClass.ClassParent;
  end;
  Result := nil;
end;
 
function FindPublishedMethodEntryByAddr(aClass: TClass; 
  AAddr: Pointer): PPublishedMethodEntry;
var
  i : integer;
begin
  while Assigned(aClass) do
  begin
    Result := GetFirstPublishedMethodEntry(aClass);
    for i := 0 to GetPublishedMethodCount(aClass)-1 do
    begin
      if Result.Address = AAddr then
        Exit;
      Result := GetNextPublishedMethodEntry(aClass, Result);
    end;
    aClass := aClass.ClassParent;
  end;
  Result := nil;
end;
 
function FindPublishedMethodEntryAddr(aClass: TClass; const 
  aName: ShortString): Pointer;
var
  Method: PPublishedMethodEntry;
begin
  Method := FindPublishedMethodEntryByName(aClass, aName);
  if Assigned(Method)
  then Result := Method.Address
  else Result := nil;
end;
 
function FindPublishedMethodEntryName(aClass: TClass; 
  AAddr: Pointer): Shortstring;
var
  Method: PPublishedMethodEntry;
begin
  Method := FindPublishedMethodEntryByAddr(aClass, AAddr);
  if Assigned(Method)
  then Result := Method.Name
  else Result := '';
end;

function GetMethodType(aClass: TClass; aMethod: Pointer; var Index: Integer): TMethodType;
begin
  Result := mtUnknown;
  if aMethod <> nil then
  begin
    if Index <> -1 then //if the Index is -1 then means do not get published method type. if u wann search the published method first, set index =0
    begin // test whether it is published
      Index := Integer(FindPublishedMethodEntryByAddr(aClass, aMethod));
      if Index <> 0 then
      begin
        Result := mtPublished;
        Exit;
      end;
    end;
    Index := FindVirtualMethodIndex(aClass, aMethod); 
    if Index <> -1 then
    begin
      Result := mtVirtual;
      Exit;
    end;
    Index := FindDynamicMethodIndex(aClass, aMethod); 
    if Index <> -1 then
      Result := mtDynamic
    else
      Result := mtStatic;
  end
  else
    Index := -1;
end;

function GetPublishedMethodType(aClass: TClass; const aName: ShortString; out Index: Integer): TMethodType;
var
  P: Pointer;
begin
  Result := mtUnknown;
  //TODO the MethodAddress will search all parent class for the name!!
  //     I should only search on the aClass!!
  // it doesn't matter, if not find it will return -1. for virtual method it should a feature.
  P := aClass.MethodAddress(aName);
  if P <> nil then
  begin
    Result := GetMethodType(aClass, P, Index);
  end
  else 
    Index := -1;
end;

function SetPublishedMethod(aClass: TClass; const aName: ShortString; const aMethod: Pointer): Pointer;
var
  i : integer;
  PatchAddress: PPointer;
begin
  if Assigned(aClass) then
  begin
    Result := GetFirstPublishedMethodEntry(aClass);
    for i := 0 to GetPublishedMethodCount(aClass)-1 do
    begin
      // Note: Length(ShortString) expands to efficient inline code
      with PPublishedMethodEntry(Result)^ do
      if (Length(Name) = Length(aName)) and (StrLIComp(@Name[1], @aName[1], Length(aName)) = 0) then
      begin
        PatchAddress := @Address;
        Result := Address;
        WriteMem(PatchAddress, @aMethod, SizeOf(Pointer));
        Exit;
      end;
      Result := GetNextPublishedMethodEntry(aClass, PPublishedMethodEntry(Result));
    end;    
    //aClass := aClass.ClassParent;
  end;
  Result := nil;
end;

function GetPublishedMethod(aClass: TClass; const aName: ShortString): Pointer;
{$IFDEF PUREPASCAL}
var
  i : integer;
begin
  //while Assigned(aClass) do
  if Assigned(aClass) then
  begin
    Result := GetFirstPublishedMethodEntry(aClass);
    for i := 0 to GetPublishedMethodCount(aClass)-1 do
    begin
      // Note: Length(ShortString) expands to efficient inline code
      with PPublishedMethodEntry(Result)^ do
      if (Length(Name) = Length(aName)) and (StrLIComp(@Name[1], @aName[1], Length(aName)) = 0) then
      begin
        Result := Address;
        Exit;
      end;
      Result := GetNextPublishedMethodEntry(aClass, PPublishedMethodEntry(Result));
    end;    
    //aClass := aClass.ClassParent;
  end;
  Result := nil;
end;
{$ELSE}
//modify from System.pas
asm
        { ->    EAX     Pointer to class        }
        {       EDX     Pointer to name }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        XOR     ECX,ECX
        XOR     EDI,EDI
        MOV     BL,[EDX]
        JMP     @@haveVMT
@@outer:                                { upper 16 bits of ECX are 0 !  }
        MOV     EAX,[EAX]
@@haveVMT:
        MOV     ESI,[EAX].vmtMethodTable
        TEST    ESI,ESI
        JE      @@parent
        MOV     DI,[ESI]                { EDI := method count           }
        ADD     ESI,2
@@inner:                                { upper 16 bits of ECX are 0 !  }
        MOV     CL,[ESI+6]              { compare length of strings     }
        CMP     CL,BL
        JE      @@cmpChar
@@cont:                                 { upper 16 bits of ECX are 0 !  }
        MOV     CX,[ESI]                { fetch length of method desc   }
        ADD     ESI,ECX                 { point ESI to next method      }
        DEC     EDI
        JNZ     @@inner
@@parent:
        //MOV     EAX,[EAX].vmtParent     { fetch parent vmt              }
        //TEST    EAX,EAX
        //JNE     @@outer
        MOV     EAX,0
        JMP     @@exit                  { return NIL                    }

@@notEqual:
        MOV     BL,[EDX]                { restore BL to length of name  }
        JMP     @@cont

@@cmpChar:                              { upper 16 bits of ECX are 0 !  }
        MOV     CH,0                    { upper 24 bits of ECX are 0 !  }
@@cmpCharLoop:
        MOV     BL,[ESI+ECX+6]          { case insensitive string cmp   }
        XOR     BL,[EDX+ECX+0]          { last char is compared first   }
        AND     BL,$DF
        JNE     @@notEqual
        DEC     ECX                     { ECX serves as counter         }
        JNZ     @@cmpCharLoop

        { found it }
        MOV     EAX,[ESI+2]

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

//###################### Dynamic Methods #####################

function FindDynamicMethodIndex(aClass: TClass; MethodAddr: Pointer): Integer;
var
  Dmt: PDynamicMethodTable;
  DmtMethods: PDynamicMethodAddressList;
begin
  Dmt := GetDMT(aClass);
  if Assigned(Dmt) then 
  begin
    DmtMethods := @Dmt.Indicies[Dmt.Count];
    for Result := 0 to Dmt.Count - 1 do
    begin
      if MethodAddr = DmtMethods[Result] then
      begin
        exit;
      end;
    end
  end;
  Result := -1;
end;

function GetDMT(aClass: TClass): PDynamicMethodTable;
var
  Vmt: PVirtualMethodTable;
begin
  Vmt := GetVMT(aClass);
  if Assigned(Vmt)
  then Result := Vmt.DynamicTable
  else Result := nil;
end;

function GetDynamicMethodCount(aClass: TClass): integer;
{$IFDEF PUREPASCAL}
var
  Dmt: PDynamicMethodTable;
begin
  Dmt := GetDMT(aClass);
  if Assigned(Dmt)
  then Result := Dmt.Count
  else Result := 0;
end;
{$ELSE}
assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        TEST    EAX, EAX
        JE      @@Exit
        MOVZX   EAX, WORD PTR [EAX]
@@Exit:
end;
{$ENDIF}
  
function GetDynamicMethodIndexBySlot(aClass: TClass; Slot: integer): integer;
var
  Dmt: PDynamicMethodTable;
begin
  Dmt := GetDMT(aClass);
  if Assigned(Dmt) and (Slot < Dmt.Count)
  then Result := Dmt.Indicies[Slot]
  else Result := -1; // Or raise exception
end;
 
function SetDynamicMethod(aClass: TClass; const Slot: Integer; const Method: Pointer): Pointer;
var
  Dmt: PDynamicMethodTable;
  DmtMethods: PDynamicMethodAddressList;
  PatchAddress: PPointer;
begin
  Dmt := GetDMT(aClass);
  if Assigned(Dmt) and (Slot < Dmt.Count) then
  begin
    DmtMethods := @Dmt.Indicies[Dmt.Count];
    PatchAddress := @DmtMethods[Slot];
    //keep the old address entry
    Result := PatchAddress^;
    WriteMem(PatchAddress, @Method, SizeOf(Pointer));
  end
  else 
    Result := nil;
end;

function SetDynamicMethodByDMTIndex(aClass: TClass; const Index: TDMTIndex; const Method: Pointer): Pointer;
var
  Dmt: PDynamicMethodTable;
  DmtMethods: PDynamicMethodAddressList;
  i: integer;
  PatchAddress: PPointer;
begin
  while Assigned(aClass) do
  begin
    Dmt := GetDMT(aClass);
    if Assigned(Dmt) then
      for i := 0 to Dmt.Count-1 do
        if Index = Dmt.Indicies[i] then
        begin
          DmtMethods := @Dmt.Indicies[Dmt.Count];
          PatchAddress := @DmtMethods[i];
          //keep the old address entry
          Result := PatchAddress^; //=GetDynamicMethodByDMTIndex
          WriteMem(PatchAddress, @Method, SizeOf(Pointer));
          Exit;
        end;
    // Not in this class, try the parent class
    aClass := aClass.ClassParent;
  end;
  Result := nil;
end;

function GetDynamicMethodBySlot(aClass: TClass; Slot: Integer): Pointer;
{$IFDEF PUREPASCAL}
var
  Dmt: PDynamicMethodTable;
  DmtMethods: PDynamicMethodAddressList;
begin
  Dmt := GetDMT(aClass);
  if Assigned(Dmt) and (Slot < Dmt.Count) then
  begin
    DmtMethods := @Dmt.Indicies[Dmt.Count];
    Result := DmtMethods[Slot];
  end
  else 
    Result := nil;
end;
{$ELSE}
assembler;
asm
        { ->    EAX     vmt of class            }
        {       DX      Slot                    }
        //MOV     EAX, [EAX].vmtDynamicTable  // EAX <- DMT
        //MOVZX   ECX, Word ptr [EAX]         // ECX <- DMT.Count
        //ADD     EAX, ECX                    //
        //ADD     EAX, ECX                    // the DMIndexList'value size is word so add twice.
        //ADD     EAX, 2                      // Skip the DMT.Count field.
        CALL    GetDynamicMethodAddressList
        //ADD     EAX, EDX
        MOV     EAX, [EAX + EDX * cPointerSize]
end;
{$ENDIF}

function GetDynamicMethod(aClass: TClass; DMTIndex: Integer): Pointer;
{$IFDEF PUREPASCAL}
// Pascal variant of the faster BASM version in System.GetDynaMethod
var
  Dmt: PDynamicMethodTable;
  DmtMethods: PDynamicMethodAddressList;
  i: integer;
begin
  while Assigned(aClass) do
  begin
    Dmt := GetDMT(aClass);
    if Assigned(Dmt) then
      for i := 0 to Dmt.Count-1 do
        if DMTIndex = Dmt.Indicies[i] then
        begin
          DmtMethods := @Dmt.Indicies[Dmt.Count];
          Result := DmtMethods[i];
          Exit;
        end;
    // Not in this class, try the parent class
    aClass := aClass.ClassParent;
  end;
  Result := nil;
  asm
    JMP     System.@AbstractError
  end;
  //AbstractError; //if no such index then raise the abstract error.}
end;
{$ELSE}
assembler;
asm
        CALL    System.@FindDynaClass     //this would raise exception when not found.
        {
        PUSH    ESI                     //the compiler do not know the GetDynaMethod indentity, soo.....
        MOV     ESI,EDX                 //the GetDynaMethod entry param: ESI is dynamic method index.
        CALL    System.@GetDynaMethod   //if u do not want raise exception when not found the DMTIndex
        MOV     EAX,ESI                 //the System.@GetDynaMethod put the result to ESI, so i need copy to EAX
        POP     ESI
        /}
end;
{$ENDIF}

function GetMessageMethodAddress(aMessageId: Word; aClass: TClass): Pointer;
begin
  Result := GetDynamicMethod(aClass, aMessageId); 
end;

function GetDynamicMethodIndexList(AClass: TClass): PDynamicMethodIndexList; 
{$IFDEF PUREPASCAL}
var
  Dmt: PDynamicMethodTable;
begin
  Dmt := GetDMT(aClass);
  if Assigned(Dmt) then
     Result := @Dmt.Indicies[0]
  else 
    Result := nil;
end;
{$ELSE}
assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        ADD     EAX, 2
end;
{$ENDIF}

function GetDynamicMethodAddressList(AClass: TClass): PDynamicMethodAddressList;
{$IFDEF PUREPASCAL}
var
  Dmt: PDynamicMethodTable;
  DmtMethods: PDynamicMethodAddressList;
begin
  Dmt := GetDMT(aClass);
  if Assigned(Dmt) then
     //the last array Indicies is the MethodAddressList beginning...
     Result := @Dmt.Indicies[Dmt.Count]
  else 
    Result := nil;
end;
{$ELSE}
assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable  // EAX <- DMT
        MOVZX   ECX, Word ptr [EAX]         // ECX <- DMT.Count
        ADD     EAX, ECX                    //
        ADD     EAX, ECX                    // the DMIndexList'value size is word so add twice.
        ADD     EAX, 2                      // Skip the DMT.Count field.
end;
{$ENDIF}

function HasDynamicMethod(AClass: TClass; DMTIndex: TDMTIndex): Boolean;
{$IFDEF PUREPASCAL}
var
  Dmt: PDynamicMethodTable;
  i: integer;
begin
  Result := False;
  while Assigned(aClass) do
  begin
    Dmt := GetDMT(aClass);
    if Assigned(Dmt) then
      for i := 0 to Dmt.Count-1 do
        if DMTIndex = Dmt.Indicies[i] then
        begin
          Result := True;
          Exit;
        end;
    // Not in this class, try the parent class
    aClass := aClass.ClassParent;
  end;
end;
{$ELSE}
assembler;
// Mainly copied from System.GetDynaMethod
asm
        { ->    EAX     vmt of class            }
        {       DX      dynamic method index    }

        PUSH    EDI
        XCHG    EAX, EDX
        JMP     @@HaveVMT
@@OuterLoop:
        MOV     EDX, [EDX]
@@HaveVMT:
        MOV     EDI, [EDX].vmtDynamicTable
        TEST    EDI, EDI
        JE      @@Parent
        MOVZX   ECX, WORD PTR [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@Found
        POP     ECX
@@Parent:
        MOV     EDX,[EDX].vmtParent
        TEST    EDX,EDX
        JNE     @@OuterLoop
        MOV     EAX, 0
        JMP     @@Exit
@@Found:
        POP     EAX
        MOV     EAX, 1
@@Exit:
        POP     EDI
end;
{$ENDIF}

function GetAbstractErrorProc: PAbstractError;
assembler;
asm
  {$IFDEF Borland}
    MOV EAX, offset System.@AbstractError
  {$ENDIF}
  {$IFDEF FPC}
    MOV EAX, offset AbstractError
  {$ENDIF}
end;


function IsAbstractMethod(aMethod: Pointer): Boolean;
var
  vAbstractJump: TRedirectCodeRec;
  vJump: TRedirectCodeRec;
begin
  Result := Integer(aMethod) = Integer(@AbstractErrorProc); //for the not pulbished method or get the address from VMT or DMT.
  if not Result then //get the method address from PMT
  begin
    vAbstractJump.Jump := $E9;
    {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
    vAbstractJump.Offset := Integer(@AbstractErrorProc) - Integer(aMethod) - cNearJMPDirectiveSize;
    ReadMem(aMethod, @vJump, cNearJMPDirectiveSize);
    {$ELSE}
    vAbstractJump.Offset := Integer(@AbstractErrorProc) - Integer(aMethod) - SizeOf(TRedirectCodeRec);
    ReadMem(aMethod, @vJump, SizeOf(TRedirectCodeRec));
    {$ENDIF}
    Result := (vAbstractJump.Offset = vJump.Offset) and (vAbstractJump.Jump = vJump.Jump);
  end;
end;

function SetToString(TheEnumTypeInfo: PTypeInfo; Value: Integer; Brackets: Boolean): string;
var
  S: TIntegerSet;
  I: Integer;
begin
  Result := '';
  Integer(S) := Value;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TheEnumTypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

function StringToSet(TheEnumTypeInfo: PTypeInfo; const Value: string): Integer;
var
  P: PChar;
  EnumName: string;
  EnumValue: Longint;

  // grab the next enum name
  function NextWord(var P: PChar): string;
  var
    i: Integer;
  begin
    i := 0;

    // scan til whitespace
    while not (P[i] in [',', ' ', #0,']']) do
      Inc(i);

    SetString(Result, P, i);

    // skip whitespace
    while P[i] in [',', ' ',']'] do
      Inc(i);

    Inc(P, i);
  end;

begin
  Result := 0;
  if Value = '' then Exit;
  P := PChar(Value);

  // skip leading bracket and whitespace
  while P^ in ['[',' '] do
    Inc(P);

  EnumName := NextWord(P);
  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(TheEnumTypeInfo, EnumName);
    if EnumValue < 0 then
      raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyElement, [EnumName]);
    Include(TIntegerSet(Result), EnumValue);
    EnumName := NextWord(P);
  end;
end;

{$IFNDEF COMPILER9_UP}
function GetSetElementName(TypeInfo: PTypeInfo; Value: Integer): string;
begin
  case TypeInfo^.Kind of
    tkInteger      : Result := IntToStr(Value);      // int  range, like (2..20)
    tkChar, tkWChar: Result := '#'+IntToStr(Value);  // char range, like (#2..#20)
  else
    Result := GetEnumName(TypeInfo, Value);          // enums
  end;
end;

function GetSetElementValue(TypeInfo: PTypeInfo; const Name: string): Integer;
var
  MinValue: integer; 
begin
  MinValue := GetTypeData(TypeInfo).MinValue;;
  
  case TypeInfo^.Kind of
    tkInteger      : 
    begin 
      Result := StrToInt(Name); 
      Dec(Result, MinValue);
    end;
    tkChar, tkWChar: 
    begin 
      Result := StrToInt(Copy(Name,2,Length(Name)-1)); 
      Dec(Result, MinValue);
    end;
  else
    Result := GetEnumValue(TypeInfo, Name);
  end;  
end;
{$ENDIF}

procedure SetInstanceSize(const aClass: TClass; const aNewSize: Integer);
var
  PatchAddress: Pointer;
begin
  Integer(PatchAddress) := Integer(aClass) + vmtInstanceSize;
  WriteMem(PatchAddress, @aNewSize, SizeOf(Integer));
end;
	
function Skip(Value: PShortstring): pointer; overload;
begin
  Result := Value;
  Inc(PChar(Result), SizeOf(Value^[0]) + Length(Value^));
end;  

function Skip(Value: PPackedShortString; var NextField{: Pointer}): PShortString; overload;
begin
  Result := PShortString(Value);
  Inc(PChar(NextField), SizeOf(Char) + Length(Result^) - SizeOf(TPackedShortString));
end;  

function Skip(CurrField: pointer; FieldSize: integer): pointer; overload;
begin
  Result := PChar(Currfield) + FieldSize;
end;

function Dereference(P: PPTypeInfo): PTypeInfo;
begin
  if Assigned(P) 
  then Result := P^
  else Result := nil;
end;  

function SkipPackedShortString(Value: PShortstring): pointer;
begin
  Result := Value;
  Inc(PChar(Result), SizeOf(Value^[0]) + Length(Value^));
end;  

function PackedShortString(Value: PShortstring; var NextField{: Pointer}): PShortString; overload;
begin
  Result := Value;
  PShortString(NextField) := Value;
  Inc(PChar(NextField), SizeOf(Result^[0]) + Length(Result^));
end;  

function PackedShortString(var NextField{: Pointer}): PShortString; overload;
begin
  Result := PShortString(NextField);
  Inc(PChar(NextField), SizeOf(Result^[0]) + Length(Result^));
end;  

function GetMethodSignature(Event: PPropInfo): TMethodSignature;        
(* From TypInfo
  TTypeData = packed record
    case TTypeKind of
     ...
     tkMethod: (
        MethodKind: TMethodKind;
        ParamCount: Byte;
        Parameters: array[0..1023] of Char
       {Parameters: array[1..ParamCount] of
          record
            Flags: TMethodParamFlags;
            ParamName: ShortString;
            TypeName: ShortString;
          end;
        ResultTypeName: ShortString);*)
type
  PParamListRecord = ^TParamListRecord;
  TParamListRecord = packed record 
    Flags: TMethodParamFlags;
    ParamName: {packed} ShortString; // Really string[Length(ParamName)]
    TypeName:  {packed} ShortString; // Really string[Length(TypeName)]
  end;
var
  EventData: PTypeData;
  i: integer;
  MethodParam: PMethodParam;
  ParamListRecord: PParamListRecord;
begin
  Assert(Assigned(Event) and Assigned(Event.PropType));
  Assert(Event.PropType^.Kind = tkMethod);
  EventData := GetTypeData(Event.PropType^);
  Result.MethodKind := EventData.MethodKind;
  Result.ParamCount := EventData.ParamCount;
  SetLength(Result.Parameters, Result.ParamCount);
  ParamListRecord := @EventData.ParamList;
  for i := 0 to Result.ParamCount-1 do
  begin
    MethodParam := @Result.Parameters[i];
    MethodParam.Flags     := ParamListRecord.Flags;
    MethodParam.ParamName := PackedShortString(@ParamListRecord.ParamName, ParamListRecord)^;
    MethodParam.TypeName  := PackedShortString(ParamListRecord)^;
  end;  
  Result.ResultTypeName := PackedShortString(ParamListRecord)^;
end;  

type
  // compiler implementation-specific structures, subject to change in future Delphi versions
  // Derived from declarations in ObjAuto.pas
  PReturnInfo = ^TReturnInfo;
  TReturnInfo = packed record
    Version: Byte; 
    CallingConvention: TCallingConvention;
    ReturnType: PPTypeInfo;
    ParamSize: Word;
  end;
  PParamInfo = ^TParamInfo;
  TParamInfo = packed record
    Flags: TMethodParamFlags;
    ParamType: PPTypeInfo;
    Access: Word;
    Name: ShortString;
  end;
  
function ClassOfTypeInfo(P: PPTypeInfo): TClass;
begin
  Result := nil;
  if Assigned(P) and (P^.Kind = tkClass) then
    Result := GetTypeData(P^).ClassType;
end;  
  
procedure GetClassInfo(ClassTypeInfo: PTypeInfo; 
  var ClassInfo: TClassInfo);
// Converts from raw RTTI structures to user-friendly Info structures
var
  TypeData: PTypeData;
  i, j: integer;
  MethodInfo: PMethodSignature;
  PublishedMethod: PPublishedMethodEntry;
  MethodParam: PMethodParam;
  ReturnRTTI: PReturnInfo;
  ParameterRTTI: PParamInfo;
  SignatureEnd: Pointer;
begin
  Assert(Assigned(ClassTypeInfo));
  Assert(ClassTypeInfo.Kind = tkClass);
  // Class
  TypeData  := GetTypeData(ClassTypeInfo);
  ClassInfo.UnitName        := TypeData.UnitName;
  ClassInfo.ClassType       := TypeData.ClassType;
  ClassInfo.Name            := TypeData.ClassType.ClassName;
  ClassInfo.ParentClass     := ClassOfTypeInfo(TypeData.ParentInfo);  
  ClassInfo.MethodCount     := GetPublishedMethodCount(ClassInfo.ClassType);
  SetLength(ClassInfo.Methods, ClassInfo.MethodCount);
  // Methods
  PublishedMethod := GetFirstPublishedMethodEntry(ClassInfo.ClassType);
  for i := Low(ClassInfo.Methods) to High(ClassInfo.Methods) do
  begin
    // Method
    MethodInfo := @ClassInfo.Methods[i];
    MethodInfo.Name       := PublishedMethod.Name;
    MethodInfo.Address    := PublishedMethod.Address;
    MethodInfo.MethodKind := mkProcedure; // Assume procedure by default
    
    // Return info and calling convention
    ReturnRTTI := Skip(@PublishedMethod.Name);
    SignatureEnd := Pointer(Cardinal(PublishedMethod) 
      + PublishedMethod.Size);
    if Cardinal(ReturnRTTI) >= Cardinal(SignatureEnd) then
    begin
      MethodInfo.CallConv := ccRegister; // Assume register calling convention 
      MethodInfo.HasSignatureRTTI := False;
    end
    else  
    begin
      MethodInfo.ResultTypeInfo := Dereference(ReturnRTTI.ReturnType);
      if Assigned(MethodInfo.ResultTypeInfo) then 
      begin
        MethodInfo.MethodKind := mkFunction;
        MethodInfo.ResultTypeName := MethodInfo.ResultTypeInfo.Name;
      end  
      else 
        MethodInfo.MethodKind := mkProcedure;
      MethodInfo.CallConv := ReturnRTTI.CallingConvention;
      MethodInfo.HasSignatureRTTI := True;
      // Count parameters
      ParameterRTTI := Pointer(Cardinal(ReturnRTTI) + SizeOf(ReturnRTTI^));
      MethodInfo.ParamCount := 0;
      while Cardinal(ParameterRTTI) < Cardinal(SignatureEnd) do
      begin
        Inc(MethodInfo.ParamCount); // Assume less than 255 parameters ;)!
        ParameterRTTI := Skip(@ParameterRTTI.Name);
      end;  
      // Read parameter info
      ParameterRTTI := Pointer(Cardinal(ReturnRTTI) + SizeOf(ReturnRTTI^));
      SetLength(MethodInfo.Parameters, MethodInfo.ParamCount);
      for j := Low(MethodInfo.Parameters) to High(MethodInfo.Parameters) do
      begin
        MethodParam := @MethodInfo.Parameters[j];
        MethodParam.Flags      := ParameterRTTI.Flags;
        if mpfResult in MethodParam.Flags 
        then MethodParam.ParamName  := 'Result'
        else MethodParam.ParamName  := ParameterRTTI.Name;
        MethodParam.TypeInfo   := Dereference(ParameterRTTI.ParamType);
        if Assigned(MethodParam.TypeInfo) then
          MethodParam.TypeName := MethodParam.TypeInfo.Name;
        MethodParam.Location   := TParamLocation(ParameterRTTI.Access);
        ParameterRTTI := Skip(@ParameterRTTI.Name);
      end;  
    end;
    PublishedMethod := GetNextPublishedMethodEntry(ClassInfo.ClassType, 
      PublishedMethod);
  end;  
end;  

initialization
  //NOTE: canot use AbstractErrorProc := @GetAbstractErrorProc;!!! it's error! this only return the address of GetAbstractErrorProc!!!!
  //so only should be this!!
  {$IFDEF Borland}
  Pointer(@AbstractErrorProc) := Pointer(GetAbstractErrorProc);
  {$ENDIF}
  {$IFDEF FPC}
  AbstractErrorProc := GetAbstractErrorProc();
  {$ENDIF}

finalization

end.
