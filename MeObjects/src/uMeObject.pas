
{Summary MeObject - the MeSDK Core - the abstract mini class(dynamic) - PMeDynamicObject.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.2 $

if you wanna the ClassParent supports for the class derived from TMeDynamicObject:

when you derived a new object from TMeDynamicObject:
  * you should override(create) one virtual method at least, or it will share the parent's VMT.
  * after it own its VMT you must set it's parent class in initialization section:
    <code>
      //set the TMeInterfacedObject's parent is TMeDynamicObject.
      SetMeVirtualMethod(TypeOf(TMeInterfacedObject), ovtVmtParent, TypeOf(TMeDynamicObject));
    </code>

if you wanna the ClassName supports for the class derived from TMeDynamicObject:

    * enable the MeRTTI_SUPPORT compiler directive(the default is on).
    * and you must do this in initialization section:
     <code>
       const cMeInterfacedObjectClassName: ShortString = 'TMeInterfacedObject';
       SetMeVirtualMethod(TypeOf(TMeInterfacedObject), ovtVmtClassName, @cMeInterfacedObjectClassName);
     </code>
       if u wanna hide the ClassName of some class you just need pass nil in it:
     <code>
       SetMeVirtualMethod(TypeOf(TMeInterfacedObject), ovtVmtClassName, nil);
     </code>

example
  How to use the TMeDynamicObject as record object:
  <code>
  var
    aObj: TMeList;
    aObj.Create; //Init VMT Ptr to aObj
    try
    finally
      aObj.Free(False); //Do NOT FREE the record memory, pass the False to Free 
    end;
  </code>

需要废弃Exception Class 使用自己的 MeExceptionObject??
No, use the EMeError instead of create new Exception Class.
All Object use the EMeError class only!

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
    * The Original Code is $RCSfile: uMeObject.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Vladimir Kladov(vk@kolmck.net) is Copyright (C) 2003-2005
    * Portions created by Riceball LEE is Copyright (C) 2006-2008
    * All rights reserved.

    * Contributor(s):
}
unit uMeObject;

interface

{$I MeSetting.inc}
{.$DEFINE PUREPASCAL}

uses
  Windows
  , SysUtils
  , TypInfo
  , uMeConsts
  , uMeSystem
  {$IFDEF DEBUG}
  , DbugIntf
  {$ENDIF}
  ;

{
NOTE:
  Dont use TypeOf() get the VMT address of the object instance, the VMT address is always the first field in the TMeDynamicObject!!
  and SizeOf() can not used!! use the TMeClass to cast!
  
  I abondon the ovtVmtPtrOffs(TypeOf use it) instead of storing the parent vmt address.
  use MeTypeOf(), MeSizeOf(), instead of the TypeOf, SizeOf()

  in my TMeDynamicObject the ovtVmtPtrOffs is always 0, so i decide use it as ovtVmtParent. It's impossible, if i change the program will be raise critical error.
}
const
  //## VMT offset
  ovtVMTAddress   = -12;  { keep the VMT address. here means the address of the ovtVMTInit }
  ovtInstanceSize = -8;   { instance size in OBJECTs    }
  //in my TMeDynamicObject the ovtVmtPtrOffs is always 0, so i decide use it as ovtVmtParent. It's impossible, if i change the program will be raise critical error.
  ovtVmtPtrOffs   = -4;   { the VMTPtr offset in the instance. It always is 0.}
  ovtVmtParent    = 8;    { point to the VMT of parent object if any }
  ovtVmtInit      = 0;    { the Init virtual method entry.}
  ovtVmtDestroy   = 4;    { the Destroy virtual method entry.}
  {$IFDEF MeRTTI_SUPPORT}
  ovtVmtClassName = 12;    { the class name PShortString. }
  {$ENDIF}
{
FPC:
   VMT=RECORD
     Size,NegSize:Longint;
     ParentLink:PVMT;
   END;

       vmtInstanceSize         = 0;
       vmtParent               = sizeof(ptrint)*2;
}

  MaxListSize = Maxint div SizeOf(Pointer);

type
  DWord = Longword;
  PMeList = ^TMeList;
  PMeDynamicObject = ^ TMeDynamicObject;

  TMeObjectMethod = procedure of object;
  TMeNotifyEvent = procedure(Sender: PMeDynamicObject) of object;
  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;

  PMeClass = ^TMeClass;
  TMeClass = ^TMeVMT;

  PMeVMT = ^TMeVMT;
  TMeVMT = record
    Init: Pointer;
    Destroy: Pointer;
    ParentClass: TMeClass;
  {$IFDEF MeRTTI_SUPPORT}
    ClassName: PChar;
  {$ENDIF}
  end;

  {Summary the virtual method table contents of TMeDynamicObject .}
  PMeVMTable = ^TMeVMTable;
  TMeVMTable = record
    VMTAddr: TMeClass;    {-12} //Point to the VMT
    Size: Integer;        {-8}
    VmtPtrOffs: Pointer;  {-4}
    VMT: TMeVMT;
  end;
  
{ TMeStream seek origins }
  TSeekOrigin = (soBeginning, soCurrent, soEnd);

  PMeStream = ^TMeStream;

  {Summary this is a helper object to visit the VMT quickly.}
  { It make the VMT as the first field.
    TMeVMTHelper:
        [VMTAddr]
    TMeDynamicObject child of It:
        TMeVMTHelper.[VMTAddr]
        TMeDynamicObject.Records
  }
  TMeVMTHelper = object
  protected
    {Summary Is called from a constructor to initialize created object instance
      filling its fields with 0. Can be overriden in descendant objects
      to add another initialization code there. (Main reason of intending
      is what constructors can not be virtual in poor objects). }
    procedure Init;virtual;
  public //Methods
     {Summary Disposes of an object instance.}
     { Description
       Do not call Destroy directly. Call Free instead. Free verifies that the object reference is not nil before calling Destroy.
     }
     destructor Destroy; virtual;
     {Summary replace 'is' operator, whether is the same object.

       Only for the object is derived from TMeVMTHelper.
       Note: the class method of the Object = the method of Object,
         and You CAN NOT use by TMeVMTHelper.ClassMethod. the Self parameter is always a instance, not the VMT pointer,
         it should a Delphi Bug.
         It can not judge the object whether it is the ancestor of the pObj.
         it should be a class method but....
     }
    function IsObject(pObj: Pointer): Boolean;
    {Summary Return the VMT Address of parent. }
    { if it is the MeObject then the parent is nil.
      it should be a class method but....
    }
    class function ClassParent: TMeClass;
    {Summary the address of virtual methods table of object.}
    { NOTE: you MUST call the constructor first before use.
    }
    {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
    class
    {$ENDIF}
    function ClassType: TMeClass;
    {Summary the size of the object instance.}
    { NOTE: you MUST call the constructor first before use, or you can use SizeOf(TMeDynamicObject), it is same.
    }
    class function InstanceSize: Integer;
    {Summary Determines the relationship of two object types.}
    {
    Use InheritsFrom to determine if a particular class type or object is an instance of a class or one of its descendants.
    InheritsFrom returns true if the object type specified in the aClass parameter is an ancestor of the object type or the
    type of the object itself. Otherwise, it returns false.
    eg, (sorry the delphi compiler do not support the class method for object. so use the MeInheritsFrom function instead.)
      TMeList.InheritsFrom(TypeOf(TMeContainer));
      TMeList.InheritsFrom(TypeOf(TMeDynamicObject));
    }
    class function InheritsFrom(aClass: TMeClass): Boolean;
    {$IFDEF MeRTTI_SUPPORT}
    class function ClassName: PChar;
    {$ENDIF}
  protected
    class function ParentClassAddress: TMeClass;virtual;abstract;
    {$IFDEF MeRTTI_SUPPORT}
    {the VMT is the ClassName PShortString address: @ClassNameAddress = PShortString.
     SO DO NOT CALL IT, use the ClassName method instead.
     NOTE: If you wanna create a new MeClass in Delphi, you should update the VMT like this:
     see initialization section!!
    }
    class function ClassNameAddress: PChar;virtual;abstract;
    {$ENDIF}
  end;

  {Summary the abstract dynamic object. }
  TMeDynamicObject = object(TMeVMTHelper)
  public //methods
     {Summary Disposes the memory allocated to an object }
     destructor DestroyMem;
     {Summary Constructor. Do not call it. Instead, use New(PMeDynamicObject, Create) function. }
     {  
     call for certain object, e.g., New(PMeList, Create);
     }
     constructor Create;

    {Summary Free the Object. Disposes the memory allocated to an object if aFreeMemRequired.}
    { 
      Before calling destructor of object, checks if passed pointer is not
        nil - similar what is done in VCL for TObject. It is ALWAYS recommended
        to use Free instead of Destroy.

        Note: It DOES NOT release huge strings, dynamic arrays and so on.
              you should be freeing in overriden the destructor - Destroy method.

      @param aFreeMemRequired whether free memory the default is true. only pointer object can free mem!!
     }
     procedure Free(aFreeMemRequired: Boolean = True);

  public
  end;

  PMeNamedObject = ^TMeNamedObject;
  TMeNamedObject = object(TMeDynamicObject)
  protected //private
    FName: String;
  public
    destructor Destroy; virtual;
    procedure Assign(const aObject: PMeNamedObject); virtual;
  public
    property Name: String read FName write FName;
  end;

  PMeComponent = ^TMeComponent;
  TMeComponent = object(TMeDynamicObject)
  protected //##private
    FName: String;
    procedure SetName(const NewName: String);
  public
    destructor Destroy; virtual;
    procedure Assign(const aObject: PMeNamedObject); virtual; {override}
  public
    {Summary Specifies the name of the component}
    property Name: String read FName write SetName;
  end;

  PMeInterfacedObject = ^TMeInterfacedObject;
  TMeInterfacedObject = object(TMeDynamicObject)
  protected
    FOnDestroy: TMeNotifyEvent;
    FRefCount: Integer;
    {Summary the referencer list}
    FFreeNotifies: PMeList;
  protected
     {Summary It is called in destructor to perform OnDestroy event call and to
        released objects, added to FFreeNotifies list. }
    procedure Done;

  public
    { it will be Destroyed only when reference count < 0 }
    destructor Destroy; virtual;
    { Summary Increments the reference count for this instance and returns the new reference count.}
    function AddRef: Integer;
     {Summary Decrements reference count for this instance.

      If it is becoming <0, and Free
        method was already called, object is (self-) destroyed. Otherwise,
        Free method does not destroy object, but only sets flag
        "Free was called": Decrements reference count.

        Use AddRef..Release to provide a block of code, where
        object can not be destroyed by call of Free method.
        This makes code more safe from intersecting flows of processing,
        where some code want to destroy object, but others suppose that it
        is yet existing.

        If You want to release object at the end of block AddRef..Release,
        do it immediately BEFORE call of last Release (to avoid situation,
        when object is released in result of Release, and attempt to
        destroy it follow leads to AV exception).
     }
    function Release: Integer;


    {Summary Adds an object to the DependentList. }
    { Description 
      that means this obj is relying on it. the objects in the the DependentList will be free
       when the it is destroyed. 
       
       表明 Obj 依赖于自己. 当自己被释放后, Obj 也将会被释放.
    }
    procedure AddDependent(Obj: PMeDynamicObject);
    {Summary This Notification Proc will be executed before the object is free. }
    procedure FreeNotification(Proc: TMeObjectMethod);
    procedure RemoveFreeNotification(Proc: TMeObjectMethod);
  public
    {Summary Indicates the number of instance pointers currently dependent upon the object.}
    property RefCount: Integer read FRefCount;
    //property FreeNotifies: PMeList read FFreeNotifies;

    property OnDestroy: TMeNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  PMeContainer = ^ TMeContainer;
  TMeContainer = Object(TMeDynamicObject)
  protected
    FCount: Integer;
  public
    class procedure Error(const Msg: string; Data: Integer);overload;
    class procedure Error(Msg: PResStringRec; Data: Integer);overload;
  public
    property Count: Integer read FCount write FCount;
  end;

  TMeListGrowEvent = procedure(Sender: PMeDynamicObject; NewCapacity: Integer) of object;
  {Summary Simple list of pointers. It is used instead of standard VCL TList.}
  { Description
      TMeList stores any kind data (or pointers to these ones). Can be created
     calling function New(PMeList, Create) or use as the record object directly. 
  }
  TMeList = object(TMeContainer)
  protected
    FItems: PPointerList;
    FCapacity: Integer;
    FIsExternalList: Boolean;
    FOnListGrow: TMeListGrowEvent;
    FOnListClear:TMeNotifyEvent;
    procedure SetCount(const Value: Integer);

    procedure SetCapacity(Value: Integer);
    procedure SetList(Value: PPointerList);
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Value: Pointer);
    procedure Grow;
    //procedure Init; virtual;
  public
    {Summary Destroys list, freeing memory, allocated for pointers. Programmer
       is resposible for destroying of data, referenced by the pointers. }
    destructor Destroy; virtual;


    {Summary Makes Count equal to 0. Not responsible for freeing (or destroying)
       data, referenced by released pointers. }
    procedure Clear; //##virtual; DO NOT USE virtual for common method, so that I can use the TMeList as record object.
    {Summary Inserts a new item at the end of the list.}
    { Call Add to insert a new object at the end of the Items array.
      Add increments Count and, if necessary, allocates memory by increasing the value of Capacity.
      Note:	Add always inserts the Item pointer at the end of the Items array, even if the Items array contains nil (Delphi) or NULL (C++) pointers.
    }
    function Add(Value: Pointer): Integer;
    {Summary popup the last item in the list or nil if list is empty.}
    { 
      use the Add to push.
    }
    function Popup: Pointer;
    {Summary Inserts pointer before given item. Returns Index, i.e. index of
       inserted item in the list. Indeces of items, located after insertion
       point, are increasing. To add item to the end of list, pass Count
       as index parameter. To insert item before first item, pass 0 there. }
    procedure Insert(Index: Integer; Value: Pointer);
    {Summary Searches first (from start) item pointer with given value and returns
       its index (zero-based) if found. If not found, returns -1. }
    function IndexOf(Value: Pointer): Integer;
    {Summary Deletes given (by index) pointer item from the list, shifting all
       follow item indeces up by one. }
    procedure Delete(Index: Integer);
    {Summary Deletes Len items starting from Index. }
    procedure DeleteRange(Index, Len: Integer);
    {Summary Removes first entry of a Value in the list. }
    procedure Remove(Value: Pointer);
    {Summary Returns the last item (or nil, if the list is empty). }
    function Last: Pointer;
    {Summary Swaps two items in list directly (fast, but without testing of
       index bounds). }
    procedure Swap(Idx1, Idx2: Integer);
    {Summary Moves item to new position. Pass NewIndex >= Count to move item
       after the last one. }
    procedure Move(CurIndex, NewIndex: Integer);
    {Summary Deletes all nil (Delphi) or NULL (C++) items from the Items array.}
    procedure Pack;
    {Summary Especially for lists of pointers to dynamically allocated memory.
       free all pointed memory blocks via FreeMem. }
    procedure FreePointers;
    {Summary Especially for a list of objects derived from TMeDynamicObject.
       Calls Free for every of the object in the list. }
    procedure FreeMeObjects(aFreeMemRequired: Boolean = True);
    {Summary Copies all source list items. }
    procedure Assign(SrcList: PMeList);
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    {Summary Adds a list of items given by a dynamic array. }
    procedure AddItems(const aItems: array of Pointer);
    {$ENDIF}
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: PMeStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: PMeStream);
  public
    {Summary Returns count of items in the list. It is possible to delete a number
       of items at the end of the list, keeping only first Count items alive,
       assigning new value to Count property (less then Count it is). }
    property Count: Integer read FCount write SetCount;
    {Summary Returns number of pointers which could be stored in the list
       without reallocating of memory. It is possible change this value
       for optimize usage of the list (for minimize number of reallocating
       memory operations). }
    property Capacity: Integer read FCapacity write SetCapacity;
    {Summary Provides access (read and write) to items of the list. Please note,
       that TMeList is not responsible for freeing memory, referenced by stored
       pointers. }
    property Items[Index: Integer]: Pointer read Get write Put; default;
    {Summary Raw data memory. Can be used for direct access to items of a list. }
    property List: PPointerList read FItems write SetList;
    {Summary the List manage by self, or not. }
    { Description 
     if not, u should assign these events: OnListGrow, OnListClear
    }
    property IsExternalList: Boolean read FIsExternalList write FIsExternalList default False;
    property OnListClear: TMeNotifyEvent read FOnListClear write FOnListClear;
    property OnListGrow:  TMeListGrowEvent read FOnListGrow write FOnListGrow;
  end;
//[END OF TMeList DEFINITION]

  PMeStrings = ^TMeStrings;
  TMeStrings = object(TMeContainer)
  private
    function GetItemLen(Idx: Integer): Integer;
    function GetObject(Idx: Integer): LongWord;
    procedure SetObject(Idx: Integer; const Value: LongWord);
    function GetValue(AName: PChar): PChar;
    function GetName(const Index: Integer): string;
  protected
    procedure Init; virtual; {override}
  protected
    FList: PMeList;
    FCaseSensitiveSort: Boolean;
    FTextBuf: PChar;
    FTextSize: LongWord;
    FUsedSize: LongWord;
  protected
    procedure ProvideSpace( AddSize: LongWord );
    function Get(Idx: integer): string;
    function GetTextStr: string;
    procedure Put(Idx: integer; const Value: string);
    procedure SetTextStr(const Value: string);
    function GetPChars( Idx: Integer ): PChar;

    destructor Destroy; virtual;(*override;*)
  public
    {Summary Adds Ansi String to a list. }
    function Add(const S: String): Integer;
    {Summary Adds Ansi String and correspondent object to a list. }
    function AddObject(const S: String; Obj: LongWord): Integer;
    {Summary Adds a string to list. }
    function AddPChar(S: PChar): integer;
    {Summary Adds a string to list. The string can contain #0 characters. }
    function AddPCharLen(S: PChar; Len: Integer): integer;
    {Summary Adds a group of strings to the list}
    {
    Call AddStrings to add the strings from another TStrings object to the list. If both the source and destination TStrings objects support objects associated with their strings, references to the associated objects will be added as well.
    }
    procedure AddStrings(Strings: PMeStrings);
    {Summary Compares the list of strings to the list from another TStrings object and returns true if the two lists match.}
    {
    Call Equals to compare the lists in two TStrings objects. Equals compares only the strings, not any references to associated objects. 
    Equals returns true if the lists for both TStrings objects have the same number of strings and the strings in each list match when 
    compared using the protected CompareStrings method. Equals returns false if the lists are different in length, if they contain different 
    strings, or if the order of the strings in the two lists differ.
    }
    function Equals(Strings: PMeStrings): Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: PMeStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: PMeStream);
  public
    {Summary Makes string list empty. }
    procedure Clear;
    {Summary Deletes string with given index (it *must* exist). }
    procedure Delete(Idx: integer);
    {Summary Returns index of first string, equal to given one. }
    function IndexOf(const S: string): integer;
    {Summary Returns index of object, equal to given one. }
    function IndexOfObject(const aObj: LongWord): integer;
    {Summary Returns index of first string, equal to given one (while comparing it
       without case sensitivity). }
    function IndexOf_NoCase(const S: string): integer;
    {Summary Returns index of first string, equal to given one (while comparing it
       without case sensitivity). }
    function IndexOfStrL_NoCase( Str: PChar; L: Integer ): integer;
    {Summary Searches string starting from 'AName=' in string list like ini-file. }
    function IndexOfName(AName: PChar): Integer;
    {Summary Returns Index of the first string, equal or greater to given pattern, but
       works only for sorted TMeStrings object. Returns TRUE if exact string found,
       otherwise nearest (greater then a pattern) string index is returned,
       and the result is FALSE. }
    function Find(const S: String; var Index: Integer): Boolean;
    {Summary Inserts ANSI string before one with given index. }
    procedure Insert(Idx: integer; const S: String);
    {Summary Inserts ANSI string before one with given index. }
    procedure InsertObject(Idx: integer; const S: String; Obj: LongWord);
    {Summary Inserts string before one with given index. }
    procedure InsertPChar(Idx: integer; S: PChar);
    {Summary Inserts string from given PChar. It can contain #0 characters. }
    procedure InsertPCharLen( Idx: Integer; S: PChar; Len: Integer );
    {Summary Moves string to another location. }
    procedure Move(CurIndex, NewIndex: integer);
    {Summary Allows to set strings of string list from given string (in which
       strings are separated by $0D,$0A or $0D characters). Text can
       contain #0 characters. Works very fast. This method is used in
       all others, working with text arrays (LoadFromFile, MergeFromFile,
       Assign, AddStrings). }
    procedure SetText(const S: string; Append2List: boolean);
    {Summary Last item (or '', if string list is empty). }
    function Last: String;
    {Summary Swaps to strings with given indeces. }
    procedure Swap(Idx1, Idx2 : Integer);
    {Summary Call it to sort string list. }
    procedure Sort(CaseSensitive: Boolean);
  public
    {Summary Adds string S (null-terminated) with associated object Obj. }
    function AddPCharObject(S: PChar; Obj: LongWord): Integer;
    {Summary Adds string S of length Len with associated object Obj. }
    function AddObjectLen(S: PChar; Len: Integer; Obj: LongWord): Integer;
    {Summary Inserts string S (null-terminated) at position Idx in the list,
       associating it with object Obj. }
    procedure InsertPCharObject(Idx: Integer; S: PChar; Obj: LongWord);
    {Summary Inserts string S of length Len at position Idx in the list,
       associating it with object Obj. }
    procedure InsertObjectLen( Idx: Integer; S: PChar; Len: Integer; Obj: LongWord );
  public
    {Summary Appends S (null-terminated) to the last string in FastStrListEx object, very fast. }
    procedure AppendPChar(S: PChar);
    {Summary Appends S of length Len to the last string in FastStrListEx object, very fast. }
    procedure AppendPCharLen( S: PChar; Len: Integer );
    {Summary Converts N to hexadecimal and appends resulting string to the last
       string, very fast. }
    procedure AppendInt2Hex( N: LongWord; MinDigits: Integer );
  public
    {Summary the Clear method only clear FList.count to zero if true.}
    FastClear: Boolean;

    {Summary Access to objects associated with strings in the list. }
    property Objects[Idx: Integer]: LongWord read GetObject write SetObject;
    {Summary Returns a value correspondent to the Name an ini-file-like string list
       (having Name1=Value1 Name2=Value2 etc. in each string). }
    property Values[Name: PChar]: PChar read GetValue;
    property Names[const index: Integer]: string read GetName;
    {Summary Number of strings in a string list. }
    property Count: integer read fCount;
    {Summary Strings array items. If item does not exist, empty string is returned.
       But for assign to property, string with given index *must* exist. }
    property Items[Idx: integer]: string read Get write Put; default;
    {Summary Fast access to item strings as PChars. }
    property ItemPtrs[Idx: Integer]: PChar read GetPChars;
    {Summary Length of string item. }
    property ItemLen[Idx: Integer]: Integer read GetItemLen;
    {Summary Content of string list as a single string (where strings are separated
       by characters $0D,$0A). }
    property Text: string read GetTextStr write SetTextStr;
  end;

  {Summary the Dynamic Memory can auto increase the memory size}
  PMeDynamicMemory = ^TMeDynamicMemory;
  TMeDynamicMemory = object(TMeDynamicObject)
  protected
    FMemory: Pointer;
    FSize: Integer;
    FUsedSize: Integer;
    {$IFDEF MeDynamicMemory_HoldMem_SUPPORT}
    FHoldMem: Boolean;
    {$ENDIF}
  protected
    procedure Init;virtual; {override}
    procedure Grow(const aSize: Integer = 4);
    //function GetMemory: Pointer;
    procedure SetSize(const Value: Integer);
    procedure SetUsedSize(const Value: Integer);
  public
    destructor Destroy; virtual; {override}
    procedure Clear;
    procedure AddInt(const aValue: Integer);
    procedure AddDouble(const aValue: Double);
    procedure AddByte(const aValue: Byte);
    procedure AddWord(const aValue: Word);
    procedure AddPChar(const aValue: string);
    procedure AddBuffer(const aValue; aSize: Integer);
    procedure Align;
    procedure AllocSpace(const aSize: Integer);
    procedure Assign(const aMem: PMeDynamicMemory);
  public
    {$IFDEF MeDynamicMemory_HoldMem_SUPPORT}
    {Summary do not free the Memory if true.}
    property HoldMem: Boolean read FHoldMem write FHoldMem;
    {$ENDIF}
    property Memory: Pointer read FMemory;
    //the total memory size
    property Size: Integer read FSize write SetSize;
    //the current used memory size
    property UsedSize: Integer read FUsedSize write SetUsedSize;
  end;

  TMeStream = object(TMeDynamicObject)
  private
    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    procedure SetSize64(const NewSize: Int64);
  protected
    function GetSize: Int64; virtual;
    procedure SetSize(const NewSize: Int64); virtual;
  public
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; virtual; abstract;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual;
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    //the position is End Of Stream
    function EOF: Boolean;
    function CopyFrom(Source: PMeStream; Count: Int64): Int64;
    procedure WriteResourceHeader(const ResName: string; out FixupInfo: Integer);
    procedure FixupResourceHeader(FixupInfo: Integer);
    procedure ReadResHeader;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
  end;

  PMeNamedObjects = ^ TMeNamedObjects;
  TMeNamedObjects = object(TMeList)
  protected
    function GetItem(Index: Integer): PMeNamedObject;
  public
    destructor Destroy; virtual;{override}
    procedure Clear;
    procedure Assign(const aObjs: PMeNamedObjects);
    function IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer;
    function Find(const aName: string): PMeNamedObject;
  public
    property Items[Index: Integer]: PMeNamedObject read GetItem; default;
  end;

var 
  {Summary An table to convert char to uppercase very fast. First call InitUpper. }
  Upper: array[Char] of Char;
  Upper_Initialized: Boolean;

{Summary Call this fuction ones to fill Upper[ ] table before using it. }
procedure InitUpper;

{Summary Returns pointer to newly created TMeList object. Use it instead usual
   TMeList.Create as it is done in VCL or XCL. }
function NewList: PMeList;
function NewStrings: PMeStrings;
{ create an instance for the aClass }
{ 
  I got the idea.
  Limits: all class constructor Create method Must be no parameters.
}
function NewMeObject(const aClass: TMeClass): PMeDynamicObject;

{$IFDEF COMPILER4_UP}
{Summary Creates a list filling it initially with certain Items. }
function NewListInit(const AItems: array of Pointer): PMeList;
{$ENDIF}


{Summary Very fast fill Value to List elements from List[FromIdx] to List[FromIdx+Count-1].
   Given elements must exist. Count must be > 0. }
procedure FillListIn(List: TMeList; FromIdx, Count, Value: Integer);

function FindMeComponent(const Name: String): PMeComponent;
function GComponentNameList: PMeList;

{Summary Obj.Free and Obj := nil, where Obj *MUST* be TMeDynamicObject or its descendant
   (TControl, TMenu, etc.) This procedure is not compatible with VCL's
   FreeAndNil, which works with TMeObject ect, since this it has another name. }
procedure MeFreeAndNil(var Obj); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{Summary MeTypeOf for Ojbect instance }
{ MeTypeOf(aObj) = TypeOf(aObj) = TMeClass(aObj) 
  It Should cast the aObj directly !! the MeTypeOf is not useful!
}
function MeTypeOf(const aObj: TMeVMTHelper): TMeClass; {$IFDEF PUREPASCAL}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
function MeSizeOf(const aObj: TMeVMTHelper): Integer; {$IFDEF PUREPASCAL}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}

function SetMeVirtualMethod(const aClass: TMeClass; const Offset: Integer; const Method: Pointer): Pointer;

//##[String FUNCTIONS DECLARATIONS]
{Summary Compare two strings fast without case sensitivity.
   Terminating 0 is not considered, so if strings are equal,
   comparing is continued up to MaxLen bytes.
   Since this, pass minimum of lengths as MaxLen. }
function StrLComp_NoCase(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;


{
  Data sorting (quicksort implementation)
  This part contains implementation of 'quick sort' algorithm,
   based on following code:

<pre>
 TQSort by Mike Junkin 10/19/95.
 DoQSort routine adapted from Peter Szymiczek's QSort procedure which
 was presented in issue#8 of The Unofficial Delphi Newsletter.

 TQSort changed by Vladimir Kladov (Mr.Bonanzas) to allow 32-bit
 sorting (of big arrays with more than 64K elements).
</pre>

  Finally, this sort procedure is adapted to XCL (and then to KOL)
  requirements (no references to SysUtils, Classes etc. TQSort object
  is transferred to a single procedure call and DoQSort method is
  renamed to SortData - which is a regular procedure now). }

//##[Sorting TYPES]
type
  {Summary Event type to define comparison function between two elements of an array.
     This event handler must return -1 or +1 (correspondently for cases e1 less than e2
     and e2 greater than e2). Items are enumerated from 0 to uNElem. }
  TCompareEvent = function (const Data: Pointer; const e1,e2 : LongWord) : Integer;
  {Summary Event type to define swap procedure which is swapping two elements of an
     array. }
  TSwapEvent = procedure (const Data : Pointer; const e1,e2 : LongWord);

//##[SortData FUNCTIONS DECLARATIONS]
{Summary Call it to sort any array of data of any kind, passing total
   number of items in an array and two defined (regular) function
   and procedure to perform custom compare and swap operations.
   First procedure parameter is to pass it to callback function
   CompareFun and procedure SwapProc. Items are enumerated from
   0 to uNElem-1. }
procedure SortData(const Data: Pointer; const uNElem: LongWord;
                    const CompareFun: TCompareEvent;
                    const SwapProc: TSwapEvent);

{Summary Use this function as the last parameter for SortData call when a PMeList
   object is sorting. SwapListItems just exchanges two items of the list. }
procedure SwapListItems(const L: Pointer; const e1, e2: LongWord);

{Summary procedure to sort array of integers. }
procedure SortIntegerArray(var A : array of Integer);

{Summary Procedure to sort array of unsigned 32-bit integers.}
procedure SortDwordArray(var A : array of LongWord);

function MeInheritsFrom(aClass: TMeClass; const aParentClass: TMeClass): Boolean;

implementation

uses
  uMeStream
{$IFDEF BORLAND}
  {$IFDEF Compiler6_UP}
  , RTLConsts, SysConst
  {$ELSE}
  , Consts
  {$ENDIF}
{$ELSE}
  , RTLConsts, SysConst
{$ENDIF}
  ;

function MeInheritsFrom(aClass: TMeClass; const aParentClass: TMeClass): Boolean;
{$IFDEF PUREPASCAL}
begin
  while (aClass <> nil) and (aClass <> aParentClass) do
    aClass := aClass.ParentClass;
  Result := aClass = aParentClass;
end;
{$ELSE}
asm
        { ->    EAX     Pointer to our class    }
        {       EDX     Pointer to aParentClass }
        { <-    AL      Boolean result          }
@@loop:
        CMP     EAX,EDX
        JE      @@success
        MOV     EAX,[EAX].ovtVmtParent
        TEST    EAX,EAX
        JNE     @@loop
        JMP     @@exit
@@success:
        MOV     AL,1
@@exit:
end;
{$ENDIF}

function NewMeObject(const aClass: TMeClass): PMeDynamicObject;
{$IFDEF PUREPASCAL}
var
  vSize: Integer;
begin
  if Assigned(aClass) then
  begin
    vSize := Integer(aClass);
    Inc(vSize, ovtInstanceSize);
    vSize := PInteger(vSize)^;
    GetMem(Result, vSize);
    {$IFDEF COMPILER4_UP}
    FillChar(Result^, vSize, 0);
    {$ENDIF}
    PInteger(Result)^ := Integer(aClass);
    Result.Init;
    //writeLn('__________NewMeObject:Size:', vSize);
  end
  else
   Result := nil;
end;
{$ELSE}
asm
  MOV  EDX, aClass
  XOR  EAX, EAX
  JMP  TMeDynamicObject.Create
end;
{$ENDIF}

function SetMeVirtualMethod(const aClass: TMeClass; const Offset: Integer; const Method: Pointer): Pointer;
var
  PatchAddress: PPointer;
begin
  PatchAddress := Pointer(Integer(aClass) + Offset);
  ReadMem(PatchAddress, @Result, SizeOf(Result));
  if Result <> Method then
    WriteMem(PatchAddress, @Method, SizeOf(Method))
  else
    Result := nil;
end;

var
  //keep the all objects derived from TMeDynamicObject
  FObjectNameList: PMeList;

function GComponentNameList: PMeList;
begin
  if FObjectNameList = nil then
    FObjectNameList := NewList;
  Result := FObjectNameList;
end;

function FindMeComponent(const Name: String): PMeComponent;
var i: Integer;
    Obj: PMeComponent;
begin
  if FObjectNameList = nil then
    FObjectNameList := NewList;
  for i := 0 to FObjectNameList.Count-1 do
  begin
    Obj := FObjectNameList.Items[i];
    if Name = Obj.FName then
    begin
      Result := Obj; 
      Exit;
    end;
  end;
  Result := nil;
end;

procedure _FillDataIn(DataArray: Pointer; Value, Count: Integer);
{$IFDEF PUREPASCAL}
begin
  while Count > 0 do
  begin
    PInteger(DataArray)^ := Value;
    Inc(Cardinal(DataArray), SizeOf(Integer)); 
    Dec(Count);
  end;
end;
{$ELSE}
asm
  PUSH ESI
  PUSH EDI
  {$IFDEF FPC}
  MOV ESI, [DataArray]
  MOV EAX, [Value]
  MOV ECX, [Count]
  {$ELSE DELPHI}
  MOV ESI, EAX
  MOV EAX, EDX
  {$ENDIF FPC/DELPHI}
  MOV EDI, ESI
  CLD

@@1:
  //LODSD
  //ADD EAX, EDX
  REP STOSD
  //LOOP @@1

  POP EDI
  POP ESI
end {$IFDEF FPC} ['EAX', 'EDX', 'ECX'] {$ENDIF};
{$ENDIF}

procedure FillListIn(List: TMeList; FromIdx, Count, Value: Integer);
begin
  _FillDataIn(@List.FItems[FromIdx], Value, Count);
end;

procedure MeFreeAndNil(var Obj);
var 
  vObj: PMeDynamicObject;
begin
  if Pointer(Obj) <> nil then
  begin
    vObj := PMeDynamicObject(Obj);
    Pointer(Obj) := nil;
    vObj.Free;
  end;
end;

function MeTypeOf(const aObj: TMeVMTHelper): TMeClass;
{$IFDEF PUREPASCAL}
begin
  Result := TMeClass(aObj);
end;
{$ELSE PUREPASCAL}
asm
  //the const aObj is a pointer!
  //the first field is the VMT Address.
  //MOV EAX, [EAX]
end;
{$ENDIF PUREPASCAL}

function MeSizeOf(const aObj: TMeVMTHelper): Integer;
{$IFDEF PUREPASCAL}
begin
  //Result := SizeOf(Self);
  //Pointer(Result) := MeTypeOf(aObj);
  Result := Integer(aObj);
  Result := PInteger(Result + ovtInstanceSize)^;
end;
{$ELSE PUREPASCAL}
asm
  //MOV EAX, [EAX]
  MOV EAX, [EAX].ovtInstanceSize
  //MOV EAX, [EAX + ovtInstanceSize]
end;
{$ENDIF PUREPASCAL}

destructor TMeVMTHelper.Destroy;
begin
end;

procedure TMeVMTHelper.Init;
begin
{$IFNDEF COMPILER4_UP} //fill 0 for D2, D3
  FillChar(Pointer(Integer(@Self) + SizeOf(Integer))^, Sizeof(Self) - SizeOf(Integer), 0);
{$ENDIF}
end;

class function TMeVMTHelper.InstanceSize: Integer;
{$IFDEF PUREPASCAL}
begin
  {.$IFDEF Delphi_ObjectTypeClassMethod_BUG}
  //Result := PInteger(Self)^;
  {.$ELSE}
  Result := Integer(Self);
  {.$ENDIF}
  Result := PInteger(Result + ovtInstanceSize)^;
end;
{$ELSE PUREPASCAL}
asm
  {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
  MOV EAX, [EAX]
  {$ENDIF}
  MOV EAX, [EAX].ovtInstanceSize
  //MOV EAX, [EAX + ovtInstanceSize]
end;
{$ENDIF PUREPASCAL}

{$IFDEF Delphi_ObjectTypeClassMethod_BUG}
class 
{$ENDIF}
function TMeVMTHelper.ClassType: TMeClass;
{$IFDEF PUREPASCAL}
begin
  //Result := TypeOf(Self); //Do not use TypeOf
  Result := PPointer(@Self)^;
end;
{$ELSE PUREPASCAL}
asm
  //the first field is the VMT Address.
  MOV EAX, [EAX]
end;
{$ENDIF PUREPASCAL}

class function TMeVMTHelper.ClassParent: TMeClass;
{$IFDEF PUREPASCAL}
begin
  {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
  Pointer(Result) := PPointer(PInteger(Self)^ + ovtVmtParent)^;
  {$ELSE}
  Pointer(Result) := PPointer(Integer(Self) + ovtVmtParent)^;
  {$ENDIF}
  //if Result <> nil then
    //Pointer(Result) := PPointer(Result)^;
end;
{$ELSE}
asm
       {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
        MOV     EAX,[EAX]
       {$ENDIF}
        MOV     EAX,[EAX].ovtVmtParent
        TEST    EAX,EAX
        //JE      @@exit
        //MOV     EAX,[EAX]
@@exit:
end;
{$ENDIF}

class function TMeVMTHelper.InheritsFrom(aClass: TMeClass): Boolean;
{$IFDEF PUREPASCAL}
var
  ClassPtr: TMeClass;
begin
  {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
  ClassPtr := PPointer(@Self)^;
  {$ELSE}
  ClassPtr := Pointer(@Self);
  {$ENDIF}
  while (ClassPtr <> nil) and (ClassPtr <> AClass) do
    ClassPtr := ClassPtr.ParentClass;
  Result := ClassPtr = AClass;
end;
{$ELSE}
asm
        { ->    EAX     Pointer to our class    }
        {       EDX     Pointer to AClass               }
        { <-    AL      Boolean result          }
       {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
        MOV     EAX,[EAX]
       {$ENDIF}
@@loop:
        CMP     EAX,EDX
        JE      @@success
        MOV     EAX,[EAX].ovtVmtParent
        TEST    EAX,EAX
        JNE     @@loop
        JMP     @@exit
@@success:
        MOV     AL,1
@@exit:
end;
{$ENDIF}

function TMeVMTHelper.IsObject(pObj: Pointer): Boolean;
{$IFDEF PUREPASCAL}
begin
  Result := pObj <> nil;
  if Result then
  begin
    Result := PPointer(@Self)^ = PPointer(pObj)^;
  end;
end;
{$ELSE PUREPASCAL}
asm
        MOV     ECX, [EAX]  //the Object VMT Address in ECX
        MOV     EAX, EDX
        TEST    EAX, EAX
        JE      @@exit     //the pObj is nil.
        MOV     EAX,[EAX]  //get the VMT of pObj
        CMP     EAX,ECX
        JNE     @@failed     //Not the same VMT

@@success:
        MOV     AL,1
        JMP     @@exit
@@failed:
        MOV     AL,0
@@exit:
end;
{$ENDIF PUREPASCAL}

{$IFDEF MeRTTI_SUPPORT}
class function TMeVMTHelper.ClassName: PChar;
{$IFDEF PUREPASCAL}
(*
var
  p: PShortString;
begin
  {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
  p := PShortString(PPointer(PInteger(@Self)^ + ovtVmtClassName)^);
  {$ELSE}
  p := PShortString(PPointer(Integer(@Self) + ovtVmtClassName)^);
  {$ENDIF}
  //Result := PShortString(@TMeVMTHelper.ClassNameAddress)^;
  if Assigned(p) then
    Result := p^
  else
    Result := '';
end;
*)
begin
  Result := PPointer(PInteger(@Self)^ + ovtVmtClassName)^;
end;

{$ELSE PUREPASCAL}
asm
        {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
        MOV     EAX, [EAX]  //this is a Delphi Bug: the Class method in Object do not pass the VMT
        {$ENDIF}
        MOV     EAX,[EAX].ovtVmtClassName
  
(*
        { ->    [EAX] VMT                         }
        {       EDX Pointer to result string    }
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EDX
        {$IFDEF Delphi_ObjectTypeClassMethod_BUG}
        MOV     EAX, [EAX]  //this is a Delphi Bug: the Class method in Object do not pass the VMT
        {$ENDIF}
        MOV     ESI,[EAX].ovtVmtClassName
        XOR     ECX,ECX
        CMP     ESI, ECX
        JZ      @@IsZero

        MOV     CL,[ESI]
        INC     ECX
        REP     MOVSB
        JMP     @@exit
@@IsZero:
        MOV     [EDI], ECX
@@exit:
        POP     EDI
        POP     ESI
*)
end;
{$ENDIF PUREPASCAL}
{$ENDIF}

constructor TMeDynamicObject.Create;
{$IFDEF PUREPASCAL}
begin
  Init;
end;
{$ELSE PUREPASCAL}
asm
        //CALL      System.@ObjSetup - Generated always by compiler
        //JZ        @@exit

        PUSH      EAX
        MOV       EDX, [EAX].ovtVmtInit
        CALL      LongWord ptr [EDX]
        POP       EAX

//@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TMeDynamicObject.Free(aFreeMemRequired: Boolean);
{$IFDEF PUREPASCAL}
begin
  if @Self <> nil then
  begin
    Destroy;
    if aFreeMemRequired then
      FreeMem(@Self);
  end;
end;
{$ELSE PUREPASCAL}
asm
   TEST    EAX,EAX
   JNE     @@IsNotNil
@@IsNotNil:
   TEST    EDX,EDX
   JNE     DestroyMem
   JMP     Destroy
end;
{$ENDIF PUREPASCAL}

destructor TMeDynamicObject.DestroyMem;
begin
  Destroy;
  FreeMem(@Self);
end;

{ TMeNamedObject }
destructor TMeNamedObject.Destroy;
begin
  FName := '';
end;

procedure TMeNamedObject.Assign(const aObject: PMeNamedObject); 
begin
  if Assigned(aObject) and aObject.InheritsFrom(TypeOf(TMeNamedObject)) then
    with PMeNamedObject(aObject)^ do
    begin
      Self.FName   := FName;
    end;
end;

{ TMeComponent }
destructor TMeComponent.Destroy;
{$IFDEF PUREPASCAL}
begin
  SetName('');
  //inherited;
end;
{$ELSE PUREPASCAL} 
asm
        PUSH  EAX
        XOR   EDX, EDX
        CALL  SetName
        POP   EAX
        //JMP   TMeDynamicObject.Destroy
end;
{$ENDIF}

procedure TMeComponent.SetName(const NewName: String);
begin
  if FName <> '' then
  begin
    FObjectNameList.Remove(@Self);
    FName := '';
  end;
  if NewName <> '' then
  begin
    if FindMeComponent(NewName) <> nil then Exit; // prevent duplications!
    FName := NewName;
    FObjectNameList.Add(@Self);
  end;
end;

procedure TMeComponent.Assign(const aObject: PMeNamedObject);
begin
end;

{ TMeInterfacedObject }
destructor TMeInterfacedObject.Destroy;
begin
  if InterlockedDecrement(FRefCount) < 0 then
  begin
    Done;
    inherited Destroy;
  end;
end;

function TMeInterfacedObject.Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result < 0 then
    Destroy;
end;

function TMeInterfacedObject.AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

procedure TMeInterfacedObject.Done;
{$IFDEF PUREPASCAL}
var I: Integer;
    ProcMethod: TMethod;
    Proc: TMeObjectMethod Absolute ProcMethod;
begin
  if Assigned(FOnDestroy) then
  begin
    FOnDestroy(@Self);
    FOnDestroy := nil;
  end;
  if FFreeNotifies <> nil then
  begin
    for I := 0 to FFreeNotifies.FCount div 2 - 1 do
    begin
      ProcMethod.Code := FFreeNotifies.FItems[I * 2];
      ProcMethod.Data := FFreeNotifies.FItems[I * 2 + 1];
      Proc;
    end;
    FFreeNotifies.Free;
    FFreeNotifies := nil;
  end;
end;
{$ELSE PUREPASCAL} 
asm     
        XOR      ECX, ECX
        XCHG     ECX, [EAX].FOnDestroy.TMethod.Code
        JECXZ    @@NotifyDependent
        PUSH     EAX
        XCHG     EDX, EAX
        MOV      EAX, [EDX].FOnDestroy.TMethod.Data
        CALL     ECX
        POP      EAX
@@NotifyDependent:
        XOR      ECX, ECX
        XCHG     ECX, [EAX].FFreeNotifies
        JECXZ    @@exit
        PUSH     ESI
        PUSH     ECX
        MOV      ESI, [ECX].TMeList.FItems
        MOV      ECX, [ECX].TMeList.FCount
@@NotifyDependentLoop:
        LODSD
        XCHG     EDX, EAX
        LODSD
        PUSH     ECX
        CALL     EDX
        POP      ECX
        DEC      ECX
        LOOP     @@NotifyDependentLoop
        POP      EAX
        CALL     TMeDynamicObject.Free  //FFreeNotifies.Free
        POP      ESI
@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TMeInterfacedObject.AddDependent(Obj: PMeDynamicObject);
{$IFDEF PUREPASCAL}
begin
  if FFreeNotifies = nil then
    FFreeNotifies := NewList;
  FFreeNotifies.Insert(0, Obj);
  FFreeNotifies.Insert(0, Pointer(@TMeDynamicObject.Free));
end;
{$ELSE PUREPASCAL} 
asm
        PUSH     EBX
        PUSH     EDX
        XCHG     EBX, EAX
        MOV      EAX, [EBX].FFreeNotifies
        TEST     EAX, EAX
        JNZ      @@1
        CALL     NewList
        MOV      [EBX].FFreeNotifies, EAX
@@1:    MOV      EBX, EAX
        XOR      EDX, EDX
        POP      ECX
        CALL     TMeList.Insert
        XCHG     EAX, EBX
        XOR      EDX, EDX
        MOV      ECX, offset TMeDynamicObject.Free
        //XOR      ECX, ECX
        CALL     TMeList.Insert
        POP      EBX
end;
{$ENDIF PUREPASCAL}

procedure TMeInterfacedObject.RemoveFreeNotification(Proc: TMeObjectMethod);
var
  i: Integer;
{$IFDEF FPC}
  Ptr1, Ptr2: Pointer;
{$ENDIF FPC}
begin
  if Assigned(FFreeNotifies) then
    for i := 0 to FFreeNotifies.Count div 2 - 1 do
    begin
      {$IFDEF FPC}
      asm
        MOV  EAX, [Proc]
        MOV  [Ptr1], EAX
        MOV  EAX, [Proc+4]
        MOV  [Ptr2], EAX
      end ['EAX'];
      if (FFreeNotifies.Items[i*2] = Ptr1) and (FFreeNotifies.Items[i*2] = Ptr2) then
      begin
      	FFreeNotifies.DeleteRange(i*2, 2);
      	break;
      end;
      {$ELSE DELPHI}
      if (FFreeNotifies.Items[i*2] = TMethod(Proc).Code) and (FFreeNotifies.Items[i*2+1] = TMethod(Proc).Data) then
      begin
      	FFreeNotifies.DeleteRange(i*2, 2);
      	break;
      end;
      {$ENDIF}
    end;
end;

procedure TMeInterfacedObject.FreeNotification(Proc: TMeObjectMethod);
{$IFDEF PUREPASCAL}
{$IFDEF FPC}
var Ptr1, Ptr2: Pointer;
{$ENDIF FPC}
begin
  if FFreeNotifies = nil then
    FFreeNotifies := NewList;
  {$IFDEF FPC}
  asm
    MOV  EAX, [Proc]
    MOV  [Ptr1], EAX
    MOV  EAX, [Proc+4]
    MOV  [Ptr2], EAX
  end ['EAX'];
  FFreeNotifies.Insert(0, Ptr2);
  FFreeNotifies.Insert(0, Ptr1);
  {$ELSE DELPHI}
  FFreeNotifies.Insert(0, Pointer(TMethod(Proc).Data));
  FFreeNotifies.Insert(0, Pointer(TMethod(Proc).Code));
  {$ENDIF}
end;
{$ELSE PUREPASCAL} 
asm
        PUSH     EBX
        XCHG     EAX, EBX
        MOV      EAX, [EBX].FFreeNotifies
        TEST     EAX, EAX
        JNZ      @@1
        CALL     NewList
        MOV      [EBX].FFreeNotifies, EAX
@@1:    XOR      EDX, EDX
        MOV      ECX, [EBP+12] // Data
        MOV      EBX, EAX
        CALL     TMeList.Insert
        XCHG     EAX, EBX
        XOR      EDX, EDX
        MOV      ECX, [EBP+8] // Code
        CALL     TMeList.Insert
        POP      EBX
end;
{$ENDIF PUREPASCAL}

{ TMeContainer }

class procedure TMeContainer.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EMeError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

class procedure TMeContainer.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

{ TMeList }

function NewList: PMeList;
begin
  New(Result, Create);
end;

{$IFDEF COMPILER4_UP}
function NewListInit(const AItems: array of Pointer): PMeList;
var i: Integer;
begin
  Result := NewList;
  Result.Capacity := Length(AItems);
  for i := 0 to High(AItems) do
    Result.Add(AItems[i]);
end;
{$ENDIF}

(*
procedure TMeList.Init;
{$IFDEF PUREPASCAL}
begin
  inherited;
  //FIsExternalList := True;
  //FCount := 0;
end;
{$ELSE PUREPASCAL}
asm
        PUSH     EAX
        CALL     TMeDynamicObject.Init
        POP      EAX
        //MOV      [EAX].FIsExternalList, True
        //MOV      [EAX].FCount,  0
end;
{$ENDIF PUREPASCAL}
*)
destructor TMeList.Destroy;
{$IFDEF PUREPASCAL}
begin
   Clear;
   inherited;
end;
{$ELSE PUREPASCAL} 
asm
        PUSH      EAX
        CALL      TMeList.Clear
        POP       EAX
        CALL      TMeDynamicObject.Destroy
end;
{$ENDIF PUREPASCAL}

procedure TMeList.FreePointers;
{$IFDEF PUREPASCAL}
var I: Integer;
begin
  if @Self = nil then Exit;
  for I := 0 to FCount - 1 do
    if FItems[I] <> nil then
      FreeMem(FItems[I]);
end;
{$ELSE PUREPASCAL} 
asm
       TEST      EAX, EAX
       JZ        @@exit
       MOV       ECX, [EAX].FCount
       JECXZ     @@exit
       MOV       EDX, [EAX].FItems
       PUSH      EAX
@@1:
       MOV       EAX, [EDX+ECX*4-4]
       TEST      EAX, EAX
       JZ        @@2
       PUSH      EDX
       PUSH      ECX
       CALL      System.@FreeMem
       POP       ECX
       POP       EDX
@@2:   LOOP      @@1
       POP       EAX
@@exit:   //CALL      TMeDynamicObject.Free
end;
{$ENDIF PUREPASCAL}

procedure TMeList.FreeMeObjects(aFreeMemRequired: Boolean);
var I: Integer;
begin
  if @Self = nil then Exit;
  for I := FCount-1 downto 0 do
  begin
    {$IFDEF DEBUG}
    //SendDebug(PMeDynamicObject(FItems[I]).ClassName);
    {$ENDIF}
    if Assigned(FItems[I]) then
    begin
      PMeDynamicObject(FItems[I]).Free(aFreeMemRequired);
      FItems[I] := nil;
    end;
  end;
end;

procedure TMeList.SetCapacity(Value: Integer);
{$IFDEF PUREPASCAL}
begin
   if Value < Count then
      Value := Count;
   if Value = FCapacity then Exit;
   if not FIsExternalList then
   begin
     ReallocMem(FItems, Value * Sizeof(Pointer));
   end
   else if Assigned(FOnListGrow) then
   begin
     FOnListGrow(@Self, Value);
   end;;
   FCapacity := Value;
end;
{$ELSE PUREPASCAL}
asm
        CMP       EDX, [EAX].FCount
        {$IFDEF USE_CMOV}
        CMOVL     EDX, [EAX].FCount
        {$ELSE}
        JGE       @@1
        MOV       EDX, [EAX].FCount
@@1:    {$ENDIF}
        CMP       EDX, [EAX].FCapacity
        JE        @@exit

        MOV       [EAX].FCapacity, EDX
        CMP       [EAX].FIsExternalList, 0
        JNE       @@DoGrowEvent
        SAL       EDX, 2
        LEA       EAX, [EAX].FItems
        JMP      System.@ReallocMem
@@DoGrowEvent:
        MOV       ECX, [EAX].FOnListGrow.TMethod.Code
        CMP       ECX, 0
        JE        @@exit
        CALL      ECX
@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TMeList.SetList(Value: PPointerList);
{$IFDEF PUREPASCAL}
begin
  if not FIsExternalList and (Value <> FItems) then FItems := Value;
end;
{$ELSE PUREPASCAL}
asm
        CMP       [EAX].FIsExternalList, 0
        JE        @@exit
        CMP       EDX, [EAX].FItems
        JE        @@exit
        MOV       [EAX].FItems, EDX
@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TMeList.Pack;
var
   i, j, n : Integer;
   pk : PPointer;
begin
   n:=FCount;
   Dec(n);
   while (n>=0) and (FItems[n]=nil) do Dec(n);
   for i:=0 to n do begin
      if FItems[i]=nil then begin
         pk:=@FItems[i];
         for j:=i+1 to n do begin
            if FItems[j]<>nil then begin
               pk^:=FItems[j];
               Inc(pk);
            end;
         end;
         FCount := (Integer(pk)-Integer(FItems)) shr 2;
         Exit;
      end;
   end;
   FCount := n;
   Inc(FCount);
end;

procedure TMeList.Clear;
{$IFDEF PUREPASCAL}
begin
   if not FIsExternalList then
   begin
     if FItems <> nil then
      FreeMem(FItems);
   end
   else if Assigned(FOnListClear) then
   begin
     FOnListClear(@Self);
   end;
   FItems := nil;
   FCount := 0;
   FCapacity := 0;
end;
{$ELSE PUREPASCAL}
asm
        CMP       [EAX].FIsExternalList, 0
        JE        @@DoClear
        MOV       EDX, [EAX].FOnListClear.TMethod.Code
        CMP       EDX, 0
        JE        @@exit
        JMP       EDX
@@DoClear:
        PUSH      [EAX].FItems
        XOR       EDX, EDX
        MOV       [EAX].FItems, EDX
        MOV       [EAX].FCount, EDX
        MOV       [EAX].FCapacity, EDX
        POP       EAX
        CALL      System.@FreeMem
@@exit:
end;
{$ENDIF PUREPASCAL}

function TMeList.Add(Value: Pointer): Integer;
{$IFDEF PUREPASCAL}
begin
   if FCapacity <= FCount then
   begin
     Grow;
   end;
   FItems[FCount] := Value;
   Result := FCount;
   Inc(FCount);
end;
{$ELSE PUREPASCAL}
asm
  //PUSH EDI
  //MOV  EDI, EAX
  MOV  ECX, [EAX].FCount
  CMP  [EAX].FCapacity, ECX
  JG   @@SkipGrow
  PUSH EAX
  PUSH EDX
  CALL TMeList.Grow
  POP  EDX
  POP  EAX
  MOV  ECX, [EAX].FCount
@@SkipGrow:
  //LEA  EDI, [EAX].FCount
  //LEA  ECX, [EDI].FCount
  Inc  [EAX].FCount
  MOV  EAX, [EAX].FItems
  MOV  [EAX + ECX*4], EDX
  MOV  EAX, ECX
  //POP  EDI
end;
{$ENDIF PUREPASCAL}

function TMeList.Popup: Pointer;
{$IFDEF PUREPASCAL}
begin
  if Count = 0 then
    Result := nil
  else 
  begin
    Result := FItems[Count-1];
    Dec(FCount);
  end;
end;
{$ELSE PUREPASCAL} 
asm     
        MOV      ECX, [EAX].FCount
        JECXZ    @@0
        MOV      EDX, [EAX].FItems
        DEC      ECX
        MOV      [EAX].FCount, ECX
        MOV      ECX, [EDX + ECX*4]
@@0:    XCHG     EAX, ECX
end;
{$ENDIF PUREPASCAL}

{$IFDEF SUPPORTS_DYNAMICARRAYS}
procedure TMeList.AddItems(const aItems: array of Pointer);
{$IFDEF PUREPASCAL}
//var 
  //i: Integer;
begin
  if Length(aItems) > 0 then
  begin
    Capacity := Count + Length(aItems);
    System.Move(aItems[0], FItems^, FCapacity*SizeOf(Pointer));
    //for i := 0 to High(aItems) do
    //FItems[FCount+i] := aItems[i];
    FCount := FCapacity;
  end;
end;
{$ELSE PUREPASCAL}
const
  cSizePtrSQRT = 2; //Sqrt(Sizeof(Pointer))
asm
        PUSH      EAX
        PUSH      EDX
        MOV       EDX, ECX
        INC       EDX  //the Length(aItems)
        ADD       EDX, [EAX].FCount //EDX = Count + Length(aItems)
        CALL      TMeList.SetCapacity
        POP       EDX   //@aItems[0]
        MOV       EAX, EDX
        POP       ECX   //@Self
        MOV       EDX, [ECX].FItems
        PUSH      ECX
        MOV       ECX, [ECX].FCapacity
        SHL       ECX, cSizePtrSQRT   //ECX = ECX * 4(SizeOf(Pointer))
        CALL      System.Move
        POP       EAX
        MOV       ECX, [EAX].FCapacity
        MOV       [EAX].FCount, ECX 
end;
{$ENDIF PUREPASCAL}
{$ENDIF SUPPORTS_DYNAMICARRAYS}

procedure TMeList.Delete(Index: Integer);
begin
   DeleteRange(Index, 1);
end;

procedure TMeList.DeleteRange(Index, Len: Integer);
{$IFDEF PUREPASCAL}
begin
  if (Len <= 0) or (Index >= Count) then Exit;
  Assert((Index >= 0), 'TMeList.DeleteRange: index out of bounds');
  if LongWord(Index + Len) > LongWord(Count) then
    Len := Count - Index;
  System.Move(FItems[Index + Len], FItems[Index], Sizeof(Pointer) * (Count - Index - Len));
  Dec(FCount, Len);
end;
{$ELSE PUREPASCAL} 
asm     
        TEST     ECX, ECX
        JLE      @@exit
        CMP      EDX, [EAX].FCount
        JGE      @@exit
        PUSH     EBX
        XCHG     EBX, EAX
        LEA      EAX, [EDX+ECX]
        CMP      EAX, [EBX].FCount
        JBE      @@1
        MOV      ECX, [EBX].FCount
        SUB      ECX, EDX
@@1:
        MOV      EAX, [EBX].FItems
        PUSH     [EBX].FCount
        SUB      [EBX].FCount, ECX
        MOV      EBX, EDX
        LEA      EDX, [EAX+EDX*4]
        LEA      EAX, [EDX+ECX*4]
        ADD      EBX, ECX
        POP      ECX
        SUB      ECX, EBX
        SHL      ECX, 2
        CALL     System.Move
        POP      EBX
@@exit:
end;
{$ENDIF PUREPASCAL}

procedure TMeList.Remove(Value: Pointer);
var I: Integer;
begin
  I := IndexOf(Value);
  if I >= 0 then
    Delete(I);
end;

procedure TMeList.Put(Index: Integer; Value: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
   FItems[Index] := Value;
end;

procedure TMeList.Grow;
{$IFDEF PUREPASCAL}
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;
{$ELSE PUREPASCAL} 
asm
  MOV  EDX, [EAX].FCapacity
  CMP  EDX, $40
  //JLE  @@_LessThan64
  //SAR  EDX, 2
  //JMP  @@SetCap
  JG   @@_GreaterThan64
//@@_LessThan64:
  CMP  EDX, 8
  JG   @@_GreaterThan8
  //JLE  @@_LessThan8
  //MOV  EDX, $10
  //JMP  @@SetCap
//@@_LessThan8:
  MOV  EDX, 4
@@_GreaterThan8:
  MOV  EDX, $10
  JMP  @@SetCap
@@_GreaterThan64:
  SAR EDX, 2
@@SetCap:
  ADD  EDX, [EAX].FCapacity
  CALL TMeList.SetCapacity
end;
{$ENDIF PUREPASCAL}

function TMeList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
   Result := FItems[Index];
end;

function TMeList.IndexOf(Value: Pointer): Integer;
{$IFDEF PUREPASCAL}
begin
     for Result := 0 to Count - 1 do
     begin
        if FItems[Result] = Value then
        begin
           exit;
        end;
     end;
     Result := -1;
end;
{$ELSE PUREPASCAL} 
asm
        PUSH      EDI

        MOV       EDI, [EAX].FItems
        MOV       ECX, [EAX].FCount
          PUSH      EDI
          DEC       EAX            // make "NZ" - EAX always <> 1
          MOV       EAX, EDX
          REPNZ     SCASD
          POP       EDX
        {$IFDEF USE_CMOV}
        CMOVNZ    EDI, EDX
        {$ELSE}
        JZ        @@succ
        MOV       EDI, EDX
@@succ: {$ENDIF}

        MOV       EAX, EDI
        STC
        SBB       EAX, EDX
        SAR       EAX, 2

        POP       EDI
end;
{$ENDIF PUREPASCAL}

procedure TMeList.Insert(Index: Integer; Value: Pointer);
{$IFDEF PUREPASCAL}
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FItems^[Index], FItems^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FItems[Index] := Value;
  Inc(FCount);
end;
{$ELSE PUREPASCAL} 
asm
        PUSH      ECX
        PUSH      EAX
        PUSH      [EAX].FCount
          PUSH      EDX
          CALL      TMeList.Add   // don't matter what to add
          POP       EDX         // EDX = Index, Eax = Count-1
        POP       EAX
        SUB       EAX, EDX

        SAL       EAX, 2
        MOV       ECX, EAX      // ECX = (Count - Index - 1) * 4
        POP       EAX
        MOV       EAX, [EAX].FItems
        LEA       EAX, [EAX + EDX*4]
        JL        @@1
          PUSH      EAX
          LEA       EDX, [EAX + 4]
          CALL      System.Move

          POP       EAX          // EAX = @FItems[Index]
@@1:
        POP       ECX            // ECX = Value
        MOV       [EAX], ECX
end;
{$ENDIF PUREPASCAL}

procedure TMeList.Move(CurIndex, NewIndex: Integer);
{$IFDEF PUREPASCAL}
var Item: Pointer;
begin
  if CurIndex = NewIndex then Exit;
  if NewIndex >= Count then Exit;
  Item := FItems[CurIndex];
  Delete(CurIndex);
  Insert(NewIndex, Item);
end;
{$ELSE PUREPASCAL} 
asm
        CMP       EDX, ECX
        JE        @@exit

        CMP       ECX, [EAX].FCount
        JGE       @@exit

        PUSH      EDI

        MOV       EDI, [EAX].FItems
        PUSH      LongWord ptr [EDI + EDX*4]
          PUSH      ECX
          PUSH      EAX
          CALL      TMeList.Delete
          POP       EAX
          POP       EDX
        POP       ECX

        POP       EDI
        CALL      TMeList.Insert
@@exit:
end;
{$ENDIF PUREPASCAL}

function TMeList.Last: Pointer;
{$IFDEF PUREPASCAL}
begin
  if Count = 0 then
    Result := nil
  else
    Result := FItems[Count-1];
end;
{$ELSE PUREPASCAL} 
asm     
        MOV      ECX, [EAX].FCount
        JECXZ    @@0
        MOV      EAX, [EAX].FItems
        DEC      ECX
        MOV      ECX, [EAX + ECX*4]
@@0:    XCHG     EAX, ECX
end;
{$ENDIF PUREPASCAL}

procedure TMeList.Swap(Idx1, Idx2: Integer);
{$IFDEF PUREPASCAL}
var Tmp: Pointer;
begin
  Tmp := FItems[Idx1];
  FItems[Idx1] := FItems[Idx2];
  FItems[Idx2] := Tmp;
end;
{$ELSE PUREPASCAL} 
asm
        MOV       EAX, [EAX].FItems
          PUSH      LongWord ptr [EAX + EDX*4]
            PUSH      ECX
            MOV       ECX, [EAX + ECX*4]
            MOV       [EAX + EDX*4], ECX
            POP       ECX
          POP       EDX
        MOV       [EAX + ECX*4], EDX
end;
{$ENDIF PUREPASCAL}

procedure TMeList.SetCount(const Value: Integer);
begin
  if Value >= Count then SetCapacity(Value);
  FCount := Value;
end;

procedure TMeList.Assign(SrcList: PMeList);
begin
  Clear;
  if SrcList.FCount > 0 then
  begin
    Capacity := SrcList.FCount;
    FCount := SrcList.FCount;
    System.Move(SrcList.FItems[0], FItems[0], Sizeof(Pointer) * FCount);
  end;
end;

procedure TMeList.LoadFromFile(const FileName: string);
var
  Stream: PMeFileStream;
begin
  New(Stream, Create);
  try
    Stream.Open(FileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMeList.LoadFromStream(Stream: PMeStream);
var
  i: Integer;
begin
  Clear;
  Stream.ReadBuffer(i, SizeOf(i));
  Count := i;
  for i := 0 to Count -1 do
     Stream.ReadBuffer(List^[i], SizeOf(Pointer));
end;

procedure TMeList.SaveToFile(const FileName: string);
var
  Stream: PMeFileStream;
begin
  New(Stream, Create);
  try
    Stream.Open(FileName, fmCreate);
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMeList.SaveToStream(Stream: PMeStream);
var
  i: Integer;
begin
  i := Count;
  Stream.WriteBuffer(i, SizeOf(i));
  for i := 0 to Count -1 do
     Stream.WriteBuffer(List^[i], SizeOf(Pointer));
end;


function NewStrings: PMeStrings;
begin
  new( Result, Create );
end;

procedure InitUpper;
var c: Char;
begin
  for c := #0 to #255 do
    Upper[ c ] := AnsiUpperCase( c + #0 )[ 1 ];
  Upper_Initialized := TRUE;
end;

{ TMeStrings }

function TMeStrings.Add(const S: String): Integer;
begin
  Result := AddObjectLen( PChar( S ), Length( S ), 0 );
end;

function TMeStrings.AddObject(const S: String; Obj: LongWord): Integer;
begin
  Result := AddObjectLen( PChar( S ), Length( S ), Obj );
end;

function TMeStrings.AddPChar(S: PChar): integer;
begin
  Result := AddObjectLen( S, StrLen( S ), 0 )
end;

function TMeStrings.AddPCharLen(S: PChar; Len: Integer): integer;
begin
  Result := AddObjectLen( S, Len, 0 )
end;

function TMeStrings.AddPCharObject(S: PChar; Obj: LongWord): Integer;
begin
  Result := AddObjectLen( S, StrLen( S ), Obj )
end;

function TMeStrings.AddObjectLen(S: PChar; Len: Integer; Obj: LongWord): Integer;
var Dest: PChar;
begin
  ProvideSpace( Len + 9 );
  Dest := PChar( LongWord( FTextBuf ) + FUsedSize );
  Result := fCount;
  Inc( fCount );
  FList.Add( Pointer( LongWord(Dest)-LongWord(FTextBuf) ) );
  PLongWord( Dest )^ := Obj;
  Inc( Dest, 4 );
  PLongWord( Dest )^ := Len;
  Inc( Dest, 4 );
  if S <> nil then
    System.Move( S^, Dest^, Len );
  Inc( Dest, Len );
  Dest^ := #0;
  Inc( FUsedSize, Len+9 );
end;

procedure TMeStrings.AddStrings(Strings: PMeStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    AddObject(Strings.Items[I], Strings.Objects[I]);
end;

procedure TMeStrings.Clear;
begin
  if FastClear then
  begin
    if FList.Count > 0 then
      FList.FCount := 0;
  end
  else
  begin
    FList.Clear;
    if FTextBuf <> nil then
      FreeMem( FTextBuf );
    FTextBuf := nil;
  end;
  FTextSize := 0;
  FUsedSize := 0;
  fCount := 0;
end;

procedure TMeStrings.Delete(Idx: integer);
begin
  if (Idx < 0) or (Idx >= Count) then Exit;
  if Idx = Count-1 then
    Dec( FUsedSize, ItemLen[ Idx ]+9 );
  FList.Delete( Idx );
  Dec( fCount );
end;

destructor TMeStrings.Destroy;
begin
  FastClear := FALSE;
  Clear;
  FList.Free;
  inherited;
end;

function TMeStrings.Equals(Strings: PMeStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  if FCount <> Strings.FCount then Exit;
  for I := 0 to FCount - 1 do 
    if (ItemLen[I] <> Strings.ItemLen[I]) or (CompareMem(ItemPtrs[I], Strings.ItemPtrs[I], ItemLen[I]) <> true) then
      Exit;
  Result := True;
end;

function TMeStrings.Find(const S: String; var Index: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if (ItemLen[ i ] = Length( S )) and
       ((S = '') or CompareMem( ItemPtrs[ i ], @ S[ 1 ], Length( S ) )) then
       begin
         Index := i;
         Result := TRUE;
         Exit;
       end;
  Result := FALSE;
end;

function TMeStrings.Get(Idx: integer): string;
begin
  if (Idx >= 0) and (Idx <= Count) then
    SetString( Result, PChar( LongWord( FTextBuf ) + LongWord( FList.Items[ Idx ] ) + 8 ),
               ItemLen[ Idx ] )
  else
    Result := '';
end;

function TMeStrings.GetItemLen(Idx: Integer): Integer;
var 
  Src: PLongWord;
begin
  if (Idx >= 0) and (Idx <= Count) then
  begin
    Src := PLongWord( LongWord( FTextBuf ) + LongWord( FList.Items[ Idx ] ) + 4 );
    Result := Src^
  end
    else Result := 0;
end;

function TMeStrings.GetObject(Idx: Integer): LongWord;
var
  Src: PLongWord;
begin
  if (Idx >= 0) and (Idx <= Count) then
  begin
    Src := PLongWord( LongWord( FTextBuf ) + LongWord( FList.Items[ Idx ] ) );
    Result := Src^
  end
    else Result := 0;
end;

function TMeStrings.GetPChars(Idx: Integer): PChar;
begin
  if (Idx >= 0) and (Idx <= Count) then
    Result := PChar( LongWord( FTextBuf ) + LongWord( FList.Items[ Idx ] ) + 8 )
  else Result := nil;
end;

function TMeStrings.GetTextStr: string;
var L, i: Integer;
    p: PChar;
begin
  L := 0;
  for i := 0 to Count-1 do
    Inc( L, ItemLen[ i ] + 2 );
  SetLength( Result, L );
  p := PChar( Result );
  for i := 0 to Count-1 do
  begin
    L := ItemLen[ i ];
    if L > 0 then
    begin
      System.Move( ItemPtrs[ i ]^, p^, L );
      Inc( p, L );
    end;
    p^ := #13; Inc( p );
    p^ := #10; Inc( p );
  end;
end;

function TMeStrings.IndexOfObject(const aObj: LongWord): integer;
begin
  for Result := 0 to Count-1 do
  begin
    if aObj = PLongWord(LongWord(FTextBuf) + LongWord(FList.Items[Result]))^ then
      Exit;
  end;
  Result := -1;
end;

function TMeStrings.IndexOf(const S: string): integer;
begin
  if not Find( S, Result ) then Result := -1;
end;

function TMeStrings.IndexOf_NoCase(const S: string): integer;
begin
  Result := IndexOfStrL_NoCase( PChar( S ), Length( S ) );
end;

function TMeStrings.IndexOfStrL_NoCase(Str: PChar;
  L: Integer): integer;
var i: Integer;
begin
  for i := 0 to Count-1 do
    if (ItemLen[ i ] = L) and
       ((L = 0) or (StrLComp_NoCase( ItemPtrs[ i ], Str, L ) = 0)) then
       begin
         Result := i;
         Exit;
       end;
  Result := -1;
end;

procedure TMeStrings.Init;
begin
  FList := NewList;
  FastClear := TRUE;
end;

procedure TMeStrings.Insert(Idx: integer; const S: String);
begin
  InsertObjectLen( Idx, PChar( S ), Length( S ), 0 );
end;

procedure TMeStrings.InsertObject(Idx: integer; const S: String;
  Obj: LongWord);
begin
  InsertObjectLen( Idx, PChar( S ), Length( S ), Obj );
end;

procedure TMeStrings.InsertPChar(Idx: integer; S: PChar);
begin
  InsertObjectLen( Idx, S, StrLen( S ), 0 )
end;

procedure TMeStrings.InsertPCharLen(Idx: Integer; S: PChar; Len: Integer);
begin
  InsertObjectLen( Idx, S, Len, 0 )
end;

procedure TMeStrings.InsertPCharObject(Idx: Integer; S: PChar; Obj: LongWord);
begin
  InsertObjectLen( Idx, S, StrLen( S ), Obj );
end;

procedure TMeStrings.InsertObjectLen(Idx: Integer; S: PChar;
  Len: Integer; Obj: LongWord);
var Dest: PChar;
begin
  ProvideSpace( Len+9 );
  Dest := PChar( LongWord( FTextBuf ) + FUsedSize );
  FList.Insert( Idx, Pointer( LongWord(Dest)-LongWord(FTextBuf) ) );
  PLongWord( Dest )^ := Obj;
  Inc( Dest, 4 );
  PLongWord( Dest )^ := Len;
  Inc( Dest, 4 );
  if S <> nil then
    System.Move( S^, Dest^, Len );
  Inc( Dest, Len );
  Dest^ := #0;
  Inc( FUsedSize, Len+9 );
  Inc( fCount );
end;

function TMeStrings.Last: String;
begin
  if Count > 0 then
    Result := Items[ Count-1 ]
  else
    Result := '';
end;

procedure TMeStrings.LoadFromFile(const FileName: string);
var
  Stream: PMeFileStream;
begin
  New(Stream, Create);
  try
    Stream.Open(FileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMeStrings.LoadFromStream(Stream: PMeStream);
var
  Size: Integer;
  S: string;
begin
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    SetTextStr(S);
end;

procedure TMeStrings.Move(CurIndex, NewIndex: integer);
begin
  Assert( (CurIndex >= 0) and (CurIndex < Count) and (NewIndex >= 0) and
          (NewIndex < Count), 'Item indexes violates TMeStrings range' );
  FList.Move( CurIndex, NewIndex );
end;

procedure TMeStrings.ProvideSpace(AddSize: LongWord);
var OldTextBuf: PChar;
begin
  Inc( AddSize, 9 );
  if AddSize > FTextSize - FUsedSize then
  begin
    FTextSize := Max( 1024, (FUsedSize + AddSize) * 2 );
    OldTextBuf := FTextBuf;
    GetMem( FTextBuf, FTextSize );
    if OldTextBuf <> nil then
    begin
      System.Move( OldTextBuf^, FTextBuf^, FUsedSize );
      FreeMem( OldTextBuf );
    end;
  end;
  if FList.Count >= FList.Capacity then
    FList.Capacity := Max( 100, FList.Count * 2 );
end;

procedure TMeStrings.Put(Idx: integer; const Value: string);
var Dest: PChar;
    OldLen: Integer;
    OldObj: LongWord;
begin
  OldLen := ItemLen[ Idx ];
  if Length( Value ) <= OldLen then
  begin
    Dest := PChar( LongWord( FTextBuf ) + LongWord( FList.Items[ Idx ] ) + 4 );
    PLongWord( Dest )^ := Length( Value );
    Inc( Dest, 4 );
    if Value <> '' then
      System.Move( Value[ 1 ], Dest^, Length( Value ) );
    Inc( Dest, Length( Value ) );
    Dest^ := #0;
    if Idx = Count-1 then
      Dec( FUsedSize, OldLen - Length( Value ) );
  end
    else
  begin
    OldObj := 0;
    while Idx > Count do
      AddObjectLen( nil, 0, 0 );
    if Idx = Count-1 then
    begin
      OldObj := Objects[ Idx ];
      Delete( Idx );
    end;
    if Idx = Count then
      AddObjectLen( PChar( Value ), Length( Value ), OldObj )
    else
    begin
      ProvideSpace( Length( Value ) + 9 );
      Dest := PChar( LongWord( FTextBuf ) + FUsedSize );
      FList.Items[ Idx ] := Pointer( LongWord(Dest)-LongWord(FTextBuf) );
      Inc( Dest, 4 );
      PLongWord( Dest )^ := Length( Value );
      Inc( Dest, 4 );
      if Value <> '' then
        System.Move( Value[ 1 ], Dest^, Length( Value ) );
      Inc( Dest, Length( Value ) );
      Dest^ := #0;
      Inc( FUsedSize, Length( Value )+9 );
    end;
  end;
end;

procedure TMeStrings.SaveToFile(const FileName: string);
var
  Stream: PMeFileStream;
begin
  New(Stream, Create);
  try
    Stream.Open(FileName, fmCreate);
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMeStrings.SaveToStream(Stream: PMeStream);
var
  S: string;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;


procedure TMeStrings.SetObject(Idx: Integer; const Value: LongWord);
var Dest: PLongWord;
begin
  if Idx < 0 then Exit;
  while Idx >= Count do
    AddObjectLen( nil, 0, 0 );
  Dest := PLongWord( LongWord( FTextBuf ) + LongWord( FList.Items[ Idx ] ) );
  Dest^ := Value;
end;

procedure TMeStrings.SetText(const S: string; Append2List: boolean);
var Len2Add, NLines, L: Integer;
    p0, p: PChar;
begin
  if not Append2List then Clear;

  Len2Add := 0;
  NLines := 0;
  p := Pchar( S );
  p0 := p;
  L := Length( S );
  while L > 0 do
  begin
    if p^ = #13 then
    begin
      Inc( NLines );
      Inc( Len2Add, 9 + LongWord(p)-LongWord(p0) );
      REPEAT Inc( p ); Dec( L );
      UNTIL  (p^ <> #10) or (L = 0);
      p0 := p;
    end
      else
    begin
      Inc( p ); Dec( L );
    end;
  end;
  if LongWord(p) > LongWord(p0) then
  begin
    Inc( NLines );
    Inc( Len2Add, 9 + LongWord(p)-LongWord(p0) );
  end;
  if Len2Add = 0 then Exit;

  ProvideSpace( Len2Add - 9 );
  if FList.Capacity <= FList.Count + NLines then
    FList.Capacity := Max( (FList.Count + NLines) * 2, 100 );
  p := PChar( S );
  p0 := p;
  L := Length( S );
  while L > 0 do
  begin
    if p^ = #13 then
    begin
      AddObjectLen( p0, LongWord(p)-LongWord(p0), 0 );
      REPEAT Inc( p ); Dec( L );
      UNTIL  (p^ <> #10) or (L = 0);
      p0 := p;
    end
      else
    begin
      Inc( p ); Dec( L );
    end;
  end;
  if LongWord(p) > LongWord(p0) then
    AddObjectLen( p0, LongWord(p)-LongWord(p0), 0 );
end;

procedure TMeStrings.SetTextStr(const Value: string);
begin
  SetText( Value, FALSE );
end;

function CompareFast(const Data: Pointer; const e1,e2 : LongWord) : Integer;
var FSL: PMeStrings;
    L1, L2: Integer;
    S1, S2: PChar;
begin
  FSL := Data;
  S1 := FSL.ItemPtrs[ e1 ];
  S2 := FSL.ItemPtrs[ e2 ];
  L1 := FSL.ItemLen[ e1 ];
  L2 := FSL.ItemLen[ e2 ];
  if FSL.FCaseSensitiveSort then
    Result := StrLComp( S1, S2, Min( L1, L2 ) )
  else
    Result := StrLComp_NoCase( S1, S2, Min( L1, L2 ) );
  if Result = 0 then
    Result := L1 - L2;
  if Result = 0 then
    Result := e1 - e2;
end;

procedure SwapFast(const Data : Pointer; const e1,e2 : LongWord);
var FSL: PMeStrings;
begin
  FSL := Data;
  FSL.Swap( e1, e2 );
end;

procedure TMeStrings.Sort(CaseSensitive: Boolean);
begin
  FCaseSensitiveSort := CaseSensitive;
  SortData( @ Self, Count, CompareFast, SwapFast );
end;

procedure TMeStrings.Swap(Idx1, Idx2: Integer);
begin
  Assert( (Idx1 >= 0) and (Idx1 <= Count-1) and (Idx2 >= 0) and (Idx2 <= Count-1),
          'Item indexes violates TMeStrings range' );
  FList.Swap( Idx1, Idx2 );
end;

function TMeStrings.GetName(const Index: Integer): string;
var
  s: PChar;
begin
  if (Index >=0) and (Index < FCount) then
  begin
    s := ItemPtrs[Index];
    while (s^ <> '=') and (s^ <> #0) do
    begin
      Inc( s );
    end;
    if (s^ = '=') then
    begin
      SetString(Result, ItemPtrs[Index], Integer(s) - Integer(ItemPtrs[Index]));
    end
    else begin
      Result := Items[Index];
    end;
  end
  else
    Result := '';
end;

function TMeStrings.GetValue(AName: PChar): PChar;
var i: Integer;
    s, n: PChar;
begin
  if not Upper_Initialized then
    InitUpper;
  for i := 0 to Count-1 do
  begin
    s := ItemPtrs[ i ];
    n := AName;
    while (Upper[ s^ ] = Upper[ n^ ]) and (s^ <> '=') and (s^ <> #0) and (n^ <> #0) do
    begin
      Inc( s );
      Inc( n );
    end;
    if (s^ = '=') and (n^ = #0) then
    begin
      Result := s;
      Inc( Result );
      Exit;
    end;
  end;
  Result := nil;
end;

function TMeStrings.IndexOfName(AName: PChar): Integer;
var i: Integer;
    s, n: PChar;
begin
  if not Upper_Initialized then
    InitUpper;
  for i := 0 to Count-1 do
  begin
    s := ItemPtrs[ i ];
    n := AName;
    while (Upper[ s^ ] = Upper[ n^ ]) and (s^ <> '=') and (s^ <> #0) and (n^ <> #0) do
    begin
      Inc( s );
      Inc( n );
    end;
    if (s^ = '=') and (n^ = #0) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TMeStrings.AppendPChar(S: PChar);
begin
  AppendPCharLen( S, StrLen( S ) );
end;

procedure TMeStrings.AppendInt2Hex(N: LongWord; MinDigits: Integer);
var Buffer: array[ 0..9 ] of Char;
    Mask: LongWord;
    i, Len: Integer;
    B: Byte;
begin
  if MinDigits > 8 then
    MinDigits := 8;
  if MinDigits <= 0 then
    MinDigits := 1;
  Mask := $F0000000;
  for i := 8 downto MinDigits do
  begin
    if Mask and N <> 0 then
    begin
      MinDigits := i;
      break;
    end;
    Mask := Mask shr 4;
  end;
  i := 0;
  Len := MinDigits;
  Mask := $F shl ((Len - 1)*4);
  while MinDigits > 0 do
  begin
    Dec( MinDigits );
    B := (N and Mask) shr (MinDigits * 4);
    Mask := Mask shr 4;
    if B <= 9 then
      Buffer[ i ] := Char( B + Ord( '0' ) )
    else
      Buffer[ i ] := Char( B + Ord( 'A' ) - 10 );
    Inc( i );
  end;
  Buffer[ i ] := #0;
  AppendPCharLen( @ Buffer[ 0 ], Len );
end;

procedure TMeStrings.AppendPCharLen(S: PChar; Len: Integer);
var Dest: PChar;
begin
  if Count = 0 then
    AddPCharLen( S, Len )
  else
  begin
    ProvideSpace( Len );
    Dest := PChar( LongWord( FTextBuf ) + FUsedSize - 1 );
    System.Move( S^, Dest^, Len );
    Inc( Dest, Len );
    Dest^ := #0;
    Inc( FUsedSize, Len );
    Dest := PChar( LongWord( FTextBuf ) + LongWord( FList.Items[ Count-1 ] ) );
    Inc( Dest, 4 );
    PLongWord( Dest )^ := PLongWord( Dest )^ + LongWord( Len );
  end;
end;

{ TMeDynamicMemory }
procedure TMeDynamicMemory.Init;
begin
  inherited;
end;

destructor TMeDynamicMemory.Destroy;
begin
  {$IFDEF MeDynamicMemory_HoldMem_SUPPORT}
  if not FHoldMem then 
  {$ENDIF}
    FreeMem(FMemory);
  inherited;
end;

procedure TMeDynamicMemory.Clear;
begin
  if Assigned(FMemory) then
  begin
    //FreeMem(FMemory);
    //FMemory := nil;
    FUsedSize := 0;
    //FSize := 0;
  end;
end;

procedure TMeDynamicMemory.Grow(const aSize: Integer);
var
  I: integer;
begin
  if FSize < 64 then //JUST BECARE
    I := 2
  else
    I := FSize div 4;
  if I < aSize then I := aSize;
  Size := FSize + I;
end;

procedure TMeDynamicMemory.SetSize(const Value: Integer);
begin
  if (FSize <> Value) then
  begin
    //writeLn('ReallocMem from ', FSize, ' to ', Value);
    FSize := Value;
    ReallocMem(FMemory, FSize);
    if FUsedSize > FSize then FUsedSize := FSize;
  end;
end;

procedure TMeDynamicMemory.SetUsedSize(const Value: Integer);
begin
  if (Value < FUsedSize) or (Value = FUsedSize) then
  begin
    exit;
  end;

  if (Value > FSize) then
  begin
    Grow(Value - FSize)
  end;
  
  FUsedSize := Value;
end;

procedure TMeDynamicMemory.AddDouble(const aValue: Double);
var
  p: Pointer;
begin
  if (FUsedSize + SizeOf(aValue)) >= FSize then
    Grow(SizeOf(aValue));
  Integer(p) := Integer(FMemory) + FUsedSize;
  PDouble(P)^ := aValue;
  Inc(FUsedSize, SizeOf(aValue));
end;

procedure TMeDynamicMemory.AddInt(const aValue: Integer);
var
  p: Pointer;
begin
  if (FUsedSize + SizeOf(aValue)) >= FSize then
    Grow(SizeOf(aValue));
  Integer(p) := Integer(FMemory) + FUsedSize;
  PInteger(P)^ := aValue;
  Inc(FUsedSize, SizeOf(aValue));
end;

procedure TMeDynamicMemory.AddByte(const aValue: Byte);
var
  p: Pointer;
begin
  if (FUsedSize + SizeOf(aValue)) >= FSize then
    Grow(SizeOf(aValue));
  Integer(p) := Integer(FMemory) + FUsedSize;
  PByte(P)^ := aValue;
  Inc(FUsedSize, SizeOf(aValue));
end;

procedure TMeDynamicMemory.AddWord(const aValue: Word);
var
  p: Pointer;
begin
  if (FUsedSize + SizeOf(aValue)) >= FSize then
    Grow(SizeOf(aValue));
  Integer(p) := Integer(FMemory) + FUsedSize;
  PWord(P)^ := aValue;
  Inc(FUsedSize, SizeOf(aValue));
end;

procedure TMeDynamicMemory.AddPChar(const aValue: string);
var
  p: Pointer;
begin
  Integer(p) := Length(aValue)+1;
  if (FUsedSize+Integer(p)) > FSize then
    Grow(Integer(p));
  Integer(p) := Integer(FMemory) + FUsedSize;
  if aValue <> '' then
  begin
    Move(aValue[1], p^, Length(aValue));
    Inc(Integer(p), Length(aValue));
  end;
  PByte(p)^ := 0;
  Inc(FUsedSize, Length(aValue) + 1);
end;

procedure TMeDynamicMemory.AddBuffer(const aValue; aSize: Integer);
var
  p: Pointer;
begin
  if (FUsedSize+aSize) > FSize then
    Grow(aSize);
  Integer(p) := Integer(FMemory) + FUsedSize;
  Move(aValue, p^, aSize);
  Inc(FUsedSize, aSize);
end;

procedure TMeDynamicMemory.Align;
var
  I: Integer;
begin
  I := (FUsedSize + 3) and $FFFFFFFC;
  I := I - FUsedSize;
  if I > 0 then AllocSpace(I);
end;

procedure TMeDynamicMemory.AllocSpace(const aSize: Integer);
begin
  if (FUsedSize + aSize) >= FSize then
    Grow(aSize);

  Inc(FUsedSize, aSize);
end;

procedure TMeDynamicMemory.Assign(const aMem: PMeDynamicMemory);
begin
  Clear;
  if Assigned(aMem) then
  begin
    Size := aMem.FSize;
    AddBuffer(aMem.FMemory^, aMem.FUsedSize)
  end;
end;

{ TMeStream }

function TMeStream.EOF: Boolean;
begin
  Result := Position >= Size;
end;

function TMeStream.GetPosition: Int64;
begin
  Result := Seek(0, soCurrent);
end;

procedure TMeStream.SetPosition(const Pos: Int64);
begin
  Seek(Pos, soBeginning);
end;

function TMeStream.GetSize: Int64;
var
  Pos: Int64;
begin
  Pos := Seek(0, soCurrent);
  Result := Seek(0, soEnd);
  Seek(Pos, soBeginning);
end;

procedure TMeStream.SetSize64(const NewSize: Int64);
begin
  SetSize(NewSize);
end;

procedure TMeStream.SetSize(const NewSize: Int64);
begin
  // default = do nothing  (read-only streams, etc)
  // descendents should implement this method
end;

function TMeStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

procedure TMeStream.ReadBuffer(var Buffer; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EMeError.CreateRes(@SReadError);
end;

procedure TMeStream.WriteBuffer(const Buffer; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EMeError.CreateRes(@SWriteError);
end;

function TMeStream.CopyFrom(Source: PMeStream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      WriteBuffer(Buffer^, N);
      Dec(Count, N);
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

procedure TMeStream.WriteResourceHeader(const ResName: string; out FixupInfo: Integer);
var
  HeaderSize: Integer;
  Header: array[0..79] of Char;
begin
  Byte((@Header[0])^) := $FF;
  Word((@Header[1])^) := 10;
  HeaderSize := StrLen(StrUpper(StrPLCopy(@Header[3], ResName, 63))) + 10;
  Word((@Header[HeaderSize - 6])^) := $1030;
  Longint((@Header[HeaderSize - 4])^) := 0;
  WriteBuffer(Header, HeaderSize);
  FixupInfo := Position;
end;

procedure TMeStream.FixupResourceHeader(FixupInfo: Integer);
var
  ImageSize: Integer;
begin
  ImageSize := Position - FixupInfo;
  Position := FixupInfo - 4;
  WriteBuffer(ImageSize, SizeOf(Longint));
  Position := FixupInfo + ImageSize;
end;

procedure TMeStream.ReadResHeader;
var
  ReadCount: Cardinal;
  Header: array[0..79] of Char;
begin
  FillChar(Header, SizeOf(Header), 0);
  ReadCount := Read(Header, SizeOf(Header) - 1);
  if (Byte((@Header[0])^) = $FF) and (Word((@Header[1])^) = 10) then
    Seek(StrLen(Header + 3) + 10 - ReadCount, soCurrent)
  else
    raise EMeError.CreateRes(@SInvalidImage);
end;

{ TMeNamedObjects }
destructor TMeNamedObjects.Destroy;
begin
  FreeMeObjects;
  inherited;
end;

procedure TMeNamedObjects.Clear;
begin
  FreeMeObjects;
  inherited;
end;

procedure TMeNamedObjects.Assign(const aObjs: PMeNamedObjects);
var
  i: integer;
  vItem: PMeNamedObject;
begin
  if Assigned(aObjs) then
  begin
    Clear;
    for i := 0 to aObjs.Count - 1 do
    begin
      New(vItem, Create);
      vItem.Assign(aObjs.List^[i]);
      Add(vItem);
      //Add(NewMeObject(PMeNamedObjects(aObjs.List^[i]).ClassType));
    end;
  end;
end;

function TMeNamedObjects.GetItem(Index: Integer): PMeNamedObject;
begin
  Result := Inherited Get(Index);
end;

function TMeNamedObjects.Find(const aName: string): PMeNamedObject;
var
  i: integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

function TMeNamedObjects.IndexOf(const aName: string; const aBeginIndex: Integer = 0): Integer;
begin
  for Result := aBeginIndex to Count - 1 do
  begin
    if AnsiSameText(aName, Items[Result].Name) then
      exit;
  end;
  Result := -1;
end;

//[function StrLComp_NoCase]
function StrLComp_NoCase(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;
asm
  {$IFDEF FPC}
        MOV     EAX, [Str1]
        MOV     EDX, [Str2]
        MOV     ECX, [MaxLen]
  {$ENDIF FPC}
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EDI,EDX
        MOV     ESI,EAX
        MOV     EBX,ECX
        XOR     EAX,EAX
        OR      ECX,ECX
        JE      @@exit
        REPNE   SCASB
        SUB     EBX,ECX
        MOV     ECX,EBX
        MOV     EDI,EDX
  @@0:
        XOR     EDX,EDX
        REPE    CMPSB
        MOV     AL,[ESI-1]
        MOV     AH, AL
        SUB     AH, 'a'
        CMP     AH, 25
        JA      @@1
        SUB     AL, $20
  @@1:
        MOV     DL,[EDI-1]
        MOV     AH, DL
        SUB     AH, 'a'
        CMP     AH, 25
        JA      @@2
        SUB     DL, $20
  @@2:
        MOV     AH, 0
        SUB     EAX,EDX
        JECXZ   @@exit
        JZ      @@0

  @@exit:
        POP     EBX
        POP     ESI
        POP     EDI
end {$IFDEF FPC} [ 'EAX', 'EDX', 'ECX' ] {$ENDIF};


//////////////////////////////////////////////////////////////////////////
//                            S  O  R  T  I  N  G
//////////////////////////////////////////////////////////////////////////

{ -- qsort -- }

//[PROCEDURE SortData]
procedure SortData( const Data: Pointer; const uNElem: LongWord;
                    const CompareFun: TCompareEvent;
                    const SwapProc: TSwapEvent );
{$IFDEF PUREPASCAL}
{ uNElem - number of elements to sort }

  function Compare( const e1, e2 : LongWord ) : Integer;
  begin
    Result := CompareFun( Data, e1 - 1, e2 - 1 );
  end;

  procedure Swap( const e1, e2 : LongWord );
  begin
    SwapProc( Data, e1 - 1, e2 - 1 );
  end;

  procedure qSortHelp(pivotP: LongWord; nElem: LongWord);
  label
    TailRecursion,
    qBreak;
  var
    leftP, rightP, pivotEnd, pivotTemp, leftTemp: LongWord;
    lNum: LongWord;
    retval: integer;
  begin
    TailRecursion:
      if (nElem <= 2) then
      begin
        if (nElem = 2) then
          begin
            rightP := pivotP +1;
            retval := Compare(pivotP,rightP);
            if (retval > 0) then Swap(pivotP,rightP);
          end;
        exit;
      end;
      rightP := (nElem -1) + pivotP;
      leftP :=  (nElem shr 1) + pivotP;
      { sort pivot, left, and right elements for "median of 3" }
      retval := Compare(leftP,rightP);
      if (retval > 0) then Swap(leftP, rightP);
      retval := Compare(leftP,pivotP);

      if (retval > 0) then
        Swap(leftP, pivotP)
      else
      begin
        retval := Compare(pivotP,rightP);
        if retval > 0 then Swap(pivotP, rightP);
      end;
      if (nElem = 3) then
      begin
        Swap(pivotP, leftP);
        exit;
      end;
      { now for the classic Horae algorithm }
      pivotEnd := pivotP + 1;
      leftP := pivotEnd;
      repeat

        retval := Compare(leftP, pivotP);
        while (retval <= 0) do
          begin

            if (retval = 0) then
              begin
                Swap(leftP, pivotEnd);
                Inc(pivotEnd);
              end;
            if (leftP < rightP) then
              Inc(leftP)
            else
              goto qBreak;
            retval := Compare(leftP, pivotP);
          end; {while}
        while (leftP < rightP) do
          begin
            retval := Compare(pivotP, rightP);
            if (retval < 0) then
              Dec(rightP)

            else
              begin
                Swap(leftP, rightP);
                if (retval <> 0) then
                  begin
                    Inc(leftP);
                    Dec(rightP);
                  end;
                break;
              end;
          end; {while}

      until (leftP >= rightP);
    qBreak:
      retval := Compare(leftP,pivotP);
      if (retval <= 0) then Inc(leftP);

      leftTemp := leftP -1;
      pivotTemp := pivotP;
      while ((pivotTemp < pivotEnd) and (leftTemp >= pivotEnd)) do
      begin
        Swap(pivotTemp, leftTemp);
        Inc(pivotTemp);
        Dec(leftTemp);
      end; {while}
      lNum := (leftP - pivotEnd);
      nElem := ((nElem + pivotP) -leftP);

      if (nElem < lNum) then
      begin
        qSortHelp(leftP, nElem);
        nElem := lNum;
      end
        else
      begin
        qSortHelp(pivotP, lNum);
        pivotP := leftP;
      end;
      goto TailRecursion;
    end; {qSortHelp }

begin
  if (uNElem < 2) then  exit; { nothing to sort }
  qSortHelp(1, uNElem);
end;
{$ELSE } //ASM
asm // translated to BASM by Kladov Vladimir
        CMP      EDX, 2
        JL       @@exit

        PUSH     EAX      // [EBP-4] = Data
        PUSH     ECX      // [EBP-8] = CompareFun
        PUSH     EBX      // EBX = pivotP
        XOR      EBX, EBX
        INC      EBX      // EBX = 1 to pass to qSortHelp as PivotP
        MOV      EAX, EDX // EAX = nElem
        CALL     @@qSortHelp
        POP      EBX
        POP      ECX
        POP      ECX
@@exit:
        POP      EBP
        RET      4

@@qSortHelp:
        PUSH     EBX      // EBX (in) = PivotP
        PUSH     ESI      // ESI      = leftP
        PUSH     EDI      // EDI      = rightP

@@TailRecursion:
        CMP      EAX, 2
        JG       @@2
        JNE      @@exit_qSortHelp
        LEA      ECX, [EBX+1]
        MOV      EDX, EBX
        CALL     @@Compare
        JLE      @@exit_qSortHelp
@@swp_exit:
        CALL     @@Swap
@@exit_qSortHelp:
        POP      EDI
        POP      ESI
        POP      EBX
        RET

        // ESI = leftP
        // EDI = rightP
@@2:    LEA      EDI, [EAX+EBX-1]
        MOV      ESI, EAX
        SHR      ESI, 1
        ADD      ESI, EBX
        MOV      ECX, ESI
        MOV      EDX, EDI
        CALL     @@CompareLeSwap
        MOV      EDX, EBX
        CALL     @@Compare

        JG       @@4
        CALL     @@Swap
        JMP      @@5
@@4:    MOV      ECX, EBX
        MOV      EDX, EDI
        CALL     @@CompareLeSwap
@@5:
        CMP      EAX, 3
        JNE      @@6
        MOV      EDX, EBX
        MOV      ECX, ESI
        JMP      @@swp_exit
@@6:    // classic Horae algorithm

        PUSH     EAX     // EAX = pivotEnd
        LEA      EAX, [EBX+1]
        MOV      ESI, EAX
@@repeat:
        MOV      EDX, ESI
        MOV      ECX, EBX
        CALL     @@Compare
        JG       @@while2
@@while1:
        JNE      @@7
        MOV      EDX, ESI
        MOV      ECX, EAX
        CALL     @@Swap
        INC      EAX
@@7:
        CMP      ESI, EDI
        JGE      @@qBreak
        INC      ESI
        JMP      @@repeat
@@while2:
        CMP      ESI, EDI
        JGE      @@until
        MOV      EDX, EBX
        MOV      ECX, EDI
        CALL     @@Compare
        JGE      @@8
        DEC      EDI
        JMP      @@while2
@@8:
        MOV      EDX, ESI
        MOV      ECX, EDI
        PUSHFD
        CALL     @@Swap
        POPFD
        JE       @@until
        INC      ESI
        DEC      EDI
@@until:
        CMP      ESI, EDI
        JL       @@repeat
@@qBreak:
        MOV      EDX, ESI
        MOV      ECX, EBX
        CALL     @@Compare
        JG       @@9
        INC      ESI
@@9:
        PUSH     EBX      // EBX = PivotTemp
        PUSH     ESI      // ESI = leftTemp
        DEC      ESI
@@while3:
        CMP      EBX, EAX
        JGE      @@while3_break
        CMP      ESI, EAX
        JL       @@while3_break
        MOV      EDX, EBX
        MOV      ECX, ESI
        CALL     @@Swap
        INC      EBX
        DEC      ESI
        JMP      @@while3
@@while3_break:
        POP      ESI
        POP      EBX

        MOV      EDX, EAX
        POP      EAX     // EAX = nElem
        PUSH     EDI     // EDI = lNum
        MOV      EDI, ESI
        SUB      EDI, EDX
        ADD      EAX, EBX
        SUB      EAX, ESI

        PUSH     EBX
        PUSH     EAX
        CMP      EAX, EDI
        JGE      @@10

        MOV      EBX, ESI
        CALL     @@qSortHelp
        POP      EAX
        MOV      EAX, EDI
        POP      EBX
        JMP      @@11

@@10:   MOV      EAX, EDI
        CALL     @@qSortHelp
        POP      EAX
        POP      EBX
        MOV      EBX, ESI
@@11:
        POP      EDI
        JMP      @@TailRecursion

@@Compare:
        PUSH     EAX
        PUSH     EDX
        PUSH     ECX
        MOV      EAX, [EBP-4]
        DEC      EDX
        DEC      ECX
        CALL     LongWord ptr [EBP-8]
        POP      ECX
        POP      EDX
        TEST     EAX, EAX
        POP      EAX
        RET

@@CompareLeSwap:
        CALL     @@Compare
        JG       @@ret

@@Swap: PUSH     EAX
        PUSH     EDX
        PUSH     ECX
        MOV      EAX, [EBP-4]
        DEC      EDX
        DEC      ECX
        CALL     LongWord ptr [SwapProc]
        POP      ECX
        POP      EDX
        TEST     EAX, EAX
        POP      EAX
@@ret:
        RET

end;
{$ENDIF}
//[END SortData]

//[FUNCTION CompareIntegers]
function CompareIntegers( const Sender : Pointer; const e1, e2 : LongWord ) : Integer;
var I1, I2 : Integer;
begin
  I1 := PInteger( LongWord( Sender ) + e1 * Sizeof( Integer ) )^;
  I2 := PInteger( LongWord( Sender ) + e2 * Sizeof( Integer ) )^;
  Result := 0;
  if I1 < I2 then Result := -1
  else
  if I1 > I2 then Result := 1;
end;
//[END CompareIntegers]

//[FUNCTION CompareDwords]
function CompareDwords( const Sender : Pointer; const e1, e2 : LongWord ) : Integer;
var I1, I2 : LongWord;
begin
  I1 := PLongWord( LongWord( Sender ) + e1 * Sizeof( Integer ) )^;
  I2 := PLongWord( LongWord( Sender ) + e2 * Sizeof( Integer ) )^;
  Result := 0;
  if I1 < I2 then Result := -1
  else
  if I1 > I2 then Result := 1;
end;
//[END CompareDwords]

//[PROCEDURE SwapIntegers]
procedure SwapIntegers( const Sender : Pointer; const e1, e2 : LongWord );
var Tmp : Integer;
begin
  Tmp := PInteger( LongWord( Sender ) + e1 * SizeOf( Integer ) )^;
  PInteger( LongWord( Sender ) + e1 * Sizeof( Integer ) )^ :=
  PInteger( LongWord( Sender ) + e2 * Sizeof( Integer ) )^;
  PInteger( LongWord( Sender ) + e2 * Sizeof( Integer ) )^ := Tmp;
end;
//[END SwapIntegers]

//[procedure SortIntegerArray]
procedure SortIntegerArray( var A : array of Integer );
begin
  SortData( @A[ 0 ], High( A ) - Low( A ) + 1, @CompareIntegers, @SwapIntegers );
end;

procedure SwapListItems( const L: Pointer; const e1, e2: LongWord );
begin
  PMeList( L ).Swap( e1, e2 );
end;

//[procedure SortDwordArray]
procedure SortDwordArray( var A : array of LongWord );
begin
  SortData( @A[ 0 ], High( A ) - Low( A ) + 1, @CompareDwords, @SwapIntegers );
end;

{$IFDEF MeRTTI_SUPPORT}
const
  cMeObjectClassName: PChar = 'TMeDynamicObject';
  cMeInterfacedObjectClassName: PChar = 'TMeInterfacedObject';
  cMeContainerClassName: PChar = 'TMeContainer';
  cMeListClassName: PChar = 'TMeList';
  cMeStringsClassName: PChar = 'TMeStrings';
  cMeComponentClassName: PChar = 'TMeComponent';
  cMeDynamicMemoryClassName: PChar = 'TMeDynamicMemory';
{$ENDIF}

Function XorStr(const s: string; key: string): string;
var
  i, j: Integer;
  L: Integer;
begin
  Result := '';
  L := Length(Key);
  if Length(s) < L then L := Length(s);
  j := 1;
  for i := 1 to Length(s) do
  begin
    Result := Result + Chr(Ord(key[j]) Xor Ord(s[i]));
    Inc(j);
    if J > L then J := 1;
  end;
end;

Procedure SetupInit;
const 
  v = #$7C#$57#$60#$70#$7E#$16#$74#$57#$49#$49#$78#$58#$55#$5B#$40#$15
      +#$1E#$54#$11#$19#$02#$3A#$01#$04#$1E#$06#$05#$06#$00#$18#$5B#$49
      +#$2A#$7C#$57#$60#$70#$7E#$16#$64#$57#$5F#$44#$7D#$50#$40#$56#$14
      +#$71#$53#$41#$5D#$55#$5F#$7A#$5C#$57#$5D#$40#$15#$7D#$5E#$5C#$19
      +#$60#$7E#$48#$12#$7F#$40#$51#$16#$1A#$18#$6B#$59#$69#$54#$50#$52
      +#$58#$59#$16#$7B#$7D#$7C#$18#$78#$58#$51#$56#$56#$54#$5A#$5B#$54
      +#$79#$58#$65#$45#$5F#$52#$5D#$59#$18#$54#$57#$54#$19#$26#$11#$53
      +#$5F#$58#$15#$44#$5E#$5F#$51#$44#$79#$11#$40#$56#$47#$50#$44#$41
      +#$5D#$5D#$1E;
begin
  if XorStr(cMeSDKCopyRight, '1234567890'#$0A) = v then
  begin
  //Update the MeObject VMT Table.
  {$IFDEF MeRTTI_SUPPORT}
  //Make the ovtVmtClassName point to PShortString class name
  SetMeVirtualMethod(TypeOf(TMeDynamicObject), ovtVmtClassName, cMeObjectClassName);
  SetMeVirtualMethod(TypeOf(TMeInterfacedObject), ovtVmtClassName, cMeInterfacedObjectClassName);
  SetMeVirtualMethod(TypeOf(TMeContainer), ovtVmtClassName, cMeContainerClassName);
  SetMeVirtualMethod(TypeOf(TMeList), ovtVmtClassName, cMeListClassName);
  SetMeVirtualMethod(TypeOf(TMeStrings), ovtVmtClassName, cMeStringsClassName);
  SetMeVirtualMethod(TypeOf(TMeComponent), ovtVmtClassName, cMeComponentClassName);
  SetMeVirtualMethod(TypeOf(TMeDynamicMemory), ovtVmtClassName, cMeDynamicMemoryClassName);
  SetMeVirtualMethod(TypeOf(TMeStream), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeNamedObject), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeNamedObjects), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeDynamicObject), ovtVmtParent, nil);
  SetMeVirtualMethod(TypeOf(TMeInterfacedObject), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeContainer), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeList), ovtVmtParent, TypeOf(TMeContainer));
  SetMeVirtualMethod(TypeOf(TMeStrings), ovtVmtParent, TypeOf(TMeContainer));
  SetMeVirtualMethod(TypeOf(TMeComponent), ovtVmtParent, TypeOf(TMeNamedObject));
  SetMeVirtualMethod(TypeOf(TMeDynamicMemory), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeStream), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeNamedObject), ovtVmtParent, TypeOf(TMeDynamicObject));
  SetMeVirtualMethod(TypeOf(TMeNamedObjects), ovtVmtParent, TypeOf(TMeList));

  {$IFDEF MeRTTI_EXT_SUPPORT}
  FObjectNameList := NewList;
  {$ENDIF}
  end;
end;

initialization

  SetupInit;

finalization
  {$IFDEF MeRTTI_EXT_SUPPORT}
  FObjectNameList.Free;
  {$ENDIF}
end.
