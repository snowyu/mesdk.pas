
{Summary: the abstract Interceptor Object for method or function.}

{ Description
  the Interceptor allow us intercept the specified method or procedure, and you
  can do something before the method execute, after the method exceute or when
  the method raise exception..
  Interceptor Chain: a method intercepted by a interceptor, another one can
  intercept it too.

* cn *
拦截器的设计目标: 
  拦截指定的方法,在指定方法执行前,执行后拦截或发生异常的时候拦截。
  允许多次拦截：当一个方法被一个拦截器拦截后，另一个拦截器同样指定该方法拦截。
    拦截的顺序为后一个拦截器会先执行。

License:
    * The contents of this file are released under the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeInterceptor.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2006-2008
    * All rights reserved.

    * Contributor(s):
}
unit uMeInterceptor;

interface

{$I MeSetting.inc}
uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, TypInfo
  , Contnrs
  , uMeConsts
  , uMeSystem
  , uMeObject
  , uMeTypInfo
  {$IFDEF MeRTTI_SUPPORT}
  , uMeTypes
  , uMeProcType
  {$ENDIF}
  , uMeInjector
  ;

const
  vmtiExecute = 0;

type
  {
  @param esAllowed the procedure or method is allowed to execute.
  @param esBefore the BeforeExecute event is already triggered
  @param esAfter the intercepted procedure or method is already executed
  @param esException the occur exception .  
  }
  TMeExecuteState = (esAllowed, esBefore, esAfter, esException);
  TMeExecuteStates = set of TMeExecuteState;

type
  TMeInterceptedMethodItem = class;
  TMeInterceptedClassItem = class;
  TMeInterceptedClassList = class;
  TMeAbstractInterceptor = class;
  EMeInterceptorError = Class(EMeError);
  TMeInterceptorClass = class of TMeAbstractInterceptor;
  TMeAllowExecuteEvent = function(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const Params: PMeProcParams = nil): Boolean of object;
  TMeExecuteEvent = procedure(Sender: TObject; MethodItem: TMeInterceptedMethodItem; const Params: PMeProcParams = nil) of object;
  TMeAfterExecuteEvent = procedure(Sender: TObject; MethodItem: TMeInterceptedMethodItem
     ; const thisState: TMeExecuteStates
     ; const Params: PMeProcParams = nil) of object;
  TMeAfterExceptionEvent = function(Sender: TObject; MethodItem: TMeInterceptedMethodItem
    ; const E: Exception
    ; const Params: PMeProcParams = nil): Boolean of object;

  //##see uMeObject.TMeObjectMethod
  //TMeMethodProc = procedure of object;

  //the list item for the Intercept Method(procedure) List.
  {PInterceptMethod = ^TInterceptMethod;
  TInterceptMethod = packed record
    //Method or procedure name if any
    MethodName: string;
    //the original method location is in the first Interceptor's Injector  
    MethodInjector: PMeInjector;
    //for the procedure(method) with parameters.
    MethodTypeInfo: PTypeData;
    Interceptors: TMeInterceptMethodItem;
  end; //}

  { Description
  NOTE: we treat the procedure as a special class(InjectedClass = nil)

  one method(procedure) is injected only once.

  the Items is the interceptor-links to the method.

  拦截的方法（过程）仅仅只被 Injector 注入一次。
  注入的新过程在第一个拦截器上(Items[0])。
  然后由拦截器上的这个新过程遍历所有的 Items(该方法上的所有拦截器).
  }
  TMeInterceptedMethodItem = class(TList)
  private
    FMethodParams: {$IFDEF MeRTTI_SUPPORT} PMeProcType {$ELSE}
            PTypeInfo{$ENDIF};
    FName: string;
    function GetAddress: Pointer;
    function GetItems(Index: Integer): TMeAbstractInterceptor;
    function GetMethodType: TMethodType;
  protected
    FInjector: TMeInjector;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    destructor Destroy; override;
    function Add(aItem: TMeAbstractInterceptor): Integer;
    { Summary Return the index of the list when the NewMethod pointer is in the
            list }
    { Description
    if found Return the index of the list else return -1.
    }
    function Find(aClass: TMeInterceptorClass): Integer;
    function RemoveInterceptorClass(aClass: TMeInterceptorClass): Integer;
    procedure RemoveInterceptor(aInterceptor: TMeAbstractInterceptor);
    { Summary the method address }
    property Address: Pointer read GetAddress;
    property Injector: TMeInjector read FInjector write FInjector;
    property Items[Index: Integer]: TMeAbstractInterceptor read GetItems;
            default;
    { Summary the method parameters if any. }
    property MethodParams: {$IFDEF MeRTTI_SUPPORT} PMeProcType {$ELSE}
            PTypeInfo{$ENDIF} read FMethodParams write FMethodParams;
    property MethodType: TMethodType read GetMethodType;
    { Summary Method or procedure name if any }
    property Name: string read FName write FName;
  end;

  { Summary the Intecerpted class info(Methods) }
  { Description
  被拦截的类的信息
  }
  TMeInterceptedClassItem = class(TList)
  private
    function GetItems(Index: Integer): TMeInterceptedMethodItem;
  protected
    FOwner: TClass;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Add(aItem: TMeInterceptedMethodItem): Integer;
    { Summary Find Static method or procedure. }
    function FindDynamicMethod(aIndex: Integer; const aName: string = ''):
            Integer;
    { Summary Find method or procedure. }
    { Description
    if the Method is Virtual or dynamic then 
     aMethod is the index number.
    }
    function FindMethod(aMethod: Pointer; aMethodType: TMethodType; const
            aName: string = ''): Integer;
    function FindMethodByActualAddr(aMethod: Pointer): Integer;
    function FindMethodByAddr(aMethod: Pointer): Integer;
    function FindMethodByName(aName: string): Integer;
    { Summary Find Static method or procedure. }
    function FindPublishedMethod(aEntry: Pointer; const aName: string = ''):
            Integer;
    { Summary Find Static method or procedure. }
    function FindStaticMethod(aPatchLocation: Pointer; aMethodType: TMethodType;
            const aName: string = ''): Integer;
    { Summary Find Static method or procedure. }
    function FindVirtualMethod(aIndex: Integer; const aName: string = ''):
            Integer;
    { Summary Find Static method or procedure. }
    function GetDynamicMethod(aIndex: Integer; const aName: string = ''):
            TMeInterceptedMethodItem;
    { Summary Find method or procedure. }
    { Description
    if the Method is Virtual or dynamic then 
     aMethod is the index number.
    }
    function GetMethod(aMethod: Pointer; aMethodType: TMethodType; const aName:
            string = ''): TMeInterceptedMethodItem;
    function GetMethodByActualAddr(aMethod: Pointer): TMeInterceptedMethodItem;
    function GetMethodByAddr(aMethod: Pointer): TMeInterceptedMethodItem;
    function GetMethodByName(aName: string): TMeInterceptedMethodItem;
    { Summary Find Static method or procedure. }
    function GetStaticMethod(aProc: Pointer; aMethodType: TMethodType; const
            aName: string = ''): TMeInterceptedMethodItem;
    { Summary Find Static method or procedure. }
    function GetVirtualMethod(aIndex: Integer; const aName: string = ''):
            TMeInterceptedMethodItem;
    { Summary is intercepted procedure or static method? }
    function IsProcedureIntercepted(aInterceptorClass: TMeInterceptorClass;
            aMethod: Pointer; aMethodType: TMethodType; const aName: string =
            ''): Boolean;
    procedure RemoveInterceptor(aInterceptor: TMeAbstractInterceptor);
    property Items[Index: Integer]: TMeInterceptedMethodItem read GetItems;
            default;
    { Summary these methods are owned by the owner. }
    { Description
    Note: we treat the procedure as a special class(Owner = nil)
    }
    property Owner: TClass read FOwner;
  end;

  { Summary maintain the injected classes in the list. }
  { Description
  NOTE: we treat the procedure as a special class(InjectedClass = nil)
  }
  TMeInterceptedClassList = class(TList)
  private
    function GetItems(Index: Integer): TMeInterceptedClassItem;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Add(aItem: TMeInterceptedClassItem): Integer;
    { Summary Return the index of the list when the NewMethod pointer is in the
            list }
    { Description
    if found Return the index of the list else return -1.
    }
    function Find(aClass: TClass): Integer;
    function GetItem(aClass: TClass): TMeInterceptedClassItem;
    { Summary Return the index of the list when the aMethod(ActualAddress)
            pointer is in the list }
    { Description
    if found Return the index of the list else return -1.
    }
    function GetMethodItem(aMethod: Pointer): TMeInterceptedMethodItem;
    { Summary is intercepted procedure or static method? }
    function IsProcedureIntercepted(aInterceptorClass: TMeInterceptorClass;
            aPatchLocation: Pointer; aMethodType: TMethodType; aClass: TClass =
            nil; const aName: string = ''): Boolean;
    procedure RemoveInterceptor(aInterceptor: TMeAbstractInterceptor);
    property Items[Index: Integer]: TMeInterceptedClassItem read GetItems;
            default;
  end;

  { Summary 最底层的拦截器原型抽象,支持无参数的方法和过程,是AOP的核心类。 }
  TMeAbstractInterceptor = class(TObject)
  private
    FEnabled: Boolean;
    FOnAfterException: TMeAfterExceptionEvent;
    FOnAfterExecute: TMeAfterExecuteEvent;
    FOnAllowExecute: TMeAllowExecuteEvent;
    FOnBeforeExecute: TMeExecuteEvent;
  protected
    { Summary when occur the exception in execute the method }
    { Description
    Note: the Self is point to the injected method's class instance.
    }
    function iAfterException(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const E: Exception; const Params: PMeProcParams
            = nil): Boolean;
    procedure iAfterExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
            Params: PMeProcParams = nil);
    function iAllowExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const Params: PMeProcParams = nil):
            Boolean;
    procedure iBeforeExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
    { Summary Return the address to inject. }
    { Description
    this is procedure is the Static Method Process Center
    }
    class function GetMethodProcessCenter: Pointer; virtual; abstract;
    { Summary trigger if raised exception. }
    { Description
     @param Sender      the Sender is the intercepted method's class instance. 
                        nil means this is a procedure.
     @param MethodItem  the intercepted method or procedure item.
     @param Params      the parameters of the intercepted method or procedure. 
                        nil means this procedure is no any parameters.
     @param Result      re-raise the exception if true, the default is true.
    }
    function AfterException(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const E: Exception; const Params: PMeProcParams
            = nil): Boolean; virtual;
    { Summary trigger on after the MethodItem is executed even though the MethodItem's raised exception }
    { Description
     @param Sender      the Sender is the intercepted method's class instance. 
                        nil means this is a procedure.
     @param MethodItem  the intercepted method or procedure item.
     @param Params      the parameters of the intercepted method or procedure. 
                        nil means this procedure is no any parameters.

    Note: 
      this will always be triggered after done.
    }
    procedure AfterExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
            Params: PMeProcParams = nil); virtual;
    { Summary run the methodItem only return True. }
    { Description
    this occur before the BeforeExecute.
    If you do not override it always return true.

     @param Sender      the Sender is the intercepted method's class instance. 
                        nil means this is a procedure.
     @param MethodItem  the intercepted method or procedure item.
     @param Params      the parameters of the intercepted method or procedure. 
                        nil means this procedure is no any parameters.
    }
    function AllowExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem;
            const Params: PMeProcParams = nil): Boolean; virtual;
    { Summary trigger on before the MethodItem is executed and after
            AllowExecute return true. }
    { Description
     @param Sender      the Sender is the intercepted method's class instance. 
                        nil means this is a procedure.
     @param MethodItem  the intercepted method or procedure item.
     @param Params      the parameters of the intercepted method or procedure. 
                        nil means this procedure is no any parameters.
    }
    procedure BeforeExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
            virtual;
    { Summary Check this method whether has already been inptercepted by itself
            }
    { Description
    it raise exception if already intercepted by itself 

    or return the aInterceptClass(it will create if not exists) and a
    TMeInterceptMethodItem(if any)
    }
    class function CheckIntercept(aMethod: Pointer; aMethodType: TMethodType;
            out aInterceptClass: TMeInterceptedClassItem; const aName: string =
            ''): TMeInterceptedMethodItem;
    { Summary Add the Interceptor to a method. }
    class function AddTo(aClass: TClass; aMethod: Pointer;  const aMethodName:
            string = ''; aMethodParams: PTypeInfo = nil):
            TMeAbstractInterceptor; overload;
    { Summary Add a Interceptor to a dynamic method. }
    class function AddToDynamicMethod(aClass: TClass; aMethodIndex: Integer;
            const aMethodName: string = ''; aMethodParams: PTypeInfo = nil):
            TMeAbstractInterceptor;
    { Summary Add the Interceptor to a published method. }
    class function AddTo(aClass: TClass; aMethodName: String;  aMethodParams:
            PTypeInfo = nil): TMeAbstractInterceptor; overload;
    { Summary Add the Interceptor to a published Property. }
    { Description
             TODO:  need DUnit test.
    Note: the property method Must be named by: Get/Set/IsStored + property
    name
    }
    class function AddToProperty(aClass: TClass; aPropName: String;
            aPropSpecifiers: TMePropertySpecifiers = cAllPropertySpecifiers;
            aCallingConvention: TCallingConvention = ccRegister):
            TMeAbstractInterceptor;
    { Summary remove the Interceptor from the procedure. }
    class function RemoveFrom(aProc: Pointer): Boolean; overload;
    { Summary remove the Interceptor from the method or procedure. }
    class function RemoveFrom(aClass: TClass; aMethod: Pointer): Boolean;
            overload;
    { Summary remove the Interceptor from the published method. }
    class function RemoveFrom(aClass: TClass; aMethodName: String;
            aMethodParams: PTypeData = nil): Boolean; overload;
    { Summary Add the Interceptor to a procedure. }
    class function AddTo(aProc:Pointer; const aProcName: string = '';
            aMethodParams: PTypeInfo = nil): TMeAbstractInterceptor; overload;
    { Summary Add a Interceptor to a procedure. }
    class function AddToProcedure(aProc: Pointer; const aProcName: string = '';
            aProcParams: PTypeInfo = nil): TMeAbstractInterceptor;
    { Summary Add a Interceptor to a method or a procedure. }
    class function AddToStaticMethod(aClass: TClass; aMethod: Pointer;  const
            aMethodName: string = ''; aMethodParams: PTypeInfo = nil):
            TMeAbstractInterceptor;
    { Summary Add a Interceptor to a virtual method. }
    class function AddToVirtualMethod(aClass: TClass; aMethodIndex: Integer;
            const aMethodName: string = ''; aMethodParams: PTypeInfo = nil):
            TMeAbstractInterceptor;
    class procedure ApplyMethodParams(aMethodItem: TMeInterceptedMethodItem;
            aClass: TClass; aMethodParams: PTypeInfo); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    { Summary Enabled/Disabled the Interceptor, the default is true. }
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property OnAfterException: TMeAfterExceptionEvent read FOnAfterException
            write FOnAfterException;
    property OnAfterExecute: TMeAfterExecuteEvent read FOnAfterExecute write
            FOnAfterExecute;
    property OnAllowExecute: TMeAllowExecuteEvent read FOnAllowExecute write
            FOnAllowExecute;
    property OnBeforeExecute: TMeExecuteEvent read FOnBeforeExecute write
            FOnBeforeExecute;
  end;

{$IFDEF MeRTTI_SUPPORT}
  TMeCustomInterceptor_NoParam = class(TMeAbstractInterceptor)
  protected
    { Summary the part is the new injected in the method. }
    { Description
    Warining: NERVER call it directly!!
    it should be always the first virtual method in the class.

    in this class impl to inject the no parameter methods only.
    }
    procedure iExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem);
    { Summary Return the address to inject. }
    { Description
    this is procedure is the Static Method Process Center
    }
    class function GetMethodProcessCenter: Pointer; override;
    function Execute(Sender: TObject; MethodItem: TMeInterceptedMethodItem):
            Pointer;
  end;

  { Summary 支持带参数或不带参数的方法和过程,包括修改参数或返回值 }
  TMeCustomInterceptor = class(TMeAbstractInterceptor)
  protected
    class function GetMethodProcessCenter: Pointer; override;
    procedure iExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem;
            const Params: PMeProcParams);
  end;

{$ENDIF}

{ Summary Return the Interceptor is aClass in the Interceptors list if any.}
{ Description
  if not found return nil.
}
function FindInterceptor(aClass: TMeInterceptorClass): TMeAbstractInterceptor;

{ Summary Return the Interceptor is aClass in the Interceptors list if any.}
{ Description
  if not found return create one to the Interceptors list.
}
function GetInterceptor(aClass: TMeInterceptorClass): TMeAbstractInterceptor;

function GInterceptedClasses(): TMeInterceptedClassList;

implementation

var
  FInterceptedClasses: TMeInterceptedClassList;
  //collect the interceptor instances.
  FInterceptors: TObjectList;

type
  //to visit the proteced fields.
  //TMeInterceptMethodItemAccess = class(TMeInterceptMethodItem);
  {$IFDEF MeRTTI_SUPPORT}
  TMeProcTypeAccess = object(TMeProcType)
  end;
  TMeParamTypeAccess = object(TMeParamType)
  end;
  {$ENDIF}
  TInjectorAccess =object(TMeInjector)
  end;

function GInterceptedClasses(): TMeInterceptedClassList;
begin
  Result := FInterceptedClasses;
end;

function FindInterceptorItem(aClass: TMeInterceptorClass): Integer;
begin
  for Result := 0 to FInterceptors.Count - 1 do
  begin
    if FInterceptors[Result].ClassType = aClass then
    begin
      exit;
    end;
  end;
  Result := -1;
end;

function FindInterceptor(aClass: TMeInterceptorClass): TMeAbstractInterceptor;
var
  i: Integer;
begin
  i := FindInterceptorItem(aClass);
  if i <> - 1 then
    Result := TMeAbstractInterceptor(FInterceptors[i])
  else
    Result := nil; 
end;

function GetInterceptor(aClass: TMeInterceptorClass): TMeAbstractInterceptor;
begin
  Result := FindInterceptor(aClass);
  if Result = nil then
  begin 
    Result := aClass.Create;
    FInterceptors.Add(Result);
  end;
end;

//由于过程地址是唯一的,所以可以通过 aMethod 来查找 Interceptor.
procedure RunInterceptorByAddr(Self: TObject; aMethod: Pointer);
var
  vMethodItem: TMeInterceptedMethodItem;
  vInterceptor: TMeCustomInterceptor_NoParam;
begin
  //the intercepted method item
  vMethodItem := FInterceptedClasses.GetMethodItem(aMethod);
  if Assigned(vMethodItem) and (vMethodItem.Count > 0) then
  begin
    vInterceptor := TMeCustomInterceptor_NoParam(vMethodItem[0]);
    if vMethodItem.Injector.MethodType = mtProcedure then
    begin
      Self := nil;
    end;
    //编译器应该将vInterceptor 传入到 EAX 中了。
    vInterceptor.iExecute(Self, vMethodItem);
  end;
end;

procedure StaticMethodProcessCenter(Self: TObject);
asm
   //EAX - Self
   //PUSH EAX
   //MOV  ECX, EDX //the EDX maybe is return paramter if it is method function
   POP  EDX  // the return address
   SUB  EDX, cNearJMPDirectiveSize //get the entry address of the procedure or method
   //"TYPE TRedirectCodeRec" means SizeOf(TRedirectCodeRec) in asm. 
   CALL RunInterceptorByAddr
   //POP  EAX
end;


{$IFDEF MeRTTI_SUPPORT}
function RunInterceptorWithParamByAddr(const aMethod: Pointer
  ; aLastParamPtr: Pointer
  ; aReigsters: PInteger
  ; Self: TObject): PMeProcType;
var
  vMethodItem: TMeInterceptedMethodItem;
  vInterceptor: TMeCustomInterceptor;
  vParams: PMeProcParams;
begin
  //the intercepted method item
  vMethodItem := FInterceptedClasses.GetMethodItem(aMethod);
  if Assigned(vMethodItem) and (vMethodItem.Count > 0) and (vMethodItem[0] is TMeCustomInterceptor) then
  try
    vParams := nil;
    vInterceptor := TMeCustomInterceptor(vMethodItem[0]);
    if Assigned(vMethodItem.MethodParams) then
    begin
      New(vParams, Create);
      vParams.InitFromType(vMethodItem.MethodParams); 
      vParams.AssignFromStack(aLastParamPtr, aReigsters);
    end;
    if vMethodItem.Injector.MethodType = mtProcedure then
    begin
      Self := nil;
    end;
    //编译器应该将vInterceptor 传入到 EAX 中了。
    vInterceptor.iExecute(Self, vMethodItem, vParams);
    if Assigned(vParams) then
    begin
      //vParams.SaveToStack(aLastParamPtr, aReigsters);
      vParams.SaveResult(aReigsters);
    end;
  finally
    MeFreeAndNil(vParams);
  end;
  if Assigned(vMethodItem) then
    Result := vMethodItem.MethodParams
  else 
    Result := nil;
end;

{
在调用后的堆栈：
  压入的最后一个参数
  返回地址  <-- 原始的CALL留下的
  返回地址  <-- 我的CALL留下的
  PUSH EBP  <-- 编译器建立的代码（局部变量开辟Stack Frame）
  MOV  EBP, ESP
}
procedure StaticMethodProcessCenterWithParam(Self: TObject);
var
  vRegisters: TMeCommonRegs;
  vLastParamPtr: Pointer; //the parameter address in the stack.
  vProcAddress: Pointer;
  //vProcInfo: PMeProcType;
const
  cLocalVarSize = SizeOf(vRegisters) + SizeOf(vLastParamPtr) + SizeOf(vProcAddress) 
  ;
  cLocalStackFrameSize = cLocalVarSize
    + SizeOf(Pointer) //the pushed EBP
  ;
asm
    MOV  vRegisters.&EAX, EAX  //vEAX Address: EBP - $0C
    MOV  vRegisters.&EDX, EDX  //-$8
    MOV  vRegisters.&ECX, ECX  //-$4

    MOV  EDX, EBP
    ADD  EDX, $0C   //EDX = EBP + $0C(OldEBP(4bytes)+RETAddrss(4bytes) + RETAddrss(4bytes))
    MOV  vLastParamPtr, EDX  //move the last parameter address in the stack to vParamPtr

    //MOV  ECX, EDX //the EDX maybe is return paramter if it is method function
    //POP  ECX  // the Old EBP
    //POP  EAX  // the my return address
    //PUSH ECX  // restore the EBP
    //Get the procedure address
    MOV  EAX, [ESP+cLocalStackFrameSize]
    MOV  vProcAddress, EAX

    //remove the procedure address from stack
    MOV  ECX, cLocalStackFrameSize
    SUB  ECX, 8
    LEA  EAX, [ESP]
    LEA  EDX, [ESP+Type Pointer]  
@@SmallMove: {9..32 Byte Move}
  fild    qword ptr [eax+ecx] {Load Last 8}
  fild    qword ptr [eax] {Load First 8}
  cmp     ecx, 8
  jle     @@Small16
  fild    qword ptr [eax+8] {Load Second 8}
  cmp     ecx, 16
  jle     @@Small24
  fild    qword ptr [eax+16] {Load Third 8}
  fistp   qword ptr [edx+16] {Save Third 8}
@@Small24:
  fistp   qword ptr [edx+8] {Save Second 8}
@@Small16:
  fistp   qword ptr [edx] {Save First 8}
  fistp   qword ptr [edx+ecx] {Save Last 8}
    
    ADD  ESP, Type Pointer
    ADD  EBP, Type Pointer

    MOV  EAX, vProcAddress    
   {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
    SUB  EAX, cNearJMPDirectiveSize //get the entry address of the procedure or method
   {$ELSE}
    SUB  EAX, TYPE TRedirectCodeRec //get the entry address of the procedure or method
   {$ENDIF}
    //"TYPE TRedirectCodeRec" means SizeOf(TRedirectCodeRec) in asm. 

    MOV  EDX, vLastParamPtr
    LEA  ECX, vRegisters
    PUSH vRegisters.&EAX // push the Self Parameter in RunInterceptorWithParamByAddr 
    CALL RunInterceptorWithParamByAddr

    TEST EAX, EAX
    JZ   @@exit
    
    MOV  ECX, [EAX].TMeProcTypeAccess.FResultType
    TEST ECX, ECX
    JZ   @@exit
    

    MOV  CL, [EAX].TMeProcTypeAccess.FCallingConvention
    CMP  CL, ccCdecl
    JZ   @@RetStoreResult
    CALL TMeProcType.GetStackTypeSize
    TEST EAX, EAX
    JZ   @@RetStoreResult
    MOV  ECX, EAX

    MOV  EAX, vRegisters.&EAX 
    MOV  EDX, vRegisters.&EDX 

    MOV  ESP, EBP //clear the local var space in the stack

    POP  EBP      //restore the old EBP before calling..
    //POP  ECX // pop-up the ret address
    FLD  [ESP]
    //ADD  ECX, TYPE Pointer
    ADD  ESP, ECX //Clear the proc-parameters in the stack
    FSTP [ESP]
    RET
//}
@@RetStoreResult:
    //the local var address is alread changed!
    MOV  EAX, vRegisters.&EAX   //vEAX Address: EBP - $0C
    MOV  EDX, vRegisters.&EDX  //-$8
@@exit:
end;
{$ENDIF}

{
*************************** TMeInterceptedMethodItem ***************************
}
destructor TMeInterceptedMethodItem.Destroy;
begin
  Injector.Enabled := False;
  inherited Destroy;
end;

function TMeInterceptedMethodItem.Add(aItem: TMeAbstractInterceptor): Integer;
begin
  Result := -1;
  if Assigned(aItem) then
  begin
    Result := IndexOf(aItem);
    if Result = -1 then
      Result := inherited Add(aItem);
    //else //i have already checked in TMeCustomInterceptor_NoParam.CheckIntercept
      //Raise EMeInterceptorError.CreateRes();
  end;
end;

function TMeInterceptedMethodItem.Find(aClass: TMeInterceptorClass): Integer;
begin
  For Result := 0 to Count -1 do
  begin
      if (Items[Result].ClassType = aClass) then exit;
  end;
  Result := -1;
end;

function TMeInterceptedMethodItem.GetAddress: Pointer;
begin
  {$IFDEF INJECT_DLL_SUPPORT}
  Result := FInjector.MethodOriginalActualLocation;
  {$ELSE}
  Result := FInjector.MethodOriginalLocation;
  {$ENDIF}
end;

function TMeInterceptedMethodItem.GetItems(Index: Integer):
        TMeAbstractInterceptor;
begin
  Result := Get(Index);
end;

function TMeInterceptedMethodItem.GetMethodType: TMethodType;
begin
  Result := FInjector.MethodType;
end;

procedure TMeInterceptedMethodItem.Notify(Ptr: Pointer; Action:
        TListNotification);
begin
  {if (Action = lnDeleted) and (Ptr <> nil) then
  begin
    FreeAndNil(TMeCustomInterceptor_NoParam(Ptr));
  end;//}
end;

function TMeInterceptedMethodItem.RemoveInterceptorClass(aClass: TMeInterceptorClass): Integer;
begin
  Result := Find(aClass);
  if Result >= 0 then
    Delete(Result);
end;

procedure TMeInterceptedMethodItem.RemoveInterceptor(aInterceptor:
        TMeAbstractInterceptor);
var
  i: Integer;
begin
  For i := Count -1 downto 0 do
  begin
    if Items[i] = aInterceptor then
     Delete(i);
  end;
end;

{
*************************** TMeInterceptedClassItem ****************************
}
function TMeInterceptedClassItem.Add(aItem: TMeInterceptedMethodItem): Integer;
begin
  Result := inherited Add(aItem);
end;

function TMeInterceptedClassItem.FindDynamicMethod(aIndex: Integer; const
        aName: string = ''): Integer;
begin
  for Result := 0 to Count -1 do
  begin
    with Items[Result] do
    if (Injector.MethodType = mtDynamic) and (aName = Name)
      and (Injector.MethodIndex=aIndex)
    then
      Exit;
  end;
  Result := -1;
end;

function TMeInterceptedClassItem.FindMethod(aMethod: Pointer; aMethodType:
        TMethodType; const aName: string = ''): Integer;
begin
  Result := -1;
  case aMethodType of
    mtPublished: Result := FindPublishedMethod(aMethod, aName);
    mtVirtual: Result := FindVirtualMethod(Integer(aMethod), aName);
    mtDynamic: Result := FindDynamicMethod(Integer(aMethod), aName);
    mtStatic, mtProcedure: Result := FindStaticMethod(aMethod, aMethodType, aName);
    mtUnknown: if aName <> '' then
    begin
      Result := FindMethodByName(aName);
    end;
  end;//case
end;

function TMeInterceptedClassItem.FindMethodByActualAddr(aMethod: Pointer):
        Integer;

  {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
  var
    vAddr: Pointer;
  {$ENDIF}

begin
  for Result := 0 to Count -1 do
  begin
    {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
    with TInjectorAccess(Items[Result].Injector) do
    begin
      vAddr := MethodOriginalActualLocation;
      if (FMethodOriginalBackup.Offset = Integer(cX86NoOpDirective4Bytes))
        and (FMethodOriginalBackup.Jump = cX86NoOpDirective) then
        {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
        Integer(vAddr) := Integer(vAddr) - cNearJMPDirectiveSize;
        {$ELSE}
        Integer(vAddr) := Integer(vAddr) - SizeOf(TRedirectCodeRec);
        {$ENDIF}
    end;
    if aMethod = vAddr then
      Exit;
    {$ELSE}
    if aMethod = Items[Result].Injector.MethodOriginalActualLocation then
      Exit;
    {$ENDIF}
  end;
  Result := -1;
end;

function TMeInterceptedClassItem.FindMethodByAddr(aMethod: Pointer): Integer;
begin
  for Result := 0 to Count -1 do
  begin
    if aMethod = Items[Result].Injector.MethodOriginalLocation then
      Exit;
  end;
  Result := -1;
end;

function TMeInterceptedClassItem.FindMethodByName(aName: string): Integer;
begin
  for Result := 0 to Count -1 do
  begin
    if aName = Items[Result].Name then
      Exit;
  end;
  Result := -1;
end;

function TMeInterceptedClassItem.FindPublishedMethod(aEntry: Pointer; const
        aName: string = ''): Integer;
begin
  for Result := 0 to Count -1 do
  begin
    with Items[Result] do
    if (Injector.MethodType = mtPublished) and (aName = Name)
      and (Injector.MethodIndex=Integer(aEntry))
    then
      Exit;
  end;
  Result := -1;
end;

function TMeInterceptedClassItem.FindStaticMethod(aPatchLocation: Pointer;
        aMethodType: TMethodType; const aName: string = ''): Integer;
begin
  for Result := 0 to Count -1 do
  begin
    with Items[Result] do
    if (Injector.MethodType = aMethodType) and (aName = Name)
      and (Injector.MethodOriginalLocation=aPatchLocation)
    then
      Exit;
  end;
  Result := -1;
end;

function TMeInterceptedClassItem.FindVirtualMethod(aIndex: Integer; const
        aName: string = ''): Integer;
begin
  for Result := 0 to Count -1 do
  begin
    with Items[Result] do
    if (Injector.MethodType = mtVirtual) and (aName = Name)
      and (Injector.MethodIndex=aIndex)
    then
      Exit;
  end;
  Result := -1;
end;

function TMeInterceptedClassItem.GetDynamicMethod(aIndex: Integer; const aName:
        string = ''): TMeInterceptedMethodItem;
var
  I: Integer;
begin
  I := FindDynamicMethod(aIndex, aName);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TMeInterceptedClassItem.GetItems(Index: Integer):
        TMeInterceptedMethodItem;
begin
  Result := Get(Index);
end;

function TMeInterceptedClassItem.GetMethod(aMethod: Pointer; aMethodType:
        TMethodType; const aName: string = ''): TMeInterceptedMethodItem;
var
  I: Integer;
begin
  I := FindMethod(aMethod, aMethodType, aName);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TMeInterceptedClassItem.GetMethodByActualAddr(aMethod: Pointer):
        TMeInterceptedMethodItem;
var
  I: Integer;
begin
  I := FindMethodByActualAddr(aMethod);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TMeInterceptedClassItem.GetMethodByAddr(aMethod: Pointer):
        TMeInterceptedMethodItem;
var
  I: Integer;
begin
  I := FindMethodByAddr(aMethod);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TMeInterceptedClassItem.GetMethodByName(aName: string):
        TMeInterceptedMethodItem;
var
  I: Integer;
begin
  I := FindMethodByName(aName);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TMeInterceptedClassItem.GetStaticMethod(aProc: Pointer; aMethodType:
        TMethodType; const aName: string = ''): TMeInterceptedMethodItem;
var
  I: Integer;
begin
  I := FindStaticMethod(aProc, aMethodType, aName);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TMeInterceptedClassItem.GetVirtualMethod(aIndex: Integer; const aName:
        string = ''): TMeInterceptedMethodItem;
var
  I: Integer;
begin
  I := FindVirtualMethod(aIndex, aName);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TMeInterceptedClassItem.IsProcedureIntercepted(aInterceptorClass:
        TMeInterceptorClass; aMethod: Pointer; aMethodType: TMethodType; const
        aName: string = ''): Boolean;
var
  I: Integer;
  vInterceptors: TMeInterceptedMethodItem;
begin
  I := FindStaticMethod(aMethod, aMethodType, aName);
  Result := I >= 0;
  if Result then
  begin
    vInterceptors := Items[I];
    I := vInterceptors.Find(aInterceptorClass);
    Result := I >= 0;
  end;
end;

procedure TMeInterceptedClassItem.Notify(Ptr: Pointer; Action:
        TListNotification);
begin
  if (Action = lnDeleted) and (Ptr <> nil) then
  begin
    FreeAndNil(TMeInterceptedMethodItem(Ptr));
  end;
end;

procedure TMeInterceptedClassItem.RemoveInterceptor(aInterceptor:
        TMeAbstractInterceptor);
var
  i: Integer;
begin
  For i := Count -1 downto 0 do
  begin
     Items[i].RemoveInterceptor(aInterceptor);
  end;
end;

{
*************************** TMeInterceptedClassList ****************************
}
function TMeInterceptedClassList.Add(aItem: TMeInterceptedClassItem): Integer;
begin
  Result := inherited Add(aItem)
end;

function TMeInterceptedClassList.Find(aClass: TClass): Integer;
begin
  For Result := 0 to Count -1 do
  begin
     if Items[Result].Owner = aClass then exit;
  end;
  Result := -1;
end;

function TMeInterceptedClassList.GetItem(aClass: TClass):
        TMeInterceptedClassItem;
var
  I: Integer;
begin
  I := Find(aClass);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TMeInterceptedClassList.GetItems(Index: Integer):
        TMeInterceptedClassItem;
begin
  Result := Get(Index);
end;

function TMeInterceptedClassList.GetMethodItem(aMethod: Pointer):
        TMeInterceptedMethodItem;
var
  I: Integer;
begin
  For I := 0 to Count -1 do
  begin
     Result := Items[I].GetMethodByActualAddr(aMethod);
     if Result <> nil then
     begin
       Exit;
     end;
  end;
  Result := nil;
end;

function TMeInterceptedClassList.IsProcedureIntercepted(aInterceptorClass:
        TMeInterceptorClass; aPatchLocation: Pointer; aMethodType: TMethodType;
        aClass: TClass = nil; const aName: string = ''): Boolean;
var
  I: Integer;
  vInterceptClass: TMeInterceptedClassItem;
begin
  I := Find(aClass);
  Result := I >= 0;
  if Result then
  begin
    vInterceptClass := Items[I];
    Result := vInterceptClass.IsProcedureIntercepted(aInterceptorClass, aPatchLocation, aMethodType, aName);
  end;
end;

procedure TMeInterceptedClassList.Notify(Ptr: Pointer; Action:
        TListNotification);
begin
  if (Action = lnDeleted) and (Ptr <> nil) then
  begin
    FreeAndNil(TMeInterceptedClassItem(Ptr));
  end;
end;

procedure TMeInterceptedClassList.RemoveInterceptor(aInterceptor:
        TMeAbstractInterceptor);
var
  i: Integer;
begin
  For i := Count -1 downto 0 do
  begin
     Items[i].RemoveInterceptor(aInterceptor);
  end;
end;

{
**************************** TMeAbstractInterceptor ****************************
}
constructor TMeAbstractInterceptor.Create;
begin
  if FindInterceptor(TMeInterceptorClass(ClassType)) <> nil then
  begin
    Raise EMeInterceptorError.CreateResFmt(@rsDuplicateInterceptorError, [ClassName]);
  end;
  FEnabled := True;
  inherited Create;
end;

destructor TMeAbstractInterceptor.Destroy;
begin
  if Assigned(FInterceptedClasses) then
    FInterceptedClasses.RemoveInterceptor(Self);
  if Assigned(FInterceptors) then
    FInterceptors.Extract(Self);
  inherited Destroy;
end;

class function TMeAbstractInterceptor.AddTo(aProc:Pointer; const aProcName:
        string = ''; aMethodParams: PTypeInfo = nil): TMeAbstractInterceptor;
begin
  Result := AddToProcedure(aProc, aProcName, aMethodParams);
end;

class function TMeAbstractInterceptor.AddTo(aClass: TClass; aMethod: Pointer;
        const aMethodName: string = ''; aMethodParams: PTypeInfo = nil):
        TMeAbstractInterceptor;
begin
  Result := AddToStaticMethod(aClass, aMethod, aMethodName, aMethodParams);
end;

class function TMeAbstractInterceptor.AddTo(aClass: TClass; aMethodName: String;
        aMethodParams: PTypeInfo = nil): TMeAbstractInterceptor;
var
  vInterceptedMethod: TMeInterceptedMethodItem;
  vClass: TMeInterceptedClassItem;
  vMethod: PPublishedMethodEntry;
begin
  if aClass <> nil then
  begin
    vMethod := FindPublishedMethodEntryByName(aClass, aMethodName);
    if vMethod = nil then
      Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownMethodError, [aMethodName]);
    vClass := FInterceptedClasses.GetItem(aClass);
    vInterceptedMethod := CheckIntercept(vMethod.Address, mtStatic, vClass, aMethodName);
    if vClass.FOwner = nil then
      vClass.FOwner := aClass;
    Result := GetInterceptor(Self);
    try
      //Result.Injector
      if Assigned(vInterceptedMethod) then
      //Do not patch only add the result to the chain.
      begin
        //Result.FInterceptedChain := vInterceptedMethod;
        vInterceptedMethod.Add(Result);
      end
      else //this is the first patch it;
      begin
        vInterceptedMethod := TMeInterceptedMethodItem.Create;
        try
          vInterceptedMethod.Name := aMethodName;
          ApplyMethodParams(vInterceptedMethod, aClass, aMethodParams);
          with vInterceptedMethod.Injector do
            if not InjectStaticMethod(aClass, vMethod.Address, GetMethodProcessCenter, cX86CallDirective) then
              Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownError, [aMethodName]);
          vInterceptedMethod.Add(Result);
        except
          FreeAndNil(vInterceptedMethod);
          Raise;
        end;
        if Assigned(vInterceptedMethod)then
        begin
          vClass.Add(vInterceptedMethod);
          //Result.FInterceptedChain := vInterceptedMethod;
        end;
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

class function TMeAbstractInterceptor.AddToDynamicMethod(aClass: TClass;
        aMethodIndex: Integer;  const aMethodName: string = ''; aMethodParams:
        PTypeInfo = nil): TMeAbstractInterceptor;
var
  vInterceptedMethod: TMeInterceptedMethodItem;
  vClass: TMeInterceptedClassItem;
  vMethod: Pointer;
begin
  if aClass <> nil then
  begin
    vClass := FInterceptedClasses.GetItem(aClass);
    vMethod := GetDynamicMethodBySlot(aClass, aMethodIndex);
    if IsAbstractMethod(vMethod) then
      raise EMeInterceptorError.CreateRes(@rsInjectAbstractMethodError);

    vInterceptedMethod := CheckIntercept(vMethod, mtStatic, vClass, aMethodName);
    if vClass.FOwner = nil then
      vClass.FOwner := aClass;
    Result := GetInterceptor(Self);
    try
      //Result.Injector
      if Assigned(vInterceptedMethod) then
      //Do not patch only add the result to the chain.
      begin
        //Result.FInterceptedChain := vInterceptedMethod;
        vInterceptedMethod.Add(Result);
      end
      else //this is the first patch it;
      begin
        vInterceptedMethod := TMeInterceptedMethodItem.Create;
        try
          vInterceptedMethod.Name := aMethodName;
           ApplyMethodParams(vInterceptedMethod, aClass, aMethodParams);
         with vInterceptedMethod.Injector do
            if not InjectStaticMethod(aClass, vMethod, GetMethodProcessCenter, cX86CallDirective) then
              Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownError, [aMethodName]);
          vInterceptedMethod.Add(Result);
        except
          FreeAndNil(vInterceptedMethod);
          Raise;
        end;
        if Assigned(vInterceptedMethod)then
          vClass.Add(vInterceptedMethod);
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

class function TMeAbstractInterceptor.AddToProcedure(aProc: Pointer; const
        aProcName: string = ''; aProcParams: PTypeInfo = nil):
        TMeAbstractInterceptor;
var
  vInterceptedMethod: TMeInterceptedMethodItem;
  vClass: TMeInterceptedClassItem;
begin
  if aProc <> nil then
  begin
    vClass := FInterceptedClasses.GetItem(nil);
    vInterceptedMethod := CheckIntercept(aProc, mtProcedure, vClass, aProcName);
    //vClass.FOwner := nil;
    Result := GetInterceptor(Self);
    try
      //Result.Injector
      if Assigned(vInterceptedMethod) then
      //Do not patch only add the result to the chain.
      begin
        //Result.FInterceptedChain := vInterceptedMethod;
        vInterceptedMethod.Add(Result);
      end
      else //this is the first patch it;
      begin
        vInterceptedMethod := TMeInterceptedMethodItem.Create;
        try
          vInterceptedMethod.Name := aProcName;
          ApplyMethodParams(vInterceptedMethod, nil, aProcParams);
          with vInterceptedMethod.Injector do
            if not InjectProcedure(aProc, GetMethodProcessCenter, cX86CallDirective) then
            Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownError, [aProcName]);
          vInterceptedMethod.Add(Result);
        except
          FreeAndNil(vInterceptedMethod);
          Raise;
        end;
        if Assigned(vInterceptedMethod)then
          vClass.Add(vInterceptedMethod);
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

class function TMeAbstractInterceptor.AddToProperty(aClass: TClass; aPropName:
        String; aPropSpecifiers: TMePropertySpecifiers = cAllPropertySpecifiers;
        aCallingConvention: TCallingConvention = ccRegister):
        TMeAbstractInterceptor;
var
  vInterceptedMethod: TMeInterceptedMethodItem;
  vClass: TMeInterceptedClassItem;
  vPropInfo: PPropInfo;

  Type
    TPointerBytes = array[0..SizeOf(Pointer)-1] of Byte;
  procedure DoInject(var aInterceptMethod: TMeInterceptedMethodItem; const aSpecifier:
          TMePropertySpecifier);
  var
    s: string;
    vMethodParams: PMeProcType;
    vProc: Pointer;
    procedure SetProcTypeForPropInfo(aMethodParams: PMeProcType;
      aSpecifier: TMePropertySpecifier
      ; aCallingConvention: TCallingConvention
      ; const aPropInfo: PPropInfo
    );
    var
      vParamType: PMeParamType;
      vProcType: PMeProcType;
    begin
      with TMeProcTypeAccess(aMethodParams^) do
      begin
        FParamList.FreeMeObjects;
        FParamList.Clear;
        FCallingConvention := aCallingConvention;
        case aSpecifier of
          mpsGet:
          begin
            FMethodKind := mkFunction;
            if LongWord(aPropInfo.Index) <> $80000000 then
            begin
             //has index.
              vParamType := New(PMeParamType);
              FParamList.Add(vParamType);
              with TMeParamTypeAccess(vParamType^) do
              begin
                FProcType := aMethodParams;
                FParamType := GetRegisteredTypeByName('Integer');
                if FParamType = nil then
                  raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, ['Integer']);
                {$IFDEF MeRTTI_EXT_SUPPORT}
                FParamName := 'Index';
                {$ENDIF}
              end; //with
            end;
            FResultType := GetRegisteredTypeByName(aPropInfo.PropType^.Name);
            if FResultType = nil then
              Raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, [aPropInfo.PropType^.Name]);
            if RetOnStack then
            begin
              vParamType := New(PMeParamType);
              FParamList.Add(vParamType);
              with TMeParamTypeAccess(vParamType^) do
              begin
                FProcType := aMethodParams;
                FFlags := [pfVar];
                FParamType := FResultType;
              end;
            end;
          end;
          mpsSet:
          begin
            FMethodKind := mkProcedure;
            if LongWord(aPropInfo.Index) <> $80000000 then
            begin
             //has index.
              vParamType := New(PMeParamType);
              FParamList.Add(vParamType);
              with TMeParamTypeAccess(vParamType^) do
              begin
                FProcType := aMethodParams;
                FParamType := GetRegisteredTypeByName('Integer');
                if FParamType = nil then
                  raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, ['Integer']);
                {$IFDEF MeRTTI_EXT_SUPPORT}
                FParamName := 'Index';
                {$ENDIF}
              end; //with
            end;
            vParamType := New(PMeParamType);
            FParamList.Add(vParamType);
            with TMeParamTypeAccess(vParamType^) do
            begin
              FProcType := aMethodParams;
              FParamType := GetRegisteredTypeByName(aPropInfo.PropType^.Name);
              if FParamType = nil then
                raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, [aPropInfo.PropType^.Name]);
              FFlags := [pfConst];
              {$IFDEF MeRTTI_EXT_SUPPORT}
              FParamName := 'Value';
              {$ENDIF}
            end; //with
          end;
          mpsStored:
          begin
            FMethodKind := mkFunction;
            if LongWord(aPropInfo.Index) <> $80000000 then
            begin
             //has index.
              vParamType := New(PMeParamType);
              FParamList.Add(vParamType);
              with TMeParamTypeAccess(vParamType^) do
              begin
                FProcType := aMethodParams;
                FParamType := GetRegisteredTypeByName('Integer');
                if FParamType = nil then
                  raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, ['Integer']);
                {$IFDEF MeRTTI_EXT_SUPPORT}
                FParamName := 'Index';
                {$ENDIF}
              end; //with
            end;
            FResultType := GetRegisteredTypeByName('Boolean');
            if FResultType = nil then
              Raise EMeTypeError.CreateResFmt(@rsTypeNotSupportError, ['Boolean']);
          end;
        end; //case
      end; //with
      vProcType := RegisterProcType(aMethodParams);
      if vProcType <> aMethodParams then
      begin
        Dispose(aMethodParams);
        aMethodParams := nil;
      end;
    end;
  begin
      Case aSpecifier of
        mpsGet:
          begin
            s := 'Get'+aPropName;
            vProc := vPropInfo.GetProc;
          end;
        mpsSet:
          begin
            s := 'Set'+aPropName;
            vProc := vPropInfo.SetProc;
          end;
        mpsStored:
          begin
            s := 'IsStored'+aPropName;
            vProc := vPropInfo.StoredProc;
          end;
        else
          vProc := nil;
      End;
      aInterceptMethod := CheckIntercept(vPropInfo.GetProc, mtStatic, vClass, s);
      try
        //Result.Injector
        if Assigned(aInterceptMethod) then
        //Do not patch only add the result to the chain.
        begin
          //Result.FInterceptedChain := vInterceptedMethod;
          aInterceptMethod.Add(Result);
        end
        else //this is the first patch it;
        begin
          aInterceptMethod := TMeInterceptedMethodItem.Create;
          try
            aInterceptMethod.Name := s;
            New(vMethodParams, Create);
            SetProcTypeForPropInfo(vMethodParams, aSpecifier
              , aCallingConvention, vPropInfo);
            if Assigned(vMethodParams) then
              aInterceptMethod.MethodParams := vMethodParams
            else
              Raise EMeInterceptorError.CreateResFmt(@rsInterceptMethodParamsError, [s]);
            //ApplyMethodParams(aInterceptMethod, aClass, aMethodParams);
            with aInterceptMethod.Injector do
              if not InjectStaticMethod(aClass, vProc, GetMethodProcessCenter, cX86CallDirective) then
                Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownError, [s]);
            aInterceptMethod.Add(Result);
          except
            FreeAndNil(aInterceptMethod);
            Raise;
          end;
          if Assigned(vInterceptedMethod)then
          begin
            vClass.Add(vInterceptedMethod);
            //Result.FInterceptedChain := vInterceptedMethod;
          end;
        end;
      except
        FreeAndNil(Result);
        raise;
      end;
  end;

begin
  Result := nil;
  if aClass <> nil then
  begin
    vPropInfo := GetPropInfo(aClass, aPropName);
    if vPropInfo = nil then
      Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownPropertyError, [aPropName]);

    if not ((mpsGet in aPropSpecifiers) and Assigned(vPropInfo.GetProc)
       and (TPointerBytes(vPropInfo.GetProc)[High(TPointerBytes)] < $FE)) then
      Exclude(aPropSpecifiers, mpsGet);

    if not ((mpsSet in aPropSpecifiers) and Assigned(vPropInfo.SetProc)
       and (TPointerBytes(vPropInfo.SetProc)[High(TPointerBytes)] < $FE)) then
      Exclude(aPropSpecifiers, mpsSet);

    if not ((mpsStored in aPropSpecifiers) and Assigned(vPropInfo.StoredProc)
       and (TPointerBytes(vPropInfo.StoredProc)[High(TPointerBytes)] < $FE)) then
      Exclude(aPropSpecifiers, mpsStored);

    if aPropSpecifiers <> [] then
    begin
      vClass := FInterceptedClasses.GetItem(aClass);
      if vClass.FOwner = nil then
        vClass.FOwner := aClass;

      Result := GetInterceptor(Self);

      if mpsGet in aPropSpecifiers then
      begin
        DoInject(vInterceptedMethod, mpsGet);
      end;
      if mpsSet in aPropSpecifiers then
      begin
        DoInject(vInterceptedMethod, mpsSet);
      end;
      if mpsStored in aPropSpecifiers then
      begin
        DoInject(vInterceptedMethod, mpsStored);
      end;
    end;
  end;
end;

class function TMeAbstractInterceptor.AddToStaticMethod(aClass: TClass;
        aMethod: Pointer;  const aMethodName: string = ''; aMethodParams:
        PTypeInfo = nil): TMeAbstractInterceptor;
var
  vInterceptedMethod: TMeInterceptedMethodItem;
  vClass: TMeInterceptedClassItem;
begin
  if aMethod <> nil then
  begin
    vClass := FInterceptedClasses.GetItem(aClass);
    vInterceptedMethod := CheckIntercept(aMethod, mtStatic, vClass, aMethodName);
    if vClass.FOwner = nil then
      vClass.FOwner := aClass;
    Result := GetInterceptor(Self);
    try
      //Result.Injector
      if Assigned(vInterceptedMethod) then
      //Do not patch only add the result to the chain.
      begin
        //Result.FInterceptedChain := vInterceptedMethod;
        vInterceptedMethod.Add(Result);
      end
      else //this is the first patch it;
      begin
        vInterceptedMethod := TMeInterceptedMethodItem.Create;
        try
          vInterceptedMethod.Name := aMethodName;
          ApplyMethodParams(vInterceptedMethod, aClass, aMethodParams);
          with vInterceptedMethod.Injector do
            if not InjectStaticMethod(aClass, aMethod, GetMethodProcessCenter, cX86CallDirective) then
            Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownError, [aMethodName]);
          vInterceptedMethod.Add(Result);
        except
          FreeAndNil(vInterceptedMethod);
          Raise;
        end;
        if Assigned(vInterceptedMethod)then
          vClass.Add(vInterceptedMethod);
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

class function TMeAbstractInterceptor.AddToVirtualMethod(aClass: TClass;
        aMethodIndex: Integer;  const aMethodName: string = ''; aMethodParams:
        PTypeInfo = nil): TMeAbstractInterceptor;
var
  vInterceptedMethod: TMeInterceptedMethodItem;
  vClass: TMeInterceptedClassItem;
  vMethod: Pointer;
begin
  if aClass <> nil then
  begin
    vClass := FInterceptedClasses.GetItem(aClass);
    vMethod := GetVirtualMethod(aClass, aMethodIndex);
    if IsAbstractMethod(vMethod) then
      raise EMeInterceptorError.CreateRes(@rsInjectAbstractMethodError);
    vInterceptedMethod := CheckIntercept(vMethod, mtStatic, vClass, aMethodName);
    if vClass.FOwner = nil then
      vClass.FOwner := aClass;
    Result := GetInterceptor(Self);
    try
      //Result.Injector
      if Assigned(vInterceptedMethod) then
      //Do not patch only add the result to the chain.
      begin
        //Result.FInterceptedChain := vInterceptedMethod;
        vInterceptedMethod.Add(Result);
      end
      else //this is the first patch it;
      begin
        vInterceptedMethod := TMeInterceptedMethodItem.Create;
        try
          vInterceptedMethod.Name := aMethodName;
          ApplyMethodParams(vInterceptedMethod, aClass, aMethodParams);
          with vInterceptedMethod.Injector do
          if not InjectStaticMethod(aClass, vMethod, GetMethodProcessCenter, cX86CallDirective) then
            Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownError, [aMethodName]);
          vInterceptedMethod.Add(Result);
        except
          FreeAndNil(vInterceptedMethod);
          Raise;
        end;
        if Assigned(vInterceptedMethod)then
          vClass.Add(vInterceptedMethod);
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

function TMeAbstractInterceptor.AfterException(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const E: Exception; const Params: PMeProcParams =
        nil): Boolean;
begin
  if Assigned(FOnAfterException) then
    Result := FOnAfterException(Sender, MethodItem, E)
  else
    Result := True;
end;

procedure TMeAbstractInterceptor.AfterExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
        Params: PMeProcParams = nil);
begin
  if Assigned(FOnAfterExecute) then
   FOnAfterExecute(Sender, MethodItem, thisState);
end;

function TMeAbstractInterceptor.AllowExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil): Boolean;
begin
  Result := True;
  if Assigned(FOnAllowExecute) then
   Result := FOnAllowExecute(Sender, MethodItem);
end;

class procedure TMeAbstractInterceptor.ApplyMethodParams(aMethodItem:
        TMeInterceptedMethodItem; aClass: TClass; aMethodParams: PTypeInfo);
begin
  {$IFDEF MeRTTI_SUPPORT}
  if Assigned(aClass) and (aMethodParams = nil) then
  begin
    aMethodParams := TypeInfo(TMeObjectMethod);
  end;

  aMethodItem.MethodParams := RegisterProcTypeInfo(aMethodParams, aClass);

  {$ELSE}
  aMethodItem.MethodParams := aMethodParams;
  //aMethodItem.MethodParams := nil;
  {$ENDIF}
end;

procedure TMeAbstractInterceptor.BeforeExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
begin
  if Assigned(FOnBeforeExecute) then
   FOnBeforeExecute(Sender, MethodItem);
end;

class function TMeAbstractInterceptor.CheckIntercept(aMethod: Pointer;
        aMethodType: TMethodType; out aInterceptClass: TMeInterceptedClassItem;
        const aName: string = ''): TMeInterceptedMethodItem;
begin
  Result := nil;
  if aInterceptClass <> nil then
  begin
    Result := aInterceptClass.GetMethod(aMethod, aMethodType, aName);
    if Result <> nil then
    begin
      //vInterceptedMethod := vClass[i];
      if Result.Find(Self) >= 0 then
        Raise EMeInterceptorError.CreateResFmt(@rsAlreadyInterceptedError, [aName]);
    end;
  end
  else begin
    aInterceptClass := TMeInterceptedClassItem.Create;
    FInterceptedClasses.Add(aInterceptClass);
  end;
end;

function TMeAbstractInterceptor.iAfterException(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const E: Exception; const Params: PMeProcParams =
        nil): Boolean;
var
  i: Integer;
begin
  //if Assigned(MethodItem) then
  Result := True;
  for i := MethodItem.Count -1 downto 0 do
  begin
    if MethodItem[i].Enabled then
    begin
      if not MethodItem[i].AfterException(Sender, MethodItem, E, Params) then
        if Result = True then Result := False;
    end;
  end;
end;

procedure TMeAbstractInterceptor.iAfterExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
        Params: PMeProcParams = nil);
var
  i: Integer;
begin
  //if Assigned(MethodItem) then
  begin
    for i := MethodItem.Count -1 downto 0 do
    begin
      if MethodItem[i].Enabled then
        MethodItem[i].AfterExecute(Sender, MethodItem, thisState, Params);
    end;
  end;
end;

function TMeAbstractInterceptor.iAllowExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Assigned(MethodItem) then
  begin
    for i := MethodItem.Count -1 downto 0 do
    begin
      Result := (not MethodItem[i].Enabled) or MethodItem[i].AllowExecute(Sender, MethodItem, Params);
      if not Result then
        Exit;
    end;
  end
end;

procedure TMeAbstractInterceptor.iBeforeExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
var
  i: Integer;
begin
  //if Assigned(MethodItem) then
  begin
    for i := MethodItem.Count -1 downto 0 do
    begin
      MethodItem[i].BeforeExecute(Sender, MethodItem, Params);
    end;
  end;
end;

class function TMeAbstractInterceptor.RemoveFrom(aProc: Pointer): Boolean;
begin
  Result := RemoveFrom(TClass(nil), aProc);
end;

class function TMeAbstractInterceptor.RemoveFrom(aClass: TClass; aMethod:
        Pointer): Boolean;
var
  vInterceptedMethod: TMeInterceptedMethodItem;
  vClass: TMeInterceptedClassItem;
begin
  Result := False;
  vClass := FInterceptedClasses.GetItem(aClass);
  if Assigned(vClass) then
  begin
    vInterceptedMethod := vClass.GetMethodByAddr(aMethod);
    if vInterceptedMethod <> nil then
    begin
      Result := vInterceptedMethod.RemoveInterceptorClass(Self) >= 0;
      if vInterceptedMethod.Count = 0 then
      begin
        vInterceptedMethod.Injector.Enabled := False;
        Result := vClass.Remove(vInterceptedMethod) >= 0;
      end;
    end;
  end;
end;

class function TMeAbstractInterceptor.RemoveFrom(aClass: TClass; aMethodName:
        String;  aMethodParams: PTypeData = nil): Boolean;
var
  vInterceptedMethod: TMeInterceptedMethodItem;
  vClass: TMeInterceptedClassItem;
  vMethod: PPublishedMethodEntry;
begin
  Result := False;
  if aClass <> nil then
  begin
    vMethod := FindPublishedMethodEntryByName(aClass, aMethodName);
    if vMethod = nil then
      Raise EMeInterceptorError.CreateResFmt(@rsInterceptUnknownMethodError, [aMethodName]);
    vClass := FInterceptedClasses.GetItem(aClass);
    if Assigned(vClass) then
    begin
      vInterceptedMethod := vClass.GetMethod(vMethod.Address, mtStatic, aMethodName);
      if vInterceptedMethod <> nil then
      begin
        vInterceptedMethod.Injector.Enabled := False;
        Result := vClass.Remove(vInterceptedMethod) >= 0;
      end;
    end;
  end;
end;

{$IFDEF MeRTTI_SUPPORT}
{
************************* TMeCustomInterceptor_NoParam *************************
}
function TMeCustomInterceptor_NoParam.Execute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem): Pointer;

  {$IFNDEF X_PUREPASCAL}
  asm
  {$ELSE}
  var
    vProc: PProcedure;
    {$MESSAGE  ERROR 'This place ERROR: Nerver used!!' }

begin
  vProc := MethodItem.Injector.MethodOriginalActualLocation;
  if IsRedirectCodeNoop(TInjectorAccess(MethodItem.FInjector).FMethodOriginalBackup) then
    vProc := @TInjectorAccess(MethodItem.FInjector).FMethodOriginalBackup;
  asm
  {$ENDIF}

  {      MOV   EDI, MethodItem.FInjector.TInjectorAccess.FMethodOriginalActualLocation
        CMP   MethodItem.FInjector.TInjectorAccess.FMethodOriginalBackup.Jump, cX86NoOpDirective
        JNZ   @CallFunc
        CMP   dword ptr MethodItem.FInjector.TInjectorAccess.FMethodOriginalBackup.Offset, cX86NoOpDirective4Bytes
        JNZ   @CallFunc
  }
  @CallFunc:
  {$IFNDEF X_PUREPASCAL}
    {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
      {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
        PUSH EAX
        PUSH ECX
        LEA  EAX, MethodItem.FInjector.TInjectorAccess.FMethodOriginalBackup
        //MOV  EAX, offset [EAX].TInjectorAccess.FMethodOriginalBackup
        //MOV  EAX, offset [EAX].TInjectorAccess.FMethodOriginalBackup
        //+TInjectorAccess.FMethodOriginalBackup
        //MOV  EAX, MethodItem.FInjector.TInjectorAccess.FMethodOriginalBackup
        CALL IsRedirectCodeNoop
        CMP  EAX, 0
        POP  ECX
        POP  EAX
        JNZ  @CallFuncOld  //jump if Is ReirectCode Noop

      {$ENDIF}
  @DoCallScramblingCode:
        LEA  MethodItem, MethodItem.FInjector.TInjectorAccess.FMethodOriginalBackup
        CALL MethodItem
        RET
        //CALL MethodItem+TInjectorAccess.FMethodOriginalBackup

    {$ENDIF}
  @CallFuncOld:
        CALL  MethodItem.FInjector.TInjectorAccess.FMethodOriginalActualLocation
  {$ELSE}
        CALL  vProc
  end;
  {$ENDIF}
end;

class function TMeCustomInterceptor_NoParam.GetMethodProcessCenter: Pointer;
begin
  //Result := GetVirtualMethod(Self, vmtiExecute);
  Result := @StaticMethodProcessCenter;
end;

procedure TMeCustomInterceptor_NoParam.iExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem);
var
  vExecuteStates: TMeExecuteStates;
begin
  vExecuteStates := [];

  try
   try
    if iAllowExecute(Sender, MethodItem) then
    begin
      Include(vExecuteStates, esAllowed);
      iBeforeExecute(Sender, MethodItem);
      Include(vExecuteStates, esBefore);
      Execute(Sender, MethodItem);
      Include(vExecuteStates, esAfter);
    end;
   except
    On E: Exception do
    begin
      Include(vExecuteStates, esException);
      if iAfterException(Sender, MethodItem, E) then
        raise;
    end;
   end;
  finally
    iAfterExecute(Sender, MethodItem, vExecuteStates);
    //MethodItem.Injector.Enabled := True;
  end;
end;

{
***************************** TMeCustomInterceptor *****************************
}
class function TMeCustomInterceptor.GetMethodProcessCenter: Pointer;
begin
  Result := @StaticMethodProcessCenterWithParam;
end;

procedure TMeCustomInterceptor.iExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams);
var
  vExecuteStates: TMeExecuteStates;
begin
  if not FEnabled then
  begin
      if Assigned(Params) then
      begin
        {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
          if not IsRedirectCodeNoop(MethodItem.Injector.MethodOriginalBackup) then
            Params.Execute(@TInjectorAccess(MethodItem.Injector).FMethodOriginalBackup)
          else
        {$ENDIF}
            Params.Execute(MethodItem.Injector.MethodOriginalActualLocation);
      end
      else begin
        {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
          if not IsRedirectCodeNoop(MethodItem.Injector.MethodOriginalBackup) then
            TProcedure(@TInjectorAccess(MethodItem.Injector).FMethodOriginalBackup)
          else
        {$ENDIF}
            TProcedure(MethodItem.Injector.MethodOriginalActualLocation);
      end;
    Exit;
  end;

  vExecuteStates := [];

  try
   try
    if iAllowExecute(Sender, MethodItem, Params) then
    begin
      Include(vExecuteStates, esAllowed);
      iBeforeExecute(Sender, MethodItem, Params);
      Include(vExecuteStates, esBefore);

      if Assigned(Params) then
      begin
        {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
          if not IsRedirectCodeNoop(MethodItem.Injector.MethodOriginalBackup) then
            Params.Execute(@TInjectorAccess(MethodItem.Injector).FMethodOriginalBackup)
          else
        {$ENDIF}
            Params.Execute(MethodItem.Injector.MethodOriginalActualLocation);
      end
      else begin
        {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
          if not IsRedirectCodeNoop(MethodItem.Injector.MethodOriginalBackup) then
            TProcedure(@TInjectorAccess(MethodItem.Injector).FMethodOriginalBackup)
          else
        {$ENDIF}
            TProcedure(MethodItem.Injector.MethodOriginalActualLocation);
      end;
      Include(vExecuteStates, esAfter);
    end;
   except
    On E: Exception do
    begin
      Include(vExecuteStates, esException);
      if iAfterException(Sender, MethodItem, E, Params) then
        raise;
    end;
   end;
  finally
    iAfterExecute(Sender, MethodItem, vExecuteStates, Params);
    //MethodItem.Injector.Enabled := True;
  end;
end;

{$ENDIF}

procedure FreeInterceptors;
begin
  while FInterceptors.Count > 0 do
  begin
    FInterceptors.Last.Free;
  end; 
end;

initialization
  FInterceptedClasses := TMeInterceptedClassList.Create;
  FInterceptors := TObjectList.Create;
finalization
  FreeInterceptors;
  FreeAndNil(FInterceptedClasses);
  FreeAndNil(FInterceptors);
end.
