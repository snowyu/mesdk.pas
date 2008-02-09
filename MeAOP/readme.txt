= MeAOP SDK - Delphi AOP(Aspect Oriented Programming) Ver 0.8.0.0 alpha =

HomePage: http://dev.cqhot.cn/mesdk/
Email: riceballl@hotmail.com
Author: Riceball LEE
Codebase: http://code.google.com/p/meaop/

License:
The contents of MeAOP are released under the Mozilla Public License
1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)

== Introduce AOP ==
Aspect Oriented Programming, IMO, the aspect is the general feature in fact. It's a helper for object-oriented programming to
re-use the feature(function), such as the Login feature. Aspect Oriented Programming attempts to aid programmers in the 
separation of features, or the breaking down of a program into distinct parts that overlap in functionality as little as possible. 

  1. distill/separate the general features from many classes;
  2. many classes can share the feature, so it need not to modify many classes if the feature've been changed.
  3. no necessary to modify the method when adding a new feature to the method.

I treat the Aspect Oriented Programming as Feature Oriented Programming.

IMO, the AOP core implementation is inject(patch) and intercept the method of class.


=== Example ===

Let's introduce AOP with the help of an example. Imagine an application that works concurrently on shared data. The shared data may be encapsulated in a Data object (an instance of class Data). In this application, there are multiple objects of different classes working on a single Data object, whereas only one of these objects may have access to the shared data at a time. To achieve the desired behaviour, some kind of locking must be introduced. That means, that whenever one of these objects wants to access the data, the Data object must be locked (and unlocked after the object has finished using it). The traditional approach is to introduce an (abstract) base class, from which all "visiter" classes inherit. This class defines a method Lock() and a method Unlock() which must be called before and after the actual work is done (semaphores, basically). 

<code>
  TCustomVisiter = Class
  protected
    procedure Locked();virtual;abstract;
    procedure Unlocked();virtual;abstract;
  published
    procedure AccessDataObject();virtual;
  end;
}
</code>


This approach has the following drawbacks:

    * Every method that works on the data has to take care of locking. The code is cluttered with statements related to locking.
    * In a single inheritance world, it is not always possible to let all visiter classes inherit from a common base class, 
      because the one and only inheritance link may already be consumed by another concept. This is especially true if the locking 
      features must be introduced into a class hierarchy after the hierarchy has been designed, possibly by another programmer 
      (e.g. the developer of a class library).
    * Reusability is compromised: The visiter classes may be reused in another context where they don't need locking 
      (or where they have to use another locking scheme). By putting the locking code into the visiter classes, the classes 
      are tied to the locking approach used in this specific application. 

The feature of locking in our example application can be described with the following properties:

    * It is not the primary job of the visiter classes
    * The locking featrure is independent of the visiter's primary job
    * locking featrure cross-cuts the system, i.e. many classes, and probably many methods of these classes, are affected by locking. 

So, a new program construct should be defined that takes care of cross-cutting features of a system, eg, the locking feature of this application.


In our example application, the aspect Lock would have the following responsibilities:

    * provide the necessary features to lock and unlock objects to the classes that have to be 
      locked/unlocked (in our example add lock() and unlock() to the Data class)
    * ensure that all methods that modify the Data object call lock() before their work and unlock() 
      when they have finished (in our example the visiter classes). 

the locking feature looks like this:

<code>
  TLockingFeature = Class(TMeCustomFeature)
  protected
    procedure Locked();virtual;
    procedure Unlocked();virtual;
    function Islocked(): Boolean;virtual;
  protected
    function AllowExecute(
       Sender: TObject; 
       MethodItem: TMeInterceptedMethodItem; 
       const thisState: TMeExecuteStates; 
       const Params: TMeProcParams = nil
    ): Boolean;override;
    procedure AfterExecute(
       Sender: TObject; 
       MethodItem: TMeInterceptedMethodItem; 
       const thisState: TMeExecuteStates; 
       const Params: TMeProcParams = nil
    );override;
  end;

function TLockingFeature.AllowExecute(
       Sender: TObject; 
       MethodItem: TMeInterceptedMethodItem; 
       const thisState: TMeExecuteStates; 
       const Params: TMeProcParams = nil
): Boolean;
begin
  Result := not IsLocked; //only not isLocked can be allow execute.
  if Result then Locked;
end;

procedure TLockingFeature.AfterExecute(
       Sender: TObject; 
       MethodItem: TMeInterceptedMethodItem; 
       const thisState: TMeExecuteStates; 
       const Params: TMeProcParams = nil
);
begin
  Unlocked;
end;
</code>

Now, you need to add the locking feature to the methods of classes.  that' all.
<code>
  //add the feature to the published method: TCustomVisiter.AccessDataObject.
  TLockingFeature.AddTo(TCustomVisiter, 'AccessDataObject');
</code>

Note: the locking feature only affects the current injected method of the class, not affects the derived class if the derived class override the method.

There are two solvers if you wanna add the feature to the derived class:

solver 1: the derived class have to call the inherited method the parent class
<code>
  TMyVisiter = Class
  protected
    procedure AccessDataObject();override;
  end;

procedure TMyVisiter.AccessDataObject();
begin
  inherited AccessDataObject();
  .....
end;

</code>

Solver 2: add the feature to derived class if need.
<code>
  TLockingFeature.AddTo(TMyVisiter, @TMyVisiter.AccessDataObject, 'AccessDataObject');
  TLockingFeature.AddTo(TOtherVisiter, @TOtherVisiter.AccessDataObject, 'AccessDataObject');
</code>

===  The AOP Purposes ===
the general features for AOP:

|| * Authentication   || 
|| * Caching          || 
|| * Context passing  || 
|| * Error handling   || 
|| * Lazy loading     || 
|| * Debugging        || 
|| * logging, tracing || 
|| * profiling        || 
|| * monitoring       || 
|| * Performance optimization || 
|| * Persistence              ||
|| * Resource pooling         ||
|| * Synchronization          ||
|| * Transactions             ||
|| * Remote Method Calling    ||

== MeAOP: My AOP For Delphi Frame ==
the MeAOP For Delphi Frame is base on the code injection. heavy hack to delphi, patch the binary codes to implement the interception of the method. 
I do not like the implementation of the dynamic proxy. It's performance is very low, inject the code directly is the fastest and the best solver.
The MeAOP is the simple, effective and easy to use enough.

You do not care the bewildering concepts: joint, advise, cross-cut etc in the MeAOP. you just need to know the concept: feature.
the core of MeAOP distill the custom feature class, and your work is design your feature class and add it to the specified method of class.
You just call the AddTo class function of the TMeCustomFeature to add your feature to the specified method of class.

=== Design your feature ===

=== Add feature to method ===

<code>
//add a method with no parameters:
TMyFeature.AddTo(aClass, @aClass.aMethod, 'aMethod').
//add a published method with no parameters:
TMyFeature.AddTo(aClass, 'theMethodName').
</code>

BTW, you can add your feature to a procedure.
<code>
TMyFeature.AddTo(@aProcedure, 'aProcedure').
</code>


how can I add my feature to a procedure or method with parameters?
that's easy:
<code>
type
  //defined your procedure type here, must use "of object" to get the TypeInfo!!
  TMyProc = function (a,b:integer): integer; stdcall of object;

function MyProc(a,b:integer): integer; stdcall;
begin
  Result := a + b;
  ShowMessage('Running MyProc...');
end;

procedure TForm1.Button1Click(Sender: TObject);  
begin
  ShowMessage('Running Button1Click...');
end;

Initialization
  TMyFeature.AddTo(@MyProc, 'MyProc', TypeInfo(TMyProc));
  TMyFeature.AddTo(TForm1, 'Button1Click', TypeInfo(TNotifyEvent));
end.
</code>


then how to design your feature class? very simple.
just override the BeforeExecute, AfterExecute etc method in your TMyFeature class. That's all.
of cause your all features should be derived from the TMeCustomFeature.


<code>
  TMyFeature = class(TMeCustomFeature)
  protected
    {: run the methodItem only return True }
    { If you do not override it always return true.}
    function AllowExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem;
            const Params: PMeProcParams = nil): Boolean; override;
    {: trigger on before the MethodItem is executed and after AllowExecute return true.}
    procedure BeforeExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
            override;
    {: trigger on after the MethodItem is executed even though the MethodItem's raised exception . }
    procedure AfterExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
            Params: PMeProcParams = nil); override;
    {: trigger on after the MethodItem's raised exception . }
    procedure AfterException(Sender: TObject; MethodItem: TMeInterceptedMethodItem; E: Exception; const Params: PMeProcParams = nil);override;
  end;

function TMeLoginFeature.AllowExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil): Boolean;
var
  vLoginState: TLoginState;
begin
  vLoginState := GLoginManager.Login;
  Result := vLoginState = lsLogined;
  if not Result then
    raise Exception.Create('your password wrong can not login!!'#10'the user and password are admin');
end;

procedure TMeLogFeature.AfterExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
        Params: PMeProcParams = nil);
var
  vLevel: TLogLevel;
  s: string;
  I: Integer;
begin
  if Assigned(FLogBase) then
  begin
    //s := '';
    vLevel := vlDebug;
    if esException in thisState then
    begin
    //whether the Exception occur.
      vLevel := vlErrors;
      s := 'Error! Exception occur:';
    end
    else if not (esAllowed in thisState) then
    begin
    //this function can not be allowed to execute.
      vLevel := vlAudit;
      s := 'Login Failed:';
    end;
    if Assigned(Sender) then //whether is method?
      s := s + Sender.ClassName + '.';
    s := s + MethodItem.name + ':';
    S := s + uMeTypInfo.SetToString(TypeInfo(TMeExecuteState), Byte(thisState));
    FLogBase.Log(vLevel, s);

    if Assigned(Params) then //function with parameters?
    begin
      For I := 0 to Params.Count - 1 do
      begin
        if Params.Items[I].IsByRef then
        begin
          s:= 'ByRef '; 
        end
        else s := '';
        s := s + Format('Param[%s]=$%x', [Params.Items[I].Name, Params.Items[I].AsInteger]);
        FLogBase.Log(vlDebug, s);

      end;
      if Assigned(Params.SelfParam) and (Sender <> Params.SelfParam.AsPointer) then
      begin
        s := 'Sender <> Params.SelfParam';
        FLogBase.Log(vlErrors, s);
      end;
      if (esAfter in thisState) and Assigned(Params.ResultParam) and (Params.ResultParam.DataType.Kind = mtkInt64) then
      begin
        //show the original result value
        s :=  Format('Old Result=$%x', [Params.ResultParam.AsInt64]);
        FLogBase.Log(vlDebug, s);
        //now change the result value:
        Params.ResultParam.AsInt64 := $3344556600;
        s :=  Format('New Result=$%x', [Params.ResultParam.AsInt64]);
        FLogBase.Log(vlDebug, s);
      end;
    end;
  end;
end;

procedure TMeLogFeature.BeforeExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
var
  I: Integer;
begin
  if Assigned(Params) then
  begin
    For I := 0 to Params.Count - 1 do
    begin
      if Params.Items[I].IsByRef then
      begin
        //modify all the integer parameters before the function is executed.
        if Params.Items[I].DataType.Kind = mtkInteger then
          Params.Items[I].AsInteger := Params.Items[I].AsInteger + 1;
      end
    end
  end
end;
</code>

=== MeAOP Goal ===
 1. Simple and easy to use: No necessary to write a lot of codes(maybe you need not write at all) to port the old class to MeAOP,
 2. Fastest: the speed is the fastest via opcode inject directly.
 3. Small: occupied memory is very small.

=== MeAOP Structure ===
Three Levels:
 1.the lowest level core : the TMeInjector object for the method and procedure
   provide the simplest and the lightest injector(one injector take 36 bytes memory only)
 2.the middle level core: the TMeInterceptor class for the method and procedure
    2.1. TMeCustomInterceptor : supports the method or procedure with no paramaters
    2.2. TMeInterceptor: supports the method or procedure with paramaters
 3.the top core: the TMeCustomFeature 
   provide the feature oriented programming class library. You can easy add the new feature class to the specified method of the class .


==== Low level Core: MeInjector ====
provide the simplest and the lightest injector(one injector only take 36 bytes memory)

===== function library =====
include the Low level function library and injector.

===== TMeInjector =====
One TMeInjector maintain one Method(Procedure) Only.
  The TMeInjector is the smallest, simplest and fastest object in my AOP.
  One TMeInjector only occupy 36 bytes about in the memory.

Inject the Method(Procedure) through the TMeInjector.InjectXXX methods.
And the original method(Procedure) entry is stored into the TMeInjector.OriginalProc property. 
Uninject the Method(Procedure) just set the Enabled property to false(MUST NO other injector injects into it.).

==== Middle level Core: MeInterceptor ====

==== Top level Core: MeCustomFeature ====

provide the feature oriented programming core class. You can easy add the new feature class to the specified method of the class .

the core of MeAOP distill the custom feature class, and your work is design your feature class and add it to the specified method of class.
You just call the AddTo class function of the TMeCustomFeature to add your feature to the specified method of class. eg,
<code>
TMyFeature.AddTo(aClass, @aClass.aMethod, 'aMethod').
</code>

BTW, you can add your feature to a procedure.
<code>
TMyFeature.AddTo(@aProcedure, 'aProcedure').
</code>

<code>
  TMeCustomFeature = class
  protected
    {: run the methodItem only return True }
    { If you do not override it always return true.}
    function AllowExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem;
            const Params: TMeProcParams = nil): Boolean; override;
    {: trigger on before the MethodItem is executed and after AllowExecute return true.}
    procedure BeforeExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const Params: TMeProcParams = nil);
            override;
    {: trigger on after the MethodItem is executed even though the MethodItem's raised exception . }
    procedure AfterExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
            Params: TMeProcParams = nil); override;
    {: trigger on after the MethodItem's raised exception . }
    procedure AfterException(Sender: TObject; MethodItem: TMeInterceptedMethodItem; E: Exception; const Params: TMeProcParams = nil);override;
  public
    {: Add the feature to a procedure. }
    class function AddTo(aProc:Pointer; const aProcName: string): TMeCustomInterceptor; overload;
    {: Add the feature to a method. }
    class function AddTo(aClass: TClass; aMethod: Pointer;  const aMethodName: string): TMeCustomInterceptor; overload;
    {: Add the feature to a published method. }
    class function AddTo(aClass: TClass; aMethodName: String): TMeCustomInterceptor; overload;
    {: remove the feature from the method or procedure. }
    class function RemoveFrom(aClass: TClass; aMethod: Pointer): Boolean;overload;
    {: remove the feature from the published method. }
    class function RemoveFrom(aClass: TClass; aMethodName: String): Boolean; overload;
  end;

  TMeInterceptedMethodItem = class
  public
    { the method or procedure address }
    property Address: Pointer read GetAddress;
    { Summary Method or procedure name }
    property Name: string read FName;
  end;

  //the parameters of the method/procedure
  TMeProcParams = Class
    Property Items[Index: Integer]: TMeParam read Get;
    //the Self Param if it's method;
    Property SelfParam: TMeParam read FSelfParam;
    //the result Param if it's function;
    Property ResultParam: TMeParam read FResultParam;
  end;
  TMeParam = Class
    //Represents the value of the parameter as a Variant.
    Property Value: Variant read GetAsVariant write SetAsVariant;
    Property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    Property AsInteger: Integer read GetAsInteger write SetAsInteger;
    Property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    //Property AsXXX: AsString, AsInteger, AsBoolean, AsDateTime....
    //Indicates the type of parameter.
    Property DataType: TMeParamType read FDataType;
    {: the Param Name }
    //property Name: ShortString read GetName;
  end;
  TMeParamType = Class
  public
    {: the param is by reference. means pointer.}
    function IsByRef: Boolean;
  public
    {: the param type info object.}
    property ParamType: PMeType read FParamType;
    property Flags: TParamFlags read FFlags;
    property ParamTypeName: ShortString read GetParamTypeName;
    property Kind: TMeTypeKind read GetKind;
    {: the parameter name.}
    //property Name: ShortString read GetName;
  end;

</code>

=== MeAOP History ===

Ver 1.0.0.0
  * + Add feature to Property
  * * [Bug] the other all registered features are lost when remove some feature from a method.
  * * Minor changed.

Ver 0.8.0.0
  * * [Bug] Memory Leak
  * * Minor changed.
Ver 0.8.0.0 alpha
  * + Support to intercept the common method or function with paramaters.
  * + Support the register, pascal, stdcall(safecall), cdecl, fastcall(the delphi compiler do not support) calling conventions.
  * + Support to get/modify the paramaters' value of the intercepted functions.
  * + Support to get/modify the result's value of the intercepted functions.
  * + Support the user defined type of the paramater.
  
TODO: 
   * + full test.
   * + support the constructor method yet.
   * + support the record, static array, dynamic array etc paramaters

Ver 0.5.4.0
  * + the trampoline mechanism make the procedure and static-method to support the reentrant for multi-thread. No need to make pre-hole. 
      This will increase the Injector's size to 64 bytes.
      Switcher: {$DEFINE STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
      Other purposes: hiding/scrambling code
  * * [Bug] minor bug fixed.

Ver 0.5.3.0 [Released]
  * + the pre-hole mechanism make the intercepted procedure and static-method to support the reentrant for multi-thread.
    {$DEFINE STATIC_METHOD_THREADSAFE_SUPPORT}
    you must add this "{$I uMeMakeHole.inc}" to the beginning of the procedure for support reentrant:
    <code>
    procedure TestProc;
    begin
      {$I uMeMakeHole.inc}
      ...
      ...
    end;
    </code>
    Note: you MUST define the {$DEFINE STATIC_METHOD_THREADSAFE_SUPPORT} in your unit file first or the uMeMakeHole.inc will include nothing.

Ver 0.5.2.0
  * + the return value type of the intercepted the function can be any type now!
  * * [Bug] minor bug fixed.

Ver 0.5.1.0
  * + Support to intercept the function with no parameters.
  * + Support to intercept the procedure/method in the DLL or BPL.
  * * [Bug] minor bug fixed.

Note[0.5.1.0]:
  * !* Only Support the register calling convention function.
  * !* the return value type of the intercepted the function must be a simple type:
    Ordinal type, Real type, Pointer type(class, class-reference, and procedure-pointer), 
    little static-array, record, and set type(it must be less than 4bytes)

Ver 0.5.0.0 [Released]
  * + First Released.
  * + intercept the procedure and method with no parameters.

Note[0.5.0.0]:
  * !* DO NOT Support the Multi-thread with the intercepted procedure and method 
  * !* DO NOT Support to intercept the functions!
  * !* DO NOT Support to intercept the Constructors and destructors
  * !* DO NOT Support to intercept the procedure/method in the DLL or BPL.


=== MeAOP Download ===
Current Version: 0.8.0.0

http://www.mesdk.com/download.htm

