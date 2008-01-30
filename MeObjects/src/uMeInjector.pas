{ Summary the method(procedure) Code Injector }
{ Description
  Provide the lightest and simplest injector object -- TMeInjector.
  This object do not use any virtual method, so you can use it directly.
  Each injector only take 36 bytes about in the memory. One injector 
  maintains the one injected method(procedure) only. Call the InjectXXX 
  Method to inject. The injector object is the smallest, simplest and 
  fastest object in the MeAOP .

  
  CN:
  提供最简单最轻巧的注入器对象(Object)，该Object没有任何虚方法动态方法，
  因此你可以直接使用它。每一个注入器大约仅占内存36个字节（指字段域）。
  一个注入器(TMeInjector)维护一个过程或方法，使用注入器的 Inject
  系列方法进行注入，
  注入后Enbaled属性为真，撤销注入只要设置Enabled属性为假即可，重新注入只要重新设置
  Enabled属性为真。注意，注入后，如果想改为注入其他方法，首先必须要设置Enabled属性
  为假。如果在注入后，又有其他注入器再次注入，那么只有当那个注入器首先撤销注入后，
  你才能撤销注入！！
}
unit uMeInjector;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, TypInfo
  , uMeConsts
  , uMeSystem
  , uMeTypInfo
  ;

const
  //for static method or procedure
  DefaultInjectDirective = cX86JumpDirective;

type
  EMeInjectorError = Class(EMeError);
  PMeInjector = ^TMeInjector; 
  TMeInjector = object
  protected
    //必须要，因为要恢复
    FEnabled: Boolean;

    FMethodType: TMethodType;

    FMethodNewLocation: Pointer;
    // keep the Original method(procedure) proc address.
    FMethodOriginalLocation: Pointer; 

    //only for static method or procedure.
    // the actual original static method(procedure) location
    // if the procedure is in BPL(DLL) then the OriginalLocation <> OriginalActualLocation
    // else the OriginalLocation = OriginalActualLocation 
    FMethodOriginalActualLocation: Pointer; 
    FMethodOriginalBackup: TRedirectCodeRec; 

    //if this is a procedure then it will be nill.
    FMethodClass: TClass;
    // for virtual method(Index) or dynamic method(Slot) 
    // or published method(PPublishedMethodEntry)
    // or static method(procedure) it is the drecitve: JMP or CALL
    FMethodIndex: Integer; 

    procedure SetEnabled(Value: Boolean);

    function _InjectPublishedMethod: Boolean;
    function _InjectStaticMethod: Boolean;
    function _InjectVirtualMethod: Boolean;
    function _InjectDynamicMethod: Boolean;
    function _UnInjectStaticMethod: Boolean;
    function _UnInjectVirtualMethod: Boolean;
    function _UnInjectDynamicMethod: Boolean;
    function _UnInjectPublishedMethod: Boolean;

    function Inject:Boolean;overload;
    function UnInject:Boolean;
  public
    {: return the original procedure address for the procedure or static-method}
    {
     only available for STATIC_METHOD_SCRAMBLING_CODE_SUPPORT or pre-holed procedure
     other return nil. 
    }
    function OriginalProc: Pointer;
    {###only when the enabled is false can inject!!}
    { Summay: Inject the procedure }
    { Description
      @param aDirective the inject directive: near CALL or JMP.
                        the default is the near JMP directive used.
    }
    function InjectProcedure(aPatchLocation: Pointer; aNewLocation: Pointer; const aDirective: Integer = DefaultInjectDirective): Boolean;
    { Summay: Inject the static method }
    function InjectStaticMethod(aClass: TClass; aPatchLocation: Pointer; aNewLocation: Pointer; const aDirective: Integer = DefaultInjectDirective): Boolean;
    { Summary Inject the aNewLocation to the VMT.}
    {NOTE: if you insert a new virtual method the index number may be changed.}
    function InjectVirtualMethod(aClass: TClass; aIndex: Integer; aNewLocation: Pointer): Boolean;
    { Summary Inject the aNewLocation to the DMT.}
    function InjectDynamicMethod(aClass: TClass; aSlot: Integer; aNewLocation: Pointer): Boolean;
    { Summary Inject the aNewLocation to the PMT.}
    function InjectPublishedMethod(aClass: TClass; aEntry: PPublishedMethodEntry; aNewLocation: Pointer): Boolean;
    //the gernal inject method
    {
    Inject the procedure : Inject(@aProc, @MyNewProc);
    Inject the method : Inject(@TAClass.Method, @MyNewMethod, TAClass [, mtVirtual]);
      Note:
        * the last parameter is optional, it should be mtStatic, mtVirtual and mtDynamic
        * the TAClass.Method can not be the abstract method unless it is pulbished.
            you should call the InjectVirtualMethod or InjectDynamicMethod directly 
            if you wanna inject an abstract method 
    } 
    function Inject(aPatchLocation: Pointer; aNewLocation: Pointer
      ; aClass: TClass = nil
      ; aMethodType: TMethodType=mtUnknown): Boolean;overload;
    { Summay Inject the Published method of the class by InjectStaticMethod!} 
    { Note: the published method can be the abstract method!
       Inject('MyMethodName', @MyNewMethodProc, TMyClass);
      See Also InjectStaticMethod
      Why do I inject it as StaticMethod?
        I wanna the return address via CALL. 
    }
    function Inject(const aMethodName: string; aNewLocation: Pointer
      ; aClass: TClass
      ; aMethodType: TMethodType=mtPublished
      ; const aDirective: Integer = DefaultInjectDirective): Boolean;overload;
    //set enabled to false will restore the Original method(procedure)
    //set enabled to true will patch the NewLocation again if NewLocation <> nil.
    property Enabled: Boolean read FEnabled write SetEnabled;

    property MethodType: TMethodType read FMethodType;  
    property MethodNewLocation: Pointer read FMethodNewLocation;
    property MethodOriginalLocation: Pointer read FMethodOriginalLocation; 

    property MethodClass: TClass read FMethodClass;  
    property MethodIndex: Integer read FMethodIndex;  


    //only for static method or procedure.
    property MethodOriginalActualLocation: Pointer read FMethodOriginalActualLocation; 
    property MethodOriginalBackup: TRedirectCodeRec read FMethodOriginalBackup; 
  end;

  



implementation

{##### TMeInjector ######}
procedure TMeInjector.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then
  begin
    if (FMethodType <> mtUnknown) and (FMethodOriginalLocation <> nil) then
    begin 
      if Value then
      begin
        Value := Inject;
      end else
      begin
        if not UnInject() then
          Value := True;
          //Raise EMeInjectorError.CreateRes(@rsCanNotUnInjectError); 
      end;
    end
    else 
      Value := False;
    //if Value = False then 
    FEnabled := Value;
  end;
end;

function TMeInjector.InjectProcedure(aPatchLocation: Pointer; aNewLocation: Pointer; const aDirective: Integer): Boolean;
begin
  Result := not Enabled; 
  if Result then
  begin
    FMethodOriginalActualLocation := GetActualAddress(aPatchLocation);
    FMethodNewLocation := aNewLocation;
    FMethodIndex := aDirective;
    Result := _InjectStaticMethod();
    if Result then
    begin
      FEnabled := True;
      FMethodType := mtProcedure;
      FMethodOriginalLocation := aPatchLocation;
      FMethodClass := nil; 
    end; 
  end; 
end;

function TMeInjector.InjectStaticMethod(aClass: TClass; aPatchLocation: Pointer
  ; aNewLocation: Pointer; const aDirective: Integer): Boolean;
begin
  Result := aClass <> nil;
  if Result then
  begin
    Result := InjectProcedure(aPatchLocation, aNewLocation, aDirective);
    if Result then
    begin
      FMethodClass := aClass; 
      FMethodType := mtStatic;
    end;
  end;
end;

function TMeInjector.InjectPublishedMethod(aClass: TClass; aEntry: PPublishedMethodEntry; aNewLocation: Pointer): Boolean;
begin
  Result := not Enabled and (aClass <> nil);
  if Result then
  begin
    FMethodClass := aClass; 
    FMethodIndex := Integer(aEntry); 
    FMethodNewLocation := aNewLocation;
    Result := _InjectPublishedMethod();
    if Result then
    begin
      FEnabled := True;
      FMethodType := mtPublished;
    end;
  end;
end;

function TMeInjector.InjectVirtualMethod(aClass: TClass; aIndex: Integer; aNewLocation: Pointer): Boolean;
begin
  Result := not Enabled and (aClass <> nil);
  if Result then
  begin
    FMethodClass := aClass; 
    FMethodIndex := aIndex; 
    FMethodNewLocation := aNewLocation;
    Result := _InjectVirtualMethod();
    if Result then
    begin
      FEnabled := True;
      FMethodType := mtVirtual;
    end;
  end;
end;

function TMeInjector.InjectDynamicMethod(aClass: TClass; aSlot: Integer; aNewLocation: Pointer): Boolean;
begin
  Result := not Enabled and (aClass <> nil);
  if Result then
  begin
    FMethodClass := aClass; 
    FMethodIndex := aSlot; 
    FMethodNewLocation := aNewLocation;
    Result := _InjectDynamicMethod();
    if Result then
    begin
      FEnabled := True;
      FMethodType := mtDynamic;
    end;
  end;
end;

function TMeInjector.Inject(aPatchLocation: Pointer; aNewLocation: Pointer
  ; aClass: TClass = nil; aMethodType: TMethodType=mtUnknown): Boolean;
begin
  Result := not Enabled and (aPatchLocation <> nil);
  if Result = False then Exit;
  if aClass = nil then
  begin
    Result := InjectProcedure(aPatchLocation, aNewLocation); 
  end
  else begin
    //if IsAbstractMethod(aPatchLocation) then
      //raise EMeInjectorError.CreateRes(@rsInjectAbstractMethodError);
    case aMethodType of
      mtPublished:
      begin
        FMethodIndex := Integer(FindPublishedMethodEntryByAddr(aClass, aPatchLocation));
        Result := FMethodIndex <> 0;
        if Result then
          Result := InjectPublishedMethod(aClass, PPublishedMethodEntry(FMethodIndex), aNewLocation);
      end;
      mtVirtual: 
      begin
        FMethodIndex := FindVirtualMethodIndex(aClass, aPatchLocation);
        Result := FMethodIndex >= 0;
        if Result then
          Result := InjectVirtualMethod(aClass, FMethodIndex, aNewLocation);
      end;
      mtDynamic:
      begin
        FMethodIndex := FindDynamicMethodIndex(aClass, aPatchLocation);
        Result := FMethodIndex >= 0;
        if Result then
          Result := InjectDynamicMethod(aClass, FMethodIndex, aNewLocation);
      end;
      mtStatic:
      begin
        Result := InjectStaticMethod(aClass, aPatchLocation, aNewLocation);
      end;
      mtUnknown:
      begin
        FMethodIndex := Integer(FindPublishedMethodEntryByAddr(aClass, aPatchLocation));
        Result := FMethodIndex <> 0;
        if Result then
          Result := InjectPublishedMethod(aClass, PPublishedMethodEntry(FMethodIndex), aNewLocation);

        FMethodIndex := FindVirtualMethodIndex(aClass, aPatchLocation);
        Result := FMethodIndex >= 0;
        if Result then
        begin
          Result := InjectVirtualMethod(aClass, FMethodIndex, aNewLocation);
          Exit;
        end;
        FMethodIndex := FindDynamicMethodIndex(aClass, aPatchLocation);
        Result := FMethodIndex >= 0;
        if Result then
        begin
          Result := InjectDynamicMethod(aClass, FMethodIndex, aNewLocation);
          Exit;
        end;
        Result := InjectStaticMethod(aClass, aPatchLocation, aNewLocation);
      end;
    end;
  end;
end;

function TMeInjector.Inject(const aMethodName: string; aNewLocation: Pointer
  ; aClass: TClass; aMethodType: TMethodType
  ; const aDirective: Integer): Boolean;
begin
  Result := aClass <> nil;
  if Result then
  begin
    FMethodIndex := Integer(FindPublishedMethodEntryByName(aClass, aMethodName));
    Result := FMethodIndex <> 0;
    if Result then
      Result := InjectStaticMethod(aClass, 
        PPublishedMethodEntry(FMethodIndex).Address, aNewLocation, aDirective);

    {//this will inject it to PMT, abondoned
    FMethodOriginalLocation := aClass.MethodAddress(aMethodName);
    Result := Inject(FMethodOriginalLocation, aNewLocation, aClass, aMethodType);
    //}
  end
end;

function TMeInjector.Inject:Boolean;
var
  vInjectProc: function: Boolean of object;
begin
  case FMethodType of
    mtPublished: vInjectProc := _InjectPublishedMethod;
    mtVirtual: vInjectProc := _InjectVirtualMethod;
    mtProcedure, mtStatic: vInjectProc := _InjectStaticMethod;
    mtDynamic: vInjectProc := _InjectDynamicMethod;
  else
    vInjectProc := nil;
  end;
  Result := @vInjectProc <> nil;
  if Result then 
    Result := vInjectProc();
end;

function TMeInjector.UnInject:Boolean;
var
  vUnInjectProc: function: Boolean of object;
begin
  case FMethodType of
    mtPublished: vUnInjectProc := _UnInjectPublishedMethod;
    mtVirtual: vUnInjectProc := _UnInjectVirtualMethod;
    mtProcedure, mtStatic: vUnInjectProc := _UnInjectStaticMethod;
    mtDynamic: vUnInjectProc := _UnInjectDynamicMethod;
  else
    vUnInjectProc := nil;
  end;
  Result := @vUnInjectProc <> nil;
  if Result then
  begin 
    Result := vUnInjectProc();
  end;
end;

function TMeInjector._InjectPublishedMethod: Boolean;
begin
  FMethodOriginalLocation := PPublishedMethodEntry(FMethodIndex).Address;
  FMethodOriginalActualLocation := @PPublishedMethodEntry(FMethodIndex).Address;
  //FMethodOriginalActualLocation := GetActualAddress(FMethodOriginalActualLocation);
  WriteMem(FMethodOriginalActualLocation, @FMethodNewLocation, SizeOf(FMethodNewLocation));
  Result := True;
end;

function TMeInjector._InjectStaticMethod: Boolean;
begin
  Result := PatchDirective(FMethodOriginalActualLocation, FMethodNewLocation, FMethodOriginalBackup, FMethodIndex);
  {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
  with FMethodOriginalBackup do
    if IsRedirectCodeNoop(FMethodOriginalBackup) then
    begin 
      Integer(FMethodOriginalActualLocation) := Integer(FMethodOriginalActualLocation) 
        + cNearJMPDirectiveSize;
    end
    {$IFNDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
    else
      raise EMeInjectorError.CreateRes(@rsCanNotInjectError); 
    {$ENDIF}
  {$ENDIF}
end;

function TMeInjector._InjectVirtualMethod: Boolean;
begin
  FMethodOriginalLocation := SetVirtualMethod(FMethodClass, FMethodIndex, FMethodNewLocation);
  Result := FMethodOriginalLocation <> nil;
end;

function TMeInjector._InjectDynamicMethod: Boolean;
begin
  FMethodOriginalLocation := SetDynamicMethod(FMethodClass, FMethodIndex, FMethodNewLocation);
  Result := FMethodOriginalLocation <> nil;
end;

function TMeInjector._UnInjectPublishedMethod: Boolean;
var
  P: Pointer;
begin
  ReadMem(FMethodOriginalActualLocation, @P, SizeOf(P));
  if P <> FMethodNewLocation then 
    raise EMeInjectorError.CreateRes(@rsInjectByOthersError); 
  WriteMem(FMethodOriginalActualLocation, @FMethodOriginalLocation, SizeOf(FMethodOriginalLocation));
  Result := True;
end;

function TMeInjector._UnInjectStaticMethod: Boolean;
begin
  {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
  with FMethodOriginalBackup do 
    if IsRedirectCodeNoop(FMethodOriginalBackup) then 
  //if (Offset = Integer(cX86NoOpDirective4Bytes)) and  (Jump = cX86NoOpDirective) then
    Integer(FMethodOriginalActualLocation) := Integer(FMethodOriginalActualLocation)
      - cNearJMPDirectiveSize;
  {$ENDIF}
  if not IsPatchedDirective(FMethodOriginalActualLocation, FMethodNewLocation, FMethodIndex) then
    raise EMeInjectorError.CreateRes(@rsInjectByOthersError); 
  Result := UnPatchDirective(FMethodOriginalActualLocation, FMethodOriginalBackup);
end;

function TMeInjector._UnInjectVirtualMethod: Boolean;
var
  p: Pointer;
begin
  p := GetVirtualMethod(FMethodClass, FMethodIndex);
  if p <> FMethodNewLocation then
    raise EMeInjectorError.CreateRes(@rsInjectByOthersError); 
  p := SetVirtualMethod(FMethodClass, FMethodIndex, FMethodOriginalLocation);
  Result := p <> nil;
end;

function TMeInjector._UnInjectDynamicMethod: Boolean;
var
  p: Pointer;
begin
  p := GetDynamicMethodBySlot(FMethodClass, FMethodIndex);
  if p <> FMethodNewLocation then
    raise EMeInjectorError.CreateRes(@rsInjectByOthersError); 
  p := SetDynamicMethod(FMethodClass, FMethodIndex, FMethodOriginalLocation);
  Result := p <> nil;
end;

function TMeInjector.OriginalProc: Pointer;
begin
  {$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
  if IsRedirectCodeNoop(FMethodOriginalBackup) then
    Result := FMethodOriginalActualLocation 
  else 
  {$ENDIF}
  {$IFDEF STATIC_METHOD_SCRAMBLING_CODE_SUPPORT}
    Result := @FMethodOriginalBackup; 
  {$ELSE}
    Result := nil;
  {$ENDIF}
end;

end.
