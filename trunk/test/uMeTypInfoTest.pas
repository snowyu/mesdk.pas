unit uMeTypInfoTest;

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
  TestFramework
  //uStringListEx,
  , uMeTypInfo
  , uInternalTestObj
  ;

const
  cNewMethodResult = 1230321;

type
  TGetCountProc = function (aClass: TClass): Integer;
  TGetMethodByIndexProc = function (aClass: TClass; Index: Integer): Pointer;
  TGetIndexByMethodProc = function (aClass: TClass; aMethod: Pointer): Integer;
  TGetMethodByNameProc = function (aClass: TClass; aName: ShortString): Pointer;
  TSetMethodByIndexProc = function (aClass: TClass; const Index: Integer; const Method: Pointer): Pointer;
  TNewMethodProc = function (Self: TObject): Integer;

  TCustomTest_Method = class (TTestCase)
  protected
    FNew_Result: string;
    FObject: TObject;

    //create obj to FObject
    procedure CreateObject(aClass: TClass);
    //procedure SetUp; override;
    procedure TearDown; override;
  protected
    //the aName should be named VirtualMethodXXX, DynamicMethodXXX
    procedure TestGetMethodType(aClass: TClass; const aName: string; aMethod: Pointer);
    procedure TestGetMethodCount(aClass: TClass; aTestMethods: TTestMethods; aGetCountProc: TGetCountProc);
    procedure TestGetMethodByIndex(aClass: TClass; aTestMethods: TTestMethods; aGetProc: TGetMethodByIndexProc);
    procedure TestGetIndexByMethod(aClass: TClass; aTestMethods: TTestMethods; aGetProc: TGetIndexByMethodProc);
    procedure TestGetMethodByName(aClass: TClass; aTestMethods: TTestMethods; aGetProc: TGetMethodByNameProc);
    procedure TestSetMethodByName(aClass: TClass; aTestMethods: TTestMethods);

    procedure TestSetMethodByIndex(aClass: TClass; aTestMethods: TTestMethods; aSetProc: TSetMethodByIndexProc; aGetProc: TGetMethodByIndexProc);
  public
    function TheNewMethod(): Integer;
  end;

  TTest_DynamicMethod = class (TCustomTest_Method)
  protected
  public
  published
    procedure Test_GetMethodCount;
    procedure Test_GetMethodBySlot;
    procedure Test_GetSlotByMethod;
    procedure Test_SetMethodBySlot;
  end;

  TTest_VirtualMethod = class (TCustomTest_Method)
  protected
  public
    //procedure SetUp; override;
  published
    procedure Test_GetMethodCount;
    procedure Test_GetMethodByIndex;
    procedure Test_GetIndexByMethod;
    procedure Test_SetMethodByIndex;
  end;

  TTest_PublishedMethod = class (TCustomTest_Method)
  protected
  public
    //procedure SetUp; override;
  published
    procedure Test_GetMethodCount;
    procedure Test_GetMethod;
    procedure Test_SetMethod;
  end;

implementation

{------------------------------------------------------------------}

procedure TCustomTest_Method.CreateObject(aClass: TClass);
begin
  FreeAndNil(FObject);
  FObject := aClass.Create;
end;

procedure TCustomTest_Method.TearDown;
begin
  FreeAndNil(FObject);
end;

function TCustomTest_Method.TheNewMethod(): Integer;
begin
  // it's imposible!! the Self pointer is point to injected class, can not visit the FNew_Result.
  //FNew_Result := cNewMethodRun;//Self.ClassName;
  Result := cNewMethodResult;
  //writeln(Self.ClassName);
end;

procedure TCustomTest_Method.TestGetMethodCount(aClass: TClass; aTestMethods: TTestMethods; aGetCountProc: TGetCountProc);
var
  i: Integer;
  vS: string;
begin
  vS := ClassName;
  i :=Pos('_', vS);
  vS := Copy(vS, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  //Status('Check '+aClass.ClassName+' ' + vS + ' TestGetMethodCount');
  Status(aClass.ClassName + ': Check '+ vS +' method Count');
  {$ENDIF}
  CheckEquals(High(aTestMethods) +1, aGetCountProc(aClass),  vS  + ' Count');
end;

procedure TCustomTest_Method.TestGetMethodType(aClass: TClass; const aName: string; aMethod: Pointer);
var
  vMethodType: TMethodType;
  t: Integer;
begin
  case aName[1] of
    'V': vMethodType := mtVirtual;
    'D': vMethodType := mtDynamic;
    //'P': vMethodType := mtPublished;
    'P', 'M': vMethodType := mtStatic;
  else 
    vMethodType := mtStatic;
  end;
  t := -1; //do not get published method type.
  CheckEquals(Integer(vMethodType)
        , Integer(GetMethodType(aClass, aMethod, t))
        , 'the '+ aName + ' method type mismatch.');
  if aName[1] = 'P' then
  begin //test published method type.
    t := 0;
    CheckEquals(Integer(mtPublished)
        , Integer(GetMethodType(aClass, aMethod, t))
        , 'the '+ aName + ' method type mismatch.');
  end;
end;

procedure TCustomTest_Method.TestGetMethodByIndex(aClass: TClass; aTestMethods: TTestMethods; aGetProc: TGetMethodByIndexProc);
var
  vProc: PProcedure;
  i: Integer;
  s: string;
  vS: string;
begin
  vS := ClassName;
  i :=Pos('_', vS);
  vS := Copy(vS, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(aClass.ClassName + ': Check '+ vS + ' TestGetMethodByIndex');
  {$ENDIF}
  CreateObject(aClass);
  for i := 0 to High(aTestMethods) do
  begin
    vProc := PProcedure(aGetProc(aClass,i));
    {$IFDEF Debug_WriteToConsole_Support}
    Status(Format('Check '+vS+' method %d %x', [i, Integer((@vProc))]));
    {$ENDIF}
    CheckEquals(Integer((@aTestMethods[i].Method)), Integer((@vProc)), 'the '+vS+IntToStr(i)+' address is NOT the same address');
    if Integer((@vProc)) <> Integer(@AbstractErrorProc) then
    begin
      {$IFDEF Debug_WriteToConsole_Support}
      Status(Format('Run '+vS+' method %d', [i]));
      {$ENDIF}
      TestGetMethodType(aClass, aTestMethods[i].Name, @vProc);
      uInternalTestObj.RunResult := 'No Result';
      vProc(FObject);
      if not aTestMethods[i].IsOverride and (aClass.ClassParent <> nil) and (aClass.ClassParent <> TObject) then
        s := aClass.ClassParent.ClassName + '.'+aTestMethods[i].Name + ' Run!'
      else
        s := aClass.ClassName + '.'+aTestMethods[i].Name + ' Run!';
      CheckEquals(s, uInternalTestObj.RunResult, 'the '+vS+' Run Result error');
      CheckFalse(IsAbstractMethod(@vProc), 'the '+ aTestMethods[i].Name +' method should not be an abstract method');
      //Status(Format('Run '+vS+IntToStr(i)+' method Result:%s', [RunResult]));
    end
    else begin //check abstract method
        CheckEquals(True, IsAbstractMethod(@vProc), 'the '+ aTestMethods[i].Name +' method should be an abstract method');
    end;
  end;
  
end;

procedure TCustomTest_Method.TestGetIndexByMethod(aClass: TClass; aTestMethods: TTestMethods; aGetProc: TGetIndexByMethodProc);
var
  vProc: PProcedure;
  vIndex: Integer;
  i: Integer;
  s: string;
  vS: string;
  vMethodType: TMethodType;
begin
  vS := ClassName;
  i :=Pos('_', vS);
  vS := Copy(vS, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(aClass.ClassName + ': Check '+ vS + ' TestGetIndexByMethod');
  {$ENDIF}
  for i := 0 to High(aTestMethods) do
  begin
    vProc := aTestMethods[i].Method;
    if Integer((@vProc)) <> Integer(@AbstractErrorProc) then
    begin //Skip the abstract methods
      vIndex := aGetProc(aClass, @vProc);
      //Status(Format('Check '+vS+' method %d %x', [i, Integer((@vProc))]));
      CheckEquals(I, vIndex, 'the '+vS+IntToStr(i)+' index number error');
    end;
  end;
end;

procedure TCustomTest_Method.TestGetMethodByName(aClass: TClass; aTestMethods: TTestMethods; aGetProc: TGetMethodByNameProc);
var
  vProc: PProcedure;
  vGetProc: Pointer;
  i: Integer;
  j: Integer;
  s: ShortString;
  vS: string;
begin
  vS := ClassName;
  i :=Pos('_', vS);
  vS := Copy(vS, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(aClass.ClassName +': Check '+ vS + ' TestGetMethodByName');
  {$ENDIF}
  for i := 0 to High(aTestMethods) do
  begin
    s := aTestMethods[i].Name;
    if s <> '' then
    begin //Skip the empty name 
      vProc := aTestMethods[i].Method;
      vGetProc := aGetProc(aClass, s);
      if Integer((@vProc)) = Integer(@AbstractErrorProc) then 
      begin
        CheckEquals(True, IsAbstractMethod(vGetProc), 'the '+ s +' method should be an abstract method');
      end
      else begin
      //Status(Format('Check '+vS+' method %d %x', [i, Integer((@vProc))]));
        CheckEquals(Integer((@vProc)), Integer(vGetProc), 'the '+s+'; I='+IntToStr(i)+' method address mismatch.');
        TestGetMethodType(aClass, aTestMethods[i].Name, @vProc);
        CheckFalse(IsAbstractMethod(vGetProc), 'the '+ s +' method should not be an abstract method');
      end;
    end;
  end;
end;

procedure TCustomTest_Method.TestSetMethodByIndex(aClass: TClass; aTestMethods: TTestMethods; aSetProc: TSetMethodByIndexProc
  ; aGetProc: TGetMethodByIndexProc);
var
  i: Integer;
  vProc: TNewMethodProc;
  vGetProc: Pointer;
  vNewProc: Pointer;
  vS: string;
begin
  vS := ClassName;
  i :=Pos('_', vS);
  vS := Copy(vS, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(aClass.ClassName +': Check '+ vS + ' TestSetMethodByIndex');
  {$ENDIF}
  vNewProc :=  @TCustomTest_Method.TheNewMethod;
  CreateObject(aClass);
  for i := 0 to High(aTestMethods) do
  begin
    vGetProc := aSetProc(aClass, i, vNewProc); //return the old address.
    Check(vGetProc <> nil,  'can not set the method ' + IntToStr(i) + ' failed!');
    vProc := TNewMethodProc(aGetProc(aClass, i));
    //Status(IntToStr(Integer(@TCustomTest_Method.TheNewMethod)));
    //CheckEquals(Integer(vNewProc), Integer(@vProc),  'the SetMethod ' + IntToStr(i) + ' address is not the NewMethod!' + IntToStr(Integer(vNewProc^)));
    aSetProc(aClass, i, vGetProc); //restore old address
    CheckEquals(Integer(vNewProc), Integer(@vProc),  'the SetMethod ' + IntToStr(i) + ' address is not the NewMethod!');
    CheckEquals(cNewMethodResult, vProc(FObject), 'the SetMethod ' + IntToStr(i) + ' called result mismatch.!');
  end;
end;

procedure TCustomTest_Method.TestSetMethodByName(aClass: TClass; aTestMethods: TTestMethods);
var
  i: Integer;
  vProc: TNewMethodProc;
  vGetProc: Pointer;
  vNewProc: Pointer;
  vS: string;
begin
  vS := ClassName;
  i :=Pos('_', vS);
  vS := Copy(vS, i+1, MaxInt);
  {$IFDEF Debug_WriteToConsole_Support}
  Status(aClass.ClassName +': Check '+ vS + ' TestSetMethodByName');
  {$ENDIF}
  vNewProc :=  @TCustomTest_Method.TheNewMethod;
  CreateObject(aClass);
  for i := 0 to High(aTestMethods) do
  begin
    vGetProc := SetPublishedMethod(aClass, aTestMethods[i].Name, vNewProc); //return the old address.
    Check(vGetProc <> nil,  'can not set the method ' + IntToStr(i) + ' failed!');
    vProc := TNewMethodProc(GetPublishedMethod(aClass, aTestMethods[i].Name));
    //Status(IntToStr(Integer(@TCustomTest_Method.TheNewMethod)));
    //CheckEquals(Integer(vNewProc), Integer(@vProc),  'the SetMethod ' + IntToStr(i) + ' address is not the NewMethod!' + IntToStr(Integer(vNewProc^)));
    SetPublishedMethod(aClass, aTestMethods[i].Name, vGetProc); //restore old address
    CheckEquals(Integer(vNewProc), Integer(@vProc),  'the SetPublishedMethod ' + IntToStr(i) + ' address is not the NewMethod!');
    CheckEquals(cNewMethodResult, vProc(FObject), 'the SetPublishedMethod ' + IntToStr(i) + ' called result mismatch.!');
  end;
end;
//}

{------------------------------------------------------------------}
procedure TTest_VirtualMethod.Test_GetMethodCount;
begin
  with TestBaseObj do
    TestGetMethodCount(Obj, VirtualMethods, @GetVirtualMethodCount);
  with TestChildObj do
    TestGetMethodCount(Obj, VirtualMethods, @GetVirtualMethodCount);
end;

procedure TTest_VirtualMethod.Test_GetMethodByIndex;
begin
  with TestBaseObj do
    TestGetMethodByIndex(Obj, VirtualMethods, @GetVirtualMethod);
  with TestChildObj do
    TestGetMethodByIndex(Obj, VirtualMethods, @GetVirtualMethod);
end;

procedure TTest_VirtualMethod.Test_GetIndexByMethod;
begin
  with TestBaseObj do
    TestGetIndexByMethod(Obj, VirtualMethods, @FindVirtualMethodIndex);
  with TestChildObj do
    TestGetIndexByMethod(Obj, VirtualMethods, @FindVirtualMethodIndex);
end;

procedure TTest_VirtualMethod.Test_SetMethodByIndex;
begin
  with TestBaseObj do
    TestSetMethodByIndex(Obj, VirtualMethods, @SetVirtualMethod, @GetVirtualMethod);
  with TestChildObj do
    TestSetMethodByIndex(Obj, VirtualMethods, @SetVirtualMethod, @GetVirtualMethod);
end;

{------------------------------------------------------------------}
procedure TTest_DynamicMethod.Test_GetMethodCount;
begin
  with TestBaseObj do
    TestGetMethodCount(Obj, DynamicMethods, @GetDynamicMethodCount);
  with TestChildObj do
    TestGetMethodCount(Obj, DynamicMethods, @GetDynamicMethodCount);
end;

procedure TTest_DynamicMethod.Test_GetMethodBySlot;
begin
  with TestBaseObj do
    TestGetMethodByIndex(Obj, DynamicMethods, @GetDynamicMethodBySlot);
  with TestChildObj do
    TestGetMethodByIndex(Obj, DynamicMethods, @GetDynamicMethodBySlot);
end;

procedure TTest_DynamicMethod.Test_GetSlotByMethod;
begin
  with TestBaseObj do
    TestGetIndexByMethod(Obj, DynamicMethods, @FindDynamicMethodIndex);
  with TestChildObj do
    TestGetIndexByMethod(Obj, DynamicMethods, @FindDynamicMethodIndex);
end;

procedure TTest_DynamicMethod.Test_SetMethodBySlot;
begin
  with TestBaseObj do
    TestSetMethodByIndex(Obj, DynamicMethods, @SetDynamicMethod, @GetDynamicMethodBySlot);
  with TestChildObj do
    TestSetMethodByIndex(Obj, DynamicMethods, @SetDynamicMethod, @GetDynamicMethodBySlot);
end;

{------------------------------------------------------------------}

procedure TTest_PublishedMethod.Test_GetMethod;
begin
  with TestBaseObj do
    TestGetMethodByName(Obj, PublishedMethods, @GetPublishedMethod);
  with TestChildObj do
    TestGetMethodByName(Obj, PublishedMethods, @GetPublishedMethod);
end;

procedure TTest_PublishedMethod.Test_GetMethodCount;
begin
  with TestBaseObj do
    TestGetMethodCount(Obj, PublishedMethods, @GetPublishedMethodCount);
  with TestChildObj do
    TestGetMethodCount(Obj, PublishedMethods, @GetPublishedMethodCount);
end;

procedure TTest_PublishedMethod.Test_SetMethod;
begin
  with TestBaseObj do
    TestSetMethodByName(Obj, PublishedMethods);
  with TestChildObj do
    TestSetMethodByName(Obj, PublishedMethods);
end;

Initialization
  RegisterTests('MeTypInfo suites',
                [
                 TTest_DynamicMethod.Suite
                 , TTest_VirtualMethod.Suite
                 , TTest_PublishedMethod.Suite

                 //TTestDQueue.Suite,
                 //TTestDStack.Suite }
                ]);//}
finalization
end.
