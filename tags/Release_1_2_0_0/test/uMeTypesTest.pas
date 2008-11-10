unit uMeTypesTest;

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
  TypInfo,
  TestFramework
  //uStringListEx,
  , uMeObject
  , uMeTypes
  ;

type
  TTestNumber = (one, two, three, four, five, six, seven);
  TTestNumber2 = (eight, nine, ten);
  TTestNumber3 = (eightdf, ninsde, tsden, fsfe);
  TTestSuite = (tsRTTI, tsObject, tsAOP, tsRemote, tsService, tsRemoteSvr, tsApp,tsThread, tsGood);
  TTestSuite1 = (tsRTTI1, tsObject1, tsAOP1, tsRemote1, tsService1, tsRemoteSvr1, tsApp1,tsThread1, tsGood1);
  TTestSuites = set of TTestSuite;
  TTestSuite1s = set of TTestSuite;
  TTestNumbers = set of TTestNumber;
  TTestNumber2s = set of TTestNumber2;
  TTestNumber3s = set of TTestNumber3;


type
  {
   to test the RTTI TypInfo assign
    you should override the MeTypeClass and TestAssignFromTypeData
    then published your procedure Test_AssignTypeInfo, like this:

procedure TTest_MeSetType.Test_AssignTypeInfo;
begin
  //Must RegisterType(TTestNumber) first
  TestAssignFromTypeInfo(TypeInfo(TTestNumbers));
  TestAssignFromTypeInfo(TypeInfo(TTestNumber3s)); 
end;

  }
  TTest_MeType = class (TTestCase)
  protected
    FType: PMeType;
    //FTypeInfo: PTypeInfo;
  protected
    //Free the FType
    procedure Setup;override;
    procedure TearDown;override;
    procedure TestAssignFromTypeData(aTypeData: PTypeData);virtual;abstract;
    function MeTypeClass: TMeClass;virtual;

    procedure TestAssignFromTypeInfo(aTypeInfo: PTypeInfo);
  public
  published
    //procedure Test_AssignTypeInfo;
  end;

  TTest_MeCustomOrdinalType = class (TTest_MeType)
  protected
    procedure TestAssignFromTypeData(aTypeData: PTypeData);override;
  end;

  TTest_MeOrdinalType = class (TTest_MeCustomOrdinalType)
  protected
    procedure TestAssignFromTypeData(aTypeData: PTypeData);override;
  end;

  TTest_MeIntegerType = class (TTest_MeOrdinalType)
  protected
    function MeTypeClass: TMeClass;override;
  published
    procedure Test_AssignTypeInfo;
  end;

  TTest_MeSetType = class(TTest_MeCustomOrdinalType)
  protected
    function MeTypeClass: TMeClass;override;
    procedure TestAssignFromTypeData(aTypeData: PTypeData);override;
  published
    procedure Test_AssignTypeInfo;
  end;

  TTest_MeFloatType = class(TTest_MeType)
  protected
    function MeTypeClass: TMeClass;override;
    procedure TestAssignFromTypeData(aTypeData: PTypeData);override;
  published
    procedure Test_AssignTypeInfo;
  end;

  TTest_MeRegisteredTypes = class (TTestCase)
  protected
    FTypes: PMeRegisteredTypes;
  protected
    procedure Setup;override;
    //Free the FTypes
    procedure TearDown;override;
    procedure RegisterTypes(const aTypes: PMeRegisteredTypes);
  public
  published
    procedure Test_LoadFromStream;
  end;

implementation

{ TTest_MeRegisteredTypes }
procedure TTest_MeRegisteredTypes.Setup;
begin
  New(FTypes, Create);
end;

procedure TTest_MeRegisteredTypes.TearDown;
begin
  MeFreeAndNil(FTypes);
end;

procedure TTest_MeRegisteredTypes.RegisterTypes(const aTypes: PMeRegisteredTypes);
var
  vSetType: PMeSetType;
begin
  with aTypes^ do
  begin
    RegisterTypeInfo(Typeinfo(Integer));
    RegisterTypeInfo(Typeinfo(Shortint));
    RegisterTypeInfo(Typeinfo(Smallint));
    RegisterTypeInfo(Typeinfo(Cardinal));
    RegisterTypeInfo(Typeinfo(Byte));
    RegisterTypeInfo(Typeinfo(Word));

    RegisterTypeInfo(Typeinfo(Int64));

    RegisterTypeInfo(Typeinfo(String));
    RegisterTypeInfo(Typeinfo(ShortString));
    RegisterTypeInfo(Typeinfo(WideString));
    RegisterTypeInfo(Typeinfo(WideChar));
    RegisterTypeInfo(Typeinfo(Char));
    RegisterTypeInfo(Typeinfo(Real));
    RegisterTypeInfo(Typeinfo(Single));
    RegisterTypeInfo(Typeinfo(Double));
    RegisterTypeInfo(Typeinfo(Extended));
    RegisterTypeInfo(Typeinfo(Comp));
    RegisterTypeInfo(Typeinfo(Currency));
    RegisterTypeInfo(Typeinfo(TDateTime));
  
    RegisterTypeInfo(Typeinfo(Boolean));

    RegisterTypeInfo(Typeinfo(ByteBool));
    RegisterTypeInfo(Typeinfo(WordBool));
    RegisterTypeInfo(Typeinfo(LongBool));

    RegisterTypeInfo(Typeinfo(TObject));

    RegisterTypeInfo(TypeInfo(TTestSuite));
    RegisterTypeInfo(TypeInfo(TTestSuites));

    RegisterTypeInfo(TypeInfo(TTestSuite1));
    RegisterTypeInfo(TypeInfo(TTestSuite1s));

    RegisterTypeInfo(TypeInfo(TTestNumber));
    RegisterTypeInfo(TypeInfo(TTestNumbers));

    RegisterTypeInfo(TypeInfo(TTestNumber2));
    RegisterTypeInfo(TypeInfo(TTestNumber2s));

    //test 当类型的参数子类型引用了后面的暂时还没有存入流中的类型项的情况。
    //我这里通过强制修改集合类型，将起基类型改为TTestNumber2 来实现后面引用的情况！
    vSetType := PMeSetType(GetRegisteredTypeByTypeInfo(TypeInfo(TTestNumbers)));
    CheckEquals(True, Assigned(vSetType), 'TTestNumbers register failed!.');
    vSetType.CompType := GetRegisteredTypeByTypeInfo(TypeInfo(TTestNumber2));

    vSetType := PMeSetType(GetRegisteredTypeByTypeInfo(TypeInfo(TTestSuites)));
    CheckEquals(True, Assigned(vSetType), 'TTestSuites register failed!.');
    vSetType.CompType := GetRegisteredTypeByTypeInfo(TypeInfo(TTestSuite1));
  end;
end;

procedure TTest_MeRegisteredTypes.Test_LoadFromStream;
var
  vStream: TMemoryStream;
  vCount: Integer;
  vTypes: PMeRegisteredTypes;
begin
  FTypes.Clear;
  RegisterTypes(FTypes);
  vCount := FTypes.Count;
  vStream := TMemoryStream.Create;
  try
    FTypes.SaveToStream(vStream);
    FTypes.Clear;
    CheckEquals(0, FTypes.Count, 'Can not clear the types!');
    vStream.Position := 0;
    FTypes.LoadFromStream(vStream);
  finally
    FreeAndNil(vStream);
  end;

  CheckEquals(vCount, FTypes.Count, 'The types Count is mismatch!');
  New(vTypes, Create);
  try
    RegisterTypes(vTypes);
    for vCount := 0 to vTypes.Count - 1 do
    begin
      CheckEquals(PMeType(vTypes.Items[vCount]).Name, PMeType(FTypes.Items[vCount]).Name, 'The types('+IntToStr(vCount)+') name is mismatch!');
      try
      CheckEquals(True, PMeType(vTypes.Items[vCount]).IsSameAs(PMeType(FTypes.Items[vCount])), 'The types('+IntToStr(vCount)+') '+ PMeType(vTypes.Items[vCount]).Name +' is mismatch!');
      except
        on E: Exception do 
        begin
          writeln('Error:',E.Message, ' ErrorIndex=', vCount, ' TypeName=', PMeType(vTypes.Items[vCount]).Name);
          with PMeType(FTypes.Items[vCount])^ do if ClassName <> '' then writeln(' ClassName=', ClassName);
        end;
      end;
      //writeln('...ok');
    end;
  finally
    MeFreeAndNil(vTypes);
  end;
end;

{ TTest_MeType }
procedure TTest_MeType.Setup;
var
  vMeTypeClass: TMeClass;
begin
  inherited;
  vMeTypeClass := MeTypeClass;
  CheckEquals(True, Assigned(vMeTypeClass), 'Setup: the MeTypeClass is not assigned.');
  FType := PMeType(NewMeObject(vMeTypeClass));
  CheckEquals(True, Assigned(FType), 'NewMeObject: the FTypeis not assigned.');
  CheckEquals(False, Assigned(FType.Owner), 'FType.Onwer should be nil.');
end;

procedure TTest_MeType.TestAssignFromTypeInfo(aTypeInfo: PTypeInfo);
begin
  CheckEquals(Integer(MeTypeClass), Integer(FType.ClassType), 'the MeTypeClass is mismatch.');
  CheckEquals(Ord(aTypeInfo.Kind), Ord(FType.Kind), 'the TypeKind is mismatch.');
  FType.Assign(aTypeInfo);
  CheckEquals(Ord(aTypeInfo.Kind), Ord(FType.Kind), 'the TypeKind is mismatch.');
  CheckEquals(aTypeInfo.Name, FType.Name, 'the Type Name is mismatch.');
  TestAssignFromTypeData(GetTypeData(aTypeInfo));
end;

procedure TTest_MeType.TearDown;
begin
  MeFreeAndNil(FType);
end;

function TTest_MeType.MeTypeClass: TMeClass;
begin
  Result := TypeOf(TMeType);
end;

procedure TTest_MeCustomOrdinalType.TestAssignFromTypeData(aTypeData: PTypeData);
begin
  CheckEquals(Ord(aTypeData.OrdType), Ord(PMeCustomOrdinalType(FType).OrdType), 'the OrdType is mismatch.');
end;

procedure TTest_MeOrdinalType.TestAssignFromTypeData(aTypeData: PTypeData);
begin
  inherited;
  CheckEquals(aTypeData.MinValue, PMeOrdinalType(FType).MinValue, 'the MinValue is mismatch.');
  CheckEquals(aTypeData.MaxValue, PMeOrdinalType(FType).MaxValue, 'the MinValue is mismatch.');
end;

function TTest_MeIntegerType.MeTypeClass: TMeClass;
begin
  Result := TypeOf(TMeCustomIntegerType);
end;

procedure TTest_MeIntegerType.Test_AssignTypeInfo;
begin
  TestAssignFromTypeInfo(TypeInfo(Integer));
  TestAssignFromTypeInfo(TypeInfo(LongWord));
  TestAssignFromTypeInfo(TypeInfo(Smallint));
  TestAssignFromTypeInfo(TypeInfo(ShortInt));
  TestAssignFromTypeInfo(TypeInfo(Word));
  TestAssignFromTypeInfo(TypeInfo(Byte));
end;

function TTest_MeSetType.MeTypeClass: TMeClass;
begin
  Result := TypeOf(TMeSetType);
end;

procedure TTest_MeSetType.Test_AssignTypeInfo;
begin
  //Must RegisterType(TTestNumber) first
  TestAssignFromTypeInfo(TypeInfo(TTestNumbers));
  TestAssignFromTypeInfo(TypeInfo(TTestNumber3s)); 
end;

procedure TTest_MeSetType.TestAssignFromTypeData(aTypeData: PTypeData);
begin
  inherited;
  CheckEquals(Integer(GetRegisteredTypeByTypeInfo(aTypeData.CompType^)), Integer(PMeSetType(FType).CompType), 'the CompType is mismatch.');
end;

function TTest_MeFloatType.MeTypeClass: TMeClass;
begin
  Result := TypeOf(TMeFloatType);
end;

procedure TTest_MeFloatType.TestAssignFromTypeData(aTypeData: PTypeData);
begin
  inherited;
  CheckEquals(Ord(aTypeData.FloatType), Ord(PMeFloatType(FType).FloatType), 'the FloatType is mismatch.');
end;

procedure TTest_MeFloatType.Test_AssignTypeInfo;
begin
  TestAssignFromTypeInfo(TypeInfo(Real));
  TestAssignFromTypeInfo(TypeInfo(Double));
  TestAssignFromTypeInfo(TypeInfo(Single));
  TestAssignFromTypeInfo(TypeInfo(Extended));
  TestAssignFromTypeInfo(TypeInfo(Comp));
  TestAssignFromTypeInfo(TypeInfo(Currency));
  TestAssignFromTypeInfo(TypeInfo(TDateTime));
end;

Initialization
  RegisterTypeInfo(TypeInfo(TTestNumber));
  RegisterTypeInfo(TypeInfo(TTestNumber3));

  RegisterTests('MeTypes suites',
                [
                 TTest_MeFloatType.Suite
                 , TTest_MeSetType.Suite
                 , TTest_MeIntegerType.Suite
                 , TTest_MeRegisteredTypes.Suite
                ]);//}
finalization
end.
