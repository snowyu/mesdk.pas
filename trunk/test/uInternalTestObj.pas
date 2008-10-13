unit uInternalTestObj;

{$I MeSetting.inc}

interface

//uses
  //uMeTypInfo;

type
  //procedure for method.
  PProcedure         = procedure (Self: TObject);
  PAbstractError     = PProcedure;
  TTestMethodRec = packed record
    Name: string; //the method name
    Method: PProcedure;
    IsOverride: Boolean; //该方法是否是重载的父类方法
  end;
  TTestMethods = array of TTestMethodRec;

  TTestPropObj = class
  protected
    FName: AnsiString;
    function GetName: AnsiString;
  published
    property Name: string read GetName;
  end;

  {$M+}
  TTestBaseObj = Class
  private
    procedure VirtualMethod1; virtual;abstract;
    procedure VirtualMethod2; virtual;abstract;
    procedure VirtualMethod3; virtual;

  protected
    procedure Method1;
    procedure DynamicMethod1; dynamic;abstract;

    procedure VirtualMethod4; virtual;abstract;

    procedure DynamicMethod2; dynamic;abstract;
    procedure DynamicMethod3; dynamic;
  public
    procedure VirtualMethod5; virtual;abstract;

    procedure DynamicMethod4; dynamic;abstract;
    procedure DynamicMethod5; dynamic;abstract;
    procedure Method2;
  published
    procedure VirtualMethod6; virtual;abstract;
    procedure DynamicMethod6; dynamic;abstract;

    procedure PublishedMethod3;

    procedure DynamicMethod7; dynamic;
    procedure VirtualMethod7; virtual;

  end;

  TTestChildObj = Class(TTestBaseObj)
  private
    procedure VirtualMethod8; virtual;abstract;

  protected
    procedure DynamicMethod8; dynamic;abstract;
  public
    procedure VirtualMethod6; override;
    procedure DynamicMethod6; override;

  published
    procedure VirtualMethod9; virtual;abstract;
    procedure DynamicMethod9; dynamic;abstract;

    procedure PublishedMethod3;
    procedure PublishedMethod4;
  end;

  TTestMeClassRec = packed record
    Obj: TClass;
    DynamicMethods: TTestMethods;
    VirtualMethods: TTestMethods;
    PublishedMethods: TTestMethods;
    StaticMethods: TTestMethods;
  end;

var
  RunResult: string;
  AbstractErrorProc: PAbstractError;
  TestBaseObj: TTestMeClassRec;
  TestChildObj: TTestMeClassRec;
  //TestBaseObjDynamicMethods: array of PProcedure;
  //TestChildObjDynamicMethods: array of Integer;

implementation


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

function TTestPropObj.GetName: AnsiString;
begin
  Result := FName;
end;

procedure TTestBaseObj.VirtualMethod3;
begin
  RunResult := 'TTestBaseObj.VirtualMethod3 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestBaseObj.DynamicMethod3;
begin
  RunResult := 'TTestBaseObj.DynamicMethod3 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestBaseObj.DynamicMethod7;
begin
  RunResult := 'TTestBaseObj.DynamicMethod7 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestBaseObj.VirtualMethod7; 
begin
  RunResult := 'TTestBaseObj.VirtualMethod7 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestBaseObj.Method1; 
begin
  RunResult := 'TTestBaseObj.Method1 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestBaseObj.Method2; 
begin
  RunResult := 'TTestBaseObj.Method2 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestBaseObj.PublishedMethod3; 
begin
  RunResult := 'TTestBaseObj.PublishedMethod3 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestChildObj.VirtualMethod6; 
begin
  RunResult := 'TTestChildObj.VirtualMethod6 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestChildObj.DynamicMethod6; 
begin
  //the DynamicMethod1 is the slot in the class.
  RunResult := 'TTestChildObj.DynamicMethod6 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestChildObj.PublishedMethod3;
begin
  RunResult := 'TTestChildObj.PublishedMethod3 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

procedure TTestChildObj.PublishedMethod4;
begin
  RunResult := 'TTestChildObj.PublishedMethod4 Run!';
  {$IFDEF Debug_WriteToConsole_Support}
  WriteLn(RunResult);
  {$ENDIF}
end;

{ i got the reason, so abondon it.
function CopyMethods(const aMethods: TTestMethods): TTestMethods;
var
  i: integer;
begin
  SetLength(Result, High(aMethods)+1);
  for i := 0 to High(aMethods) do
    Result[i] := aMethods[i];
end;
}
initialization
  {$IFDEF Borland}
  Pointer(@AbstractErrorProc) := Pointer(GetAbstractErrorProc);
  {$ENDIF}
  {$IFDEF FPC}
  AbstractErrorProc := GetAbstractErrorProc();
  {$ENDIF}

  //############# Dynamic Methods ###############
  with TestBaseObj do
  begin
    Obj := TTestBaseObj;
    SetLength(DynamicMethods, 7);
    DynamicMethods[0].Method := AbstractErrorProc;
    DynamicMethods[0].Name := 'DynamicMethod1';
    DynamicMethods[1].Method := AbstractErrorProc;
    DynamicMethods[1].Name := 'DynamicMethod2';
    //Integer(@DynamicMethods[2]) := Integer((@TTestBaseObj.DynamicMethod3)^);
    DynamicMethods[2].Method := PProcedure(@TTestBaseObj.DynamicMethod3);
    DynamicMethods[2].Name := 'DynamicMethod3';
    DynamicMethods[3].Method := AbstractErrorProc;
    DynamicMethods[3].Name := 'DynamicMethod4';
    DynamicMethods[4].Method := AbstractErrorProc;
    DynamicMethods[4].Name := 'DynamicMethod5';
    DynamicMethods[5].Method := AbstractErrorProc;
    DynamicMethods[5].Name := 'DynamicMethod6';
    //Integer(@DynamicMethods[6]) := Integer((@TTestBaseObj.DynamicMethod7)^);
    DynamicMethods[6].Method := PProcedure(@TTestBaseObj.DynamicMethod7);
    DynamicMethods[6].Name := 'DynamicMethod7';
  end;
  with TestChildObj do
  begin
    Obj := TTestChildObj;
    SetLength(DynamicMethods, 3);
    DynamicMethods[0].Method := AbstractErrorProc;
    DynamicMethods[0].Name := 'DynamicMethod8';
    DynamicMethods[1].Method := PProcedure(@TTestChildObj.DynamicMethod6);
    DynamicMethods[1].Name := 'DynamicMethod6';
    DynamicMethods[1].IsOverride := True;
    //DynamicMethods[1] := PProcedure(TTestChildObj.MethodAddress('DynamicMethod6'));
    DynamicMethods[2].Method := AbstractErrorProc;
    DynamicMethods[2].Name := 'DynamicMethod9';
  end;

  //############# Virtual Methods ###############
  with TestBaseObj do
  begin
    Obj := TTestBaseObj;
    SetLength(VirtualMethods, 7);
    VirtualMethods[0].Method := AbstractErrorProc;
    VirtualMethods[0].Name := 'VirtualMethod1';
    VirtualMethods[1].Method := AbstractErrorProc;
    VirtualMethods[1].Name := 'VirtualMethod2';
    //Integer(@VirtualMethods[2]) := Integer((@TTestBaseObj.DynamicMethod3)^);
    VirtualMethods[2].Method := PProcedure(@TTestBaseObj.VirtualMethod3);
    VirtualMethods[2].Name := 'VirtualMethod3';
    VirtualMethods[3].Method := AbstractErrorProc;
    VirtualMethods[3].Name := 'VirtualMethod4';
    VirtualMethods[4].Method := AbstractErrorProc;
    VirtualMethods[4].Name := 'VirtualMethod5';
    VirtualMethods[5].Method := AbstractErrorProc;
    VirtualMethods[5].Name := 'VirtualMethod6';
    //Integer(@VirtualMethods[6]) := Integer((@TTestBaseObj.VirtualMethod7)^);
    VirtualMethods[6].Method := PProcedure(@TTestBaseObj.VirtualMethod7);
    VirtualMethods[6].Name := 'VirtualMethod7';
  end;
  with TestChildObj do
  begin
    Obj := TTestChildObj;
    //VirtualMethods := CopyMethods(TestBaseObj.VirtualMethods);
    VirtualMethods := TestBaseObj.VirtualMethods; //only reference.
    SetLength(VirtualMethods, 9); //now really create and copy when changed.
    //VirtualMethods[0].Method := AbstractErrorProc;
    //VirtualMethods[1].Method := AbstractErrorProc;
    //Integer(@VirtualMethods[2]) := Integer((@TTestBaseObj.DynamicMethod3)^);
    //VirtualMethods[2].Method := PProcedure(@TTestBaseObj.VirtualMethod3);
    //VirtualMethods[3].Method := AbstractErrorProc;
    //VirtualMethods[4].Method := AbstractErrorProc;
    VirtualMethods[5].Method := PProcedure(@TTestChildObj.VirtualMethod6);
    VirtualMethods[5].IsOverride := True;
    //Integer(@VirtualMethods[6]) := Integer((@TTestBaseObj.VirtualMethod7)^);
    //VirtualMethods[6].Method := PProcedure(@TTestBaseObj.VirtualMethod7);

    //new introduced methods:
    //Writeln('TestBaseObj.VirtualMethods:',TestBaseObj.VirtualMethods[5].IsOverride);
    VirtualMethods[7].Method := AbstractErrorProc;
    VirtualMethods[8].Method := AbstractErrorProc;
  end;

  //############# Published Methods ###############
  with TestBaseObj do
  begin
    Obj := TTestBaseObj;
    SetLength(PublishedMethods, 5);
    PublishedMethods[0].Method := AbstractErrorProc;
    PublishedMethods[0].Name := 'VirtualMethod6';
    PublishedMethods[1].Method := AbstractErrorProc;
    PublishedMethods[1].Name := 'DynamicMethod6';
    PublishedMethods[2].Method := PProcedure(@TTestBaseObj.PublishedMethod3);
    PublishedMethods[2].Name := 'PublishedMethod3';
    PublishedMethods[3].Method := PProcedure(@TTestBaseObj.DynamicMethod7);
    PublishedMethods[3].Name := 'DynamicMethod7';
    PublishedMethods[4].Method := PProcedure(@TTestBaseObj.VirtualMethod7);
    PublishedMethods[4].Name := 'VirtualMethod7';
  end;
  with TestChildObj do
  begin
    Obj := TTestChildObj;
    SetLength(PublishedMethods, 4);
    PublishedMethods[0].Method := AbstractErrorProc;
    PublishedMethods[0].Name := 'VirtualMethod9';
    PublishedMethods[1].Method := AbstractErrorProc;
    PublishedMethods[1].Name := 'DynamicMethod9';
    PublishedMethods[2].Method := PProcedure(@TTestChildObj.PublishedMethod3);
    PublishedMethods[2].Name := 'PublishedMethod3';
    PublishedMethods[3].Method := PProcedure(@TTestChildObj.PublishedMethod4);
    PublishedMethods[3].Name := 'PublishedMethod4';
  end;

  //############# Static Methods ###############
  with TestBaseObj do
  begin
    Obj := TTestBaseObj;
    SetLength(StaticMethods, 3);
    StaticMethods[0].Method := PProcedure(@TTestBaseObj.Method1);
    StaticMethods[0].Name := 'Method1';
    StaticMethods[1].Method := PProcedure(@TTestBaseObj.Method2);
    StaticMethods[1].Name := 'Method2';
    StaticMethods[2].Method := PProcedure(@TTestBaseObj.PublishedMethod3);
    StaticMethods[2].Name := 'PublishedMethod3';
  end;
  with TestChildObj do
  begin
    Obj := TTestChildObj;
    SetLength(StaticMethods, 2);
    StaticMethods[0].Method := PProcedure(@TTestChildObj.PublishedMethod3);
    StaticMethods[0].Name := 'PublishedMethod3';
    StaticMethods[1].Method := PProcedure(@TTestChildObj.PublishedMethod4);
    StaticMethods[1].Name := 'PublishedMethod4';
  end;

end.