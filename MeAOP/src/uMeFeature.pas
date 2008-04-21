
{ Summary: MeFeature - implements the AOP -- feature-oriented programming class.}
{
  License
   * The contents of this file are released under the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
     * The Original Code is $RCSfile: uMeFeature.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEEis Copyright (C) 2006-2008
    * All rights reserved.
    * Contributor(s):
}
unit uMeFeature;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, TypInfo
  , Contnrs
  , uMeConsts
  , uMeTypInfo
  , uMeObject
  {$IFDEF MeRTTI_SUPPORT}
  , uMeTypes
  , uMeProcType
  {$ENDIF}
  , uMeInjector
  , uMeInterceptor
  ;

type
  { Summary the abstract AOP feature class. }
  TMeCustomFeature = class(TMeCustomInterceptor)
  public
    class function AddTo(aProc:Pointer; const aProcName: string = '';
            aMethodParams: PTypeInfo = nil): TMeAbstractInterceptor; overload;
    class function AddTo(aClass: TClass; aMethod: Pointer;  const aMethodName:
            string = ''; aMethodParams: PTypeInfo = nil):
            TMeAbstractInterceptor; overload;
    class function AddTo(aClass: TClass; aMethodName: String;  aMethodParams:
            PTypeInfo = nil): TMeAbstractInterceptor; overload;
    class function RemoveFrom(aProc: Pointer): Boolean; overload;
    class function RemoveFrom(aClass: TClass; aMethod: Pointer): Boolean;
            overload;
    class function RemoveFrom(aClass: TClass; aMethodName: String;
            aMethodParams: PTypeData = nil): Boolean; overload;
  end;

  { Summary  the abstract AOP instance feature class. the additional properties of the object can be added.}
  { Description
  there are two ways to do:
  * It can add new properties for object instance to modify the VMT, append the object size.
     you must override the AdditionalSize class method if u wanna add some data to the class.
  * override the DoObjectAfterConstruction and DoObjectBeforeDestruction, add the additional properties for every object instance here.
  TODO: not fine yet.

  * cn *
  基于类实例的AOP功能类,这样可以为每一对象实例添加新的属性
  有两个方案：
  * 修改VMT, 增加Object记录的大小,这样我就可以存放附加数据了.不过这样的话,
    必须要小心,在注入前不能有任何已经创建的注入对象!你必须重载AdditionalSize类方法，设置待加记录的大小。
  * 重载DoObjectAfterConstruction and DoObjectBeforeDestruction 方法，在这里给每一个实例添加或释放附加的空间。
  
  对于方案1和2都要注意当有对象实例的时候不能允许RemoveFrom!

  }
  TMeCustomInstanceFeature = class(TMeCustomInterceptor)
  protected
    FOriginalSize: Integer;
    FClass: TClass;
    FInstanceCount: Integer;
    class function AdditionalSize: Integer; virtual;
    procedure AfterExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
            Params: PMeProcParams = nil); override;
    procedure BeforeExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
            override;
    procedure DoObjectAfterConstruction(aObject: TObject); virtual;
    procedure DoObjectBeforeDestruction(aObject: TObject); virtual;
  public
    class function AddTo(aClass: TClass): TMeCustomInstanceFeature; overload;
            virtual;
    class function RemoveFrom(aClass: TClass): Boolean; overload;
  end;

type
  {
  this very simplest version to implement the FreeNotify On object.Free.
  it inject the TObject.FreeInstance method and check it here!!
  }
  TFreeNotifyProc = procedure(Instance : TObject) of object;
  TFreeNotificationObjects = class;
  PFreeNotificationInfo = ^ TFreeNotificationInfo;
  TFreeNotificationInfo = object
  protected
    FFreeNotifies: PMeList;
  public
    Instance: TObject;
    //Owner: TFreeNotificationObjects;
    function IndexOfProc(const aProc : TFreeNotifyProc): Integer;
    procedure AddFreeNotification(const aProc : TFreeNotifyProc);
    procedure RemoveFreeNotification(const aProc : TFreeNotifyProc);
    procedure NotifyObjectFree;
  end;

  //the objects need watch to FreeNotification
  TFreeNotificationObjects = class(TList)
  protected
    FFreeInstanceInjector: TMeInjector;
    FFreeInstance: PProcedure;

    procedure Inject(const aInstance : TObject);

    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItem(const Index: Integer): PFreeNotificationInfo;
  public
    destructor Destroy; override;
    function Add(const aInstance : TObject): PFreeNotificationInfo;
    function FindByInstance(const aInstance : TObject): PFreeNotificationInfo;
    function IndexOfInstance(const aInstance : TObject): Integer;
    property Items[const Index: Integer]: PFreeNotificationInfo read GetItem;
  end;

procedure AddFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);
procedure RemoveFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);

function GFreeNotificationObjects: TFreeNotificationObjects;

implementation


{ TFreeNotificationInfo }
procedure TFreeNotificationInfo.AddFreeNotification(const aProc : TFreeNotifyProc);
{$IFDEF PUREPASCAL}
{$IFDEF FPC}
var Ptr1, Ptr2: Pointer;
{$ENDIF FPC}
begin
  if FFreeNotifies = nil then
    FFreeNotifies := NewList;
  {$IFDEF FPC}
  asm
    MOV  EAX, [aProc]
    MOV  [Ptr1], EAX
    MOV  EAX, [aProc+4]
    MOV  [Ptr2], EAX
  end ['EAX'];
  FFreeNotifies.Insert(0, Ptr2);
  FFreeNotifies.Insert(0, Ptr1);
  {$ELSE DELPHI}
  FFreeNotifies.Insert(0, Pointer(TMethod(aProc).Data));
  FFreeNotifies.Insert(0, Pointer(TMethod(aProc).Code));
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

procedure TFreeNotificationInfo.NotifyObjectFree;
var
  I: Integer;
    ProcMethod: TMethod;
    Proc: TFreeNotifyProc Absolute ProcMethod;
begin
  if Assigned(FFreeNotifies) then
    for I := 0 to FFreeNotifies.Count div 2 - 1 do
    begin
      ProcMethod.Code := FFreeNotifies.List[I * 2];
      ProcMethod.Data := FFreeNotifies.List[I * 2 + 1];
      Proc(Instance);
    end;
end;

procedure TFreeNotificationInfo.RemoveFreeNotification(const aProc : TFreeNotifyProc);
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
        MOV  EAX, [aProc]
        MOV  [Ptr1], EAX
        MOV  EAX, [aProc+4]
        MOV  [Ptr2], EAX
      end ['EAX'];
      if (FFreeNotifies.Items[i*2] = Ptr1) and (FFreeNotifies.Items[i*2] = Ptr2) then
      begin
      	FFreeNotifies.DeleteRange(i*2, 2);
      	break;
      end;
      {$ELSE DELPHI}
      if (FFreeNotifies.Items[i*2] = TMethod(aProc).Code) and (FFreeNotifies.Items[i*2+1] = TMethod(aProc).Data) then
      begin
      	FFreeNotifies.DeleteRange(i*2, 2);
      	break;
      end;
      {$ENDIF}
    end;
end;

function TFreeNotificationInfo.IndexOfProc(const aProc : TFreeNotifyProc): Integer;
begin
  for Result := 0 to FFreeNotifies.Count div 2 - 1 do
  begin
    if FFreeNotifies.List[Result * 2] = TMethod(aProc).Code then exit;
  end;
  Result := -1;
end;

{ TFreeNotificationObjects }
destructor TFreeNotificationObjects.Destroy;
begin
  FFreeInstanceInjector.Enabled := False;
  inherited;
end;

procedure DoObjectFreeInstance(aSelf: TObject);
var
  vInfo: PFreeNotificationInfo;
begin
  with GFreeNotificationObjects do
  begin
    vInfo := FindByInstance(aSelf);
    if Assigned(vInfo) then
    begin
      vInfo.NotifyObjectFree();
      Remove(vInfo);
    end;
    FFreeInstance(aSelf);
  end;
end;

procedure TFreeNotificationObjects.Inject(const aInstance : TObject);
var
  vMethodIndex: Integer;
begin
  if not FFreeInstanceInjector.Enabled and Assigned(aInstance) then
  begin
    vMethodIndex := Integer(FFreeInstanceInjector.InjectStaticMethod(TObject, @TObject.FreeInstance, @DoObjectFreeInstance));
    Assert(vMethodIndex <> 0, 'Can not Inject the FreeInstance virtual method.');
    {
    vMethodIndex := FindVirtualMethodIndex(TObject, @TObject.FreeInstance);
    Assert(vMethodIndex >= 0, 'not found the TObject.FreeInstance virtual method');
    vMethodIndex := Integer(FFreeInstanceInjector.InjectVirtualMethod(aInstance.ClassType, vMethodIndex, @TFreeNotificationObjects.DoObjectFreeInstance));
    Assert(vMethodIndex <> 0, 'Can not Inject the FreeInstance virtual method.')
    }

    @FFreeInstance := FFreeInstanceInjector.OriginalProc;
  end;
end;

procedure TFreeNotificationObjects.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and Assigned(Ptr) then
  begin
    PFreeNotificationInfo(Ptr).FFreeNotifies.Free;
    FreeMem(Ptr);
  end;
end;

function TFreeNotificationObjects.Add(const aInstance : TObject): PFreeNotificationInfo;
begin
  Result := FindByinstance(aInstance);
  if not Assigned(Result) then
  begin
    New(Result);
    Result.Instance := aInstance;
    New(Result.FFreeNotifies, Create);
    Inject(aInstance);
    //Result.Owner := Self;
    inherited Add(Result);
  end;
end;

function TFreeNotificationObjects.IndexOfInstance(const aInstance : TObject): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if Items[Result].Instance = aInstance then 
      exit;
  end;
  Result := -1;
end;

function TFreeNotificationObjects.FindByInstance(const aInstance : TObject): PFreeNotificationInfo;
var
  i: Integer;
begin
  i := IndexOfInstance(aInstance);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

function TFreeNotificationObjects.GetItem(const Index: Integer): PFreeNotificationInfo;
begin
  Result := inherited Get(Index);
end;

//################################################################
var
  //the objects need monitor to FreeNotification
  FFreeNotificationObjects: TFreeNotificationObjects;

function GFreeNotificationObjects: TFreeNotificationObjects;
begin
  if not Assigned(FFreeNotificationObjects) then
    FFreeNotificationObjects := TFreeNotificationObjects.Create;
  Result := FFreeNotificationObjects;
end;

procedure AddFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);
var
  vInfo: PFreeNotificationInfo;
begin
  with GFreeNotificationObjects do
  begin
    vInfo := FindByInstance(aInstance);
    if not Assigned(vInfo) then
    begin
      vInfo := Add(aInstance);
      vInfo.AddFreeNotification(aProc);
    end
    else if vInfo.IndexOfProc(aProc) < 0 then begin
      
    end;
    //else ; //already exits.
  end;
end;

procedure RemoveFreeNotification(const aInstance : TObject; const aProc : TFreeNotifyProc);
var
  vInfo: PFreeNotificationInfo;
begin
  if Assigned(FFreeNotificationObjects) then
    with FFreeNotificationObjects do
    begin
      vInfo := FindByInstance(aInstance);
      if Assigned(vInfo) then
      begin
        vInfo.RemoveFreeNotification(aProc);
      end;
    end;
end;

{
******************************* TMeCustomFeature *******************************
}
class function TMeCustomFeature.AddTo(aProc:Pointer; const aProcName: string =
        ''; aMethodParams: PTypeInfo = nil): TMeAbstractInterceptor;
begin
  Result := inherited AddTo(aProc, aProcName, aMethodParams);
end;

class function TMeCustomFeature.AddTo(aClass: TClass; aMethod: Pointer;  const
        aMethodName: string = ''; aMethodParams: PTypeInfo = nil):
        TMeAbstractInterceptor;
begin
  Result := inherited AddTo(aClass, aMethod, aMethodName, aMethodParams);
end;

class function TMeCustomFeature.AddTo(aClass: TClass; aMethodName: String;
        aMethodParams: PTypeInfo = nil): TMeAbstractInterceptor;
begin
  Result := inherited AddTo(aClass, aMethodName, aMethodParams);
end;

class function TMeCustomFeature.RemoveFrom(aProc: Pointer): Boolean;
begin
  Result := inherited RemoveFrom(aProc);
end;

class function TMeCustomFeature.RemoveFrom(aClass: TClass; aMethod: Pointer):
        Boolean;
begin
  Result := inherited RemoveFrom(aClass, aMethod);
end;

class function TMeCustomFeature.RemoveFrom(aClass: TClass; aMethodName: String;
        aMethodParams: PTypeData = nil): Boolean;
begin
  Result := inherited RemoveFrom(aClass, aMethodName, aMethodParams);
end;

{
*************************** TMeCustomInstanceFeature ***************************
}
class function TMeCustomInstanceFeature.AdditionalSize: Integer;
begin
  Result := 0;
end;

class function TMeCustomInstanceFeature.AddTo(aClass: TClass):
        TMeCustomInstanceFeature;
var
  vOriginalSize: Integer;
begin
  if Assigned(aClass) then
  begin
    vOriginalSize := aClass.InstanceSize;
    if (AdditionalSize > 0) then
    begin
      //modify the VMT Size.
      SetInstanceSize(aClass, vOriginalSize + AdditionalSize);
    end;
    //inject the AfterConstruction, BeforeDestruction methods:
    Result := TMeCustomInstanceFeature(AddTo(aClass, @TObject.AfterConstruction, 'AfterConstruction'));
    AddTo(aClass, @TObject.BeforeDestruction, 'BeforeDestruction');
    Result.FOriginalSize := vOriginalSize;
    Result.FClass := aClass;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TMeCustomInstanceFeature.AfterExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
        Params: PMeProcParams = nil);
begin
  if Assigned(Sender) then
  begin
    if MethodItem.Name = 'BeforeDestruction' then
    begin
      DoObjectBeforeDestruction(Sender);
      InterlockedDecrement(FInstanceCount);
    end;
  end;
  inherited AfterExecute(Sender, MethodItem, thisState, Params);
end;

procedure TMeCustomInstanceFeature.BeforeExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
begin
  if Assigned(Sender) then
  begin
    if MethodItem.Name = 'AfterConstruction' then
    begin
      InterlockedIncrement(FInstanceCount);
      DoObjectAfterConstruction(Sender);
    end;
  end;
  inherited BeforeExecute(Sender, MethodItem, Params);
end;

procedure TMeCustomInstanceFeature.DoObjectAfterConstruction(aObject: TObject);
begin
end;

procedure TMeCustomInstanceFeature.DoObjectBeforeDestruction(aObject: TObject);
begin
end;

class function TMeCustomInstanceFeature.RemoveFrom(aClass: TClass): Boolean;
var
  vInstanceFeature: TMeCustomInstanceFeature;
  vClass: TMeInterceptedClassItem;
  vInterceptedMethod: TMeInterceptedMethodItem;
begin
  Result := Assigned(aClass); 
  if Result then
  begin
    Result := (AdditionalSize <= 0);
    if not Result then
    begin
      vInstanceFeature := TMeCustomInstanceFeature(FindInterceptor(Self));
      Result := Assigned(vInstanceFeature) and (vInstanceFeature.FInstanceCount <= 0);
      if Result then
      begin
        vClass := GInterceptedClasses.GetItem(aClass);
        Result := Assigned(vClass);
        if Result then
        begin
          vInterceptedMethod := vClass.GetMethodByAddr(@TObject.AfterConstruction);
          Result := Assigned(vInterceptedMethod);
          if Result then
          begin
            Result := vInterceptedMethod.Remove(vInstanceFeature) >= 0;
            if vInterceptedMethod.Count = 0 then
            begin
              vInterceptedMethod.Injector.Enabled := False;
              Result := vClass.Remove(vInterceptedMethod) >= 0;
            end;
          end;
          vInterceptedMethod := vClass.GetMethodByAddr(@TObject.BeforeDestruction);
          Result := Assigned(vInterceptedMethod);
          if Result then
          begin
            Result := vInterceptedMethod.Remove(vInstanceFeature) >= 0;
            if vInterceptedMethod.Count = 0 then
            begin
              vInterceptedMethod.Injector.Enabled := False;
              Result := vClass.Remove(vInterceptedMethod) >= 0;
            end;
          end;
        end;
      end;
    end;
  end;
end;


initialization
finalization
  FreeAndNil(FFreeNotificationObjects);
end.
