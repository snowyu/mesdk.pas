
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

  { Summary  the abstract AOP instance feature class. add the additional properties to the class.}
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

implementation


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
end.
