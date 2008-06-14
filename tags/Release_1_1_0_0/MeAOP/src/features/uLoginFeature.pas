unit uLoginFeature;

interface

{$I MeSetting.inc}

uses
  {$IFDEF LINUX}
  Types,
  Libc,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes
  {$IFDEF MeRTTI_SUPPORT}
  , uMeTypes
  , uMeProcType
  {$ENDIF}
  , uMeInterceptor
  , uMeFeature
  , uLoginManager
  ;

type
  TMeLoginFeature = class(TMeCustomFeature)
  protected
    function AllowExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem;
            const Params: PMeProcParams = nil): Boolean; override;
  end;


implementation

{
******************************* TMeLoginFeature ********************************
}
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


end.
