//just a demo
unit uLoginManager;

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
  , Controls
  , LoginDialog
  ;

type
  TLoginState = (lsNone, lsLogout, lsLogined);
  TLoginManager = class
  protected
    FUser: string;
    FPassword: string;
    FLogined: Boolean;
    FOnStatus: TNotifyEvent;
  public
    function IsLogined: Boolean;
    function Login: TLoginState;
    procedure Logout;
    property OnStatus: TNotifyEvent read FOnStatus write FOnStatus;
  end;


function GLoginManager: TLoginManager;

implementation

var
  FLoginManager: TLoginManager;

function GLoginManager: TLoginManager;
begin
  if not assigned(FLoginManager) then
    FLoginManager := TLoginManager.Create;
  Result := FLoginManager;
end;
function TLoginManager.IsLogined: Boolean;
begin
  Result := FLogined;
end;

function TLoginManager.Login: TLoginState;
begin
  Result := lsNone;
  if IsLogined then Result := lsLogined;
  if Result <> lsLogined then
  begin
    Case ShowLoginDialog(FUser, FPassword) of
      mrOK : Result := lsLogined;
    end;
    if Result = lsLogined then
    begin
      if (FUser <> 'admin') or (FPassword <> 'admin') then Result := lsLogout;
      FLogined := Result = lsLogined;
      if Assigned(FOnStatus) then FOnStatus(Self);
    end;
  end;
end;

procedure TLoginManager.Logout;
begin
  FLogined := False;
  if Assigned(FOnStatus) then FOnStatus(Self);
end;

initialization
finalization
  FreeAndNil(FLoginManager);
end.
