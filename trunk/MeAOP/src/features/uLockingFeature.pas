{ Summary Block until the execution is finished. }
unit uLockingFeature;

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
  {$IFDEF COMPILER5_UP}
  SyncObjs, // critical sections
  {$ENDIF}
  SysUtils, Classes
  {$IFDEF MeRTTI_SUPPORT}
  , uMeTypes
  , uMeProcType
  {$ENDIF}
  , uMeInterceptor
  , uMeFeature
  ;

type
  TMeCustomLockingFeature = class(TMeCustomFeature)
  protected
    FLock: {$IFDEF COMPILER5_UP} TCriticalSection {$ELSE}
            TRTLCriticalSection{$ENDIF};
    procedure AfterExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
            Params: PMeProcParams = nil); override;
    function AllowExecute(Sender: TObject; MethodItem: TMeInterceptedMethodItem;
            const Params: PMeProcParams = nil): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  end;


implementation

{
*************************** TMeCustomLockingFeature ****************************
}
constructor TMeCustomLockingFeature.Create;
begin
  inherited Create;
  {$IFDEF COMPILER5_UP}
    FLock:=TCriticalSection.Create;
  {$ELSE}
    InitializeCriticalSection(FLock);
  {$ENDIF}
end;

destructor TMeCustomLockingFeature.Destroy;
begin
  {$IFDEF COMPILER5_UP}
    FLock.Free;
  {$ELSE}
    DeleteCriticalSection(FLock);
  {$ENDIF}
  inherited Destroy;
end;

procedure TMeCustomLockingFeature.AfterExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
        Params: PMeProcParams = nil);
begin
  UnLock;
end;

function TMeCustomLockingFeature.AllowExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil): Boolean;
begin
  Lock;
  Result := True;
end;

procedure TMeCustomLockingFeature.Lock;
begin
  {$IFDEF COMPILER5_UP}
    FLock.Enter;
  {$ELSE}
    EnterCriticalSection(FLock);
  {$ENDIF}
end;

procedure TMeCustomLockingFeature.UnLock;
begin
  {$IFDEF COMPILER5_UP}
    FLock.Leave;
  {$ELSE}
    LeaveCriticalSection(FLock);
  {$ENDIF}
end;


end.
