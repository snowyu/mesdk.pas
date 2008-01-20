
{Summary: MeCoroutine - implements the coroutine object. }
{
  CoRoutines management classes
  CoRoutines provides two main classes. TMeCustomCoRoutine is the base class for
  CoRoutine management. TMeCoRoutineEnumerator is a derived class, specialised in
  Delphi 2005 enumerators implementation.
  This unit needs tests under Windows 95/98/Me, in case of growth of the stack,
  because PAGE_GUARD is not supported under these versions.
}
(*
 * The contents of this file are released under a dual license, and
 * you may choose to use it under either the Mozilla Public License 
 * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html) 
 * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
 * http://www.opensource.org/licenses/lgpl-license.php).
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is $RCSfile: uMeCoroutine.pas,v $.
 *
 * The Initial Developers of the Original Code are Riceball LEE.
 * Portions created by sjrd is Copyright (C) 2007
 * Portions created by Riceball LEE<riceballl@hotmail.com> is Copyright (C) 2007-2008
 * All rights reserved.
 *
 * limitations:
 *  Does not support continuation invoke chains that include the same instance twice.
 *  Limited to 32000 instances because of the 64kb minimum stacksize. (or 48k if 3gb mode is used)
 *
 * Contributor(s):
 *   sjrd (based on an idea of Bart van der Werf)
 *
 *
 *
 * Usage:
 *

 *)
unit uMeCoroutine;

interface

{$I MeSetting.inc}
{.$Define YieldClass_Supports} //use the delphi class type instead.

uses
  Windows
  , SysUtils
  {$IFNDEF YieldClass_Supports}
  , uMeSystem
  , uMeObject
  {$ENDIF}
  ;

const
  /// Miminum stack size: 64kb
  MinStackSize = $10000;

resourcestring
  rsCoRoutineErrInvalidOpWhileRunning =
    'Invalid operation while the CoRoutine is running';
  rsCoRoutineErrInvalidOpWhileNotRunning =
    'Invalid operation while the CoRoutine is not running';
  rsCoRoutineErrBadStackSize =
    'Invalid stack size (%d): must be a multiple of 64 Kb';
  rsCoRoutineErrTerminating =
    'The CoRoutine is terminating';
  rsCoRoutineErrTerminated =
    'Can''t continue: the CoRoutine is terminated';
  rsCoRoutineErrNotTerminated =
    'Can''t reset: the CoRoutine is not terminated';

type
  PTIB = ^TTIB;
  {: Thread Information Block }
  { FS:[0]}
  TTIB = packed record
    SEH: Pointer;
    StackTop: Pointer;
    StackBottom: Pointer;
  end;

  TPreservedRegisters = packed record
    FEAX,FEBX,FECX,FEDX,FESI,FEDI,FEBP:pointer;
  end;

  TMeRunningFrame = object
  protected
  public
    SEH: Pointer;
    StackTop: Pointer;
    StackBottom: Pointer;
    StackPtr: Pointer;
    InstructionPtr: Pointer;
    Regs: TPreservedRegisters;

    function StackSize: Cardinal;
    procedure AllocStackSpace(const aStackSize: Cardinal);
    procedure FreeStackSpace;
    procedure Assign(const Value: TMeRunningFrame);
  end;

  {$IFDEF YieldClass_Supports}
  TMeCustomCoRoutine = Class;
  {$ELSE}
  PMeCustomCoRoutine = ^TMeCustomCoRoutine;
  PMeCoRoutineEnumerator = ^TMeCoRoutineEnumerator;

  //PYieldString = ^TYieldString;
  //PYieldInteger = ^TYieldInteger;
  {$ENDIF}

  TMeCoroutineProc = procedure (const aCoRoutine: {$IFDEF YieldClass_Supports}TMeCustomCoRoutine{$ELSE} PMeCustomCoRoutine{$endif});
  TMeCoroutineMethod = procedure () of object;

  {
    CoRoutine loop kind
    - clRunOnce : runs once and doesn't loop, calling Invoke when Terminated is True will raise an exception.
    - clLoop : loops immediately after end of Execute
    - clNextInvoke : waits for next Invoke before looping
  }
  TMeCoRoutineLoop = (clRunOnce, clNextInvoke, clLoop);

  {: CoRoutine state
    @param coRunning      CoRoutine is running
    @param coDead         CoRoutine is Terminated.
    @param coTerminating  CoRoutine must terminate(die)
    @param coPn           preserved for future.
  }
  TMeCoroutineState = (coInited, coRunning, coTerminating, coDead, coP1, coP2, coP3, coP4, coP5);
  TMeCoroutineStates = set of TMeCoroutineState;

  /// CoRoutine Error
  ECoRoutineError = {$IFDEF YieldClass_Supports}class(Exception) {$ELSE}EMeError{$ENDIF};

  /// CoRoutine is terminating
  ECoRoutineTerminating = {$IFDEF YieldClass_Supports}class(Exception) {$ELSE}EMeError{$ENDIF};

  {
    CoRoutine support class
    The Invoke method can't be executed twice at the same moment. It can't be
    called simultaneously in two separate threads ; nor can it be called from
    Execute (which should cause recursive call).
    However, it can be called successively by two separate threads.

    The Loop property determines how the CoRoutine loops. It can either not
    loop(clRunOnce): calling Invoke when Terminated is True will raise an exception.
    Either loop immediately (clLoop): as soon as Execute ends, it is
    re-called without go back to the caller. Or loop at next Invoke
    (clNextInvoke): the caller gets running between the end of Execute and the
    next beginning.

    The Execute method shoud test for the Terminating property after each call
    to Yield, and terminate itself gracefully if Terminating is True. It will
    be set so when the TMeCustomCoRoutine object must release itself, before running
    the CoRoutine again. If you call Yield when Terminating is True, an
    ECoRoutineTerminating exception will be raised in order to ensure that
    Execute ends immediately.

    The amount of simultaneous instances of TMeCustomCoRoutine must never exceed 32 K,
    because each one must reserve a virtual memory range of 64 Ko minimum.

    @author sjrd, based on an idea of Bart van der Werf
    @version 1.0
  }
  TMeCustomCoRoutine = {$IFDEF YieldClass_Supports}class{$ELSE}object(TMeDynamicObject){$ENDIF}
  protected
    FStackSize: Cardinal;  /// Maximum stack size
    FStackBuffer: Pointer; /// Entire virtual stack
    FStack: Pointer;       /// Beginning (top) of the stack

    FLoop: TMeCoRoutineLoop; /// Loop kind
    FStates: TMeCoroutineStates;

    //FIsRunning: Boolean;
    //FTerminating: Boolean;      /// True if the CoRoutine must terminate
    //FTerminated: Boolean;       /// True if the CoRoutine is terminated

    FCoRoutineFrame: TMeRunningFrame; /// CoRoutine running frame
    FCallerFrame: TMeRunningFrame;    /// Caller running frame

    FExceptObject: TObject;  /// Exception object raised by the CoRoutine
    FExceptAddress: Pointer; /// Exception raise address

    procedure InitCoRoutine;
    procedure MainExecute;
    procedure SwitchRunningFrame;
    procedure Terminate;
    function GetState(const Index: TMeCoroutineState): Boolean;

  protected
    procedure Invoke;
    procedure Yield;
    procedure Reset;

    { the CoRoutine itself. Override Execute to give the CoRoutine code. }
    procedure Execute; virtual; abstract;

    property Loop: TMeCoRoutineLoop read FLoop write FLoop;

    property IsRunning: Boolean index Ord(coRunning) read GetState;
    property Terminating: Boolean index Ord(coTerminating) read GetState;
    property Terminated: Boolean index Ord(coDead) read GetState;
  public
{ Creates a CoRoutine with a given stack size

  @param ALoop       Loop kind (default: clRunOnce)
  @param StackSize   Stack size (default and minimum: MinStackSize)
}
    constructor Create(const ALoop: TMeCoRoutineLoop = clRunOnce;
      const aStackSize: Cardinal = MinStackSize);
    destructor Destroy; {$IFDEF YieldClass_Supports}override{$ELSE} virtual{$endif}; 

    {$IFDEF YieldClass_Supports}
    procedure BeforeDestruction; override;
    {$ENDIF}

    class procedure Error(const Msg: string; Data: Integer = 0); overload; virtual;
    class procedure Error(const Msg: PResStringRec; Data: Integer = 0); overload;
  end;

  TMeCoRoutine = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCustomCoRoutine)
  protected
    FCoRoutineProc: TMeCoRoutineProc;
    procedure Execute;{$IFDEF YieldClass_Supports}override{$ELSE} virtual{$endif}; 
  public
    constructor Create(const CoRoutineProc: TMeCoRoutineProc; const ALoop: TMeCoRoutineLoop = clRunOnce);
  end;

  { the abstract enumerator running in a CoRoutine
    In order to obtain a concrete enumerator, you must override the Execute and
    SetNextValue methods, and define a Current property. The Execute method can
    call Yield many times with any value as a parameter. The SetNextValue must
    store this value, and the Current property should read it.
    @author sjrd, based on an idea of Sergey Antonov
    @version 1.0
  }
  TMeCoRoutineEnumerator = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCustomCoRoutine)
  protected
    procedure Yield(const Value); {$IFDEF SUPPORTS_REINTRODUCE}reintroduce;{$ENDIF}

    {*
      Store the value sent by Yield
      Override SetNextValue to properly store Value.
      @param Value   Value to store
    *}
    procedure SetNextValue(const Value); virtual; abstract;
  public
    function MoveNext: Boolean;
  end;

implementation

var
  PageSize: Cardinal = 4096;

{-----------------}
{ Global routines }
{-----------------}

{*
  Initialize all global variables
*}
procedure InitGlobalVars;
var
  SystemInfo: TSystemInfo;
begin
  GetSystemInfo(SystemInfo);
  PageSize := SystemInfo.dwPageSize;
end;

{ Global routines used by TMeCustomCoRoutine }

{  Reset a clean running state for Delphi code, and find the TIB

  @return TIB linear address
}
function CleanUpAndGetTIB: PTIB;
const
  TIBSelfPointer = $18;
asm
        // Clear Direction flag
        CLD

        // Reinitialize the FPU - see System._FpuInit
        FNINIT
        FWAIT
        FLDCW Default8087CW

        // Get TIB
        MOV     EAX,TIBSelfPointer
        MOV     EAX,FS:[EAX]
end;

{
  Pop all registers from the stack
  PopRegisters is used as return point in SaveRunningFrame.
}
procedure PopRegisters;
asm
  { ->  EDX  Pointer to Frame  }
{//        POPAD
        POP  EBP
        POP  EDI
        POP  ESI
        POP  EDX
        POP  ECX
        POP  EBX
        POP  EAX
}
       MOV     EAX, [EDX].TMeRunningFrame.Regs.FEAX
       MOV     EBX, [EDX].TMeRunningFrame.Regs.FEBX
       MOV     ECX, [EDX].TMeRunningFrame.Regs.FECX
       MOV     ESI, [EDX].TMeRunningFrame.Regs.FESI
       MOV     EDI, [EDX].TMeRunningFrame.Regs.FEDI
       MOV     EBP, [EDX].TMeRunningFrame.Regs.FEBP
       MOV     EDX, [EDX].TMeRunningFrame.Regs.FEDX
end;

{
  Save the current running state
  @param TIB     Pointer to TIB
  @param Frame   Where to store the running state
  @return Pointer to TIB
}
function SaveRunningFrame(TIB: PTIB; var Frame: TMeRunningFrame): PTIB;
asm
        { ->    EAX     Pointer to TIB
                EDX     Pointer to frame
          <-    EAX     Pointer to TIB   }

        // TIB
        MOV     ECX,[EAX].TTIB.SEH
        MOV     [EDX].TMeRunningFrame.SEH,ECX
        MOV     ECX,[EAX].TTIB.StackTop
        MOV     [EDX].TMeRunningFrame.StackTop,ECX
        MOV     ECX,[EAX].TTIB.StackBottom
        MOV     [EDX].TMeRunningFrame.StackBottom,ECX

        // ESP
        LEA     ECX,[ESP+4] // +4 because of return address
        MOV     [EDX].TMeRunningFrame.StackPtr,ECX

        // Return address
        MOV     [EDX].TMeRunningFrame.InstructionPtr,OFFSET PopRegisters
end;

{
  Set up a running state
  This routine never returns: it continues its execution at the instruction
  pointed by Frame.InstructionPtr.
  @param TIB     Pointer to TIB
  @param Frame   Frame informations to set up
}
procedure SetupRunningFrame(TIB: PTIB; const Frame: TMeRunningFrame);
asm
        { Make sure you do a *JMP* to this procedure, not a *CALL*, because it
          won't get back and musn't get the return address in the stack. }

        { ->    EAX     Pointer to TIB
                EDX     Pointer to frame
                EBX     Value for EAX just before the jump }

        // TIB
        MOV     ECX,[EDX].TMeRunningFrame.SEH
        MOV     [EAX].TTIB.SEH,ECX
        MOV     ECX,[EDX].TMeRunningFrame.StackBottom
        MOV     [EAX].TTIB.StackBottom,ECX
        MOV     ECX,[EDX].TMeRunningFrame.StackTop
        MOV     [EAX].TTIB.StackTop,ECX

        // ESP
        MOV     ESP,[EDX].TMeRunningFrame.StackPtr

        // Jump to the instruction
        MOV     EAX,EBX
        MOV     ECX,[EDX].TMeRunningFrame.InstructionPtr
        JMP     ECX
end;

{ TMeRunningFrame }
procedure TMeRunningFrame.AllocStackSpace(const aStackSize: Cardinal);
begin
  // Check stack size
  if (aStackSize < MinStackSize) or (aStackSize mod MinStackSize <> 0) then
    Raise ECoRoutineError.CreateResFmt(@rsCoRoutineErrBadStackSize, [aStackSize]);

  StackBottom := VirtualAlloc(nil, aStackSize, MEM_RESERVE, PAGE_READWRITE);
  if not Assigned(StackBottom) then
    RaiseLastOSError;

  StackTop := Pointer(Cardinal(StackBottom) + aStackSize);

  // Allocate base stack
  if not Assigned(VirtualAlloc(Pointer(Cardinal(StackTop) - PageSize),
    PageSize, MEM_COMMIT, PAGE_READWRITE)) then
    RaiseLastOSError;
  if not Assigned(VirtualAlloc(Pointer(Cardinal(StackTop) - 2*PageSize),
    PageSize, MEM_COMMIT, PAGE_READWRITE or PAGE_GUARD)) then
    RaiseLastOSError;

  StackPtr := StackTop;
  SEH := nil;
  InstructionPtr := nil;
end;

procedure TMeRunningFrame.FreeStackSpace;
begin
  // Release stack address space
  if Assigned(StackBottom) then
    if not VirtualFree(StackBottom, 0, MEM_RELEASE) then
      RaiseLastOSError;
  StackBottom := nil;
end;

procedure TMeRunningFrame.Assign(const Value: TMeRunningFrame);
begin
  //if Assigned(Value) then
  begin
    Regs := Value.Regs;
    //SEH  := Value.SEH;
    //InstructionPtr := Value.InstructionPtr;
  end;
end;

function TMeRunningFrame.StackSize: Cardinal;
begin
  Result := Cardinal(StackBottom) - Cardinal(StackTop);
end;

{ TMeCustomCoRoutine class }
constructor TMeCustomCoRoutine.Create(const ALoop: TMeCoRoutineLoop = clRunOnce;
  const aStackSize: Cardinal = MinStackSize);
begin
  inherited Create;
  FCoRoutineFrame.AllocStackSpace(aStackSize);
  FStackBuffer := FCoRoutineFrame.StackBottom;
  FStackSize := aStackSize;
  FStack := FCoRoutineFrame.StackTop;
  
  {// Check stack size
  if (aStackSize < MinStackSize) or (aStackSize mod MinStackSize <> 0) then
    Error(@rsCoRoutineErrBadStackSize, aStackSize);

  // Reserve stack address space
  FStackSize := aStackSize;
  FStackBuffer := VirtualAlloc(nil, FStackSize, MEM_RESERVE, PAGE_READWRITE);
  if not Assigned(FStackBuffer) then
    RaiseLastOSError;
  FStack := Pointer(Cardinal(FStackBuffer) + FStackSize);

  // Allocate base stack
  if not Assigned(VirtualAlloc(Pointer(Cardinal(FStack) - PageSize),
    PageSize, MEM_COMMIT, PAGE_READWRITE)) then
    RaiseLastOSError;
  if not Assigned(VirtualAlloc(Pointer(Cardinal(FStack) - 2*PageSize),
    PageSize, MEM_COMMIT, PAGE_READWRITE or PAGE_GUARD)) then
    RaiseLastOSError;
}

  // Set up configuration
  FLoop := ALoop;

  // Set up original state
  //FIsRunning := False;
  //FTerminating := False;
  //FTerminated := False;

  // Initialize CoRoutine
  InitCoRoutine;
end;

{ Destroy the instance }
destructor TMeCustomCoRoutine.Destroy;
begin
  {$IFNDEF YieldClass_Supports}
  if coRunning in FStates then
    Error(@rsCoRoutineErrInvalidOpWhileRunning);

  //FTerminating := True;
  Include(FStates, coTerminating);

  if not (coDead in FStates) then
  begin
    SwitchRunningFrame;
    if Assigned(FExceptObject) then
      FExceptObject.Free;
  end;
  {$ENDIF}


  // Release stack address space
  FCoRoutineFrame.FreeStackSpace;
  {if Assigned(FStackBuffer) then
    if not VirtualFree(FStackBuffer, 0, MEM_RELEASE) then
      RaiseLastOSError;
  }
  inherited;
end;

{ Initialize the CoRoutine before its first execution }
procedure TMeCustomCoRoutine.InitCoRoutine;
begin
  with FCoRoutineFrame do
  begin
    SEH := nil;
    StackTop := FStack;
    //StackBottom := FStackBuffer;
    StackPtr := FStack;
    InstructionPtr := @TMeCustomCoRoutine.MainExecute;
  end;

  FExceptObject := nil;
end;

{ Main CoRoutine method }
procedure TMeCustomCoRoutine.MainExecute;
begin
  if not (coTerminating in FStates) then
  try
    repeat
      Execute;
      if (Loop = clNextInvoke) and (not (coTerminating in FStates)) then
        Yield;
    until (Loop = clRunOnce) or (coTerminating in FStates);
  except
    FExceptObject  := AcquireExceptionObject;
    FExceptAddress := ExceptAddr;
  end;

  Terminate;
end;

{ Switch the two running states (caller-CoRoutine and vice versa) }
procedure TMeCustomCoRoutine.SwitchRunningFrame;
asm
        { ->    EAX     Self }

        // Save all registers
{        //PUSHAD
        PUSH  EAX
        PUSH  EBX
        PUSH  ECX
        PUSH  EDX
        PUSH  ESI
        PUSH  EDI
        PUSH  EBP
}
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     EBX,EAX

        // Get IsRunning value into CF then switch it
        BTC     WORD PTR [EBX].TMeCustomCoRoutine.FStates,coRunning

        // Get frame addresses
        LEA     ESI,[EBX].TMeCustomCoRoutine.FCoRoutineFrame
        LEA     EDI,[EBX].TMeCustomCoRoutine.FCallerFrame
        JC      @@running // from BTC
        XCHG    ESI,EDI   // saving the CallerFrame for current suspende switch to run....

@@running:
        // Save all registers
        MOV [ESI].TMeRunningFrame.Regs.FEAX, EAX
        MOV [ESI].TMeRunningFrame.Regs.FECX, ECX
        MOV [ESI].TMeRunningFrame.Regs.FEDX, EDX
        MOV [ESI].TMeRunningFrame.Regs.FEBP, EBP
        POP ECX
        MOV [ESI].TMeRunningFrame.Regs.FEBX, ECX
        POP ECX
        MOV [ESI].TMeRunningFrame.Regs.FEDI, ECX
        POP ECX
        MOV [ESI].TMeRunningFrame.Regs.FESI, ECX

        // Clean up and get TIB, Return in EAX
        CALL    CleanUpAndGetTIB

        // Save current running frame
        MOV     EDX,ESI
        CALL    SaveRunningFrame

        // Set up new running frame
        MOV     EDX,EDI
        JMP     SetupRunningFrame
end;

{ Terminate the CoRoutine, must be called in the CoRoutine!!}
procedure TMeCustomCoRoutine.Terminate;
asm
        { ->    EAX     Self }
        BT      [EAX].TMeCustomCoRoutine.FStates, coRunning
        JNC      @@exit

        // Update state
        //set coDead bit
        BTS     [EAX].TMeCustomCoRoutine.FStates, coDead
        //clear coRunning bit
        BTC     [EAX].TMeCustomCoRoutine.FStates, coRunning

        // Go back to caller running frame
        LEA     EDX,[EAX].TMeCustomCoRoutine.FCallerFrame
        CALL    CleanUpAndGetTIB
        JMP     SetupRunningFrame
@@exit:
end;

{ Execute the CoRoutine until the next call to Yield }
procedure TMeCustomCoRoutine.Invoke;
var
  TempError: TObject;
begin
  if coRunning in FStates then
    Error(@rsCoRoutineErrInvalidOpWhileRunning);
  if coDead in FStates then
    Error(@rsCoRoutineErrTerminated);

  // Enter the CoRoutine
  SwitchRunningFrame;

  if Assigned(FExceptObject) then
  begin
    {$WARN SYMBOL_DEPRECATED OFF} // EStackOverflow is deprecated
    if FExceptObject is EStackOverflow then
    try
      // Reset guard in our stack - in case of upcoming call to Reset
      if not Assigned(VirtualAlloc(FStackBuffer, PageSize, MEM_COMMIT,
        PAGE_READWRITE or PAGE_GUARD)) then
        RaiseLastOSError;
    except
      FExceptObject.Free;
      raise;
    end;
    {$WARN SYMBOL_DEPRECATED ON}

    // Re-raise exception
    TempError := FExceptObject;
    FExceptObject := nil;
    raise TempError at FExceptAddress;
  end;
end;

{ Go back to the caller - will return at the next call to Invoke }
procedure TMeCustomCoRoutine.Yield;
begin
  if not (coRunning in FStates) then
    Error(@rsCoRoutineErrInvalidOpWhileNotRunning);
  if coTerminating in FStates then
    raise ECoRoutineTerminating.CreateRes(@rsCoRoutineErrTerminating);

  SwitchRunningFrame;
end;

{  Completely reset the CoRoutine
  The CoRoutine must be terminated in order to call Reset (Terminated = True).
  Reset can also be called if the CoRoutine is terminated due to an exception.
}
procedure TMeCustomCoRoutine.Reset;
begin
  if coRunning in FStates then
    Error(@rsCoRoutineErrInvalidOpWhileRunning);
  if not (coDead in FStates) then
    Error(@rsCoRoutineErrNotTerminated);

  //FTerminated := False;
  Exclude(FStates, coDead);
  InitCoRoutine;
end;

{$IFDEF YieldClass_Supports}
{ Called just before the first destructor
  BeforeDestruction ensures that you don't release the CoRoutine object from
  the CoRoutine code itself.
  If the CoRoutine is not terminated, BeforeDestruction tries to stop it
  gracefully. Calling Yield in this state wille raise an ECoRoutineTerminating
  exception to force the CoRoutine to stop.
}
procedure TMeCustomCoRoutine.BeforeDestruction;
begin
  if coRunning in FStates then
    Error(@rsCoRoutineErrInvalidOpWhileRunning);

  //FTerminating := True;
  Include(FStates, coTerminating);

  if not (coDead in FStates) then
  begin
    SwitchRunningFrame;
    if Assigned(FExceptObject) then
      FExceptObject.Free;
  end;

  inherited;
end;
{$ENDIF}

{ Raise a ECoRoutineError error
  @param Msg    Message format string
  @param Data   Format parameter
}
class procedure TMeCustomCoRoutine.Error(const Msg: string; Data: Integer = 0);

  function ReturnAddr: Pointer;
  asm
        MOV     EAX,[EBP+4]
  end;

begin
  raise ECoRoutineError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

{ Raise a ECoRoutineError error
  @param Msg    Message format resource string
  @param Data   Format parameter
}
class procedure TMeCustomCoRoutine.Error(const Msg: PResStringRec; Data: Integer = 0);
begin
  Error(LoadResString(Msg), Data);
end;

function TMeCustomCoRoutine.GetState(const Index: TMeCoroutineState): Boolean;
begin
  Result := Index in FStates;
end;

{ TMeCoRoutine }
constructor TMeCoRoutine.Create(const CoRoutineProc: TMeCoRoutineProc; const aLoop: TMeCoRoutineLoop = clRunOnce);
begin
  FCoRoutineProc := CoRoutineProc;
  inherited Create(aLoop);
end;

procedure TMeCoRoutine.Execute;
begin
  if Assigned(FCoRoutineProc) then FCoRoutineProc({$IFNDEF YieldClass_Supports}@{$ENDIF}Self);
end;

{ TMeCoRoutineEnumerator }

{ Send an intermediate value
  Yield uses SetNextValue to store the value. It should then be accessible via
  the Current property.
  @param Value   Value to send
}
procedure TMeCoRoutineEnumerator.Yield(const Value);
begin
  SetNextValue(Value);
  inherited Yield;
end;

{ Go forward to the next item
  @return True if there is still an item, False if the enumerator is terminated
}
function TMeCoRoutineEnumerator.MoveNext: Boolean;
begin
  if not (coDead in FStates) then
    Invoke;
  Result := not (coDead in FStates);
end;

initialization
  InitGlobalVars;
end.
