{*
  Classes de gestion de coroutines
  ScCoroutines propose deux classes principales. TCoroutine est la classe de
  base de gestion d'une coroutine. TCoroutineEnumerator en est une dérivée
  spécialisée pour la création d'un énumérateur en coroutine.
  Cette unité a besoin d'un test sous Windows 95/98/Me pour l'agrandissement de
  la pile, car PAGE_GUARD n'est pas supporté dans ces versions.
  @author sjrd, sur une idée de Bart van der Werf
  @version 1.0
*}
unit ScCoroutines;

interface

uses
  Windows, SysUtils;

const
  /// Taille minimale de pile
  MinStackSize = $10000;

resourcestring
  SCoroutInvalidOpWhileRunning =
    'Opération invalide lorsque la coroutine est en exécution';
  SCoroutInvalidOpWhileNotRunning =
    'Opération invalide lorsque la coroutine n''est pas en exécution';
  SCoroutBadStackSize =
    'Taille de pile incorrecte (%d) : doit être multiple de 64 Ko';
  SCoroutTerminating =
    'La coroutine est en train de se terminer';
  SCoroutTerminated =
    'Impossible de continuer : la coroutine est terminée';
  SCoroutNotTerminated =
    'Impossible de réinitialiser : la coroutine n''est pas terminée';

type
  PTIB = ^TTIB;
  TTIB = packed record
    SEH: Pointer;
    StackTop: Pointer;
    StackBottom: Pointer;
  end;

  TRunningFrame = record
    SEH: Pointer;
    StackTop: Pointer;
    StackBottom: Pointer;
    StackPtr: Pointer;
    InstructionPtr: Pointer;
  end;

  {*
    Type de boucle de coroutine
    - clNoLoop : exécutée une fois, ne boucle pas
    - clImmediate : relance immédiatement jusqu'au premier Yield
    - clNextInvoke : relance lors du prochain appel à Invoke
  *}
  TCoroutineLoop = (clNoLoop, clImmediate, clNextInvoke);

  /// Erreur liée à l'utilisation d'une coroutine
  ECoroutineError = class(Exception);

  /// Interruption prématurée d'une coroutine
  ECoroutineTerminating = class(Exception);

  {*
    Classe de support des coroutines
    La méthode Invoke ne peut avoir qu'une seule exécution à la fois. Elle ne
    peut ni être appelée dans deux threads différents en même temps ; ni être
    appelée depuis Execute (ce qui constitue un appel récursif).
    En revanche, elle peut être appelée successivement par deux threads
    différents.

    La propriété Loop détermine le comportement de bouclage de la coroutine.
    Celle-ci peut soit ne pas boucler (clNoLoop) : un appel à Invoke lorsque
    Terminated vaut True déclenchera une exception. Soit boucler immédiatement
    (clImmediate) : dès que Execute se termine, elle est rappelée sans revenir
    à l'appelant. Soit boucler au prochain Invoke : dans ce cas l'appelant
    reprend la main entre la fin d'une exécution et le début de la suivante.

    La procédure Execute devrait tester l'état de Terminating après chaque
    appel à Yield, et se terminer proprement si cette propriété vaut True.
    Cette propriété sera positionnée à True lorsque l'objet coroutine devra se
    libérer, avant de relancer l'exécution. Si un appel à Yield est fait dans
    cet état, une exception de type ECoroutineTerminating assure que celle-ci
    se termine immédiatement.

    Le nombre d'instances simultanées de TCoroutine ne peut jamais excéder
    32 K, et ce dans le meilleur des cas, car chacune doit réserver une plage
    de mémoire virtuelle de taille minimum 64 Ko.

    @author sjrd, sur une idée de Bart van der Werf
    @version 1.0
  *}
  TCoroutine = class(TObject)
  private
    FStackSize: Cardinal;  /// Taille maximale de la pile
    FStackBuffer: Pointer; /// Pile virtuelle totale
    FStack: Pointer;       /// Début de la pile de la coroutine

    FLoop: TCoroutineLoop; /// Type de boucle de la coroutine

    FCoroutineRunning: Boolean; /// True si la coroutine est cours d'exécution
    FTerminating: Boolean;      /// True si la coroutine doit se terminer
    FTerminated: Boolean;       /// True si la coroutine est terminée

    FCoroutineFrame: TRunningFrame; /// Cadre d'exécution de la coroutine
    FCallerFrame: TRunningFrame;    /// Cadre d'exécution de l'appelant

    FExceptObject: TObject;  /// Objet exception déclenchée par la coroutine
    FExceptAddress: Pointer; /// Adresse de déclenchement de l'exception

    procedure InitCoroutine;
    procedure Main;
    procedure SwitchRunningFrame;
    procedure Terminate;
  protected
    procedure Invoke;
    procedure Yield;
    procedure Reset;

    {*
      Coroutine à exécuter
      Surchargez Execute pour donner le code de la coroutine.
    *}
    procedure Execute; virtual; abstract;

    property Loop: TCoroutineLoop read FLoop write FLoop;

    property CoroutineRunning: Boolean read FCoroutineRunning;
    property Terminating: Boolean read FTerminating;
    property Terminated: Boolean read FTerminated;
  public
    constructor Create(ALoop: TCoroutineLoop = clNoLoop;
      StackSize: Cardinal = MinStackSize);
    destructor Destroy; override;

    procedure BeforeDestruction; override;

    class procedure Error(const Msg: string;
      Data: Integer = 0); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer = 0); overload;
  end;

  {*
    Énumérateur en coroutine
    Pour obtenir un énumérateur concret, il faut surcharger les méthode Execute
    et SetNextValue, et définir une propriété Current. La méthode Execute peut
    appeler plusieurs fois Yield avec une valeur quelconque en paramètre. La
    méthode SetNextValue doit stocker cette valeur retransmise là où la
    propriété Current pourra la relire.
    @author sjrd, sur une idée de Sergey Antonov
    @version 1.0
  *}
  TCoroutineEnumerator = class(TCoroutine)
  protected
    procedure Yield(const Value); reintroduce;

    {*
      Stocke une valeur envoyée par Yield
      Surchargez SetNextValue pour stocker correctement Value selon son type.
      @param Value   Valeur à stocker
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
  Initialise les variables globales
*}
procedure InitGlobalVars;
var
  SystemInfo: TSystemInfo;
begin
  GetSystemInfo(SystemInfo);
  PageSize := SystemInfo.dwPageSize;
end;

{------------------------------------}
{ Global routines used by TCoroutine }
{------------------------------------}

{*
  Restaure un état serein d'exécution de code Delphi et trouve le TIB
  @return Adresse linéaire du TIB
*}
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

{*
  Pop tous les registres de la pile
  PopRegisters est utilisée comme point de retour dans SaveRunningFrame.
*}
procedure PopRegisters;
asm
        POPAD
end;

{*
  Sauvegarde le cadre d'exécution courant
  @param TIB     Pointeur sur le TIB
  @param Frame   Où stocker le cadre d'exécution
  @return Pointeur sur le TIB
*}
function SaveRunningFrame(TIB: PTIB; var Frame: TRunningFrame): PTIB;
asm
        { ->    EAX     Pointer to TIB
                EDX     Pointer to frame
          <-    EAX     Pointer to TIB   }

        // TIB
        MOV     ECX,[EAX].TTIB.SEH
        MOV     [EDX].TRunningFrame.SEH,ECX
        MOV     ECX,[EAX].TTIB.StackTop
        MOV     [EDX].TRunningFrame.StackTop,ECX
        MOV     ECX,[EAX].TTIB.StackBottom
        MOV     [EDX].TRunningFrame.StackBottom,ECX

        // ESP
        LEA     ECX,[ESP+4] // +4 because of return address
        MOV     [EDX].TRunningFrame.StackPtr,ECX

        // Return address
        MOV     [EDX].TRunningFrame.InstructionPtr,OFFSET PopRegisters
end;

{*
  Met en place un cadre d'exécution
  Cette procédure ne retourne jamais : elle continue l'exécution à
  l'instruction pointée par Frame.InstructionPtr.
  @param TIB     Pointeur sur le TIB
  @param Frame   Informations sur le cadre à mettre en place
*}
procedure SetupRunningFrame(TIB: PTIB; const Frame: TRunningFrame);
asm
        { Make sure you do a *JMP* to this procedure, not a *CALL*, because it
          won't get back and musn't get the return address in the stack. }

        { ->    EAX     Pointer to TIB
                EDX     Pointer to frame
                EBX     Value for EAX just before the jump }

        // TIB
        MOV     ECX,[EDX].TRunningFrame.SEH
        MOV     [EAX].TTIB.SEH,ECX
        MOV     ECX,[EDX].TRunningFrame.StackBottom
        MOV     [EAX].TTIB.StackBottom,ECX
        MOV     ECX,[EDX].TRunningFrame.StackTop
        MOV     [EAX].TTIB.StackTop,ECX

        // ESP
        MOV     ESP,[EDX].TRunningFrame.StackPtr

        // Jump to the instruction
        MOV     EAX,EBX
        MOV     ECX,[EDX].TRunningFrame.InstructionPtr
        JMP     ECX
end;

{------------------}
{ TCoroutine class }
{------------------}

{*
  Crée une coroutine avec une taille de pile donnée
  @param ALoop       Type de boucle de la coroutine (défaut : clNoLoop)
  @param StackSize   Taille de la pile (défaut et minimum : MinStackSize)
*}
constructor TCoroutine.Create(ALoop: TCoroutineLoop = clNoLoop;
  StackSize: Cardinal = MinStackSize);
begin
  inherited Create;

  // Check stack size
  if (StackSize < MinStackSize) or (StackSize mod MinStackSize <> 0) then
    Error(@SCoroutBadStackSize, StackSize);

  // Reserve stack address space
  FStackSize := StackSize;
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

  // Set up configuration
  FLoop := ALoop;

  // Set up original state
  FCoroutineRunning := False;
  FTerminating := False;
  FTerminated := False;

  // Initialize coroutine
  InitCoroutine;
end;

{*
  Détruit l'instance
*}
destructor TCoroutine.Destroy;
begin
  // Release stack address space
  if Assigned(FStackBuffer) then
    if not VirtualFree(FStackBuffer, 0, MEM_RELEASE) then
      RaiseLastOSError;

  inherited;
end;

{*
  Initialise la coroutine avant sa première exécution
*}
procedure TCoroutine.InitCoroutine;
begin
  with FCoroutineFrame do
  begin
    SEH := nil;
    StackTop := FStack;
    StackBottom := FStackBuffer;
    StackPtr := FStack;
    InstructionPtr := @TCoroutine.Main;
  end;

  FExceptObject := nil;
end;

{*
  Méthode principale de la coroutine
*}
procedure TCoroutine.Main;
begin
  if not Terminating then
  try
    repeat
      Execute;
      if (Loop = clNextInvoke) and (not Terminating) then
        Yield;
    until (Loop = clNoLoop) or Terminating;
  except
    FExceptObject := AcquireExceptionObject;
    FExceptAddress := ExceptAddr;
  end;

  Terminate;
end;

{*
  Switche entre les deux cadres d'exécution (appelant-coroutine et vice versa)
*}
procedure TCoroutine.SwitchRunningFrame;
asm
        { ->    EAX     Self }

        // Save all registers
        PUSHAD
        MOV     EBX,EAX

        // Get CoroutineRunning value into CF then switch it
        BTC     WORD PTR [EBX].TCoroutine.FCoroutineRunning,0

        // Get frame addresses
        LEA     ESI,[EBX].TCoroutine.FCoroutineFrame
        LEA     EDI,[EBX].TCoroutine.FCallerFrame
        JC      @@running // from BTC
        XCHG    ESI,EDI
@@running:

        // Clean up and get TIB
        CALL    CleanUpAndGetTIB

        // Save current running frame
        MOV     EDX,ESI
        CALL    SaveRunningFrame

        // Set up new running frame
        MOV     EDX,EDI
        JMP     SetupRunningFrame
end;

{*
  Termine la coroutine
*}
procedure TCoroutine.Terminate;
asm
        { ->    EAX     Self }

        // Update state
        MOV     [EAX].TCoroutine.FTerminated,1
        MOV     [EAX].TCoroutine.FCoroutineRunning,0

        // Go back to caller running frame
        LEA     EDX,[EAX].TCoroutine.FCallerFrame
        CALL    CleanUpAndGetTIB
        JMP     SetupRunningFrame
end;

{*
  Exécute la coroutine jusqu'au prochain appel à Yield
*}
procedure TCoroutine.Invoke;
var
  TempError: TObject;
begin
  if CoroutineRunning then
    Error(@SCoroutInvalidOpWhileRunning);
  if Terminated then
    Error(@SCoroutTerminated);

  // Enter the coroutine
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

{*
  Rend la main à l'appelant - retournera lors du prochain appel à Invoke
*}
procedure TCoroutine.Yield;
begin
  if not CoroutineRunning then
    Error(@SCoroutInvalidOpWhileNotRunning);
  if Terminating then
    raise ECoroutineTerminating.CreateRes(@SCoroutTerminating);

  SwitchRunningFrame;
end;

{*
  Réinitialise complètement la coroutine
  La coroutine doit être terminée pour appeler Reset (Terminated = True).
  Reset peut également être appelée si la coroutine s'est terminée à cause
  d'une exception.
*}
procedure TCoroutine.Reset;
begin
  if CoroutineRunning then
    Error(@SCoroutInvalidOpWhileRunning);
  if not Terminated then
    Error(@SCoroutNotTerminated);

  FTerminated := False;
  InitCoroutine;
end;

{*
  Appelé juste avant le premier destructeur
  BeforeDestruction assure qu'on n'essaie pas de détruire l'objet coroutine
  depuis le code de la coroutine.
  Si la coroutine n'a pas terminé son exécution lors du dernier appel à Invoke,
  BeforeDestruction tente de la faire se terminer correctement. Si un appel à
  Yield survient, une exception ECoroutineTerminating est déclenchée pour
  forcer la coroutine à se terminer.
*}
procedure TCoroutine.BeforeDestruction;
begin
  if FCoroutineRunning then
    Error(@SCoroutInvalidOpWhileRunning);

  FTerminating := True;

  if not Terminated then
  begin
    SwitchRunningFrame;
    if Assigned(FExceptObject) then
      FExceptObject.Free;
  end;

  inherited;
end;

{*
  Déclenche une erreur ECoroutineError
  @param Msg    Chaîne de format du message
  @param Data   Paramètre du format
*}
class procedure TCoroutine.Error(const Msg: string; Data: Integer = 0);

  function ReturnAddr: Pointer;
  asm
        MOV     EAX,[EBP+4]
  end;

begin
  raise ECoroutineError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

{*
  Déclenche une erreur ECoroutineError
  @param Msg    Chaîne de ressource de format du message
  @param Data   Paramètre du format
*}
class procedure TCoroutine.Error(Msg: PResStringRec; Data: Integer = 0);
begin
  Error(LoadResString(Msg), Data);
end;

{----------------------}
{ TYieldIterator class }
{----------------------}

{*
  Renvoie une valeur intermédiaire
  Yield utilise SetNextValue pour stocker la valeur, qui devra ensuite être
  accessible via la définition d'une propriété Current.
  @param Value   Valeur à renvoyer
*}
procedure TCoroutineEnumerator.Yield(const Value);
begin
  SetNextValue(Value);
  inherited Yield;
end;

{*
  Passe à l'élément suivant de l'énumérateur
  @return True s'il y a encore un élément, False si l'énumérateur est terminé
*}
function TCoroutineEnumerator.MoveNext: Boolean;
begin
  if not Terminated then
    Invoke;
  Result := not Terminated;
end;

initialization
  InitGlobalVars;
end.

