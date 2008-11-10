{ Summary The Script Engine Factory }
{ Description
We wrap the script enginge via TCustomScriptEngineAdapter..

Use RegisterScriptEngineClass the register a new ScriptEngineAdapter class
like this: 
  RegisterScriptEngineClass(TRemPascalScriptEngineAdapter);

Use FindScriptEngineClass function to get the registed ScriptEngineAdapter
class.

Authors
 * Architect By Riceball LEE

 2004-07-30
 Modified By creation_zy
}
unit uScriptEngineAdapter; 

{$IFDEF RELEASE_BUILD} {$D-} {$ENDIF} //Build 23+

interface

uses
  SysUtils, Classes, SyncObjs;

ResourceString
  rsFeatureNotSupportedError = '[%s] feature is not supported.';

type
  TSEScriptState = ( ssUncompiled,    // The script has yet to be compiled.
                     ssCompileErrors, // Errors occurred while compiling.
                     ssCompiled,      // The script has been compiled and is
                                      // ready to be executed/started.
                     ssRunningErrors, // Errors occured while the script was
                                      // running.
                     ssRunning );     // The script is currently active and
                                      // is running without error.

  { Summary the script engine error type }
  TSEErrorType = (etNone, etHint, etWarning, etError);
  { Summary the script engine supported languages. }
  TSEScriptLanguage = (slPascal, slBasic, slJava, slC);
  { Summary the script engine supported features.}
  { Description
    sfCompile: can compile.
    sfRun: can run
    sfHalt: can halt
    sfPause: can Pause
    sfDebug: can debug
    sfDebugBreakpoint: can apply breakpoint
    sfDebugTraceInto: the debugger trace into the next source line.
                      TraceToSource advances the debugger, possible 
                      into function calls, stopping at the next line 
                      of source code.
    sfDebugRunToLine: can Runs debugger to the specified line no.
                      like run to cursor.
    sfDebugRunUntilReturn: Runs the process until execution returns from the current function
  }
  TSESupportedFeature = (
    sfCompile, sfRun, sfHalt, sfPause, sfDebug,
    sfDebugBreakpoint, sfDebugTraceInto, sfDebugStepOver,
    sfDebugRunToLine, sfDebugRunUntilReturn	 
  );
  TSESupportedFeatures = set of TSESupportedFeature;
  TCustomScriptEngineAdapter = class;
  TScriptEngineAdapterClass = class of TCustomScriptEngineAdapter;
  { Summary The script compiler message. }
  TSECompilerMessage = packed record
    { Summary The column(from 0)  this message occured on }
    Column: Integer;
    ErrorType: TSEErrorType;
    { Summary The message string }
    Message: string;
    { Summary The position (from 0) this message occured on }
    Position: Integer;
    { Summary The Row(from 0)  this message occured on }
    Row: Integer;
  end;
  
  { Summary The Script Engine Adapter Breakpoint }
  TSEBreakpoint = record
    { Summary the breakppoint occur on the filename }
    { Description
    NOTE: some script engine can NOT supports
    }
    FileName: string;
    { Summary The Line No(from 0) of the breakpoint }
    Line: Integer;
  end;
  
  { Summary the abstract script engine adapter class. }
  { Description
  The Class method GetScriptEngineName return the script engine name.
  The Class method GetSupportedFeatures return the script engine supports
  features.
  }
  TCustomScriptEngineAdapter = class(TObject)
  private
    FCompilerSection: TCriticalSection;
    procedure SetScript(const Value: IScript);
  protected                
    //FCERegister: TCERegister;
    //FCompiled: Boolean;
    //FLanguage: TSEScriptLanguage;
    //FScriptExec: IScriptExec;
    FScript: IScript;
    //function GetBreakPoint(Index: Integer): TSEBreakPoint; virtual; abstract;
    { Summary Get the compiler's message: error/warning/hint. }
    //function GetMessage(Index: Integer): TSECompilerMessage; virtual; abstract;
    function GetPaused: Boolean; virtual; abstract;
    function GetRunning: Boolean; virtual; abstract;
    //function GetSourceCode: TStrings; virtual; abstract;
    { Summary return the script engine supported features }
    function GetSupportedFeatures: TSESupportedFeatures; virtual; abstract;
    { Summary get the compiled virtual machine code. }
    //function GetVMCode: String; virtual; abstract;
    function InternalCompile: Boolean; virtual; abstract;
    { Summary execute the compiled script and return the reuslt. }
    function InternalExecute: Boolean; virtual; abstract;
    //procedure SetBreakPoint(Index: Integer; const Value: TSEBreakPoint);
    //        virtual; abstract;
    //procedure SetLanguage(const Value: TSEScriptLanguage); virtual;
    procedure SetPaused(const Value: Boolean); virtual; abstract;
    //procedure SetSourceCode(const Value: TStrings); virtual; abstract;
    function GetState: TSEScriptState; virtual; abstract;
  public
    destructor Destroy; override;
    { Summary Compile the script }
    //function Compile(const aScript: String = ''; const CanDebug: Boolean =
    //        False): Boolean;
    function Compile: Boolean;
    { Summary execute the compiled script. }
    //function Execute(const input:Variant): Variant;
    function Execute: Boolean;                                
    { Summary Return the Script Engine Name. }
    class function ScriptEngineName: String; virtual; abstract;
    { Summary force the script halt(termiate) the running. }
    procedure Halt; virtual; abstract;
    procedure Initialize; virtual;
    //procedure SetCERegister(CERegister: TCERegister);
    { Summary the Compiler Message Count }
    //function MessageCount: Integer; virtual; abstract;
    //procedure AddBreakpoint(const aBreakpoint: TSEBreakpoint); virtual;
    //        abstract;
    { Summary the count of breakpoint }
    //function BreakPointCount: Integer; virtual; abstract;
    { Summary delete the breakpoint. }
    //procedure DeleteBreakpoint(const aIndex: integer); virtual; abstract;
    { Summary return the breakpoint index value if find else return -1. }
    //function IndexOfBreakpoint(const aBreakpoint: TSEBreakpoint): Integer;
    //        virtual; abstract;
    //procedure RemoveBreakpoint(const aBreakpoint: TSEBreakpoint); virtual;
    //        abstract;
    //property Compiled: Boolean read FCompiled;
    { Summary The current used language of this script engine }
    //property Language: TSEScriptLanguage read FLanguage write SetLanguage;
    { Summary The compiler messages/warnings/errors }
    //property Message[Index: Integer]: TSECompilerMessage read GetMessage;
    { Summary Whether the script paused }
    { Description
    set the paused to true to pause the script.
    }
    property Paused: Boolean read GetPaused write SetPaused;
    { Summary The script is running? }
    property Running: Boolean read GetRunning;
    property Script: IScript read FScript write SetScript;
    //property ScriptExec: IScriptExec read FScriptExec write SetScriptExec;
    //property SourceCode: TStrings read GetSourceCode write SetSourceCode;
    property SupportedFeatures: TSESupportedFeatures read GetSupportedFeatures;
    property State : TSEScriptState read GetState;
    { Summary The compiled code: virtual machine code. }
    //property VMCode: String read GetVMCode;
    //property BreakPoint[Index: Integer]: TSEBreakPoint read GetBreakPoint write
    //        SetBreakPoint;
  end;


{ Summary Register the Script Engine Adpater class in the script engine factory}
procedure RegisterScriptEngineClass(const aScriptEngineAdpaterClass:
        TScriptEngineAdapterClass);

function FindScriptEngineClass(const aScriptEngineName: string): TScriptEngineAdapterClass;

implementation

var
  FScriptEngineClasses: TList;
  
procedure RegisterScriptEngineClass(const aScriptEngineAdpaterClass:
        TScriptEngineAdapterClass);
begin
  FScriptEngineClasses.Add(aScriptEngineAdpaterClass);
end;

function FindScriptEngineClass(const aScriptEngineName: string): TScriptEngineAdapterClass;
var
  I: Integer;
begin
  for I := 0 to Pred(FScriptEngineClasses.Count) do
  begin
    Result := FScriptEngineClasses[I];
    if Result.ScriptEngineName = aScriptEngineName then
      Exit;
  end;
  Result := nil;
end;

{
************************** TCustomScriptEngineAdapter **************************
}
{
function TCustomScriptEngineAdapter.Compile(const aScript: string = ''; const
        CanDebug: Boolean = False): Boolean;
begin
  if aScript <> '' then
    SourceCode.Text := aScript;
  Result := InternalCompile;

  FCompiled := Result;
end;

function TCustomScriptEngineAdapter.Execute(const input:Variant):
        Variant;
begin
  if Compiled then
    Result := InternalExecute(input);
end;

procedure TCustomScriptEngineAdapter.SetLanguage(const Value:
        TSEScriptLanguage);
begin
end;
}

{ TCustomScriptEngineAdapter }

function TCustomScriptEngineAdapter.Compile: Boolean;
begin
  if Assigned(FCompilerSection) then
    FCompilerSection.Acquire;
  try
    if FScript=nil then
      Result:=false
    else if not FScript.Compiled then
      Result:=InternalCompile
    else
      Result:=true;
  finally
    if Assigned(FCompilerSection) then
      FCompilerSection.Release;
  end;
end;                                              

destructor TCustomScriptEngineAdapter.Destroy;
begin
  FreeAndNil(FCompilerSection);
  inherited;
end;

function TCustomScriptEngineAdapter.Execute: Boolean;
begin
  Result:=false;
  if FScript=nil then
    exit;
  if Compile then
    Result := InternalExecute;
end;

procedure TCustomScriptEngineAdapter.Initialize;
begin
  FCompilerSection:=TCriticalSection.Create;
end;

procedure TCustomScriptEngineAdapter.SetScript(const Value: IScript);
begin
  FScript := Value;
end;

initialization
  FScriptEngineClasses := TList.Create;

finalization
  FreeAndNil(FScriptEngineClasses);
end.
