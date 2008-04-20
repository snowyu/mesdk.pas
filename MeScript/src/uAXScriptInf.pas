
{ Summary: the AX Script Interface Declaration

  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uAXScriptInf.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.
    * Contributor(s):


}

(*
The following information on IBindEventHandler might be useful to you.

I've just discovered, IBindEventHandler has no appication with IE (at least
IE5). Even more, all IE objects that support
  function <object>.<event>() { .. }
binding don't expose IBindEventHandler. Still note, complex things like this
  <script>
  function window.document.onkeydown() { alert("Blah") }
  </script>
do work!

My IBindEventHandler was never being queried for. It turns out, when JScript
sees
  function <object>.<event>() { alert("Blah") }
syntax, it understands it and merely assigns object.event, which is much the
same as:
  <object>.<event> = function() { alert("Blah") }

I implemented a special property per event (like IE does):

[propput] HRESULT event([in] IDispatch* handler)
[propget] HRESULT event([out, retval] IDispatch** handler)

and it works perfectly. You should call Invoke(DISPID_VALUE,...) on passed
IDispatch when the corresponding event occurs. Some time it's even easier and
more efficient than implementing standard IConnectionPointContainer schema. It
allows you to sink events from children objects as easy as:
  function <root>.<child1>.<child2>.<event>() { alert("Blah") }
with VBScript, you can do the same with
 sub handler
 end sub
 <root>.<child1>.<child2>.<event> = GetRef(handler);

Eric please, should IBindEventHandler be consdered as legacy interface?

Btw, new cool features are here for engines V5. Look:

****************
interface IActiveScriptProperty : IUnknown
{
  // NOTES:
  // * This is a generic information passing interface to allow
  //     the host to get and set pre-defined properties of the engine
  // * dwProperty must be a SCRIPTPROP_* value
  // * pvarIndex (when used) further identifies the dwProperty
  // * pvarValue is the value of the property, can be any VARIANT including
  //     binary data in a VT_BSTR, most common is VT_BOOL
        HRESULT GetProperty(
                [in] DWORD dwProperty,
                [in] VARIANT *pvarIndex,
                [out] VARIANT *pvarValue
        );
        HRESULT SetProperty(
                [in] DWORD dwProperty,
                [in] VARIANT *pvarIndex,
                [in] VARIANT *pvarValue
        );

}

/* Properties for IActiveScriptProperty */
#define SCRIPTPROP_NAME                     0x00000000
#define SCRIPTPROP_MAJORVERSION             0x00000001
#define SCRIPTPROP_MINORVERSION             0x00000002
#define SCRIPTPROP_BUILDNUMBER              0x00000003

#define SCRIPTPROP_DELAYEDEVENTSINKING      0x00001000
#define SCRIPTPROP_CATCHEXCEPTION           0x00001001

#define SCRIPTPROP_DEBUGGER                 0x00001100
#define SCRIPTPROP_JITDEBUG                 0x00001101

// These properties are defined and available, but are not
// officially supported.
#define SCRIPTPROP_HACK_FIBERSUPPORT        0x70000000
#define SCRIPTPROP_HACK_TRIDENTEVENTSINK    0x70000001

****************

I'm especially interested at SCRIPTPROP_DEBUGGER, SCRIPTPROP_DEBUGGER,
SCRIPTPROP_CATCHEXCEPTION.
Note also this:

****************

[
        object,
        uuid(1DC9CA50-06EF-11d2-8415-006008C3FBFC),
        pointer_default(unique)
]
interface ITridentEventSink : IUnknown
{
        HRESULT FireEvent(
                [in] LPCOLESTR pstrEvent,
                [in] DISPPARAMS *pdp,
                [out] VARIANT *pvarRes,
                [out] EXCEPINFO *pei
        );

}

These are NOT cool new features, these are hacks we added to the engine in
order to get performance features into IE and ASP without re-writing the
Windows Script Interfaces entirely.  DO NOT USE THEM.  As far as I know,
they will NEVER be documented or supported.  Their behaviour is entirely
dependent on my whims, and you know how capricious I can be.  ;-)  If by
some strange twist of fate we do decide to support these as public WSIs,
we'll add them to the doc then.  Until then, please don't muck with them as
we cannot guarantee that future script engines will be forward compatible
with any host that uses these interfaces.

In particular, ITridentEventSink is a hack that we added which allows
VBScript to walk the IE type info looking for events to bind much faster
than the traditional type info walking code.  We found that we had
situations where there would be a table with a thousand entries, say, and
we'd spend thirty, forty seconds just looking at every entry to see if there
were any events bound on it.  (JScript doesn't have this problem because
JScript doesn't have automagic event hookup, events must be hooked up
explicitly in the code or the HTML.)  This interface is a PRIVATE interface
between VBScript and the IE HTML rendering surface, DO NOT USE IT.  Pay no
attention to the man behind the curtain!



*)

unit uAXScriptInf;

{$I MeSetting.inc}

interface

uses
  Windows, ActiveX
  ;

{$Z4}

const
  SCATID_ActiveScript =               '{F0B7A1A1-9847-11cf-8F20-00805F2CD064}';
  SCATID_ActiveScriptParse =          '{F0B7A1A2-9847-11cf-8F20-00805F2CD064}';

  SCATID_VBScript          =          '{B54F3741-5B07-11CF-A4B0-00AA004A55E8}';
  SCATID_VBScriptEncode    =          '{B54F3743-5B07-11CF-A4B0-00AA004A55E8}';
  SCATID_JScript           =          '{F414C260-6AC0-11CF-B6D1-00AA00BBBB58}';
  SCATID_JScriptEncode     =          '{F414C262-6AC0-11CF-B6D1-00AA00BBBB58}';
  SCATID_PythonScript      =          '{DF630910-1C1D-11D0-AE36-8C0F5E000000}';
  SCATID_PerlScript        =          '{F8D77580-0F09-11D0-AA61-3C284E000000}';
  SCATID_CypressEnable     =          '{3C6F3220-4A4A-11D0-89EB-444553540001}';
  SCatID_JScriptNet        =          '{9888F5B2-0A2C-11D3-B354-00105A98B7CE}';

  SID_IActiveScript =                 '{BB1A2AE1-A4F9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptParse =            '{BB1A2AE2-A4F9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptParseProcedureOld ='{1CFF0050-6FDD-11d0-9328-00A0C90DCAA9}';
  SID_IActiveScriptParseProcedure =   '{AA5B6A80-B834-11d0-932F-00A0C90DCAA9}';
  SID_IActiveScriptSite =             '{DB01A1E3-A42B-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptSiteWindow =       '{D10F6761-83E9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptSiteInterruptPoll ='{539698A0-CDCA-11CF-A5EB-00AA0047A063}';
  SID_IActiveScriptError =            '{EAE1BA61-A4ED-11cf-8F20-00805F2CD064}';
  SID_IBindEventHandler =             '{63CDBCB0-C1B1-11d0-9336-00A0C90DCAA9}';
  SID_IActiveScriptStats =            '{B8DA6310-E19B-11d0-933C-00A0C90DCAA9}';

  SID_ProcessDebugManager  = '{78a51822-51f4-11d0-8f20-00805f2cd064}';
  SID_MachineDebugManager  ='{0C0A3666-30C9-11D0-8F20-00805F2CD064}';


  CATID_ActiveScript:                 TGUID = SCATID_ActiveScript;
  CATID_ActiveScriptParse:            TGUID = SCATID_ActiveScriptParse;
  IID_IActiveScript:                  TGUID = SID_IActiveScript;
  IID_IActiveScriptParse:             TGUID = SID_IActiveScriptParse;
  IID_IActiveScriptParseProcedureOld: TGUID = SID_IActiveScriptParseProcedureOld;
  IID_IActiveScriptParseProcedure:    TGUID = SID_IActiveScriptParseProcedure;
  IID_IActiveScriptSite:              TGUID = SID_IActiveScriptSite;
  IID_IActiveScriptSiteWindow:        TGUID = SID_IActiveScriptSiteWindow;
  IID_IActiveScriptSiteInterruptPoll: TGUID = SID_IActiveScriptSiteInterruptPoll;
  IID_IActiveScriptError:             TGUID = SID_IActiveScriptError;
  IID_IBindEventHandler:              TGUID = SID_IBindEventHandler;
  IID_IActiveScriptStats:             TGUID = SID_IActiveScriptStats;

// Constants used by ActiveX Scripting:
//

(* IActiveScript::AddNamedItem() input flags *)

  SCRIPTITEM_ISVISIBLE     = $00000002;
  SCRIPTITEM_ISSOURCE      = $00000004;
  SCRIPTITEM_GLOBALMEMBERS = $00000008;
  SCRIPTITEM_ISPERSISTENT  = $00000040;
  SCRIPTITEM_CODEONLY      = $00000200;
  SCRIPTITEM_NOCODE        = $00000400;
  SCRIPTITEM_ALL_FLAGS     =(SCRIPTITEM_ISSOURCE or
                             SCRIPTITEM_ISVISIBLE or
                             SCRIPTITEM_ISPERSISTENT or
                             SCRIPTITEM_GLOBALMEMBERS or
                             SCRIPTITEM_NOCODE or
                             SCRIPTITEM_CODEONLY);

(* IActiveScript::AddTypeLib() input flags *)

  SCRIPTTYPELIB_ISCONTROL    = $00000010;
  SCRIPTTYPELIB_ISPERSISTENT = $00000040;
  SCRIPTTYPELIB_ALL_FLAGS    = (SCRIPTTYPELIB_ISCONTROL or
                                SCRIPTTYPELIB_ISPERSISTENT);

(* IActiveScriptParse::AddScriptlet() and
   IActiveScriptParse::ParseScriptText() input flags *)

  SCRIPTTEXT_NULL              = $00000000;    { added for demo}
  {
  SCRIPTTEXT_DELAYEXECUTION will do nothing if you're evaluating an expression; if the SCRIPTTEST_ISEXPRESSION flag is set or the pvarResult parameter is non-NULL, the script code you pass in will be executed immediately if the script engine is at the very least initialized. 

The SCRIPTTEXT_DELAYEXECUTION flag causes the code you're
adding to the engine to be placed in queue for execution.  The code will not
be executed until the script engine's state is shifted from
SCRIPTSTATE_INITIALIZED to SCRIPTSTATE_STARTED.  This means that if you add
SCRIPTTEXT_DELAYEXECUTION code to a script engine in the SCRIPTSTATE_STARTED
state, the code will be put into a queue for execution, but it will not
immediatey execute.  The code will not run until you revert the engine state
to SCRIPTSTATE_INITIALIZED and back to SCRIPTSTATE_STARTED.  Of course, the
state switch to SCRIPTSTATE_INITIALIZED will reset the engine to a
pre-running state, and the subsequent switch to SCRIPTSTATE_STARTED will
cause all of the persistent code you've added so far to run from scratch. 

  }
  SCRIPTTEXT_DELAYEXECUTION    = $00000001;
  //Indicates that the script text should be visible (and, therefore, callable by name) as a global method in the name space of the script.
  SCRIPTTEXT_ISVISIBLE         = $00000002;
  //If the distinction between a computational expression and a statement is important but syntactically ambiguous in the script language, this flag specifies that the scriptlet is to be interpreted as an expression, rather than as a statement or list of statements. By default, statements are assumed unless the correct choice can be determined from the syntax of the scriptlet text.
  SCRIPTTEXT_ISEXPRESSION      = $00000020;
  //Indicates that the code added during this call should be saved if the scripting engine is saved (for example, through a call to IPersist*::Save), or if the scripting engine is reset by way of a transition back to the initialized state.
  SCRIPTTEXT_ISPERSISTENT      = $00000040;
  SCRIPTTEXT_HOSTMANAGESSOURCE = $00000080;
  SCRIPTTEXT_ALL_FLAGS         = (SCRIPTTEXT_DELAYEXECUTION or
                                  SCRIPTTEXT_ISVISIBLE or
                                  SCRIPTTEXT_ISEXPRESSION or
                                  SCRIPTTEXT_ISPERSISTENT or
                                  SCRIPTTEXT_HOSTMANAGESSOURCE);

(* IActiveScriptParseProcedure::ParseProcedureText() input flags *)

  SCRIPTPROC_HOSTMANAGESSOURCE = $00000080;
  SCRIPTPROC_IMPLICIT_THIS     = $00000100;
  SCRIPTPROC_IMPLICIT_PARENTS  = $00000200;
  SCRIPTPROC_ALL_FLAGS         = (SCRIPTPROC_HOSTMANAGESSOURCE or
                                  SCRIPTPROC_IMPLICIT_THIS or
                                  SCRIPTPROC_IMPLICIT_PARENTS);

(* IActiveScriptSite::GetItemInfo() input flags *)

  SCRIPTINFO_IUNKNOWN  = $00000001;
  SCRIPTINFO_ITYPEINFO = $00000002;
  SCRIPTINFO_ALL_FLAGS = (SCRIPTINFO_IUNKNOWN or
                          SCRIPTINFO_ITYPEINFO);

(* IActiveScript::Interrupt() Flags *)

  SCRIPTINTERRUPT_DEBUG          = $00000001;
  SCRIPTINTERRUPT_RAISEEXCEPTION = $00000002;
  SCRIPTINTERRUPT_ALL_FLAGS      = (SCRIPTINTERRUPT_DEBUG or
                                    SCRIPTINTERRUPT_RAISEEXCEPTION);

(* IActiveScriptStats::GetStat() values *)

  SCRIPTSTAT_STATEMENT_COUNT   = 1;
  SCRIPTSTAT_INSTRUCTION_COUNT = 2;
  SCRIPTSTAT_INTSTRUCTION_TIME = 3;
  SCRIPTSTAT_TOTAL_TIME        = 4;
  

const
  // Caller of interface may be untrusted
  INTERFACESAFE_FOR_UNTRUSTED_CALLER = $00000001;
  // Data passed into interface may be untrusted
  INTERFACESAFE_FOR_UNTRUSTED_DATA = $00000002;
  // Object knows to use IDispatchEx
  INTERFACE_USES_DISPEX = $00000004;
  // Object knows to use IInternetHostSecurityManager
  INTERFACE_USES_SECURITY_MANAGER = $00000008;

(* script state values *)

type
  tagSCRIPTSTATE = longword;
  SCRIPTSTATE = tagSCRIPTSTATE;
const
{
脚本引擎的各种状态：
    Uninitialized（未初始化） 这表明脚本引擎还没有准备好，通常，主机在脚本引擎离开这一状态前，要装入运行脚本所需的脚本和实体。
    Initialized（已初始化） 脚本已装入（通常用IPersist ），运行站点已设定（使用IActiveScriptSite），但脚本引擎还没有真正地与一个主机相联系。脚本引擎处于这种状态时，不能运行任何代码。一旦脚本引擎进入已启动状态，则由IActiveScriptParse.ParseScriptText()执行的代码将开始运行。
    Started（已启动）这是一种过渡状态。从这种状态使用IDispatch就能运行代码了。所有代码与对象一起装入。但是，脚本引擎尚未在脚本元素与对象事件之间建立连接。如果脚本运行到需要执行事件处理的地方，它就被阻塞（停止），直至脚本引擎完成所需的初始化。
    Connected（已连接） 脚本引擎做好了去执行需要由它来执行的每项任务的准备时，就是进入了这种状态。
    Disconnected（连接暂停）脚本引擎进入这一状态后，实际上脚本已暂停。脚本仍处于装入状态，所有连接依然存在，但脚本引擎却不准备回答主机的请求。脚本引擎可以在不丢失脚本当前运行位置的情况下，离开这一状态并返回已连接状态。
    Closed（关闭） 在这种状态中的脚本引擎，不再回答调用。发出IActiveScript.Close()调用后，就进入这一状态。
}
  SCRIPTSTATE_UNINITIALIZED = $00000000;
  SCRIPTSTATE_INITIALIZED   = $00000005;
  SCRIPTSTATE_STARTED       = $00000001;
  SCRIPTSTATE_CONNECTED     = $00000002;
  SCRIPTSTATE_DISCONNECTED  = $00000003;
  SCRIPTSTATE_CLOSED        = $00000004;

(* script thread state values *)

type
  tagSCRIPTTHREADSTATE = longword;
  SCRIPTTHREADSTATE = tagSCRIPTTHREADSTATE;
const
  SCRIPTTHREADSTATE_NOTINSCRIPT = $00000000;
  SCRIPTTHREADSTATE_RUNNING     = $00000001;

(* Thread IDs *)

type
  SCRIPTTHREADID = DWORD;
const
  SCRIPTTHREADID_CURRENT = SCRIPTTHREADID(-1);
  SCRIPTTHREADID_BASE    = SCRIPTTHREADID(-2);
  SCRIPTTHREADID_ALL     = SCRIPTTHREADID(-3);

// evaluate flags
  DEBUG_TEXT_ISEXPRESSION       = $00000001;
  DEBUG_TEXT_RETURNVALUE        = $00000002;
  DEBUG_TEXT_NOSIDEEFFECTS      = $00000004;
  DEBUG_TEXT_ALLOWBREAKPOINTS   = $00000008;
  DEBUG_TEXT_ALLOWERRORREPORT   = $00000010;

  DBGPROP_INFO_NAME       = $001;// init the bstrName field
  DBGPROP_INFO_TYPE       = $002; // init the bstrType field
  DBGPROP_INFO_VALUE      = $004; // init the bstrValue field
  DBGPROP_INFO_FULLNAME   = $020; // init the full name field
  DBGPROP_INFO_ATTRIBUTES = $008; // init the dwAttrib field
  DBGPROP_INFO_DEBUGPROP  = $010; // init the pDebugProp field
  DBGPROP_INFO_AUTOEXPAND = $8000000; // make the Value result auto-expand

  DBGPROP_INFO_STANDARD = DBGPROP_INFO_NAME or DBGPROP_INFO_TYPE or DBGPROP_INFO_VALUE or
                          DBGPROP_INFO_ATTRIBUTES;

  DBGPROP_INFO_ALL      = DBGPROP_INFO_NAME or DBGPROP_INFO_TYPE or DBGPROP_INFO_VALUE or
                          DBGPROP_INFO_FULLNAME or DBGPROP_INFO_ATTRIBUTES or DBGPROP_INFO_DEBUGPROP;

type
  TEXT_DOC_ATTR = DWORD;

const
  // Indicates that the document is read-only.
  TEXT_DOC_ATTR_READONLY = $00000001;

  // DEBUGGER_BLOCK
  // languages should break immediately with BREAKREASON_DEBUGGER_BLOCK
  APPBREAKFLAG_DEBUGGER_BLOCK= $00000001;

  // DEBUGGER_HALT
  // languages should break immediately with BREAKREASON_DEBUGGER_HALT
  APPBREAKFLAG_DEBUGGER_HALT= $00000002;

  // STEP
  // languages should break immediately in the stepping thread with BREAKREASON_STEP
  APPBREAKFLAG_STEP= $00010000;

  // NESTED - the application is in nested execution on a breakpoint
  APPBREAKFLAG_NESTED= $00020000;

  // STEP TYPES - defines whether we are stepping at source, bytecode, or machine level.
  APPBREAKFLAG_STEPTYPE_SOURCE   = $00000000;
  APPBREAKFLAG_STEPTYPE_BYTECODE = $00100000;
  APPBREAKFLAG_STEPTYPE_MACHINE  = $00200000;
  APPBREAKFLAG_STEPTYPE_MASK     = $00F00000;

  // BREAKPOINT IN_PROGRESS
  APPBREAKFLAG_IN_BREAKPOINT = $80000000;

  // -----------------------------------------------------------------------
  // OBJECT ATTRIBUTES
  DBGPROP_ATTRIB_NO_ATTRIB = $00000000;

  // ---------------------
  // Characteristics

  DBGPROP_ATTRIB_VALUE_IS_INVALID = $00000008; // the value in this slot is invalid
  DBGPROP_ATTRIB_VALUE_IS_EXPANDABLE =$00000010; // the object has children
  DBGPROP_ATTRIB_VALUE_READONLY = $00000800; // the value is read-only

  // Attributes about a slot's type
  // ---------------------
  // Common attributes
  // field access control

  DBGPROP_ATTRIB_ACCESS_PUBLIC = $00001000;
  DBGPROP_ATTRIB_ACCESS_PRIVATE = $00002000;
  DBGPROP_ATTRIB_ACCESS_PROTECTED = $00004000;
  DBGPROP_ATTRIB_ACCESS_FINAL = $00008000;

  // storage types

  DBGPROP_ATTRIB_STORAGE_GLOBAL = $00010000;
  DBGPROP_ATTRIB_STORAGE_STATIC = $00020000;
  DBGPROP_ATTRIB_STORAGE_FIELD = $00040000;

  // type modifiers

  DBGPROP_ATTRIB_STORAGE_VIRTUAL = $00080000; // this slot is virtual
  DBGPROP_ATTRIB_TYPE_IS_CONSTANT = $00100000; // this slot is a constant value
  DBGPROP_ATTRIB_TYPE_IS_SYNCHRONIZED =$00200000; // this slot is thread synchronized
  DBGPROP_ATTRIB_TYPE_IS_VOLATILE = $00400000; // this slot is volatile storage
  DBGPROP_ATTRIB_HAS_EXTENDED_ATTRIBS =$00800000; // has more attributes

  THREAD_STATE_RUNNING   = $00000001;
  THREAD_STATE_SUSPENDED = $00000002;
  THREAD_BLOCKED         = $00000004;
  THREAD_OUT_OF_CONTEXT  = $00000008;

{
Application break flags. Indicates the current debug state for the application
and the current thread. When certain of these bits are set (some are per-thread
and some are for all threads), language engines should break at the next
opportunity.
}
type
  TAPPBREAKFLAGS = DWORD;

  TTextDocAttr = DWORD;
  TSourceTextAttr = WORD;

type
  TDocumentNameType = (
    DOCUMENTNAMETYPE_APPNODE,// Gets the name as it appears in the app tree
    DOCUMENTNAMETYPE_TITLE,// Gets the name as it appears on the doc viewer title bar
    DOCUMENTNAMETYPE_FILE_TAIL,// Gets the filename without a path (for save as...)
    DOCUMENTNAMETYPE_URL// Gets the URL of the document, if any
  );

  TBreakPointState = (
    BREAKPOINT_DELETED,// The breakpoint no longer exists but there are still references
    BREAKPOINT_DISABLED,// The breakpoint exists but is disabled
    BREAKPOINT_ENABLED// The breakpoint exists and is enabled
  );

  TBreakResumeAction = (
    braAbort,// Abort the application
    braContinue,// Continue running
    braStepInto,// Step into a procedure
    braStepOver,// Step over a procedure
    braStepOut,// Step out of the current procedure
    braIgnore
  );

  TErrorResumeAction = (
    eraReexecute,// try executing the erroneous line again
    eraAbortAndReturnError,// let the language engine handle the error
    eraSkipError// resume execution from beyond the error
  );

  TBreakReason = (
    BREAKREASON_STEP,// Caused by the stepping mode
    BREAKREASON_BREAKPOINT,// Caused by an explicit breakpoint
    BREAKREASON_DEBUGGER_BLOCK,// Caused by another thread breaking
    BREAKREASON_HOST_INITIATED,// Caused by host requested break
    BREAKREASON_LANGUAGE_INITIATED,// Caused by a scripted break
    BREAKREASON_DEBUGGER_HALT,// Caused by debugger IDE requested break
    BREAKREASON_ERROR// Caused by an execution error
  );


  TDbgPropInfoFlags = DWORD; // Flags used to specify DebugPropertyInfo (and ExtendedDebugPropertyInfo) fields
  TDbgPropAttribFlags = DWORD;

{$IFDEF Compiler4_Up}
  {$IFDEF BCB4_UP}
type
  POleVariant = ^OleVariant;
  {$ENDIF}
{$ELSE}
type
  POleVariant = ^OleVariant;
{$ENDIF}

type
  IActiveScriptSite =           interface;
  IActiveScriptSiteWindow =     interface;
  IActiveScript =               interface;
  IActiveScriptParse =          interface;
  IActiveScriptParseProcedure = interface;
  IActiveScriptError =          interface;
  LPCOLESTR = PWideChar;

  IActiveScriptSite = interface(IUnknown)
    [SID_IActiveScriptSite]
    function GetLCID(out plcid: LCID): HResult; stdcall;
    function GetItemInfo(
      pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
    function GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    function OnScriptTerminate(
      var pvarResult: OleVariant;
      var pexcepinfo: EXCEPINFO): HResult; stdcall;
    function OnStateChange(ssScriptState: SCRIPTSTATE): HResult; stdcall;
    function OnScriptError(
      const pscripterror: IActiveScriptError): HResult; stdcall;
    function OnEnterScript: HResult; stdcall;
    function OnLeaveScript: HResult; stdcall;
  end;

  IActiveScriptError = interface(IUnknown)
    [SID_IActiveScriptError]
    function GetExceptionInfo(out pexcepinfo: EXCEPINFO): HResult; stdcall;
    function GetSourcePosition(
      out pdwSourceContext: DWORD;
      out pulLineNumber: ULONG;
      out plCharacterPosition: Integer): HResult; stdcall;
    function GetSourceLineText(out pbstrSourceLine: WideString): HResult; stdcall;
  end;

  IActiveScriptSiteWindow = interface(IUnknown)
    [SID_IActiveScriptSiteWindow]
    function GetWindow(out phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  end;

  IActiveScriptSiteInterruptPoll = interface(IUnknown)
    [SID_IActiveScriptSiteInterruptPoll]
    function QueryContinue: HResult; stdcall;
  end;

  IActiveScript = interface(IUnknown)
    [SID_IActiveScript]
    function SetScriptSite(const pass: IActiveScriptSite): HResult; stdcall;
    function GetScriptSite(
      const riid: TGUID;
      out ppvObject: Pointer): HResult; stdcall;
    function SetScriptState(ss: SCRIPTSTATE): HResult; stdcall;
    function GetScriptState(out pssState: SCRIPTSTATE): HResult; stdcall;
    function Close: HResult; stdcall;
    function AddNamedItem(
      pstrName: LPCOLESTR;
      dwFlags: DWORD): HResult; stdcall;
    function AddTypeLib(
      const rguidTypeLib: TGUID;
      dwMajor: DWORD;
      dwMinor: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function GetScriptDispatch(
      pstrItemName: LPCOLESTR;
      out ppdisp: IDispatch): HResult; stdcall;
    function GetCurrentScriptThreadID(
      out pstidThread: SCRIPTTHREADID): HResult; stdcall;
    function GetScriptThreadID(dwWin32ThreadId: DWORD;
      out pstidThread: SCRIPTTHREADID): HResult; stdcall;
    function GetScriptThreadState(
      stidThread: SCRIPTTHREADID;
      out pstsState: SCRIPTTHREADSTATE): HResult; stdcall;
    function InterruptScriptThread(
      stidThread: SCRIPTTHREADID;
      var pexcepinfo: EXCEPINFO;
      dwFlags: DWORD): HResult; stdcall;
    function Clone(out ppscript: IActiveScript): HResult; stdcall;
  end;

  IActiveScriptParse = interface(IUnknown)
    [SID_IActiveScriptParse]
    function InitNew: HResult; stdcall;
    {
    What is the best way to add event-handling code to the script engine?

Events can be synced from any object that has been added to IActiveScript::AddNamedItem with the SCRIPTITEM_ISSOURCE flag, and any immediate child object of such objects. So, for example, when
IE adds the window object with the SCRIPTITEM_ISSOURCE flag, the OnClick event of a button on that window object can automatically be synced.

To add an event handler for that child object, you use IActiveScriptParse::AddScriptlet like the following:

   pScriptEngineParse->AddScriptlet( 
   NULL,   /*pstrDefaultName*/
   L"MsgBox \"Hello\"",   /*pstrCode*/
   L"window",   /*pstrItemName*/
   L"button1",   /*pstrSubItemName*/
   L"OnClick",   /*pstrEventName*/
   NULL,   /*pstrDelimiter*/
   0,   /*dwSourceContextCookie/*
   ulStartingLineNumber,
   SCRIPTTEXT_ISPERSTENT,   /*dwFlags*/
   &bstrName, 
   &excepInfo );


When called like this, the script engine will call IActiveScriptSite::GetItemInfo() on the window object, and then IDispatch::GetIDsOfNames/Invoke on the button1 object. Then it will QI for IConnectionPointContainer and use Connection Points to sync the OnClick event.
    }
    function AddScriptlet(
      //a default name to associate with the scriptlet. If the scriptlet does not contain naming information (as in the ONCLICK example above), this name will be used to identify the scriptlet. If this parameter is NULL, the scripting engine manufactures a unique name, if necessary.
      pstrDefaultName: LPCOLESTR;
      //the scriptlet text 
      pstrCode: LPCOLESTR;
      //a buffer that contains the item name associated with this scriptlet. This parameter, in addition to pstrSubItemName, identifies the object for which the scriptlet is an event handler.
      pstrItemName: LPCOLESTR;
      //a buffer that contains the name of a subobject of the named item with which this scriptlet is associated; this name must be found in the named item's type information. This parameter is NULL if the scriptlet is to be associated with the named item instead of a subitem. This parameter, in addition to pstrItemName, identifies the specific object for which the scriptlet is an event handler.
      pstrSubItemName: LPCOLESTR;
      //a buffer that contains the name of the event for which the scriptlet is an event handler.
      pstrEventName: LPCOLESTR;
      //the end-of-scriptlet delimiter. When the pstrCode parameter is parsed from a stream of text, the host typically uses a delimiter, such as two single quotation marks (''), to detect the end of the scriptlet. This parameter specifies the delimiter that the host used, allowing the scripting engine to provide some conditional primitive preprocessing (for example, replacing a single quotation mark ['] with two single quotation marks for use as a delimiter). Exactly how (and if) the scripting engine makes use of this information depends on the scripting engine. Set this parameter to NULL if the host did not use a delimiter to mark the end of the scriptlet.
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      //Actual name used to identify the scriptlet. This is to be in order of preference: a name explicitly specified in the scriptlet text, the default name provided in pstrDefaultName, or a unique name synthesized by the scripting engine.
      out pbstrName: WideString;
      out pexcepinfo: EXCEPINFO): HResult; stdcall;
    {
    Returns one of the following values:
    Return Value 	     Meaning
    S_OK               Success.
    DISP_E_EXCEPTION   An exception occurred in the processing of the scriptlet. The pexcepinfo parameter contains information about the exception.
    E_INVALIDARG       An argument was invalid.
    E_POINTER          An invalid pointer was specified.
    E_NOTIMPL          This method is not supported. The scripting engine does not support run-time evaluation of expressions or statements.
    E_UNEXPECTED       The call was not expected (for example, the scripting engine is in the uninitialized or closed state, or the SCRIPTTEXT_ISEXPRESSION flag was set and the scripting engine is in the initialized state).
    OLESCRIPT_E_SYNTAX An unspecified syntax error occurred in the scriptlet. 

    Remarks
    
    If the scripting engine is in the initialized state, no code will actually be evaluated during this call; rather, such code is queued and executed when the scripting engine is transitioned into (or through) the started state. Because execution is not allowed in the initialized state, it is an error to call this method with the SCRIPTTEXT_ISEXPRESSION flag when in the initialized state.
    
    The scriptlet can be an expression, a list of statements, or anything allowed by the script language. For example, this method is used in the evaluation of the HTML <SCRIPT> tag, which allows statements to be executed as the HTML page is being constructed, rather than just compiling them into the script state.
    
    The code passed to this method must be a valid, complete portion of code. For example, in VBScript it is illegal to call this method once with Sub Function(x) and then a second time with End Sub. The parser must not wait for the second call to complete the subroutine, but rather must generate a parse error because a subroutine declaration was started but not completed. 

    }
    function ParseScriptText(
      //scriptlet text
      pstrCode: LPCOLESTR;
      //item name that gives the context in which the scriptlet is to be evaluated. If this parameter is NULL, the code is evaluated in the scripting engine's global context.
      pstrItemName: LPCOLESTR;
      //debugging context. This object is reserved for use in a debugging environment, where such a context may be provided by the debugger to represent an active run-time context. If this parameter is NULL, the engine uses pstrItemName to identify the context.
      const punkContext: IUnknown;
      //end-of-scriptlet delimiter. When pstrCode is parsed from a stream of text, the host typically uses a delimiter, such as two single quotation marks (''), to detect the end of the scriptlet. This parameter specifies the delimiter that the host used, allowing the scripting engine to provide some conditional primitive preprocessing (for example, replacing a single quotation mark ['] with two single quotation marks for use as a delimiter). Exactly how (and if) the scripting engine makes use of this information depends on the scripting engine. Set this parameter to NULL if the host did not use a delimiter to mark the end of the scriptlet.
      pstrDelimiter: LPCOLESTR;
      //Application-defined value that is used for debugging purposes.
      dwSourceContextCookie: DWORD;
      //starting line of the script. Zero-based value that specifies which line the parsing will begin at.
      ulStartingLineNumber: ULONG;
      //scriptlet flags. Flags associated with the scriptlet. Can be a combination of these values:SCRIPTTEXT_ISEXPRESSION,SCRIPTTEXT_ISPERSISTENT,SCRIPTTEXT_ISVISIBLE
      dwFlags: DWORD;
      //buffer for results. Address of a buffer that receives the results of scriptlet processing, or NULL if the caller expects no result (that is, the SCRIPTTEXT_ISEXPRESSION value is not set).
      out pvarResult: OleVariant;
      //buffer for error data. Address of a structure that receives exception information. This structure is filled if IActiveScriptParse::ParseScriptText returns DISP_E_EXCEPTION.
      out pexcepinfo: EXCEPINFO): HResult; stdcall;
  end;

  IActiveScriptParseProcedureOld = interface(IUnknown)
    [SID_IActiveScriptParseProcedureOld]
    function ParseProcedureText(
      pstrCode: LPCOLESTR;
      pstrFormalParams: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out ppdisp: IDispatch): HResult; stdcall;
  end;

  IActiveScriptParseProcedure = interface(IUnknown)
    [SID_IActiveScriptParseProcedure]
    function ParseProcedureText(
      pstrCode: LPCOLESTR;
      pstrFormalParams: LPCOLESTR;
      pstrProcedureName: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out ppdisp: IDispatch): HResult; stdcall;
  end;

  IBindEventHandler = interface(IUnknown)
    [SID_IBindEventHandler]
    function BindHandler(
      pstrEvent: LPCOLESTR;
      const pdisp: IDispatch): HResult; stdcall;
  end;

  IActiveScriptStats = interface(IUnknown)
    [SID_IActiveScriptStats]
    function GetStat(
      stid: DWORD;
      out pluHi: ULONG;
      out pluLo: ULONG): HResult; stdcall;
    function GetStatEx(
      const guid: TGUID;
      out pluHi: ULONG;
      out pluLo: ULONG): HResult; stdcall;
    function ResetStats: HResult; stdcall;
  end;


  IDebugApplication = interface;
  IDebugDocumentHost = interface;
  IDebugProperty = interface;
  IDebugApplicationNode = interface;
  IEnumDebugCodeContexts = interface;
  IDebugDocumentContext = interface;

  IDebugDocumentHelper = interface(IUnknown)
  ['{51973C26-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Initialize a debug doc helper with the given name and
    // initial attributes.
    //
    // Note: The document will not actually appear in the tree
    // until Attach is called.
    function Init(const pda : IDebugApplication; const pszShortName : PWideChar;
                  const pszLongName : PWideChar; const docAttr : TTextDocAttr) : HRESULT; stdcall;

    // Add the document to the doc tree, using pddhParent as the parent.
    // If the ppdhParent is NULL, the document will be top-level.
    function Attach(const pddhParent : IDebugDocumentHelper) : HRESULT; stdcall;

    // Remove the document from the doc tree.
    function Detach : HRESULT; stdcall;

    // Add the given set of unicode characters to end of the document.
    // (This will generate IDebugDocumentTextEvent notifications.)
    //
    // If this method is called after AddDeferredText has been called,
    // E_FAIL will be returned.
    function AddUnicodeText(const pszText : PWideChar) : HRESULT; stdcall;

    // Add the given set of DBCS characters to end of the document.
    // (This will generate IDebugDocumentTextEvent notifications.)
    //
    // If this method is called after AddDeferredText has been called,
    // E_FAIL will be returned.
    function AddDBCSText(const pszText : PWideChar): HRESULT; stdcall;

    // Set the DebugDocumentHost interface.
    // If provided, this interface will be used for
    // smart-host syntax coloring, fetching deferred text, and returning
    // controlling unknowns for newly created document contexts.
    function SetDebugDocumentHost(const pddh : IDebugDocumentHost) : HRESULT; stdcall;

    // Notify the helper that the given text is available, but don't actually
    // provide the characters.
    // This allows the host to defer providing the characters unless they
    // are actually needed, while still allowing the helper to generate
    // accurate notifications and size information.
    //
    // dwTextStartCookie is a cookie, defined by the host, that represents
    // the starting position of the text. (For example, in a host that
    // represents text in DBCS, the cookie could be a byte offset.)
    // This cookie will be provided in subsequent calls to GetText.
    //
    // NB: It is assumed that a single call to
    // GetText can get characters from multiple calls to AddDeferredText.
    // The helper classes may also ask for the same range of deferred
    // characters more than once.
    //
    // It is an error to mix calls to AddDeferredText with calls to
    // AddUnicodeText or AddDBCSText-- Doing so will cause E_FAIL to be
    // returned.

    function AddDeferredText(const cChars : ULONG;// number of (Unicode) characters to add
             const dwTextStartCookie : DWORD) : HRESULT; stdcall; // host-defined cookie representing the starting position of the text.

    // Notify the helper that a particular range of
    // characters is a script block handled by the given script engine.
    // All syntax coloring and code context lookups for that range will be
    // delegated to that script engine.
    //
    // This method would be used by a smart host whose documents contained
    // embedded script blocks, or by a language engine containing embedded
    // scripts for other languages.
    //
    // DefineScriptBlock should be called after the text has been
    // added (via AddDBCSText, etc) but before the script script block
    // has been parsed (via IActiveScriptParse).

    function DefineScriptBlock(const ulCharOffset, cChars : ULONG; const pas : IActiveScript;
                               const fScriptlet : BOOL;out pdwSourceContext : DWORD) : HRESULT; stdcall;

    // Set the default attribute to use for text that is not in a
    // script block. (If not explicitly set, the default attributes for
    // text outside of a script block is SOURCETEXT_ATTR_NONSOURCE.)
    //
    // This would allow, for example, for text outside of script blocks
    // to be colored grey and marked read-only.

    function SetDefaultTextAttr(staTextAttr : TSourceTextAttr) : HRESULT; stdcall;

    // Explicilty set the attributes on a range of text, overriding any
    // other attributes on that text.
    //
    // It is an error to set the attributes on a text range that has not
    // yet been added using AddText.

    function SetTextAttributes(const ulCharOffset, cChars : ULONG; const pstaTextAttr : TSourceTextAttr) : HRESULT; stdcall;

    // Set a new long name for the document
    function SetLongName(const pszLongName : PWideChar) : HRESULT; stdcall;

    // Set a new short name for the document
    function SetShortName(const pszShortName : PWideChar) : HRESULT; stdcall;

    // Define a new set of document attributes
    function SetDocumentAttr(const pszAttributes : TTextDocAttr) : HRESULT; stdcall;

    // Return the debug application node corresponding to this document
    function GetDebugApplicationNode(out ppdan : IDebugApplicationNode) : HRESULT; stdcall;

    // Once a script block has been defined, this method allows the
    // associate range and script engine to be retrieved.
    function GetScriptBlockInfo(const dwSourceContext : DWORD;out ppasd : IActiveScript;
                                out piCharPos : ULONG;out pcChars : ULONG) : HRESULT; stdcall;

    // Allows the host to create a new debug document context
    function CreateDebugDocumentContext(const iCharPos, cChars : ULONG;
                out ppddc : IDebugDocumentContext) : HRESULT; stdcall;

    // Bring this document to the top in the debugger UI.
    // If the debugger isn't started already, start it now.
    function BringDocumentToTop : HRESULT; stdcall;

    // Bring the given context in this document to the top
    // in the debugger UI.
    function BringDocumentContextToTop(pddc : IDebugDocumentContext) : HRESULT; stdcall;
  end;

  IProcessDebugManager = interface(IUnknown)
  ['{51973C2f-CB0C-11d0-B5C9-00A0244A0E7A}']

    // Creates a new debug application object. The new object is not added to the
    // running application list and has no name.
    function CreateApplication(out ppda : IDebugApplication) : HRESULT; stdcall;

    // Returns a default application object for the current process, creating one and adding
    // it to the running application list if necessary. Language engines should use this
    // application if they are running on a host that does not provide an application.
    function GetDefaultApplication(out ppda : IDebugApplication) : HRESULT; stdcall;

    // Adds an application to the running application list in the machine debug manager.
    function AddApplication(const pda : IDebugApplication;
              out pdwAppCookie : DWORD // Returns a cookie used to remove the application from the machine debug manager.
              ) : HRESULT; stdcall;

    // Removes an application from the running application list.
    function RemoveApplication(const dwAppCookie : DWORD) : HRESULT; stdcall;// The cookie provided by AddApplication.

    function CreateDebugDocumentHelper(const punkOuter : IUnknown; out pddh : IDebugDocumentHelper) : HRESULT; stdcall;
  end;

  TDebugPropertyInfo = packed record
    m_dwValidFields : TDbgPropInfoFlags; // which DebugPropertyInfo fields were successfully initialized
    m_bstrName     : TBSTR; // property name
    m_bstrType     : TBSTR; // property type, as formatted string
    m_bstrValue    : TBSTR; // property value, as formatted string
    m_bstrFullName : TBSTR; // property's full name, like pObject->m_fFlag
    m_dwAttrib     : TDbgPropAttribFlags; // property attributes
    m_pDebugProp   : IDebugProperty; // IDebugProperty object corresponding to this DebugPropertyInfo
  end;

  PDebugPropertyInfo = ^TDebugPropertyInfo;

  IEnumDebugPropertyInfo = interface (IUnknown)
  ['{51973C51-CB0C-11d0-B5C9-00A0244A0E7A}']
    function Next(const celt : ULONG;
                  out pi : TDebugPropertyInfo;
                  out pceltFetched : ULONG) : HRESULT; stdcall;
    function Skip(const celt : ULONG) : HRESULT; stdcall;
    function Reset : HRESULT; stdcall;
    function Clone(out ppepi : IEnumDebugPropertyInfo) : HRESULT; stdcall;
    function GetCount(out pcelt : ULONG) : HRESULT; stdcall;
  end;

  IDebugProperty = interface(IUnknown)
  ['{51973C50-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Get Information for this object
    // By setting various PROPERTY_INFO_FLAGS, any subset of the basic info
    // contained in DebugPropertyInfo can be fetched
    function GetPropertyInfo(const dwFieldSpec : TDBGPROPINFOFLAGS;
             const nRadix : UINT;out pPropertyInfo : TDebugPropertyInfo) : HRESULT; stdcall;

    // Get ExtendedInfo for this object. This API only exists for the purpose
    // of retrieving info that does not lend itself to being retrieved via the
    // standard means (i.e. using DebugPropertyInfo)
    //
    // An array of GUIDs and result VARIANTs is passed so that multiple items
    // of extended info can be fetched at the same time. If a variant cannot
    // be initialized for some reason, the vt field should be set to VT_ERROR.

    // The currently defined extended info guids are described below.
    // A QI is typically required to obtain interfaces on the right from
    // IUnknowns in the variant.

    // GUID VALUE
    //
    // guidDefinitionContext IDebugDocumentContext2
    // ISSUE: Add additional GUIDS, such as:
    // <guidSomeRandomBSTR> BSTR
    // <guidSomeRandomI4> I4
    function GetExtendedInfo(const cInfos : ULONG; const rgguidExtendedInfo : TGUID;
                 out rgvar : POleVariant) : HRESULT; stdcall;


    // Set the value of this object as a string
    function SetValueAsString(const pszValue : PWideChar;const nRadix : UINT) : HRESULT; stdcall;

    // Get enumerator for props of members
    function EnumMembers(const dwFieldSpec : TDBGPROPINFOFLAGS; const nRadix : UINT;
                const refiid : TCLSID; out ppepi : IEnumDebugPropertyInfo) : HRESULT; stdcall;

    // Get the parent property
    function GetParent(out ppDebugProp : IDebugProperty) : HRESULT; stdcall;
  end;

  IDebugDocumentInfo = interface(IUnknown)
  ['{51973C1f-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Returns the specified name for the document. If the indicated name is
    // not known, E_FAIL is returned.
    function GetName(const dnt : TDOCUMENTNAMETYPE; out pbstrName:WideString): HRESULT; stdcall;

    // Returns a CLSID describing the document type. This allows the debugger IDE
    // to host custom viewers for this document. returns CLSID_NULL if this document
    // does not have viewable data.
    function GetDocumentClassId(out pclsidDocument : TCLSID): HRESULT; stdcall;
  end;

  //The base interface to all debug documents.
  IDebugDocument = interface(IDebugDocumentInfo)
  ['{51973C21-CB0C-11d0-B5C9-00A0244A0E7A}']
  end;

  IDebugDocumentContext = interface (IUnknown)
  ['{51973C28-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Returns the document that contains this context.
    function GetDocument(out ppsd : IDebugDocument) : HRESULT; stdcall;

    // Enumerates the code contexts associated with this document context. Generally there will
    // only be one code context but there are important exceptions, such as include file
    // or templates (in C++).
    function EnumCodeContexts(out ppescc : IEnumDebugCodeContexts) : HRESULT; stdcall;
  end;

  IDebugCodeContext = interface(IUnknown)
  ['{51973C13-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Returns the document context associated with this code context.
    //
    // Note: For text documents, the character-position
    // range should include the text for the entire statement. This allows the debugger IDE
    // to hilight the current source statement.
    function GetDocumentContext(out ppsc : IDebugDocumentContext) : HRESULT; stdcall;

    // Sets or clears a breakpoint at this code context.
    function SetBreakPoint(const bps : TBreakPointState) : HRESULT; stdcall;
  end;

  IEnumDebugCodeContexts = interface (IUnknown)
  ['{51973C1d-CB0C-11d0-B5C9-00A0244A0E7A}']
    function Next(const celt : ULONG;
                  out pscc : IDebugCodeContext;
                  out pceltFetched : ULONG) : HRESULT; stdcall;
    function Skip(const celt : ULONG) : HRESULT; stdcall;
    function Reset : HRESULT; stdcall;
    function Clone(out ppescc : IEnumDebugCodeContexts) : HRESULT; stdcall;
  end;

  IRemoteDebugApplicationThread = interface;
  IActiveScriptErrorDebug = interface;

  IApplicationDebugger = interface(IUnknown)
  ['{51973C2a-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Indicates if the debugger is alive. Should always return S_OK. If the debugger
    // has rudely shut down COM will return an error from the marshalling proxy.
    function QueryAlive : HRESULT; stdcall;

    // Provides a mechanism for hosts and language engines running out-of-process to the
    // debugger to create objects in the debugger process. This can be used for any purpose,
    // including extending the debugger UI. This method simply delegates to CoCreateInstance.
    function CreateInstanceAtDebugger(
      const rclsid : TCLSID;// Class identifier (CLSID) of the object
      const pUnkOuter : IUnknown; // Object is or isn't part of an aggregate
      const dwClsContext : DWORD; // Context for running executable code
      const riid : TIID; // Interface identifier
      out   ppvObject : IUnknown) : HRESULT; stdcall;

    // Points to requested interface pointer
    // This method is called when IDebugApplication::DebugOutput is called. The debugger
    // can use this to display the string in an output window.
    function onDebugOutput(const pstr : PWideChar) : HRESULT; stdcall;

    // This method is called when a breakpoint is hit. The application will remain
    // suspended until the debugger IDE calls IDebugApplication::ResumeFromBreakPoint.
    function onHandleBreakPoint(
    // Indicates the thread in which the breakpoint occured.
      const prpt : IRemoteDebugApplicationThread;
    // Indicates the reason for the breakpoint.
      const br : TBreakReason;
    // optional runtime error info (for when br == BREAKREASON_ERROR)
      const pError : IActiveScriptErrorDebug) : HRESULT; stdcall;

    // This method is called when IDebugApplication::Close is called.
    function onClose : HRESULT; stdcall;

    // Handle a custom event.
    // The semantics of the GUID and IUnknown are entirely application/debugger defined
    // This method may return E_NOTIMPL.
    function onDebuggerEvent(const riid : TIID; const punk : IUnknown) : HRESULT; stdcall;
  end;

  IEnumRemoteDebugApplicationThreads = interface (IUnknown)
  ['{51973C3c-CB0C-11d0-B5C9-00A0244A0E7A}']
    function Next(const celt : ULONG; out pprdat : IRemoteDebugApplicationThread;
                out pceltFetched : ULONG) : HRESULT; stdcall;
    function Skip(const celt : ULONG) : HRESULT; stdcall;
    function Reset : HRESULT; stdcall;
    function Clone(out pperdat : IEnumRemoteDebugApplicationThreads) : HRESULT; stdcall;
  end;

  IDebugExpressionCallBack = interface (IUnknown)
  ['{51973C16-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Indicates that the expression evaluation is complete. Note that
    // IDebugExpression::GetResultAsString can be called from within this event
    // handler.
    function onComplete : HRESULT; stdcall;
  end;

  IDebugExpression = interface(IUnknown)
  ['{51973C14-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Begins the evaluation of the expression.
    function Start(
      // Provides an event driven means for indicating that the expression evaluation
      // is complete. If NULL, no events will be fired and the client will need to
      // poll the expression state using QueryIsComplete.
      const pdecb : IDebugExpressionCallBack) : HRESULT; stdcall;

    // Aborts the expression. Evaluation of an expression in progress will be stopped
    // at the earliest opportunity. If the expression is actually aborted, GetResultAsString
    // will return E_ABORT as phrResult.
    function Abort : HRESULT; stdcall;

    // Returns S_FALSE if the operation is still pending.
    // Returns S_OK if the operation is complete.
    function QueryIsComplete : HRESULT; stdcall;

    // Returns the result of the expression evaluation as a string and an HRESULT. Returns
    // E_PENDING if the operation is still pending. Returns S_OK and E_ABORT in phrResult
    // when the operation was aborted with Abort.
    function GetResultAsString(out phrResult : HRESULT; out pbstrResult : TBSTR) : HRESULT; stdcall;

    // Returns the result of the expression evaluation as an
    // IDebugProperty and an HRESULT. Returns
    // E_PENDING if the operation is still pending. Returns S_OK and E_ABORT in phrResult
    // when the operation was aborted with Abort.
    function GetResultAsDebugProperty(out phrResult : HRESULT; out ppdp : IDebugProperty) : HRESULT; stdcall;
  end;

  IDebugExpressionContext = interface(IUnknown)
  ['{51973C15-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Creates an IDebugExpression for the specified text.
    function ParseLanguageText(
      // Provides the text of the expression or statement(s).
      const pstrCode : PWideChar;
      // Radix to use
      const nRadix : UINT;
      // See IActiveScriptParse::ParseScriptText
      const pstrDelimiter : PWideChar;
      // See above flags.
      const dwFlags : DWORD;
      // Returns the IDebugExpression for the given text.
      out ppe : IDebugExpression) : HRESULT; stdcall;
  end;

  IEnumDebugExpressionContexts  = interface(IUnknown)
  ['{51973C40-CB0C-11d0-B5C9-00A0244A0E7A}']
    function Next(const celt : ULONG;out ppdec : IDebugExpressionContext;out pceltFetched : ULONG) : HRESULT; stdcall;
    function Skip(const celt : ULONG) : HRESULT; stdcall;
    function Reset : HRESULT; stdcall;
    function Clone(out ppedec : IEnumDebugExpressionContexts) : HRESULT; stdcall;
  end;

  IRemoteDebugApplication = interface(IUnknown)
  ['{51973C30-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Continue an application which is currently in a breakpoint.
    function ResumeFromBreakPoint(
      // For stepping modes, the thread which is to be affected by the stepping mode.
      const prptFocus : IRemoteDebugApplicationThread;
      // The action to take (step mode, etc.) upon resuming the application
      const bra : TBreakResumeAction;
      // the action to take in the case that we stopped because of an error
      const era : TErrorResumeAction) : HRESULT; stdcall;

    // Causes the application to break into the debugger at the earliest opportunity. Note
    // that a long time may elapse before the application actually breaks, particularly if
    // the application is not currently executing script code.
    function CauseBreak : HRESULT; stdcall;

    // Connects a debugger to the application. Only one debugger may be connected at a
    // time; this method fails if there is already a debugger connected
    function ConnectDebugger(const pad : IApplicationDebugger) : HRESULT; stdcall;

    // Disconnects the current debugger from the application.
    function DisconnectDebugger : HRESULT; stdcall;

    // Returns the current debugger connected to the application.
    function GetDebugger(out pad : IApplicationDebugger) : HRESULT; stdcall;

    // Provides a mechanism for the debugger IDE, running out-of-process to the
    // application, to create objects in the application process.
    // This method simply delegates to CoCreateInstance.
    function CreateInstanceAtApplication(
      const rclsid : TCLSID;// Class identifier (CLSID) of the object
                            // Note: This parameter may have to be removed.
      const pUnkOuter : IUnknown;// Object is or isn't part of an aggregate
      const dwClsContext : DWORD;// Context for running executable code
      const riid : TIID;// Interface identifier
      out ppvObject : IUnknown) : HRESULT; stdcall;

    // Points to requested interface pointer
    // Indicates if the application is alive. Should always return S_OK. If the application
    // process has rudely shut down COM will return an error from the marshalling proxy.
    function QueryAlive : HRESULT; stdcall;

    // Enumerates all threads known to be associated with the application.
    // New threads may be added at any time.
    function EnumThreads(out pperdat : IEnumRemoteDebugApplicationThreads) : HRESULT; stdcall;

    // Returns the application node under which all nodes associated with the
    // application are added.
    function GetName(out pbstrName:WideString) : HRESULT; stdcall;

    // Returns a node for the application
    function GetRootNode(out ppdanRoot : IDebugApplicationNode) : HRESULT; stdcall;

    // Returns an enumerator that lists the global expression
    // contexts for all languages running in this application
    function EnumGlobalExpressionContexts (out ppedec : IEnumDebugExpressionContexts) : HRESULT; stdcall;
  end;

  IDebugStackFrame = interface;

  TDebugStackFrameDescriptor = record
    pdsf      : IDebugStackFrame;
    dwMin     : DWORD;
    dwLim     : DWORD;
    fFinal    : BOOL;
    punkFinal : IUnknown;
  end;

  IEnumDebugStackFrames = interface;

  IRemoteDebugApplicationThread = interface(IUnknown)
  ['{51973C37-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Returns an operating system dependent identifier associated with the thread.
    // Note: The returned value does not need to be unique across machines.
    function GetSystemThreadId(out dwThreadId : DWORD) : HRESULT; stdcall;

    // Returns the application object associated with the thread.
    function GetApplication(out pprda : IRemoteDebugApplication) : HRESULT; stdcall;

    // Returns an enumerator for the stack frames associated with the thread. Can only
    // be called when in a breakpoint. The stack frame enumerator enumerates stack frames
    // in the most recently called order.
    function EnumStackFrames(out ppedsf : IEnumDebugStackFrames) : HRESULT; stdcall;

    function GetDescription(out pbstrDescription, pbstrState : TBSTR) : HRESULT; stdcall;

    // Forces execution to continue as close as possible to the
    // given code context, in the context of the given frame.
    // Either of these arguments may be NULL, representing the
    // current frame or context.
    function SetNextStatement (const pStackFrame : IDebugStackFrame;
                               const pCodeContext :IDebugCodeContext) : HRESULT; stdcall;

    // returns the current state of the thread
    function  GetState (out pState : DWORD) : HRESULT; stdcall;

    // suspends the thread (increments the suspend count)
    function Suspend (out pdwCount : DWORD) : HRESULT; stdcall;

    // resumes the thread (decrements the suspend count)
    function Resume (out pdwCount : DWORD) : HRESULT; stdcall;

    // returns the current suspend count of the thread
    function GetSuspendCount (out pdwCount : DWORD) : HRESULT; stdcall;
  end;

  IDebugThreadCall = interface(IUnknown)
  ['{51973C36-CB0C-11d0-B5C9-00A0244A0E7A}']
    function ThreadCallHandler(const dwParam1, dwParam2, dwParam3 : DWORD) : HRESULT; stdcall;
  end;


  IDebugApplicationThread = interface(IRemoteDebugApplicationThread)
  ['{51973C38-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Provides a mechanism for the caller to run code in another thread. This is generally
    // used so that language engines and hosts can implement free threaded objects on top
    // of their single threaded implementations.
    function SynchronousCallIntoThread(
      // The interface to be called back in the target thread.
      const pstcb : IDebugThreadCall;
      // Three arguments passed to the IDebugThreadCall.
      const dwParam1 : DWORD;
      const dwParam2 : DWORD;
      const dwParam3 : DWORD) : HRESULT; stdcall;

    // Returns S_OK when this is the currently running thread. Otherwise S_FALSE is returned.
    function QueryIsCurrentThread : HRESULT; stdcall;

    // Returns S_OK when this is the debugger thread. Otherwise, returns S_FALSE.
    function QueryIsDebuggerThread : HRESULT; stdcall;
    function SetDescription(const pstrDescription : PWideChar) : HRESULT; stdcall;
    function SetStateString(const pstrState : PWideChar) : HRESULT; stdcall;
  end;


  IDebugStackFrame = interface (IUnknown)
  ['{51973C17-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Returns the current code context associated with the stack frame.
    function GetCodeContext(out ppcc : IDebugCodeContext) : HRESULT; stdcall;

    // Returns a short or long textual description of the stack frame.
    // Normally, when fLong if false, this will provide only the name of the
    // function associated with the stack frame. When fLong is true it may
    // also provide the parameter(s) to the function or whatever else is
    // relevant.

    function GetDescriptionString(const fLong : BOOL;out pbstrDescription : WideString) : HRESULT; stdcall;

    // Returns a short or long textual description of the language. When fLong
    // is false, just the language name should be provided, eg, "Pascal". When
    // fLong is true a full product description may be provided, eg,
    // "Gnat Software's Flaming Pascal v3.72".
    function GetLanguageString(const fLong : BOOL;out pbstrLanguage : TBSTR) : HRESULT; stdcall;

    // Returns the thread associated with this stack frame.
    function GetThread(out ppat : IDebugApplicationThread) : HRESULT; stdcall;

    // Returns a property browser for the current frame (locals, etc.)
    function GetDebugProperty(out ppDebugProp : IDebugProperty) : HRESULT; stdcall;
  end;


  IActiveScriptErrorDebug = interface(IActiveScriptError)
  ['{51973C12-CB0C-11d0-B5C9-00A0244A0E7A}']
  // Provides the document context for the associated error. The character-position range
  // should include the entire offending text.
    function GetDocumentContext(out ppssc : IDebugDocumentContext) : HRESULT; stdcall;

  // For runtime errors, provides the stack frame that is in effect.
    function GetStackFrame(out ppdsf : IDebugStackFrame) : HRESULT; stdcall;
  end;


  IActiveScriptDebug = interface(IUnknown)
  ['{51973C10-CB0C-11d0-B5C9-00A0244A0E7A}']
  // Returns the text attributes for an arbitrary block of script text. Smart hosts
  // use this call to delegate GetText calls made on their IDebugDocumentText.
    function GetScriptTextAttributes(
           // The script block text. This string need not be null terminated.
             const Code : PWideChar;
           // The number of characters in the script block text.
             const NumCodeChars : ULONG;
           // See IActiveScriptParse::ParseScriptText for a description of this argument.
             const pstrDelimiter : PWideChar;
           // See IActiveScriptParse::ParseScriptText for a description of this argument.
             const dwFlags : DWORD;
           // Buffer to contain the returned attributes.
             var pattr) : HRESULT; stdcall;


// Returns the text attributes for an arbitrary scriptlet. Smart hosts
// use this call to delegate GetText calls made on their IDebugDocumentText.
// Note: this call is provided because scriptlets tend to be expressions and
// may have a different syntax than a script block. For many languages the implementation
// will be identical to GetScriptTextAttributes.

  function GetScriptletTextAttributes(
           // The script block text. This string need not be null terminated.
             const Code : PWideChar;

           // The number of characters in the script block text.
             const NumCodeChars : ULONG;

           // See IActiveScriptParse::AddScriptlet for a description of this argument.
             const pstrDelimiter : PWideChar;

           // See IActiveScriptParse::AddScriptlet for a description of this argument.
             const dwFlags : DWORD;

           // Buffer to contain the returned attributes.
              out pattr) : HRESULT; stdcall;

    // Used by the smart host to delegate IDebugDocumentContext::EnumDebugCodeContexts.
    function EnumCodeContextsOfPosition(
           // As provided to IActiveScriptParse::ParseScriptText
           // or IActiveScriptParse::AddScriptlet
              const dwSourceContext : DWORD;
           // character offset relative
           // to start of script text
              const uCharacterOffset : ULONG;
           // Number of characters in context
              const NumChars : ULONG;
           // Returns an enumerator of code contexts.
              out ppescc : IEnumDebugCodeContexts) : HRESULT; stdcall;
  end;

  IEnumDebugApplicationNodes = interface (IUnknown)
  ['{51973C3a-CB0C-11d0-B5C9-00A0244A0E7A}']
    function Next(const celt : ULONG; out pprddp : IDebugApplicationNode;out pceltFetched : ULONG) : HRESULT; stdcall;
    function Skip(const celt : ULONG) : HRESULT; stdcall;
    function Reset : HRESULT; stdcall;
    function Clone(out pperddp : IEnumDebugApplicationNodes) : HRESULT; stdcall;
  end;

  IEnumDebugStackFrames = interface(IUnknown)
  ['{51973C1e-CB0C-11d0-B5C9-00A0244A0E7A}']
    function Next(const celt : ULONG; out prgdsfd : TDebugStackFrameDescriptor; out pceltFetched : ULONG) : HRESULT; stdcall;
    function Skip(const celt : ULONG) : HRESULT; stdcall;
    function Reset : HRESULT; stdcall;
    function Clone(out ppedsf : IEnumDebugStackFrames) : HRESULT; stdcall;
  end;

  IDebugDocumentProvider = interface (IDebugDocumentInfo)
  ['{51973C20-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Causes the document to be instantiated if it does not already exist.
    function GetDocument(out ppssd : IDebugDocument): HRESULT; stdcall;
  end;

  IDebugApplicationNode = interface (IDebugDocumentProvider)
  ['{51973C34-CB0C-11d0-B5C9-00A0244A0E7A}']
    function EnumChildren(out pperddp : IEnumDebugApplicationNodes) : HRESULT; stdcall;
    function GetParent(out pprddp : IDebugApplicationNode) : HRESULT; stdcall;
    function SetDocumentProvider(const pddp : IDebugDocumentProvider) : HRESULT; stdcall;
    function Close : HRESULT; stdcall;
    function Attach(const pdanParent : IDebugApplicationNode) : HRESULT; stdcall;
    function Detach : HRESULT; stdcall;
  end;

  IDebugSyncOperation = interface(IUnknown)
  ['{51973C1a-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Get TargetThread is called by PDM to determine what thread
    // to call Evaluate() in
    function GetTargetThread(out ppatTarget : IDebugApplicationThread): HRESULT; stdcall;

    // Execute is called synchronously by the PDM in the target thread. It
    // synchronously peforms the operation and returns. It returns E_ABORT if
    // the operation was aborted with InProgressAbort();
    function Execute(out ppunkResult : IUnknown) : HRESULT; stdcall;

    // InProgressAbort() is called by the PDM, from within the debugger thread,
    // to cancel an operation which is in progress in another thread. The
    // operation should be completed or error out with E_ABORT as soon as
    // possible. E_NOTIMPL can be returned if the operation cannot be cancelled.
    function InProgressAbort : HRESULT; stdcall;
  end;

  IDebugAsyncOperationCallBack = interface(IUnknown)
  ['{51973C1c-CB0C-11d0-B5C9-00A0244A0E7A}']
    // onComplete() is fired by the AsyncDebugOperation when a result is available.
    // The event is fired in the debugger thread.
    function onComplete : HRESULT; stdcall;
  end;

  IDebugAsyncOperation = interface(IUnknown)
  ['{51973C1b-CB0C-11d0-B5C9-00A0244A0E7A}']
    function GetSyncDebugOperation(out ppsdo : IDebugSyncOperation) : HRESULT; stdcall;

    // Start() causes the asynchronous operation to begin. It asynchronously
    // causes IDebugSyncOperation::Execute() to be called in the thread obtained
    // from IDebugSyncOperation::GetTargetThread(). It should only
    // be called from within the debugger thread, or it will not return until
    // the operation is complete (it degenerates to synchronous).
    // Returns E_UNEXPECTED if an operation is already pending.
    function Start(padocb : IDebugAsyncOperationCallBack) : HRESULT; stdcall;

    // Abort() causes InProgressAbort() to be called on the IDebugSyncOperation
    // object. It is normally called from within the debugger thread to cancel
    // a hung operation. If the abort happens before the request completes,
    // GetResult() will return E_ABORT. E_NOTIMPL may be returned from this
    // function if the operation is not cancellable.
    function Abort : HRESULT; stdcall;

    // QueryIsComplete() returns S_OK if the operation is complete; otherwise it
    // returns S_FALSE;
    function QueryIsComplete : HRESULT; stdcall;

    // If the request is complete, returns the HRESULT and object parameter
    // returned from IDebugSyncOperation::Execute(). Otherwise, returns
    // E_PENDING.
    function GetResult(out phrResult : HRESULT; out ppunkResult : IUnknown) : HRESULT; stdcall;
  end;

  IProvideExpressionContexts = interface (IUnknown)
  ['{51973C41-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Returns an enumerator of expression contexts.
    function EnumExpressionContexts(out ppedec : IEnumDebugExpressionContexts) : HRESULT; stdcall;
  end;

  IDebugStackFrameSniffer = interface(IUnknown)
  ['{51973C18-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Returns an enumerator of stack frames for the current thread. Top of stack should
    // be returned first (the most recently pushed frame).
    function EnumStackFrames(out ppedsf : IEnumDebugStackFrames) : HRESULT; stdcall;
  end;


  IDebugDocumentHost = interface(IUnknown)
  ['{51973C27-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Return a particular range of characters in the original host document,
    // added using AddDeferredText.
    //
    // It is acceptable for a host to return E_NOTIMPL for this method,
    // as long as the host doesn't call AddDeferredText.
    //
    // (Note that this is text from the _original_ document. The host
    // does not need to be responsible for keeping track of edits, etc.)
    function GetDeferredText(const dwTextStartCookie : DWORD;
       // Specifies a character text buffer. NULL means do not return characters.
       const pcharText : PWideChar;
       // Specifies a character attribute buffer. NULL means do not return attributes.
       const pstaTextAttr : TSourceTextAttr;
       // Indicates the actual number of characters/attributes returned. Must be set to zero
       // before the call.
       const pcNumChars : ULONG;
       // Specifies the number maximum number of character desired.
       const cMaxChars : ULONG) : HRESULT; stdcall;

    // Return the text attributes for an arbitrary block of document text.
    // It is acceptable for hosts to return E_NOTIMPL, in which case the
    // default attributes are used.
    function GetScriptTextAttributes(
       // The script block text. This string need not be null terminated.
       const pstrCode : PWideChar;
       // The number of characters in the script block text.
       const uNumCodeChars : ULONG;
       // See IActiveScriptParse::ParseScriptText for a description of this argument.
       const pstrDelimiter : PWideChar;
       // See IActiveScriptParse::ParseScriptText for a description of this argument.
       dwFlags : DWORD;
       // Buffer to contain the returned attributes.
       out pattr : TSourceTextAttr) : HRESULT; stdcall;

    // Notify the host that a new document context is being created
    // and allow the host to optionally return a controlling unknown
    // for the new context.
    //
    // This allows the host to add new functionality to the helper-provided
    // document contexts. It is acceptable for the host to return E_NOTIMPL
    // or a null outer unknown for this method, in which case the context is
    // used "as is".
    function OnCreateDocumentContext(out ppunkOuter : IUnknown) : HRESULT; stdcall;

    // Return the full path (including file name) to the document's source file.
    //*pfIsOriginalPath is TRUE if the path refers to the original file for the document.
    //*pfIsOriginalPath is FALSE if the path refers to a newly created temporary file
    //Returns E_FAIL if no source file can be created/determined.
    function GetPathName(out pbstrLongName : TBSTR;out pfIsOriginalFile : BOOL) : HRESULT; stdcall;

    // Return just the name of the document, with no path information.
    // (Used for "Save As...")
    function GetFileName(out pbstrShortName : TBSTR) : HRESULT; stdcall;

    // Notify the host that the document's source file has been saved and
    // that its contents should be refreshed.
    function NotifyChanged : HRESULT; stdcall;
   end;


  IEnumRemoteDebugApplications=Interface
    ['{51973C3b-CB0C-11d0-B5C9-00A0244A0E7A}']
    Function Next(celt:Cardinal;Var ppda:IRemoteDebugApplication;Var pceltFetched:Cardinal):HResult;stdcall;
    Function Skip(celt:Cardinal):HResult;stdcall;
    Function Reset:HResult;stdcall;
    Function Clone(Var ppessd:IEnumRemoteDebugApplications):HResult;stdcall;
  End;

  IMachineDebugManager=Interface
    ['{51973C2c-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Adds an application to the running application list. This method is called by the
    // process debug manager whenever IProcessDebugManager::AddApplication is called
    Function AddAplication(const pda:IRemoteDebugApplication;Var pdwAppCookie:DWord):HResult;stdcall;

    // Removes an application from the running application list. This method is called by the
    // process debug manager whenever IProcessDebugManager::RemoveApplication is called.
    Function RemoveApplication(dwAppCookie:DWord):HResult;stdcall;

    // Returns an enumerator of the current list of running applications. Used by the debugger
    // IDE to display and attach applications for debugging purposes.

    Function EnumApplications(Var ppeda:IEnumRemoteDebugApplications):HResult;stdcall;
  End;

// *********************************************************************//
// Interface: IMachineDebugManagerEvents
// Flags:     (0)
// GUID:      {51973C2E-CB0C-11D0-B5C9-00A0244A0E7A}
// *********************************************************************//
  IMachineDebugManagerEvents = interface(IUnknown)
    ['{51973C2E-CB0C-11D0-B5C9-00A0244A0E7A}']
    function  onAddApplication(const pda: IRemoteDebugApplication; dwAppCookie: DWord): HResult; stdcall;
    function  onRemoveApplication(const pda: IRemoteDebugApplication; dwAppCookie: DWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDebugApplicationNodeEvents
// Flags:     (0)
// GUID:      {51973C35-CB0C-11D0-B5C9-00A0244A0E7A}
// *********************************************************************//
  IDebugApplicationNodeEvents = interface(IUnknown)
    ['{51973C35-CB0C-11D0-B5C9-00A0244A0E7A}']
    function  onAddChild(const prddpChild: IDebugApplicationNode): HResult; stdcall;
    function  onRemoveChild(const prddpChild: IDebugApplicationNode): HResult; stdcall;
    function  onDetach: HResult; stdcall;
    function  onAttach(const prddpParent: IDebugApplicationNode): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDebugDocumentText
// Flags:     (0)
// GUID:      {51973C22-CB0C-11D0-B5C9-00A0244A0E7A}
// *********************************************************************//
  IDebugDocumentText = interface(IDebugDocument)
    ['{51973C22-CB0C-11D0-B5C9-00A0244A0E7A}']
    function  GetDocumentAttributes(out ptextdocattr: DWord): HResult; stdcall;
    function  GetSize(out pcNumLines: DWord; out pcNumChars: DWord): HResult; stdcall;
    function  GetPositionOfLine(cLineNumber: DWord; out pcCharacterPosition: DWord): HResult; stdcall;
    function  GetLineOfPosition(cCharacterPosition: DWord; out pcLineNumber: DWord;
                                out pcCharacterOffsetInLine: DWord): HResult; stdcall;
    function  GetText(cCharacterPosition: DWord; pcharText:PWideChar; 
                      pstaTextAttr:PWord; var pcNumChars: DWord;
                      cMaxChars: DWord): HResult; stdcall;
    function  GetPositionOfContext(const psc: IDebugDocumentContext; 
                                   out pcCharacterPosition: DWord; out cNumChars: DWord): HResult; stdcall;
    function  GetContextOfPosition(cCharacterPosition: DWord; cNumChars: DWord; 
                                   out ppsc: IDebugDocumentContext): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDebugDocumentTextEvents
// Flags:     (0)
// GUID:      {51973C23-CB0C-11d0-B5C9-00A0244A0E7A}
// *********************************************************************//
  IDebugDocumentTextEvents = interface(IUnknown)
    ['{51973C23-CB0C-11d0-B5C9-00A0244A0E7A}']
    function  onDestroy: HResult; stdcall;
    function  onInsertText(const cCharacterPosition: ULONG; const cNumToInsert: ULONG): HResult; stdcall;
    function  onRemoveText(const cCharacterPosition: ULONG; const cNumToRemove: ULONG): HResult; stdcall;
    function  onReplaceText(const cCharacterPosition: ULONG; const cNumToReplace: ULONG): HResult; stdcall;
    function  onUpdateTextAttributes(const cCharacterPosition: ULONG; const cNumToUpdate: ULONG): HResult; stdcall;
    function  onUpdateDocumentAttributes(const textdocattr: TEXT_DOC_ATTR): HResult; stdcall;
  end;

  IDebugApplication = interface(IRemoteDebugApplication)
  ['{51973C32-CB0C-11d0-B5C9-00A0244A0E7A}']
    // Sets the name of the application. The provided name will be returned in subsequent calls
    // to IRemoteDebugApplication::GetName.
    function SetName(const pstrName : PWideChar) : HRESULT; stdcall;

    // This method is called by language engines, in single step mode, just before they
    // return to their caller. The process debug manager uses this opportunity to notify all
    // other script engines that they should break at the first opportunity. This is how
    // cross language step modes are implemented.
    function StepOutComplete : HRESULT; stdcall;

    // Causes the given string to be displayed by the debugger IDE, normally in an output
    // window. This mechanism provides the means for a language engine to implement language
    // specific debugging output support. Example: Debug.writeln("Help") in JavaScript.
    function DebugOutput(const pstr : PWideChar) : HRESULT; stdcall;

    // Causes a default debugger IDE to be started and a debug session to be attached to
    // this application if one does not already exist. This is used to implement just-in-time
    // debugging.
    function StartDebugSession : HRESULT; stdcall;

    // Called by the language engine in the context of a thread that has hit a breakpoint.
    // This method causes the current thread to block and a notification of the breakpoint
    // to be sent to the debugger IDE. When the debugger IDE resumes the application this
    // method returns with the action to be taken.
    //
    // Note: While in the breakpoint the language engine may be called in this thread to do
    // various things such as enumerating stack frames or evaluating expressions.
    function HandleBreakPoint(const br : TBreakReason; out pbra : TBreakResumeAction) : HRESULT; stdcall;

    // Causes this application to release all references and enter a zombie state. Called
    // by the owner of the application generally on shut down.
    function Close : HRESULT; stdcall;

    // Returns the current break flags for the application.
    function GetBreakFlags(out pabf : TAppBreakFlags;
                  out pprdatSteppingThread : IRemoteDebugApplicationThread) : HRESULT; stdcall;

    // Returns the application thread object associated with the currently running thread.
    function GetCurrentThread(out pat : IDebugApplicationThread) : HRESULT; stdcall;

    // Creates an IDebugAsyncOperation object to wrap a provided IDebugSyncOperation object.
    // This provides a mechanism for language engines to implement asynchronous expression and
    // evaluation, etc. without having to know the details of synchronization with the
    // debugger thread. See the descriptions for IDebugSyncOperation and IDebugAsyncOperation
    // for more details.
    function CreateAsyncDebugOperation(const psdo : IDebugSyncOperation;
                              out ppado : IDebugAsyncOperation) : HRESULT; stdcall;

    // Adds a stack frame sniffer to this application. Generally called by a language engine
    // to expose its stack frames to the debugger. It is possible for other entities to
    // expose stack frames.
    function AddStackFrameSniffer(const pdsfs : IDebugStackFrameSniffer;
      // Returns a cookie that is used to remove this stack frame sniffer
      // from the application.
      out pdwCookie : DWORD) : HRESULT; stdcall;

    // Removes a stack frame sniffer from this application.
    function RemoveStackFrameSniffer(
      // The cookie returned by AddStackFrameSniffer.
      const dwCookie : DWORD) : HRESULT; stdcall;

    // Returns S_OK if the current running thread is the debugger thread.
    // Otherwise, returns S_FALSE.
    function QueryCurrentThreadIsDebuggerThread : HRESULT; stdcall;

    // Provides a mechanism for the caller to run code in the debugger thread. This is generally
    // used so that language engines and hosts can implement free threaded objects on top
    // of their single threaded implementations.
    function SynchronousCallInDebuggerThread(const pptc : IDebugThreadCall;
      const dwParam1 : DWORD;
      const dwParam2 : DWORD;
      const dwParam3 : DWORD) : HRESULT; stdcall;

    // Creates a new application node which is associated with a specific
    // document provider. Before it is visible, the new node must be
    // attached to a parent node.
    function CreateApplicationNode(out ppdanNew : IDebugApplicationNode) : HRESULT; stdcall;

    // Fire a generic event to the IApplicationDebugger (if any)
    // The semantics of the GUID and IUnknown are entirely application/debugger defined
    //
    // This method is currently unimplemented but is here to allow for future extensions.
    function FireDebuggerEvent(const riid : TGUID; const punk : IUnknown) : HRESULT; stdcall;

    // Called by the language engine in the context of a thread that has caused a runtime error.
    // This method causes the current thread to block and a notification of the error
    // to be sent to the debugger IDE. When the debugger IDE resumes the application this
    // method returns with the action to be taken.
    //
    // Note: While in the runtime error the language engine may be called in this thread to do
    // various things such as enumerating stack frames or evaluating expressions.
    function HandleRuntimeError(const pErrorDebug : IActiveScriptErrorDebug;// the error that occurred
                       const pScriptSite : IActiveScriptSite;// the script site of the thread
                       out pbra : TBreakResumeAction;// how to continue execution (stepping etc...)
                       out perra : TErrorResumeAction;// how to handle the error case
                       out pfCallOnScriptError : BOOL) : HRESULT; stdcall;// if TRUE then engine should call IActiveScriptSite::OnScriptError()

    // return TRUE if there is a JIT debugger registered
    function FCanJitDebug : BOOL; stdcall;

    // returns TRUE if a JIT debugger is registered and it is registered to auto-JIT debug dumb hosts
    function FIsAutoJitDebugEnabled : BOOL; stdcall;

    // Adds a global expression context provider to this application
    function AddGlobalExpressionContextProvider(const pdsfs : IProvideExpressionContexts;
    // Returns a cookie that is used to remove this global expression context provider
    // from the application.
    out pdwCookie : DWORD) : HRESULT; stdcall;

    // Removes a global expression context provider from this application.
    function RemoveGlobalExpressionContextProvider(
    // The cookie returned by AddGlobalExpressionContextProvider.
    const dwCookie : DWORD) : HRESULT; stdcall;
  end;

  IActiveScriptSiteDebug = interface
  ['{51973C11-CB0C-11d0-B5C9-00A0244A0E7A}']
    function GetDocumentContextFromPosition(const dwSourceContext : DWORD;
          const uCharacterOffset: ULONG; const uNumChars : ULONG;
          out ppsc : IDebugDocumentContext) : HRESULT; stdcall;

    function GetApplication(out ppda : IDebugApplication) : HRESULT; stdcall;
    function GetRootApplicationNode(out ppdanRoot : IDebugApplicationNode) : HRESULT; stdcall;
    function OnScriptErrorDebug(const pErrorDebug : IActiveScriptErrorDebug;
                out pfEnterDebugger : BOOL;out pfCallOnScriptErrorWhenContinuing : BOOL) : HRESULT; stdcall;
  end;

  IDebugSessionProvider = interface(IUnknown)
    ['{51973C29-CB0C-11D0-B5C9-00A0244A0E7A}']
    Function StartDebugSession(const pda: IRemoteDebugApplication):HResult;stdcall;
  end;

  IRemoteDebugApplicationEvents = interface(IUnknown)
    ['{51973C33-CB0C-11D0-B5C9-00A0244A0E7A}']
    Function OnConnectDebugger(const pad: IApplicationDebugger):HResult;stdcall;
    Function OnDisconnectDebugger:HResult;stdcall;
    Function OnSetName(pstrName: PWideChar):HResult;stdcall;
    Function OnDebugOutput(pstr: PWideChar):HResult;stdcall;
    Function OnClose:HResult;stdcall;
    Function OnEnterBreakPoint(const prdat: IRemoteDebugApplicationThread):HResult;stdcall;
    Function OnLeaveBreakPoint(const prdat: IRemoteDebugApplicationThread):HResult;stdcall;
    Function OnCreateThread(const prdat: IRemoteDebugApplicationThread):HResult;stdcall;
    Function OnDestroyThread(const prdat: IRemoteDebugApplicationThread):HResult;stdcall;
    Function OnBreakFlagChange(abf:DWord;const prdatSteppingThread: IRemoteDebugApplicationThread):HResult;stdcall;
  end;

{$Z1}

implementation



end.
