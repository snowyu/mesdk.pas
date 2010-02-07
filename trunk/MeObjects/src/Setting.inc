{ General Compiler Directives and Compiler Versions Setting }
{$IFNDEF SETTING_INC}
{$DEFINE SETTING_INC}

{Include Delphi Versions File}
{$I jedi.inc}

{$IFNDEF JEDI_INC}
ALERT_jedi_inc_incompatible
// secure against old versions of jedi.inc.
{$ENDIF ~JEDI_INC}

{------------------------------------------------------------------------------}
{ Please change this to suit your needs                                        }
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{------------------------------------------------------------------------------}
// support for multibyte character sets
{$IFDEF COMPILER3_UP}
  {$DEFINE MBCS_SUPPORT}
{$ENDIF}

//Real48is maintained for backward compatibility. 
//Since its storage format is not native to the Intel processor architecture, 
//it results in slower performance than other floating-point types.
//{$DEFINE REALCOMPATIBILITY_ON}

{***************** These compiler settings shouldn't be changed ***************}
  {$A+ Force alignment on word/dword boundaries}
  {$S+ stack checking}

  {$B- Short evaluation of boolean values}
  {$H+ Long string support}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$P+ Open string parameters}
  {$J+ Writeable typed constants}
  {$R- No Range checking}
  {$T-} // Type checked pointers off
  {$MINENUMSIZE 1}  {$DEFINE ByteEnum} {$DEFINE WordEnum}
  //{$MINENUMSIZE 2}  {$DEFINE WordEnum}
  //{$MINENUMSIZE 4}  

//pre-hole intercept mechanism:
{$DEFINE STATIC_METHOD_THREADSAFE_SUPPORT}

{$IFDEF BCB}
  {$OBJEXPORTALL ON}
{$ENDIF}

// additional tests for debugging
{$IFDEF DEBUG}
  {$HINTS ON; WARNINGS ON}
  {$D+,L+,W+,Y+,Q+}
  {$IFNDEF FPC}
    {$YD}
  {$ENDIF}
{$ELSE}
  {$HINTS OFF; WARNINGS OFF}
  {$D-,Q-,W-}
  {$OPTIMIZATION ON}
  {$IFNDEF FPC}
    {$L-,Y-}
    {$IMAGEBASE $00400000}
  {$ENDIF}
{$ENDIF}

{$IFNDEF COMPILER7}
  {$undef XPTHEMES}
{$ENDIF}


{$IFDEF CLR}
 {$WARN UNSAFE_TYPE ON}
 {$WARN UNSAFE_CODE ON}
 {$WARN UNSAFE_CAST ON}
 {$WARN UNIT_PLATFORM OFF}

 {$DEFINE MSWINDOWS}
 {$DEFINE PIC}
 {$DEFINE PUREPASCAL}
{$ENDIF CLR}

{$IFDEF FPC}
 {.$MODE DELPHI} // Delphi-compatible mode in FreePascal
 {$DEFINE PUREPASCAL}
{$ENDIF FPC}

{$IFNDEF FPC}
  {$IFDEF LINUX}
    {$DEFINE IntThreadPriority}
  {$ENDIF}
{$ENDIF}

{$IFDEF MeRTTI_EXT_SUPPORT}
  {$IFNDEF MeRTTI_SUPPORT}
    {$DEFINE MeRTTI_SUPPORT}
  {$ENDIF}
{$ENDIF}

{$IFDEF REALCOMPATIBILITY_ON}
  {$REALCOMPATIBILITY ON}
{$ENDIF}

{$ENDIF ~SETTING_INC}
