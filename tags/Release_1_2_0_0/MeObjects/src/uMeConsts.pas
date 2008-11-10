{Summary MeConsts - the constants for the MeSDK Core.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision$

  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
      or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeConsts.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2006-2008
    * All rights reserved.

    * Contributor(s):
}

unit uMeConsts;

interface

{$I MeSetting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  ;

const
  //JUST BE CAREFUL, CHANGE THIS, YOU MUST APPLY the new CRC32 by CalcStr.dpr, see also uMeObject.SetupInit!!!
  cMeSDKCopyRight : String = 'MeSDK Copyright (c) 2006-2007 by MeSDK Software Development Kid Pty Ltd - Riceball LEE(riceballl@hotmail.com), all rights reserved.';

resourcestring
  {$IFDEF FPC}
  SListIndexError = 'List index out of bounds (%d)';
  {$ENDIF}
  rsMemoryWriteError = 'Error writing memory (%s)';
  rsMemoryReadError = 'Error reading memory (%s)';
  rsInvalidOpCodeError = 'Error Invalid Opcode: %x';
  rsCanNotUnInjectError = 'Error can not UnInject !!';
  rsInjectByOthersError = 'Error CAN NOT UnInject: Already Injected by others ';
  rsInjectAbstractMethodError = 'Error can not inject the abstract method by method address.';
  rsCanNotInjectError = 'Error can not Inject : no pre-hole found!!';
  rsAlreadyInterceptedError = 'Error this %s method or procedure is already intercepted.';
  rsInterceptUnknownError = 'Unkown error can not intercept to %s method or procedure.';
  rsInterceptUnknownMethodError = 'Error the %s method is unknown.';
  rsInterceptMethodParamsError = 'Error the %s method parameters Can not be created!.';
  rsInterceptUnknownPropertyError = 'Error the %s property is unknown.';
  rsDuplicateInterceptorError = 'Error Duplicate %s Interceptor Error';
  rsTypeNotSupportError = 'ERROR: this type %s is not supported';
  rsUnsupportedCallingConventionError = 'ERROR : Unsupported CallingConvention: %x Error';
  rsDuplicateIdentityNameError = 'Error Duplicate %s Identity Name Error';

implementation


end.
