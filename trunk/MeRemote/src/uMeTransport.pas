

{Summary Abstract MeTransport class and factory.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.00 $

HTTP Method   CRUD Action   Description
POST          CREATE        Create a new resource
GET           RETRIEVE      Retrieve a representation of a resource
PUT           UPDATE        Update a resource
DELETE        DELETE        Delete a resource  


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
    * The Original Code is $RCSfile: uMeTransport.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeTransport;

interface

{$I Setting.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeObject
  ;

type
  //how to convert the params to stream?
  {
  http://host/cat/aMethod?param=xx
  i will use REST protocol to do?
  }
  TMeReceivedDataEvent = procedure (const Sender: TObject; const aReply: TStream) of object;
  //abstract Transport class
  TMeTransport = class()
  protected
    //for Async method.
    FOnReceived: TMeReceivedDataEvent;
  public
    //send the aRequest, received the aReply.
    procedure Send(const aRequest: TStream; const aReply: TStream);virtual;abstract;
    procedure SendAsyn(const aRequest: TStream);virtual;abstract;

    property OnReceived: TMeReceivedDataEvent read FOnReceived write FOnReceived;
  end;

implementation

initialization
end.
