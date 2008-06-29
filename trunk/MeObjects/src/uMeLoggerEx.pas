
{ Summary the extented logger object. }
{
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
    * The Original Code is $RCSfile: uMeLoggerEx.pas,v $.
    * The Initial Developers of the Original Code are Borland.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):
}
unit uMeLoggerEx;

interface

uses
  SysUtils, Classes
  , uMeObject
  , uMeLog
  , uMeStream
  ;

type
  PMeFileLogger = ^ TMeFileLogger;
  PMeConsoleLogger = ^ TMeConsoleLogger;

  { Summary for logging information to the file. }
  TMeFileLogger = object(TMeStreamLogger)
  private
    FFileName: string;
  protected
  public
    constructor Create(const aFileName: string);
    procedure Open; virtual; //override;
    procedure Close; virtual; //override;
  end;
  
  { Summary for logging information to console. }
  { note: only for console application}
  TMeConsoleLogger = object(TMeCustomLogger)
  private
  protected
    procedure WriteLog(aMsg: string); virtual; //override;
  public
    procedure Open; virtual; //override;
    procedure Close; virtual; //override;
  end;

implementation

{ TMeFileLogger }
constructor TMeFileLogger.Create(const aFileName: string);
begin
  inherited Create(New(PMeFileStream, Create));
  FFileName := aFileName;
  
end;

procedure TMeFileLogger.Open;
var
  vMode: Word;
begin
  if FileExists(FFileName) then
  begin
    vMode := fmOpenWrite;
  end
  else
    vMode := fmCreate;
  vMode := vMode or fmShareDenyWrite;
  PMeFileStream(FStream).Open(FFileName, vMode);
end;

procedure TMeFileLogger.Close;
begin
  PMeFileStream(FStream).Close();
end;

{ TMeConsoleLogger }
procedure TMeConsoleLogger.WriteLog(aMsg: string);
begin
  Writeln(aMsg);
end;

procedure TMeConsoleLogger.Open;
begin
end;

procedure TMeConsoleLogger.Close;
begin
end;

initialization
  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeFileLogger), ovtVmtClassName, nil);
  SetMeVirtualMethod(TypeOf(TMeConsoleLogger), ovtVmtClassName, nil);
  {$ENDIF}
  SetMeVirtualMethod(TypeOf(TMeFileLogger), ovtVmtParent, TypeOf(TMeStreamLogger));
  SetMeVirtualMethod(TypeOf(TMeConsoleLogger), ovtVmtParent, TypeOf(TMeCustomLogger));
finalization
end.
