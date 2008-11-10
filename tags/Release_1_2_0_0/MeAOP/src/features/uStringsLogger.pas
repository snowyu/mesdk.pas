
{ Summary the logger for TStrings. }
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
    * The Original Code is $RCSfile: uStringsLogger.pas,v $.
    * The Initial Developers of the Original Code are Borland.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):
}
unit uStringsLogger;

interface

{$I MeSetting.inc}

uses
  SysUtils, Classes
  , uMeLog
  ;

type
  PStringsLogger = ^ TStringsLogger;
  TStringsLogger = object(TMeCustomLogger)
  private
    FStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  protected
    procedure WriteLog(aMsg: string); virtual; //override;
  public
    constructor Create(const aStrings: TStrings {$IFDEF SUPPORTS_DEFAULTPARAMS} = nil {$ENDIF});
    procedure Open; virtual; //override;
    property Strings: TStrings read FStrings write SetStrings;
  end;
  

implementation

constructor TStringsLogger.Create(const aStrings: TStrings);
begin
  inherited Create;
  FStrings := aStrings;
end;

procedure TStringsLogger.Open;
begin
  if Assigned(FStrings) then
    FStrings.Clear;
end;

procedure TStringsLogger.SetStrings(const Value: TStrings);
begin
  if Value <> FStrings then
  begin
    if Assigned(FRootLogger) then
      FRootLogger.Lock;
    try
      FStrings := Value;
    finally
      if Assigned(FRootLogger) then
        FRootLogger.Unlock;
    end;
  end;
end;

procedure TStringsLogger.WriteLog(aMsg: string);
begin
  //Note: jf u uses the TMemo etc VCL control, u should call SetIdleProc to Process Messages.
  if Assigned(FStrings) then
    FStrings.Add(aMsg);
end;


initialization
finalization
end.
