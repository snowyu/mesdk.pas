

{Summary Enumerator Pattern -- MeYield..}
{
   @author  Riceball LEE<riceballl@hotmail.com>
   @version $Revision: 1.00 $

usage
  just a demo for YieldClass_Supports D2007.
  for s in EnumStrs(aStrs, @StrsReverse) do 
    writeln(s);


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
 * The Original Code is $RCSfile: uMeEnumerator.pas,v $.
 *
 * The Initial Developers of the Original Code are Riceball LEE<riceballl@hotmail.com>.
 * Portions created by Riceball LEE<riceballl@hotmail.com> is Copyright (C) 2007
 * All rights reserved.
 *
 * Contributor(s):
 *
 *)
unit uMeEnumerator;

interface

{$I MeSetting.inc}


uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeYield
  {$IFNDEF YieldClass_Supports}
  {$Message error 'Switch the YieldClass_Supports directive on (MeSetting.inc file) '}
  , uMeObject
  {$ENDIF}
  ;

type
  TStringsEnumerator = class(TYieldString)
  protected
    FStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  public
    property Strings: TStrings read FStrings write SetStrings;
  end;

  IStringsEnumeratorFactory = interface
    function GetEnumerator(): TStringsEnumerator;
  end;

  TStringsEnumeratorFactory = class(TInterfacedObject, IStringsEnumeratorFactory)
  protected
    FStrings: TStrings;
    FControlProc: TMeCoroutineProc;
  public
    constructor Create(const aStrs: TStrings; const aControlProc: TMeCoroutineProc);
    function GetEnumerator(): TStringsEnumerator;
  end;

function EnumStrs(const aStrs: TStrings; const aControlProc: TMeCoroutineProc): IStringsEnumeratorFactory;

procedure StrsReverse(const aStrsEnumerator: TStringsEnumerator);

implementation

function EnumStrs(const aStrs: TStrings; const aControlProc: TMeCoroutineProc): IStringsEnumeratorFactory;
begin
  Result := TStringsEnumeratorFactory.Create(aStrs, aControlProc);
end;

//要想抽象出通用的控制逻辑，首先要抽象出容器，这里仅作为示例罢了
procedure StrsReverse(const aStrsEnumerator: TStringsEnumerator);
var
  i: integer;
  s: string;
begin
  with aStrsEnumerator do if Assigned(Strings) then
  begin
    for i := Strings.Count -1 downto 0 do
    begin
      s := Strings[i];
      Yield(s);
    end;
  end;
end;

{ TStringsEnumerator }
procedure TStringsEnumerator.SetStrings(const Value: TStrings);
begin
  if ((FStatus = coDead) or (FStatus = coSuspended)) and (Value <> FStrings) then
    FStrings := Value;
end;

{ TStringsEnumeratorFactory }
constructor TStringsEnumeratorFactory.Create(const aStrs: TStrings; const aControlProc: TMeCoroutineProc);
begin
  inherited Create;
  FStrings := aStrs;
  FControlProc := aControlProc;
end;

function TStringsEnumeratorFactory.GetEnumerator(): TStringsEnumerator;
begin
  Result := TStringsEnumerator.Create(FControlProc);
  Result.Strings := FStrings;
end;

initialization
end.
