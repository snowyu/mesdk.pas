
{Summary the CoRountine object of the MeRegExpr .}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision: 1.00 $

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
    * The Original Code is $RCSfile: uMeRegExprCoRoutine.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2008
    * All rights reserved.

    * Contributor(s):

MeObject usage:
var
  r: PMeCustomRegExpr;
  Enumerator: PMeRegExprEnumerator;
begin
  New(r, Create);
  New(Enumerator, Create(r));
  try try 
    r.InputString := 'this is not a Good New, but it is Better. it is not Bad.';
    r.Pattern := '/Good|Better|Bad/:n';
    while Enumerator.MoveNext() do
    begin
      if Assigned(r.RegExpr) then
        writeln('found: ', r.RegExpr.Match[0]);
    end;
  except
    on e: exception do writeln(e.ClassName, ' Exception:', e.message);
  end;
  finally
    r.free;
    Enumerator.free;
  end;
end.

Class Usage:


var
  r: PMeCustomRegExpr;
  for r in RegEx('/Hello|Word|Hi/:n', 'Hello Word Hi Word') do
    writeln('found: ', r.RegEx.Match[0]);
  
  See the unit uMeRegExprEnumerator.pas
var
  r: PMeCustomRegExpr;
  Enumerator: TMeRegExprEnumerator;


  TRegExprEx = class
  protected
  public
    constructor Create(const aRegExpr: PMeCustomRegExpr);
    function GetEnumerator(): TMeRegExprEnumerator;
  end;


function RegEx(const aPatten: string; const aStr: string): IRegExprEnumeratorFactory;



}
unit uMeRegExprCoRoutine;

interface

{$I MeSetting.inc}

uses
{$IFDEF MSWINDOWS}
  Windows, 
{$ENDIF}
  //TypInfo,
  SysUtils
  , uRegExpr
  , uMeConsts
  , uMeSystem
  , uMeObject
  //, uMeYield
  , uMeCoroutine
  , uMeRegExpr
  {$IFDEF DEBUG}
  , DbugIntf
  {$ENDIF}
  ;

type
  {$IFNDEF YieldClass_Supports}
  PMeRegExprEnumerator = ^ TMeRegExprEnumerator;
  {$ENDIF}

  TMeRegExprEnumerator = {$IFDEF YieldClass_Supports}class{$ELSE}object{$ENDIF}(TMeCustomCoRoutine)
  protected
    FRegExpr: PMeCustomRegExpr;
    FCurrent: PMeAbstractRegExpr; //only available on the execution time.

    procedure DoFound(const Sender: PMeAbstractRegExpr; const aResult: PMeRegExprResultItem);
    //##CoRoutine
    procedure CoExecute; {$IFDEF YieldClass_Supports}override{$ELSE}virtual{$ENDIF};
  public
    constructor Create(const aRegExpr: PMeCustomRegExpr);
    property Current: PMeAbstractRegExpr read FCurrent;
  end;

implementation

{ TMeRegExprEnumerator }
constructor TMeRegExprEnumerator.Create(const aRegExpr: PMeCustomRegExpr);
begin
  inherited Create;
  FRegExpr := aRegExpr;
  
end;

procedure TMeRegExprEnumerator.CoExecute;
begin
  if Assigned(FRegExpr) then
  begin
    FRegExpr.OnFound := DoFound;
    FRegExpr.Execute();
  end;
end;

procedure TMeRegExprEnumerator.DoFound(const Sender: PMeAbstractRegExpr; const aResult: PMeRegExprResultItem);
begin
  FCurrent := Sender;
  Yield;
end;

initialization
{$IFNDEF YieldClass_Supports}
  SetMeVirtualMethod(TypeOf(TMeRegExprEnumerator), ovtVmtParent, TypeOf(TMeCustomCoRoutine));

  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeRegExprEnumerator), ovtVmtClassName, nil);
  {$ENDIF}
{$ENDIF}
end.

