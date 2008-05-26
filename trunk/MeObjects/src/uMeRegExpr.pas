
{Summary the RegExpr object .}
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
    * The Original Code is $RCSfile: uMeRegExpr.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Andrey V. Sorokin, St.Petersburg, Russia is Copyright (C) 1999-2004
    * Portions created by Riceball LEE is Copyright (C) 2003-2008
    * All rights reserved.

    * Contributor(s):
        Andrey V. Sorokin(RegExpr)
                                http://RegExpStudio.com
                                mailto:anso@mail.ru
}
unit uMeRegExpr;

interface

{$I MeSetting.inc}

uses
{$IFDEF MSWINDOWS}
  Windows, 
{$ENDIF}
  //TypInfo,
  SysUtils
  , uMeConsts
  , uMeSystem
  , uMeObject
  {$IFDEF DEBUG}
  , DbugIntf
  {$ENDIF}
  ;

type
{
将所有的正则表达式集中在一起
  Content=/():Count SearchListBegin ():SearchList SearchListEnd ():NextPageURL/:1
  SearchListBegin = //
  SearchList = /():Field1 ():Field2/:n

冒号后面的数字表示执行匹配搜索的次数，如果是n表示一直搜索直到没有匹配的内容。
如果省略最后的冒号和数字，表示此表达式只搜索1次。

支持嵌套匹配以及匹配次数。
等号分隔匹配定义名称以及匹配内容: Name=Content[:n]
如果Content为引号内容，表示为简单字符串匹配（支持*号和?号通配符号，转义符号为\）： Name='my*.*':1
如果Content为"/"斜杠扩起的，则表示为正则匹配，可以定义子字段
如： Name =/hello(.*):myfield/:n


举一个复杂的例子：
<table>
<tr><th>性别</th><th>年龄</th><th>名字</th></tr>
<tr><td>Male</td><td>13</td><td>Rose</td></tr>
<tr><td>Female</td><td>20</td><td>Jacky</td></tr>
</table>

Content=//[ListBegin]/ Good /[List]//
ListBegin=/<tr><th>(.*):Sex</th><th>(.*):Age</th><th>(.*):Name</th></tr>/:1
List=/<tr><td>(.*):$[ListBegin.Sex]</td><td>(.*):$[ListBegin.Age]</td><td>(.*):$[ListBegin.Name]</td></tr>/:n

/[ListBegin]/ 为子表达式
}
  TMeRegEx = object(TMeDynamicObject)
  protected
    FOrgExpression : RegExprString;
    FSubRegExprs: PMeSubRegExprs; //of TMeRegEx
    FExpressions: PMeStrings;
    FName: RegExprString;
  public
    //the current execute Expression.
    property Expression : RegExprString read GetExpression write SetExpression;
    property Name: RegExprString;
  end;

implementation

uses 
  RTLConsts, SysConst;

procedure TMeRegEx.SetExpression(const Value: RegExprString);
var
  s: RegExprString;
begin  
      1. AnsiExtractQuotedStr(var s: PChar; Quote: Char = '/'): string;
         s := AnsiExtractQuotedStr(PChar(Value), '/');
      2. Search all the sub RegExpressions: 
        inherited Expression := '\/\[(.+?):SubRegEx:\]\/';
        if Exec(s) then
        REPEAT
          vRegEx := TMeRegEx.Create;
        UNTIL not ExecNext;
end;

initialization
  SetMeVirtualMethod(TypeOf(TMeRegEx), ovtVmtParent, TypeOf(TMeDynamicObject));

  {$IFDEF MeRTTI_SUPPORT}
  SetMeVirtualMethod(TypeOf(TMeRegEx), ovtVmtClassName, nil);
  {$ENDIF}
end.
