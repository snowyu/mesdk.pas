{Summary MeRemote Utils.}
{
   @author  Riceball LEE(riceballl@hotmail.com)
   @version $Revision$

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
    * The Original Code is $RCSfile: uMeRemoteUtils.pas,v $.
    * The Initial Developers of the Original Code are Riceball LEE.
    * Portions created by Riceball LEE is Copyright (C) 2007-2008
    * All rights reserved.

    * Contributor(s):

}
unit uMeRemoteUtils;

{$I MeSetting.inc}
{$IFNDEF MeRTTI_EXT_SUPPORT}
  {$Message Fatal 'need MeRTTI_EXT_SUPPORT'}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes
  , uMeObject
  , uMeTypInfo
  , uMeProcType
  ;

type
  TMeStreamProxy = class(TStream)
  protected
    FMeStream: PMeStream;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); overload; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: Classes.TSeekOrigin): Int64; overload; override;
  public
    constructor Create(const aMeStream: PMeStream);
    property MeStream: PMeStream read FMeStream write FMeStream;
  end;

procedure SaveParamsToStream(const aParams: PMeProcParams; const aStream: PMeStream);
procedure LoadParamsFromStream(const aParams: PMeProcParams; const aStream: PMeStream);

implementation

{ TStreamMeProxy }
constructor TMeStreamProxy.Create(const aMeStream: PMeStream);
begin
  Assert(Assigned(aMeStream));
  inherited Create;
  FMeStream := aMeStream;
end;

function TMeStreamProxy.GetSize: Int64;
begin
  Result := FMeStream.GetSize;
end;

procedure TMeStreamProxy.SetSize(const NewSize: Int64); 
begin
  FMeStream.SetSize(NewSize);
end;

function TMeStreamProxy.Read(var Buffer; Count: Longint): Longint; 
begin
  Result := FMeStream.Read(Buffer, Count);
end;

function TMeStreamProxy.Write(const Buffer; Count: Longint): Longint; 
begin
  Result := FMeStream.Write(Buffer, Count);
end;

function TMeStreamProxy.Seek(const Offset: Int64; Origin: Classes.TSeekOrigin): Int64;
begin
  Result := FMeStream.Seek(Offset, uMeObject.TSeekOrigin(Origin));
end;

{---------------------------------}
procedure SaveParamsToStream(const aParams: PMeProcParams; const aStream: PMeStream);
var
  i: Integer;
begin
  with aParams^ do
  begin
    //if Assigned(SelfParam) then
    //  SelfParam.SaveToStream(aStream);
    for i := 0 to Count - 1 do
    begin
      PMeParam(Items[i]).SaveToStream(aStream);
    end;
    if Assigned(ResultParam) then
      ResultParam.SaveToStream(aStream);
  end;
end;

procedure LoadParamsFromStream(const aParams: PMeProcParams; const aStream: PMeStream);
var
  i: Integer;
begin
  with aParams^ do
  begin
    //if Assigned(SelfParam) then
    //  SelfParam.LoadFromStream(aStream);
    for i := 0 to aParams.Count - 1 do
    begin
      PMeParam(aParams.Items[i]).LoadFromStream(aStream);
    end;
    if Assigned(aParams.ResultParam) then
      PMeParam(aParams.ResultParam).LoadFromStream(aStream);
  end;
end;


initialization
end.
