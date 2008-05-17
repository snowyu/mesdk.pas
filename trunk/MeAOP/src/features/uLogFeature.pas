unit uLogFeature;

interface

{$I MeSetting.inc}

uses
  {$IFDEF LINUX}
  Types,
  Libc,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes, TypInfo
  {$IFDEF MeRTTI_SUPPORT}
  , uMeTypes
  , uMeProcType
  {$ENDIF}
  , uMeTypInfo
  , uMeInterceptor
  , uMeFeature
  , uMeLog
  ;

type
  TMeLogFeature = class(TMeCustomFeature)
  private
  protected
    { Summary te }
    procedure AfterExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
            Params: PMeProcParams = nil); override;
    procedure BeforeExecute(Sender: TObject; MethodItem:
            TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
            override;
  public
  end;


implementation

{
******************************** TMeLogFeature *********************************
}
procedure TMeLogFeature.AfterExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const thisState: TMeExecuteStates; const
        Params: PMeProcParams = nil);
var
  vLevel: TMeLogLevel;
  s: string;
  I: Integer;
begin
  //if Assigned(GLogger) then
  begin
    //s := '';
    vLevel := vlDebug;
    if esException in thisState then
    begin
      vLevel := vlError;
      s := 'Error! Exception occur:';
    end
    else if not (esAllowed in thisState) then
    begin
      vLevel := vlAudit;
      s := 'Login Failed:';
    end;
    if Assigned(Sender) then
      s := s + Sender.ClassName + '.';
    s := s + MethodItem.name + ':';
    S := s + uMeTypInfo.SetToString(TypeInfo(TMeExecuteState), Byte(thisState));
    GLogger.Log(vLevel, s);

    if Assigned(Params) then
    begin
      For I := 0 to Params.Count - 1 do
      begin
        if Params.Items[I].IsByRef then
        begin
          s:= 'ByRef ';
          //if Params.Items[I].DataType.Kind = mtkInteger then
            //Params.Items[I].AsInteger := Params.Items[I].AsInteger + 1;
        end
        else s := '';
        s := s + Format('Param[%s]=$%x', [Params.Items[I].Name, Params.Items[I].AsInteger]);
        GLogger.Log(vlDebug, s);

      end;
      if Assigned(Params.SelfParam) and (Sender <> Params.SelfParam.AsPointer) then
      begin
        s := 'Sender <> Params.SelfParam';
        GLogger.Log(vlError, s);
      end;
      if (esAfter in thisState) and Assigned(Params.ResultParam) and (Params.ResultParam.DataType.Kind = mtkInt64) then
      begin
        s :=  Format('Old Result=$%x', [Params.ResultParam.AsInt64]);
        GLogger.Log(vlDebug, s);
        Params.ResultParam.AsInt64 := $3344556600;
        s :=  Format('New Result=$%x', [Params.ResultParam.AsInt64]);
        GLogger.Log(vlDebug, s);
      end;
    end;
  end;
end;

procedure TMeLogFeature.BeforeExecute(Sender: TObject; MethodItem:
        TMeInterceptedMethodItem; const Params: PMeProcParams = nil);
var
  I: Integer;
begin
  if Assigned(Params) then
  begin
    For I := 0 to Params.Count - 1 do
    begin
      if Params.Items[I].IsByRef then
      begin
        if Params.Items[I].DataType.Kind = mtkInteger then
          Params.Items[I].AsInteger := Params.Items[I].AsInteger + 1;
      end
    end
  end
end;


end.
