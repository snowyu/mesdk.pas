{$IFDEF STATIC_METHOD_THREADSAFE_SUPPORT}
{ You need use uMeSystem unit first. }
asm
  db cX86NoOpDirective, cX86NoOpDirective, cX86NoOpDirective, cX86NoOpDirective, cX86NoOpDirective
end;
{$ENDIF}
