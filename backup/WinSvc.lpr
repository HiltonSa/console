Program WinSvc;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, DaemonManager, DaemonMain, zcomponent;

begin
  Application.Initialize;
  Application.Run;
end.
