Program contconserv;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, contconserv.mapper, contconserv.horse, rota.registro
  { add your units here };

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
