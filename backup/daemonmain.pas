unit DaemonMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type
  TDaemon_Main = class(TDaemon)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleShutDown(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
  end;

var
  Daemon_Main: TDaemon_Main;

implementation

uses
  Horse, Rota.Base, registro.service, rota.registro,
  consultapagamento.service, rota.consultapagamento, console.util, Horse.CORS,
  Horse.JWT, tabela.service, rota.tabela, Horse.Compression, Horse.OctetStream,
  model.log, dbconfig;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TDaemon_Main)
end;

procedure RunHorse;
begin
  // Need to set "HORSE_DAEMON" compilation directive
  THorse.Listen(8083);
end;

{$R *.lfm}

procedure TDaemon_Main.DataModuleCreate(Sender: TObject);
begin
  THorse.Use(@CORS);
  THorse.Use(@OctetStream);
  THorse.Use(HorseJWT(DB_SECRET, THorseJWTConfig.New.SkipRoutes( ROTAS_EXCLUIDAS )));

  TBase.Rota;
  TRotaRegistro.RotaRegistro;
  TRotaConsultaPagamento.RotaConsultaPagamento;
  TRotaTabela.RotaTabela;
  //THorse.Get('ping', @Ping)
  //      .Get('/',@GetHome);
end;

procedure TDaemon_Main.DataModuleShutDown(Sender: TCustomDaemon);
begin
  THorse.StopListen;
end;

procedure TDaemon_Main.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
  TThread.CreateAnonymousThread(@RunHorse).Start;
end;

initialization
  RegisterDaemon;

end.
