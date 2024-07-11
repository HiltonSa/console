unit contconserv.horse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rota.registro, DaemonApp, Horse.JWT,Horse, Horse.CORS, Horse.OctetStream;

type

  { TServidorHorse }

  TServidorHorse = class(TDaemon)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleShutDown(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
  private

  public

  end;

var
  ServidorHorse: TServidorHorse;

implementation

uses Rota.Base, rota.tabela, rota.consultapagamento, dbconfig, rota.pagamento;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TServidorHorse)
end;

{$R *.lfm}

{ TServidorHorse }

procedure RunHorse;
begin
  // Need to set "HORSE_DAEMON" compilation directive
  THorse.Listen(9089);
end;

procedure TServidorHorse.DataModuleCreate(Sender: TObject);
begin
  THorse.Use(@CORS);
  THorse.Use(@OctetStream);
  THorse.Use(HorseJWT(DB_SECRET, THorseJWTConfig.New.SkipRoutes( ROTAS_EXCLUIDAS )));

  TBase.Rota;
  TRotaRegistro.RotaRegistro;
  TRotaConsultaPagamento.RotaConsultaPagamento;
  TRotaTabela.RotaTabela;
  TRotaPagamento.RotaPagamento;

end;

procedure TServidorHorse.DataModuleShutDown(Sender: TCustomDaemon);
begin
  THorse.StopListen;
end;

procedure TServidorHorse.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  TThread.CreateAnonymousThread(@RunHorse).Start;
end;


initialization
  RegisterDaemon;
end.

