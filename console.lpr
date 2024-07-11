program console;

{$mode delphi}{$H+}

uses
  {$ifdef linux}
  cthreads,
  {$endif}
  Classes, SysUtils, Horse, Rota.Base, registro.service, rota.registro,
  consultapagamento.service, rota.consultapagamento, console.util, Horse.CORS,
  Horse.JWT, tabela.service, rota.tabela,{ Horse.Compression,} Horse.OctetStream,
  model.log, dbconfig, Horse.BasicAuthentication, rota.pagamento;

{$ifndef horse_cgi}
procedure OnListen;
begin
  WriteLn(Format('Servidor ativo na porta %d',[THorse.Port]));
end;
{$endif}


begin
  //THorse.Use(Compression());
  THorse.Use(CORS);
  THorse.Use(OctetStream);
  THorse.Use(HorseJWT(DB_SECRET, THorseJWTConfig.New.SkipRoutes( ROTAS_EXCLUIDAS )));

  TBase.Rota;
  TRotaRegistro.RotaRegistro;
  TRotaConsultaPagamento.RotaConsultaPagamento;
  TRotaTabela.RotaTabela;
  TRotaPagamento.RotaPagamento;

  {$ifdef HORSE_CGI}
  THorse.Listen;
  {$else}
  THorse.Listen(9095, OnListen);
  {$endif}
end.

