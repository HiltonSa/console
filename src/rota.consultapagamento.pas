unit rota.consultapagamento;

{$mode Delphi} {$H+}

interface

uses
  Classes, SysUtils, DateUtils, Horse, json.retorno, Horse.JWT;
type

  { TRotaConsultaPagamento }

  TRotaConsultaPagamento = class
    public
      class procedure RotaConsultaPagamento;
  end;

implementation

uses consultapagamento.service, dbconfig;

{ TRotaConsultaPagamento }

procedure OnRetornaCarteiraStream(aReq: THorseRequest; aRes: THorseResponse);
var
  vDiaVenc : TDateTime;
begin
  if TryISO8601ToDate(Copy(aReq.Params.Required(True).Field('diavenc').AsString,1,23), vDiaVenc)
  then aRes.ContentType('application/octet-stream')
           .Send<TStream>( TConsultaPagamentoService.New.CarteiraStream(
                         aReq.Params.Required(True).Field('cobrador').AsInteger,
                         vDiaVenc))
  else aRes.ContentType('application/octet-stream')
           .Send<TStream>( TRetornoJson.New.Sucesso(False).Mensagem('Data Inválida!').ToStream);
end;

procedure OnNomeTitular(aReq: THorseRequest; aRes: THorseResponse);
begin
  aRes.ContentType('application/octet-stream')
      .Send<TStream>( TConsultaPagamentoService.New.ConsultaNome(
                         aReq.Params.Required(True).Field('cobrador').AsInteger,
                         aReq.Params.Required(True).Field('nome').AsString));
end;

procedure OnAssociado(aReq: THorseRequest; aRes: THorseResponse);
var
  vAssociado: Integer;
begin
  vAssociado := aReq.Params.Required(True).Field('associado').AsInteger;
  aRes.ContentType('application/json; charset=UTF-8')
      .Send( TConsultaPagamentoService.New.Associado( vAssociado ) );
end;

procedure OnDependente(aReq: THorseRequest; aRes: THorseResponse);
var
  vAssociado: Integer;
begin
  vAssociado := aReq.Params.Required(True).Field('associado').AsInteger;
  aRes.ContentType('application/json; charset=UTF-8')
      .Send( TConsultaPagamentoService.New.Dependente( vAssociado ) );
end;

procedure OnDtRec(aReq: THorseRequest; aRes: THorseResponse);
var
  vContcon: Integer;
begin
  vContcon := aReq.Params.Required(True).Field('contcon').AsInteger;
  aRes.ContentType('application/octet-stream')
      .Send<TStream>( TConsultaPagamentoService.New.DataRecebimento( vContcon ) );
end;
procedure OnProximoAcerto(aReq: THorseRequest; aRes: THorseResponse);
var
  vDtRec : TDateTime;
  vResp,
  vIsoDate: String;
begin
  vIsoDate := aReq.Params.Required(True).Field('dtrec').AsString;
  if TryISO8601ToDate(Copy(vIsoDate, 1, Length(vIsoDate)-3), vDtRec)
  then vResp := TConsultaPagamentoService.New.ProximoAcerto(
                   aReq.Params.Required(True).Field('cobrador').AsInteger,
                   vDtRec)
  else vResp := TRetornoJson.New.Sucesso(False).Mensagem('Data Inválida! '+vIsoDate).ParaString;
  aRes.ContentType('application/json; charset=UTF-8').Send( vResp);
end;

procedure OnConsultaPagamento(aReq: THorseRequest; aRes: THorseResponse);
begin
  aRes.ContentType('text/html; charset=UTF-8')
      .Send( TConsultaPagamentoService.New.ConsultarPagamento(
                   aReq.Query.Field('recibo').AsString));
end;

procedure OnFrmpg(aReq: THorseRequest; aRes: THorseResponse);
begin
  aRes.ContentType('application/json; charset=UTF-8')
      .Send( TConsultaPagamentoService.New.FormasPagamento );
end;

procedure OnImagem(aReq: THorseRequest; aRes: THorseResponse);
begin
  aRes.ContentType('application/octet-stream')
      .Send<TStream>( TConsultaPagamentoService.New.GetImagem(aReq.Params.Required(True).Field('imagem').AsInteger));
end;


class procedure TRotaConsultaPagamento.RotaConsultaPagamento;
begin
  THorse.Get('/carteira/cobrador/:cobrador/diavenc/:diavenc', OnRetornaCarteiraStream)
        .Get('/consulta', OnConsultaPagamento)
        .Get('/titular/:cobrador/:nome',OnNomeTitular)
        .Get('/frmpg', OnFrmpg)
        .Get('/dtrec/:contcon', OnDtRec)
        .Get('/associado/:associado', OnAssociado)
        .Get('/dependentes/:associado',onDependente)
        .Get('/imagem/:imagem', OnImagem)
        .Get('/acerto/cobrador/:cobrador/dtrec/:dtrec', OnProximoAcerto);

end;

end.

