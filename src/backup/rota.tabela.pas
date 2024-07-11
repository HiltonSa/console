unit rota.tabela;

{$mode delphi} {$H+}

interface

uses
  Classes, SysUtils, fpjson, Horse, console.util, Horse.BasicAuthentication;
type

   { TRotaTabela }

   TRotaTabela = class
    public
      class procedure RotaTabela;
  end;
implementation

uses tabela.service;

{ TRotaTabela }

procedure OnBlob(aReq: THorseRequest; aRes: THorseResponse);
var
  vTabela, vCampo, vCodigo: String;
  vStream: TMemoryStream;
begin
  vTabela := aReq.Headers.Field('x-tabela').AsString;
  vCampo  := aReq.Headers.Field('x-campo').AsString;
  vCodigo := aReq.Headers.Field('x-codigo').AsString;

  vStream := aReq.Body<TMemoryStream>;

  //TLog.New.Identificador('Stream').Log(TStream(aReq.Body).Size.ToString);

  aRes.ContentType('application/octet-stream')
      .Send( TTabelaService.New.AtualizarBlob( vTabela,
                                               vCampo,
                                               vStream,
                                               StrToInt(vCodigo)));
end;

procedure OnTabela(aReq: THorseRequest; aRes: THorseResponse);
begin
  TConsoleUtil.LogData('RotaTabela:Ontabela:aReq.Body', aReq.Body);
  if Pos('SELECT', UpperCase(aReq.Body)) > 0
  then aRes.ContentType('application/octet-stream')
           .Send<TStream>( TTabelaService.New.AbrirSql( aReq.Body ))
  else aRes.ContentType('application/octet-stream')
           .Send( TTabelaService.New.ExecutarSql( aReq.Body ));
end;

procedure OnLista(aReq: THorseRequest; aRes: THorseResponse);
begin
  TConsoleUtil.LogData('RotaTabela:OnLista:aReq.Body', aReq.Body);
  aRes.ContentType('application/octet-stream')
      .Send<TStream>( TTabelaService.New.ListasComboBox( GetJSON(aReq.Body) ));
end;

procedure OnConfig(aReq: THorseRequest; aRes: THorseResponse);
var
  vTabela: String;
begin
  vTabela := aReq.Query.Field('tabela').AsString;
  TConsoleUtil.LogData('RotaTabela:OnConfig:VTabela', vTabela);
  aRes.ContentType('application/octet-stream')
      .Send<TStream>( TTabelaService.New.Config( Trim(vTabela) ) );
end;

procedure OnStatus(aReq: THorseRequest; aRes: THorseResponse; aNext: TNextProc);
begin
  aRes.ContentType('application/json')
      .Send(TTabelaService.New.Status));
end;

function DoLogin(const AUsername, APassword: string): Boolean;
begin
  // Here inside you can access your database and validate if username and password are valid
  Result := AUsername.Equals('SYSDBA') and APassword.Equals('masterkey');
end;


class procedure TRotaTabela.RotaTabela;
begin
  THorse.AddCallback(HorseBasicAuthentication(DoLogin))
        .Post('/tabela', OnTabela)
        .Post('/tabela/lista', OnLista)
        .Post('/tabela/blob', OnBlob)
        .Post('/tabela/config', onConfig)
        .Get('/tabela/status', onStatus);
end;

end.

