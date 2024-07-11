unit registro.service;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, conexao.zeos, LazJWT, ZDataset, fpjson, jsonparser;
type

  { IRegistroUsuario }

  IRegistroUsuarioService = interface
     ['{F6C37CB3-89FD-4D9C-B0D8-2B7A8C6D9FAE}']
     function Registro(pBody: String): String;
     function Logar(pBody: String): String;
  end;

  { TRegistroUsuarioService }

  TRegistroUsuarioService = class(TInterfacedObject, IRegistroUsuarioService)
    private
      FConexao: IDBConexao;
      procedure GravarAcesso(pRegistro: String);
    public
      constructor Create;
      destructor Destroy; override;
      class function New: IRegistroUsuarioService;
      function Registro(pBody: String): String;
      function Logar(pBody: String): String;
  end;

implementation

uses json.retorno, console.util, dbconfig;

const
  SQL_LOGIN  =
    'SELECT w.codigo, w.perfil, w.contcon, w.acesso, w.ativo, c.nome, '+
    '       c.pccom, c.seqrecibo                                      '+
    'FROM usuweb w                                                    '+
    'INNER JOIN cobradores c ON c.codigo = w.contcon AND c.sit = 1     '+
    'WHERE w.registro = :registro                                      ';
  SQL_ATUALIZA_ACESSO =
    'update USUWEB set ACESSO = %s where REGISTRO = %s';
  SQL_RETORNA_PERFIL =
    'select NOME, PCCOM, SEQRECIBO from COBRADORES where SIT = 1 and CODIGO = :codigo';
{ TRegistroUsuarioService }

procedure TRegistroUsuarioService.GravarAcesso(pRegistro: String);
var
  vSql: String;
begin
  vSql := Format(SQL_ATUALIZA_ACESSO, [QuotedStr(TConsoleUtil.RetornaTimeStampSql(Now)), QuotedStr(pRegistro)]);
  FConexao.ExecutarSQL(vSql);
end;

constructor TRegistroUsuarioService.Create;
begin
  FConexao := TDBConexaoZeos.New;
end;

destructor TRegistroUsuarioService.Destroy;
begin
  inherited Destroy;
end;

class function TRegistroUsuarioService.New: IRegistroUsuarioService;
begin
  Result := Self.Create;
end;

function TRegistroUsuarioService.Registro(pBody: String): String;
var
  vQry: TZQuery;
  vUsuario,
  vBody: TJsonObject;
  vRegistro: String;

begin
  try
    if pBody.IsEmpty and not pBody.StartsWith('{') and not pBody.EndsWith('}')
    then raise Exception.Create('Body Invalido!');

    vBody := TJSONObject(GetJSON(pBody));

    if vBody.Find('registro') = nil
    then raise Exception.Create('Formato Body Invalido!');

    vRegistro := vBody.Find('registro').AsString;

    vUsuario := TJsonObject.Create();
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(SQL_LOGIN);
      vQry.Params[0].AsString := vRegistro;

      vQry.Open;

      if not vQry.EOF
      then begin
        if vQry.FieldByName('acesso').IsNull
        then begin

          vUsuario.Add('codigo'   , vQry.FieldByName('codigo').AsInteger);
          vUsuario.Add('perfil'   , vQry.FieldByName('perfil').AsInteger);
          vUsuario.Add('contcon'  , vQry.FieldByName('contcon').AsInteger);
          vUsuario.Add('nome'     , vQry.FieldByName('nome').AsString);
          vUsuario.Add('comissao' , vQry.FieldByName('pccom').AsFloat);
          vUsuario.Add('seqrecibo', vQry.FieldByName('seqrecibo').AsInteger);

          GravarAcesso(vRegistro);

          Result := TRetornoJson.New.Sucesso(True)
                                .Mensagem('Registro Aprovado!')
                                .Data(TJSONArray(GetJSON(Format('[%s]',[vUsuario.FormatJSON]))))
                                .ParaString;
        end
        else raise Exception.Create('Chave já Registrada, acesso negado!');
      end
      else raise Exception.Create('Chave não Existe, acesso negado!');

    finally
      vQry.Close;
      //FreeAndNil(vUsuario);
      //FreeAndNil(vBody);
      FreeAndNil(vQry);

    end;

  except
    on E: Exception do
      Result := TRetornoJson.New
                            .Sucesso(False)
                            .Mensagem(E.Message)
                            .ParaString;
  end;
end;

function TRegistroUsuarioService.Logar(pBody: String): String;
var
  vQry: TZQuery;
  vUsuario,
  vBody: TJsonObject;
  vRegistro,
  vToken: String;
begin
  try
    if pBody.IsEmpty and not pBody.StartsWith('{') and not pBody.EndsWith('}')
    then raise Exception.Create('Body Invalido!');

    vBody := TJSONObject(GetJSON(pBody));

    if vBody.Find('registro') = nil
    then raise Exception.Create('Formato Body Invalido!');

    vRegistro := vBody.Find('registro').AsString;

    vUsuario := TJSONObject.Create;
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(SQL_LOGIN);
      vQry.Params[0].AsString := vRegistro;

      vQry.Open;

      if not vQry.EOF
      then begin
        if vQry.FieldByName('acesso').IsNull
        then raise Exception.Create('Chave não registrada!')
        else begin
          vUsuario.Add('codigo'   , vQry.FieldByName('codigo').AsInteger);
          vUsuario.Add('perfil'   , vQry.FieldByName('perfil').AsInteger);
          vUsuario.Add('contcon'  , vQry.FieldByName('contcon').AsInteger);
          vUsuario.Add('nome'     , vQry.FieldByName('nome').AsString);
          vUsuario.Add('comissao' , vQry.FieldByName('pccom').AsFloat);
          vUsuario.Add('seqrecibo', vQry.FieldByName('seqrecibo').AsInteger);

          vToken := TLazJWT.New
                           .SecretJWT(DB_SECRET)
                           .Exp(DateTimeToUnix(IncDay(Now)))
                           .AddClaim('perfil',vUsuario)
                           .Token;

          GravarAcesso(vRegistro);

          Result := TRetornoJson.New.Sucesso(True)
                                .Mensagem('Login Aprovado!')
                                .Data(TJSONArray(GetJSON(Format('[{"token": "%s"}]',[vToken]))))
                                //.Data(vUsuario)
                                .ParaString;

        end;
      end
      else raise Exception.Create('Chave não Existe, acesso negado!');
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vBody.Free;
      //vUsuario.Free;
    end;
  except
    on E: exception do
       Result := TRetornoJson.New.Sucesso(False).Mensagem(E.Message).ParaString;
  end;

end;

end.

