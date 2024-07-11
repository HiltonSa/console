unit pagamento.service;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, conexao.zeos, ZDataset, fpjson, jsonparser, db;

type

  { IPagamentoService }

  IPagamentoService = interface
     ['{A03857EE-F8B2-4621-B8F3-1DB9269F0286}']
     function Recebimento(pBody: String): String;
     function Reembolso(pBody: String): String;
     function Repasse(pBody: String): String;
     function RepasseCompleto(pRepasse: String; pRelatorio: TMemoryStream): String;
  end;

  { TPagamentoService }

  TPagamentoService = class(TInterfacedObject, IPagamentoService)
    private
      FConexao: IDBConexao;
      procedure TotalizarAcerto(pAcerto: Integer);
      procedure IncluirReembolso(pCobrador: Integer; pQry: TZQuery; pObj: TJSONObject
        );
      procedure IncluirRecibo(pAcerto: Integer; pQry: TZQuery; pObj: TJSONObject);
    public
      constructor Create;
      destructor Destroy; override;
      class function New: IPagamentoService;

      function Recebimento(pBody: String): String;
      function Reembolso(pBody: String): String;
      function Repasse(pBody: String): String;
      function RepasseCompleto(pRepasse: String; pRelatorio: TMemoryStream): String;
  end;

implementation

uses json.retorno, console.util;

{ TPagamentoService }

procedure TPagamentoService.TotalizarAcerto(pAcerto: Integer);
const
  SQL_TOTALIZAR_ACERTO =
      'execute procedure PROC_TOTALIZAR_ACERTO ( :ACERTO)';
var
  vQry: TZQuery;
begin
  vQry := FConexao.GetQuery;
  try
    vQry.Close;
    vQry.SQL.Clear;
    vQry.SQL.Add(SQL_TOTALIZAR_ACERTO);
    vQry.Prepare;
    vQry.ParamByName('ACERTO'   ).AsInteger := pAcerto;
    vQry.ExecSQL;
  finally
    vQry.Close;
    FreeAndNil(vQry);
  end;
end;

procedure TPagamentoService.IncluirReembolso(pCobrador: Integer; pQry: TZQuery; pObj: TJSONObject);
const
  SQL_REEMBOLSO =
      'execute procedure PROC_APP_DESPESA ( :COBRADOR, :DTVEN, :VLDES, :OBS, :ACERTO)';
var
  vData: String;
begin
  TConsoleUtil.LogData('TPagamentoService.IncluirReembolso pObj:',pObj.ToString);
  vData := Copy(pObj.Find('data').AsString,1,23);  // tam max = 23

  pQry.SQL.Clear;
  pQry.SQL.Add(SQL_REEMBOLSO);
  pQry.Prepare;

  pQry.ParamByName('COBRADOR' ).AsInteger := pCobrador;
  pQry.ParamByName('DTVEN'    ).AsDate    := ISO8601ToDate(vData);
  pQry.ParamByName('ACERTO'   ).AsInteger := pObj.Find('acerto').AsInteger;
  pQry.ParamByName('VLDES'    ).AsFloat   := pObj.Find('valor').AsFloat;
  pQry.ParamByName('OBS'      ).AsString  := pObj.Find('observacao').AsString;

  pQry.ExecSQL;
end;

procedure TPagamentoService.IncluirRecibo(pAcerto: Integer; pQry: TZQuery;
  pObj: TJSONObject);
const
  SQL_RECEBIMENTO =
        'execute procedure PROC_APP_RECEBE ( :COBRADOR, :CONTRATO, :DTREC, '+
            ':QTDE, :RECIBO, :ACERTO, :VALOR, :LATITUDE, :LONGITUDE, :FRMPG, :STATUS)';
var
  vData: String;
begin
   TConsoleUtil.LogData('TPagamentoService.IncluirRecibo pObj:',pObj.ToString);
  vData := Copy(pObj.Find('dtrec').AsString,1,23);   // tam max = 23

  pQry.SQL.Clear;
  pQry.SQL.Add(SQL_RECEBIMENTO);
  pQry.Prepare;

  pQry.ParamByName('COBRADOR' ).AsInteger := pObj.Find('contcon').AsInteger;
  pQry.ParamByName('CONTRATO' ).AsInteger := pObj.Find('contrato').AsInteger;
  pQry.ParamByName('DTREC'    ).AsDate    := ISO8601ToDate(vData);
  pQry.ParamByName('QTDE'     ).AsInteger := pObj.Find('qtdParcelas').AsInteger;
  pQry.ParamByName('RECIBO'   ).AsInteger := pObj.Find('numero').AsInteger;
  pQry.ParamByName('ACERTO'   ).AsInteger := pAcerto;
  pQry.ParamByName('VALOR'    ).AsFloat   := pObj.Find('valor').AsFloat;
  pQry.ParamByName('LATITUDE' ).AsString  := pObj.Find('latitude').AsString;
  pQry.ParamByName('LONGITUDE').AsString  := pObj.Find('longitude').AsString;
  pQry.ParamByName('FRMPG'    ).AsInteger := pObj.Find('frmpg').AsInteger;
  pQry.ParamByName('STATUS'   ).AsInteger := pObj.Find('status').AsInteger;

  pQry.ExecSQL;
end;

constructor TPagamentoService.Create;
begin
  FConexao := TDBConexaoZeos.New;
end;

destructor TPagamentoService.Destroy;
begin
  inherited Destroy;
end;

class function TPagamentoService.New: IPagamentoService;
begin
  Result := Self.Create;
end;

function TPagamentoService.Recebimento(pBody: String): String;
var
  vBody: TJsonObject;
  vQry: TZQuery;
  vInvalido: Boolean;
  vAcerto: Integer;
begin
  TConsoleUtil.LogData('TPagamentoService.Recebimento pBody:',pBody);
  try
    if pBody.IsEmpty and not pBody.StartsWith('{') and not pBody.EndsWith('}')
    then raise Exception.Create('Body Invalido!');

    vBody := TJSONObject(GetJSON(pBody));

    vInvalido := (vBody.Find('acerto') = nil);
    if not vInvalido
    then vInvalido := (vBody.Find('contcon') = nil);
    if not vInvalido
    then vInvalido := (vBody.Find('dtrec') = nil);
    if not vInvalido
    then vInvalido := (vBody.Find('qtdParcelas') = nil);
    if not vInvalido
    then vInvalido := (vBody.Find('contrato') = nil);

    if vInvalido
    then raise Exception.Create('Formato Body Invalido!');

    vAcerto   := vBody.Find('acerto').AsInteger;

    vQry := FConexao.GetQuery;
    try
      vQry.Close;

      IncluirRecibo(vAcerto, vQry, vBody);

      //TotalizarAcerto(vAcerto);

      Result := TRetornoJson.New.Sucesso(True)
                                .Mensagem('Registro Aprovado!')
                                .Data(TJSONArray(GetJSON('[]')))
                                .ParaString;

    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vBody.Free;
    end;

  except
    on E: Exception do
      Result := TRetornoJson.New.Sucesso(False)
                            .Mensagem(E.Message)
                            .ParaString;;
  end;
end;

function TPagamentoService.Reembolso(pBody: String): String;
var
  vBody: TJsonObject;
  vQry: TZQuery;
  vInvalido: Boolean;
  vAcerto,
  vCobrador: Integer;
begin
  TConsoleUtil.LogData('TPagamentoService.Reembolso pBody:',pBody);
  try
    if pBody.IsEmpty and not pBody.StartsWith('{') and not pBody.EndsWith('}')
    then raise Exception.Create('Body Invalido!');

    vBody := TJSONObject(GetJSON(pBody));

    vInvalido := (vBody.Find('acerto') = nil);
    if not vInvalido
    then vInvalido := (vBody.Find('cobrador') = nil);
    if not vInvalido
    then vInvalido := (vBody.Find('data') = nil);
    if not vInvalido
    then vInvalido := (vBody.Find('valor') = nil);

    if vInvalido
    then raise Exception.Create('Formato Body Invalido!');

    vAcerto   := vBody.Find('acerto'  ).AsInteger;
    vCobrador := vBody.Find('cobrador').AsInteger;

    vQry := FConexao.GetQuery;
    try
      vQry.Close;

      IncluirReembolso(vCobrador, vQry, vBody);

      //TotalizarAcerto(vAcerto);

      Result := TRetornoJson.New.Sucesso(True)
                                .Mensagem('Registro Aprovado!')
                                .Data(TJSONArray(GetJSON('[]')))
                                .ParaString;


    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vBody.Free;
    end;

  except
    on E: Exception do
      Result := TRetornoJson.New.Sucesso(False)
                            .Mensagem(E.Message)
                            .ParaString;;
  end;
end;

function TPagamentoService.Repasse(pBody: String): String;
const
  SQL_ENVIAR_ACERTO =
      'execute procedure PROC_APP_ENVIAR(:ACERTO)';
var
  vBody,
  vObj: TJsonObject;
  vDespesas,
  vRecibos: TJSONArray;
  vQry: TZQuery;
  vAcerto, I, vCobrador: Integer;
begin
  TConsoleUtil.LogData('TPagamentoService.Repasse pBody:',pBody);
  try
    if pBody.IsEmpty and not pBody.StartsWith('{') and not pBody.EndsWith('}')
    then raise Exception.Create('Body Invalido!');

    vBody     := TJSONObject(GetJSON(pBody));

    if vBody.Find('acerto') = nil
    then raise Exception.Create('Body Invalido! Falta Acerto.');

    if vBody.Find('contcon') = nil
    then raise Exception.Create('Body Invalido! Falta Cobrador');

    vAcerto   := vBody.Find('acerto' ).AsInteger;
    vCobrador := vBody.Find('contcon').AsInteger;
    vDespesas := TJSONArray(vBody.FindPath('reembolsos'));
    vRecibos  := TJSONArray(vBody.FindPath('recibos'));

    vQry := FConexao.GetQuery;
    try

      if vDespesas.Count > 0
      then begin
        vQry.Close;
        for I := 0 to Pred(vDespesas.Count) do
        begin
          vObj := TJSONObject(vDespesas[I]);
          IncluirReembolso(vCobrador, vQry, vObj);
        end;
      end;
      TConsoleUtil.LogData('TPagamentoService.Repasse Despesas enviadas:',I.ToString);

      if vRecibos.Count > 0
      then begin
        vQry.Close;
        for I := 0 to Pred(vRecibos.Count) do
        begin
          vObj := TJSONObject(vRecibos[I]);
          IncluirRecibo(vAcerto, vQry, vObj);
        end;
      end;

      TConsoleUtil.LogData('TPagamentoService.Repasse recibos enviados:',I.ToString);

      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(SQL_ENVIAR_ACERTO);
      vQry.Prepare;
      vQry.ParamByName('ACERTO').AsInteger := vAcerto;
      vQry.ExecSQL;
      TConsoleUtil.LogData('TPagamentoService.Repasse acerto enviado:',vAcerto.ToString);
      Result := TRetornoJson.New.Sucesso(True)
                                .Mensagem('Repasse Efetuado!')
                                .Data(TJSONArray(GetJSON(Format('[ {"acerto": %d} ]', [vAcerto]))))
                                .ParaString;
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vBody.Free;
      //vObj.Free;
      //vDespesas.Free;
      //vRecibos.Free;
    end;
  except
    on E: Exception do
      Result := TRetornoJson.New.Sucesso(False)
                            .Mensagem(E.Message)
                            .ParaString;
  end;
end;

function TPagamentoService.RepasseCompleto(pRepasse : String ;
  pRelatorio : TMemoryStream) : String ;
const
  SQL_ENVIAR_ACERTO =
      'execute procedure PROC_APP_ENVIAR(:ACERTO)';
   SQL_ATUALIZA_BLOB = 'update ACERTOS set REPASSE = :CONTEUDO where CODIGO = %d';
var
  vBody,
  vObj: TJsonObject;
  vDespesas,
  vRecibos: TJSONArray;
  vQry: TZQuery;
  vAcerto, I, vCobrador: Integer;
begin
  TConsoleUtil.LogData('TPagamentoService.Repasse pRepasse:',pRepasse);
  try
    if pRepasse.IsEmpty and not pRepasse.StartsWith('{') and not pRepasse.EndsWith('}')
    then raise Exception.Create('Repasse Invalido!');

    vBody     := TJSONObject(GetJSON(pRepasse));

    if vBody.Find('acerto') = nil
    then raise Exception.Create('Repasse Invalido! Falta Acerto.');

    if vBody.Find('contcon') = nil
    then raise Exception.Create('Repasse Invalido! Falta Cobrador');

    vAcerto   := vBody.Find('acerto' ).AsInteger;
    vCobrador := vBody.Find('contcon').AsInteger;
    vDespesas := TJSONArray(vBody.FindPath('reembolsos'));
    vRecibos  := TJSONArray(vBody.FindPath('recibos'));

    vQry := FConexao.GetQuery;
    try
      pRelatorio.Position := 0;
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(Format(SQL_ATUALIZA_BLOB, [vAcerto]));
      vQry.ParamByName('CONTEUDO').LoadFromStream(pRelatorio, ftBlob);
      vQry.ExecSQL;
      vQry.ApplyUpdates();

      if vDespesas.Count > 0
      then begin
        vQry.Close;
        for I := 0 to Pred(vDespesas.Count) do
        begin
          vObj := TJSONObject(vDespesas[I]);
          IncluirReembolso(vCobrador, vQry, vObj);
        end;
      end;
      TConsoleUtil.LogData('TPagamentoService.Repasse Despesas enviadas:',I.ToString);

      if vRecibos.Count > 0
      then begin
        vQry.Close;
        for I := 0 to Pred(vRecibos.Count) do
        begin
          vObj := TJSONObject(vRecibos[I]);
          IncluirRecibo(vAcerto, vQry, vObj);
        end;
      end;

      TConsoleUtil.LogData('TPagamentoService.Repasse recibos enviados:',I.ToString);

      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(SQL_ENVIAR_ACERTO);
      vQry.Prepare;
      vQry.ParamByName('ACERTO').AsInteger := vAcerto;
      vQry.ExecSQL;
      TConsoleUtil.LogData('TPagamentoService.Repasse acerto enviado:',vAcerto.ToString);
      Result := TRetornoJson.New.Sucesso(True)
                                .Mensagem('Repasse Efetuado!')
                                .Data(TJSONArray(GetJSON(Format('[ {"acerto": %d} ]', [vAcerto]))))
                                .ParaString;
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vBody.Free;
      //vObj.Free;
      //vDespesas.Free;
      //vRecibos.Free;
    end;
  except
    on E: Exception do
      Result := TRetornoJson.New.Sucesso(False)
                            .Mensagem(E.Message)
                            .ParaString;
  end;
end ;



end.

