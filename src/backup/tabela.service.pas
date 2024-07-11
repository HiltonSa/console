unit tabela.service;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, fpjson, conexao.zeos, model.log,
  console.util, ZDataset, DataSet.Serialize;
type
  ITabelaService = Interface
    ['{A54B9CFB-40BF-4554-A994-961C1FBF6B22}']
    function ExecutarSql(pSql: String): String;
    function AtualizarBlob(pTabela, pCampo: String; pConteudo: TMemoryStream;
      pCodigo: Integer): String;
    function AbrirSql(pSql: String): TMemoryStream;
    function ListasComboBox(pLista: TJSONData): TMemoryStream;
    //function ClienteWindows: TStream;
    function Config(pTabela: String): TMemoryStream;
    function Status: String;
  end;

  { TTabelaService }

  TTabelaService = class(TInterfacedObject, ITabelaService)
    private
      FConexao: IDBConexao;
    public
      constructor Create;
      destructor Destroy; override;
      class function New: ITabelaService;

      function ExecutarSql(pSql: String): String;
      function AtualizarBlob(pTabela, pCampo: String; pConteudo: TMemoryStream;
        pCodigo: Integer): String;
      function AbrirSql(pSql: String): TMemoryStream;
      function ListasComboBox(pLista: TJSONData): TMemoryStream;
      function Config(pTabela: String): TMemoryStream;
      //function ClienteWindows: TStream;
      function Status: String;
  end;

implementation

uses json.retorno;

{ TTabelaService }

constructor TTabelaService.Create;
begin
  FConexao := TDBConexaoZeos.New;
end;

destructor TTabelaService.Destroy;
begin
  inherited Destroy;
end;

class function TTabelaService.New: ITabelaService;
begin
  Result := Self.Create;
end;

function TTabelaService.ExecutarSql(pSql: String): String;
var
  vQry: TZQuery;
begin
  Result := 'Nao Executado';
  try
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(pSql);

      vQry.ExecSQL;

      Result := 'Executado!';

    finally
      vQry.Close;
    end;

  except
    on E: exception do
      raise Exception.Create(e.Message);
  end;
end;

function TTabelaService.AtualizarBlob(pTabela, pCampo: String; pConteudo: TMemoryStream; pCodigo:Integer): String;
const
   SQL_ATUALIZA_BLOB = 'update %s set %s = :CONTEUDO where CODIGO = %d';
var
  vQry: TZQuery;
  vSql: String;
begin
  Result := 'Nao Executado';
  vSql := Format(SQL_ATUALIZA_BLOB, [pTabela, pCampo, pCodigo]);
  try
    if pConteudo.Size > 0
    then begin
      vQry := FConexao.GetQuery;
      try
        vQry.Close;
        vQry.SQL.Clear;
        vQry.SQL.Add(vSql);

        vQry.ParamByName('CONTEUDO').LoadFromStream(pConteudo, ftBlob);
        vQry.ExecSQL;
        vQry.ApplyUpdates();

        Result := 'Executado!';

      finally
        vQry.Close;
      end;
    end;

  except
    on E: exception do
      raise Exception.Create(e.Message);
  end;
end;

function TTabelaService.AbrirSql(pSql: String): TMemoryStream;
var
  vQry: TZQuery;
begin
  Result := TMemoryStream.Create;
  TConsoleUtil.LogData('TabelaService:AbrirSql: pSql',pSql);
  try
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(pSql);

      vQry.Open;

      TConsoleUtil.LogData('TabelaService:AbrirSql: Registros',vQry.RecordCount.ToString);
      FConexao.DataSetToStream(vQry, Result);

    finally
      vQry.Close;
    end;
  except
    on E: exception do
      raise Exception.Create(e.Message);
  end;
end;

function TTabelaService.ListasComboBox(pLista: TJSONData): TMemoryStream;
var
  vBuf: TBufDataset;
  vArray: TJSONArray;
  I: Integer;
begin
  Result := TMemoryStream.Create;
  vArray := pLista as TJSONArray;

  vBuf := TBufDataset.Create(nil);
  for I := 0 to Pred(vArray.Count) do
  begin
    vBuf.FieldDefs.Add(vArray[I].GetPath('nome').AsString, ftBlob);
  end;
  vBuf.CreateDataset;
  vBuf.Edit;
  for I := 0 to Pred(vArray.Count) do
  begin
    TBlobField(vBuf.FieldByName(vArray[I].GetPath('nome').AsString)).LoadFromStream(AbrirSql(vArray[I].GetPath('sql').AsString));
  end;
  vBuf.Post;
  vBuf.SaveToStream(Result, dfBinary);
  Result.Position := 0;

end;

function TTabelaService.Config(pTabela: String): TMemoryStream;
const
  SQL_EMPRESA =
     'select CODIGO, FANTASIA, RAZAO , CGC, ENDERECO, BAIRRO, CIDADE, CEP, TELEFONE, '+
            'UF, MSG1, MSG2, MSG3, MSG4, MSG5, ULTCON, ULTREF, MAXPEN, ULTGER, '+
            'DIAGER, PRXGER, ALTPRE, PCTOT, GRAVULSO, ANT1, ANT2, ANT3, ANT4, ANT5, '+
            'ULTCXA, MVCXA, CDOBS, MVACE, DECOM, TPCOB, SINPEN, VLCAR, DIAVENC, '+
            'TPVENC, VERSERCON, VERCODIGO, LEMA, TPREC, TPCXACLI, TPCXACOB, CNPJPREF, '+
            'CNAE, CTM, ITLSTSERV, EMAIL, ARQUIVOPFX, SENHAWEB, USERWEB, CHAVEACESSO, '+
            'FRASESECRETA, CERTSENHA, PATHACBR, EMITENFSE, CONSLOTE, NOMELONGO, '+
            'RTSIMPLES, PRODUCAO, ALIQISS, INSCMUN, IBGE, ENDRECCXA, CDMVCXASIGN, ULTOS, '+
            'ATENDIMENTO, IMGLOGIN, IMGTIPO, CDPIX, DTEMP, DTUSU, DTITG, DTIMG, DTCPT, DTEXE,'+
            'MODOCONEXAO, SERVIDOR, DBNOME, PORTA, PROTOCOLO, BDUSU, BDSEN, EXECUTAVEL '+
      'from EMPRESA where CODIGO = 1 ';
  SQL_USUARIOS =
      'select U.*, F.ENDERECO, F.BAIRRO, F.CEP, '+
      '       F.TELEFONE, C.NOME NMCID, C.UF '+
      'from USUARIOS U '+
      'left outer join FILIAIS F on F.CODIGO = U.FILIAL '+
      'left outer join CIDADES C on C.CODIGO = F.CIDADE '+
      'where U.ATIVO = 1 order by CODIGO';
  SQL_COMPUTADORES =
     'select C.CODIGO, C.NOME, C.OPERACAIXA, C.PORTA from COMPUTADORES C '+
     'order by C.CODIGO';
  SQL_ITGRU =
    'select I.GRUSU, I.ROTINA, I.PRIVIL, I.FORMULARIO, R.ACAO '+
    'from ITGRU I '+
    'inner join ROTINAS R on R.CODIGO=I.ROTINA '+
    'order by I.FORMULARIO, R.ACAO ';
  SQL_IMAGENS =
   'select CODIGO, IMAGEM from IMAGENS order by CODIGO';

begin

  Result := TMemoryStream.Create;

  TConsoleUtil.LogData('TabelaService:Config:',Format( 'pTabela: %s',[pTabela]));

  if pTabela.Equals('empresa')
  then Result := AbrirSql(SQL_EMPRESA)
  else if pTabela.Equals('usuarios')
  then Result := AbrirSql(SQL_USUARIOS)
  else if pTabela = 'computadores'
  then Result := AbrirSql(SQL_COMPUTADORES)
  else if pTabela = 'itgru'
  then Result := AbrirSql(SQL_ITGRU)
  else if pTabela = 'imagens'
  then Result := AbrirSql(SQL_IMAGENS);

end;

function TTabelaService.Status: String;
begin
  Result := FConexao.CheckDB;
end;

end.

