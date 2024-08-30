unit conexao.zeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset,
  ZStoredProcedure, dbconfig, db, BufDataset{, model.arquivoini};

type

  { IDBConexao }

  IDBConexao = interface
    ['{3B9E7F01-FC22-4326-9314-A9B59AA6D99E}']
    function Connected: Boolean; overload;
    function Connected(aValue: Boolean): IDBConexao; overload;
    function EmTransacao: Boolean;
    function IniciarTransacao: IDBConexao;
    function GravarTransacao: IDbConexao;
    function CancelarTransacao: IDBConexao;
    function GetQuery: TZQuery;
    function GetProcedure: TZStoredProc;
    function DataSetToStream(pDataset: TZQuery; pStream: TMemoryStream): IDBConexao;
    function GetListasComboBox: TMemoryStream;
    function GetDataset(pSql: String): TDataSet;
    function Paginar(aPagina, aLimite: Integer): String;
    function CheckDB: String;
    function GenGUID: String;
    procedure ExecutarSQL(pSql: String);
  end;

  { TDBConexaoZeos }

  TDBConexaoZeos = class(TInterfacedObject, IDBConexao)
    private
      FConexao : TZConnection;
      procedure OnBeforeConnect(Sender: TObject);
    public
      constructor Create;
      destructor Destroy; override;
      class function New: IDBConexao;

      function Connected: Boolean; overload;
      function Connected(aValue: Boolean): IDBConexao; overload;
      function EmTransacao: Boolean;
      function IniciarTransacao: IDBConexao;
      function GravarTransacao: IDbConexao;
      function CancelarTransacao: IDBConexao;
      function GetQuery: TZQuery;
      function GetProcedure: TZStoredProc;
      function DataSetToStream(pDataset: TZQuery; pStream: TMemoryStream): IDBConexao;
      function GetListasComboBox: TMemoryStream;
      function GetDataset(pSql: String): TDataSet;
      procedure ExecutarSQL(pSql: String);
      function Paginar(aPagina, aLimite: Integer): String;
      function CheckDB: String;
      function GenGUID: String;
  end;

implementation

uses json.retorno;

{ TDBConexaoZeos }

procedure TDBConexaoZeos.OnBeforeConnect(Sender: TObject);
begin
  FConexao.Protocol := DB_PROTOCOL;
  FConexao.HostName := DB_HOST;
  FConexao.Port     := DB_PORT;
  FConexao.Database := DB_DATABASE;
  FConexao.User     := DB_USERNAME;
  FConexao.Password := DB_PASSWORD;
  FConexao.LibraryLocation := DB_PATH_LIB;
  //FConexao.ControlsCodePage := cCP_UTF8;
  FConexao.ClientCodepage := 'WIN1252';
end;

constructor TDBConexaoZeos.Create;
begin
  FConexao := TZConnection.Create(nil);
  FConexao.BeforeConnect := @onBeforeConnect;
end;

destructor TDBConexaoZeos.Destroy;
begin
  FConexao.Connected := False;
  FreeAndNil(FConexao);
  inherited Destroy;
end;

class function TDBConexaoZeos.New: IDBConexao;
begin
  Result := Self.Create;
end;

function TDBConexaoZeos.Connected: Boolean;
begin
  Result := FConexao.Connected;
end;

function TDBConexaoZeos.Connected(aValue: Boolean): IDBConexao;
begin
  FConexao.Connected := aValue;
  Result := Self;
end;

function TDBConexaoZeos.EmTransacao: Boolean;
begin
  Result := FConexao.InTransaction;
end;

function TDBConexaoZeos.IniciarTransacao: IDBConexao;
begin
  FConexao.Connected := False;
  FConexao.Connected := True;
  FConexao.StartTransaction;
  Result := Self;
end;

function TDBConexaoZeos.GravarTransacao: IDbConexao;
begin
  Result := Self;
  if FConexao.InTransaction
  then FConexao.Commit;
end;

function TDBConexaoZeos.CancelarTransacao: IDBConexao;
begin
  Result := Self;
  if FConexao.InTransaction
  then FConexao.Rollback;
end;

function TDBConexaoZeos.GetQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := FConexao;
end;

function TDBConexaoZeos.GetProcedure: TZStoredProc;
begin
  Result := TZStoredProc.Create(nil);
  Result.Connection := FConexao;
end;

function TDBConexaoZeos.DataSetToStream(pDataset: TZQuery;
  pStream: TMemoryStream): IDBConexao;
var
  vBufDataset: TBufDataset;
begin
  Result := Self;
  vBufDataset := TBufDataset.Create(nil);
  try
    pDataset.Open;
    vBufDataset.CopyFromDataset(pDataset);
    vBufDataset.SaveToStream(pStream);
    pStream.Position := 0;
  finally
    FreeAndNil(vBufDataset);
  end;
end;

function TDBConexaoZeos.GetListasComboBox: TMemoryStream;
var
  vBufDataset: TBufDataset;
  vDataset: TDataSet;
begin
  Result := TMemoryStream.Create;
  vBufDataset := TBufDataset.Create(nil);
  vBufDataset.FieldDefs.Add('', ftBlob);
  try
    vDataset := GetDataset('');
    vBufDataset.CopyFromDataset(VDataset);
    vBufDataset.SaveToStream(Result);
    Result.Position := 0;
  finally
    FreeAndNil(vBufDataset);
  end;
end;

function TDBConexaoZeos.GetDataset(pSql: String): TDataSet;
var
  vQry: TZQuery;
begin
  vQry := TZQuery.Create(nil);
  try
    vQry.Connection := FConexao;
    vQry.Close;
    vQry.SQL.Clear;
    vQry.SQL.Add(pSql);
    vQry.Open;
    Result := vQry;
  finally
    FreeAndNil(vQry);
  end;

end;

procedure TDBConexaoZeos.ExecutarSQL(pSql: String);
var
  vQry: TZQuery;
begin
  vQry := TZQuery.Create(nil);
  try
    vQry.Connection := FConexao;
    vQry.Close;
    vQry.SQL.Clear;
    vQry.SQL.Add(pSql);
    vQry.ExecSQL;
  finally
    FreeAndNil(vQry);
  end;
 end;

function TDBConexaoZeos.Paginar(aPagina, aLimite: Integer): String;
var
  lRows,
  lTo: Integer;
begin
  Result := '';
  if (aPagina > 0)
  then begin
    if (aLimite <= 0)
	then aLimite := 50;
	lRows := aPagina;
	lTo := aLimite;
	if(aPagina > 1)
	then begin
	  lRows := (aLimite * (aPagina - 1));
	  lTo := (lRows + aLimite);
	  Inc(lRows);
	end;
	Result := Format('rows %d to %d',[lRows, lTo]);
  end;
end;

function TDBConexaoZeos.CheckDB: String;
var
  vMsg: String;
begin
  vMsg := Format('Servidor Database (%s:%d) conectado.',[DB_HOST, DB_PORT]);
  try
    FConexao.Connected := false;
    FConexao.Connected := True;
    FConexao.Connected := false;
    Result := TRetornoJson.New
                          .Sucesso(True)
                          .Mensagem(vMsg)
                          .ParaString;
  except
    on E: exception do
	  Result := TRetornoJson.New.Sucesso(False).Mensagem(E.Message).ParaString;
  end;
end;

function TDBConexaoZeos.GenGUID: String;
begin
  Result := TGUID.NewGuid.ToString;
  Result := StringReplace(Result,'{','',[rfReplaceAll]);
  Result := StringReplace(Result,'}','',[rfReplaceAll]);
end;

end.

