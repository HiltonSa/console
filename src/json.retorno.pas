unit json.retorno;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fpjson;
type

  { IRetornoJson }

  IRetornoJson = interface
    ['{569AB87F-F267-4844-901B-9B2B408F8927}']
    function Sucesso(pValor: Boolean): IRetornoJson;
    function Mensagem(pValor: String): IRetornoJson;
    function Data(pValor: TJsonObject): IRetornoJson; overload;
    function Data(pValor: TJsonArray): IRetornoJson; overload;
    function Data: String; overload;
    function DataArray: TJSONArray;
    function Estrutura(pValor: TJsonArray): IRetornoJson;
    function ParaString: String;
    function DaString(pStrJson: String): IRetornoJson;
    function ToJson: TJsonObject;
    function ToStream: TMemoryStream;
  end;

  { TRetornoJson }

  TRetornoJson= class(TInterfacedObject, IRetornoJson)
  private
    FRetorno : TJsonObject;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IRetornoJson;

    function Sucesso(pValor: Boolean): IRetornoJson;
    function Mensagem(pValor: String): IRetornoJson;
    function Data(pValor: TJsonObject): IRetornoJson; overload;
    function Data(pValor: TJsonArray): IRetornoJson; overload;
    function Data: String; overload;
    function DataArray: TJSONArray;
    function DaString(pStrJson: String): IRetornoJson;
    function Estrutura(pValor: TJsonArray): IRetornoJson;
    function ParaString: String;
    function ToJson: TJsonObject;
    function ToStream: TMemoryStream;
  end;

implementation

{ TRetornoJson }

constructor TRetornoJson.Create;
begin
  FRetorno := TJsonObject.Create();
end;

destructor TRetornoJson.Destroy;
begin
  FreeAndNil(FRetorno);
  inherited Destroy;
end;

class function TRetornoJson.New: IRetornoJson;
begin
  Result := Self.Create;
end;

function TRetornoJson.Sucesso(pValor: Boolean): IRetornoJson;
begin
  Result := Self;
  FRetorno.Clear;
  FRetorno.Add('sucesso', pValor);
end;

function TRetornoJson.Mensagem(pValor: String): IRetornoJson;
begin
  Result := Self;
  FRetorno.Add('mensagem', pValor);
end;

function TRetornoJson.Data(pValor: TJsonObject): IRetornoJson;
begin
  Result := Self;
  FRetorno.Add('data', pValor);
end;

function TRetornoJson.Data(pValor: TJsonArray): IRetornoJson;
begin
  Result := Self;
  FRetorno.Add('data', pValor);
end;

function TRetornoJson.Data: String;
begin
  Result := FRetorno.FindPath('data').FormatJSON;
end;

function TRetornoJson.DataArray: TJSONArray;
begin
  if FRetorno.FindPath('data') <> nil
  then Result := TJSONArray(FRetorno.FindPath('data'))
  else Result := TJSONArray.Create;
end;

function TRetornoJson.DaString(pStrJson: String): IRetornoJson;
var
  vJson, vStru, vData: TJSONData;
begin
  vJson := GetJSON(pStrJson);
  FRetorno.Clear;
  FRetorno.Add('sucesso', vJson.FindPath('sucesso'));
  FRetorno.Add('mensagem', vJson.FindPath('mensagem'));

  if vJson.FindPath('estrutura') <> nil
  then vStru := vJson.FindPath('estrutura')
  else vStru := TJSONArray.Create;
  FRetorno.Add('estrutura', vStru);

  if vJson.FindPath('data') <> nil
  then vData := vJson.FindPath('data')
  else vData := TJSONArray.Create;
  FRetorno.Add('data', vData);
  Result := Self;
end;

function TRetornoJson.Estrutura(pValor: TJsonArray): IRetornoJson;
begin
  Result := Self;
  FRetorno.Add('estrutura', pValor);
end;

function TRetornoJson.ParaString: String;
begin
  Result := FRetorno.FormatJSON;
end;

function TRetornoJson.ToJson: TJsonObject;
begin
  Result := FRetorno;
end;

function TRetornoJson.ToStream: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  FRetorno.DumpJSON(Result);
  Result.Position := 0;
end;

end.


