unit Rota.Base;

{$mode delphi} {$H+}

interface

uses
  Classes, SysUtils, Horse, console.util;
type

  { TBase }

  TBase = class
    public
      class procedure Rota;
  end;

implementation

//uses registro.service;

const
   REPOSITORIO = 'arquivos\';

{ TBase }

procedure onDownload(aReq: THorseRequest; aRes: THorseResponse);
var
  vNome: String;
begin
  vNome := Format('%s%s%s',[ExtractFilePath(ParamStr(0)), REPOSITORIO,
           aReq.Query.Field('nome').AsString ]);
  TConsoleUtil.LogData('RotaBase:OnDownload:vNome', vNome);
  if FileExists(vNome)
  then aRes.ContentType('application/octet-stream')
           .Download( vNome )
  else aRes.Status(404).Send(Format('Erro: %s nao encontrado!', [vNome]));
end;

procedure onMobile(aReq: THorseRequest; aRes: THorseResponse);
const
  NOME_ARQ = 'cobipax.apk';
var
  vNome: String;
begin
  vNome := Format('%s%s%s',[ExtractFilePath(ParamStr(0)), REPOSITORIO, NOME_ARQ ]);
  TConsoleUtil.LogData('RotaBase:onMobile:vNome', vNome);
  if FileExists(vNome)
  then aRes.ContentType('application/octet-stream')
           .Download(vNome)
  else aRes.Status(404).Send(Format('Erro: %s nao encontrado!', [vNome]));
end;

procedure onCliente(aReq: THorseRequest; aRes: THorseResponse);
const
  NOME_ARQ = 'ControleContratosSetup.exe';
var
  vNome: String;
begin
  vNome := Format('%s%s%s',[ExtractFilePath(ParamStr(0)), REPOSITORIO, NOME_ARQ ]);
  TConsoleUtil.LogData('RotaBase:onCliente:vNome', vNome);
  if FileExists(vNome)
  then aRes.ContentType('application/octet-stream')
           .Download(vNome)
  else aRes.Status(404).Send(Format('Erro: %s nao encontrado!', [vNome]));
end;

procedure onUpload(aReq: THorseRequest; aRes: THorseResponse);
var
  vPasta,
  vNome: String;
  vStream : TMemoryStream;
begin
  try
    vStream := TMemoryStream.Create;
    try
      vNome := aReq.ContentFields.Field('nome').AsString;

      if vNome.Trim.IsEmpty
      then Raise Exception.Create('Nome arquivo invalido!');

      vStream.LoadFromStream(aReq.ContentFields.Field('conteudo').AsStream);

      if vStream.Size = 0
      then Raise Exception.Create('Arquivo Invalido');

      vPasta := Format('%s%s',[ExtractFilePath(ParamStr(0)), REPOSITORIO]);
      if not DirectoryExists(vPasta)
      then CreateDir(vPasta);

      vStream.Position := 0;
      vStream.SaveToFile(Format('%s%s',[vPasta, vNome]));

      aRes.Send('OK');
    except
      on E:Exception do aRes.Send(E.Message);
    end;
  finally
    vStream.Free;
  end;
end;

procedure onUpWin(aReq: THorseRequest; aRes: THorseResponse);
var
  vPasta,
  vNome: String;
  vStream : TMemoryStream;
begin
  try
    vStream := TMemoryStream.Create;
    try
      vNome := 'ControleContratos.exe';

      vStream.LoadFromStream(aReq.ContentFields.Field('conteudo').AsStream);

      if vStream.Size = 0
      then Raise Exception.Create('Arquivo Invalido');

      vPasta := Format('%s%s',[ExtractFilePath(ParamStr(0)), REPOSITORIO]);
      if not DirectoryExists(vPasta)
      then CreateDir(vPasta);

      vStream.Position := 0;
      vStream.SaveToFile(Format('%s%s',[vPasta, vNome]));

      TConsoleUtil.GravaDataExecutavel;
      aRes.Send('OK');
    except
      on E:Exception do aRes.Send(E.Message);
    end;
  finally
    vStream.Free;
  end;
end;

procedure OnStatus(aReq: THorseRequest; aRes: THorseResponse; aNext: TNextProc);
begin
  aRes.ContentType('text/html; charset=UTF-8')
      .Send(Format('<H1>API Controle Contratos (Horse versão %s)<H1>',[THorse.Version]));
end;

procedure OnPing(aReq: THorseRequest; aRes: THorseResponse; aNext: TNextProc);
begin
  aRes.ContentType('text/html; charset=UTF-8')
      .Send(Format('<H1>Pong!<H1>',[THorse.Version]));
end;
class procedure TBase.Rota;
begin
  THorse.Get('/', OnStatus)
        .Post('/upload', onUpload)
        .Post('/upwin', onUpWin)
        .Get('/download', onDownload)
        .Get('/cliente', onCliente)
        .Get('/mobile', onMobile)
        .Get('/ping', OnPing);
end;

end.

