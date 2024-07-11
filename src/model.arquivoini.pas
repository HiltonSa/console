unit model.arquivoini;

{$mode delphi}

interface

uses
  Classes, Sysutils, Controls, IniFiles, Forms;

type

   iArquivoIni = interface
     ['{99241DD4-27D1-4A4B-BE3B-1E20DFD5B3FD}']
     function ArquivoExiste: Boolean;
     function AtualizarValores: iArquivoIni;
     function Gravar: iArquivoIni;
     function ModoConexao: Integer; overload;
     function ModoConexao(aValue: Integer): iArquivoIni; overload;
     function Servidor: String; overload;
     function Servidor(aValue: String): iArquivoIni; overload;
     function BancoDados: String; overload;
     function BancoDados(aValue: String): iArquivoIni; overload;
     function Porta: Integer; overload;
     function Porta(aValue: Integer): iArquivoIni; overload;
     function Protocolo: String; overload;
     function Protocolo(aValue: String): iArquivoIni; overload;
     function Usuario: String; overload;
     function Usuario(aValue: String): iArquivoIni; overload;
     function Senha: String; overload;
     function Senha(aValue: String): iArquivoIni; overload;
     function Filial: Integer; overload;
     function Filial(aValue: Integer): iArquivoIni; overload;
     function DiretorioBkp: String; overload;
     function DiretorioBkp(aValue: String): iArquivoIni; overload;
     function ExecutarRotinas: Boolean; overload;
     function ExecutarRotinas(aValue: Boolean): iArquivoIni; overload;
     function CaminhoCompactador: String; overload;
     function CaminhoCompactador(aValue: String): iArquivoIni; overload;
     function ApagarFbk: Boolean; overload;
     function ApagarFbk(aValue: Boolean): iArquivoIni; overload;
     function BancoLocal: String; overload;
     function BancoLocal(aValue: String): iArquivoIni; overload;
     function ProtocoloLocal: String; overload;
     function ProtocoloLocal(aValue: String): iArquivoIni; overload;
     function NomeExecutavel: String; overload;
     function NomeExecutavel(aValue: String): iArquivoIni; overload;
     function Repositorio: String; overload;
     function Repositorio(aValue: String): iArquivoIni; overload;
     function NomePooler: String; overload;
     function NomePooler(aValue: String): iArquivoIni; overload;
   end;

   { TArquivoIni }


   TArquivoIni = class (TInterfacedObject, iArquivoIni)
     private
       //class var FInstancia: TArquivoIni;

       FNomeArquivo: String;

       FModoConexao: Integer;
       FServidor: string;
       FBancoDados: string;
       FPorta: Integer;
       FProtocolo: String;
       FUsuario: String;
       FSenha: String;
       FFilialLog: Integer;
       FDirBkp: String;
       FExecRot: Boolean;
       FCaminhoCompactador: String;
       FApagarFbk: Boolean;

       FBancoLocal: String;
       FProtocoloLocal: String;

       FNomeExecutavel: String;
       FRepositorio: String;
       FPooler: String;

       procedure ValorarCampos;

     public
       constructor Create;
       destructor Destroy; override;
       class function New: iArquivoIni;

       function AtualizarValores: iArquivoIni;
       function ArquivoExiste: Boolean;
       function Gravar: iArquivoIni;
       function ModoConexao: Integer; overload;
       function ModoConexao(aValue: Integer): iArquivoIni; overload;
       function Servidor: String; overload;
       function Servidor(aValue: String): iArquivoIni; overload;
       function BancoDados: String; overload;
       function BancoDados(aValue: String): iArquivoIni; overload;
       function Porta: Integer; overload;
       function Porta(aValue: Integer): iArquivoIni; overload;
       function Protocolo: String; overload;
       function Protocolo(aValue: String): iArquivoIni; overload;
       function Usuario: String; overload;
       function Usuario(aValue: String): iArquivoIni; overload;
       function Senha: String; overload;
       function Senha(aValue: String): iArquivoIni; overload;
       function Filial: Integer; overload;
       function Filial(aValue: Integer): iArquivoIni; overload;
       function DiretorioBkp: String; overload;
       function DiretorioBkp(aValue: String): iArquivoIni; overload;
       function ExecutarRotinas: Boolean; overload;
       function ExecutarRotinas(aValue: Boolean): iArquivoIni; overload;
       function CaminhoCompactador: String; overload;
       function CaminhoCompactador(aValue: String): iArquivoIni; overload;
       function ApagarFbk: Boolean; overload;
       function ApagarFbk(aValue: Boolean): iArquivoIni; overload;
       function BancoLocal: String; overload;
       function BancoLocal(aValue: String): iArquivoIni; overload;
       function ProtocoloLocal: String; overload;
       function ProtocoloLocal(aValue: String): iArquivoIni; overload;
       function NomeExecutavel: String; overload;
       function NomeExecutavel(aValue: String): iArquivoIni; overload;
       function Repositorio: String; overload;
       function Repositorio(aValue: String): iArquivoIni; overload;
       function NomePooler: String; overload;
       function NomePooler(aValue: String): iArquivoIni; overload;
   end;


implementation


const
  NOME_INI = 'contcon.ini';
  TAG_CONEXAO = 'CON';
  TAG_LOCAL   = 'LOC';
  TAG_EXECUTAVEL = 'EXE';

  MODO_CONEXAO = 'ModoConexao';
  TAG_SERVIDOR = 'Servidor';
  BANCO_DADOS  = 'BD';
  TAG_PORTA    = 'Porta';
  TAG_PROTOCOLO= 'Protocolo';
  TAG_USUARIO  = 'Resu';
  TAG_SENHA    = 'Ssap';
  TAG_FILIAL   = 'Filial';
  DIR_BKP      = 'DirBkp';
  EXECUTA_ROT  = 'ExecRot';
  COMPACTADOR  = 'Compactador';
  APAGA_FBK    = 'ApagarFbk';
  BANCO_LOCAL  = 'BD';
  PROTOCOLO_LOCAL = 'protocolo';
  EXECUTAVEL      = 'executavel';
  PASTA_BIN       = 'pastabin';
  NOME_POOLER     = 'PoolerName';

{ TArquivoIni }

procedure TArquivoIni.ValorarCampos;
var
  arqIni: TIniFile;
begin
  arqIni := TIniFile.Create(FNomeArquivo);
  try
     FModoConexao        := arqIni.ReadInteger(TAG_CONEXAO,MODO_CONEXAO, 0);
     FServidor           := arqIni.ReadString(TAG_CONEXAO,TAG_SERVIDOR,'localhost');
     FBancoDados         := arqIni.ReadString(TAG_CONEXAO,BANCO_DADOS,'contratos');
     FPorta              := arqIni.ReadInteger(TAG_CONEXAO,TAG_PORTA, 3050);
     FProtocolo          := arqIni.ReadString(TAG_CONEXAO,TAG_PROTOCOLO,'firebird');
     FUsuario            := arqIni.ReadString(TAG_CONEXAO,TAG_USUARIO,'SYSDBA');
     FSenha              := arqIni.ReadString(TAG_CONEXAO,TAG_SENHA,'masterkey');
     FFilialLog          := arqIni.ReadInteger(TAG_CONEXAO,TAG_FILIAL, 1);
     FDirBkp             := arqIni.ReadString(TAG_CONEXAO,DIR_BKP,'');
     FExecRot            := (arqIni.ReadInteger(TAG_CONEXAO,EXECUTA_ROT, 1) = 1);
     FCaminhoCompactador := arqIni.ReadString(TAG_CONEXAO,COMPACTADOR,'');
     FApagarFbk          := (arqIni.ReadInteger(TAG_CONEXAO,APAGA_FBK, 1) = 1);
     FPooler             := arqIni.ReadString(TAG_CONEXAO,NOME_POOLER,'TMethodClass.PoolerDB');

     FBancoLocal     := arqIni.ReadString(TAG_LOCAL,BANCO_LOCAL,'contcon.db');
     FProtocoloLocal := arqIni.ReadString(TAG_LOCAL,PROTOCOLO_LOCAL,'sqlite-3');

     FNomeExecutavel := arqIni.ReadString(TAG_EXECUTAVEL,EXECUTAVEL,'ControleContratos');
     FRepositorio    := arqIni.ReadString(TAG_EXECUTAVEL,PASTA_BIN,'bin');
  finally
    arqIni.Free;
  end;

end;

function TArquivoIni.ArquivoExiste: Boolean;
begin
  Result := FileExists(FNomeArquivo)
end;

constructor TArquivoIni.Create;
begin
  FNomeArquivo := ExtractFilePath(Application.ExeName)+NOME_INI;
  ValorarCampos;
end;

destructor TArquivoIni.Destroy;
begin
  inherited Destroy;
end;

class function TArquivoIni.New: iArquivoIni;
begin
  //if not Assigned(FInstancia)
  //then FInstancia := Self.Create;
  //Result := FInstancia;
  Result := Self.Create;
end;

function TArquivoIni.AtualizarValores: iArquivoIni;
begin
  ValorarCampos;
  Result := Self;
end;

function TArquivoIni.Gravar: iArquivoIni;
var
  arqIni: TIniFile;
begin
  Result := Self;
  arqIni := TIniFile.Create(FNomeArquivo);

  try
     arqIni.WriteInteger(TAG_CONEXAO,MODO_CONEXAO, FModoConexao);
     arqIni.WriteString(TAG_CONEXAO,TAG_SERVIDOR,FServidor);
     arqIni.WriteString(TAG_CONEXAO,BANCO_DADOS,FBancoDados);
     arqIni.WriteInteger(TAG_CONEXAO,TAG_PORTA, FPorta);
     arqIni.WriteString(TAG_CONEXAO,TAG_PROTOCOLO,FProtocolo);
     arqIni.WriteString(TAG_CONEXAO,TAG_USUARIO,FUsuario);
     arqIni.WriteString(TAG_CONEXAO,TAG_SENHA,FSenha);
     arqIni.WriteInteger(TAG_CONEXAO,TAG_FILIAL, FFilialLog);
     arqIni.WriteString(TAG_CONEXAO,DIR_BKP,FDirBkp);

     if FExecRot
     then arqIni.WriteInteger(TAG_CONEXAO,EXECUTA_ROT, 1)
     else arqIni.WriteInteger(TAG_CONEXAO,EXECUTA_ROT, 0);

     arqIni.WriteString(TAG_CONEXAO,COMPACTADOR,FCaminhoCompactador);

     if FApagarFbk
     then arqIni.WriteInteger(TAG_CONEXAO,APAGA_FBK, 1)
     else arqIni.WriteInteger(TAG_CONEXAO,APAGA_FBK, 0);

     arqIni.WriteString(TAG_CONEXAO,NOME_POOLER,FPooler);

     arqIni.WriteString(TAG_LOCAL,BANCO_LOCAL,FBancoLocal);
     arqIni.WriteString(TAG_LOCAL,PROTOCOLO_LOCAL,FProtocoloLocal);

     arqIni.WriteString(TAG_EXECUTAVEL,EXECUTAVEL,FNomeExecutavel);
     arqIni.WriteString(TAG_EXECUTAVEL,PASTA_BIN,FRepositorio);
  finally
    arqIni.Free;
  end;

end;

function TArquivoIni.ModoConexao: Integer;
begin
  Result := FModoConexao;
end;

function TArquivoIni.ModoConexao(aValue: Integer): iArquivoIni;
begin
  FModoConexao := aValue;
  Result := Self;
end;

function TArquivoIni.Servidor: String;
begin
  Result := FServidor;
end;

function TArquivoIni.Servidor(aValue: String): iArquivoIni;
begin
  FServidor := aValue;
  Result := Self;
end;

function TArquivoIni.BancoDados: String;
begin
  Result := FBancoDados;
end;

function TArquivoIni.BancoDados(aValue: String): iArquivoIni;
begin
  FBancoDados := aValue;
  Result := Self;
end;

function TArquivoIni.Porta: Integer;
begin
  Result := FPorta;
end;

function TArquivoIni.Porta(aValue: Integer): iArquivoIni;
begin
  FPorta := aValue;
  Result := Self;
end;

function TArquivoIni.Protocolo: String;
begin
  Result := FProtocolo;
end;

function TArquivoIni.Protocolo(aValue: String): iArquivoIni;
begin
  FProtocolo := aValue;
  Result := Self;
end;

function TArquivoIni.Usuario: String;
begin
  Result := FUsuario;
end;

function TArquivoIni.Usuario(aValue: String): iArquivoIni;
begin
  FUsuario := aValue;
  Result := Self;
end;

function TArquivoIni.Senha: String;
begin
  Result := FSenha;
end;

function TArquivoIni.Senha(aValue: String): iArquivoIni;
begin
  FSenha := aValue;
  Result := Self;
end;

function TArquivoIni.Filial: Integer;
begin
  Result := FFilialLog;
end;

function TArquivoIni.Filial(aValue: Integer): iArquivoIni;
begin
  FFilialLog := aValue;
  Result := Self;
end;

function TArquivoIni.DiretorioBkp: String;
begin
  Result := FDirBkp;
end;

function TArquivoIni.DiretorioBkp(aValue: String): iArquivoIni;
begin
  FDirBkp := aValue;
  Result := Self;
end;

function TArquivoIni.ExecutarRotinas: Boolean;
begin
  Result := FExecRot;
end;

function TArquivoIni.ExecutarRotinas(aValue: Boolean): iArquivoIni;
begin
  FExecRot := aValue;
  Result := Self;
end;

function TArquivoIni.CaminhoCompactador: String;
begin
  Result := FCaminhoCompactador;
end;

function TArquivoIni.CaminhoCompactador(aValue: String): iArquivoIni;
begin
  FCaminhoCompactador := aValue;
  Result := Self;
end;

function TArquivoIni.ApagarFbk: Boolean;
begin
  Result := FApagarFbk;
end;

function TArquivoIni.ApagarFbk(aValue: Boolean): iArquivoIni;
begin
  FApagarFbk := aValue;
  Result := Self;
end;

function TArquivoIni.BancoLocal: String;
begin
  Result := FBancoLocal;
end;

function TArquivoIni.BancoLocal(aValue: String): iArquivoIni;
begin
  FBancoLocal := aValue;
  Result := Self;
end;

function TArquivoIni.ProtocoloLocal: String;
begin
  Result := FProtocoloLocal;
end;

function TArquivoIni.ProtocoloLocal(aValue: String): iArquivoIni;
begin
  FProtocoloLocal := aValue;
  Result := Self;
end;

function TArquivoIni.NomeExecutavel: String;
begin
  Result := FNomeExecutavel;
end;

function TArquivoIni.NomeExecutavel(aValue: String): iArquivoIni;
begin
  FNomeExecutavel := aValue;
  Result := Self;
end;

function TArquivoIni.Repositorio: String;
begin
  Result := FRepositorio;
end;

function TArquivoIni.Repositorio(aValue: String): iArquivoIni;
begin
  FRepositorio := aValue;
  Result := Self;
end;

function TArquivoIni.NomePooler: String;
begin
  Result := FPooler;
end;

function TArquivoIni.NomePooler(aValue: String): iArquivoIni;
begin
  FPooler := aValue;
  Result := Self;
end;

end.

