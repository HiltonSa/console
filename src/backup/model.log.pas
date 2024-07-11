unit model.log;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil;

type

  iLog = interface
    ['{E2CCF4F3-B157-4849-A847-403834255191}']
    function Usuario(pValue: String): iLog;
    function Identificador(pValue: String): iLog;
    function Log(pMsg: String): iLog;
    function Erro(pMsg: String): iLog;
  end;

  { TLog }

  TLog = class(TInterfacedObject, iLog)
    private
      FArquivo       : TextFile;
      FNomeLog       : String;
      FUsuario       : String;
      FIdentificador : String;
      FNomeComputador: String;
    public
      constructor Create;
      destructor Destroy; override;
      class function New: iLog;

      function Usuario(pValue: String): iLog;
      function Identificador(pValue: String): iLog;
      function Log(pMsg: String): iLog;
      function Erro(pMsg: String): iLog;
  end;

implementation


{ TLog }

constructor TLog.Create;
begin
  FNomeComputador := '';
end;

destructor TLog.Destroy;
begin
  inherited Destroy;
end;

class function TLog.New: iLog;
begin
  Result := Self.Create;
end;

function TLog.Usuario(pValue: String): iLog;
begin
  FUsuario := pValue;
  Result := Self;
end;

function TLog.Identificador(pValue: String): iLog;
begin
  Result := Self;
  FIdentificador := pValue;
end;

function TLog.Log(pMsg: String): iLog;
var
  vMsg: String;
begin
  Result := Self;
  FNomeLog := 'console.log';
  AssignFile(FArquivo, FNomeLog);

  if FileExists(FNomeLog) { *Converted from FileExists* }
  then Append(FArquivo) { se existir, apenas adiciona linhas }
  else ReWrite(FArquivo); { cria um novo se não existir }

  vMsg := Format('%s: Msg: %s ',[DateTimeToStr(Now), pMsg]);
  if Length(Trim(FIdentificador)) > 0
  then vMsg := Format('%s: Ident.: %s Msg: %s ',[DateTimeToStr(Now), FIdentificador, pMsg]);
  try
    WriteLn(FArquivo, vMsg);
    WriteLn(FArquivo,'----------------------------------------------------------------------');
  finally
    CloseFile(FArquivo)
  end;
end;

function TLog.Erro(pMsg: String): iLog;
begin
  Result := Self;
  FNomeLog := 'console.err');
  AssignFile(FArquivo, FNomeLog);

  if FileExists(FNomeLog) { *Converted from FileExists* }
  then Append(FArquivo) { se existir, apenas adiciona linhas }
  else ReWrite(FArquivo); { cria um novo se não existir }

  try
    WriteLn(FArquivo, Format('%s: Usuario: %s Computador: %s ',[DateTimeToStr(Now), FUsuario, FNomeComputador]));
    WriteLn(FArquivo, 'Erro:' + pMsg);
    WriteLn(FArquivo,'----------------------------------------------------------------------');
  finally
    CloseFile(FArquivo)
  end;
end;

end.

