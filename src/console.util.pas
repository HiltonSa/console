unit console.util;

{$mode Delphi} {$H+}

interface

uses
  Classes, SysUtils, model.log, conexao.zeos, DateUtils, ZDataset;

type

  { TConsoleUtil }

  TConsoleUtil = class
    class function RetornaDataSql(aData: TDateTime): String;
    class function RetornaTimeStampSql(aData: TDateTime): String;
    class procedure LogErro(pUsuario, pMensagem: String);
    class procedure LogData(pIdentificador, pMensagem: String);
    class procedure GravaDataExecutavel;
  end;

implementation

{ TConsoleUtil }

class function TConsoleUtil.RetornaDataSql(aData: TDateTime): String;
const
  Meses : array[1..12] of string = ('jan', 'feb','mar','apr',
             'may','jun','jul','aug','sep','oct','nov','dec');
begin
  Result := Format('%d-%s-%d',[DayOf(Adata), Meses[MonthOf(Adata)],YearOf(Adata)]);
end;

class function TConsoleUtil.RetornaTimeStampSql(aData: TDateTime): String;
begin
  Result := Retornadatasql(Adata) + ' '+FormatDateTime('hh:mm:ss', Adata);
end;

class procedure TConsoleUtil.LogErro(pUsuario, pMensagem: String);
begin
  TLog.New.Usuario(pUsuario).Erro(pMensagem);
end;

class procedure TConsoleUtil.LogData(pIdentificador, pMensagem: String);
begin
  TLog.New.Identificador(pIdentificador).Log(pMensagem);
end;

class procedure TConsoleUtil.GravaDataExecutavel;
begin
  TDBConexaoZeos.New.ExecutarSQL('update EMPRESA set DTEXE = CURRENT_TIME where CODIGO = 1');
end;

end.

