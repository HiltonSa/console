unit rota.pagamento;

{$mode Delphi} {$H+}

interface

uses
  Classes, SysUtils, DateUtils, Horse;
type

  { TRotaPagamento }

  TRotaPagamento = class
    public
      class procedure RotaPagamento;
  end;

implementation

uses dbconfig, pagamento.service, console.util;

{ TRotaPagamento }

procedure OnRecebimento(aReq: THorseRequest; aRes: THorseResponse; aNext: TNextProc);
begin
  TConsoleUtil.LogData('OnRecebimento: aReq.body', aReq.Body);
  aRes.ContentType('application/json; charset=UTF-8')
      .Send( TPagamentoService.New.Recebimento( aReq.Body ) );
end;

procedure OnRepasse(aReq: THorseRequest; aRes: THorseResponse; aNext: TNextProc);
begin
  TConsoleUtil.LogData('OnRepasse: aReq.body', aReq.Body);
  aRes.ContentType('application/json; charset=UTF-8')
      .Send( TPagamentoService.New.Repasse( aReq.Body ) );
end;

procedure OnReembolso(aReq: THorseRequest; aRes: THorseResponse; aNext: TNextProc);
begin
  TConsoleUtil.LogData('OnReembolso: aReq.body', aReq.Body);
  aRes.ContentType('application/json; charset=UTF-8')
      .Send( TPagamentoService.New.Reembolso( aReq.Body ) );
end;

class procedure TRotaPagamento.RotaPagamento;
begin
  THorse.Post('/recebimento', OnRecebimento)
        .Post('/repasse', OnRepasse)
        .Post('/reembolso', OnReembolso);
end;

end.

