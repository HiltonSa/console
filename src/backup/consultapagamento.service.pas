unit consultapagamento.service;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, DB, conexao.zeos, console.util, ZDataset,
  Dataset.Serialize;
type

  { IRegistroUsuario }

  IConsultaPagamentoService = interface
     ['{F6C37CB3-89FD-4D9C-B0D8-2B7A8C6D9FAE}']
     function CarteiraStream(pCobrador: Integer; pDiaVenc: TDateTime): TMemoryStream;
     function ProximoAcerto(pCobrador: Integer; pDtRec: TDateTime): String;
     function ConsultarPagamento(pRecibo: String): String;
     function FormasPagamento: String;
     function GetImagem(pCodigo: Integer): TMemoryStream;
     function Associado(pAssociado: Integer): String;
     function ConsultaNome(pCobrador: Integer;pNome: String): TMemoryStream;
     function DataRecebimento(pCobrador: Integer): TMemoryStream;
  end;

  { TConsultaPagamentoService }

  TConsultaPagamentoService = class(TInterfacedObject, IConsultaPagamentoService)
    private
      FConexao: IDBConexao;
      function RetornaParcelas(pInicio, pFim: TDateTime): Integer;
      function RetornaPerfilCobrador(pCodigo: Integer; var pCidade:Integer): Integer;
    public
      constructor Create;
      destructor Destroy; override;
      class function New: IConsultaPagamentoService;
      function CarteiraStream(pCobrador: Integer; pDiaVenc: TDateTime): TMemoryStream;
      function ProximoAcerto(pCobrador: Integer; pDtRec: TDateTime): String;
      function ConsultarPagamento(pRecibo: String): String;
      function FormasPagamento: String;
      function GetImagem(pCodigo: Integer): TMemoryStream;
      function Associado(pAssociado: Integer): String;
      function ConsultaNome(pCobrador: Integer;pNome: String): TMemoryStream;
      function DataRecebimento(pCobrador: Integer): TMemoryStream;

  end;

implementation

uses json.retorno, fpjson;

{ TConsultaPagamentoService }

function TConsultaPagamentoService.RetornaParcelas(pInicio, pFim: TDateTime
  ): Integer;
var
  vRef, vFim: TDateTime;
  vDia, vMes, vAno: Word;
  vPcl: Integer;
begin
  vRef := pInicio;
  vFim := StartOfTheMonth(pFim);
  vPcl := 0;
  while vRef<vFim do
  begin
    DecodeDate(vRef, vAno, vMes, vDia);
    Inc(vPcl);
    Inc(vMes);
    if vMes>12
    then begin
      Inc(vAno);
      vMes := 1;
    end;
    vRef := EncodeDate(vAno, vMes, 1);
  end;
  Result := vPcl;
end;

function TConsultaPagamentoService.RetornaPerfilCobrador(pCodigo: Integer;
  var pCidade: Integer): Integer;
var
  vQry: TZQuery;
  vSql: String;
begin
  Result := 0;
  vSql := Format('select U.PERFIL, C.CIDADE from COBRADORES C '+
                 'inner join USUWEB U on U.CONTCON = C.CODIGO '+
                 'where C.codigo = %d', [pCodigo]);
  vQry := FConexao.GetQuery;
  try
    vQry.Close;
    vQry.SQL.Clear;
    vQry.SQL.Add(vSql);
    vQry.Open;

    pCidade := 0;
    if not vQry.EOF
    then begin
      pCidade := vQry.FieldByName('CIDADE').AsInteger;
      Result := vQry.FieldByName('PERFIL').AsInteger;
    end
  finally
    vQry.Close;
    FreeAndNil(vQry);
  end;


end;

constructor TConsultaPagamentoService.Create;
begin
  FConexao := TDBConexaoZeos.New;
end;

destructor TConsultaPagamentoService.Destroy;
begin
  inherited Destroy;
end;

class function TConsultaPagamentoService.New: IConsultaPagamentoService;
begin
  Result := Self.Create;
end;

function TConsultaPagamentoService.CarteiraStream(pCobrador: Integer;
  pDiaVenc: TDateTime): TMemoryStream;
const
  SQL_CARTEIRA =
    'SELECT C.CODIGO, C.CODAREA, C.DCAREA, C.COBRADOR, C.NMCOB, C.INIREF, '+
    '       C.DIAVENC, C.MENSALIDADE,C.GRUPO, C.DCGRU, C.NOME, '+
    '       TRANSLATOR(C.ENDERECO) ENDERECO, TRANSLATOR(C.BAIRRO ) BAIRRO, '+
    '       TRANSLATOR(C.REFERENCIA) REFERENCIA, C.NMCID, C.UF, '+
    '       trim(replace(C.CEP, ascii_char(44), ascii_char(32))) CEP, '+
    '       C.TELEFONE, C.CELULAR, C.LATITUDE, C.LONGITUDE, C.RFATUAL, '+
    '       C.PAGAMENTOS, C.ULTREF, C.TXDEV, C.DTREC '+
    'FROM CARTEIRA c  '+
    'WHERE (C.COBRADOR = :COBRADOR OR C.COBSUB = :COBRADOR) AND C.DIAVENC =:DIA '+
    'ORDER BY C.GRUPO, C.CODAREA';
    //'select * from (select X.*, datediff(month from coalesce(X.ULTREF, X.INIREF) to X.RFATUAL) TXDEV from  '+
    //'( select C.CODIGO, C.CODAREA, A.DESCRICAO DCAREA, A.COBRADOR,                                         '+
    //'        B.NOME NMCOB, C.INIREF, C.DIAVENC, P.MENSALIDADE,                                             '+
    //'        G.DESCRICAO DCGRU, C.NOME, translator(E.ENDERECO) ENDERECO,                                   '+
    //'        translator(E.BAIRRO) bairro, translator(E.REFERENCIA) referencia, CI.NOME NMCID, CI.UF,       '+
    //'        trim(replace(E.CEP, ascii_char(44), ascii_char(32))) CEP, C.TELEFONE, C.CELULAR,              '+
    //'        coalesce(E.LATITUDE,0.0) LATITUDE, coalesce(E.LONGITUDE, 0.0) LONGITUDE, EMP.ULTREF RFATUAL,  '+
    //'        PAG.PAGAMENTOS, emp.maxpen, max(I.REFER) ULTREF                                               '+
    //' from CONTRATOS C                                                                                     '+
    //' inner join AREAS A on A.CODIGO = C.CODAREA and A.GERABOL = 1                                         '+
    //' inner join EMPRESA EMP on EMP.CODIGO = 1                                                             '+
    //' inner join COBRADORES B on B.CODIGO = A.COBRADOR                                                     '+
    //' inner join PLANOS P on P.CODIGO = C.PLANO                                                            '+
    //' inner join ENDERECOS E on E.CONTRATO = C.CODIGO and E.CODIGO = C.ENDCOB                              '+
    //' inner join GRUPOS G on G.CODIGO = C.GRUPO                                                            '+
    //' left outer join CIDADES CI on CI.CODIGO = E.CIDADE                                                   '+
    //' left outer join v_pagamentos PAG on PAG.CONTRATO = C.CODIGO                                          '+
    //' left outer join ITFIN I on I.CONTRATO=C.CODIGO and I.TIPO = 1                                        '+
    //' where C.diavenc = :DIA                                                                               '+
    //'       and (A.COBRADOR = :COBRADOR or A.COBSUB = :COBRADOR)                                           '+
    //'       and C.sitcobra < 2                                                                             '+
    //' group by C.CODIGO, C.CODAREA, A.DESCRICAO, A.COBRADOR, B.NOME, C.INIREF,                             '+
    //'          C.DIAVENC, P.MENSALIDADE, G.DESCRICAO, C.NOME, E.ENDERECO,                                  '+
    //'          E.BAIRRO, E.REFERENCIA, CI.NOME, CI.UF, E.CEP, C.TELEFONE,                                  '+
    //'          C.CELULAR, E.LATITUDE, E.LONGITUDE, EMP.ULTREF, PAG.PAGAMENTOS, emp.maxpen                  '+
    //' order by C.CODAREA, E.BAIRRO, E.ENDERECO  ) X) where TXDEV between 0 and MAXPEN                      ';

var
  vQry: TZQuery;
  vResultado: TJsonArray;
begin
  TDataSetSerializeConfig.GetInstance.DateTimeIsISO8601 := True;

  try
    vResultado := TJsonArray.Create();
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(SQL_CARTEIRA);
      vQry.ParamByName('dia').AsInteger := DayOf(pDiaVenc);
      vQry.ParamByName('cobrador').AsInteger := pCobrador;

      vQry.Open;

      if not vQry.EOF
      then begin
        vResultado := vQry.ToJSONArray();
        Result := TRetornoJson.New.Sucesso(True)
                              .Mensagem('Dados Encontrados!')
                              .Data(vResultado)
                              .ToStream;
      end
      else Raise Exception.Create(
                 Format('Cobrança do dia %s para o Cobrador %d não encontrada!',
                               [DateToStr(pDiaVenc), pCobrador]));
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vResultado.Free;
    end;
  except
    on E: exception do
       Result := TRetornoJson.New.Sucesso(False).Mensagem(E.Message).ToStream;
  end;
end;


function TConsultaPagamentoService.ProximoAcerto(pCobrador: Integer;
  pDtRec: TDateTime): String;
const
  SQL_PRXACERTO = 'select COD from PROC_PRXACERTO ( :cobrador, :dtrec)';
var
  vQry: TZQuery;
  vResultado: TJsonArray;
begin
  TDataSetSerializeConfig.GetInstance.DateTimeIsISO8601 := True;

  try
    vResultado := TJsonArray.Create();
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(SQL_PRXACERTO);
      vQry.ParamByName('dtrec').AsDateTime := pDtRec;
      vQry.ParamByName('cobrador').AsInteger := pCobrador;

      vQry.Open;

      if not vQry.EOF
      then begin
        vResultado := vQry.ToJSONArray();
        Result := TRetornoJson.New.Sucesso(True)
                              .Mensagem('Dados Encontrados!')
                              .Data(vResultado)
                              .ParaString;
      end
      else Raise Exception.Create('Falha na execução de PROC_PRXACERTO!');
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vResultado.Free;
    end;
  except
    on E: exception do
       Result := TRetornoJson.New.Sucesso(False).Mensagem(E.Message).ParaString;
  end;
end;

function TConsultaPagamentoService.ConsultarPagamento(pRecibo: String): String;
const
  SQL_CONSULTA_PAGAMENTO =
    'select R.CODIGO, COB.NOME NMCOB, C.NOME, R.DTREC, R.QTDE, R.VLREC, A.SIT '+
    'from RCCOB R '+
    'inner join ACERTOS A on A.CODIGO=R.ACERTO '+
    'inner join COBRADORES COB on COB.CODIGO=A.COBRADOR '+
    'inner join CONTRATOS C on C.CODIGO=R.CONTRATO '+
    'where A.COBRADOR = %d and R.CONTRATO = %d and R.ACERTO = %d and R.NUMERO = %d';
Var
  vQry: TZQuery;
  vPagina,
  vContent,
  vFooter,
  vSit,
  vSql: String;
  vCobrador, vAcerto, vContrato, vRecibo: Integer;
begin

 vCobrador := StrToIntDef(Copy(pRecibo, 1, 6),0);
 vAcerto   := StrToIntDef(Copy(pRecibo, 7, 6),0);
 vContrato := StrToIntDef(Copy(pRecibo, 13, 6),0);
 vRecibo   := StrToIntDef(Copy(pRecibo, 19, 6),0);

 vSql := Format(SQL_CONSULTA_PAGAMENTO,[ vCobrador,
                                         vContrato,
                                         vAcerto,
                                         vRecibo]);
 vPagina := '<!DOCTYPE html>'+
            '<html dir="ltr" lang="pt-br"> '+
             '<head> '+
	     '<meta http-equiv="content-type" content="text/html; charset=utf-8" />'+
	     '<meta name="copyright" content="© 2018 - 2022 - www.anapax.com.br">'+
	     '<link href="https://fonts.googleapis.com/css?family=Lato:300,400,400i,700|Raleway:300,400,500,600,700|Crete+Round:400i" rel="stylesheet" type="text/css" />'+
	     '<meta name="viewport" content="width=device-width, initial-scale=1" />'+
	     '<!-- Favicon --> '+
	     '<!-- Document Title '+
	     '============================================= -->'+
	     '<title>Anapax | Serviços Póstumos</title>'+
	     '<meta name="description" content="Anapax | Serviços Póstumos"> '+
             '</head>'+
             '<body>'+
             '<div id="wrapper" class="clearfix">'+
		'<div id="home" class="page-section" style="position:absolute;top:0;left:0;width:100%;height:200px;z-index:-2;"></div>'+
		'<!-- Header'+
		'============================================= -->'+
		'<header id="header" class="">'+
			'<div id="header-wrap">'+
				'<div class="container clearfix">'+
					'<div id="primary-menu-trigger"><i class="icon-reorder"></i></div>'+
					'<!-- Logo '+
					'============================================= -->'+
					'<div id="logo"> '+
						'<a href="http://anapax.com.br" class="standard-logo" data-dark-logo="http://anapax.com.br/website/images/anapax-servicos-postumos.png">'+
							'<img src="http://anapax.com.br/website/images/anapax-servicos-postumos.png" alt="Anapax | Deus é o principal tema desta empresa.">'+
						'</a>'+
					'</div><!-- #logo end -->'+
				'</div>'+
			'</div>'+
		'</header><!-- #header end -->';
 vQry := FConexao.GetQuery;
 try
   vQry.Close;
   vQry.SQL.Clear;
   vQry.SQL.Add(vSql);
   vQry.Open;
   if not vQry.EOF
   then Begin
     vSit := 'Pagamento em Conferencia.';
     if vQry.FieldByName('SIT').AsInteger > 0
     then vSit := 'Pagamento Conferido e Lançado.';
     vContent:=        '<!-- Content'+
                  '============================================= -->    '+
                  '<section id="content">'+
  	           '<div class="content-wrap">'+
                     '<h4 class="" style="margin-bottom: 10px;"><span>Confirmamos o recebimento a seguir</span></h4><br>'+
  		   '<div class="col_full nobottommargin">'+
  		      '<address>'+
  			'<strong>Recibo: </strong>'+vQry.FieldByName('CODIGO').AsString +'<br><br>'+
  			'<strong>Agente: </strong>'+vQry.FieldByName('NMCOB').AsString+' <br>'+
  			'<strong>Associado: </strong>'+vQry.FieldByName('NOME').AsString+'<br>'+
  			'<strong>Qtde Taxas: </strong>'+vQry.FieldByName('QTDE').AsString+'<br>'+
  			'<strong>Valor (R$): </strong>'+FormatFloat(',#0.00',vQry.FieldByName('VLREC').AsFloat) +'<br>'+
  			'<strong>Recebido em: </strong>'+FormatDateTime('dd/mm/yyyy',vQry.FieldByName('DTREC').AsDateTime) +'<br><br>'+
  			'<strong>Situação do Recebimento: </strong>'+vSit+'<br><br><br><br>'+
  		      '</address>'+
  		   '</div>'+
                     '</div>'+
                  '</section><!-- #content end -->';

   end
   else
      vContent:= '<!-- Content'+
                  '============================================= -->    '+
                  '<section id="content">'+
  	           '<div class="content-wrap">'+
                     '<h4 class="" style="margin-bottom: 10px;"><span>PAGAMENTO NÃO ENCONTRADO</span></h4><br><br>'+
             	'<strong>Cobrador: </strong>'+IntToStr(vCobrador) +' <br>'+
             			'<strong>Associado: </strong>'+IntToStr(vContrato)+'<br>'+
             			'<strong>Recibo: </strong>'+IntToStr(vRecibo)+'<br><br><br>'+
             			//'<strong>Data Pagto: </strong>'+DateToStr(vDtRec) +'<br>'+
                     '</div>'+
                  '</section><!-- #content end -->';
 finally
   FreeAndNil(vQry);
 end;
 vFooter := '<!-- Footer'+
		'============================================= -->'+
		'<footer id="footer" class="dark"> '+
			'<!-- Copyrights'+
			'============================================= -->  '+
			'<div id="copyrights">'+
				'<div class="container clearfix">'+
					'<div class="col_half">'+
						'<!-- <img src="images/ft-anapax.png" alt="Footer Logo" class="footer-logo"> -->'+
						'Copyrights &copy; 2018 - Todos os direitos reservados por Anapax.'+
					'</div>'+

				'</div>'+
			'</div><!-- #copyrights end -->'+
		'</footer>'+
		'<!-- #footer end -->'+
	        '</div><!-- #wrapper end -->'+
             '</body></html>';

 Result := vPagina+vContent+vFooter;
end;

function TConsultaPagamentoService.FormasPagamento: String;
const
  SQL_FRMPG =
  'select F.CODIGO, F.DESCRICAO, '+
         'case F.TPPIX '+
           'when 0 then ''CPF'' '+
           'when 1 then ''CNPJ'' '+
           'when 2 then ''Celular'' '+
           'when 3 then ''E-mail'' '+
           'when 4 then ''Aleatoria'' '+
         'end as TPPIX, F.CHPIX, F.QRCODE '+
  'from FRMPG F '+
  'inner join EMPRESA E on E.CODIGO = 1 '+

  'where F.MEIO = E.CDPIX '+
  'order by F.CODIGO ';

var
  vQry: TZQuery;
  vResultado: TJsonArray;
begin

  try
    vResultado := TJsonArray.Create();
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(SQL_FRMPG);

      vQry.Open;

      if not vQry.EOF
      then begin
        vResultado := vQry.ToJSONArray();
        Result := TRetornoJson.New.Sucesso(True)
                              .Mensagem('Dados Encontrados!')
                              .Data(vResultado)
                              .ParaString;
        TConsoleUtil.LogData('ConsultaPagamentoService: vResultado', Result);
      end
      else Raise Exception.Create('Formas de Pagamento não encontradas!');
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vResultado.Free;
    end;
  except
    on E: exception do
       Result := TRetornoJson.New.Sucesso(False).Mensagem(E.Message).ParaString;
  end;
end;

function TConsultaPagamentoService.GetImagem(pCodigo:Integer): TMemoryStream;
const
  SQL_IMAGEM =
  'select IMAGEM from IMAGENS where CODIGO = :CODIGO ';

var
  vQry: TZQuery;
begin

  Result := TMemoryStream.Create;

  vQry := FConexao.GetQuery;
  try
    vQry.Close;
    vQry.SQL.Clear;
    vQry.SQL.Add(SQL_IMAGEM);
    vQry.ParamByName('CODIGO').AsInteger := pCodigo;

    vQry.Open;

    if not vQry.EOF
    then begin
      TBlobField(vQry.FieldByName('IMAGEM')).SaveToStream(Result);
      Result.Position := 0;
    end
    else Raise Exception.Create('Formas de Pagamento não encontradas!');
  finally
    vQry.Close;
    FreeAndNil(vQry);
  end;
end;

function TConsultaPagamentoService.Associado(pAssociado: Integer): String;
const
  SQL_ASSOCIADO =
    //'SELECT C.CODIGO , C.CODAREA , C.DCAREA , C.COBRADOR , C.NMCOB , C.INIREF,   '+
    //'       C.DIAVENC, C.MENSALIDADE, C.GRUPO, C.DCGRU, C.NOME,                  '+
    //'       TRANSLATOR(C.ENDERECO) ENDERECO, TRANSLATOR(C.BAIRRO ) BAIRRO,       '+
    //'       TRANSLATOR(C.REFERENCIA) REFERENCIA, C.NMCID, C.UF,                  '+
    //'       trim(replace(C.CEP, ascii_char(44), ascii_char(32))) CEP, C.TELEFONE,'+
    //'       C.CELULAR, C.LATITUDE, C.LONGITUDE, C.RFATUAL,  C.PAGAMENTOS, C.ULTREF,'+
    //'       C.TXDEV, C.dtrec                                                       '+
    //'FROM CARTEIRA c                                                               '+
    //'WHERE C.CODIGO = :CODIGO                                                      ';

     'select X.*,  datediff(month from coalesce(X.ULTREF, X.INIREF) to X.RFATUAL) TXDEV '+
    'from ( select C.CODIGO, C.CODAREA, A.DESCRICAO DCAREA, A.COBRADOR,                               '+
    '        B.NOME NMCOB, C.INIREF, C.DIAVENC, P.MENSALIDADE,                                        '+
    '        G.DESCRICAO DCGRU, C.NOME, translator(E.ENDERECO) ENDERECO,                              '+
    '        translator(E.BAIRRO) bairro, translator(E.REFERENCIA) referencia, CI.NOME NMCID, CI.UF,  '+
    '        trim(replace(E.CEP, ascii_char(44), ascii_char(32))) CEP, C.TELEFONE, C.CELULAR,         '+
    '        coalesce(E.LATITUDE,0.0) LATITUDE, coalesce(E.LONGITUDE, 0.0) LONGITUDE, EMP.ULTREF RFATUAL,'+
    '        PAG.PAGAMENTOS, emp.maxpen, FNVENC(EMP.ULTREF, C.DIAVENC, EMP.TPVENC, EMP.DIAVENC) DTREC,   '+
    '        VU.ULTREF                                                                                   '+
    ' from CONTRATOS C                                                                                   '+
    ' inner join AREAS A on A.CODIGO = C.CODAREA and A.GERABOL = 1                                       '+
    ' inner join EMPRESA EMP on EMP.CODIGO = 1                                                           '+
    ' inner join COBRADORES B on B.CODIGO = A.COBRADOR                                                   '+
    ' inner join PLANOS P on P.CODIGO = C.PLANO                                                          '+
    ' inner join ENDERECOS E on E.CONTRATO = C.CODIGO and E.CODIGO = C.ENDCOB                            '+
    ' inner join GRUPOS G on G.CODIGO = C.GRUPO                                                         '+
    ' left outer join CIDADES CI on CI.CODIGO = E.CIDADE                                                '+
    ' left outer join v_pagamentos PAG on PAG.CONTRATO = C.CODIGO                                       '+
    ' left outer join V_ULTREFPG vu on VU.CONTRATO=C.CODIGO                                             '+
    ' where C.CODIGO = :CODIGO ) X                    ';
var
  vQry: TZQuery;
  vResultado: TJsonArray;
begin
  TDataSetSerializeConfig.GetInstance.DateTimeIsISO8601 := True;

  try
    vResultado := TJsonArray.Create();
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(SQL_ASSOCIADO);
      vQry.ParamByName('CODIGO').AsInteger := pAssociado;

      vQry.Open;

      if not vQry.EOF
      then begin
        vResultado := vQry.ToJSONArray();
        Result := TRetornoJson.New.Sucesso(True)
                              .Mensagem('Dados Encontrados!')
                              .Data(vResultado)
                              .ParaString;
      end
      else Raise Exception.Create(
                 Format('Associado %d não encontrado!', [pAssociado]));
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vResultado.Free;
    end;
  except
    on E: exception do
       Result := TRetornoJson.New.Sucesso(False).Mensagem(E.Message).ParaString;
  end;
end;

function TConsultaPagamentoService.ConsultaNome(pCobrador: Integer;
  pNome: String): TMemoryStream;
const
  CONSULTA_TITULAR =
    //'SELECT C.CODIGO , C.CODAREA , C.DCAREA , C.COBRADOR , C.NMCOB , C.INIREF,   '+
    //'       C.DIAVENC, C.MENSALIDADE, C.GRUPO, C.DCGRU, C.NOME,                  '+
    //'       TRANSLATOR(C.ENDERECO) ENDERECO, TRANSLATOR(C.BAIRRO ) BAIRRO,       '+
    //'       TRANSLATOR(C.REFERENCIA) REFERENCIA, C.NMCID, C.UF,                  '+
    //'       trim(replace(C.CEP, ascii_char(44), ascii_char(32))) CEP, C.TELEFONE,'+
    //'       C.CELULAR, C.LATITUDE, C.LONGITUDE, C.RFATUAL,  C.PAGAMENTOS, C.ULTREF,'+
    //'       C.TXDEV, C.dtrec                                                       '+
    //'FROM CARTEIRA c                                                               '+
    //'WHERE C.NOME LIKE %s AND C.COBRADOR = %d  ORDER BY C.NOME                     ';

    'select * from (select X.*,  datediff(month from coalesce(X.ULTREF, X.INIREF) to X.RFATUAL) TXDEV '+
    'from ( select C.CODIGO, C.CODAREA, A.DESCRICAO DCAREA, A.COBRADOR,                               '+
    '        B.NOME NMCOB, C.INIREF, C.DIAVENC, P.MENSALIDADE,                                        '+
    '        G.DESCRICAO DCGRU, C.NOME, translator(E.ENDERECO) ENDERECO,                              '+
    '        translator(E.BAIRRO) bairro, translator(E.REFERENCIA) referencia, CI.NOME NMCID, CI.UF,  '+
    '        trim(replace(E.CEP, ascii_char(44), ascii_char(32))) CEP, C.TELEFONE, C.CELULAR,         '+
    '        coalesce(E.LATITUDE,0.0) LATITUDE, coalesce(E.LONGITUDE, 0.0) LONGITUDE, EMP.ULTREF RFATUAL,'+
    '        PAG.PAGAMENTOS, emp.maxpen, FNVENC(EMP.ULTREF, C.DIAVENC, EMP.TPVENC, EMP.DIAVENC) DTREC,   '+
    '        VU.ULTREF                                                                                   '+
    ' from CONTRATOS C                                                                                   '+
    ' inner join AREAS A on A.CODIGO = C.CODAREA and A.GERABOL = 1                                       '+
    ' inner join EMPRESA EMP on EMP.CODIGO = 1                                                           '+
    ' inner join COBRADORES B on B.CODIGO = A.COBRADOR                                                   '+
    ' inner join PLANOS P on P.CODIGO = C.PLANO                                                          '+
    ' inner join ENDERECOS E on E.CONTRATO = C.CODIGO and E.CODIGO = C.ENDCOB                            '+
    ' inner join GRUPOS G on G.CODIGO = C.GRUPO                                                         '+
    ' left outer join CIDADES CI on CI.CODIGO = E.CIDADE                                                '+
    ' left outer join v_pagamentos PAG on PAG.CONTRATO = C.CODIGO           '+
    ' left outer join V_ULTREFPG vu on VU.CONTRATO=C.CODIGO  ';
  SQL_PERFIL1 =
    ' where C.NOME LIKE %s                                                  '+
    '       and A.COBRADOR = %d                                             '+
    '       and C.sitcobra < 3                                              '+
    ' order by C.NOME ) X) where TXDEV between 0 and 18                     ';
  SQL_PERFIL4 =
    ' where C.NOME LIKE %s                                                  '+
    '       and E.CIDADE = %d                                               '+
    '       and C.sitcobra < 3                                              '+
    ' order by C.NOME ) X) where TXDEV between 0 and 18                     ';
var
  vQry: TZQuery;
  vResultado: TJsonArray;
  vSQl: String;
  vPerfil, vCidade:  Integer;
begin
  TDataSetSerializeConfig.GetInstance.DateTimeIsISO8601 := True;

  vCidade := 0;
  vPerfil := 0;

  vPerfil := RetornaPerfilCobrador(pCobrador, vCidade);


  if vPerfil = 4
  then vSQl := Format(CONSULTA_TITULAR+SQL_PERFIL4,[QuotedStr(Trim(UpperCase(pNome))+'%'), vCidade])
  else vSQl := Format(CONSULTA_TITULAR+SQL_PERFIL1,[QuotedStr(Trim(UpperCase(pNome))+'%'), pCobrador]);

  TConsoleUtil.LogData('TConsultaPagamentoService.ConsultaNome vSql:',vSQl);

  try
    vResultado := TJsonArray.Create();
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(vSQl);
      vQry.Open;

      if not vQry.EOF
      then begin
        vResultado := vQry.ToJSONArray();
        Result := TRetornoJson.New.Sucesso(True)
                              .Mensagem('Dados Encontrados!')
                              .Data(vResultado)
                              .ToStream;
      end
      else Raise Exception.Create(
                 Format('Sem resultados para %s!', [pNome]));
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vResultado.Free;
    end;
  except
    on E: exception do
       Result := TRetornoJson.New.Sucesso(False).Mensagem(E.Message).ToStream;
  end;
end;

function TConsultaPagamentoService.DataRecebimento(pCobrador: Integer
  ): TMemoryStream;
const
  CONSULTA_DATAS =
    'SELECT C.DTREC, COUNT(*) CLIENTES FROM CARTEIRA c '+
    'WHERE C.COBRADOR = %d                             '+
    'GROUP BY C.DTREC ORDER BY C.DTREC                 ';
 var
  vQry: TZQuery;
  vResultado: TJsonArray;
  vSQl: String;
begin
  TDataSetSerializeConfig.GetInstance.DateTimeIsISO8601 := True;

  vSQl := Format(CONSULTA_DATAS,[ pCobrador ]);
  //TConsoleUtil.LogData('TConsultaPagamentoService.ConsultaNome vSql:',vSQl);
  try
    vResultado := TJsonArray.Create();
    vQry := FConexao.GetQuery;
    try
      vQry.Close;
      vQry.SQL.Clear;
      vQry.SQL.Add(vSQl);
      vQry.Open;

      if not vQry.EOF
      then begin
        vResultado := vQry.ToJSONArray();
        Result := TRetornoJson.New.Sucesso(True)
                              .Mensagem('Dados Encontrados!')
                              .Data(vResultado)
                              .ToStream;
      end
      else Raise Exception.Create('Sem resultados!');
    finally
      vQry.Close;
      FreeAndNil(vQry);
      //vResultado.Free;
    end;
  except
    on E: exception do
       Result := TRetornoJson.New.Sucesso(False).Mensagem(E.Message).ToStream;
  end;
end;

end.

