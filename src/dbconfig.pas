unit dbconfig;

{$mode Delphi}{$H+}
//{$define anapax}
//{$define ssa}
{$define debug}

interface

uses
  Classes, SysUtils;

const
  DB_PROTOCOL = 'firebird';
  DB_HOST     = 'localhost';

  {$ifdef anapax}
  DB_PORT = 3051;
  DB_DATABASE = 'anapax';
  DB_PATH_LIB = '';
  {$endif}
  {$ifdef ssa}
  DB_PORT = 3051;
  DB_DATABASE = 'bdcon';
  DB_PATH_LIB = 'C:\Program Files\Firebird\Firebird_5_0\fbclient.dll';
  {$endif}
  {$ifdef debug}
  DB_PORT = 3050;
  DB_DATABASE = 'anapax';
  DB_PATH_LIB = '';
  {$endif}

  DB_USERNAME = 'SYSDBA';
  DB_PASSWORD = 'masterkey';

  DB_SECRET   = 'AJJDOP2098SHPO0823HLKDJFAINEjajdinjs';
  ROTAS_EXCLUIDAS: array of String = [ '/'
                                       ,'/login'
                                       ,'/ping'
                                       ,'/registro'
                                       ,'/tabela'
                                       ,'/tabela/lista'
                                       ,'/tabela/blob'
                                       ,'/tabela/status'
                                       ,'/tabela/config'
                                       ,'/consulta'
                                       ,'/upload'
                                       ,'/upwin'
                                       ,'/download'
                                       ,'/cliente'
                                       ,'/mobile'
                                       ];

implementation

end.

