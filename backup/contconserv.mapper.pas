unit contconserv.mapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type
  TMapper = class(TDaemonMapper)
  private

  public

  end;

var
  Mapper: TMapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TMapper)
end;

{$R *.lfm}


initialization
  RegisterMapper;
end.

