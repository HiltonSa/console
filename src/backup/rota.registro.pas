unit rota.registro;

{$mode Delphi} {$H+}

interface

uses
  Classes, SysUtils, Horse, Horse.JWT,  json.retorno;
type

  { TRotaRegistro }

  TRotaRegistro = class
    public
      class procedure RotaRegistro;
  end;

implementation

uses registro.service, dbconfig;

{ TRotaRegistro }

procedure OnRegistro(aReq: THorseRequest; aRes: THorseResponse;  Next: TNextProc);
begin
  aRes.ContentType('application/json; charset=UTF-8')
      .Send( TRegistroUsuarioService.New.Registro( aReq.Body ));
end;

procedure OnLogin(aReq: THorseRequest; aRes: THorseResponse;  Next: TNextProc);
var
  vResp: String;
begin
  vResp := TRegistroUsuarioService.New.Logar( aReq.Body);
  aRes.ContentType('application/json')
      .Send( vResp );
end;

class procedure TRotaRegistro.RotaRegistro;
begin
  THorse.AddCallback(HorseJWT(DB_SECRET))
        .Post('/registro', OnRegistro)
        .Post('/login', OnLogin);
end;

end.

