unit LuaFMD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaFMDRegister(L: Plua_State);

implementation

uses
  LuaClass, LuaUtils, FMDOptions;

procedure luaFMDRegister(L: Plua_State);
var
  t: Integer;
begin
  t := luaNewTable(L);
  luaAddStringToTable(L, t, 'Directory', PAnsiChar(FMD_DIRECTORY));
  luaAddStringToTable(L, t, 'ExeName', PAnsiChar(FMD_EXENAME));
  luaAddStringToTable(L, t, 'Version', PAnsiChar(FMD_VERSION_STRING));
  luaAddStringToTable(L, t, 'Revision', PAnsiChar(REVISION_NUMBER));
  luaAddStringToTable(L, t, 'LuaDirectory', PAnsiChar(LUA_REPO_FOLDER));
  lua_setglobal(L, 'FMD');
end;

end.
