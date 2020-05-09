unit LuaFMD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaFMDRegister(L: Plua_State);

implementation

uses
  LuaClass, LuaUtils, FMDOptions, SimpleTranslator;

procedure luaFMDRegister(L: Plua_State);
var
  t: Integer;
begin
  t := luaNewTable(L);
  luaAddStringToTable(L, t, 'Directory', FMD_DIRECTORY);
  luaAddStringToTable(L, t, 'ExeName', FMD_EXENAME);
  luaAddStringToTable(L, t, 'Version', FMD_VERSION_STRING);
  luaAddStringToTable(L, t, 'Revision', REVISION_NUMBER);
  luaAddStringToTable(L, t, 'LuaDirectory', LUA_REPO_FOLDER);
  luaAddStringToTable(L, t, 'SelectedLanguage', SimpleTranslator.LastSelected);
  lua_setglobal(L, 'FMD');
end;

end.
