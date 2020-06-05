unit LuaDuktape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif}, uBaseUnit;

implementation

uses JSUtils, LuaUtils, LuaPackage;

function lua_execjs(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ExecJS(luaGetString(L, 1)));
  Result := 1;
end;

function luaopen_duktape(L: Plua_State): Integer; cdecl;
var
  t: Integer;
begin
  t := luaNewTable(L);
  luaAddCFunctionToTable(L, t, 'ExecJS', @lua_execjs);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('duktape', @luaopen_duktape);

end.

