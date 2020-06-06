unit LuaDuktape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses JSUtils, LuaUtils, LuaPackage;

function lua_execjs(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ExecJS(luaGetString(L, 1)));
  Result := 1;
end;

const
  methods: packed array [0..1] of luaL_Reg = (
    (name: 'ExecJs'; func: @lua_execjs),
    (name: nil; func: nil)
    );

function luaopen_duktape(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, methods);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('duktape', @luaopen_duktape);

end.

