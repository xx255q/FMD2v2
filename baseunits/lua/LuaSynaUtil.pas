unit LuaSynaUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif}, synautil;

procedure luaSynaUtilRegister(L: Plua_State); inline;

implementation

uses
  LuaUtils;

function lua_getbetween(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, GetBetween(luaGetString(L, 1), luaGetString(L, 2), luaGetString(L, 3)));
  Result := 1;
end;

function lua_separateleft(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SeparateLeft(luaGetString(L, 1), luaGetString(L, 2)));
  Result := 1;
end;

function lua_separateright(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SeparateRight(luaGetString(L, 1), luaGetString(L, 2)));
  Result := 1;
end;

function lua_parseurl(L: Plua_State): Integer; cdecl;
var
  prot, user, pass, host, port, path, para, s: string;
begin
  s := ParseURL(luaGetString(L, 1), prot, user, pass, host, port, path, para);
  lua_pushstring(L, prot);
  lua_pushstring(L, user);
  lua_pushstring(L, pass);
  lua_pushstring(L, host);
  lua_pushstring(L, port);
  lua_pushstring(L, path);
  lua_pushstring(L, para);
  lua_pushstring(L, s);
  Result := 8;
end;

const
  methods: packed array [0..4] of luaL_Reg = (
    (name: 'GetBetween'; func: @lua_getbetween),
    (name: 'SeparateLeft'; func: @lua_separateleft),
    (name: 'SeparateRight'; func: @lua_separateright),
    (name: 'ParseURL'; func: @lua_parseurl),
    (name: nil; func: nil)
    );

procedure luaSynaUtilRegister(L: Plua_State);
begin
  luaNewLib(L, methods);
end;

end.

