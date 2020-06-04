unit LuaSynaUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif}, synautil;

procedure luaSynaUtilRegister(L: Plua_State);

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

function lua_replacestring(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ReplaceString(luaGetString(L, 1), luaGetString(L, 2), luaGetString(L, 3)));
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

procedure luaSynaUtilRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'GetBetween', @lua_getbetween);
  luaPushFunctionGlobal(L, 'SeparateLeft', @lua_separateleft);
  luaPushFunctionGlobal(L, 'SeparateRight', @lua_separateright);
  luaPushFunctionGlobal(L, 'ReplaceString', @lua_replacestring);
  luaPushFunctionGlobal(L, 'ParseURL', @lua_parseurl);
end;

end.

