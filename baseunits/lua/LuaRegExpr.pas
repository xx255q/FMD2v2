unit LuaRegExpr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, RegExpr;

procedure luaRegExprRegister(L: Plua_State);

implementation

uses
  LuaUtils;

function re_exec(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, ExecRegExpr(luaGetString(L, 1), luaGetString(L, 2)));
  Result := 1;
end;

function re_replace(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ReplaceRegExpr(luaGetString(L, 1), luaGetString(L, 2), luaGetString(L, 3), True));
  Result := 1;
end;

function re_getmatch(L: Plua_State): Integer; cdecl;
begin
  if (lua_gettop(L) < 3) or (lua_tointeger(L, 3) < 0) then
  begin
    lua_pushstring(L, '');
  end
  else
  begin
    with TRegExpr.Create(luaGetString(L, 1)) do
      try
        if Exec(luaGetString(L, 2)) and (SubExprMatchCount > 0) then
          lua_pushstring(L, Match[lua_tointeger(L, 3)]);
      finally
        Free;
      end;
  end;
  Result := 1;
end;

procedure luaRegExprRegister(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'RegExprExec', @re_exec);
  luaPushFunctionGlobal(L, 'RegExprReplace', @re_replace);
  luaPushFunctionGlobal(L, 'RegExprGetMatch', @re_getmatch);
end;

end.
