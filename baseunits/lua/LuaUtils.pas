unit LuaUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, LuaClass;

function luaNewTable(const L: Plua_State): Integer;
procedure luaAddCFunctionToTable(const L: Plua_State; const Table: Integer;
  const Name: String; const Func: lua_CFunction);
procedure luaAddCClosureToTable(const L: Plua_State; const Table, Value: Integer;
  const Name: String; const Func: lua_CFunction); overload;
procedure luaAddCClosureToTable(const L: Plua_State; const Table: Integer; const Value: Pointer;
  const Name: String; const Func: lua_CFunction); overload;
procedure luaAddStringToTable(const L: Plua_State; const Table: Integer; const Name, Value: String);
procedure luaAddIntegerToTable(const L: Plua_State; const Table: Integer; const Name: String; const Value: lua_Integer);
procedure luaAddBooleanToTable(const L: Plua_State; const Table: Integer; const Name: String; const Value: Boolean);

procedure luaPushFunctionGlobal(const L: Plua_State; const Name: String; const Func: lua_CFunction);

procedure luaPushStringGlobal(const L: Plua_State; const Name, Value: String);
procedure luaPushIntegerGlobal(const L: Plua_State; const Name: String; const Value: lua_Integer);
procedure luaPushBooleanGlobal(const L: Plua_State; const Name: String; const Value: Boolean);
procedure luaPushObjectGlobal(const L: Plua_State; const AObj: TObject; const AName: String;
  const AddMetaTable: TluaClassAddMetaTable = nil; const AutoFree: Boolean = False); inline;

procedure luaPushUserData(const L: Plua_State; const Value: Pointer); overload; inline;
procedure luaPushUserData(const L: Plua_State; const Value: Pointer; var UIndex: Integer); overload; inline;
function luaGetUserData(const L: Plua_State; const idx: Integer): Pointer; inline;
function luaGetString(const L: Plua_State; const idx: Integer): String; inline;

function LuaToString(const L: Plua_State; const idx: Integer): String;
function LuaStackToString(const L: Plua_State): String;

procedure luaL_newlib(const L: Plua_State; const Name: String; const lr: PluaL_Reg); overload; inline;

// deprecated since 5.2
procedure luaL_openlib(const L: Plua_State; const Name: String; const lr: PluaL_Reg;
  const {%H-}nup: Integer); inline;
procedure luaL_register(const L: Plua_State; const Name: String; const lr: PluaL_Reg); inline;

implementation

uses MultiLog;

function luaNewTable(const L: Plua_State): Integer;
begin
  lua_newtable(L);
  Result := lua_gettop(L);
end;

procedure luaAddCFunctionToTable(const L: Plua_State; const Table: Integer;
  const Name: String; const Func: lua_CFunction);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushcfunction(L, Func);
  lua_rawset(L, Table);
end;

procedure luaAddCClosureToTable(const L: Plua_State; const Table,
  Value: Integer; const Name: String; const Func: lua_CFunction);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushvalue(L, Value);
  lua_pushcclosure(L, Func, 1);
  lua_rawset(L, Table);
end;

procedure luaAddCClosureToTable(const L: Plua_State; const Table: Integer;
  const Value: Pointer; const Name: String; const Func: lua_CFunction);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushlightuserdata(L, Value);
  lua_pushcclosure(L, Func, 1);
  lua_rawset(L, Table);
end;

procedure luaAddStringToTable(const L: Plua_State; const Table: Integer;
  const Name, Value: String);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushstring(L, Value);
  lua_rawset(L, Table);
end;

procedure luaAddIntegerToTable(const L: Plua_State; const Table: Integer;
  const Name: String; const Value: lua_Integer);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushinteger(L, Value);
  lua_rawset(L, Table);
end;

procedure luaAddBooleanToTable(const L: Plua_State; const Table: Integer;
  const Name: String; const Value: Boolean);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushboolean(L, Value);
  lua_rawset(L, Table);
end;

procedure luaPushFunctionGlobal(const L: Plua_State; const Name: String;
  const Func: lua_CFunction);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushcfunction(L, Func);
  lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaPushStringGlobal(const L: Plua_State; const Name, Value: String);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Value);
  lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaPushIntegerGlobal(const L: Plua_State; const Name: String;
  const Value: lua_Integer);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushinteger(L, Value);
  lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaPushBooleanGlobal(const L: Plua_State; const Name: String;
  const Value: Boolean);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushboolean(L, Value);
  lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaPushObjectGlobal(const L: Plua_State; const AObj: TObject;
  const AName: String; const AddMetaTable: TluaClassAddMetaTable;
  const AutoFree: Boolean);
begin
  luaClassPushObject(L, AObj, AName, AutoFree, AddMetaTable);
end;

procedure luaPushUserData(const L: Plua_State; const Value: Pointer);
begin
  PPointer(lua_newuserdata(L, SizeOf(PPointer)))^ := Value;
end;

procedure luaPushUserData(const L: Plua_State; const Value: Pointer;
  var UIndex: Integer);
begin
  luaPushUserData(L, Value);
  UIndex := lua_gettop(L);
end;

function luaGetUserData(const L: Plua_State; const idx: Integer): Pointer;
begin
  Result := PPointer(lua_touserdata(L, idx))^;
end;

function luaGetString(const L: Plua_State; const idx: Integer): String;
var
  slen: size_t;
begin
  Result := lua_tolstring(L, idx, @slen);
  SetLength(Result, slen);
end;

function LuaToString(const L: Plua_State; const idx: Integer): String;
begin
  if lua_isuserdata(L, idx) then
    Result := 'userdata: ' + hexStr(lua_touserdata(L, idx))
  else
  if lua_isstring(L, idx) then
    Result := 'string: ' + luaGetString(L, idx)
  else
  if lua_isinteger(L, idx) then
    Result := 'integer: ' + IntToStr(lua_tointeger(L, idx))
  else
  if lua_iscfunction(L, idx) then
    Result := 'cfunc: ' + hexStr(lua_topointer(L, idx))
  else
  if lua_isfunction(L, idx) then
    Result := 'func: ' + hexStr(lua_topointer(L, idx))
  else
  if lua_isnoneornil(L, idx) then
    Result := 'nil'
  else
  if lua_isboolean(L, idx) then
    Result := 'boolean: ' + BoolToStr(lua_toboolean(L, idx), True)
  else
  if lua_isnumber(L, idx) then
    Result := 'number: ' + FloatToStr(lua_tonumber(L, idx))
  else
  if lua_istable(L, idx) then
    Result := 'table: ' + hexStr(lua_topointer(L, idx))
  else
  if lua_islightuserdata(L, idx) then
    Result := 'ligthuserdata: ' + hexStr(lua_topointer(L, idx))
  else
    Result := 'unknown: ' + hexStr(lua_topointer(L, idx));
end;

function LuaStackToString(const L: Plua_State): String;
var
  i: Integer;
begin
  Result := '';
  i := lua_gettop(L);
  if i = 0 then
    Exit;
  for i := 1 to i do
    Result := Result + IntToStr(i) + '=' + LuaToString(L, i) + LineEnding;
  SetLength(Result, Length(Result) - Length(LineEnding));
end;

procedure luaL_newlib(const L: Plua_State; const Name: String;
  const lr: PluaL_Reg);
begin
  luaL_newlib(L, lr);
  if Name <> '' then
    lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaL_openlib(const L: Plua_State; const Name: String;
  const lr: PluaL_Reg; const nup: Integer);
begin
  luaL_setfuncs(L, lr, 0);
  if Name <> '' then
    lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaL_register(const L: Plua_State; const Name: String;
  const lr: PluaL_Reg);
begin
  luaL_openlib(L, Name, lr, 0);
end;

end.
