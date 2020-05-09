unit LuaIXQValue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53, xquery;

type
  { TLuaIXQValue }

  TLuaIXQValue = class
  public
    FIXQValue: IXQValue;
    constructor Create(const AIX: IXQValue);
  end;

procedure luaIXQValuePush(const L: Plua_State; const Obj: IXQValue); inline;

implementation

uses
  LuaClass, LuaUtils;

type
  TUserData = TLuaIXQValue;

{ TLuaIXQValue }

constructor TLuaIXQValue.Create(const AIX: IXQValue);
begin
  FIXQValue := AIX;
end;

function ixqvalue_tostring(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toString);
  Result := 1;
end;

function ixqvalue_getattribute(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toNode.getAttribute(
    luaGetString(L, 1)));
  Result := 1;
end;

function ixqvalue_getproperty(L: Plua_State): Integer; cdecl;
begin
  luaIXQValuePush(L, TUserData(luaClassGetObject(L)).FIXQValue.getProperty(luaGetString(L, 1)));
  Result := 1;
end;

function ixqvalue_innerHTML(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toNode.innerHTML());
  Result := 1;
end;

function ixqvalue_outerHTML(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toNode.outerHTML());
  Result := 1;
end;

function ixqvalue_innerText(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toNode.innerText());
  Result := 1;
end;

function ixqvalue_count(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).FIXQValue.Count);
  Result := 1;
end;

function ixqvalue_get(L: Plua_State): Integer; cdecl;
begin
  luaIXQValuePush(L, TUserData(luaClassGetObject(L)).FIXQValue.get(lua_tointeger(L, 1)));
  Result := 1;
end;

const
  methods: packed array [0..7] of luaL_Reg = (
    (name: 'Get'; func: @ixqvalue_get),
    (name: 'GetAttribute'; func: @ixqvalue_getattribute),
    (name: 'GetProperty'; func: @ixqvalue_getproperty),
    (name: 'InnerHTML'; func: @ixqvalue_innerHTML),
    (name: 'OuterHTML'; func: @ixqvalue_outerHTML),
    (name: 'InnerText'; func: @ixqvalue_innerText),
    (name: 'ToString'; func: @ixqvalue_tostring),
    (name: nil; func: nil)
    );

  props: packed array[0..1] of lual_Reg_prop = (
    (name: 'Count'; funcget: @ixqvalue_count; funcset: nil),
    (name: nil; funcget: nil; funcset: nil)
    );

procedure luaIXQValueAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
  luaClassAddProperty(L, MetaTable, UserData, props);
end;

procedure luaIXQValuePush(const L: Plua_State; const Obj: IXQValue);
begin
  luaClassPushObject(L, TLuaIXQValue.Create(Obj), '', True, @luaIXQValueAddMetaTable);
end;

initialization
  luaClassRegister(TLuaIXQValue, @luaIXQValueAddMetaTable);

end.
