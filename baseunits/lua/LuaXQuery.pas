unit LuaXQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif}, XQueryEngineHTML, xquery;

procedure luaXQueryRegister(const L: Plua_State); inline;
procedure luaXQueryAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer); inline;

implementation

uses
  LuaClass, LuaIXQValue, LuaUtils;

type
  TUserData = TXQueryEngineHTML;

function xquery_create(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := nil;
  Result := 0;
  if lua_gettop(L) = 1 then
  begin
    if lua_isstring(L, 1) then
      u := TXQueryEngineHTML.Create(luaGetString(L, 1))
    else
    if lua_isuserdata(L, 1) then
      u := TXQueryEngineHTML.Create(TStream(luaGetUserData(L, 1)));
  end
  else
    u := TXQueryEngineHTML.Create;
  if Assigned(u) then
  begin
    luaClassPushObject(L, u, '', True, @luaXQueryAddMetaTable);
    Result := 1;
  end;
end;

function xquery_parsehtml(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  Result := 0;
  u := TUserData(luaClassGetObject(L));
  if lua_isstring(L, 1) then
    u.ParseHTML(luaGetString(L, 1))
  else
  if lua_isuserdata(L, 1) then
    u.ParseHTML(TStream(luaGetUserData(L, 1)));
end;

function xquery_xpath(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
  x: IXQValue;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    x := u.XPath(luaGetString(L, 1), TLuaIXQValue(luaGetUserData(L, 2)).FIXQValue)
  else
    x := u.XPath(luaGetString(L, 1));
  luaIXQValuePush(L, x);
  Result := 1;
end;

function xquery_xpathi(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
  x: IXQValue;
  t, i: Integer;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    x := u.XPath(luaGetString(L, 1), TLuaIXQValue(luaGetUserData(L, 2)).FIXQValue)
  else
    x := u.XPath(luaGetString(L, 1));
  lua_newtable(L);
  t := lua_gettop(L);
  for i := 1 to x.Count do
  begin
    luaIXQValuePush(L, x.get(i));
    lua_rawseti(L, t, i);
  end;
  Result := 1;
end;

function xquery_xpathstring(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    lua_pushstring(L, u.XPathString(luaGetString(L, 1),
      TLuaIXQValue(luaGetUserData(L, 2)).FIXQValue))
  else
    lua_pushstring(L, u.XPathString(luaGetString(L, 1)));
  Result := 1;
end;

function xquery_xpathstringall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  Result := 0;
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    1: begin
         lua_pushstring(L, u.XPathStringAll(luaGetString(L, 1)));
         Result := 1;
       end;
    2: begin
         if lua_isstring(L, 2) then
         begin
           lua_pushstring(L, u.XPathStringAll(luaGetString(L, 1), luaGetString(L, 2)));
           Result := 1;
         end
         else
         if lua_isuserdata(L, 2) then
         begin
           u.XPathStringAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)));
           Result := 0;
         end;
       end;
    3: begin
         if lua_isstring(L, 2) then
         begin
           lua_pushstring(L, u.XPathStringAll(luaGetString(L, 1), luaGetString(L, 2),
             TLuaIXQValue(luaGetUserData(L, 3)).FIXQValue));
           Result := 1;
         end
         else
         if lua_isuserdata(L, 2) then
         begin
           u.XPathStringAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
             TLuaIXQValue(luaGetUserData(L, 3)).FIXQValue);
           Result := 0;
         end;
       end;
    end;
end;

function xquery_xpathhrefall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    3: u.XPathHREFAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
        TStrings(luaGetUserData(L, 3)));
    4: u.XPathHREFAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
        TStrings(luaGetUserData(L, 3)), TLuaIXQValue(luaGetUserData(L, 4)).FIXQValue)
  end;
  Result := 0;
end;

function xquery_xpathhreftitleall(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  case lua_gettop(L) of
    3: u.XPathHREFtitleAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
        TStrings(luaGetUserData(L, 3)));
    4: u.XPathHREFtitleAll(luaGetString(L, 1), TStrings(luaGetUserData(L, 2)),
        TStrings(luaGetUserData(L, 3)), TLuaIXQValue(luaGetUserData(L, 4)).FIXQValue)
  end;
  Result := 0;
end;

function xquery_xpathcount(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  u := TUserData(luaClassGetObject(L));
  if lua_gettop(L) = 2 then
    lua_pushinteger(L, u.XPathCount(luaGetString(L, 1),
      TLuaIXQValue(luaGetUserData(L, 2)).FIXQValue))
  else
    lua_pushinteger(L, u.XPathCount(luaGetString(L, 1)));
  Result := 1;
end;

const
  methods: packed array [0..8] of luaL_Reg = (
    (name: 'ParseHTML'; func: @xquery_parsehtml),
    (name: 'XPath'; func: @xquery_xpath),
    (name: 'XPathI'; func: @xquery_xpathi),
    (name: 'XPathString'; func: @xquery_xpathstring),
    (name: 'XPathStringAll'; func: @xquery_xpathstringall),
    (name: 'XPathHREFAll'; func: @xquery_xpathhrefall),
    (name: 'XPathHREFTitleAll'; func: @xquery_xpathhreftitleall),
    (name: 'XPathCount'; func: @xquery_xpathcount),
    (name: nil; func: nil)
    );

procedure luaXQueryAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
end;

procedure luaXQueryRegister(const L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'CreateTXQuery', @xquery_create);
end;

initialization
  luaClassRegister(TXQueryEngineHTML, @luaXQueryAddMetaTable, @luaXQueryRegister);

end.
