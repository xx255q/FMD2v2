unit LuaBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaBaseUnitRegister(L: Plua_State); inline;

implementation

uses
  LuaUtils, uBaseUnit, httpsendthread, LazFileUtils, dateutils;

function lua_trim(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) > 1 then
    lua_pushstring(L, luaGetString(L, 1).Trim(luaGetString(L, 2).ToCharArray))
  else
    lua_pushstring(L, Trim(luaGetString(L, 1)));
  Result := 1;
end;

function lua_maybefillhost(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MaybeFillHost(luaGetString(L, 1), luaGetString(L, 2)));
  Result := 1;
end;

function lua_fillhost(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, FillHost(luaGetString(L, 1), luaGetString(L, 2)));
  Result := 1;
end;

function lua_gethosturl(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, GetHostURL(luaGetString(L, 1)));
  Result := 1;
end;

function lua_removehostfromurl(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, RemoveHostFromURL(luaGetString(L, 1)));
  Result := 1;
end;

function lua_invertstrings(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to lua_gettop(L) do
    InvertStrings(TStringList(luaGetUserData(L, i)));
end;

function lua_mangainfostatusifpos(L: Plua_State): Integer; cdecl;
begin
  case lua_gettop(L) of
    3: lua_pushstring(L, MangaInfoStatusIfPos(luaGetString(L, 1),
        luaGetString(L, 2), luaGetString(L, 3)));
    2: lua_pushstring(L, MangaInfoStatusIfPos(luaGetString(L, 1), luaGetString(L, 2)));
    1: lua_pushstring(L, MangaInfoStatusIfPos(luaGetString(L, 1)));
    else
      Exit(0);
  end;
  Result := 1;
end;

function lua_appendurldelim(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, AppendURLDelim(luaGetString(L, 1)));
  Result := 1;
end;

function lua_removeurldelim(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, RemoveURLDelim(luaGetString(L, 1)));
  Result := 1;
end;

function lua_appendurldelimleft(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, AppendURLDelimLeft(luaGetString(L, 1)));
  Result := 1;
end;

function lua_removeurldelimleft(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, RemoveURLDelimLeft(luaGetString(L, 1)));
  Result := 1;
end;

function lua_spliturl(L: Plua_State): Integer; cdecl;
var
  t: Integer;
  host: string = '';
  path: string = '';
  includeprotocol: Boolean = true;
  includeport: Boolean = true;
begin
  t:=lua_gettop(L);
  if t>2 then
    includeport := lua_toboolean(L,3);
  if t>1 then
    includeprotocol := lua_toboolean(L,2);
  SplitURL(luaGetString(L, 1), @host, @path, includeprotocol, includeport);

  lua_pushstring(L, host);
  lua_pushstring(L, path);
  Result := 2;
end;

function lua_getcurrenttime(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, MilliSecondsBetween(Now, 0));
  Result := 1;
end;

const
  methods: packed array [0..13] of luaL_Reg = (
    (name: 'Trim'; func: @lua_trim),
    (name: 'MaybeFillHost'; func: @lua_maybefillhost),
    (name: 'FillHost'; func: @lua_fillhost),
    (name: 'GetHostURL'; func: @lua_gethosturl),
    (name: 'RemoveHostFromURL'; func: @lua_removehostfromurl),
    (name: 'SplitURL'; func: @lua_spliturl),
    (name: 'InvertStrings'; func: @lua_invertstrings),
    (name: 'MangaInfoStatusIfPos'; func: @lua_mangainfostatusifpos),
    (name: 'AppendURLDelim'; func: @lua_appendurldelim),
    (name: 'AppendURLDelimleft'; func: @lua_appendurldelimleft),
    (name: 'RemoveURLDelim'; func: @lua_removeurldelim),
    (name: 'RemoveURLDelimLeft'; func: @lua_removeurldelimleft),
    (name: 'GetCurrentTime'; func: @lua_getcurrenttime),
    (name: nil; func: nil)
    );

procedure luaBaseUnitRegister(L: Plua_State);
begin
  luaNewLib(L, methods);
end;

end.
