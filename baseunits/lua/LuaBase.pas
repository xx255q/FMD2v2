unit LuaBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua53, LuaClass;

procedure LuaBaseRegister(const L: Plua_State);
procedure LuaBaseRegisterPrint(const L: Plua_State); inline;
procedure LuaBaseRegisterSleep(const L: Plua_State); inline;
procedure luaPushObject(const L: Plua_State; const AObj: TObject; const AName: String;
  const AddMetaTable: TluaClassAddMetaTable = nil; const AutoFree: Boolean = False); inline;

function LuaDoFile(const AFileName: String; const AFuncName: String = ''): Plua_State;
function LuaNewBaseState: Plua_State;
procedure LuaCallFunction(const L: Plua_State; const AFuncName: String);
function LuaGetReturnString(const ReturnCode: Integer): String;

function LuaDumpFileToStream(const AFileName: String; const AStripDebug: Boolean = False): TMemoryStream; overload;
function LuaDumpFileToStream(const L: Plua_State; const AFileName: String;
  const AStripDebug: Boolean = True): TMemoryStream; overload;
function LuaLoadFromStream(const L: Plua_State; const AStream: TMemoryStream; const AName: String): Integer; inline;

procedure LuaExecute(const L: Plua_State; const AStream: TMemoryStream; const AFileName: String; const NResult: Integer = 0);

function _luawriter(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): Integer; cdecl;
function _luareader(L: Plua_State; ud: Pointer; sz: Psize_t): PAnsiChar; cdecl;

var
  AlwaysLoadLuaFromFile: Boolean = {$ifdef DEVBUILD}True{$else}False{$endif};

implementation

uses
  LuaStrings, LuaBaseUnit, LuaRegExpr, LuaPCRE2, LuaSynaUtil, LuaSynaCode,
  MultiLog, LuaCrypto, LuaImagePuzzle, LuaDuktape, LuaCriticalSection,
  LuaLogger, LuaUtils, LuaMemoryStream, LuaFMD;

function luabase_print(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to lua_gettop(L) do
    case lua_type(L, i) of
      LUA_TBOOLEAN:
        Logger.Send(BoolToStr(lua_toboolean(L, i), 'true', 'false'));
      else
        Logger.Send(luaGetString(L, i));
    end;
end;

function luabase_sleep(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  Sleep(lua_tointeger(L, 1));
end;

procedure LuaBaseRegister(const L: Plua_State);
begin
  luaPushStringGlobal(L, 'PathDelim', PathDelim);
  LuaBaseRegisterPrint(L);
  LuaBaseRegisterSleep(L);

  luaBaseUnitRegister(L);
  luaRegExprRegister(L);
  luaPCRE2Register(L);
  luaSynaUtilRegister(L);
  luaSynaCodeRegister(L);
  luaCryptoRegister(L);
  luaDuktapeRegister(L);
  luaLoggerRegister(L);

  luaClassRegisterAll(L);
end;

procedure LuaBaseRegisterPrint(const L: Plua_State);
begin
  lua_register(L, 'print', @luabase_print);
end;

procedure LuaBaseRegisterSleep(const L: Plua_State);
begin
  lua_register(L, 'Sleep', @luabase_sleep);
end;

procedure luaPushObject(const L: Plua_State; const AObj: TObject;
  const AName: String; const AddMetaTable: TluaClassAddMetaTable;
  const AutoFree: Boolean);
begin
  luaClassPushObject(L, AObj, AName, AutoFree, AddMetaTable);
end;

function LuaDoFile(const AFileName: String; const AFuncName: String
  ): Plua_State;
var
  r: Integer;
begin
  Result := nil;
  if not FileExists(AFileName) then Exit;
  Result := luaL_newstate;
  try
    luaL_openlibs(Result);
    LuaBaseRegister(Result);
    r := luaL_loadfilex(Result, PAnsiChar(AFileName), nil);
    if r = 0 then
      r := lua_pcall(Result, 0, LUA_MULTRET, 0);
    if r <> 0 then
      raise Exception.Create(LuaGetReturnString(r));
    if AFuncName <> '' then
      LuaCallFunction(Result, AFuncName);
  except
    on E: Exception do
      Logger.SendException('LuaDoFile.Error ' + E.Message + ' ' + lua_tostring(Result, -1), E);
  end;
end;

function LuaNewBaseState: Plua_State;
begin
  Result := luaL_newstate;
  try
    luaL_openlibs(Result);
    LuaBaseRegister(Result);
  except
    Logger.SendError(lua_tostring(Result, -1));
  end;
end;

procedure LuaCallFunction(const L: Plua_State; const AFuncName: String);
var
  r: Integer;
begin
  if lua_getglobal(L, PAnsiChar(AFuncName)) = 0 then
    raise Exception.Create('No function name ' + QuotedStr(AFuncName));
  r := lua_pcall(L, 0, LUA_MULTRET, 0);
  if r <> 0 then
    raise Exception.Create(LuaGetReturnString(r));
end;

function LuaGetReturnString(const ReturnCode: Integer): String;
begin
  case ReturnCode of
    LUA_OK: Result := 'LUA_OK';
    LUA_YIELD_: Result := 'LUA_YIELD_';
    LUA_ERRRUN: Result := 'LUA_ERRRUN';
    LUA_ERRSYNTAX: Result := 'LUA_ERRSYNTAX';
    LUA_ERRMEM: Result := 'LUA_ERRMEM';
    LUA_ERRGCMM: Result := 'LUA_ERRGCMM';
    LUA_ERRERR: Result := 'LUA_ERRERR';
    LUA_ERRFILE: Result := 'LUA_ERRFILE';
    else
      Result := IntToStr(ReturnCode);
  end;
end;

function _luawriter(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): Integer; cdecl;
begin
  if TMemoryStream(ud).Write(p^, sz) <> sz then
    Result := 1
  else
    Result := 0;
end;

function LuaDumpFileToStream(const AFileName: String; const AStripDebug: Boolean
  ): TMemoryStream;
var
  L: Plua_State;
begin
  if not FileExists(AFileName) then Exit;
  L := luaL_newstate;
  try
    luaL_openlibs(L);
    try
      Result := LuaDumpFileToStream(L, AFileName, AStripDebug);
    except
      on E: Exception do
        Logger.SendError(E.Message + ': ' + luaGetString(L, -1));
    end;
  finally
    lua_close(L);
  end;
end;

function LuaDumpFileToStream(const L: Plua_State; const AFileName: String;
  const AStripDebug: Boolean): TMemoryStream;
var
  strip: Integer;
begin
  if not FileExists(AFileName) then
    Exit;
  Result := TMemoryStream.Create;
  try
    if luaL_loadfilex(L, PAnsiChar(AFileName), nil) <> 0 then
      raise Exception.Create('');
    if AStripDebug then
      strip := 0
    else
      strip := 1;
    if lua_dump(L, @_luawriter, Result, strip) <> 0 then
      raise Exception.Create('');
  except
    on E: Exception do
    begin
      Result.Free;
      Result := nil;
      Logger.SendError(luaGetString(L, -1));
    end;
  end;
end;

function _luareader(L: Plua_State; ud: Pointer; sz: Psize_t): PAnsiChar; cdecl;
var
  m: TMemoryStream;
begin
  m := TMemoryStream(ud);
  if m.Size = 0 then
  begin
    Result := nil;
    Exit;
  end;
  Result := PAnsiChar(m.Memory);
  sz^ := m.Size;
end;

function LuaLoadFromStream(const L: Plua_State; const AStream: TMemoryStream;
  const AName: String): Integer;
begin
  Result := lua_load(L, @_luareader, AStream, PAnsiChar(AName), 'b');
end;

procedure LuaExecute(const L: Plua_State; const AStream: TMemoryStream;
  const AFileName: String; const NResult: Integer);
var
  r: Integer;
begin
  if AlwaysLoadLuaFromFile then
    r := luaL_loadfilex(L, PAnsiChar(AFileName), nil)
  else
    r := lua_load(L, @_luareader, AStream, PAnsiChar(AFileName), 'b');
  if r = 0 then
    r := lua_pcall(L, 0, NResult, 0);
  if r <> 0 then
    raise Exception.Create(LuaGetReturnString(r));
end;

end.
