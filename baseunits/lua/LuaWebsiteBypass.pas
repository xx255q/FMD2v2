unit LuaWebsiteBypass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread;

type

  { TWebsiteBypass }

  TWebsiteBypass = class
  public
    Module: Pointer;
    Guardian: TRTLCriticalSection;
    constructor Create(const AWebsiteModule: Pointer);
    destructor Destroy; override;
  end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const AMethod, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;

procedure doInitialization;

implementation

uses
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif},
  LuaWebsiteModules, LuaHandler, LuaBase, LuaUtils, LuaHTTPSend,
  WebsiteModules, FMDOptions, MultiLog;

var
  checkantibot_dump,
  websitebypass_dump: TMemoryStream;

  checkantibot_file: String = 'checkantibot.lua';
  websitebypass_file: String = 'websitebypass.lua';

procedure doInitialization;
begin
  checkantibot_file := LUA_WEBSITEBYPASS_FOLDER + checkantibot_file;
  websitebypass_file := LUA_WEBSITEBYPASS_FOLDER + websitebypass_file;

  if FileExists(checkantibot_file) then
    checkantibot_dump := LuaDumpFileToStream(checkantibot_file);
  if FileExists(websitebypass_file) then
    websitebypass_dump := LuaDumpFileToStream(websitebypass_file);
end;

procedure doFinalization;
begin
  if Assigned(checkantibot_dump) then
    checkantibot_dump.Free;
  if Assigned(websitebypass_dump) then
    websitebypass_dump.Free;
end;

function CheckAntiBotActive(const L: TLuaHandler): Boolean;
var
  r: Integer;
begin
  Result := False;
  if L.LoadChunkExecute(checkantibot_file, checkantibot_dump) = 0 then
  try
    L.ClearStack;
    lua_getglobal(L.Handle, '____CheckAntiBot');
    if lua_isnoneornil(L.Handle, -1) then Exit;
    r := lua_pcall(L.Handle, 0, LUA_MULTRET, 0);
    if r <> 0 then
      raise Exception.Create(LuaGetReturnString(r)+': '+luaGetString(L.Handle, -1));
    if lua_gettop(L.Handle) > 0 then
    begin
      Result := lua_toboolean(L.Handle, 1);
      lua_remove(L.Handle, 1); // remove first return(boolean)
    end;
  except
    on E: Exception do
      Logger.SendException('CheckAntiBot.Error', E);
  end;
end;

function WebsiteBypassGetAnswer(const L: TLuaHandler; const AMethod, AURL: String): Boolean;
var
  r: Integer;
begin
  Result := False;
  if L.LoadChunkExecute(websitebypass_file, websitebypass_dump) = 0 then
  try
    lua_getglobal(L.Handle, '____WebsiteBypass');
    if lua_isnoneornil(L.Handle, -1) then Exit;

    lua_pushstring(L.Handle, AMethod);
    lua_pushstring(L.Handle, AURL);
    lua_insert(L.Handle, 1); // move the param 2 to top
    lua_insert(L.Handle, 1); // move the param 1 to top
    lua_insert(L.Handle, 1); // move the function to top
    r := lua_pcall(L.Handle, lua_gettop(L.Handle)-1, LUA_MULTRET, 0); // call with all params
    if r <> 0 then
      raise Exception.Create(LuaGetReturnString(r)+': '+luaGetString(L.Handle, -1));
    Result := lua_toboolean(L.Handle, 1);
  except
    on E: Exception do
      Logger.SendException('WebsiteBypass.Error', E);
  end;
  L.ClearStack;
end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const AMethod, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;
var
  L: TLuaHandler;
begin
  if (checkantibot_dump = nil) or (websitebypass_dump = nil) then
  begin
    Result := AHTTP.HTTPRequest(AMethod, AURL);
    Exit;
  end;

  Result := False;
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.HTTPRequest(AMethod, AURL);

  if AHTTP.LuaHandler <> nil then
    L := TLuaHandler(AHTTP.LuaHandler)
  else
    L := TLuaHandler.Create;
  L.LoadObject('HTTP', AHTTP, @luaHTTPSendThreadAddMetaTable);
  if CheckAntiBotActive(L) then
  begin
    if TryEnterCriticalsection(AWebsiteBypass.Guardian) > 0 then
      try
        Result := WebsiteBypassGetAnswer(L, AMethod, AURL);
      finally
        LeaveCriticalsection(AWebsiteBypass.Guardian);
      end
    else begin
      if not AHTTP.ThreadTerminated then
        Result := AHTTP.HTTPRequest(AMethod, AURL);
    end;
  end;
  if AHTTP.LuaHandler = nil then
    L.Free;

  if Assigned(Response) then
    if Response is TStringList then
      TStringList(Response).LoadFromStream(AHTTP.Document)
    else
    if Response is TStream then
      AHTTP.Document.SaveToStream(TStream(Response));
end;

{ TWebsiteBypass }

constructor TWebsiteBypass.Create(const AWebsiteModule: Pointer);
begin
  Module:=AWebsiteModule;
  InitCriticalSection(Guardian);
end;

destructor TWebsiteBypass.Destroy;
begin
  DoneCriticalsection(Guardian);
  inherited Destroy;
end;

finalization
  doFinalization;

end.
