unit WebsiteBypass;

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
  FMDOptions, WebsiteModules, LuaWebsiteModules, lua53, LuaBase, LuaHTTPSend, LuaUtils,
  MultiLog;

var
  checkantibot_dump,
  websitebypass_dump: TMemoryStream;

  checkantibot_file: String = 'checkantibot.lua';
  websitebypass_file: String = 'websitebypass.lua';

const
  luabypass_dir = 'websitebypass';

procedure doInitialization;
begin
  checkantibot_file := LUA_REPO_FOLDER + luabypass_dir + PathDelim + checkantibot_file;
  websitebypass_file := LUA_REPO_FOLDER + 'websitebypass' + PathDelim + websitebypass_file;

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

function CheckAntiBotActive(const AHTTP: THTTPSendThread; var S: String): Boolean;
var
  L: Plua_State;
begin
  Result := False;
  if not Assigned(checkantibot_dump) then Exit;
  L := luaL_newstate;
  try
    // this method will be called very often(for each of http request), keep it minimal
    luaopen_base(L);
    luaopen_string(L);
    //luaL_openlibs(L);
    //LuaBaseRegisterPrint(L);
    luaPushObject(L, AHTTP, 'HTTP', @luaHTTPSendThreadAddMetaTable);
    lua_pop(L, lua_gettop(L));
    LuaExecute(L, checkantibot_dump, checkantibot_file, LUA_MULTRET);
    Result := lua_toboolean(L, 1);
    if Result and (lua_gettop(L) > 1) then
      S := lua_tostring(L, 2);
  except
    on E: Exception do
      Logger.SendError(E.Message + ': ' + luaGetString(L, -1));
  end;
  lua_close(L);
end;

function WebsiteBypassGetAnswer(const AHTTP: THTTPSendThread; const AMethod, AURL, S: String; const AWebsiteBypass: TWebsiteBypass): Boolean;
var
  L: Plua_State;
begin
  Result := False;
  if not Assigned(websitebypass_dump) then Exit;
  L := LuaNewBaseState;
  try
    luaPushStringGlobal(L, 'METHOD', AMethod);
    luaPushStringGlobal(L, 'URL', AURL);
    luaPushStringGlobal(L, 'S', S);
    luaPushObject(L, AHTTP, 'HTTP', @luaHTTPSendThreadAddMetaTable);
    luaPushObject(L, TLuaWebsiteModule(TModuleContainer(AWebsiteBypass.Module).LuaModule), 'MODULE', @luaWebsiteModuleAddMetaTable);
    lua_pop(L, lua_gettop(L));
    LuaExecute(L, websitebypass_dump, websitebypass_file, LUA_MULTRET);
    Result := lua_toboolean(L, 1);
  except
    on E: Exception do
      Logger.SendError(E.Message + ': ' + luaGetString(L, -1));
  end;
  lua_close(L);
end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const AMethod, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;
var
  S: String = '';
begin
  Result := False;
  if AHTTP = nil then Exit;
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.HTTPRequest(AMethod, AURL);
  if CheckAntiBotActive(AHTTP, S) then begin
    if TryEnterCriticalsection(AWebsiteBypass.Guardian) > 0 then
      try
        Result := WebsiteBypassGetAnswer(AHTTP, AMethod, AURL, S, AWebsiteBypass);
      finally
        LeaveCriticalsection(AWebsiteBypass.Guardian);
      end
    else begin
      if not AHTTP.ThreadTerminated then
        Result := AHTTP.HTTPRequest(AMethod, AURL);
    end;
  end;
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
