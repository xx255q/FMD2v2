unit WebsiteBypass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread;

type

  { TWebsiteBypass }

  TWebsiteBypass = class
  public
    WebsiteModule: TObject;
    Guardian: TRTLCriticalSection;
    constructor Create(AWebsiteModule: TObject);
    destructor Destroy; override;
  end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const Method, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;

procedure doInitialization;

implementation

uses
  FMDOptions, WebsiteModules, LuaWebsiteModules, lua53, LuaBase, LuaHTTPSend, LuaUtils,
  MultiLog;

var
  luadump_websitebypass_checkantibot,
  luadump_websitebypass: TMemoryStream;

  luafile_websitebypass_checkantibot,
  luafile_websitebypass: String;

procedure doInitialization;
begin
  luafile_websitebypass_checkantibot := LUA_REPO_FOLDER + 'websitebypass' + PathDelim + 'checkantibot.lua';
  luafile_websitebypass := LUA_REPO_FOLDER + 'websitebypass' + PathDelim + 'websitebypadd.lua';

  if FileExists(luafile_websitebypass_checkantibot) then
    luadump_websitebypass_checkantibot := LuaDumpFileToStream(luafile_websitebypass_checkantibot);
  if FileExists(luafile_websitebypass) then
    luadump_websitebypass := LuaDumpFileToStream(luafile_websitebypass);
end;

procedure doFinalization;
begin
  if Assigned(luadump_websitebypass_checkantibot) then
    luadump_websitebypass_checkantibot.Free;
  if Assigned(luadump_websitebypass) then
    luadump_websitebypass.Free;
end;

function CheckAntiBotActive(const AHTTP: THTTPSendThread; var S: String): Boolean;
var
  L: Plua_State;
  r: Integer = 0;
begin
  Result := False;
  if not Assigned(luadump_websitebypass_checkantibot) then Exit;
  L := luaL_newstate;
  try
    // this method will be called very often(for each of http request), keep it minimal
    r := lua_load(L, @_luareader, luadump_websitebypass_checkantibot, nil, 'b');
    if r <> 0 then
      raise Exception.Create(LuaGetReturnString(r));
    luaopen_base(L);
    luaopen_string(L);
    //luaL_openlibs(L);
    //LuaBaseRegisterPrint(L);
    luaPushObject(L, AHTTP, 'HTTP', @luaHTTPSendThreadAddMetaTable);
    lua_pop(L, lua_gettop(L));
    LuaExecute(L, luadump_websitebypass_checkantibot, luafile_websitebypass_checkantibot, LUA_MULTRET);
    Result := lua_toboolean(L, 1);
    if Result and (lua_gettop(L) > 1) then
      S := lua_tostring(L, 2);
  except
    on E: Exception do
      Logger.SendError(E.Message + ': ' + luaGetString(L, -1));
  end;
  lua_close(L);
end;

function WebsiteBypassGetAnswer(const AHTTP: THTTPSendThread; const AURL, S: String; const AWebsiteBypass: TWebsiteBypass): Boolean;
var
  L: Plua_State;
begin
  Result := False;
  if not Assigned(luadump_websitebypass) then Exit;
  L := LuaNewBaseState;
  try
    luaPushStringGlobal(L, 'URL', AURL);
    luaPushStringGlobal(L, 'S', S);
    luaPushObject(L, AHTTP, 'HTTP', @luaHTTPSendThreadAddMetaTable);
    luaPushObject(L, TModuleContainer(AWebsiteBypass.WebsiteModule), 'MODULE', @luaWebsiteModuleAddMetaTable);
    lua_pop(L, lua_gettop(L));
    LuaExecute(L, luadump_websitebypass, luafile_websitebypass, LUA_MULTRET);
    Result := lua_toboolean(L, 1);
  except
    on E: Exception do
      Logger.SendError(E.Message + ': ' + luaGetString(L, -1));
  end;
  lua_close(L);
end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const Method, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;
var
  S: String = '';
begin
  Result := False;
  if AHTTP = nil then Exit;
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.HTTPRequest(Method, AURL);
  if CheckAntiBotActive(AHTTP, S) then begin
    if TryEnterCriticalsection(AWebsiteBypass.Guardian) > 0 then
      try
        Result := WebsiteBypassGetAnswer(AHTTP, AURL, S, AWebsiteBypass);
      finally
        LeaveCriticalsection(AWebsiteBypass.Guardian);
      end
    else begin
      if not AHTTP.ThreadTerminated then
        Result := AHTTP.HTTPRequest(Method, AURL);
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

constructor TWebsiteBypass.Create(AWebsiteModule: TObject);
begin
  WebsiteModule:=AWebsiteModule;
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
