unit LuaWebsiteModulesExtras;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaWebsiteModulesExtrasRegisterInit(L: Plua_State);
procedure luaWebsiteModulesExtrasRegisterAfterImageSaved(L: Plua_State);

implementation

uses
  LuaClass, LuaUtils, LuaFMD, MangaFoxWatermark;

function mf_loadtemplate(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, MangaFoxWatermark.LoadTemplate(luaGetString(L, 1)));
  Result := 1;
end;

function mf_removewatermark(L: Plua_State): Integer; cdecl;
var
  ASaveAsPNG: Boolean;
begin
  ASaveAsPNG := False;
  if lua_gettop(L) = 2 then
    ASaveAsPNG := lua_toboolean(L, 2);
  lua_pushboolean(L, MangaFoxWatermark.RemoveWatermark(luaGetString(L, 1), ASaveAsPNG));
  Result := 1;
end;

procedure luaWebsiteModulesExtrasRegisterInit(L: Plua_State);
begin
  luaFMDRegister(L);
  luaPushFunctionGlobal(L, 'MangaFoxLoadTemplate', @mf_loadtemplate);
end;

procedure luaWebsiteModulesExtrasRegisterAfterImageSaved(L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'MangaFoxRemoveWatermark', @mf_removewatermark);
end;

end.
