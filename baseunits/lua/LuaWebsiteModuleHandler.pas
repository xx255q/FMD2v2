unit LuaWebsiteModuleHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif},
  LuaHandler, LuaBase, LuaClass;

type

  { TLuaWebsiteModuleHandler }

  TLuaWebsiteModuleHandler = class(TLuaHandler)
  private
    FLoadedModule: Pointer;
  public
    function LoadModule(const AModule: Pointer): TLuaWebsiteModuleHandler;
  end;

implementation

uses
  WebsiteModules, LuaWebsiteModules;

{ TLuaWebsiteModuleHandler }

function TLuaWebsiteModuleHandler.LoadModule(const AModule: Pointer): TLuaWebsiteModuleHandler;
var
  M: TLuaWebsiteModule;
begin
  if FLoadedModule = AModule then
    Exit(Self)
  else
  begin
    Result := nil;
    if FLoadedModule <> nil then // if module changed then cleanup lua state
    begin
      lua_close(FHandle);
      FHandle := LuaNewBaseState;
      FLoadedModule := nil;
      FLoadedChunks.Clear;
      FLoadedObjects.Clear;
    end;
    M:=TLuaWebsiteModule(TModuleContainer(AModule).LuaModule);
    M.LuaDoMe(FHandle);
    M.LuaPushMe(FHandle);
    FLoadedChunks.AddObject(M.Container.FileName, M.Container.ByteCode);
    FLoadedModule := AModule;
    Result := Self;
  end;
end;

end.
