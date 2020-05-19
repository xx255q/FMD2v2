unit LuaWebsiteModuleHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif},
  LuaHandler, LuaBase;

type

  { TLuaWebsiteModuleHandler }

  TLuaWebsiteModuleHandler = class(TLuaHandler)
  private
    FLoadedModule: Pointer;
  public
    function LoadModule(const AModule: Pointer): TLuaWebsiteModuleHandler;
    property Module: Pointer read FLoadedModule;
  end;

function GetLuaWebsiteModuleHandler(const AModule: Pointer): TLuaWebsiteModuleHandler;

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

var
  TM: TThreadManager;
  //OldEndThread: TEndThreadHandler;

threadvar
  _LuaHandler: TLuaWebsiteModuleHandler;

function GetLuaWebsiteModuleHandler(const AModule: Pointer): TLuaWebsiteModuleHandler;
begin
  if _LuaHandler=nil then
    _LuaHandler:=TLuaWebsiteModuleHandler.Create;
  Result:=_LuaHandler.LoadModule(AModule);
end;

// lua_close won't run __gc if it's called from different thread
// so, ReleaseThreadVars can't be used
procedure LuaEndThread(ExitCode : DWord);
begin
  if _LuaHandler<>nil then
  begin
    _LuaHandler.Free;
    _LuaHandler:=nil;
  end;
  // run in loop ?
  //if OldEndThread<>nil then
  //  OldEndThread(ExitCode);
end;

initialization
  GetThreadManager(TM);
  //OldEndThread := TM.EndThread;
  TM.EndThread := @LuaEndThread;
  SetThreadManager(TM);

end.
