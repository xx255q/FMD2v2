unit LuaPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif}, LuaBase;

procedure RegisterLoader(const L: Plua_State);
procedure ScanAndLoadPackages(const APath: String);
procedure AddLib(const AName: String; const ARegLib: lua_CFunction);

implementation

uses FileUtil, LazFileUtils, MultiLog;

type

  TLuaLib = class
    RegLib: lua_CFunction;
  end;

  { TCachedPackage }

  TCachedPackage = class
  public
    FileName: String;
    Stream: TMemoryStream;
    constructor Create(const AFileName: String; const AStream: TMemoryStream);
    destructor Destroy; override;
  end;

var
  Packages: TStringList;

{ TCacheItem }

constructor TCachedPackage.Create(const AFileName: String;
  const AStream: TMemoryStream);
begin
  FileName := AFileName;
  Stream := AStream;
end;

destructor TCachedPackage.Destroy;
begin
  Stream.Free;
  inherited Destroy;
end;

{ LuaPackage }

function _findpackage(L: Plua_State): Integer; cdecl;
var
  i: Integer;
  c: TCachedPackage;
begin
  Result := 0;
  if Packages.Find(lua_tostring(L, 1), i) then
  begin
    if Packages.Objects[i] is TLuaLib then
    begin
      lua_pushcfunction(L, TLuaLib(Packages.Objects[i]).RegLib);
      Result := 1;
    end
    else
    begin
      c := TCachedPackage(Packages.Objects[i]);
      i := LuaLoadFromStreamOrFile(L, c.Stream, c.FileName);
      if i = 0 then
        Result := 1
      else
        Logger.SendError('require '+lua_tostring(L,1)+' '+LuaGetReturnString(i)+': '+lua_tostring(L,-1));
    end;
  end;
end;

procedure RegisterLoader(const L: Plua_State);
var
  top, loaders, i: Integer;
begin
  top:=lua_gettop(L);
  lua_getglobal(L, 'package');
  if LUA_VERSION_NUM = 501 then
    lua_getfield(L, -1, 'loaders')
  else
    lua_getfield(L, -1, 'searchers');
  loaders:=lua_gettop(L);
  i:=0;
  repeat
    Inc(i);
    lua_rawgeti(L, loaders, i);
  until lua_type(L, -1) <= LUA_TNIL;
  lua_pop(L, 1);
  for i:=i downto 2 do // shift items down to make a room
    lua_rawseti(L, loaders, i);
  lua_pushcfunction(L, @_findpackage);
  lua_rawseti(L, loaders,1);
  lua_settop(L,top);
end;

procedure ScanAndLoadPackages(const APath: String);
var
  files: TStringList;
  f, s: String;
  m: TMemoryStream;
begin
  if not DirectoryExists(APath) then Exit;
  Packages.Sorted := False;
  files := TStringList.Create;
    try
      FindAllFiles(files, APath, '*.lua', True, faAnyFile);
      for f in files do
      begin
        s := ExtractFileNameWithoutExt(f.Replace(APath, '', [])).Replace(PathDelim, '.', [rfReplaceAll]);
        m := nil;
        m := LuaDumpFileToStream(f);
        if Assigned(m) then
          Packages.AddObject(s, TCachedPackage.Create(f, m));
      end;
    finally
      files.Free;
    end;
  Packages.Sorted := True;
end;

procedure AddLib(const AName: String; const ARegLib: lua_CFunction);
var
  lib: TLuaLib;
begin
  lib := TLuaLib.Create;
  lib.RegLib := ARegLib;
  Packages.AddObject(AName, lib);
end;

initialization
  Packages := TStringList.Create;
  Packages.OwnsObjects := True;

finalization
  Packages.Free;

end.
