unit LuaPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif}, LuaBase;

procedure RegisterLoader(const L: Plua_State);
procedure LoadPackages(const APath: String);

implementation

uses FileUtil, LazFileUtils, MultiLog;

type

  { TCachedPackage }

  TCachedPackage = class
  public
    FileName: String;
    Stream: TMemoryStream;
    constructor Create(const AFileName: String; const AStream: TMemoryStream);
    destructor Destroy; override;
  end;

var
  CachedPackages: TStringList;

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
  if CachedPackages.Find(lua_tostring(L, 1), i) then
  begin
    c := TCachedPackage(CachedPackages.Objects[i]);
    i := LuaLoadFromStreamOrFile(L, c.Stream, c.FileName);
    if i = 0 then
      Result := 1
    else
      Logger.SendError('require '+lua_tostring(L,1)+' '+LuaGetReturnString(i)+': '+lua_tostring(L,-1));
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
  lua_settop(L,lua_gettop(L)-1); // it pop everything if lua_pop(L,-1) on nil
  for i:=i downto 2 do // shift items down to make a room
    lua_rawseti(L, loaders, i);
  lua_pushcfunction(L, @_findpackage);
  lua_rawseti(L, loaders,1);
  lua_settop(L,top);
end;

procedure LoadPackages(const APath: String);
var
  files: TStringList;
  f, s: String;
  m: TMemoryStream;
begin
  if not DirectoryExists(APath) then Exit;
  CachedPackages.Sorted := False;
  files := TStringList.Create;
    try
      FindAllFiles(files, APath, '*.lua', True, faAnyFile);
      for f in files do
      begin
        s:=ExtractFileNameWithoutExt(f.Replace(APath, '', [])).Replace(PathDelim, '.', [rfReplaceAll]);
        m := nil;
        m := LuaDumpFileToStream(f);
        if Assigned(m) then
          CachedPackages.AddObject(s, TCachedPackage.Create(f, m));
      end;
    finally
      files.Free;
    end;
  CachedPackages.Sorted := True;
end;

initialization
  CachedPackages := TStringList.Create;
  CachedPackages.OwnsObjects := True;

finalization
  CachedPackages.Free;

end.
