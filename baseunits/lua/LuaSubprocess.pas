unit LuaSubprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaSubprocessAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses LuaClass, LuaUtils, LuaPackage;

type
  TUserData = TProcess;

function subprocess_create(L: Plua_State): Integer; cdecl;
begin
  luaClassPushObject(L, TProcess.Create(nil), '', True, @luaSubprocessAddMetaTable);
  Result := 1;
end;

function _runcommand(L:Plua_State;Options:TProcessOptions=[];SWOptions:TShowWindowOptions=swoNone): Integer;
var
  args: array of string;
  r: Boolean;
  s: String;
  i: Integer;
begin
  SetLength(args,lua_gettop(L)-1);
  s:='';
  r:=False;
  for i:=2 to lua_gettop(L) do
    args[i-2] := luaToString(L,i);
  r := process.RunCommand(luaToString(L,1), args, s, [], swoNone);
  lua_pushboolean(L, r);
  lua_pushstring(L, s);
  Result:=2;
end;

function subprocess_runcommand(L: Plua_State): Integer; cdecl;
begin
  Result:=_runcommand(L);
end;

function subprocess_runcommandhide(L: Plua_State): Integer; cdecl;
begin
  Result:=_runcommand(L, [], swoHIDE);
end;

const
  constructs: packed array [0..4] of luaL_Reg = (
    (name: 'New'; func: @subprocess_create),
    (name: 'Create'; func: @subprocess_create),
    (name: 'RunCommand'; func: @subprocess_runcommand),
    (name: 'RunCommandHide'; func: @subprocess_runcommandhide),
    (name: nil; func: nil)
    );

procedure luaSubprocessAddMetaTable(const L: Plua_State; const Obj: Pointer; const MetaTable,
  UserData: Integer);
begin

end;

function luaopen_subprocess(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, constructs);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('subprocess', @luaopen_subprocess);

end.
