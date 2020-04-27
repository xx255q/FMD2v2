unit LuaMangaInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaMangaInfoAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);

implementation

uses
  uBaseUnit, LuaClass;

procedure luaMangaInfoAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  with TMangaInfo(Obj) do
  begin
    luaClassAddStringProperty(L, MetaTable, 'URL', @url);
    luaClassAddStringProperty(L, MetaTable, 'Title', @title);
    luaClassAddStringProperty(L, MetaTable, 'Link', @link);
    luaClassAddStringProperty(L, MetaTable, 'Website', @website);
    luaClassAddStringProperty(L, MetaTable, 'CoverLink', @coverLink);
    luaClassAddStringProperty(L, MetaTable, 'Authors', @authors);
    luaClassAddStringProperty(L, MetaTable, 'Artists', @artists);
    luaClassAddStringProperty(L, MetaTable, 'Genres', @genres);
    luaClassAddStringProperty(L, MetaTable, 'Status', @status);
    luaClassAddStringProperty(L, MetaTable, 'Summary', @summary);
    luaClassAddObject(L, MetaTable, chapterName, 'ChapterNames');
    luaClassAddObject(L, MetaTable, chapterLinks, 'ChapterLinks');
  end;
end;

initialization
  luaClassRegister(TMangaInfo, @luaMangaInfoAddMetaTable);

end.

