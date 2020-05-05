unit LuaMangaInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaMangaInfoAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);

implementation

uses
  uBaseUnit, LuaClass, LuaStrings;

procedure luaMangaInfoAddMetaTable(L: Plua_State; Obj: Pointer;
  MetaTable, UserData: Integer; AutoFree: Boolean = False);
begin
  with TMangaInfo(Obj) do
  begin
    luaClassAddStringProperty(L, MetaTable, 'URL', @URL);
    luaClassAddStringProperty(L, MetaTable, 'Title', @Title);
    luaClassAddStringProperty(L, MetaTable, 'Link', @Link);
    luaClassAddStringProperty(L, MetaTable, 'CoverLink', @CoverLink);
    luaClassAddStringProperty(L, MetaTable, 'Authors', @Authors);
    luaClassAddStringProperty(L, MetaTable, 'Artists', @Artists);
    luaClassAddStringProperty(L, MetaTable, 'Genres', @Genres);
    luaClassAddStringProperty(L, MetaTable, 'Status', @Status);
    luaClassAddStringProperty(L, MetaTable, 'Summary', @Summary);
    luaClassAddObject(L, MetaTable, ChapterNames, 'ChapterNames', @luaStringsAddMetaTable);
    luaClassAddObject(L, MetaTable, ChapterLinks, 'ChapterLinks', @luaStringsAddMetaTable);
  end;
end;

initialization
  luaClassRegister(TMangaInfo, @luaMangaInfoAddMetaTable);

end.

