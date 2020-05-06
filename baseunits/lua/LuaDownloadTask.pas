unit LuaDownloadTask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

procedure luaDownloadTaskMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  LuaClass, LuaStrings, uDownloadsManager;

procedure luaDownloadTaskMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  with TTaskContainer(Obj) do
  begin
    luaClassAddObject(L, MetaTable, ChapterLinks, 'ChapterLinks', @luaStringsAddMetaTable);
    luaClassAddObject(L, MetaTable, ChapterNames, 'ChapterNames', @luaStringsAddMetaTable);
    luaClassAddObject(L, MetaTable, PageContainerLinks, 'PageContainerLinks', @luaStringsAddMetaTable);
    luaClassAddIntegerProperty(L, MetaTable, 'CurrentDownloadChapterPtr', @CurrentDownloadChapterPtr);
    luaClassAddIntegerProperty(L, MetaTable, 'PageNumber', @PageNumber);
    luaClassAddStringProperty(L, MetaTable, 'Link', @DownloadInfo.Link);
  end;
end;

initialization
  luaClassRegister(TTaskContainer, @luaDownloadTaskMetaTable);

end.

