{
        File: uData.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uData;

{$mode objfpc}{$H+}

// This unit contains all necessary functions for data processing

interface

uses
  Classes, SysUtils, uBaseUnit, DBDataProcess, FMDOptions, httpsendthread,
  BaseThread, LazFileUtils, strutils, RegExpr, httpsend, MultiLog;

type

  { TMangaInformation }

  TMangaInformation = class(TObject)
  private
    FOwner: TBaseThread;
    FModuleIndex: Integer;
    procedure SetModuleIndex(AValue: Integer);
  public
    isGetByUpdater: Boolean;
    mangaInfo: TMangaInfo;
    parse: TStringList;
    isGenerateFolderChapterName: Boolean;
    isRemoveUnicode: Boolean;
    RemoveHostFromChapterLinks: Boolean;
    FHTTP: THTTPSendThread;

    constructor Create(AOwnerThread: TBaseThread = nil; ACreateInfo: Boolean = True);
    destructor Destroy; override;
    procedure ClearInfo;
    function GetDirectoryPage(var APage: Integer; const AModuleID: String): Byte;
    function GetNameAndLink(const ANames, ALinks: TStringList; const AModuleID, AURL: String): Byte;
    function GetInfoFromURL(const AModuleID, AURL: String): Byte;
    procedure SyncInfoToData(const ADataProcess: TDBDataProcess); overload;
    procedure AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess); overload;
    //wrapper
    function GetPage(var AOutput: TObject; AURL: String; const AReconnect: Integer = 0): Boolean; inline;
    property Thread: TBaseThread read FOwner;
    property ModuleIndex: Integer read FModuleIndex write SetModuleIndex;
  end;

var
  options: TStringList;

implementation

uses
  Dialogs, frmMain, WebsiteModules, uUpdateThread;

{ TMangaInformation }

constructor TMangaInformation.Create(AOwnerThread: TBaseThread; ACreateInfo: Boolean);
begin
  inherited Create;
  FOwner := AOwnerThread;
  FHTTP := THTTPSendThread.Create(AOwnerThread);
  FHTTP.Headers.NameValueSeparator := ':';
  parse := TStringList.Create;
  if ACreateInfo then
    mangaInfo := TMangaInfo.Create;
  isGetByUpdater := False;
  ModuleIndex := -1;
  RemoveHostFromChapterLinks := True;
end;

destructor TMangaInformation.Destroy;
begin
  if Assigned(mangaInfo) then
    mangaInfo.Free;
  if Assigned(parse) then
    parse.Free;
  FHTTP.Free;
  inherited Destroy;
end;

procedure TMangaInformation.ClearInfo;
begin
  mangaInfo.Artists := '';
  mangaInfo.Authors := '';
  mangaInfo.Genres := '';
  mangaInfo.Summary := '';
  mangaInfo.CoverLink := '';
  mangaInfo.NumChapter := 0;
  mangaInfo.Status := '';
  mangaInfo.Title := '';
  mangaInfo.URL := '';
  mangaInfo.ModuleID := '';
  mangaInfo.ChapterNames.Clear;
  mangaInfo.ChapterLinks.Clear;
end;

procedure TMangaInformation.SetModuleIndex(AValue: Integer);
begin
  if FModuleIndex = AValue then Exit;
  FModuleIndex := AValue;
  if (FModuleIndex <> -1) and Assigned(FHTTP) then
    WebsiteModules.Modules[FModuleIndex].PrepareHTTP(FHTTP);
end;

function TMangaInformation.GetDirectoryPage(var APage: Integer; const AModuleID: String): Byte;
begin
  APage := 1;

  //load pagenumber_config if available
  if  Modules[ModuleIndex].Settings.Enabled and (Modules[ModuleIndex].Settings.UpdateListDirectoryPageNumber > 0) then
  begin
    APage := Modules[ModuleIndex].Settings.UpdateListDirectoryPageNumber;
    BROWSER_INVERT := True;
    Exit(NO_ERROR);
  end;

  BROWSER_INVERT := False;
  if ModuleIndex < 0 then
    ModuleIndex := Modules.LocateModuleByID(AModuleID);
  if Modules.ModuleAvailable(ModuleIndex, MMGetDirectoryPageNumber) then
    Result := Modules.GetDirectoryPageNumber(Self, APage, TUpdateListThread(Thread).workPtr, ModuleIndex)
  else
    Exit(INFORMATION_NOT_FOUND);

  if APage < 1 then
    APage := 1;
end;

function TMangaInformation.GetNameAndLink(const ANames, ALinks: TStringList;
  const AModuleID, AURL: String): Byte;
begin
  if ModuleIndex < 0 then
    ModuleIndex := Modules.LocateModuleByID(AModuleID);
  if Modules.ModuleAvailable(ModuleIndex, MMGetNameAndLink) then
  begin
    Result := Modules.GetNameAndLink(Self, ANames, ALinks, AURL, ModuleIndex)
  end
  else
    Exit(INFORMATION_NOT_FOUND);

  //remove host from AURL
  if ALinks.Count > 0 then
    RemoveHostFromURLsPair(ALinks, ANames);
end;

function TMangaInformation.GetInfoFromURL(const AModuleID, AURL: String): Byte;
var
  s, s2: String;
  j, k: Integer;
  del: Boolean;
  bmangaInfo: TBaseMangaInfo;
begin
  if Trim(AURL) = '' then
    Exit(INFORMATION_NOT_FOUND);

  GetBaseMangaInfo(mangaInfo, bmangaInfo);

  mangaInfo.ModuleID := AModuleID;
  mangaInfo.CoverLink := '';
  mangaInfo.NumChapter := 0;
  mangaInfo.ChapterNames.Clear;
  mangaInfo.ChapterLinks.Clear;

  if ModuleIndex < 0 then
    ModuleIndex := Modules.LocateModuleByID(AModuleID);
  if Modules.ModuleAvailable(ModuleIndex, MMGetInfo) then begin
    mangaInfo.URL := FillHost(Modules.Module[ModuleIndex].RootURL, AURL);
    Result := Modules.GetInfo(Self, AURL, ModuleIndex);
  end
  else
    Exit(INFORMATION_NOT_FOUND);

  with mangaInfo do begin
    if Link = '' then
      Link := RemoveHostFromURL(mangaInfo.URL);

    // cleanup info
    CoverLink := CleanURL(CoverLink);
    Title := Trim(FixWhiteSpace(RemoveStringBreaks(CommonStringFilter(Title))));
    Authors := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(Authors))));
    Artists := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(Artists))));
    Genres := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(Genres))));

    Authors := TrimRightChar(Trim(FixWhiteSpace(Authors)), [',']);
    Artists := TrimRightChar(Trim(FixWhiteSpace(Artists)), [',']);
    Genres := TrimRightChar(Trim(FixWhiteSpace(Genres)), [',']);

    Summary := CleanMultilinedString(FixWhiteSpace(Summary));

    // fix info
    if (LeftStr(Authors, 1) = '<') or (Authors = '-') or (Authors = ':') then
      Authors := '';
    if (LeftStr(Artists, 1) = '<') or (Artists = '-') or (Artists = ':') then
      Artists := '';
    if (Summary = '-') or (Summary = ':') then
      Summary := '';
    if Title = '' then
      Title := 'N/A';
    FillBaseMangaInfo(mangaInfo, bmangaInfo);

    // cleanup chapters
    if ChapterLinks.Count > 0 then begin
      while ChapterNames.Count < ChapterLinks.Count do
        ChapterNames.Add('');
      while ChapterLinks.Count < ChapterNames.Count do
        ChapterNames.Delete(ChapterNames.Count - 1);
      for j := 0 to ChapterLinks.Count - 1 do begin
        ChapterLinks[j] := Trim(ChapterLinks[j]);
        ChapterNames[j] := Trim(ChapterNames[j]);
      end;
    end;

    // remove duplicate chapter
    if ChapterLinks.Count > 0 then
    begin
      j := 0;
      while j < (ChapterLinks.Count - 1) do
      begin
        del := False;
        if (j + 1) < ChapterLinks.Count then
          for k := j + 1 to ChapterLinks.Count - 1 do
            if SameText(ChapterLinks[j], ChapterLinks[k]) then
            begin
              ChapterLinks.Delete(j);
              ChapterNames.Delete(j);
              del := True;
              Break;
            end;
        if not del then
          Inc(j);
      end;
    end;

    if ChapterLinks.Count > 0 then
    begin
      // remove host from chapter links
      if RemoveHostFromChapterLinks then
        RemoveHostFromURLsPair(ChapterLinks, ChapterNames);
      // fixing chapter name
      for j := 0 to ChapterNames.Count - 1 do
        ChapterNames[j] := Trim(CleanString(RemoveStringBreaks(
          CommonStringFilter(ChapterNames[j]))));

      //remove manga name from chapter
      if OptionRemoveMangaNameFromChapter and (Title <> '') then
      begin
        s := LowerCase(Title);
        j := Length(s);
        for k := 0 to ChapterNames.Count - 1 do begin
          s2 := LowerCase(ChapterNames[k]);
          if Length(s2) > j then
            if Pos(s, s2) = 1 then begin
              s2 := ChapterNames[k];
              Delete(s2, 1, j);
              s2 := Trim(s2);
              if LeftStr(s2, 2) = '- ' then
                Delete(s2, 1, 2);
              ChapterNames[k] := s2;
            end;
        end;
      end;
    end;

    NumChapter := ChapterLinks.Count;
  end;
end;

procedure TMangaInformation.SyncInfoToData(const ADataProcess: TDBDataProcess);
begin
  if Assigned(ADataProcess) then
    with mangaInfo do
      ADataProcess.UpdateData(Title, Link, Authors, Artists, Genres, Status, Summary,
        NumChapter, ModuleID);
end;

procedure TMangaInformation.AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess);
begin
  if Assigned(ADataProcess) then
  begin
    if (mangaInfo.Title = '') and (ATitle <> '') then mangaInfo.Title := ATitle;
    if (mangaInfo.Link = '') and (ALink <> '') then mangaInfo.Link := ALink;
    with mangaInfo do
      ADataProcess.AddData(Title, Link, Authors, Artists, Genres, Status,
        StringBreaks(Summary), NumChapter, Now);
  end;
end;

function TMangaInformation.GetPage(var AOutput: TObject; AURL: String; const AReconnect: Integer): Boolean;
begin
  Result := uBaseUnit.GetPage(FHTTP, AOutput, AURL, AReconnect);
end;

end.
