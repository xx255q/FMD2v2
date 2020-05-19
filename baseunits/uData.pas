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
  BaseThread, LazFileUtils, strutils, httpsend;

type

  { TMangaInformation }

  TMangaInformation = class(TObject)
  private
    FOwner: TBaseThread;
    FModule: Pointer;
    procedure SetModule(const AValue: Pointer);
  public
    HTTP: THTTPSendThread;
    MangaInfo: TMangaInfo;
    Parse: TStringList;
    isGetByUpdater: Boolean;
    isGenerateFolderChapterName: Boolean;
    isRemoveUnicode: Boolean;
    isRemoveHostFromChapterLinks: Boolean;

    constructor Create(const AOwnerThread: TBaseThread = nil; const ACreateInfo: Boolean = True);
    destructor Destroy; override;
    procedure ClearInfo;
    function GetDirectoryPage(var APage: Integer): Byte;
    function GetNameAndLink(const ANames, ALinks: TStringList; AURL: String): Byte;
    function GetInfoFromURL(const AURL: String): Byte;
    procedure SyncInfoToData(const ADataProcess: TDBDataProcess); overload;
    procedure AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess); overload;
    property Thread: TBaseThread read FOwner;
    property Module: Pointer read FModule write SetModule;
  end;

var
  options: TStringList;

implementation

uses
  Dialogs, WebsiteModules, uUpdateThread;

{ TMangaInformation }

constructor TMangaInformation.Create(const AOwnerThread: TBaseThread;
  const ACreateInfo: Boolean);
begin
  inherited Create;
  FOwner := AOwnerThread;
  HTTP := THTTPSendThread.Create(AOwnerThread);
  HTTP.Headers.NameValueSeparator := ':';
  Parse := TStringList.Create;
  if ACreateInfo then
    MangaInfo := TMangaInfo.Create;
  isGetByUpdater := False;
  isRemoveHostFromChapterLinks := True;
end;

destructor TMangaInformation.Destroy;
begin
  if Assigned(MangaInfo) then
    MangaInfo.Free;
  if Assigned(Parse) then
    Parse.Free;
  HTTP.Free;
  inherited Destroy;
end;

procedure TMangaInformation.ClearInfo;
begin
  MangaInfo.Artists := '';
  MangaInfo.Authors := '';
  MangaInfo.Genres := '';
  MangaInfo.Summary := '';
  MangaInfo.CoverLink := '';
  MangaInfo.NumChapter := 0;
  MangaInfo.Status := '';
  MangaInfo.Title := '';
  MangaInfo.URL := '';
  MangaInfo.Module := nil;
  MangaInfo.ChapterNames.Clear;
  MangaInfo.ChapterLinks.Clear;
end;

procedure TMangaInformation.SetModule(const AValue: Pointer);
begin
  if FModule = AValue then Exit;
  FModule := AValue;
  if Assigned(FModule) and Assigned(HTTP) then
    TModuleContainer(FModule).PrepareHTTP(HTTP);
end;

function TMangaInformation.GetDirectoryPage(var APage: Integer): Byte;
begin
  APage := 1;

  //load pagenumber_config if available
  if  TModuleContainer(FModule).Settings.Enabled and (TModuleContainer(FModule).Settings.UpdateListDirectoryPageNumber > 0) then
  begin
    APage := TModuleContainer(FModule).Settings.UpdateListDirectoryPageNumber;
    BROWSER_INVERT := True;
    Exit(NO_ERROR);
  end;

  BROWSER_INVERT := False;
  if Assigned(TModuleContainer(FModule).OnGetDirectoryPageNumber) then
    Result := TModuleContainer(FModule).OnGetDirectoryPageNumber(Self, APage, TUpdateListThread(Thread).workPtr, TModuleContainer(FModule))
  else
    Exit(INFORMATION_NOT_FOUND);

  if APage < 1 then
    APage := 1;
end;

function TMangaInformation.GetNameAndLink(const ANames, ALinks: TStringList;
  AURL: String): Byte;
begin
  if Assigned(TModuleContainer(FModule).OnGetNameAndLink) then
    Result := TModuleContainer(FModule).OnGetNameAndLink(Self, ANames, ALinks, AURL, TModuleContainer(FModule))
  else
    Exit(INFORMATION_NOT_FOUND);

  //remove host from AURL
  if ALinks.Count > 0 then
    RemoveHostFromURLsPair(ALinks, ANames);
end;

function TMangaInformation.GetInfoFromURL(const AURL: String): Byte;
var
  s, s2: String;
  j, k: Integer;
  del: Boolean;
  bmangaInfo: TBaseMangaInfo;
begin
  if Trim(AURL) = '' then
    Exit(INFORMATION_NOT_FOUND);

  GetBaseMangaInfo(MangaInfo, bmangaInfo);

  MangaInfo.Module := FModule;
  MangaInfo.CoverLink := '';
  MangaInfo.NumChapter := 0;
  MangaInfo.ChapterNames.Clear;
  MangaInfo.ChapterLinks.Clear;

  if Assigned(TModuleContainer(FModule).OnGetInfo) then
  begin
    MangaInfo.URL := FillHost(TModuleContainer(FModule).RootURL, AURL);
    Result := TModuleContainer(FModule).OnGetInfo(Self, AURL, TModuleContainer(FModule));
  end
  else
    Exit(INFORMATION_NOT_FOUND);

  with MangaInfo do begin
    if Link = '' then
      Link := RemoveHostFromURL(MangaInfo.URL);

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
    FillBaseMangaInfo(MangaInfo, bmangaInfo);

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
      if isRemoveHostFromChapterLinks then
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
    with MangaInfo do
      ADataProcess.UpdateData(Title, Link, Authors, Artists, Genres, Status, Summary,
        NumChapter, ModuleID);
end;

procedure TMangaInformation.AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess);
begin
  if Assigned(ADataProcess) then
  begin
    if (MangaInfo.Title = '') and (ATitle <> '') then MangaInfo.Title := ATitle;
    if (MangaInfo.Link = '') and (ALink <> '') then MangaInfo.Link := ALink;
    with MangaInfo do
      ADataProcess.AddData(Title, Link, Authors, Artists, Genres, Status,
        StringBreaks(Summary), NumChapter, Now);
  end;
end;

end.
