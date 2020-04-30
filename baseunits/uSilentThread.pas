{
        File: uSilentThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
        ---------------------------------------------
        As the name "silent" suggests, the job of theses classes is to get
        manga information from the site and add them to download list or
        favorites silently.
}

unit uSilentThread;

{$mode delphi}

interface

uses
  SysUtils, fgl, uBaseUnit, uData, uDownloadsManager,
  WebsiteModules, FMDOptions, httpsendthread, BaseThread, LazFileUtils, MultiLog;

type

  TMetaDataType = (MD_DownloadAll, MD_AddToFavorites);

  { TSilentThreadMetaData }

  TSilentThreadMetaData = class
  private
    FModuleIndex: Integer;
  public
    MetaDataType: TMetaDataType;
    Module: TModuleContainer;
    Title,
    URL,
    SaveTo: String;
    constructor Create(const AType: TMetaDataType;
      const AModule: TModuleContainer; const ATitle, AURL, ASaveTo: String);
  end;

  TSilentThreadManager = class;

  { TSilentThread }

  TSilentThread = class(TBaseThread)
  protected
    FSavePath: String;
    Info: TMangaInformation;
    Freconnect: Cardinal;
    // manga information from main thread
    title, website, URL: String;
    ModuleIndex: Integer;
    procedure MainThreadAfterChecking; virtual;
    procedure Execute; override;
  public
    Manager: TSilentThreadManager;
    constructor Create;
    destructor Destroy; override;
    property SavePath: String read FSavePath write FSavePath;
  end;

  { TSilentAddToFavThread }

  TSilentAddToFavThread = class(TSilentThread)
  protected
    procedure MainThreadAfterChecking; override;
  end;

  { TSilentThreadManagerThread }

  TSilentThreadManagerThread = class(TBaseThread)
  protected
    procedure Checkout;
    procedure Execute; override;
  public
    Manager: TSilentThreadManager;
    destructor Destroy; override;
  end;

  TSilentThreadMetaDatas = TFPGList<TSilentThreadMetaData>;
  TSilentThreads = TFPGList<TSilentThread>;

  { TSilentThreadManager }

  TSilentThreadManager = class
  private
    FCS_META: TRTLCriticalSection;
    FCS_THREADS: TRTLCriticalSection;
    FLockAdd: Boolean;
    FManagerThread: TSilentThreadManagerThread;
    function GetCount: Integer;
    procedure StartManagerThread;
    procedure Checkout(Index: Integer);
  public
    MetaDatas: TSilentThreadMetaDatas;
    Threads: TSilentThreads;
    procedure Add(AType: TMetaDataType; AModule: TModuleContainer; ATitle, AURL: String; ASaveTo: String = '');
    procedure StopAll(WaitFor: Boolean = True);
    procedure UpdateLoadStatus;
    procedure BeginAdd;
    procedure EndAdd;
    property Count: Integer read GetCount;
    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  RS_SilentThreadLoadStatus = 'Loading: %d/%d';

implementation

uses
  frmMain, FMDVars;

{ TSilentThreadManagerThread }

procedure TSilentThreadManagerThread.Checkout;
var
  i: Integer;
begin
  if Terminated then Exit;
  if Manager.MetaDatas.Count = 0 then Exit;
  with Manager do
  begin
    i := 0;
    while i < MetaDatas.Count do
    begin
      if Terminated then Break;
      with MetaDatas[i] do
        if (Threads.Count < OptionMaxThreads) and Modules.CanCreateConnection(FModuleIndex) then
          Manager.Checkout(i)
        else
          Inc(i);
    end;
  end;
end;

procedure TSilentThreadManagerThread.Execute;
begin
  if Manager = nil then Exit;
  Self.Checkout;
  with manager do
    while (not Terminated) and (MetaDatas.Count > 0) do
    begin
      Sleep(SOCKHEARTBEATRATE);
      while (not Terminated) and (Threads.Count >= OptionMaxThreads) do
        Sleep(SOCKHEARTBEATRATE);
      Self.Checkout;
    end;
end;

destructor TSilentThreadManagerThread.Destroy;
begin
  Manager.FManagerThread := nil;
  inherited Destroy;
end;

{ TSilentThreadManager }

function TSilentThreadManager.GetCount: Integer;
begin
  Result := MetaDatas.Count + Threads.Count;
end;

procedure TSilentThreadManager.StartManagerThread;
begin
  if FManagerThread = nil then
  begin
    FManagerThread := TSilentThreadManagerThread.Create;
    FManagerThread.Manager := Self;
    FManagerThread.Start;
  end;
end;

procedure TSilentThreadManager.Add(AType: TMetaDataType;
  AModule: TModuleContainer; ATitle, AURL: String; ASaveTo: String);
begin
  if not ((AType = MD_AddToFavorites) and
    (FavoriteManager.IsMangaExist(ATitle, AModule.ID))) then
  begin
    EnterCriticalsection(FCS_META);
    try
      MetaDatas.Add(TSilentThreadMetaData.Create(
        AType, AModule, ATitle, AURL, ASaveTo));
      if not FLockAdd then
      begin
        StartManagerThread;
        UpdateLoadStatus;
      end;
    finally
      LeaveCriticalsection(FCS_META);
    end;
    if OptionSortDownloadsWhenAddingNewDownloadTasks then
      DLManager.Sort(DLManager.SortColumn);
  end;
end;

procedure TSilentThreadManager.Checkout(Index: Integer);
begin
  if (Index < 0) or (Index >= MetaDatas.Count) then Exit;
  Modules.IncActiveConnectionCount(MetaDatas[Index].FModuleIndex);
  EnterCriticalsection(FCS_THREADS);
  try
    case MetaDatas[Index].MetaDataType of
      MD_DownloadAll: Threads.Add(TSilentThread.Create);
      MD_AddToFavorites: Threads.Add(TSilentAddToFavThread.Create);
    end;
    with Threads.Last do
    begin
      Manager := Self;
      website := MetaDatas[Index].Module.ID;
      title := MetaDatas[Index].Title;
      URL := MetaDatas[Index].URL;
      SavePath := MetaDatas[Index].SaveTo;
      ModuleIndex := MetaDatas[Index].FModuleIndex;
      Start;
      MetaDatas[Index].Free;
      MetaDatas.Delete(Index);
    end;
  finally
    LeaveCriticalsection(FCS_THREADS);
  end;
end;

procedure TSilentThreadManager.StopAll(WaitFor: Boolean);
var
  i: Integer;
begin
  if Count = 0 then Exit;
  EnterCriticalsection(FCS_META);
  try
    if MetaDatas.Count > 0 then
    begin
      for i := 0 to MetaDatas.Count - 1 do
        MetaDatas[i].Free;
      MetaDatas.Clear;
    end;
  finally
    LeaveCriticalsection(FCS_META);
  end;
  if Assigned(FManagerThread) then
  begin
    FManagerThread.Terminate;
    if WaitFor then
      FManagerThread.WaitFor;
  end;
  EnterCriticalsection(FCS_THREADS);
  try
    if Threads.Count > 0 then
      for i := 0 to Threads.Count - 1 do
        Threads[i].Terminate;
  finally
    LeaveCriticalsection(FCS_THREADS);
  end;
  if WaitFor then
    while Threads.Count < 0 do
      sleep(32);
end;

procedure TSilentThreadManager.UpdateLoadStatus;
begin
  if Count > 0 then
    MainForm.sbMain.Panels[1].Text :=
      Format(RS_SilentThreadLoadStatus, [Threads.Count, Count])
  else
    MainForm.sbMain.Panels[1].Text := '';
end;

procedure TSilentThreadManager.BeginAdd;
begin
  FLockAdd := True;
end;

procedure TSilentThreadManager.EndAdd;
begin
  FLockAdd := False;
  if MetaDatas.Count > 0 then
  begin
    StartManagerThread;
    UpdateLoadStatus;
  end;
end;

constructor TSilentThreadManager.Create;
begin
  inherited Create;
  InitCriticalSection(FCS_META);
  InitCriticalSection(FCS_THREADS);
  FLockAdd := False;
  MetaDatas := TSilentThreadMetaDatas.Create;
  Threads := TSilentThreads.Create;
end;

destructor TSilentThreadManager.Destroy;
begin
  StopAll(True);
  MetaDatas.Free;
  Threads.Free;
  DoneCriticalsection(FCS_THREADS);
  DoneCriticalsection(FCS_META);
  inherited Destroy;
end;

{ TSilentThreadMetaData }

constructor TSilentThreadMetaData.Create(const AType: TMetaDataType;
  const AModule: TModuleContainer; const ATitle, AURL, ASaveTo: String);
begin
  inherited Create;
  MetaDataType := AType;
  Module := AModule;
  Title := ATitle;
  URL := AURL;
  SaveTo := ASaveTo;
end;

{ TSilentThread }

procedure TSilentThread.MainThreadAfterChecking;
var
  d: TTaskContainer;
  s: String;
  i: Integer;
begin
  if Info.mangaInfo.NumChapter = 0 then
    Exit;
  try
    with MainForm do
    begin
      // add a new download task
      d := DLManager.Items[DLManager.AddTask];
      d.DownloadInfo.ModuleID := website;

      if Trim(title) = '' then
        title := Info.mangaInfo.Title;
      for i := 0 to Info.mangaInfo.NumChapter - 1 do
      begin
        // generate folder name
        s := CustomRename(OptionChapterCustomRename,
                          website,
                          title,
                          info.mangaInfo.Authors,
                          Info.mangaInfo.Artists,
                          Info.mangaInfo.ChapterNames.Strings[i],
                          Format('%.4d', [i + 1]),
                          OptionChangeUnicodeCharacter,
                          OptionChangeUnicodeCharacterStr);
        d.chapterName.Add(s);
        d.chapterLinks.Add(
          Info.mangaInfo.ChapterLinks.Strings[i]);
      end;

      if cbAddAsStopped.Checked then
      begin
        d.Status := STATUS_STOP;
        d.downloadInfo.Status := Format('[%d/%d] %s',[0,d.ChapterLinks.Count,RS_Stopped]);
      end
      else
      begin
        d.downloadInfo.Status := Format('[%d/%d] %s',[0,d.ChapterLinks.Count,RS_Waiting]);
        d.Status := STATUS_WAIT;
      end;

      d.currentDownloadChapterPtr := 0;
      d.downloadInfo.Link := URL;
      d.downloadInfo.Title := title;
      d.downloadInfo.DateAdded := Now;
      d.downloadInfo.DateLastDownloaded := Now;

      if FSavePath = '' then
      begin
        FillSaveTo;
	OverrideSaveTo(d.DownloadInfo.Module);
        FSavePath := edSaveTo.Text;
        // save to
        if OptionGenerateMangaFolder then
          FSavePath := AppendPathDelim(FSavePath) + CustomRename(
            OptionMangaCustomRename,
            website,
            title,
            info.mangaInfo.Authors,
            info.mangaInfo.Artists,
            '',
            '',
            OptionChangeUnicodeCharacter,
            OptionChangeUnicodeCharacterStr);
      end;
      d.downloadInfo.SaveTo := FSavePath;
      d.SaveToDB;

      UpdateVtDownload;
      DLManager.CheckAndActiveTask(False);

      // save downloaded chapters
      if Info.mangaInfo.ChapterLinks.Count > 0 then
      begin
        DLManager.DownloadedChapters.Chapters[Info.mangaInfo.ModuleID, URL]:=
          Info.mangaInfo.ChapterLinks.Text;
        FavoriteManager.AddToDownloadedChaptersList(Info.mangaInfo.ModuleID,
          URL, Info.mangaInfo.ChapterLinks);
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TSilentThread.Execute;
begin
  Synchronize(Manager.UpdateLoadStatus);
  try
    Info.ModuleIndex := Self.ModuleIndex;
    Info.mangaInfo.Title := title;
    if Info.GetInfoFromURL(website, URL) = NO_ERROR then
      if not Terminated then
        Synchronize(MainThreadAfterChecking);
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

constructor TSilentThread.Create;
begin
  inherited Create(True);
  Freconnect := 3;
  SavePath := '';
  Info := TMangaInformation.Create(Self);
  ModuleIndex := -1;
end;

destructor TSilentThread.Destroy;
begin
  EnterCriticalsection(Manager.FCS_THREADS);
  try
    Modules.DecActiveConnectionCount(ModuleIndex);
    Manager.Threads.Remove(Self);
  finally
    LeaveCriticalsection(Manager.FCS_THREADS);
  end;
  Info.Free;
  if not isExiting then
    Synchronize(Manager.UpdateLoadStatus);
  inherited Destroy;
end;

{ TSilentAddToFavThread }

procedure TSilentAddToFavThread.MainThreadAfterChecking;
var
  s: String;
begin
  try
    with MainForm do
    begin
      if Trim(title) = '' then
        title := Info.mangaInfo.Title;
      if FSavePath = '' then
      begin
        FillSaveTo;
	OverrideSaveTo(Modules.LocateModule(website));
        s := edSaveTo.Text;
      end
      else
        s := FSavePath;
      if OptionGenerateMangaFolder then
        s := AppendPathDelim(s) + CustomRename(
          OptionMangaCustomRename,
          website,
          title,
          info.mangaInfo.Authors,
          info.mangaInfo.Artists,
          '',
          '',
          OptionChangeUnicodeCharacter,
          OptionChangeUnicodeCharacterStr);
      if Trim(title) = '' then
        title := Info.mangaInfo.Title;
      FavoriteManager.Add(title,
        IntToStr(Info.mangaInfo.NumChapter),
        info.mangaInfo.ChapterLinks.Text,
        website,
        s,
        URL);
      UpdateVtFavorites;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

end.
