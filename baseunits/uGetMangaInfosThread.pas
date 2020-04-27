{
        File: uGetMangaInfosThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader

        -----------------

        This class allows us to get infomation on certain site and shows it
        in FMD's Manga infos tab.
}

unit uGetMangaInfosThread;

{$mode delphi}

interface

uses
  SysUtils, Graphics, Dialogs, uBaseUnit, uData, FMDOptions, BaseThread,
  ImgInfos, webp, MemBitmap, VirtualTrees;

type

  { TGetMangaInfosThread }

  TGetMangaInfosThread = class(TBaseThread)
  protected
    FMangaListNode: PVirtualNode;
    FCover: TPicture;
    FModuleID,
    FTitle,
    FLink: String;
    FInfo: TMangaInformation;
    FNumChapter: Cardinal;
    // Return TRUE if we can load manga cover.
    FIsHasMangaCover: Boolean;
    // Flush this thread, means that the result will not be shown.
    procedure Execute; override;
    procedure MainThreadSyncInfos;
    procedure MainThreadShowInfos;
    procedure MainThreadShowCover;
    procedure MainThreadShowCannotGetInfo;
    procedure LoadCover;
  public
    constructor Create;
    destructor Destroy; override;
    property Title: String read FTitle write FTitle;
    property ModuleID: String read FModuleID write FModuleID;
    property Link: String read FLink write FLink;
    property MangaListNode: PVirtualNode read FMangaListNode write FMangaListNode;
  end;

implementation

uses
  frmMain, WebsiteModules, FMDVars;

procedure TGetMangaInfosThread.MainThreadSyncInfos;
begin
  FInfo.SyncInfoToData(DataProcess);
  dataProcess.Commit;
end;

procedure TGetMangaInfosThread.Execute;

  function GetMangaInfo: Boolean;
  var
    infob: byte;
    data: PMangaInfoData;
  begin
    Result := False;
    try
      FInfo.mangaInfo.ModuleID := ModuleID;
      FInfo.mangaInfo.URI := Link;
      FInfo.mangaInfo.Title := Title;
      FInfo.ModuleIndex := Modules.LocateModuleByID(ModuleID);
      data := MainForm.vtMangaList.GetNodeData(FMangaListNode);
      if Assigned(FMangaListNode) and (MainForm.cbSelectManga.ItemIndex<>-1) and
        (ModuleID = MainForm.cbSelectManga.Items[MainForm.cbSelectManga.ItemIndex]) then
      begin
        if FInfo.mangaInfo.Title = '' then
          FInfo.mangaInfo.Title := data^.Title;
        FInfo.mangaInfo.URI := data^.URI;
        FInfo.mangaInfo.Authors := data^.Authors;
        FInfo.mangaInfo.Artists := data^.Artists;
        FInfo.mangaInfo.Status := data^.Status;
        FInfo.mangaInfo.Summary := data^.Summary;
        FInfo.mangaInfo.NumChapter := data^.NumChapter;
        FInfo.mangaInfo.Genres := data^.Genres;
        FNumChapter := data^.NumChapter;
      end;
      FInfo.isGenerateFolderChapterName := OptionGenerateMangaFolder;
      FInfo.isRemoveUnicode := OptionChangeUnicodeCharacter;

      infob := INFORMATION_NOT_FOUND;

      //wait if there is concurrent connection limit
      if Modules.MaxConnectionLimit[FInfo.ModuleIndex] > 0 then
      begin
        while not Modules.CanCreateConnection(FInfo.ModuleIndex) do
          Sleep(SOCKHEARTBEATRATE);
        Modules.IncActiveConnectionCount(FInfo.ModuleIndex);
      end;

      infob := FInfo.GetInfoFromURL(ModuleID, Link);

      if Terminated or isExiting then Exit;
      if infob <> NO_ERROR then Exit;

      //set back if title changed
      if (FInfo.mangaInfo.Title <> '') and (FInfo.mangaInfo.Title <> FTitle) then
        FTitle := FInfo.mangaInfo.Title;

      if Assigned(data) then
      begin
        if dataProcess.WebsiteLoaded(ModuleID) then
        begin
          if SitesWithoutInformation(ModuleID) then
          begin
            if FInfo.mangaInfo.Authors = '' then
              FInfo.mangaInfo.Authors := data^.Authors;
            if FInfo.mangaInfo.Artists = '' then
              FInfo.mangaInfo.Artists := data^.Artists;
            if FInfo.mangaInfo.Genres = '' then
              FInfo.mangaInfo.Genres := data^.Genres;
            if FInfo.mangaInfo.Summary = '' then
              FInfo.mangaInfo.Summary := data^.Summary;
          end;

          if not (Terminated or isExiting) then
            Synchronize(MainThreadSyncInfos);
        end;
      end;
      Result := True;
    except
      on E: Exception do
        MainForm.ExceptionHandler(Self, E);
    end;
  end;

begin
  try
    if not GetMangaInfo then
    begin
      if not (Terminated or isExiting) then
        Synchronize(MainThreadShowCannotGetInfo);
    end
    else
    begin
      if Terminated or isExiting then Exit;
      Synchronize(MainThreadShowInfos);
      FCover.Clear;
      // If there's cover then we will load it to the TPicture component.
      if OptionEnableLoadCover and (Trim(FInfo.mangaInfo.CoverURL) <> '') then
        try
          FInfo.FHTTP.Document.Clear;
          if FInfo.FHTTP.GET(FInfo.mangaInfo.CoverURL) then
            LoadCover;
        except
        end;
      if not (Terminated or isExiting) then
        Synchronize(MainThreadShowCover);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowCannotGetInfo;
begin
  MessageDlg('', RS_DlgCannotGetMangaInfo,
    mtInformation, [mbYes], 0);
  MainForm.rmInformation.Clear;
  MainForm.tmAnimateMangaInfo.Enabled := False;
  MainForm.pbWait.Visible := False;
  MainForm.imCover.Picture.Assign(nil);
end;

procedure TGetMangaInfosThread.LoadCover;
var
  bmp:TMemBitmap;
begin
  FIsHasMangaCover:=false;
  with FInfo.FHTTP do
  if GetImageStreamExt(Document)='webp' then
  begin
    bmp:=nil;
    bmp:=WebPToMemBitmap(Document);
    if Assigned(bmp) then
     try
       FCover.Bitmap:=bmp.Bitmap;
     finally
       FreeAndNil(bmp);
     end
    else
      Exit;
  end
  else
    FCover.LoadFromStream(FInfo.FHTTP.Document);
  FIsHasMangaCover:=True;
end;

procedure TGetMangaInfosThread.MainThreadShowInfos;
var node: PVirtualNode;
begin
  TransferMangaInfo(mangaInfo, FInfo.mangaInfo);
  with MainForm do begin
    if Assigned(FMangaListNode) and dataProcess.WebsiteLoaded(ModuleID) then
      begin
        vtMangaList.BeginUpdate;
        dataProcess.Refresh(dataProcess.Filtered);
        vtMangaList.ReinitNode(FMangaListNode, False);
        if dataProcess.Filtered then begin
          node := vtMangaList.GetNextVisible(FMangaListNode, False);
          while Assigned(node) do begin
            vtMangaList.ReinitNode(node, False);
            node := vtMangaList.GetNextVisible(node, False);
          end;
          vtMangaList.RootNodeCount := dataProcess.RecordCount;
          MainForm.UpdateVtMangaListFilterStatus;
        end;
        vtMangaList.EndUpdate;
      end;
    ShowInformation;
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowCover;
begin
  MainForm.tmAnimateMangaInfo.Enabled := False;
  MainForm.pbWait.Visible := False;
  if FIsHasMangaCover then
  begin
    try
      MainForm.imCover.Picture.Assign(FCover);
    except
      on E: Exception do ;
    end;
    FCover.Clear;
  end;
end;

constructor TGetMangaInfosThread.Create;
begin
  inherited Create(True);
  FInfo := TMangaInformation.Create(Self);
  FCover := MainForm.mangaCover;
  FIsHasMangaCover := False;
  FMangaListNode := nil;
end;

destructor TGetMangaInfosThread.Destroy;
begin
  Modules.DecActiveConnectionCount(FInfo.ModuleIndex);
  GetInfosThread := nil;
  FCover := nil;
  FInfo.Free;
  inherited Destroy;
end;

end.
