{
        File: uDownloadsManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uDownloadsManager;

{$mode objfpc}{$H+}

{$IF FPC_FULLVERSION >= 20701}
  {$DEFINE FPC271}
{$ENDIF}

interface

uses
  LazFileUtils, Classes, SysUtils, ExtCtrls, typinfo, fgl,
  blcksock, MultiLog, uBaseUnit, uPacker, uMisc, DownloadedChaptersDB, FMDOptions,
  httpsendthread, DownloadsDB, BaseThread, dateutils, strutils;

type
  TDownloadStatusType = (
    STATUS_STOP,
    STATUS_WAIT,
    STATUS_PREPARE,
    STATUS_DOWNLOAD,
    STATUS_FINISH,
    STATUS_COMPRESS,
    STATUS_PROBLEM,
    STATUS_FAILED,
    STATUS_NONE        // devault value oncreate, don't use
    );
  TDownloadStatusTypes = set of TDownloadStatusType;

  TDownloadManager = class;
  TTaskContainer = class;
  TTaskThread = class;

  { TDownloadThread }

  TDownloadThread = class(TBaseThread)
  private
  public
    // Get real image url
    function GetLinkPageFromURL(const URL: String): Boolean;

    // Download image
    function DownloadImage: Boolean;

    procedure Execute; override;
    procedure SockOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
  public
    Task: TTaskThread;
    HTTP: THTTPSendThread;
    WorkId: Integer;
    constructor Create(const ATask: TTaskThread);
    destructor Destroy; override;
  end;

  TDownloadThreads = specialize TFPGList<TDownloadThread>;

  { TTaskThread }

  TTaskThread = class(TBaseThread)
  private
    FCS_CURRENTMAXTHREADS,
    FCS_THREADS: TRTLCriticalSection;
    FCheckAndActiveTaskFlag: Boolean;
    FCurrentWorkingDir: String;
    {$IFDEF Windows}
    FCurrentMaxFileNameLength: Integer;
    {$ENDIF}
    FCurrentCustomFileName: String;
    FIsForDelete: Boolean;
    currentMaxThread, currentMaxConnections: Integer;
    procedure SetCurrentWorkingDir(AValue: String);
    procedure SetIsForDelete(AValue: Boolean);
    procedure SyncShowBallonHint;
  protected
    function CanDownload: Boolean;
    function GetPageNumber: Boolean;
    procedure GetCurrentMaxThreads;
    procedure SockOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
  protected
    procedure CheckOut;
    procedure Execute; override;
    function Compress: Boolean;
    procedure SyncStop;
    procedure StatusFailedToCreateDir;
    function FirstFailedChapters: Integer;
    function FailedChaptersExist: Boolean;
    // show notification when download completed
    procedure ShowBalloonHint;
    // general exception info
    function GetExceptionInfo: String;
  public
    HTTP: THTTPSendThread;
    Flag: TFlagType;
    // container (for storing information)
    Container: TTaskContainer;
    // download threads
    Threads: TDownloadThreads;
    constructor Create;
    destructor Destroy; override;
    function GetFileName(const AWorkId: Integer): String;
    function GetWorkId: Integer;
    property CurrentWorkingDir: String read FCurrentWorkingDir write SetCurrentWorkingDir;
    property CurrentMaxFileNameLength: Integer read FCurrentMaxFileNameLength;
    // current custom filename with only %FILENAME% left intact
    property CurrentCustomFileName: String read FCurrentCustomFileName write FCurrentCustomFileName;
    property IsForDelete: Boolean read FIsForDelete write SetIsForDelete;
  end;

  { TTaskContainer }

  TTaskContainer = class
  private
    FStoredOrder: Integer;
    FStatus: TDownloadStatusType;
    FEnabled,
    FDirtyEnabled,
    FDirty: Boolean;
    procedure SetEnabled(AValue: Boolean);
    procedure SetStatus(AValue: TDownloadStatusType);
  public
    DlId: Integer;
    // critical section
    CS_Container: TRTLCriticalSection;
    // read count for transfer rate
    ReadCount: Integer;
    // task thread of this container
    TaskThread: TTaskThread;
    // download manager
    Manager: TDownloadManager;
    DownloadInfo: TDownloadInfo;
    // current link index
    CurrentPageNumber,
    // current chapter index
    CurrentDownloadChapterPtr,
    WorkCounter,
    DownCounter,
    PageNumber: Integer;
    //Status: TDownloadStatusType;
    ThreadState: Boolean;
    ChapterNames,
    ChapterLinks,
    ChaptersStatus,
    PageContainerLinks,
    PageLinks: TStringList;
    FileNames: TStringList;
    // custom filename
    CustomFileName: String;
    constructor Create;
    destructor Destroy; override;
    procedure IncReadCount(const ACount: Integer); inline;
    procedure SaveToDB(const AOrder: Integer = -1);
    procedure ClearDirty(const AOrder: Integer = -1);
  public
    Visible: Boolean;
    property Status: TDownloadStatusType read FStatus write SetStatus;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TTaskContainers = specialize TFPGList<TTaskContainer>;

  { TDownloadManager }

  TDownloadManager = class
  private
    FSortDirection: Boolean;
    FSortColumn: Integer;
    FDownloadsDB: TDownloadsDB;
    procedure AddItemsActiveTask(const Item: TTaskContainer);
    procedure RemoveItemsActiveTask(const Item: TTaskContainer);
    function GetTask(const TaskId: Integer): TTaskContainer;
  protected
    function GetTaskCount: Integer; inline;
    function GetTransferRate: Integer;
    procedure ChangeStatusCount(const OldStatus, NewStatus: TDownloadStatusType);
    procedure DecStatusCount(const Status: TDownloadStatusType);
    procedure IncStatusCount(const Status: TDownloadStatusType);
    // it has no data validation, only StartTask after all data check passed
    procedure StartTask(const taskID: Integer);
  public
    CS_Task,
    CS_ItemsActiveTask: TRTLCriticalSection;
    Items,
    ItemsActiveTask: TTaskContainers;
    isRunningBackup, isFinishTaskAccessed, isRunningBackupDownloadedChaptersList,
    isReadyForExit: Boolean;

    // status count
    CS_StatusCount: TRTLCriticalSection;
    StatusCount: array [TDownloadStatusType] of Integer;
    // disabled count
    DisabledCount,
    CompressType,
    RetryConnect: Integer;

    //downloaded chapter list database
    DownloadedChapters: TDownloadedChaptersDB;

    //exit counter
    ExitWaitOK: Boolean;

    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetTaskCount;

    procedure Restore;
    procedure Backup;

    // These methods relate to highlight downloaded chapters.
    procedure GetDownloadedChaptersState(const AModuleID, ALink: String;
      var AChapters: array of TChapterStateItem);

    // Add new task to the list.
    function AddTask: Integer;
    // Check and active previous work-in-progress tasks.
    procedure CheckAndActiveTaskAtStartup;
    // Check and active waiting tasks.
    procedure CheckAndActiveTask(const isCheckForFMDDo: Boolean = False);
    // Active a stopped task.
    procedure SetTaskActive(const taskID: Integer);
    // Stop a download/wait task.
    procedure StopTask(const taskID: Integer; const isCheckForActive: Boolean =
      True; isWaitFor: Boolean = False);
    // Start all task
    procedure StartAllTasks;
    // Stop all download/wait tasks.
    procedure StopAllTasks;
    // Stop all download task inside a task before terminate the program.
    procedure StopAllDownloadTasksForExit;
    // Free then delete task without any check, use with caution
    procedure FreeAndDelete(const TaskId: Integer);
    // Remove a task from list.
    procedure RemoveTask(const TaskID: Integer);
    // Remove all finished tasks.
    procedure RemoveAllFinishedTasks;
    // check status of task
    function TaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;
    // enable task
    procedure EnableTask(const TaskId: Integer);
    procedure DisableTask(const TaskId: Integer);

    // Sort
    procedure Sort(const AColumn: Integer);

    property SortDirection: Boolean read FSortDirection write FSortDirection;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property TransferRate: Integer read GetTransferRate;
    property Task[const TaskId: Integer]: TTaskContainer read GetTask; default;
  end;

resourcestring
  RS_FailedToCreateDir = 'Failed to create directory!';
  RS_FailedTryResumeTask = 'Failed, try resuming this task!';
  RS_Preparing = 'Preparing';
  RS_Downloading = 'Downloading';
  RS_Stopped = 'Stopped';
  RS_Finish = 'Completed';
  RS_Waiting = 'Waiting...';
  RS_Compressing = 'Compressing...';
  RS_Failed = 'Failed';
  RS_Disabled = 'Disabled';

implementation

uses
  frmMain, WebsiteModules, SimpleException;

function IntToStr(Value: Cardinal): String;
begin
  Result := SysUtils.IntToStr(QWord(Value));
end;

{ TDownloadThread }

constructor TDownloadThread.Create(const ATask: TTaskThread);
begin
  inherited Create(False);
  Task := ATask;
  HTTP := THTTPSendThread.Create(Self);
  HTTP.Sock.OnStatus := @SockOnStatus;
  TModuleContainer(Task.Container.DownloadInfo.Module).PrepareHTTP(HTTP);
end;

destructor TDownloadThread.Destroy;
begin
  EnterCriticalsection(Task.FCS_THREADS);
  try
    Task.Threads.Remove(Self);
  finally
    LeaveCriticalsection(Task.FCS_THREADS);
  end;
  HTTP.Free;
  inherited Destroy;
end;

procedure TDownloadThread.Execute;
var
  Reslt: Boolean = False;
begin
  WorkId := Task.GetWorkId;
  while WorkId <> -1 do
  begin
    TModuleContainer(Task.Container.DownloadInfo.Module).IncActiveConnectionCount;
    try
      if Task.Flag = CS_GETPAGELINK then
      begin
        Reslt := GetLinkPageFromURL(
          Task.Container.ChapterLinks.Strings[
          Task.Container.CurrentDownloadChapterPtr]);
      end
      // Download images.
      else if Task.Flag = CS_DOWNLOAD then
        Reslt := DownloadImage;

      if not Terminated and Reslt then
      begin
        EnterCriticalSection(Task.Container.CS_Container);
        try
          InterLockedIncrement(Task.Container.DownCounter);
          Task.Container.DownloadInfo.Progress :=
            Format('%d/%d', [Task.Container.DownCounter, Task.Container.PageNumber]);
        finally
          LeaveCriticalSection(Task.Container.CS_Container);
        end;
      end;
    finally
      TModuleContainer(Task.Container.DownloadInfo.Module).DecActiveConnectionCount;
    end;
    WorkId := Task.GetWorkId;
  end;
end;

procedure TDownloadThread.SockOnStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
begin
  if Reason = HR_ReadCount then
    Task.Container.IncReadCount(StrToIntDef(Value, 0));
end;

function TDownloadThread.GetLinkPageFromURL(const URL: String): Boolean;
begin
  Result := False;
  if Task.Container.PageLinks[WorkId] <> 'W' then Exit;
  if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnGetImageURL) then
    Result := TModuleContainer(Task.Container.DownloadInfo.Module).OnGetImageURL(Self, URL, TModuleContainer(Task.Container.DownloadInfo.Module));
end;

// ----- TTaskThread -----

constructor TTaskThread.Create;
begin
  inherited Create(True);
  InitCriticalSection(FCS_CURRENTMAXTHREADS);
  InitCriticalSection(FCS_THREADS);
  Threads := TDownloadThreads.Create;
  FCheckAndActiveTaskFlag := True;
  FIsForDelete := False;
  FCurrentWorkingDir := '';
  FCurrentCustomFileName := '';
  {$IFDEF WINDOWS}
  FCurrentMaxFileNameLength := 0;
  {$ENDIF}
end;

destructor TTaskThread.Destroy;
var
  i: Integer;
begin
  Container.DownloadInfo.DateLastDownloaded := Now;
  EnterCriticalsection(FCS_THREADS);
  try
    if Threads.Count > 0 then
      for i := 0 to Threads.Count - 1 do
        Threads[i].Terminate;
  finally
    LeaveCriticalsection(FCS_THREADS);
  end;
  while Threads.Count > 0 do
    Sleep(32);

  TModuleContainer(Container.DownloadInfo.Module).DecActiveTaskCount;
  with Container do
  begin
    ThreadState := False;
    Manager.RemoveItemsActiveTask(Container);
    TaskThread := nil;
    if not (IsForDelete or Manager.isReadyForExit) then
    begin
      Container.ReadCount := 0;
      DownloadInfo.TransferRate := '';
      if Status <> STATUS_STOP then
      begin
        if (WorkCounter >= PageLinks.Count) and
           (CurrentDownloadChapterPtr >= ChapterLinks.Count) and
           (not FailedChaptersExist) then
        begin
          Status := STATUS_FINISH;
          DownloadInfo.Status := Format('[%d/%d] %s',[Container.ChapterLinks.Count,Container.ChapterLinks.Count,RS_Finish]);
          DownloadInfo.Progress := '';
        end
        else
        if not (Status in [STATUS_FAILED, STATUS_PROBLEM]) then
        begin
          Status := STATUS_STOP;
          DownloadInfo.Status :=
            Format('[%d/%d] %s', [CurrentDownloadChapterPtr + 1,
            ChapterLinks.Count, RS_Stopped]);
          FCheckAndActiveTaskFlag := False;
        end;
        if not isExiting then
          Synchronize(@SyncStop);
      end;
    end;
  end;
  Threads.Free;
  if HTTP<>nil then
    HTTP.Free;
  DoneCriticalsection(FCS_THREADS);
  DoneCriticalSection(FCS_CURRENTMAXTHREADS);
  inherited Destroy;
end;

function TTaskThread.GetFileName(const AWorkId: Integer): String;
{$IFDEF WINDOWS}
var
  s: UnicodeString;
{$ENDIF}
begin
  Result := '';
  if (Container.FileNames.Count = Container.PageLinks.Count) and
    (AWorkId < Container.FileNames.Count) then
    Result := Container.FileNames[AWorkId];
  if Result = '' then
    Result := Format('%.3d', [AWorkId + 1]);
  Result := StringReplace(CurrentCustomFileName, CR_FILENAME, Result, [rfReplaceAll]);
  {$IFDEF WINDOWS}
  s := UTF8Decode(Result);
  if Length(s) > FCurrentMaxFileNameLength then
  begin
    Delete(s, 1, Length(s) - FCurrentMaxFileNameLength);
    Result := UTF8Encode(s);
  end;
  {$ENDIF}
end;

function TTaskThread.GetWorkId: Integer;
begin
  Result := -1;
  if Terminated then Exit;

  GetCurrentMaxThreads;

  if (Threads.Count > currentMaxThread) then
    Exit;

  if (Flag = CS_GETPAGELINK) then
  begin
    if (Container.WorkCounter >= Container.PageNumber) then
      Exit;
  end
  else
  if (Flag = CS_DOWNLOAD) then
  begin
    if (Container.WorkCounter >= Container.PageLinks.Count) then
      Exit;
  end;

  if CanDownload then
  try
    EnterCriticalsection(FCS_THREADS);

    Result := Container.WorkCounter;
    InterLockedIncrement(Container.WorkCounter);

    if Flag = CS_GETPAGELINK then
      InterLockedIncrement(Container.CurrentPageNumber);

    InterLockedIncrement(Container.DownloadInfo.iProgress);
  finally
    LeaveCriticalSection(FCS_THREADS);
  end;
end;

function TTaskThread.Compress: Boolean;
var
  uPacker: TPacker;
  i: Integer;
  s: String;
begin
  Result := True;
  if (Container.Manager.CompressType >= 1) then
  begin
    Container.DownloadInfo.Status :=
      Format('[%d/%d] %s', [Container.CurrentDownloadChapterPtr + 1,
      Container.ChapterLinks.Count, RS_Compressing]);
    uPacker := TPacker.Create;
    try
      case Container.Manager.CompressType of
        1: uPacker.Format := pfZIP;
        2: uPacker.Format := pfCBZ;
        3: uPacker.Format := pfPDF;
        4: uPacker.Format := pfEPUB;
      end;
      uPacker.CompressionQuality := OptionPDFQuality;
      uPacker.Path := CurrentWorkingDir;
      uPacker.FileName := RemovePathDelim(CorrectPathSys(CorrectPathSys(Container.DownloadInfo.SaveTo) +
        Container.ChapterNames[Container.CurrentDownloadChapterPtr]));
      for i := 0 to Container.PageLinks.Count - 1 do
      begin
        s := FindImageFile(uPacker.Path + GetFileName(i));
        if s <> '' then
          uPacker.FileList.Add(s);
      end;
      Result := uPacker.Execute;
      if not Result then
        Logger.SendWarning(Self.ClassName+', failed to compress. '+uPacker.SavedFileName);
    except
      on E: Exception do
      begin
        E.Message := E.Message + LineEnding + '  In TTaskThread.Compress' + LineEnding + GetExceptionInfo;
        MainForm.ExceptionHandler(Self, E);
      end;
    end;
    uPacker.Free;
  end;
end;

procedure TTaskThread.SyncStop;
begin
  Container.Manager.CheckAndActiveTask(FCheckAndActiveTaskFlag);
end;

procedure TTaskThread.StatusFailedToCreateDir;
begin
  Logger.SendError(Format('Failed to create dir(%d) = %s', [Length(CurrentWorkingDir), CurrentWorkingDir]));
  Container.Status := STATUS_FAILED;
  Container.DownloadInfo.Status := Format('[%d/%d] %s (%d) %s', [
    Container.CurrentDownloadChapterPtr,
    Container.ChapterLinks.Count,
    RS_FailedToCreateDir, Length(CurrentWorkingDir), LineEnding + CurrentWorkingDir]);
end;

function TTaskThread.FirstFailedChapters: Integer;
var
  i: Integer;
begin
  for i := 0 to Container.ChaptersStatus.Count - 1 do
    if Container.ChaptersStatus[i] = 'F' then Exit(i);
  Result := -1;
end;

function TTaskThread.FailedChaptersExist: Boolean;
begin
  Result := FirstFailedChapters <> -1;
end;

procedure TTaskThread.ShowBalloonHint;
begin
  if OptionShowBalloonHint then
    Synchronize(@SyncShowBallonHint);
end;

function TTaskThread.GetExceptionInfo: String;
begin
  Result :=
    '  Flag        : ' + GetEnumName(TypeInfo(TFlagType), Integer(Flag)) + LineEnding +
    '  Website     : ' + Container.DownloadInfo.ModuleID + LineEnding +
    '  Title       : ' + Container.DownloadInfo.title + LineEnding +
    '  Chapterlink : ' + Container.ChapterLinks[Container.CurrentDownloadChapterPtr] + LineEnding +
    '  Chaptername : ' + Container.ChapterNames[Container.CurrentDownloadChapterPtr] + LineEnding;
end;

function TDownloadThread.DownloadImage: Boolean;
var
  workFilename,
  workURL,
  savedFilename: String;
begin
  Result := True;

  // check download path
  if not ForceDirectoriesUTF8(Task.CurrentWorkingDir) then
  begin
    Task.StatusFailedToCreateDir;
    Result := False;
    Exit;
  end;

  // check pagelinks url
  workURL := Task.Container.PageLinks[WorkId];
  if (workURL = '') or
     (workURL = 'W') or
     (workURL = 'D') then
    Exit;

  HTTP.Clear;

  // prepare filename
  workFilename := Task.GetFileName(WorkId);

  // download image
  savedFilename := '';

  if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnDownloadImage) and
    (Task.Container.PageNumber = Task.Container.PageContainerLinks.Count) and
    (WorkId < Task.Container.PageContainerLinks.Count) then
      workURL := Task.Container.PageContainerLinks[WorkId];

  // OnBeforeDownloadImage
  if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnBeforeDownloadImage) then
    Result := TModuleContainer(Task.Container.DownloadInfo.Module).OnBeforeDownloadImage(Self, workURL, TModuleContainer(Task.Container.DownloadInfo.Module));

  if Result then
  begin
    // OnDownloadImage
    if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnDownloadImage) then
      Result := TModuleContainer(Task.Container.DownloadInfo.Module).OnDownloadImage(Self, workURL, TModuleContainer(Task.Container.DownloadInfo.Module))
    else
      Result := HTTP.GET(workURL);
  end;

  if Result then
  begin
    savedFilename := FindImageFile(Task.CurrentWorkingDir + workFilename);
    Result := savedFilename <> '';
    if not Result then
    begin
      if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnSaveImage) then
        savedFilename := TModuleContainer(Task.Container.DownloadInfo.Module).OnSaveImage(Self, Task.CurrentWorkingDir, workFilename, TModuleContainer(Task.Container.DownloadInfo.Module))
      else
        savedFilename := SaveImageStreamToFile(HTTP, Task.CurrentWorkingDir, workFilename);
      Result := savedFilename <> '';
    end;
  end;

  if Result then
    Result := FileExistsUTF8(savedFilename);

  if Terminated then Exit(False);
  if Result then
  begin
    Task.Container.PageLinks[WorkId] := 'D';
    // OnAfterImageSaved
    if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnAfterImageSaved) then
      Result := TModuleContainer(Task.Container.DownloadInfo.Module).OnAfterImageSaved(Self, savedFilename, TModuleContainer(Task.Container.DownloadInfo.Module));
  end;
end;

procedure TTaskThread.SetCurrentWorkingDir(AValue: String);
{$IFDEF WINDOWS}
var
  s: UnicodeString;
{$ENDIF}
begin
  if FCurrentWorkingDir = AValue then Exit;
  FCurrentWorkingDir := CorrectPathSys(AValue);
  {$IFDEF Windows}
  s := UTF8Decode(FCurrentWorkingDir);
  FCurrentMaxFileNameLength := FMDMaxImageFilePath - Length(s);
  {$ENDIF}
end;

procedure TTaskThread.SetIsForDelete(AValue: Boolean);
begin
  if FIsForDelete = AValue then Exit;
  FIsForDelete := AValue;
end;

procedure TTaskThread.SyncShowBallonHint;
begin
  with MainForm.TrayIcon, Container.DownloadInfo do
  begin
    if Container.Status = STATUS_FAILED then
    begin
      BalloonFlags := bfError;
      BalloonHint := QuotedStrd(Title);
      if Status = '' then
        BalloonHint := BalloonHint + ' - ' + RS_Failed
      else
        BalloonHint := BalloonHint + LineEnding + Status;
    end
    else
    if Container.Status = STATUS_FINISH then
    begin
      BalloonFlags := bfInfo;
      BalloonHint :=
        '"' + Container.DownloadInfo.title + '" - ' + RS_Finish;
    end;
    ShowBalloonHint;
  end;
end;

function TTaskThread.CanDownload: Boolean;
begin
  if Container.PageLinks.Count > 0 then
    begin
      if ((Flag = CS_GETPAGELINK) and (Container.PageLinks[Container.WorkCounter] <> 'W')) or
        ((Flag = CS_DOWNLOAD) and (Container.PageLinks[Container.WorkCounter] = 'D')) then
      begin
        InterLockedIncrement(Container.WorkCounter);
        InterLockedIncrement(Container.DownCounter);
        Container.DownloadInfo.Progress :=
          Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
        if Flag = CS_GETPAGELINK then
          InterLockedIncrement(Container.CurrentPageNumber);
        Exit(False);
      end;
    end;
  Result := True;
end;

function TTaskThread.GetPageNumber: Boolean;
begin
  // Get total number of images/pages per chapter
  Container.PageNumber := 0;

  if Assigned(TModuleContainer(Container.DownloadInfo.Module).OnGetPageNumber) then
  begin
    if HTTP = nil then
    begin
      HTTP := THTTPSendThread.Create(Self);
      HTTP.Sock.OnStatus := @SockOnStatus;
      TModuleContainer(Container.DownloadInfo.Module).PrepareHTTP(HTTP);
    end;
    Result := TModuleContainer(Container.DownloadInfo.Module).OnGetPageNumber(
      Container,
      Container.ChapterLinks.Strings[Container.CurrentDownloadChapterPtr],
      TModuleContainer(Container.DownloadInfo.Module));
  end;
  if Container.PageLinks.Count > 0 then
    TrimStrings(Container.PageLinks);

  // Prepare 'space' for storing image url.
  if (not Terminated) and
    (Container.PageNumber > 0) then
  begin
    while Container.PageLinks.Count < Container.PageNumber do
      Container.PageLinks.Add('W');
  end
  else
    Result := False;
end;

procedure TTaskThread.GetCurrentMaxThreads;
begin
  if TryEnterCriticalSection(FCS_CURRENTMAXTHREADS)<>0 then
  try
    if TModuleContainer(Container.DownloadInfo.Module).MaxThreadPerTaskLimit > 0 then
      currentMaxThread := TModuleContainer(Container.DownloadInfo.Module).MaxThreadPerTaskLimit
    else
      currentMaxThread := OptionMaxThreads;
    if currentMaxThread > OptionMaxThreads then
      currentMaxThread := OptionMaxThreads;

    currentMaxConnections := TModuleContainer(Container.DownloadInfo.Module).MaxConnectionLimit;
    if currentMaxConnections <= 0 then
      currentMaxConnections := currentMaxThread;
  finally
    LeaveCriticalSection(FCS_CURRENTMAXTHREADS);
  end;

  while (not Terminated) and (not TModuleContainer(Container.DownloadInfo.Module).CanCreateConnection) do
    Sleep(SOCKHEARTBEATRATE)
end;

procedure TTaskThread.SockOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
begin
  if Reason = HR_ReadCount then
    Container.IncReadCount(StrToIntDef(Value, 0));
end;

procedure TTaskThread.CheckOut;
begin
  GetCurrentMaxThreads;

  while (not Terminated) and (Threads.Count >= currentMaxThread) do
    Sleep(SOCKHEARTBEATRATE);

  EnterCriticalsection(FCS_THREADS);
  try
    while (Threads.Count < currentMaxThread) and TModuleContainer(Container.DownloadInfo.Module).CanCreateConnection do
      Threads.Add(TDownloadThread.Create(Self));
  finally
    LeaveCriticalsection(FCS_THREADS);
  end;
end;

procedure TTaskThread.Execute;
var
  DynamicPageLink: Boolean;
  FailedRetryCount: Integer = 0;

  function CheckForPrepare: Boolean;
  var
    i: Integer;
  begin
    if Container.PageLinks.Count = 0 then
      Exit(True);
    Result := False;
    if Container.PageLinks.Count > 0 then
      for i := 0 to Container.PageLinks.Count - 1 do
        if (Container.PageLinks[i] = 'W') or
          (Container.PageLinks[i] = '') then
          Exit(True);
  end;

  function CheckForExists: Integer;
  var
    i: Integer;
  begin
    Result := 0;
    if Container.PageLinks.Count > 0 then
      begin
        for i := 0 to Container.PageLinks.Count - 1 do
        begin
          if ImageFileExists(CurrentWorkingDir + GetFileName(i)) then
          begin
            Container.PageLinks[i] := 'D';
            Inc(Result);
          end
          else
          if Container.PageLinks[i] = 'D' then
          begin
            if DynamicPageLink then
              Container.PageLinks[i] := 'G'
            else
              Container.PageLinks[i] := 'W';
          end;
        end;
      end;
  end;

  function CheckForFinish: Boolean;
  var
    c: Integer;
  begin
    Result := False;
    if Container.PageLinks.Count = 0 then
      Exit;

    c := Container.PageLinks.Count - CheckForExists;

    Result := c = 0;
    if Result = False then
      Logger.SendWarning(Format('%s, checkforfinish failed=%d/%d [%s] "%s" > "%s"',
        [Self.ClassName,
        c,
        Container.PageLinks.Count,
        Container.DownloadInfo.Website,
        Container.DownloadInfo.Title,
        Container.ChapterLinks[Container.CurrentDownloadChapterPtr]]));
  end;

  procedure WaitForThreads;
  begin
    while (not Terminated) and (Threads.Count > 0) do
      Sleep(SOCKHEARTBEATRATE);
  end;

var
  i: Integer;
begin
  Container.ThreadState := True;
  Container.DownloadInfo.DateLastDownloaded := Now;
  Container.DownloadInfo.TransferRate := FormatByteSize(Container.ReadCount, true);
  try
    DynamicPageLink := TModuleContainer(Container.DownloadInfo.Module).DynamicPageLink;

    if Trim(Container.CustomFileName) = '' then
      Container.CustomFileName := OptionFilenameCustomRename;
    if Trim(Container.CustomFileName) = '' then
      Container.CustomFileName := DEFAULT_FILENAME_CUSTOMRENAME;

    while container.ChaptersStatus.Count < Container.CurrentDownloadChapterPtr - 1 do
      Container.ChaptersStatus.Add('D');
    while Container.ChaptersStatus.Count < Container.ChapterLinks.Count do
      Container.ChaptersStatus.Add('P');

    if OptionAlwaysStartTaskFromFailedChapters and (Container.CurrentDownloadChapterPtr <> 0) then
      Container.CurrentDownloadChapterPtr := 0;

    while Container.CurrentDownloadChapterPtr < Container.ChapterLinks.Count do
    begin
      while Container.ChaptersStatus[Container.CurrentDownloadChapterPtr] = 'D' do
        Inc(Container.CurrentDownloadChapterPtr);

      WaitForThreads;
      if Terminated then Exit;

      //check path
      if OptionGenerateChapterFolder then
        CurrentWorkingDir := CorrectPathSys(Container.DownloadInfo.SaveTo) +
          Container.ChapterNames[Container.CurrentDownloadChapterPtr]
      else
        CurrentWorkingDir := Container.DownloadInfo.SaveTo;
      if not ForceDirectoriesUTF8(CurrentWorkingDir) then
      begin
        StatusFailedToCreateDir;
        ShowBalloonHint;
        Exit;
      end;

      if Assigned(TModuleContainer(Container.DownloadInfo.Module).OnTaskStart) then
        TModuleContainer(Container.DownloadInfo.Module).OnTaskStart(Container, TModuleContainer(Container.DownloadInfo.Module));

      // set current working custom filename
      CurrentCustomFileName :=  CustomRename(Container.CustomFileName,
        Container.DownloadInfo.Website,
        Container.DownloadInfo.Title,
        '',
        '',
        Container.ChapterNames[Container.CurrentDownloadChapterPtr],
        '',
        OptionChangeUnicodeCharacter,
        OptionChangeUnicodeCharacterStr,
        CR_FILENAME);

      // Get total number of images/pages per chapter
      if Container.PageLinks.Count = 0 then
      begin
        Container.PageNumber := 0;
        Flag := CS_GETPAGENUMBER;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.Progress := '0/0';
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Preparing,
          Container.ChapterNames[Container.CurrentDownloadChapterPtr]]);
        Container.Status := STATUS_PREPARE;

        GetPageNumber;

        if Terminated then begin
          Container.PageLinks.Clear;
          Container.PageNumber := 0;
          Exit;
        end;
      end;

      // Check files, if exist set mark 'D', otherwise 'W' or 'G' for dynamic image url
      CheckForExists;

      // Get the real image urls
      if Container.PageLinks.Count = 0 then
        Container.PageLinks.Add('W');
      Container.PageNumber := Container.PageLinks.Count;
      if (not DynamicPageLink) and CheckForPrepare then
      begin
        Flag := CS_GETPAGELINK;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.iProgress := 0;
        Container.DownloadInfo.Progress :=
          Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Preparing,
          Container.ChapterNames[Container.CurrentDownloadChapterPtr]]);
        Container.Status := STATUS_PREPARE;
        while Container.WorkCounter < Container.PageNumber do
        begin
          if Terminated then Exit;
            Checkout;
        end;
        WaitForThreads;
        if Terminated then Exit;

        //check if pagelink is found. Else set again to 'W'(some script return '')
        if Container.PageLinks.Count > 0 then
        begin
          for i := 0 to Container.PageLinks.Count - 1 do
          begin
            if Trim(Container.PageLinks[i]) = '' then
              Container.PageLinks[i] := 'W';
          end;
        end;
      end;
      if Terminated then Exit;

      // download the images
      // If Container doesn't have any image, we will skip the loop. Otherwise
      // download them
      Container.PageNumber := Container.PageLinks.Count;
      if Container.PageLinks.Count > 0 then
      begin
        Flag := CS_DOWNLOAD;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.iProgress := 0;
        Container.DownloadInfo.Progress :=
          Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
        Container.Status := STATUS_DOWNLOAD;
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Downloading,
          Container.ChapterNames[Container.CurrentDownloadChapterPtr]]);
        while Container.WorkCounter < Container.PageLinks.Count do
        begin
          if Terminated then Exit;
          if CanDownload then
            Checkout;
        end;
        WaitForThreads;
        if Terminated then Exit;

        //check if all page is downloaded
        if CheckForFinish then
        begin
          Container.Status := STATUS_COMPRESS;
          Container.DownloadInfo.Progress := '';
          if not Compress then
            Container.Status := STATUS_FAILED;
        end
        else
        begin
          Container.Status := STATUS_FAILED;
          Logger.SendWarningStrings(Format('%s, download failed. "%s" "%s" "%s"',
            [Self.ClassName,
             Container.DownloadInfo.Title,
             Container.ChapterNames[Container.CurrentDownloadChapterPtr],
             Container.ChapterLinks[Container.CurrentDownloadChapterPtr]
            ]), Container.PageLinks.Text);
        end;
      end
      else
      begin
        Container.Status := STATUS_FAILED;
        Logger.SendWarning(Format('%s, pagelinks is empty. "%s" "%s" "%s"',
          [Self.ClassName,
           Container.DownloadInfo.Title,
           Container.ChapterNames[Container.CurrentDownloadChapterPtr],
           Container.ChapterLinks[Container.CurrentDownloadChapterPtr]
          ]));
      end;

      if Container.Status = STATUS_FAILED  then
        Container.ChaptersStatus[Container.CurrentDownloadChapterPtr] := 'F'
      else
        Container.ChaptersStatus[Container.CurrentDownloadChapterPtr] := 'D';

      Container.CurrentPageNumber := 0;
      Container.PageLinks.Clear;
      Container.PageContainerLinks.Clear;
      InterLockedIncrement(Container.CurrentDownloadChapterPtr);

      if (Container.CurrentDownloadChapterPtr = Container.ChapterLinks.Count) and
         (FailedRetryCount < OptionRetryFailedTask) then
      begin
        Container.CurrentDownloadChapterPtr := FirstFailedChapters;
        if container.CurrentDownloadChapterPtr <> -1 then
          Inc(FailedRetryCount)
        else
          Container.CurrentDownloadChapterPtr := Container.ChapterLinks.Count;
      end;
    end;

    if FailedChaptersExist then
    begin
      Container.Status := STATUS_FAILED;
      Container.DownloadInfo.Status := Format('[%d/%d] %s', [
        Container.CurrentDownloadChapterPtr,
        Container.ChapterLinks.Count,
        RS_FailedTryResumeTask]);
      Container.DownloadInfo.Progress := '';
      Container.CurrentDownloadChapterPtr := 0;
    end
    else
    begin
      Container.Status := STATUS_FINISH;
      Container.DownloadInfo.Status := Format('[%d/%d] %s',[Container.ChapterLinks.Count,Container.ChapterLinks.Count,RS_Finish]);
      Container.DownloadInfo.Progress := '';
    end;
    ShowBalloonHint;
  except
    on E: Exception do
    begin
      E.Message := E.Message + LineEnding + '  In TTaskThread.Execute' + LineEnding + GetExceptionInfo;
      MainForm.ExceptionHandler(Self, E);
    end;
  end;
end;

{ TTaskContainer }

procedure TTaskContainer.SetStatus(AValue: TDownloadStatusType);
begin
  if FStatus = AValue then Exit;
  FDirty := True;
  if Assigned(Manager) then
    Manager.ChangeStatusCount(FStatus, AValue);
  FStatus := AValue;
end;

procedure TTaskContainer.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  FDirtyEnabled := True;
  if Assigned(Manager) then
  begin
    if Enabled then
      Dec(Manager.DisabledCount)
    else
      Inc(Manager.DisabledCount);
  end;
end;

constructor TTaskContainer.Create;
begin
  inherited Create;
  FStoredOrder := -1;
  FDirty := False;
  FDirtyEnabled := False;
  DlId := -1;
  InitCriticalSection(CS_Container);
  ThreadState := False;
  ChapterLinks := TStringList.Create;
  ChapterNames := TStringList.Create;
  ChaptersStatus := TStringList.Create;
  PageLinks := TStringList.Create;
  PageContainerLinks := TStringList.Create;
  FileNames := TStringList.Create;
  ReadCount := 0;
  WorkCounter := 0;
  CurrentPageNumber := 0;
  CurrentDownloadChapterPtr := 0;
  CustomFileName := OptionFilenameCustomRename;
  FStatus := STATUS_NONE;
  FEnabled := True;
  Visible := True;
end;

destructor TTaskContainer.Destroy;
begin
  FileNames.Free;
  PageContainerLinks.Free;
  PageLinks.Free;
  ChapterNames.Free;
  ChapterLinks.Free;
  ChaptersStatus.Free;
  DoneCriticalsection(CS_Container);
  if Assigned(Manager) then
    Manager.DecStatusCount(Status);
  inherited Destroy;
end;

procedure TTaskContainer.IncReadCount(const ACount: Integer);
begin
  InterlockedExchangeAdd(ReadCount, ACount);
end;

procedure TTaskContainer.SaveToDB(const AOrder: Integer);
var
  i: Integer;
begin
  if AOrder = -1 then
    i := Manager.Items.IndexOf(Self)
  else
    i := AOrder;
  Manager.FDownloadsDB.Add(DlId,
    FEnabled,
    i,
    Integer(Status),
    CurrentDownloadChapterPtr,
    PageNumber,
    CurrentPageNumber,
    DownloadInfo.ModuleID,
    DownloadInfo.Link,
    DownloadInfo.Title,
    DownloadInfo.Status,
    DownloadInfo.Progress,
    DownloadInfo.SaveTo,
    DownloadInfo.DateAdded,
    ChapterLinks.Text,
    ChapterNames.Text,
    PageLinks.Text,
    PageContainerLinks.Text,
    FileNames.Text,
    CustomFileName,
    ChaptersStatus.Text);
  ClearDirty(i);
end;

procedure TTaskContainer.ClearDirty(const AOrder: Integer);
begin
  FDirty := False;
  FDirtyEnabled := False;
  if AOrder<>-1 then FStoredOrder := AOrder;
end;

{ TDownloadManager }

procedure TDownloadManager.AddItemsActiveTask(const Item: TTaskContainer);
begin
  EnterCriticalsection(CS_ItemsActiveTask);
  try
    ItemsActiveTask.Add(Item);
  finally
    LeaveCriticalsection(CS_ItemsActiveTask);
  end;
end;

procedure TDownloadManager.RemoveItemsActiveTask(const Item: TTaskContainer);
begin
  EnterCriticalsection(CS_ItemsActiveTask);
  try
    ItemsActiveTask.Remove(Item);
  finally
    LeaveCriticalsection(CS_ItemsActiveTask);
  end;
end;

function TDownloadManager.GetTask(const TaskId: Integer): TTaskContainer;
begin
  Result := Items[TaskId];
end;

function TDownloadManager.GetTaskCount: Integer;
begin
  Result := Items.Count;
end;

function TDownloadManager.GetTransferRate: Integer;
var
  i: Integer;
begin
  Result := 0;
  if ItemsActiveTask.Count = 0 then Exit;
  EnterCriticalSection(CS_ItemsActiveTask);
  try
    for i := 0 to ItemsActiveTask.Count - 1 do
      with ItemsActiveTask[i] do
      begin
        EnterCriticalSection(CS_Container);
        try
          DownloadInfo.TransferRate := FormatByteSize(ReadCount, True);
          Inc(Result, ReadCount);
          ReadCount := 0;
        finally
          LeaveCriticalSection(CS_Container);
        end;
      end;
  finally
    LeaveCriticalSection(CS_ItemsActiveTask);
  end;
end;

procedure TDownloadManager.ChangeStatusCount(const OldStatus,
  NewStatus: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    if OldStatus = NewStatus then Exit;
    if StatusCount[OldStatus] > 0 then
      Dec(StatusCount[OldStatus]);
    Inc(StatusCount[NewStatus]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

procedure TDownloadManager.DecStatusCount(const Status: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    if StatusCount[Status] > 0 then
      Dec(StatusCount[Status]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

procedure TDownloadManager.IncStatusCount(const Status: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    Inc(StatusCount[Status]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

constructor TDownloadManager.Create;
var
  ds: TDownloadStatusType;
begin
  inherited Create;
  InitCriticalSection(CS_Task);
  InitCriticalSection(CS_ItemsActiveTask);
  ForceDirectoriesUTF8(USERDATA_FOLDER);
  DownloadedChapters := TDownloadedChaptersDB.Create;
  DownloadedChapters.Filename := DOWNLOADEDCHAPTERSDB_FILE;
  DownloadedChapters.OnError := @MainForm.ExceptionHandler;
  DownloadedChapters.Open;

  Items := TTaskContainers.Create;
  ItemsActiveTask := TTaskContainers.Create;
  isFinishTaskAccessed := False;
  isRunningBackup := False;
  isRunningBackupDownloadedChaptersList := False;
  isReadyForExit := False;

  InitCriticalSection(CS_StatusCount);
  for ds := Low(StatusCount) to High(StatusCount) do
    StatusCount[ds] := 0;
  DisabledCount := 0;
  FDownloadsDB := TDownloadsDB.Create(DOWNLOADSDB_FILE);
  FDownloadsDB.Open(False, False);
end;

destructor TDownloadManager.Destroy;
var
  i: Integer;
begin
  for i:=0 to Items.Count-1 do
    Items[i].Free;
  Items.Free;
  ItemsActiveTask.Free;
  DownloadedChapters.Free;
  FDownloadsDB.Free;
  DoneCriticalsection(CS_ItemsActiveTask);
  DoneCriticalsection(CS_Task);
  DoneCriticalsection(CS_StatusCount);
  inherited Destroy;
end;

procedure TDownloadManager.Restore;
var
  t: TTaskContainer;
  i: LongInt;
begin
  if not FDownloadsDB.Connection.Connected then Exit;
  if FDownloadsDB.OpenTable(False) then
  try
    if FDownloadsDB.RecordCount = 0 then Exit;
    EnterCriticalsection(CS_Task);
      try
        FDownloadsDB.Table.Last; //load all to memory
        FDownloadsDB.Table.First;
        while not FDownloadsDB.Table.EOF do
        begin
          t:=TTaskContainer.Create;
          i:=Items.Add(t);
          with t, FDownloadsDB.Table do
          begin
            Manager := Self;
            DlId                            := Fields[f_id].AsInteger;
            Enabled                         := Fields[f_enabled].AsBoolean;
            Status                          := TDownloadStatusType(Fields[f_taskstatus].AsInteger);
            CurrentDownloadChapterPtr       := Fields[f_chapterptr].AsInteger;
            PageNumber                      := Fields[f_numberofpages].AsInteger;
            CurrentPageNumber               := Fields[f_currentpage].AsInteger;
            DownloadInfo.ModuleID           := Fields[f_moduleid].AsString;
            DownloadInfo.Link               := Fields[f_link].AsString;
            DownloadInfo.Title              := Fields[f_title].AsString;
            DownloadInfo.Status             := Fields[f_status].AsString;
            DownloadInfo.Progress           := Fields[f_progress].AsString;
            if Pos('/', DownloadInfo.Progress) <> 0 then
              DownCounter := StrToIntDef(Trim(ExtractWord(1, DownloadInfo.Progress, ['/'])), 0);
            DownloadInfo.SaveTo             := Fields[f_saveto].AsString;
            DownloadInfo.DateAdded          := Fields[f_dateadded].AsDateTime;
            DownloadInfo.DateLastDownloaded   := Fields[f_datelastdownloaded].AsDateTime;
            ChapterLinks.Text               := Fields[f_chapterslinks].AsString;
            ChapterNames.Text                := Fields[f_chaptersnames].AsString;
            PageLinks.Text                  := Fields[f_pagelinks].AsString;
            PageContainerLinks.Text         := Fields[f_pagecontainerlinks].AsString;
            FileNames.Text                  := Fields[f_filenames].AsString;
            CustomFileName                  := Fields[f_customfilenames].AsString;
            ChaptersStatus.Text             := Fields[f_chaptersstatus].AsString;
            if not (Status in [STATUS_PREPARE, STATUS_DOWNLOAD]) then
              ClearDirty(i)
            else
              FStoredOrder:=i;
          end;
          FDownloadsDB.Table.Next;
        end;
      finally
        LeaveCriticalsection(CS_Task);
      end;
    finally
      FDownloadsDB.CloseTable;
    end;
end;

procedure TDownloadManager.Backup;
var
  i: Integer;
begin
  if isRunningBackup then Exit;
  if Items.Count = 0 then Exit;
  if not FDownloadsDB.Connection.Connected then Exit;
  try
    EnterCriticalSection(CS_Task);
    isRunningBackup := True;
    for i := 0 to Items.Count - 1 do
      with Items[i] do begin
        if FDirty then
        begin
          FDownloadsDB.InternalUpdate(DlId,
            FEnabled,
            i,
            Integer(Status),
            CurrentDownloadChapterPtr,
            PageNumber,
            CurrentPageNumber,
            DownloadInfo.ModuleID,
            DownloadInfo.Link,
            DownloadInfo.Title,
            DownloadInfo.Status,
            DownloadInfo.Progress,
            DownloadInfo.SaveTo,
            DownloadInfo.DateLastDownloaded,
            ChapterLinks.Text,
            ChapterNames.Text,
            PageLinks.Text,
            PageContainerLinks.Text,
            FileNames.Text,
            CustomFileName,
            ChaptersStatus.Text);
          ClearDirty(i);
        end
        else
        begin
          if i<>FStoredOrder then
          begin
            FDownloadsDB.InternalUpdateOrder(DlId,i);
            FStoredOrder := i;
          end;
          if FDirtyEnabled then
          begin
            FDownloadsDB.InternalUpdateEnabled(DlId,Enabled);
            FDirtyEnabled := False;
          end;
        end;
      end;
    FDownloadsDB.Commit;
    isRunningBackup := False;
  finally
    LeaveCriticalSection(CS_Task);
  end;
end;

procedure TDownloadManager.GetDownloadedChaptersState(const AModuleID,
  ALink: String; var AChapters: array of TChapterStateItem);
var
  s: TStringList;
  i, p: Integer;
begin
  if Length(AChapters) = 0 then Exit;
  s := TStringList.Create;
  try
    s.Sorted := True;
    s.AddText(DownloadedChapters.Chapters[AModuleID, ALink]);
    if s.Count > 0 then
      for i := Low(AChapters) to High(AChapters) do
        AChapters[i].Downloaded := s.Find(LowerCase(AChapters[i].Link), p)
    else
      for i := Low(AChapters) to High(AChapters) do
        AChapters[i].Downloaded := False;
  finally
    s.Free;
  end;
end;

function TDownloadManager.AddTask: Integer;
begin
  Result := -1;
  EnterCriticalSection(CS_Task);
  try
    Result := Items.Add(TTaskContainer.Create);
    with Items[Result] do
    begin
      Manager := Self;
      Status := STATUS_STOP;
    end;
  finally
    LeaveCriticalSection(CS_Task);
  end;
end;

procedure TDownloadManager.CheckAndActiveTask(const isCheckForFMDDo: Boolean);
var
  i, tcount: Integer;
begin
  if Items.Count = 0 then Exit;
  EnterCriticalSection(CS_Task);
  try
    tcount := 0;
    for i := 0 to Items.Count - 1 do
      if Items[i].ThreadState then
        Inc(tcount);

    // item with missing module should be already checked by CheckAndActiveTaskAtStartup
    // and their status should be STATUS_STOP
    if tcount < OptionMaxParallel then
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if (Status = STATUS_WAIT) and
            (tcount < OptionMaxParallel) and
            TModuleContainer(DownloadInfo.Module).CanCreateTask then
          begin
            StartTask(i);
            Inc(tcount);
          end;
  finally
    LeaveCriticalSection(CS_Task);
  end;

  try
    if tcount > 0 then
    begin
      if not MainForm.tmRefreshDownloadsInfo.Enabled then
        MainForm.tmRefreshDownloadsInfo.Enabled := True;
      MainForm.UpdateVtDownload;
    end
    else
    begin
      MainForm.tmRefreshDownloadsInfo.Enabled := False;
      MainForm.UpdateVtDownload;
      if isCheckForFMDDo and (OptionLetFMDDo <> DO_NOTHING) then begin
        DoAfterFMD := OptionLetFMDDo;
        MainForm.DoExitWaitCounter;
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TDownloadManager.SetTaskActive(const taskID: Integer);
begin
  with Items[taskID] do
    if not(ThreadState or (Status in [STATUS_FINISH, STATUS_WAIT])) and Enabled and Assigned(DownloadInfo.Module) then
    begin
      Status := STATUS_WAIT;
      DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
    end;
end;

procedure TDownloadManager.CheckAndActiveTaskAtStartup;
var
  i, tcount: Integer;
begin
  if Items.Count = 0 then Exit;
  tcount := 0;
  for i := 0 to Items.Count - 1 do
    with Items[i] do
    begin
      if Status in [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_WAIT] then
      begin
        if DownloadInfo.Module = nil then
        begin
          Status := STATUS_STOP;
          DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Stopped]);
        end
        else if (tcount < OptionMaxParallel) and  TModuleContainer(DownloadInfo.Module).CanCreateTask then
        begin
          Inc(tcount);
          StartTask(i);
        end
        else if Status <> STATUS_WAIT then
        begin
          Status := STATUS_WAIT;
          DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
        end;
      end;
    end;
  //force to check task if all task loaded is STATUS_WAIT
  if tcount = 0 then
    CheckAndActiveTask
  else if MainForm.tmRefreshDownloadsInfo.Enabled = False then
    MainForm.tmRefreshDownloadsInfo.Enabled := True;
  MainForm.UpdateVtDownload;
end;

procedure TDownloadManager.StartTask(const taskID: Integer);
begin
  with Items[taskID] do
  begin
    TModuleContainer(DownloadInfo.Module).IncActiveTaskCount;
    TaskThread := TTaskThread.Create;
    TaskThread.Container := Items[taskID];
    AddItemsActiveTask(TaskThread.Container);
    TaskThread.Start;
  end;
end;

procedure TDownloadManager.StopTask(const taskID: Integer;
  const isCheckForActive: Boolean; isWaitFor: Boolean);
begin
  with Items[taskID] do
  begin
    if Status = STATUS_WAIT then
    begin
      Status := STATUS_STOP;
      DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Stopped]);
    end
    else if ThreadState then
    begin
      TaskThread.Terminate;
      if isWaitFor then
        TaskThread.WaitFor;
    end;
    if isCheckForActive then
      CheckAndActiveTask();
  end;
end;

procedure TDownloadManager.StartAllTasks;
var
  i: Integer;
begin
  if Items.Count > 0 then
  begin
    for i := 0 to Items.Count - 1 do
      with Items[i] do
        if (Status <> STATUS_FINISH) and Assigned(DownloadInfo.Module) and (not ThreadState) and Enabled then
        begin
          Status := STATUS_WAIT;
          DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
        end;
    CheckAndActiveTask;
  end;
end;

procedure TDownloadManager.StopAllTasks;
var
  i: Integer;
begin
  if Items.Count = 0 then Exit;
  for i := 0 to Items.Count - 1 do
    StopTask(i, False, False);
end;

procedure TDownloadManager.StopAllDownloadTasksForExit;
var
  i: Integer;
begin
  if Items.Count > 0 then
  begin
    isReadyForExit := True;
    try
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if ThreadState then
            TaskThread.Terminate;
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if ThreadState then
            TaskThread.WaitFor;
    finally
      isReadyForExit := False;
    end;
  end;
end;

procedure TDownloadManager.FreeAndDelete(const TaskId: Integer);
begin
  FDownloadsDB.Delete(Items[TaskID].DlId);
  Items[TaskID].Free;
  Items.Delete(taskID);
end;

procedure TDownloadManager.RemoveTask(const TaskID: Integer);
begin
  EnterCriticalSection(CS_Task);
  try
    with Items[TaskID] do
      if ThreadState then begin
        TaskThread.Terminate;
        TaskThread.WaitFor;
      end;
    FreeAndDelete(TaskID);
  finally
    LeaveCriticalSection(CS_Task);
  end;
  CheckAndActiveTask;
end;

procedure TDownloadManager.RemoveAllFinishedTasks;
var
  i: Integer;
begin
  if Items.Count = 0 then Exit;
  EnterCriticalsection(CS_Task);
  try
    for i := Items.Count - 1 downto 0 do
      if Items[i].Status = STATUS_FINISH then
        FreeAndDelete(i);
  finally
    LeaveCriticalsection(CS_Task);
  end;
end;

function TDownloadManager.TaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Items.Count > 0 then
  begin
    EnterCriticalSection(CS_Task);
    try
      for i := 0 to Items.Count - 1 do
        if Items[i].Status in Stats then begin
          Result := True;
          Break;
        end;
    finally
      LeaveCriticalSection(CS_Task);
    end;
  end;
end;

procedure TDownloadManager.EnableTask(const TaskId: Integer);
begin
  If not Items[TaskId].Enabled then
    Items[TaskId].Enabled := True;
end;

procedure TDownloadManager.DisableTask(const TaskId: Integer);
begin
  with Items[TaskId] do
  begin
    if ThreadState then
      StopTask(TaskId, False);
    if Enabled then
    begin
      if Status = STATUS_WAIT then
      begin
        Status := STATUS_STOP;
        DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Stopped]);
      end;
      Enabled := False;
    end;
  end;
end;

function CompareTaskContainer(const Item1, Item2: TTaskContainer): Integer;

  function GetStr(ARow: TTaskContainer): String;
  begin
    with ARow.DownloadInfo do
      case ARow.Manager.SortColumn of
        0: Result := Title;
        1: Result := Status;
        2: Result := Progress;
        3: Result := TransferRate;
        4: Result := ModuleID;
        5: Result := SaveTo;
        else
          Result := '';
      end;
  end;

  function GetDateTime(ARow: TTaskContainer): TDateTime;
  begin
    Result := ARow.DownloadInfo.DateAdded;
  end;

begin
  if (Item1.Manager.SortColumn = 6) or (Item1.Manager.SortColumn = 7) then
  begin
    if Item1.Manager.SortDirection then
      Result := CompareDateTime(GetDateTime(Item2), GetDateTime(Item1))
    else
      Result := CompareDateTime(GetDateTime(Item1), GetDateTime(Item2));
  end
  else
  begin
    if Item1.Manager.SortDirection then
      Result := NaturalCompareStr(GetStr(Item2), GetStr(Item1))
    else
      Result := NaturalCompareStr(GetStr(Item1), GetStr(Item2));
  end;
end;

procedure TDownloadManager.Sort(const AColumn: Integer);
begin
  if Items.Count < 2 then Exit;
  EnterCriticalSection(CS_Task);
  try
    SortColumn := AColumn;
    Items.Sort(@CompareTaskContainer);
  finally
    LeaveCriticalSection(CS_Task);
  end;
end;

end.
