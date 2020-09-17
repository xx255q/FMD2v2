unit DownloadedChaptersDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, uBaseUnit, SQLDB, LazFileUtils;

type

  { TDownloadedChaptersDB }

  TDownloadedChaptersDB = class(TSQliteData)
  private
    FGuardian: TRTLCriticalSection;
    function GetChapters(const AModuleID, ALink: String): String;
    procedure SetChapters(const AModuleID, ALink: String; AValue: String);
  protected
    procedure Lock; inline;
    procedure UnLock; inline;
  public
    constructor Create;
    destructor Destroy; override;
    function Open: Boolean;
    procedure Commit; override;
    procedure Delete(const AModuleID, ALink: String);
    property Chapters[const AModuleID, ALink: String]: String read GetChapters write SetChapters;
  end;

implementation

function CleanStr(const S: String): String;
begin
  Result := LowerCase(Trim(S));
  if Pos(' ', Result) > 0 then
    Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
  while Pos(LineEnding + LineEnding, Result) > 0 do
    Result := StringReplace(Result, LineEnding + LineEnding, LineEnding, [rfReplaceAll]);
end;

{ TDownloadedChaptersDB }

function TDownloadedChaptersDB.GetChapters(const AModuleID, ALink: String
  ): String;
begin
  Result := '';
  if not Connected then Exit;
  Lock;
  with Table do
    try
      if Locate('id', LowerCase(AModuleID+ALink), []) then
        Result := Fields[1].AsString;
    finally
      UnLock;
    end;
end;

procedure TDownloadedChaptersDB.SetChapters(const AModuleID, ALink: String;
  AValue: String);
begin
  if AValue = '' then Exit;
  if not Connected then Exit;
  Lock;
  with Table do
    try
      if Locate('id', LowerCase(AModuleID+ALink), []) then
      begin
        Edit;
        Fields[1].AsString := MergeCaseInsensitive([Fields[1].AsString, AValue]);
      end
      else
      begin
        Append;
        Fields[0].AsString := LowerCase(AModuleID+ALink);
        Fields[1].AsString := AValue;
      end;
      try
        Post;
      except
        CancelUpdates;
      end;
    finally
      UnLock;
    end;
end;

procedure TDownloadedChaptersDB.Lock;
begin
  EnterCriticalSection(FGuardian);
end;

procedure TDownloadedChaptersDB.UnLock;
begin
  LeaveCriticalSection(FGuardian);
end;

constructor TDownloadedChaptersDB.Create;
begin
  inherited Create;
  InitCriticalSection(FGuardian);
  AutoApplyUpdates := True;
  Table.Options:=Table.Options-[sqoAutoCommit];
  Table.PacketRecords:=1;
  Table.UniDirectional:=False;
  TableName := 'downloadedchapters';
  CreateParams :=
    '"id" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    '"chapters" TEXT';
  FieldsParams := '"id","chapters"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM ' + QuotedStrD(TableName);
end;

destructor TDownloadedChaptersDB.Destroy;
begin
  DoneCriticalsection(FGuardian);
  inherited Destroy;
end;

function TDownloadedChaptersDB.Open: Boolean;
begin
  Result:=inherited Open(True,False);
end;

procedure TDownloadedChaptersDB.Commit;
begin
  Lock;
  try
    inherited Commit;
  finally
    UnLock;
  end;
end;

procedure TDownloadedChaptersDB.Delete(const AModuleID, ALink: String);
begin
  if not Connected then Exit;
  Lock;
  with Table do
    try
      if Locate('id', LowerCase(AModuleID+ALink), []) then
        Delete;
    finally
      UnLock;
    end;
end;

end.

