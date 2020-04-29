unit DownloadedChaptersDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, uBaseUnit, LazFileUtils;

type

  { TDownloadedChaptersDB }

  TDownloadedChaptersDB = class(TSQliteData)
  private
    locklocate: TRTLCriticalSection;
    function GetChapters(const AModuleID, ALink: String): String;
    procedure SetChapters(const AModuleID, ALink: String; AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
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
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('id', LowerCase(AModuleID+ALink), []) then
        Result := Fields[1].AsString;
    finally
      LeaveCriticalsection(locklocate);
    end;
end;

procedure TDownloadedChaptersDB.SetChapters(const AModuleID, ALink: String;
  AValue: String);
begin
  if AValue = '' then Exit;
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
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
      LeaveCriticalsection(locklocate);
    end;
end;

constructor TDownloadedChaptersDB.Create;
begin
  inherited Create;
  InitCriticalSection(locklocate);
  AutoApplyUpdates := True;
  TableName := 'downloadedchapters';
  CreateParams :=
    '"id" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    '"chapters" TEXT';
  FieldsParams := '"id","chapters"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM ' + QuotedStrD(TableName);
end;

destructor TDownloadedChaptersDB.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(locklocate);
end;

procedure TDownloadedChaptersDB.Delete(const AModuleID, ALink: String);
begin
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('id', LowerCase(AModuleID+ALink), []) then
        Delete;
    finally
      LeaveCriticalsection(locklocate);
    end;
end;

end.

