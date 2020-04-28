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
    function GetChapters(const AIdLink: String): String;
    procedure SetChapters(const AIdLink: String; AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Delete(const AIdLink: String);
    property Chapters[const AIdLink: String]: String read GetChapters write SetChapters;
    function ImportFromIni(const AFilename: String): Boolean;
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

function TDownloadedChaptersDB.GetChapters(const AIdLink: String): String;
begin
  Result := '';
  if AIdLink = '' then Exit;
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('idlink', LowerCase(AIdLink), []) then
        Result := Fields[1].AsString;
    finally
      LeaveCriticalsection(locklocate);
    end;
end;

procedure TDownloadedChaptersDB.SetChapters(const AIdLink: String; AValue: String);
begin
  if AIdLink = '' then Exit;
  if AValue = '' then Exit;
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('idlink', LowerCase(AIdLink), []) then
      begin
        Edit;
        Fields[1].AsString := MergeCaseInsensitive([Fields[1].AsString, AValue]);
      end
      else
      begin
        Append;
        Fields[0].AsString := LowerCase(AIdLink);
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
    '"idlink" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    '"chapters" TEXT';
  FieldsParams := '"idlink","chapters"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM ' + QuotedStrD(TableName);
end;

destructor TDownloadedChaptersDB.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(locklocate);
end;

procedure TDownloadedChaptersDB.Delete(const AIdLink: String);
begin
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('idlink', LowerCase(AIdLink), []) then
        Delete;
    finally
      LeaveCriticalsection(locklocate);
    end;
end;

function TDownloadedChaptersDB.ImportFromIni(const AFilename: String): Boolean;
var
  dc: TStringList;
  i: Integer = 0;
begin
  Result := False;
  if not Connected then Exit;
  if not FileExistsUTF8(AFilename) then Exit;
  dc := TStringList.Create;
  try
    dc.LoadFromFile(AFilename);
    if dc.Count > 0 then
      while i <= dc.Count - 2 do
      begin
        Chapters[dc[i]] := RemoveHostFromURL(GetParams(dc[i + 1]));
        Inc(i, 2);
      end;
    Result := True;
  finally
    dc.Free;
  end;
end;

end.

