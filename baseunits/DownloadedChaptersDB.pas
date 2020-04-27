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
    function GetChapters(const AIdURI: String): String;
    procedure SetChapters(const AIdURI: String; AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Delete(const AIdURI: String);
    property Chapters[const AIdURI: String]: String read GetChapters write SetChapters;
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

function TDownloadedChaptersDB.GetChapters(const AIdURI: String): String;
begin
  Result := '';
  if AIdURI = '' then Exit;
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('iduri', LowerCase(AIdURI), []) then
        Result := Fields[1].AsString;
    finally
      LeaveCriticalsection(locklocate);
    end;
end;

procedure TDownloadedChaptersDB.SetChapters(const AIdURI: String; AValue: String);
begin
  if AIdURI = '' then Exit;
  if AValue = '' then Exit;
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('iduri', LowerCase(AIdURI), []) then
      begin
        Edit;
        Fields[1].AsString := MergeCaseInsensitive([Fields[1].AsString, AValue]);
      end
      else
      begin
        Append;
        Fields[0].AsString := LowerCase(AIdURI);
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
    '"iduri" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    '"chapters" TEXT';
  FieldsParams := '"iduri","chapters"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM ' + QuotedStrD(TableName);
end;

destructor TDownloadedChaptersDB.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(locklocate);
end;

procedure TDownloadedChaptersDB.Delete(const AIdURI: String);
begin
  if not Connected then Exit;
  EnterCriticalsection(locklocate);
  with Table do
    try
      if Locate('iduri', LowerCase(AIdURI), []) then
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

