unit FavoritesDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, uBaseUnit;

type

  { TFavoritesDB }

  TFavoritesDB = class(TSQLiteDataWA)
  public
    constructor Create(const AFilename: String);
    procedure Add(const Aid:String;const AOrder:Integer;const AEnabled:Boolean;
      const AModuleID,ALink,ATitle,AStatus,ACurrentChapter,ADownloadedChapterList,ASaveTo:String;
      const ADateAdded:TDateTime); inline;
    procedure Update(const Aid:String;const AStatus,ACurrentChapter,ADownloadedChapterList:String;
      const ADateLastUpdated:TDateTime); inline;
    procedure UpdateTitle(const Aid,Atitle:String); inline;
    procedure UpdateEnabled(const Aid:String;const AEnabled:Boolean); inline;
    procedure UpdateDateLastChecked(const Aid:String;const ADateLastChecked:TDateTime);
    procedure UpdateSaveTo(const Aid,ASaveTo:String);
    procedure Delete(const Aid:String); inline;
  end;

const
  f_id                     = 0;
  f_order                  = 1;
  f_enabled                = 2;
  f_moduleid               = 3;
  f_link                   = 4;
  f_title                  = 5;
  f_status                 = 6;
  f_currentchapter         = 7;
  f_downloadedchapterlist  = 8;
  f_saveto                 = 9;
  f_dateadded              = 10;
  f_datelastchecked        = 11;
  f_datelastupdated        = 12;

implementation

{ TFavoritesDB }

constructor TFavoritesDB.Create(const AFilename: String);
begin
  inherited Create;
  Filename := AFilename;
  TableName := 'favorites';
  CreateParams :=
    '"id" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
    '"order" INTEGER,' +
    '"enabled" BOOLEAN,' +
    '"moduleid" TEXT,' +
    '"link" TEXT,' +
    '"title" TEXT,' +
    '"status" TEXT,' +
    '"currentchapter" TEXT,' +
    '"downloadedchapterlist" TEXT,' +
    '"saveto" TEXT,' +
    '"dateadded" DATETIME,' +
    '"datelastchecked" DATETIME,' +
    '"datelastupdated" DATETIME';
  FieldsParams := '"id","order","enabled","moduleid","link","title","status","currentchapter","downloadedchapterlist","saveto","dateadded","datelastchecked","datelastupdated"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM ' + QuotedStrD(TableName) + ' ORDER BY "order"';
end;

procedure TFavoritesDB.Add(const Aid: String; const AOrder: Integer; const AEnabled: Boolean;
  const AModuleID, ALink, ATitle, AStatus, ACurrentChapter, ADownloadedChapterList,
  ASaveTo: String; const ADateAdded: TDateTime);
begin
  AppendSQL('INSERT OR REPLACE INTO "favorites" (' +
    FieldsParams +
    ') VALUES (' +
    QuotedStr(Aid) + ',' +
    QuotedStr(AOrder) + ',' +
    QuotedStr(AEnabled) + ',' +
    QuotedStr(AModuleID) + ',' +
    QuotedStr(ALink) + ',' +
    QuotedStr(ATitle) + ',' +
    QuotedStr(AStatus) + ',' +
    QuotedStr(ACurrentChapter)  + ',' +
    QuotedStr(ADownloadedChapterList) + ',' +
    QuotedStr(ASaveTo) + ',' +
    QuotedStr(ADateAdded) + ',' +
    QuotedStr(ADateAdded) + ',' +
    QuotedStr(ADateAdded) + ');');
end;

procedure TFavoritesDB.Update(const Aid: String; const AStatus, ACurrentChapter,
  ADownloadedChapterList: String; const ADateLastUpdated: TDateTime);
begin
  AppendSQL('UPDATE "favorites" SET "status"='+QuotedStr(AStatus)+
    ',"currentchapter"='+QuotedStr(ACurrentChapter)+
    ',"downloadedchapterlist"='+QuotedStr(ADownloadedChapterList)+
    ',"datelastupdated"='+QuotedStr(ADateLastUpdated)+
    ' WHERE "id"='+QuotedStr(Aid)+';');
end;

procedure TFavoritesDB.UpdateTitle(const Aid, Atitle: String);
begin
  AppendSQLSafe('UPDATE "favorites" SET "title"='+QuotedStr(Atitle)+' WHERE "id"='+QuotedStr(Aid)+';');
end;

procedure TFavoritesDB.UpdateEnabled(const Aid: String; const AEnabled: Boolean);
begin
  AppendSQL('UPDATE "favorites" SET "enabled"='+QuotedStr(AEnabled)+' WHERE "id"='+QuotedStr(Aid)+';');
end;

procedure TFavoritesDB.UpdateDateLastChecked(const Aid: String; const ADateLastChecked: TDateTime);
begin
  AppendSQL('UPDATE "favorites" SET "datelastchecked"='+QuotedStr(ADateLastChecked)+' WHERE "id"='+QuotedStr(Aid)+';');
end;

procedure TFavoritesDB.UpdateSaveTo(const Aid, ASaveTo: String);
begin
  AppendSQL('UPDATE "favorites" SET "saveto"='+QuotedStr(ASaveTo)+' WHERE "id"='+QuotedStr(Aid)+';');
end;

procedure TFavoritesDB.Delete(const Aid: String);
begin
  AppendSQL('DELETE FROM "favorites" WHERE "id"='+QuotedStr(aid)+';');
end;

end.

