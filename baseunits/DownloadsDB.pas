unit DownloadsDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, uBaseUnit, sqlite3dyn;

type

  { TDownloadsDB }

  TDownloadsDB = class(TSQLiteDataWA)
  public
    constructor Create(const AFilename: String);
  end;

const
  f_id                 = 0;
  f_enabled            = 1;
  f_order              = 2;
  f_taskstatus         = 3;
  f_chapterptr         = 4;
  f_numberofpages      = 5;
  f_currentpage        = 6;
  f_moduleid           = 7;
  f_link               = 8;
  f_title              = 9;
  f_status             = 10;
  f_progress           = 11;
  f_saveto             = 12;
  f_dateadded          = 13;
  f_datelastdownloaded = 14;
  f_chapterslinks      = 15;
  f_chaptersnames      = 16;
  f_pagelinks          = 17;
  f_pagecontainerlinks = 18;
  f_filenames          = 19;
  f_customfilenames    = 20;
  f_chaptersstatus     = 21;

implementation

{ TDownloadsDB }

constructor TDownloadsDB.Create(const AFilename: String);
begin
  inherited Create;
  InitCriticalSection(Guardian);
  Filename := AFilename;
  TableName := 'downloads';
  CreateParams :=
    '"id" INTEGER PRIMARY KEY,' +
    '"enabled" BOOLEAN,' +
    '"order" INTEGER,' +
    '"taskstatus" INTEGER,' +
    '"chapterptr" INTEGER,' +
    '"numberofpages" INTEGER,' +
    '"currentpage" INTEGER,' +
    '"moduleid" TEXT,' +
    '"link" TEXT,' +
    '"title" TEXT,' +
    '"status" TEXT,' +
    '"progress" TEXT,' +
    '"saveto" TEXT,' +
    '"dateadded" DATETIME,' +
    '"datelastdownloaded" DATETIME,' +
    '"chapterslinks" TEXT,' +
    '"chaptersnames" TEXT,' +
    '"pagelinks" TEXT,' +
    '"pagecontainerlinks" TEXT,' +
    '"filenames" TEXT,' +
    '"customfilenames" TEXT,' +
    '"chaptersstatus" TEXT';
  FieldsParams := '"id","enabled","order","taskstatus","chapterptr","numberofpages","currentpage","moduleid","link","title","status","progress","saveto","dateadded","datelastdownloaded","chapterslinks","chaptersnames","pagelinks","pagecontainerlinks","filenames","customfilenames","chaptersstatus"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM '+QuotedStrD(TableName)+' ORDER BY "order"';
end;

end.

