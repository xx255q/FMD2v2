{
        File: uPacker.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uPacker;

{$mode delphi}

interface

uses
  Classes, Zipper, zstream, SysUtils, uBaseUnit, Img2Pdf, FileUtil,
  LazFileUtils, SimpleException, uEpub, MultiLog;

type
  TPackerFormat = (pfZIP, pfCBZ, pfPDF, pfEPUB);

  { TPacker }

  TPacker = class
  protected
    FSavedFileName, FExt: String;
    FFileList: TStringList;
    procedure FileFound(FileIterator: TFileIterator);
    function DoZipCbz: Boolean;
    procedure DoPdf;
    procedure DoEpub;
  public
    Path,
    FileName: String;
    Format: TPackerFormat;
    CompressionQuality: Cardinal;
    function Execute: Boolean;
    property FileList: TStringList read FFileList;
    property SavedFileName: String read FSavedFileName;
    property Ext: String read FExt;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure TPacker.FileFound(FileIterator: TFileIterator);
begin
  FFileList.Add(FileIterator.Filename);
end;

function TPacker.DoZipCbz: Boolean;
var
  i: Integer;
begin
  Result:=False;
  with TZipper.Create do
    try
      try
        FileName := FSavedFileName;
        for i := 0 to FFileList.Count - 1 do
          with Entries.AddFileEntry(FFileList[i]) do
          begin
            CompressionLevel := clnone;
            ArchiveFileName := ExtractFileName(FFileList[i]);
          end;
        ZipAllFiles;
      except
        on E: Exception do
        begin
          E.Message := 'DoZipCbz.Exception'#13#10 + E.Message;
          SimpleException.ExceptionHandleSaveLogOnly(Self, E);
        end;
      end;
    finally
      Free;
    end;
  Result := FileExists(FSavedFileName);
  if Result then
    with TUnZipper.Create do
      try
         FileName:=FSavedFileName;
         Examine;
         Result := FileList.Count = Entries.Count;
         if not Result then
           Logger.SendWarning('Some files failed to be compressed!');
      finally
        Free;
      end;
end;

procedure TPacker.DoPdf;
var
  pdf: TImg2Pdf;
  i: Cardinal;
  fstream: TFileStream;
begin
  try
    pdf := TImg2Pdf.Create;
    try
      pdf.CompressionQuality := CompressionQuality;
      pdf.Infos.Title := GetLastDir(Path);
      pdf.Infos.Creator := ApplicationName;
      for i := 0 to FFileList.Count - 1 do
      begin
        try
          pdf.AddImage(FFileList[i]);
        except
        end;
      end;

      fstream := TFileStream.Create(FSavedFileName, fmCreate);
      try
        pdf.SaveToStream(fstream);
      finally
        fstream.Free;
      end;
    finally
      pdf.Free;
    end;
  except
    on E: Exception do
    begin
      E.Message := 'DoPdf.Exception'#13#10 + E.Message;
      SimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
end;

procedure TPacker.DoEpub;
var
  epub: TEpubBuilder;
  i: Integer;
  fstream: TFileStream;
begin
  try
    epub := TEpubBuilder.Create;
    try
      epub.Title := GetLastDir(Path);
      for i := 0 to FFileList.Count - 1 do
      begin
        try
          epub.AddImage(FFileList[i]);
        except
        end;
      end;

      fstream := TFileStream.Create(FSavedFileName, fmCreate);
      try
        epub.SaveToStream(fstream);
      finally
        fstream.Free;
      end;
    finally
      epub.Free;
    end;
  except
    on E: Exception do
    begin
      E.Message := 'DoEpub.Exception'#13#10 + E.Message;
      SimpleException.ExceptionHandleSaveLogOnly(Self, E);
    end;
  end;
end;

function TPacker.Execute: Boolean;
var
  i: Integer;
  packResult: Boolean;
begin
  Result := False;
  Path := CorrectPathSys(Path);

  if FFileList.Count = 0 then
  begin
    if DirectoryExists(Path) = False then Exit;
    with TFileSearcher.Create do
      try
        OnFileFound := FileFound;
        Search(Self.Path, '*.jpg;*.png;*.gif;*.webp', False, False);
      finally
        Free;
      end;
  end;

  if FFileList.Count = 0 then Exit;

  FFileList.CustomSort(NaturalCustomSort);
  case Format of
    pfZIP: FExt := '.zip';
    pfCBZ: FExt := '.cbz';
    pfPDF: FExt := '.pdf';
    pfEPUB: FExt := '.epub';
  end;
  if FileName <> '' then
    FSavedFileName := FileName + FExt
  else
    FSavedFileName := TrimAndExpandFilename(Path) + FExt;
  if FileExists(FSavedFileName) then
    if DeleteFile(FSavedFileName) = False then
      Exit;
  packResult:=True;
  case Format of
    pfZIP, pfCBZ: packResult:=DoZipCbz;
    pfPDF: DoPdf;
    pfEPUB: DoEpub;
  end;
  if not packResult then Exit(False);
  Result := FileExists(FSavedFileName);
  if Result then
  begin
    for i := 0 to FFileList.Count - 1 do
      DeleteFile(FFileList[i]);
    if IsDirectoryEmpty(Path) then
      RemoveDir(Path);
  end;
end;

constructor TPacker.Create;
begin
  FFileList := TStringList.Create;
  FSavedFileName := '';
  FExt := '';
  Path := '';
  FileName := '';
  Format := pfZIP;
end;

destructor TPacker.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

end.
