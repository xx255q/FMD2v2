unit FileCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TOnLoadFile = function(const AFileName: String): TObject;

  { TFileCache }

  TFileCache = class
  private
    FGuardian: TRTLCriticalSection;
    FCachedFiles: TStringList;
  public
    OnLoadFile: TOnLoadFile;
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AName: String; var AObject: TObject);
    function Find(const AName: String): TObject;
    procedure Clear;
  end;

implementation


{ TFileCache }

constructor TFileCache.Create;
begin
  InitCriticalSection(FGuardian);
  FCachedFiles := TStringList.Create;
  FCachedFiles.OwnsObjects := True;
  FCachedFiles.Sorted := True;
  FCachedFiles.Duplicates := dupIgnore;
end;

destructor TFileCache.Destroy;
begin
  FCachedFiles.Free;
  DoneCriticalSection(FGuardian);
  inherited Destroy;
end;

procedure TFileCache.Add(const AName: String; var AObject: TObject);
var
  i: Integer;
begin
  i := FCachedFiles.Add(AName);
  if FCachedFiles.Objects[i] = nil then
    FCachedFiles.Objects[i] := AObject
  else
  begin
    AObject.Free;
    AObject := FCachedFiles.Objects[i];
  end;
end;

function TFileCache.Find(const AName: String): TObject;
var
  i: Integer;
begin
  if FCachedFiles.Find(AName, i) then
    Result := FCachedFiles.Objects[i]
  else
  if Assigned(OnLoadFile) then
  begin
    EnterCriticalSection(FGuardian);
    try
      Result := OnLoadFile(AName);
      if Result <> nil then
        Add(AName, Result);
    finally
      LeaveCriticalSection(FGuardian);
    end;
  end
  else
    Result := nil;
end;

procedure TFileCache.Clear;
begin
  FCachedFiles.Clear;
end;

end.
