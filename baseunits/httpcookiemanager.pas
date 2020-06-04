unit httpcookiemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, StrUtils, DateUtils, syncobjs, synautil, httpsend;

type

  { THTTPCookie }

  THTTPCookie = class
  private
    FName,
    FValue,
    FDomain,
    FPath,
    FSameSite: String;
    FExpires: TDateTime;
    FHostOnly,
    FHttpOnly,
    FSecure,
    FPersistent: Boolean;
  published
    property Name: String read FName write FName;
    property Value: String read FValue write FValue;
    property Domain: String read FDomain write FDomain;
    property Path: String read FPath write FPath;
    property SameSite: String read FSameSite write FSameSite;
    property Expires: TDateTime read FExpires write FExpires;
    property HostOnly: Boolean read FHostOnly write FHostOnly;
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    property Secure: Boolean read FSecure write FSecure;
    property Persistent: Boolean read FPersistent write FPersistent;
  end;

  THTTPCookies = specialize TFPGObjectList<THTTPCookie>;

  { THTTPCookieManager }

  THTTPCookieManager = class
  private
    FCookies: THTTPCookies;
    FGuardian: TCriticalSection;
  protected
    procedure InternalAddServerCookie(const AURL, ACookie: String; const AServerDate: TDateTime);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddServerCookie(const AURL, ACookie: String; const AServerDate: TDateTime);
    procedure AddServerCookies(const AURL: String; const AServerHeaders: TStringList);
    procedure SetCookies(const AURL: String; const AHTTP: THTTPSend);
    procedure Clear;
  published
    property Cookies: THTTPCookies read FCookies;
  end;

implementation

{ defined in RFC 6265 https://tools.ietf.org/html/rfc6265 }

{ THTTPCookieManager }

constructor THTTPCookieManager.Create;
begin
  FGuardian := TCriticalSection.Create;
  FCookies := THTTPCookies.Create(True);
end;

destructor THTTPCookieManager.Destroy;
begin
  FCookies.Free;
  FGuardian.Free;
  inherited Destroy;
end;

procedure THTTPCookieManager.InternalAddServerCookie(const AURL, ACookie: String;
  const AServerDate: TDateTime);
var
  Prot, User, Pass, Host, Port, Path, Para: String;
  s, n, ni, v: String;
  c: THTTPCookie;
  i: Integer;
  scookie: TStringArray;
begin
  s := Trim(ACookie);
  if s = '' then Exit;

  ParseURL(AURL, Prot, User, Pass, Host, Port, Path, Para);
  scookie := s.Split(';');
  if Length(scookie) = 0 then Exit;

  c := THTTPCookie.Create;
  try
    c.Name := Trim(SeparateLeft(scookie[0], '='));
    c.Value := Trim(SeparateRight(scookie[0], '='));
    c.Domain := Host;
    c.Path := Path;
    Delete(scookie, 0, 1);
    for s in scookie do
    begin
      n := Trim(SeparateLeft(s,'='));
      v := Trim(SeparateRight(s, '='));
      ni := LowerCase(n);
      if ni = 'domain' then
        {
          leading %x2E (".") is ignored per revised specification
          https://tools.ietf.org/html/rfc6265#section-4.1.2.3
        }
        c.Domain := v.ToLower.TrimLeft('.')
      else
      if ni = 'path' then
        c.Path := v
      else
      if ni = 'expires' then
      begin
        c.Expires := DecodeRfcDateTime(v);
        c.Persistent := True;
      end
      else
      if ni = 'max-age' then
      begin
        c.Expires := IncSecond(AServerDate, StrToIntDef(v, 0));
        c.Persistent := True;
      end
      else
      if ni = 'secure' then
        c.Secure := True
      else
      if ni = 'httponly' then
        c.HttpOnly := True
      else
      if ni = 'samesite' then
        c.SameSite := LowerCase(v);
    end;
    if c.SameSite = '' then
      c.SameSite := 'none';

    // search and delete existing cookie
    for i := 0 to FCookies.Count-1 do
      if (c.Name = FCookies[i].Name) and (c.Domain = FCookies[i].Domain) and (c.Path = FCookies[i].Path) then
      begin
        FCookies.Delete(i);
        Break;
      end;
  finally
    FCookies.Add(c);
  end;
  Finalize(scookie);
end;

procedure THTTPCookieManager.AddServerCookie(const AURL, ACookie: String;
  const AServerDate: TDateTime);
begin
  FGuardian.Enter;
  try
    InternalAddServerCookie(AURL, ACookie, AServerDate);
  finally
    FGuardian.Leave;
  end;
end;

procedure THTTPCookieManager.AddServerCookies(const AURL: String; const AServerHeaders: TStringList
  );
var
  i: Integer;
  s: String;
  d: TDateTime;
begin
  FGuardian.Enter;
  try
    s := Trim(AServerHeaders.Values['Date']);
    if s <> '' then
      d := DecodeRfcDateTime(s)
    else
      d := Now;
    for i := 0 to AServerHeaders.Count - 1 do
    begin
      if Pos('set-cookie', LowerCase(AServerHeaders[i])) = 1 then
      begin
        InternalAddServerCookie(AURL, Trim(AServerHeaders.ValueFromIndex[i]), d);
      end;
    end;
  finally
    FGuardian.Leave;
  end;
end;

function CharEquals(const AString: string; const ACharPos: Integer; const AValue: Char): Boolean;
begin
  if ACharPos < 1 then Exit(False);
  Result := (ACharPos <= Length(AString)) and (AString[ACharPos] = AValue);
end;

procedure THTTPCookieManager.SetCookies(const AURL: String;
  const AHTTP: THTTPSend);
var
  Prot, User, Pass, Host, Port, Path, Para: String;
  i: Integer;
  c: THTTPCookie;

  function IsPathMatch: Boolean;
  begin
    Result := SameText(Path, c.Path) or
              ( Path.StartsWith(c.Path) and
                ( c.Path.EndsWith('/') or CharEquals(Path, Length(c.Path), '/') )
              );
  end;

  function IsDomainMatch: Boolean;
  begin
    Result := False;
    if (Host <> '') and (c.Domain <> '') then
    begin
      if SameText(Host, c.Domain) then
        Result := True
      else
      if Host.EndsWith(c.Domain) then
      begin
        if Copy(Host, 1, Length(Host)-Length(c.Domain)).EndsWith('.') then
          Result := True;
      end;
    end;
  end;

  function MatchesHost: Boolean;
  begin
    if c.HostOnly then
      Result := SameText(Host, c.Domain)
    else
      Result := IsDomainMatch;
  end;

  function IsHTTP: Boolean;
  begin
    Result := (Prot = 'http') or (Prot = 'https');
  end;

begin
  if FCookies.Count = 0 then Exit;
  FGuardian.Enter;
  try
    ParseURL(AURL, Prot, User, Pass, Host, Port, Path, Para);
    Prot := LowerCase(Prot);
    Host := LowerCase(Host);
    i := 0;
    while i <= FCookies.Count - 1 do
    begin
      c := FCookies[i];
      if (c.Persistent) and (c.Expires <= Now) then
        FCookies.Delete(i)
      else
      begin
        Inc(i);
        if MatchesHost and IsPathMatch and
            ((not c.Secure) or (c.Secure and c.Secure)) and
            ((not c.HttpOnly) or (c.HttpOnly and IsHTTP)) then
        begin
          AHTTP.Cookies.Values[c.Name] := c.Value;
        end;
      end;
    end;
  finally
    FGuardian.Leave;
  end;
end;

procedure THTTPCookieManager.Clear;
begin
  FGuardian.Enter;
  try
    FCookies.Clear;
  finally
    FGuardian.Leave;
  end;
end;

end.

