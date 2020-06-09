unit JSUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Duktape.Api;

function ExecJS(const text: String): String;

implementation

uses base64, MultiLog;

function NativePrint(AContext: PDukContext): TDukRet; cdecl;
var
  S: String;
begin
  { Join all arguments together with spaces between them. }
  duk_push_string(AContext, ' ');
  duk_insert(AContext, 0);
  duk_join(AContext, duk_get_top(AContext) - 1);

  { Get result and output to console }
  S := duk_safe_to_string(AContext, -1);
  Logger.Send(S);

  { "print" function does not return a value. }
  Result := 0;
end;

function duk_btoa(ctx: PDukContext): TDukRet; cdecl;
var
  str: Pointer;
  strsize: Cardinal;
  Outstream : TStringStream;
  Encoder   : TBase64EncodingStream;
  outstr: String;
begin
  Result:=0;
  str:=duk_to_buffer(ctx,0,@strsize);
  if strsize=0 then Exit;
  outstr:='';
  Outstream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(outstream);
    try
      Encoder.Write(str,strsize);
    finally
      Encoder.Free;
      end;
    outstr:=Outstream.DataString;
    duk_push_lstring(ctx,PAnsiChar(outstr),Length(outstr));
    Result:=1;
  finally
    Outstream.free;
  end;
end;

function duk_atob(ctx: PDukContext): TDukRet; cdecl;
var
  SD , outstr: String;
  Instream,
  Outstream : TStringStream;
  Decoder   : TBase64DecodingStream;
begin
  Result:=0;
  SD:=String(duk_safe_to_string(ctx, 0));
  if Length(SD)=0 then Exit;
  while Length(Sd) mod 4 > 0 do
    SD := SD + '=';
  Instream:=TStringStream.Create(SD);
  try
    Outstream:=TStringStream.Create('');
    try
      Decoder:=TBase64DecodingStream.Create(Instream,bdmMIME);
      try
         Outstream.CopyFrom(Decoder,Decoder.Size);
         outstr:=Outstream.DataString;
         duk_push_lstring(ctx,PAnsiChar(outstr),Length(outstr));
         Result:=1;
      finally
        Decoder.Free;
        end;
    finally
      Outstream.Free;
    end;
  finally
    Instream.Free;
  end;
end;

procedure duk_push_cfunction(const AContext: PDukContext; const aname: String;
  const func: TDukCFunction; const nargs: TDukIdx = DUK_VARARGS); inline;
begin
  duk_push_c_function(AContext, func, nargs);
  duk_put_global_string(AContext, PAnsiChar(aname));
end;

function ExecJS(const text: String): String;
var
  ctx: PDukContext;
  r: TDukInt;
  s: String;
begin
  Result := '';
  ctx := duk_create_heap_default;
  try
    if ctx = nil then begin
      Logger.SendError('Failed to create a Duktape heap.');
      Exit;
    end;
    duk_push_cfunction(ctx, 'print', @NativePrint);
    duk_push_cfunction(ctx, 'atob', @duk_atob);
    duk_push_cfunction(ctx, 'btoa', @duk_btoa);

    duk_push_string(ctx, PAnsiChar(text));
    r := duk_peval(ctx);
    s := duk_safe_to_string(ctx, -1);
    if r <> 0 then Logger.SendError('Duktape error: ' + s)
    else Result := s;
  finally
    duk_destroy_heap(ctx);
  end;
end;

end.

