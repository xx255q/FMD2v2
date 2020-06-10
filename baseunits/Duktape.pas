unit Duktape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Duktape.Api, MultiLog;

function ExecJS(const text: String): String;

implementation

function NativePrint(AContext: PDukContext): TDukRet; cdecl;
begin
  { Join all arguments together with spaces between them. }
  duk_push_string(AContext, ' ');
  duk_insert(AContext, 0);
  duk_join(AContext, duk_get_top(AContext) - 1);
  Logger.Send(duk_safe_to_string(AContext, -1));
  Result := 0;
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
      raise Exception.Create('Failed to create a Duktape heap.');
      Exit;
    end;
    duk_module_duktape_init(ctx);
    duk_push_cfunction(ctx, 'print', @NativePrint);

    duk_push_string(ctx, PAnsiChar(text));
    r := duk_peval(ctx);
    s := duk_safe_to_string(ctx, -1);
    if r <> 0 then raise Exception.Create('Duktape error: ' + s)
    else Result := s;
  finally
    duk_destroy_heap(ctx);
  end;
end;

end.

