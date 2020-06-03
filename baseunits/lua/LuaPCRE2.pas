unit LuaPCRE2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaPCRE2Register(L: Plua_State);

implementation

uses
  pcre2, pcre2lib, MultiLog, LuaClass, LuaUtils;

{/* directly pass the pointer by lua stack to pcre2 lib to save memory /*}

function re_exec(L: Plua_State): Integer; cdecl;
var
  re: ppcre2_code_8;
  match_data: ppcre2_match_data_8;
  offset_count, error_offset: Integer;
  InputString, Expression: PAnsiChar;
  InputLength, ExpressionLength: Cardinal;
  start_offset: Cardinal = 0;
  res: Boolean;
begin
  res:=false;
  InputString := lua_tolstring(L, 1, @InputLength);
  Expression := lua_tolstring(L, 2, @ExpressionLength);
  if lua_gettop(L)>=3 then
    start_offset := lua_tointeger(L, 3)-1;
  re := pcre2_compile_8(Expression, ExpressionLength, PCRE2_UTF, @offset_count, @error_offset, nil);
  if re <> nil then
  begin
    match_data := pcre2_match_data_create_from_pattern_8(re, nil);
    offset_count := pcre2_match_8(re, InputString, InputLength, start_offset, 0, match_data, nil);
    pcre2_match_data_free_8(match_data);
    pcre2_code_free_8(re);
    if offset_count >= 0 then
      res:=true;
  end
  else
    Logger.Send(pcre2.GetErrorMessage(offset_count, error_offset));
  lua_pushboolean(L, res);
  Result := 1;
end;

{ mimicing lua string.find return the position start and end instead of length }

function re_find(L: Plua_State): Integer; cdecl;
var
  re: ppcre2_code_8;
  match_data: ppcre2_match_data_8;
  offset_count, error_offset: Integer;
  InputString, Expression: PAnsiChar;
  InputLength, ExpressionLength: Cardinal;
  start_offset: Cardinal = 0;
  ovector: PPCRE2_SIZE;
  i: Cardinal;
begin
  Result := 0;
  InputString := lua_tolstring(L, 1, @InputLength);
  Expression := lua_tolstring(L, 2, @ExpressionLength);
  if lua_gettop(L)>=3 then
    start_offset := lua_tointeger(L, 3)-1; // pcre2 lib string start at 0
  re := pcre2_compile_8(Expression, ExpressionLength, PCRE2_UTF, @offset_count, @error_offset, nil);
  if re <> nil then
  begin
    match_data := pcre2_match_data_create_from_pattern_8(re, nil);
    offset_count := pcre2_match_8(re, InputString, InputLength, start_offset, 0, match_data, nil);
    if offset_count >= 0 then
    begin
       ovector := pcre2_get_ovector_pointer_8(match_data);
       i := ovector.GetStart(0);
       lua_pushinteger(L, i);
       lua_pushinteger(L, i-1+ovector.GetLength(0));
       Result := 2;
    end;
    pcre2_match_data_free_8(match_data);
    pcre2_code_free_8(re);
  end
  else
    Logger.Send(pcre2.GetErrorMessage(offset_count, error_offset));
end;

threadvar
  _gmatch: lua_CFunction;
  _gmatch_input_string, _gmatch_expression: PAnsiChar;
  _gmatch_input_length, _gmatch_expression_length: Cardinal;
  _gmatch_start_offset: Cardinal;
  _gmatch_re: ppcre2_code_8;

function re_match(L: Plua_State): Integer; cdecl;
var
  re: ppcre2_code_8;
  match_data: ppcre2_match_data_8;
  offset_count, error_offset: Integer;
  InputString, Expression: PAnsiChar;
  InputLength, ExpressionLength: Cardinal;
  start_offset: Cardinal;
  ovector: PPCRE2_SIZE;
  i: Integer;
begin
  Result := 0;
  re := nil;
  if _gmatch<>nil then //for gmatch iterator
  begin
    InputString := _gmatch_input_string;
    Expression := _gmatch_expression;
    InputLength := _gmatch_input_length;
    ExpressionLength := _gmatch_expression_length;
    start_offset := _gmatch_start_offset;
    if _gmatch_re = nil then
      _gmatch_re := pcre2_compile_8(Expression, ExpressionLength, PCRE2_UTF, @offset_count, @error_offset, nil);
    re := _gmatch_re;
  end
  else
  begin
    InputString := lua_tolstring(L, 1, @InputLength);
    Expression := lua_tolstring(L, 2, @ExpressionLength);
    if lua_gettop(L)>=3 then
      start_offset := lua_tointeger(L, 3)-1
    else
      start_offset := 0;
  end;
  if re = nil then
    re := pcre2_compile_8(Expression, ExpressionLength, PCRE2_UTF, @offset_count, @error_offset, nil);
  if re <> nil then
  begin
    match_data := pcre2_match_data_create_from_pattern_8(re, nil);
    offset_count := pcre2_match_8(re, InputString, InputLength, start_offset, 0, match_data, nil);
    if offset_count >= 0 then
    begin
      ovector := pcre2_get_ovector_pointer_8(match_data);
      if _gmatch<>nil then
        _gmatch_start_offset := ovector.GetStart(0)+ovector.GetLength(0)-1; // for gmatch iterator
      if offset_count = 1 then
      begin
        lua_pushlstring(L, InputString+ovector.GetStart(0)-1, ovector.GetLength(0));
        Result := 1;
      end
      else
      begin
        for i:=1 to offset_count-1 do
        begin
          lua_pushlstring(L, InputString+ovector.GetStart(i)-1, ovector.GetLength(i));
          Inc(Result);
        end;
      end;
    end;
    pcre2_match_data_free_8(match_data);
    if _gmatch <> nil then
    begin
      if Result = 0 then
      begin
        pcre2_code_free_8(_gmatch_re);
        _gmatch_re := nil;
        _gmatch := nil;
      end;
    end
    else
      pcre2_code_free_8(re);
  end
  else
    Logger.Send(pcre2.GetErrorMessage(offset_count, error_offset));
end;

function re_gmatch(L: Plua_State): Integer; cdecl;
begin
  _gmatch := @re_match;
  if _gmatch_re<>nil then
    pcre2_code_free_8(_gmatch_re);
  _gmatch_re := nil;
  _gmatch_input_string := lua_tolstring(L, 1, @_gmatch_input_length);
  _gmatch_expression := lua_tolstring(L, 2, @_gmatch_expression_length);
  if lua_gettop(L) >= 3 then
    _gmatch_start_offset := lua_tointeger(L, 3)-1
  else
    _gmatch_start_offset := 0;
  lua_pushcfunction(L, _gmatch);
  Result := 1;
end;

const
  PCRE2_GSUB_OPTIONS = PCRE2_SUBSTITUTE_OVERFLOW_LENGTH + PCRE2_SUBSTITUTE_GLOBAL;

function re_gsub(L: Plua_State): Integer; cdecl;
var
  re: ppcre2_code_8;
  error_code, error_offset: Integer;
  result_string: String;
  result_len: Cardinal;
  InputString, Expression, Replacement: PAnsiChar;
  InputLength, ExpressionLength, ReplacementLength: Cardinal;
  start_offset: Cardinal = 0;

  procedure dosubstitute;
  begin
    error_code := pcre2_substitute_8(re, InputString, InputLength,
      start_offset, PCRE2_GSUB_OPTIONS, nil, nil, Replacement, ReplacementLength,
      PAnsiChar(result_string), @result_len);
    if error_code = PCRE2_ERROR_NOMEMORY then
    begin
      setlength(result_string, result_len);
      dosubstitute;
    end
    else
      setlength(result_string, result_len);
  end;

begin
  InputString := lua_tolstring(L, 1, @InputLength);
  Expression := lua_tolstring(L, 2, @ExpressionLength);
  Replacement := lua_tolstring(L, 3, @ReplacementLength);
  if lua_gettop(L)>=4 then
    start_offset := lua_tointeger(L, 4)-1;
  result_string := InputString;
  re := pcre2_compile_8(Expression, ExpressionLength, PCRE2_UTF, @error_code, @error_offset, nil);
  if re <> nil then
  begin
    result_len := MAX_PATH;
    setlength(result_string, result_len);
    dosubstitute;
    pcre2_code_free_8(re);
    if error_code < 0 then
      Logger.Send(pcre2.GetErrorMessage(error_code, error_offset));
  end
  else
    Logger.Send(pcre2.GetErrorMessage(error_code, error_offset));
  lua_pushlstring(L, PAnsiChar(result_string), result_len);
  Result := 1;
end;

const
  methods: packed array [0..5] of luaL_Reg = (
    (name: 'exec'; func: @re_exec),
    (name: 'find'; func: @re_find),
    (name: 'match'; func: @re_match),
    (name: 'gmatch'; func: @re_gmatch),
    (name: 'gsub'; func: @re_gsub),
    (name: nil; func: nil)
    );

procedure luaPCRE2Register(L: Plua_State);
begin
  luaClassNewLib(L,'RE',methods);
end;

end.
