unit BrotliDec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  function BrotliDecodeStream(inStream, outStream: TMemoryStream): Boolean;

implementation

const
  BROTLIDEC_LIB = 'libbrotlidec.dll';

type
  TBrotliSize = {$IFDEF CPU386} Integer {$ELSE} Int64 {$ENDIF};

const
  BROTLI_DECODER_PARAM_LARGE_WINDOW = 1;

  BROTLI_DECODER_RESULT_ERROR = 0;
  BROTLI_DECODER_RESULT_SUCCESS = 1;
  BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT = 2;
  BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT = 3;

  BUFFER_SIZE = 1 shl 19;

  function BrotliDecoderCreateInstance(
    const alloc_func, free_func, opaque: Pointer): Pointer; cdecl; external BROTLIDEC_LIB;

  procedure BrotliDecoderDestroyInstance(const state: Pointer); cdecl; external BROTLIDEC_LIB;

  function BrotliDecoderSetParameter(const state: Pointer;
    const BrotliDecoderParameter: Integer; const Value: Cardinal): Integer;
    cdecl; external BROTLIDEC_LIB;

  function BrotliDecoderDecompressStream(const state: Pointer;
    var available_in: TBrotliSize; var next_in: Pointer;
    var available_out: TBrotliSize; var next_out: Pointer;
    total_out: Pointer = nil): Integer; cdecl; external BROTLIDEC_LIB;


function BrotliDecodeStream(inStream, outStream: TMemoryStream): Boolean;
var
  State: Pointer;
  DecoderResult: Integer;
  AvailableIn: TBrotliSize;
  NextIn: Pointer;
  AvailableOut: TBrotliSize;
  NextOut: Pointer;
  BufferOut: array of Byte;

  procedure WriteOutput;
  var
    OutSize: TBrotliSize;
  begin
    OutSize := PAnsiChar(NextOut) - PAnsiChar(@BufferOut[0]);
    if OutSize = 0 then Exit;

    outStream.SetSize(outStream.Size + OutSize);
    outStream.Write(BufferOut[0], OutSize);
  end;

begin
  Result := False;
  State := BrotliDecoderCreateInstance(nil, nil, nil);
  if State = nil then Exit;

  try
    if BrotliDecoderSetParameter(State, BROTLI_DECODER_PARAM_LARGE_WINDOW, 1) = 0 then Exit;

    inStream.Position := 0;
    outStream.Clear;

    AvailableIn := inStream.Size;
    NextIn := inStream.Memory;
    AvailableOut := BUFFER_SIZE;
    SetLength(BufferOut, BUFFER_SIZE);
    NextOut := @BufferOut[0];

    while True do
    begin
      DecoderResult := BrotliDecoderDecompressStream(
        State, AvailableIn, NextIn, AvailableOut, NextOut);
      case DecoderResult of
        BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT:
          begin
            WriteOutput;
            AvailableOut := BUFFER_SIZE;
            NextOut := @BufferOut[0];
          end;

        BROTLI_DECODER_RESULT_SUCCESS:
          begin
            WriteOutput;
            Result := True;
            Exit;
          end;

        else begin
          Result := False;
          Exit;
        end;
      end;
    end;

    inStream.Position := 0;
    outStream.Position := 0;
  finally
    BrotliDecoderDestroyInstance(State);
  end;
end;

end.

