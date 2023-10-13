{--------------------------------------------------------------------}
{                                                                    }
{                    Pascal Utils Library (PUL)                      }
{                                                                    }
{  Copyright (C) 1996-2023 Mikhail Malakhov <malakhv@gmail.com>      }
{                                                                    }
{  Licensed under the Apache License, Version 2.0 (the "License").   }
{  You may not use this file except in compliance with the License.  }
{  You may obtain a copy of the License at                           }
{                                                                    }
{     http://www.apache.org/licenses/LICENSE-2.0                     }
{                                                                    }
{  Unless required by applicable law or agreed to in writing,        }
{  software distributed under the License is distributed on an       }
{  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      }
{  either express or implied.                                        }
{                                                                    }
{  See the License for the specific language governing permissions   }
{  and limitations under the License.                                }
{                                                                    }
{--------------------------------------------------------------------}

{--------------------------------------------------------------------}
{ The Unit contains methods to print raw data in hexadecimal format  }
{ (dump, like in hex editors).                                       }
{                                                                    }
{ Package: Mikhan.Util                                               }
{ Types: TAppLogs, TLogLevel                                         }
{ Dependencies: Mikhan.Util.StrUtils                                 }
{                                                                    }
{ Created: 14.08.2023                                                }
{ Author: Mikhail.Malakhov                                           }
{--------------------------------------------------------------------}

unit Mikhan.Util.Dump;

{$mode delphi}
{$h+}

{-------------------------------------------------------------------------}
{ Interface Section                                                       }
{-------------------------------------------------------------------------}

Interface
 
type

    { The formats of dump output, hex or char. }
    TDumpOutFormat = (dfHex, dfChar);

{
  Prints raw data in hexadecimal format.
}
procedure Dump(const Source: Array of Byte); overload;
    
{
  Prints raw data in hexadecimal format.
}
procedure Dump(const Source: Array of Byte; Limit: Integer); overload;
    
{
  Prints raw data in hexadecimal format.
}
procedure Dump(const Source: Array of Byte; Offset, Limit: Integer);
    overload;
    
{
  Prints raw data in hexadecimal or char format.
}
procedure Dump(const Source: Array of Byte; Offset, Limit: Integer;
    Format: TDumpOutFormat); overload;

{-------------------------------------------------------------------------}
{ Implementation Section                                                  }
{-------------------------------------------------------------------------}

implementation

uses SysUtils, Mikhan.Util.StrUtils;

{ Makes a symbol to output. }
function MakeSymbol(Value: Integer; Format: TDumpOutFormat): String;
begin
    // Hex
    if Format = dfHex then
    begin
        Result := IntToHex(Value, 2); Exit;
    end;
    // Char
    if Value >= 20 then
        Result := Char(Value) + Char($0)
    else
        Result := '  ';
end;

procedure Dump(const Source: Array of Byte);
begin
    Dump(Source, 0, 0, dfHex);
end;

procedure Dump(const Source: Array of Byte; Limit: Integer);
begin
    Dump(Source, 0, Limit, dfHex);
end;

procedure Dump(const Source: Array of Byte; Offset, Limit: Integer);
begin
    Dump(Source, Offset, Limit, dfHex);
end;

procedure Dump(const Source: Array of Byte; Offset, Limit: Integer;
        Format: TDumpOutFormat);

const ADDRESS_SPACE = $F;

    procedure Header();
    var i: Integer;
    begin
        Write(EMPTY:10);
        for i := 0 to ADDRESS_SPACE do
            Write(IntToHex(i, 2), CHAR_SPACE);
        WriteLn();
        Write(RepeatString('-', 57));
    end;

    procedure NewRow(Address: Integer);
    begin
        WriteLn();
        Write(IntToHex(Address, 8), CHAR_VERT_SLASH, CHAR_SPACE);
    end;

var
    COL, OFF, SKIP: Integer;

var
    i: Integer; val: Byte;

begin
    Header();

    // Calc initial parameters
    COL := ADDRESS_SPACE;
    OFF := (Offset div 16) + (ADDRESS_SPACE * (Offset div 16));
    SKIP := (Offset mod 16);
    if Limit <= 0 then Limit := MaxInt;

    // Start printing
    NewRow(OFF);
    for i := Low(Source) to High(Source) do
    begin

        // Should we go to a new row?
        if COL < 0 then
        begin
            OFF := OFF + ADDRESS_SPACE + 1;
            NewRow(OFF);
            COL := ADDRESS_SPACE;
        end;
        Dec(COL);

        // Need to skip?
        while SKIP > 0 do
        begin
            Write(EMPTY:3);
            Dec(SKIP);
            Dec(COL);
        end;

        // Print value
        val := Source[i];
        Write(MakeSymbol(val, Format), CHAR_SPACE);
        Dec(Limit);

        // Should stop?
        if Limit <= 0 then break;
    end;
    Writeln();
end;

end.

{-------------------------------------------------------------------------}
{ END                                                                     }
{-------------------------------------------------------------------------}
