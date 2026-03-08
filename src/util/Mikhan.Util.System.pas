{------------------------------------------------------------------------------}
{                                                                              }
{                          Pascal Utils Library (PUL)                          }
{                                                                              }
{  Copyright (C) 1996-2026 Mikhail Malakhov, http://mikhan.me/                 }
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License"). You may     }
{  not use this file except in compliance with the License. You may obtain     }
{  a copy of the License at                                                    }
{                                                                              }
{     http://www.apache.org/licenses/LICENSE-2.0                               }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT   }
{  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.            }
{                                                                              }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{------------------------------------------------------------------------------}

UNIT Mikhan.Util.System;                                                { UNIT }

{$MODE DELPHI}
{$H+}

INTERFACE                                                          { INTERFACE }

{------------------------------------------------------------------------------}
{                                  Endianness                                  }
{                                                                              }
{ Endianness means the order in which the bytes of a value larger than one     }
{ byte are stored in memory. This affects, e.g., integer values and pointers   }
{ while, e.g., arrays of single-byte characters are not affected. Endianness   }
{ depends on the hardware, especially the CPU.                                 }
{------------------------------------------------------------------------------}

{ Returns True for big-endian hardware platform. }
function IsBigEndian(): Boolean;

{ Returns True for little-endian hardware platform. }
function IsLittleEndian(): Boolean;

{------------------------------------------------------------------------------}

IMPLEMENTATION                                                { IMPLEMENTATION }

{------------------------------------------------------------------------------}
{ Endianness                                                                   }
{------------------------------------------------------------------------------}

function IsBigEndian(): Boolean;
var
    W: Word;
begin
    W := 1;
    Result := PByte(@W)^ = 0;
end;

function IsLittleEndian(): Boolean;
var
    W: Word;
begin
    W := 256;
    Result := PByte(@W)^ = 0;
end;

{------------------------------------------------------------------------------}

END.                                                                     { END }

{------------------------------------------------------------------------------}

