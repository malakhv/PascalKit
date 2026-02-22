{------------------------------------------------------------------------------}
{                                                                              }
{                             PROJECT-NAME Project                             }
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

{------------------------------------------------------------------------------}
{ Program or Project short description. A brief summary or general survey of   }
{ this Program. What the program does and why it is needed.                    }
{                                                                              }
{ Project: PROJECT-NAME                                                        }
{ Package: Mikhan.Templates                                                    }
{                                                                              }
{ Dependencies: [NO | list of non-standard (non RTL/VCL) Units ]               }
{ Operating Systems: [Win, Linux, MacOS]                                       }
{                                                                              }
{ Created: 14.11.2023                                                          }
{ Authors: Mikhail.Malakhov                                                    }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{                                  Overview                                    }
{                                                                              }
{ A general summary of Programm. An overview gives the big picture, while      }
{ leaving out the minor details. You can use this section to leave any         }
{ important and useful information. And sure, you can change head of this      }
{ section.                                                                     }
{                                                                              }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{                                  How to use                                  }
{                                                                              }
{ Description of the program using - input parameters, command line arguments, }
{ output data, etc. In general, this section may include a copy of the         }
{ program's help (--help). And sure, you can change head of this section.      }
{                                                                              }
{------------------------------------------------------------------------------}

PROGRAM name;                                                        { PROGRAM }

{$MODE DELPHI}
{$APPTYPE CONSOLE}
{$H+}
{$T+}

USES                                                                    { USES }

    SysUtils;

{
    Internal Program Data
}
const

    { The common debug flag. }
    DEBUG = False;

{
    About this Program
}
const

    { The name of this Program. }
    PROG_NAME = 'S-Dump';

    { The author of this Program. }
    PROG_AUTHOR = 'Mikhail.Malakhov';

    { The copyright string. }
    PROG_COPYRIGHT = 'Copyright (C) 1996-2023 Mikhail Malakhov, ' +
        'http://mikhan.me/';

{------------------------------------------------------------------------------}
{ Options and Arguments                                                        }
{------------------------------------------------------------------------------}

{
    Program command line arguments (options and commands)
}
const

    { Program option: A short description of this option, short format. }
    OPT_ONE_SHORT = '-o';
    { Program option: A short description of this option, long format. }
    OPT_ONE_LONG = '--one';

{ Program command line arguments variables }
var
    OptOne: Boolean;    // See 'One' program option

{------------------------------------------------------------------------------}
{ Global Scope                                                                 }
{------------------------------------------------------------------------------}

var
    X: Integer;     // Global variable X

{------------------------------------------------------------------------------}
{ Procedures and Functions                                                     }
{------------------------------------------------------------------------------}

{ Calculates the value depending on the program options. }
function CalcX(): Integer;
begin
    if OptOne then Result := X - 1
    else Result := X + 1;
end;

{------------------------------------------------------------------------------}

BEGIN                                                            { ENTRY POINT }

    WriteLn('Hello World!');

END.                                                                     { END }

{------------------------------------------------------------------------------}
