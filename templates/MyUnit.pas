{------------------------------------------------------------------------------}
{                                                                              }
{                             PROJECT-NAME project                             }
{                                                                              }
{  Copyright (C) 1996-2023 Mikhail Malakhov <malakhv@gmail.com>                }
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
{ [Unit synopsis. The Unit is needed for... And it contains stuff              }
{ for...]                                                                      }
{                                                                              }
{ Project: PROJECT-NAME                                                        }
{ Package: Mikhan.Templates                                                    }
{ Types: TType1, TType2                                                        }
{ Dependencies: No                                                             }
{                                                                              }
{ Created: 14.11.2023                                                          }
{ Authors: Mikhail.Malakhov [malakhv@gmail.com|http://mikhan.me/]              }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{                                Unit Overview                                 }
{                                                                              }
{ Any long description about this Unit...                                      }
{                                                                              }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{                             Common Definitions                               }
{                                                                              }
{ Term1 -   Any description, definition and information, Any description,      }
{           definition and information.                                        }
{                                                                              }
{ Term2 -   Any description, definition and information, Any description,      }
{           definition and information.                                        }
{                                                                              }
{------------------------------------------------------------------------------}

unit Mikhan.Templates.MyUnit                                            {~Unit~}

{$mode DELPHI}
{$H+}

interface                                                  {~Interface Section~}

uses SysUtils;


{ The group of constants }
const

    { The constant 1. }
    CONST_1 = 1;

    { The constant 2. }
    CONST_1 = 2;

{ Global Scope }
var
    GlobalVar: Byte;    // This is a global variable

{
    The type for. The type for. The type for. The type for. The type
    for. The type for.
}
type
    TType1 = 1..3;

{------------------------------------------------------------------------------}
{                             Unit Section 1                                   }
{                                                                              }
{ Any long description about this Unit's section..                             }
{                                                                              }
{------------------------------------------------------------------------------}


implementation                                        {~Implementation Section~}

{------------------------------------------------------------------------------}
{ Common                                                                       }
{------------------------------------------------------------------------------}

{ The group of constants }
const

    { The internal constant 1. }
    INTERNAL_CONST_1 = 1;

{ Implementation Scope }
var
    InternalVar: Byte;  // This is a variable

{ Returns any value. }
function GetValue(): Byte;
begin
    Result := InternalVar;
end;

{ Increments any value. }
procedure IncValue(var Value: Integer);
begin
    Inc(Value);
end;

{------------------------------------------------------------------------------}
{ TType1                                                                       }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ TType2                                                                       }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{~END                                                                          }
{------------------------------------------------------------------------------}
