{ Converts bytes in Word. }
function ReverseBytes(const Value: Word): Word;
begin
    Result :=  (((Value and $FF00) shr 8)
        or ((Value and $00FF) shl 8));
end;
