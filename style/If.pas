
function IfStyle(Value: Integer): Boolean;
begin

    // Only if statement, maybe in one line
    if Value > 0 then Result := False;

    // Only if statement, with begin-end
    if Value > 0 then
    begin
        Result := False; Exit;
    end;

    // if ... else, one line
    if Value > 0 then Result := False else Result := True;

    // if ... else with with begin-end
    if Value > 0 then
    begin
        Result := True;
        // Any other statement
    end else
    begin
        Result := False;
        // Any other statement
    end;

    // Several statements
    if Value = 1 then
    begin
        Result := False;
    end
    else if Value = 2 then
        begin
            Result := True;
        end;

end;
