type

    {
        The basic node with a value for all collections.
    }
    TNode<V> = class(TObject)
    private
        FValue: V;
    protected
        function GetValue(): V; virtual;
        procedure SetValue(AValue: V); virtual;
    public
        property Value: V read GetValue write SetValue;
        function HasValue(): Boolean;
        constructor Create(AValue: V); virtual;
    end;

constructor TNode<V>.Create(AValue: V);
begin
    inherited Create();
    Self.Value := AValue;
end;

function TNode<V>.GetValue(): V;
begin
    Result := FValue;
end;

procedure TNode<V>.SetValue(AValue: V);
begin
    FValue := V;
end;

function TNode<V>.HasValue(): Boolean;
begin
    Result := Self.Value <> nil;
end;
