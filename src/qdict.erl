%%
%% dict of queues wraper
%%

-module(qdict).
-compile(export_all).

%%
new() -> dict:new().


%%
-spec in(Key::term(), Item::term(), Dict1::dict:dict()) -> Dict2::dict:dict(). 
in(Key, Item, Dict1) ->
  NewQ = case dict:find(Key, Dict1) of
    {ok, Q} -> queue:in(Item, Q);
    error   -> queue:in(Item, queue:new())
  end,
  dict:store(Key, NewQ, Dict1).

%%
-spec in_l(Key::term(), Item::term(), Dict1::dict:dict()) -> {Dict2::dict:dict(), Len::integer()}. 
in_l(Key, Item, Dict1) ->
  NewQ = case dict:find(Key, Dict1) of
    {ok, Q} -> queue:in(Item, Q);
    error   -> queue:in(Item, queue:new())
  end,
  {dict:store(Key, NewQ, Dict1), queue:len(NewQ)}.


%%
-spec out(Key::term(), Dict1::dict:dict()) -> {{value, Item::term()}|empty, Dict2::dict:dict()}. 
out(Key, Dict1) ->
  case dict:find(Key, Dict1) of
    {ok, Q} -> 
      case queue:out(Q) of
        {{value, Item}, NewQ} -> 
          case queue:is_empty(NewQ) of
            true  -> {{value, Item}, dict:erase(Key, Dict1)};
            false -> {{value, Item}, dict:store(Key, NewQ, Dict1)}
          end;
        {empty, _} -> {empty, dict:erase(Key, Dict1)}
      end;
    error -> {empty, Dict1}
  end.


%%
-spec out2(Key::term(), Dict1::dict:dict()) -> 
    {{value, Item1::term()}|empty, {value, Item2::term()}|empty, Dict2::dict:dict()}. 
out2(Key, Dict1) ->
  case dict:find(Key, Dict1) of
    {ok, Q} -> 
      case queue:out(Q) of
        {{value, Item1}, NewQ1} -> 
          case queue:out(NewQ1) of
            {{value, Item2}, _NewQ2} -> {{value, Item1}, {value, Item2}, dict:store(Key, NewQ1, Dict1)};
            _                        -> {{value, Item1}, empty, dict:erase(Key, Dict1)}
          end;
        {empty, _} -> {empty, empty, dict:erase(Key, Dict1)}
      end;
    error -> {empty, empty, Dict1}
  end.
