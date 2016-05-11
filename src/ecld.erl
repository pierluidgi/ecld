
-module(ecld).

-export([get_pid/3, call/4, cast/4]).



-type answer() :: {ok, pid()}|not_started|not_exists|{err, {atom(), binary()}}.
%%
%% Options = #{
%%    start => true, %% Start process if not exists
%%    init  => true  %% Init profile if not exists in database
%% }. 
%%  

-spec get_pid(atom(), binary(), map()) -> answer().
get_pid(Cluster, Id, Options) ->
  case ecl:get(Cluster, Id) of
    {ok, {Status, [D]}} when Status == norma ->
      #{node := Node, name := Name} = D,
      gen_server:call({Name, Node}, {Status, D, Id, Options});
    {ok, {Status, Ds = [ND, _OD] }} when Status == proxy; Status == merge ->
      #{node := Node, name := Name} = ND,
      gen_server:call({Name, Node}, {Status, Ds, Id, Options});
    Else -> Else
  end.


call(Cluster, Id, Msg, Options) -> 
  case get_pid(Cluster, Id, Options) of
    {ok, Pid} -> gen_server:call(Pid, Msg);
    Else -> Else
  end.


cast(Cluster, Id, Msg, Options) ->
  case get_pid(Cluster, Id, Options) of
    {ok, Pid} -> gen_server:cast(Pid, Msg);
    Else -> Else
  end.

