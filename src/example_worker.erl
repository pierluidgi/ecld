-module(example_worker).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


% 
start_link(_Arg = #{start := true, init := Init}, Id) -> 
  %% get_profile
  case {get_profile(Id), Init} of
    {{ok, Profile}, _}  -> gen_server:start_link(?MODULE, Profile, []);
    {not_exists, true}  -> gen_server:start_link(?MODULE, #{profile_src => init}, []);
    {not_exists, false} -> not_exists;
    {Else, _}           -> Else
  end;
start_link(_,_) -> 
  not_started.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(Message, State) -> io:format("Unk msg ~p~n", [{State, Message}]), {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%%casts
handle_cast({run, _FunName, Fun, Args}, State) -> apply(Fun, [State|Args]);
handle_cast(_Req, State) -> {noreply, State}.
%%calls
handle_call({run, _FunName, Fun, Args}, From, State) -> apply(Fun, [State,From|Args]);
handle_call(_Req, _From, State) -> {reply, unknown_command, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init(Profile) ->
  S = #{profile => Profile},
  {ok, S}.



get_profile(_Id) -> {ok, #{profile_src => loaded}}.






