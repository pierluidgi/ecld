-module(example_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


% 
start_link(Arg) -> gen_server:start_link(?MODULE, Arg, []).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
handle_info(timeout, State) -> io:format("timeout shutdown ~p~n", [self()]),  {stop, normal, State};
handle_info(Message, State) -> io:format("Unk msg ~p~n", [{State, Message}]), {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%%casts
handle_cast({run, _FunName, Fun, Args}, State)       -> apply(Fun, [State|Args]);
handle_cast(_Req, State)                             -> {noreply, State}.
%%calls
handle_call({run, _FunName, Fun, Args}, From, State) -> apply(Fun, [State,From|Args]);
handle_call(ping, _From, State)                      -> {reply, pong, State, 10000};  %% Timeout for shutdown
handle_call(get_state, _From, State)                 -> {reply, {ok, State}, State};
handle_call(stop_due_migrate, _From, State)          -> {stop, normal, ok, State};
handle_call(_Req, _From, State)                      -> {reply, unknown_command, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Init process
%
init(_Args = #{start := true, init := Init, id := Id}) ->
  %% Try to load profile
  case get_profile(Id) of
    {ok, Profile} -> {ok, #{profile => Profile}};
    not_exists -> 
      case Init of
        true ->
          %% Init profile
          case init_profile(Id) of
            {ok, Profile} -> {ok, #{profile => Profile}};
            Else          -> Else
          end;
        false -> not_exists
      end;
    Else -> Else
  end.



get_profile(_Id)  -> {ok, #{profile => loaded}}.
init_profile(_Id) -> {ok, #{profile => inited}}.






