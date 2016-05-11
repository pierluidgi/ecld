%%%-------------------------------------------------------------------
%%% Erlang cluster domain controller server
%%% Created : 08 Jun 2016 by alexs <alexs@ximad.com>
%%%-------------------------------------------------------------------
-module(ecld_c).

-behaviour(gen_server).


-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_domain/2, stop_domain/1]).

%% Print message with module and line number point
-define(p(Str), list_to_binary(io_lib:format("Mod:~w line:~w ~100P~n", [?MODULE,?LINE, Str, 300]))).


-define(DCS_SUP, ecld_sup).
-define(DC_SUP,  ecld_c_sup).
-define(DMS_SUP, ecld_ms_sup).
-define(WORKER,  ecld_worker).



%% See README.md
start_domain(Name, WorkerChildSpec) -> 
  %% 1. start domain controller supervisor (dc_sup)
  DcSupName = list_to_atom(atom_to_list(Name) ++ "_c_sup"),
  DcSupChildSpec = mk_ch_sp_domain(?DC_SUP, DcSupName, supervisor, permanent, [DcSupName]),
  {ok, _DcSupPid} = supervisor:start_child(?DCS_SUP, DcSupChildSpec),
  %% 2. start domain members supervisor (dms_sup)
  DmsSupName = list_to_atom(atom_to_list(Name) ++ "_ms_sup"),
  DmsSupChildSpec = mk_ch_sp_domain(?DMS_SUP, DmsSupName, supervisor, permanent, [DmsSupName]),
  {ok, _DmsSupPid} = supervisor:start_child(DcSupName, DmsSupChildSpec),
  %% 3. start domain controler server (dc)
  DCArgs = [Name, DcSupName, DmsSupName, WorkerChildSpec],
  DcChildSpec = mk_ch_sp_domain(?MODULE, Name, worker, permanent, [DCArgs]),
  {ok, _DcPid} = supervisor:start_child(DcSupName, DcChildSpec),
  ok.


%%
stop_domain(Name) ->
  DcSupName = list_to_atom(atom_to_list(Name) ++ "_c_sup"),
  supervisor:terminate_child(?DCS_SUP, DcSupName),
  supervisor:delete_child(?DCS_SUP, DcSupName).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
start_link([Name|_] = Args) -> gen_server:start_link({local, Name}, ?MODULE, Args, []).
handle_info(Message, State) -> io:format("Unk msg ~p~n", [{State, Message}]), {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%%casts
handle_cast({run, _FunName, Fun, Args}, State)             -> apply(Fun, [State|Args]);
handle_cast({reply, {Id, Ref, Reply}}, State)       -> reply_(State, Id, Ref, Reply);
handle_cast(_Req, State) -> {noreply, State}.
%%calls
handle_call({run, _FunName, Fun, Args}, From, State)       -> apply(Fun, [State,From|Args]);
handle_call(ping, _From, State)                            -> {reply, pong, State};
%% domain modes
handle_call(flush, _From, State)                           -> flush_(State);
handle_call({flush_queue, Id}, _From, State)               -> flush_queue_(State, Id);
handle_call({norma, _Domain, Id, Options}, From, State)    -> recv_(State, From, norma,  Id, Options);
handle_call({proxy, Domains, Id, Options}, From, State)    -> recv_(State, From, {proxy, Domains}, Id, Options);
handle_call({merge, Domains, Id, Options}, From, State)    -> recv_(State, From, {merge, Domains}, Id, Options);
handle_call({start_with_state, Id, IdState}, _From, State) -> start_with_state_(State, Id, IdState);
%%
handle_call(_Req, _From, State) -> {reply, unknown_command, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init([Name, DcSupPid, DmsSupPid, WorkerChildSpec]) -> 
  S = #{mode    => non_blocking, %% TODO blocking|non_blocking
        name    => Name, 
        qs      => qdict:new(), 
        dc_sup  => DcSupPid, 
        dms_sup => DmsSupPid, 
        child   => WorkerChildSpec},
  {ok, S}.


flush_(S) ->
  {reply, ok, S#{qs := dict:new()}}.
  
flush_queue_(S = #{qs := QS}, Id) ->
  {reply, ok, S#{qs := dict:erase(Id, QS)}}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Messages to domains
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {norma,     Domain,                  Id, Options}
%% {proxy,     {NewDomain, OldDomain}}, Id, Options}
%% {merge,     {NewDomain, OldDomain}}, Id, Options}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Non blocking receive
%% recv_(S, Mode, Id, Options) -> {noreply, NewS}. {{{
recv_(S = #{dms_sup := Sup, child := TplChild, qs := QS}, From, Mode, Id, Options) ->
  Ref = erlang:make_ref(),
  QMember = #{
      from      => From, 
      mode      => Mode, 
      sup       => Sup, 
      child_sp  => TplChild, 
      opts      => Options},

  {NewQS, QSize} = qdict:in_l(Id, {Ref, QMember}, QS),

  % send to request if first in queue
  case QSize == 1 of 
    true  -> req(Id, Ref, QMember);
    false -> do_nothing
  end,

  {noreply, S#{qs := NewQS}}.
%%}}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Non blocking reply
%% reply_(S = #{qs := QS}, Id, Ref, Reply) -> {noreply, NewS}. {{{
reply_(S = #{qs := QS}, Id, Ref, Reply) ->
  case qdict:out2(Id, QS) of
    {{value, {Ref1, #{from := From} }}, empty, NewQS} when Ref1 == Ref ->
        gen_server:reply(From, Reply),
        {noreply, S#{qs := NewQS}};
    {{value, {Ref1, #{from := From} }}, {value, {Ref2, QMember2}}, NewQS} when Ref1 == Ref -> 
        gen_server:reply(From, Reply),
        req(Id, Ref2, QMember2),
        {noreply, S#{qs := NewQS}};
    _ ->
        {noreply, S}
  end.
%% }}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

%
req(Id, Ref, #{mode := norma, sup := Sup, child_sp := TplChild, opts := Options}) ->
  Self = self(),
  spawn(fun() -> norma_spawn(Self, Sup, Id, Ref, Options, TplChild) end);
%
req(Id, Ref, #{mode := {proxy, [_ND, OD]}, sup := Sup, child_sp := TplChild, opts := Options}) ->
  Self = self(),
  spawn(fun() -> proxy_spawn(Self, OD, Sup, Id, Ref, Options, TplChild) end);
%
req(Id, Ref, #{mode := {merge, [_ND, OD]}, sup := Sup, child_sp := TplChild, opts := Options}) ->
  Self = self(),
  spawn(fun() -> merge_spawn(Self, OD, Sup, Id, Ref, Options, TplChild) end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% blocking starting for migrate
%% start_with_state_(S, Id, IdState) {{{
start_with_state_(S = #{dms_sup := Sup, child := TplChild = #{start := {M,F,A}} }, Id, IdState) ->
  Child = TplChild#{id => Id, start := {M,F,[#{state => IdState}|A]}},
  Reply =
    case supervisor:start_child(Sup, Child) of
      {ok, Pid}                       -> {ok, Pid};
      Else                            -> {err, {start_or_lookup_failure, Else}}
    end,
  {reply, Reply, S}.
%%}}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% SPAWNS

% Just norma
%% norma_spawn(DcPid, Sup, Id, Ref, Options, TplChild = #{start := {M,F,A}}) {{{ 
norma_spawn(DcPid, Sup, Id, Ref, Options, TplChild = #{start := {M,F,A}}) ->
  Child = TplChild#{id => Id, start := {M,F,[Options#{id => Id}|A]}},
  Reply =
    case supervisor:start_child(Sup, Child) of
      {error, {already_started, Pid}} -> {ok, Pid};
      {ok, Pid}                       -> {ok, Pid};
      {error, {not_started,     _}}   -> not_started; %% If Type == lookup_process
      {error, {not_exists,      _}}   -> not_exists;  %% If Type == lookup_process
      Else                            -> {err, {start_or_lookup_failure, Else}}
    end,
  gen_server:cast(DcPid, {reply, {Id, Ref, Reply}}).
%%}}}


% Proxy mode
% 1. Check on new domain
%   1.1. If true return pid
% 2. Proxy to old domain in norma mode
%% proxy_spawn(DcPid, OD, Sup, Id, Ref, Options, TplChild = #{start := {M,F,A}}) {{{
proxy_spawn(DcPid, OD, Sup, Id, Ref, Options, TplChild = #{start := {M,F,A}}) ->
  ProxyOptions = #{start => false, init => false},
  ProxyChild = TplChild#{id => Id, start := {M,F,[ProxyOptions|A]}},
  Reply =
    case supervisor:start_child(Sup, ProxyChild) of
      {error, {already_started, Pid}} -> {ok, Pid};
      {ok, Pid}                       -> {ok, Pid};
      {error, {not_started,     _}}   -> 
        #{node := Node, name := Name} = OD,  
        gen_server:call({Name,Node}, {norma, OD, Id, Options});
      Else -> {err, {start_or_lookup_failure, Else}}
    end,
  gen_server:cast(DcPid, {reply, {Id, Ref, Reply}}).
%% }}}


% Merge mode
% 1. Check on new domain
%   1.1. If true return pid
% 2. Check on old domain
%   2.1. If true, do migrate and return pid
% 3. Start on new domain
%% merge_spawn(DcPid, OD, Sup, Id, Ref, Options, TplChild = #{start := {M,F,A}}) {{{
merge_spawn(DcPid, OD, Sup, Id, Ref, Options, TplChild = #{start := {M,F,A}}) ->
  MergeOptions = #{start => false, init => false},
  MergeChild = TplChild#{id => Id, start := {M,F,[MergeOptions|A]}},
  Reply =
    case supervisor:start_child(Sup, MergeChild) of
      {error, {already_started, Pid}} -> {ok, Pid};
      {error, {not_started,     _}}   ->
        #{node := Node, name := Name} = OD,
        case gen_server:call({Name, Node}, {norma, OD, Id, Options}) of
          {ok, Pid} -> do_migrate(Id, Pid, DcPid); %% do_migrate
          not_started ->
            Child = TplChild#{id => Id, start := {M,F,[Options|A]}},
            case supervisor:start_child(Sup, Child) of
              {error, {already_started, Pid}} -> {ok, Pid};
              {ok, Pid}                       -> {ok, Pid};
              {error, {not_started,     _}}   -> not_started; %% If Type == lookup_process
              {error, {not_exists,      _}}   -> not_exists;  %% If Type == lookup_process
              Else                            -> {err, {start_or_lookup_failure, Else}}
            end;
          Else -> {err, {start_or_lookup_failure, Else}}
        end;
      Else -> {err, {start_or_lookup_failure, Else}}
    end,
  gen_server:cast(DcPid, {reply, {Id, Ref, Reply}}).

%%
do_migrate(Id, Pid, DcPid) -> 
  %% 1. Take a State
  {ok, State} = gen_server:call(Pid, get_state),
  %% 2. Start with state in sync mode
  {ok, NewPid} = gen_server:call(DcPid, {start_with_state, Id, State}),
  %% 3. Stop old
  gen_server:call(Pid, stop_due_migrate),
  {ok, NewPid}.
%%}}}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MISC
%mk_ch_sp(Module, Name, Type, Restart, Args) ->
%  #{id        => Name,
%    start     => {Module, start_link, Args#{id => Name}},
%    restart   => Restart,
%    shutdown  => 5000,
%    type      => Type,
%    modules   => [Module]}.

%
mk_ch_sp_domain(Module, Name, Type, Restart, Args) ->
  #{id        => Name,
    start     => {Module, start_link, Args},
    restart   => Restart,
    shutdown  => 5000,
    type      => Type,
    modules   => [Module]}.
