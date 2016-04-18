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
-export([get_pid/3]).

%% Print message with module and line number point
-define(p(Str), list_to_binary(io_lib:format("Mod:~w line:~w ~100P~n", [?MODULE,?LINE, Str, 300]))).


-define(DCS_SUP, ecld_cs_sup).
-define(DC_SUP,  ecld_c_sup).
-define(DMS_SUP, ecld_ms_sup).
-define(WORKER,  ecld_worker).



%% See README.md
start_domain(Name, WorkerChildSpec) -> 
  %% 1. start domain controller supervisor (dc_sup)
  DcSupName = list_to_atom(atom_to_list(Name) ++ "_c_sup"),
  DcSupChildSpec = mk_ch_sp_domain(?DC_SUP, DcSupName, supervisor, permanent, [DcSupName]),
  {ok, DcSupPid} = supervisor:start_child(?DCS_SUP, DcSupChildSpec),
  %% 2. start domain members supervisor (dms_sup)
  DmsSupName = list_to_atom(atom_to_list(Name) ++ "_ms_sup"),
  DmsSupChildSpec = mk_ch_sp_domain(?DMS_SUP, DmsSupName, supervisor, permanent, [DmsSupName]),
  {ok, DmsSupPid} = supervisor:start_child(DcSupName, DmsSupChildSpec),
  %% 3. start domain controler server (dc)
  DCArgs = [Name, DcSupPid, DmsSupPid, WorkerChildSpec],
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
handle_cast({run, _FunName, Fun, Args}, State) -> apply(Fun, [State|Args]);
handle_cast(_Req, State) -> {noreply, State}.
%%calls
handle_call({run, _FunName, Fun, Args}, From, State) -> apply(Fun, [State,From|Args]);
handle_call(ping, _From, State) -> {reply, pong, State};
%% domain modes
handle_call({norma, Domain,  Id, Options}, _From, State) -> norma_(State, Domain,  Id, Options);
handle_call({proxy, Domains, Id, Options}, _From, State) -> proxy_(State, Domains, Id, Options);
handle_call({merge, Domains, Id, Options}, _From, State) -> merge_(State, Domains, Id, Options);
%%
handle_call(_Req, _From, State) -> {reply, unknown_command, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init([Name, DcSupPid, DmsSupPid, WorkerChildSpec]) -> 
  S = #{name => Name, qs => [], dc_sup => DcSupPid, dms_sup => DmsSupPid, child => WorkerChildSpec},
  {ok, S}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
-type answer()   :: {ok, pid()}|not_started|not_exists|{err, {atom(), binary()}}.


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
      gen_server:call({Name,Node}, {Status, D, Id, Options});
    {ok, {Status, Ds = [ND, _OD] }} when Status == proxy; Status == merge -> 
      #{node := Node, name := Name} = ND,
      gen_server:call({Name, Node}, {Status, Ds, Id, Options});
    Else -> Else
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Messages to domains
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {norma,     Domain,                  Id, Options}
%% {proxy,     {NewDomain, OldDomain}}, Id, Options}
%% {merge,     {NewDomain, OldDomain}}, Id, Options}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Just normal
norma_(S = #{dms_sup := Sup, child := TplChild = #{start := {M,F,A}}}, _Domain, Id, Options) ->
  Child = TplChild#{id => Id, start := {M,F,[Options|A]}},
  mk_ch_sp(?WORKER, Id, worker, temporary, Options),
  Reply =
    case supervisor:start_child(Sup, Child) of
      {error, {already_started, Pid}} -> {ok, Pid};
      {error, {not_started,     _}}   -> not_started; %% If Type == lookup_process
      {error, {not_exists,      _}}   -> not_exists;  %% If Type == lookup_process
      Else                            -> {err, {start_or_lookup_failure, Else}}
    end,
  {reply, Reply, S}.


%% Just proxy
proxy_(S, [_NewDomain, OldDomain], Id, Options) ->
  #{node := Node, name := Name} = OldDomain,
  Reply = gen_server:call({Name,Node}, {norma, OldDomain, Id, Options}),
  {reply, Reply, S}.
    


%%
% 1. Check on new domain
%   1.1. If true return pid
% 2. Check on old domain
%   2.1. If true, do migrate and return pid
% 3. Start on new domain
merge_(S = #{dms_sup := Sup, child := TplChild = #{start := {M,F,A}}}, _Ds = [_ND, OD], Id, Options) -> 

  Child = fun(OptValue) -> TplChild#{id => Id, start := {M,F,[OptValue|A]}} end,

  OptionsLookUp = #{start => false, init => false},

  Reply =
    case supervisor:start_child(Sup, Child(OptionsLookUp)) of
      {error, {already_started, Pid}} -> {ok, Pid};
      {error, {not_started, _}} -> 
        #{node := Node, name := Name} = OD,
        case gen_server:call({Name, Node}, {norma, OD, Id, OptionsLookUp}) of
          {ok, Pid}   -> %% migrate;
            %% TODO Migrate function: NewPid = migrate(Pid, ND).
            {ok, Pid};
          not_started -> 
            case Options of
              #{start := true} -> 
                  case supervisor:start_child(Sup, Child(Options)) of
                    {ok, Pid}                 -> {ok, Pid};
                    {error, {not_exists,  _}} -> not_exists;
                    Else                      -> {err, {lookup_failure, Else}}
                  end;
              _ -> not_started
            end 
        end;
      Else -> {err, {start_or_lookup_failure, Else}}
    end,

  {reply, Reply, S}.
  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MISC
mk_ch_sp(Module, Name, Type, Restart, Args) ->
  #{id        => Name,
    start     => {Module, start_link, Args#{id => Name}},
    restart   => Restart,
    shutdown  => 5000,
    type      => Type,
    modules   => [Module]}.


mk_ch_sp_domain(Module, Name, Type, Restart, Args) ->
  #{id        => Name,
    start     => {Module, start_link, Args},
    restart   => Restart,
    shutdown  => 5000,
    type      => Type,
    modules   => [Module]}.
