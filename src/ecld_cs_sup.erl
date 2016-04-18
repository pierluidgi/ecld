%%%-------------------------------------------------------------------
%% @doc Domains controllers supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('ecld_cs_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================


%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

  SupFlags = #{
      strategy  => one_for_all,
      intensity => 5,
      period    => 10}, %% Seconds

  Sup = {SupFlags, []},

  {ok, Sup}.


