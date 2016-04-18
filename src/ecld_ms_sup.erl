%%%-------------------------------------------------------------------
%% @doc Domains members subervisor.
%% @end
%%%-------------------------------------------------------------------

-module('ecld_ms_sup').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

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


