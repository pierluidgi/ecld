%%%-------------------------------------------------------------------
%% @doc domain controller supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('ecld_c_sup').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


-define(DC_MODULE, ecld_c).
-define(DM_MODULE, ecld_ms_sup).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% 
init([]) ->
  SupFlags = #{
      strategy  => one_for_all,
      intensity => 5, 
      period    => 10}, %% Seconds

  Sup = {SupFlags, []},

  {ok, Sup}.

