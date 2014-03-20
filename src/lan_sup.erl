
-module(lan_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("initting\r\n"),
    Dmm = {ethernet_instrument_server,
           {ethernet_instrument_server, start_link, [dmm, "10.0.0.60", "5025", agilent_34460]},
           permanent, 5000, worker, [ethernet_instrument_server]},
    {ok, { {one_for_one, 5, 10}, [Dmm]} }.

