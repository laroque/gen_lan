-module(ethernet_instrument_server).

-behaviour(gen_server).

%% API
-export([start_link/4,
         send_cmd/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name,
                ip,
                port,
                module,
                sockets}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Address, Port, Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Name, Address, Port, Module], []).

send_cmd(Name, Cmd) ->
    gen_server:call(Name, Cmd).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([DeviceName, Address, Port, Module]) ->
    {ok, #state{name=DeviceName,
                ip=Address,
                port=Port,
                module=Module,
                sockets=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({N, Cmd}, From, #state{name=N,
                                   ip=IP,
                                   port=Port,
                                   module=M,
                                   sockets=Sockets}=State) ->
    io:format("\nGot a call\n"),
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {socket, 0}]),
    NewSockets = lists:append(Sockets, {Socket, [], From}),
    ok = M:send_command(Cmd, Socket),
    {noreply, State#state{sockets=NewSockets}};
handle_call(_Request, _From, State) ->
    io:format("\nGot unexpected call\n"),
    {reply, call_unimplemented, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {stop, cast_unimplemented, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, #state{sockets=Socs, module=M}=State) ->
    S = lists:keyfind(Socket, 1, Socs),
    case S of
        false ->
            {stop, socket_not_found, State};
        {Socket, Resp, From} ->
            NewResp = M:parse_message(Socket, {Resp, Data, From}),
            {noreply, done_check(Socket, NewResp, State)};
        _ ->
            {stop, malformed_state_entry, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
done_check(Socket, {continue, NewResponse}, #state{sockets=Socs}=State) ->
    {Socket, _Resp, From} = lists:keyfind(Socket, 1, Socs),
    S = list:keyreplace(Socket, 1, Socs, {Socket, NewResponse, From}),
    State#state{sockets=S};
done_check(Socket, {done, NewResponse}, #state{sockets=Socs}=State) ->
    {Socket, _Resp, From} = lists:keyfind(Socket, 1, Socs),
    From ! NewResponse,
    gen_tcp:close(Socket),
    State#state{sockets=lists:keydelete(Socket, 1, Socs)}.
