-module(gen_lan_device).

-export([behaviour_info/1,
         send/2,
         process_response/2
       ]).

behaviour_info(callbacks) ->
    [
     {send_command, 2},
     {parse_message, 1}
    ];
behaviour_info(_) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Data, Soc) ->
    ToSend = <<"*OPC?;",Data,"*OPC?;*STB?\n">>,
    gen_tcp:send(Soc, ToSend).

process_response(Socket, {[], <<"1">>, _From}) ->
    ok = get_error(Socket),
    {continue, []};
%process_response(Socket, {[], <<"1;",Rest/binary>>, From}) ->
%    handle_response(Rest, State);

process_response(_Socket, {_OldResp, _NewData, _From}) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_error(Socket) ->
    ToSend = ["SYST:ERR?;*OPC?;*STB?\n"],
    gen_tcp:send(Socket, ToSend).

%handle_info({tcp, Socket, <<"1;",Rest/binary>>}, #state{socket=Socket, response=[]}=State) ->
%                io:format("info 2\n"),
%                    NewState = handle_response(Rest, State),
%                        {noreply, NewState};
%                        handle_info({tcp, Socket, Resp}, #state{socket=Socket}=State) ->
%                            io:format("info 3\n"),
%                                io:format(State#state.response),
%                                    NewState = handle_response(Resp, State),
%                                        {noreply, NewState};
%                                        handle_info(_Info, State) ->
%                                            io:format("info 4 (error)\n"),
%                                                {noreply, State}.
