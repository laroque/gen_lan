-module(gen_lan_device).

-export([behaviour_info/1,
         send/2,
         process_response/2,
         arg_to_binary/1
       ]).

behaviour_info(callbacks) ->
    [
     {send_command, 2},
     {parse_message, 2}
    ];
behaviour_info(_) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Data, Soc) ->
    ToSend = list_to_binary(["*OPC?;",Data,";*OPC?;*STB?\n"]),
    gen_tcp:send(Soc, ToSend).

process_response(Socket, {[], <<"1">>, _From}) ->
    ok = get_error(Socket),
    {continue, []};
process_response(Socket, {[]=OldData, <<"1;",Rest/binary>>, _From}) ->
    handle_response(Rest, OldData, Socket);
process_response(Socket, {OldData, Resp, _From}) ->
    handle_response(Resp, OldData, Socket);
process_response(_Socket, _Msg) ->
    debug_print("got confusing data"),
    {error, unexpected_message}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_error(Socket) ->
    ToSend = ["SYST:ERR?;*OPC?;*STB?\n"],
    gen_tcp:send(Socket, ToSend).

handle_response(Resp,R,Socket) ->
    Match = binary:match(Resp, [<<"1;+0\n">>,<<"\n">>]),
    case Match of
        nomatch -> % more messages expected
            {continue, list_to_binary([R,Resp])};
        {Pos, 1} -> % error indication from instrument
            NewResp = list_to_binary([R,binary:part(Resp, {0, Pos})]),
            ok = get_error(Socket),
            {continue, NewResp};
        {Pos, 5} -> % complete, no errors
            if Pos =:= 0 ->
                {done, <<"">>};
            true ->
                NewResp = list_to_binary([R,binary:part(Resp, {0, Pos-1})]),
                {done, NewResp}
            end
        end.

arg_to_binary(Arg) ->
    if  is_integer(Arg) ->
            erlang:integer_to_binary(Arg);
        is_float(Arg) ->
            erlang:float_to_binary(Arg);
        true ->
            Arg
    end.

debug_print(Msg) ->
     io:format("\n**************************************************\n"),
     io:format(Msg),
     io:format("\n**************************************************\n").
