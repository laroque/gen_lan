-module(agilent_34460).

-behaviour(gen_lan_device).

-export([
        send_command/2,
        parse_message/2
       ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Required callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_command({raw, Value}, Socket) ->
    gen_lan_device:send(Value, Socket);
send_command({read}, Socket) ->
    gen_lan_device:send("READ?", Socket);
send_command({status}, Socket) ->
    gen_lan_device:send("*STB?", Socket);
send_command({trig_count}, Socket) ->
    gen_lan_device:send("TRIG:COUN?", Socket);
send_command({trig_count, Value}, Socket) ->
    V = gen_lan_device:arg_to_binary(Value),
    gen_lan_device:send(["TRIG:COUN ",V], Socket);
send_command({trig_sour}, Socket) ->
    gen_lan_device:send("TRIG:SOUR?", Socket);
send_command({trig_sour, Value}, Socket) ->
    V = gen_lan_device:arg_to_binary(Value),
    gen_lan_device:send(["TRIG:SOUR ",V], Socket);
send_command({_Cmd}, _Socket) ->
    {error, command_unknown}.

parse_message(Socket, Msg) ->
    gen_lan_device:process_response(Socket, Msg).
