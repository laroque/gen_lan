-module(agilent_34460).

-behaviour(gen_lan_device).

-export([
        send_command/2,
        parse_message/2
       ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Required callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_command({status}, Socket) ->
    gen_lan_device:send("*STB?", Socket);
send_command({trig_count}, Socket) ->
    gen_lan_device:send("TRIG:COUN?", Socket);
send_command({trig_count, Value}, Socket) ->
    gen_lan_device:send_command(lists:flatten(["TRIG:COUN ",Value]), Socket).

parse_message(Socket, Msg) ->
    gen_lan_device:process_response(Socket, Msg).
