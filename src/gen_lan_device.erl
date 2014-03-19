-module(gen_lan_device).

-export([behavior_info/1,
         connect/1,
         send_command/2,
         process_response/2,
         close/1
       ]).

-include("device_state.hrl").

behavior_info(callbacks) ->
    [];
behavior_info(_) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec connect(tuple()) -> ok.
connect(#device_state{dev_name=Name,dev_ip=IP,dev_port=Port}=State) ->
    {ok, Soc} = gen_tcp:connect(IP, Port, [binary, {socket, 0}]),
    {ok, State#device_state{socket=Soc}}.

-spec send_command(atom(), iolist()) -> ok.
send_command(Data, #device_state{socket=Soc}=State) ->
    ToSend = <<"*OPC?;",Data,"*OPC?;*STB?",T>>,
    gen_tcp:send(Soc, ToSend),
    ok.

-spec proces_response(iolist(), tuple()) -> ok.
process_response(_Message, _State) ->
    ok.

-spec close(tuple()) -> ok.
close(#device_state{socket=Soc}=State) ->
    gen_tcp:close(Soc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pack_command(Data, #device_state{socket=Soc, term_sequence=T}=State) ->
