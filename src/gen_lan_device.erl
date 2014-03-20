-module(gen_lan_device).

-export([behaviour_info/1,
         send/2,
         process_response/2
       ]).

behaviour_info(callbacks) ->
    [
     {send_command, 2}
    ];
behaviour_info(_) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Data, Soc) ->
    ToSend = <<"*OPC?;",Data,"*OPC?;*STB?\n">>,
    gen_tcp:send(Soc, ToSend).

process_response(_Message, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
