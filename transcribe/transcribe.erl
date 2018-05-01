-module(transcribe).
-export([msg/1,start/0]).

msg(Msg) ->
	gen_server:call({global,transcribe_worker},{publish_msg,Msg}).

start() ->
	Pid = erlang:spawn(fun() -> transcribe() end),
	monitor(process,Pid),
	receive
		{'DOWN',_Ref,process,
                          _Pid,_} ->
			start();
		Error ->
			io:format("This is a different Exception received which is ~p~n",[Error])
	end.

transcribe() ->
	net_kernel:connect('phoenix_speak_up@127.0.0.1'),
	{ok,P}=python:start(),
	python:call(P,transcribe_streaming_mic,main,[]).
