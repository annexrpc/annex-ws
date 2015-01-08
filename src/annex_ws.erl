-module(annex_ws).

-behaviour(websocket_client_handler).

%% API.
-export([start/0]).
-export([start_link/1]).
-export([call/6]).
-export([cast/4]).
-export([stop/1]).

%% websocket_client_handler

-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-record(state, {
  marshal,
  msg_id = 0,
  requests = #{}
}).

% http://www.erlang.org/doc/efficiency_guide/advanced.html
-define(MAX_NUM, 134217728).

start() ->
  {ok, _} = application:ensure_all_started(annex_ws),
  ok.

start_link(Opts) ->
  verify_args([marshal, url], Opts),
  websocket_client:start_link(fast_key:get(url, Opts), ?MODULE, Opts).

verify_args([], _) ->
  ok;
verify_args([Arg|Args], Opts) ->
  case fast_key:get(Arg, Opts) of
    undefined ->
      exit({missing_arg, Arg});
    _ ->
      verify_args(Args, Opts)
  end.

call(Pid, Module, Function, Arguments, Sender, Ref) ->
  Pid ! {call, Module, Function, Arguments, Sender, Ref},
  ok.

cast(Pid, Module, Function, Arguments) ->
  Pid ! {cast, Module, Function, Arguments},
  ok.

stop(Pid) ->
  Pid ! stop.

init(Opts, _State) ->
  %% TODO auth
  case fast_key:get(marshal, Opts) of
    undefined ->
      exit(missing_marshal);
    Marshal ->
      {ok, #state{marshal = Marshal}}
  end.

websocket_handle({pong, _}, _ConnState, State) ->
  {ok, State};
websocket_handle({binary, Bin}, _, State = #state{marshal = Marshal}) ->
  case catch Marshal:decode(Bin) of
    {ok, MsgID, Res} ->
      handle_response(MsgID, {ok, Res}, State);
    {error, MsgID, Res} ->
      handle_response(MsgID, {error, Res}, State);
    Error ->
      error_logger:error_msg("Unable to decode response ~p~n~p~n", [Bin, Error]),
      {ok, State}
  end;
websocket_handle(Other, _, State) ->
  io:format("OTHER ~p~n", [Other]),
  {ok, State}.

websocket_info(start, _ConnState, State) ->
  {ok, State};
websocket_info({call, Module, Function, Arguments, Sender, Ref}, _, State = #state{marshal = Marshal, msg_id = MsgID, requests = Reqs}) ->
  %% TODO save sender and ref under msgid
  Out = Marshal:encode(MsgID, call, Module, Function, Arguments),
  {reply, {binary, Out}, State#state{requests = maps:put(MsgID, {Sender, Ref}, Reqs), msg_id = (MsgID + 1) rem ?MAX_NUM}}.

websocket_terminate(Reason, _ConnState, State) ->
  io:format("Websocket closed in state ~p wih reason ~p~n",
            [State, Reason]),
  ok.

handle_response(MsgID, Msg, State = #state{requests = Reqs}) ->
  case maps:find(MsgID, Reqs) of
    {ok, {Sender, Ref}} ->
      Sender ! {Ref, Msg},
      {ok, State#state{requests = maps:remove(MsgID, Reqs)}};
    error ->
      io:format("undefined message id ~p~n", [MsgID]),
      {ok, State}
  end.
