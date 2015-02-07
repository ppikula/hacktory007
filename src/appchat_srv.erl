-module(appchat_srv).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {client :: term()}).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-define(HOST, <<"xmpp.erlang.pl">>).
-define(MUC_HOST, <<"muc.xmpp.erlang.pl">>).

-export([send/1, receive_msg/0]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Nick) ->
    %gen_server:start_link({local, ?MODULE}, ?MODULE, [Nick], []).
    F = fun () ->
                init([Nick])
        end,
    Pid =  spawn_link(F),
    register(?MODULE,Pid),
    {ok, Pid}.


send(Msg) ->
    ?MODULE ! {send_message, Msg}.

receive_msg() ->
    ?MODULE ! {receive_msg, 1, self()},
    receive
        R ->
            R
    after 5000 ->
            nomsg
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Nick]) ->
    application:ensure_all_started(escalus),
    Cfg = user_spec(Nick, <<"res1">>),
    {ok, Client, _, _} = escalus_connection:start(Cfg),
    send_presence_available(Client),
    escalus:send(Client, stanza_muc_enter_room(<<"hackatory">>, Nick)),
    loop(Client).
    %%Allow presence stanza only
    %{ok, #state{client=Client}}.

loop(Client) ->
    receive
        {send_message, M} ->
            GC = escalus_stanza:groupchat_to(<<"hackatory@muc.xmpp.erlang.pl">>, M),
            escalus:send(Client, GC);
        {receive_msg, C, From} ->
            S = escalus_client:wait_for_stanzas(Client, 1, 4000),
            From ! wait_for_message(Client, S)
     end,
    loop(Client).

handle_call({send_message, M}, From, #state{client=Client} = State) ->
    GC = escalus_stanza:groupchat_to(<<"hackatory@muc.xmpp.erlang.pl">>, M),
    escalus:send(Client, GC),
    {reply, ok, State};
handle_call({receive_message, Count}, From, #state{client = Client} =  State) ->
    S = escalus_client:wait_for_stanzas(Client, 1, 4000),
    {reply, S, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
wait_for_message(_Client, [#xmlel{name = <<"message">>} = Stanza]) ->
    Stanza;
wait_for_message(_, []) -> nomsg;
wait_for_message(Client, _ ) ->
    S = escalus_client:wait_for_stanzas(Client, 1),
    wait_for_message(Client, S).


user_spec(ProfileId, Res) ->
    [ {username, ProfileId},
      {server, ?HOST},
      {host, ?HOST},
      {password, <<"iloveerlang">>},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ].

send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

%Basic MUC protocol
stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlel{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
        Room, Nick).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

room_address(Room) ->
    <<Room/binary, "@", ?MUC_HOST/binary>>.

room_address(Room, Nick) ->
    <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>.
