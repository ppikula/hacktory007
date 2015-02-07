-module(appchat_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

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


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    application:ensure_all_started(escalus),
    Cfg = user_spec(<<"case">>, <<"res1">>),
    {ok, Client, _, _} = escalus_connection:start(Cfg),
    AllowMessage = fun escalus_pred:is_groupchat_message/1,
   % escalus_connection:set_filter_predicate(Client, AllowMessage),
    send_presence_available(Client),
    escalus:send(Client, stanza_muc_enter_room(<<"hackatory">>, <<"case">>)),
    GC = escalus_stanza:groupchat_to(<<"hackatory@muc.xmpp.erlang.pl">>, <<"test">>),
    io:format("~p", [GC]),
    escalus:send(Client, GC),
    %%Allow presence stanza only
    {ok, #state{client=Client}}.

handle_call({send_message, M}, From, State) ->
    {reply, ok, State};
handle_call({receive_message, M}, From, State) ->
    {reply, ok, State};
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
