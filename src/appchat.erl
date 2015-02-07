-module(appchat).
-export([test/0]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").
-define(HOST, <<"xmpp.erlang.pl">>).
-define(MUC_HOST, <<"muc.xmpp.erlang.pl">>).

test() ->
    application:ensure_all_started(escalus),
    Cfg = user_spec(<<"case">>, <<"res1">>),
    {ok, Client, _, _} = escalus_connection:start(Cfg),
    send_presence_available(Client),
    escalus:send(Client, stanza_muc_enter_room(<<"hackatory">>, <<"case">>)),
    ok.

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
