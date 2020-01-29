-module(rjs).
-compile([no_native]).

-on_load(init/0).
-export([encode/1, decode/1, decode/2]).

-include_lib("eunit/include/eunit.hrl").

encode(Data) ->
    nif_encode(Data).

decode(Data) ->
    decode(Data, []).

decode(Bin, Opts) when is_binary(Bin) ->
    nif_decode(Bin, Opts);

decode(Data, Opts) ->
    Bin = erlang:iolist_to_binary(Data),
    nif_decode(Bin, Opts).

init() ->
    PrivDir = code:priv_dir(?MODULE),
    erlang:load_nif(filename:join(PrivDir, "crates/rjs/librjs"), 0).

-define(NOT_LOADED, not_loaded(?LINE)).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_encode(_Data) ->
    ?NOT_LOADED.

nif_decode(_Data, _Opts) ->
    ?NOT_LOADED.

-ifdef(EUNIT).

encode_integer_test() ->
    ?assertEqual({ok, <<"1">>}, encode(1)),
    ?assertEqual({ok, <<"2">>}, encode(2)),
    ?assertEqual({ok, <<"-1">>}, encode(-1)).

encode_float_test() ->
    ?assertEqual({ok, <<"1.0">>}, encode(1.0)),
    ?assertEqual({ok, <<"2.0">>}, encode(2.0)),
    ?assertEqual({ok, <<"-1.0">>}, encode(-1.0)).

encode_string_test() ->
    %% Charlists are encoded as charlists
    ?assertEqual({ok,<<"[104,101,108,108,111]">>}, encode("hello")),
    ?assertEqual({ok, <<"\"world\"">>}, encode(<<"world">>)).

encode_map_test() ->
    ?assertEqual({ok, <<"{}">>}, encode(#{})),
    ?assertEqual({ok, <<"{\"hello\":\"world\"}">>}, encode(#{hello => world})),
    ?assertEqual({ok, <<"{\"1\":2}">>}, encode(#{1 => 2})).

encode_null_test() ->
    ?assertEqual({ok, <<"null">>}, encode(null)).

encode_list_test() ->
    ?assertEqual({ok, <<"[]">>}, encode([])),
    ?assertEqual({ok, <<"[1]">>}, encode([1])),
    ?assertEqual({ok, <<"[1,-1.0]">>}, encode([1, -1.0])).

decode_integer_test() ->
    ?assertEqual({ok, 1}, decode(<<"1">>)),
    ?assertEqual({ok, 2}, decode(<<"2">>)),
    ?assertEqual({ok, -1}, decode(<<"-1">>)).

decode_float_test() ->
    ?assertEqual({ok, 1.0}, decode(<<"1.0">>)),
    ?assertEqual({ok, 2.0}, decode(<<"2.0">>)),
    ?assertEqual({ok, -1.0}, decode(<<"-1.0">>)).

decode_string_test() ->
    ?assertEqual({ok, "hello"}, decode(<<"[104,101,108,108,111]">>)),
    ?assertEqual({ok, <<"world">>}, decode(<<"\"world\"">>)).

decode_null_test() ->
    ?assertEqual({ok, null}, decode(<<"null">>)).

decode_map_test() ->
    ?assertEqual({ok, #{}}, decode(<<"{}">>)),
    ?assertEqual({ok, #{<<"hello">> => <<"world">>}}, decode(<<"{\"hello\":\"world\"}">>)),
    ?assertEqual({ok, #{<<"1">> => 2}}, decode(<<"{\"1\":2}">>)),
    ?assertEqual({ok, #{hello => <<"world">>}}, decode(<<"{\"hello\":\"world\"}">>, [atom])),
    ?assertEqual({ok, #{hello => <<"world">>}}, decode(<<"{\"hello\":\"world\"}">>, [existing_atom])).

-endif.
