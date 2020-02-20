-module(prop_call).

-include_lib("proper/include/proper.hrl").

-export([ 'prop__jsonrpc_version_in_request_must_be_2.0'/0
        , 'prop__method_must_always_be_present'/0
        , 'prop__params_if_present_must_not_be_empty'/0
        , 'prop__id_must_be_null_or_string_or_number'/0
        , 'prop__jsonrpc_version_in_response_must_be_2.0'/0
        , 'prop__response_id_must_equal_request_id'/0
        ]).

'prop__jsonrpc_version_in_request_must_be_2.0'() ->
  ?FORALL({Method, Params, Id}, correct_request(),
         begin
           Call = ejsonrpc2:call(Method, Params, Id, fun jsone:encode/1),
           Decoded = jsone:decode(Call),
           <<"2.0">> == maps:get(<<"jsonrpc">>, Decoded)
         end).

'prop__method_must_always_be_present'() ->
  ?FORALL({Method, Params, Id}, correct_request(),
         begin
           Call = ejsonrpc2:call(Method, Params, Id, fun jsone:encode/1),
           Decoded = jsone:decode(Call),
           not_found /= maps:get(<<"id">>, Decoded, not_found)
         end).

 'prop__params_if_present_must_not_be_empty'() ->
  ?FORALL({Method, Params, Id}, correct_request(),
         begin
           Call = ejsonrpc2:call(Method, Params, Id, fun jsone:encode/1),
           Decoded = jsone:decode(Call),
           not_found == maps:get(<<"params">>, Decoded, not_found) %% 'params' might be absent
           orelse begin
             Par = maps:get(<<"params">>, Decoded), %% but if present
             Par /= [] andalso Par /= #{} %% 'params' must not be empty list or map
           end
         end).

'prop__id_must_be_null_or_string_or_number'() ->
  ?FORALL({Method, Params, Id}, correct_request(),
         begin
           Call = ejsonrpc2:call(Method, Params, Id, fun jsone:encode/1),
           Decoded = jsone:decode(Call),
           Id_ = maps:get(<<"id">>, Decoded, not_found),
           Id_ == null orelse is_binary(Id_) orelse is_number(Id_)
         end).

'prop__jsonrpc_version_in_response_must_be_2.0'() ->
  ?FORALL({Method, Params, Id}, correct_request(),
         begin
           Call = ejsonrpc2:call(Method, Params, Id, fun jsone:encode/1),
           Response = ejsonrpc2:handle_call_or_notification(Call, fun call_handler/3, fun jsone:decode/1, fun jsone:encode/1),
           Decoded = jsone:decode(Response),
           <<"2.0">> == maps:get(<<"jsonrpc">>, Decoded, not_found)
         end).

'prop__response_id_must_equal_request_id'() ->
  ?FORALL({Method, Params, Id}, correct_request(),
         begin
           Call = ejsonrpc2:call(Method, Params, Id, fun jsone:encode/1),
           Response = ejsonrpc2:handle_call_or_notification(Call, fun call_handler/3, fun jsone:decode/1, fun jsone:encode/1),
           Decoded = jsone:decode(Response),
           ResponseId = maps:get(<<"id">>, Decoded, not_found),
           io:format(">>> ~p == ~p\n~p\n~p\n\n\n", [Id, ResponseId, Call, Response]),
           Id == ResponseId
         end).

%% -----------------------------
%% Helpers
%% -----------------------------

call_handler(Id, Method, Params) ->
  ok.

correct_request() ->
  Method = binary_string(),
  Params = oneof([array(), object()]),
  Id = oneof([null, binary_string(), js_number()]),
  {Method, Params, Id}.

value() ->
  String = ?LAZY(binary_string()),
  Number = ?LAZY(js_number()),
  Object = ?LAZY(object()),
  Array = ?LAZY(array()),
  oneof([String, Number, Object, Array, true, false, null]).

js_number() ->
  MaxSafeInteger = (1 bsl 53) - 1,
  MinSafeInteger = -MaxSafeInteger,
  integer(MinSafeInteger, MaxSafeInteger).

array() ->
  list(?LAZY(value())).

binary_string() ->
  ?LET(S, string(), unicode:characters_to_nfc_binary(S)).

object() ->
  ?LET(L, list({binary_string(), value()}), maps:from_list(L)).