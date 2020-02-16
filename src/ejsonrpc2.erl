-module(ejsonrpc2).

-export([ make_call/3
        , handle_call/4
        , handle_call/5
        , handle_reply/3
        , handle_reply/4
        ]).

-type positional_params() :: list(term()).
-type named_params() :: map().
-type string_type() :: string() | binary() | atom().
-type params_type() :: positional_params() | named_params().
-type reply_type() :: term().

-type encoder_fn() :: fun((map()) -> binary()).
-type decoder_fn() :: fun((binary()) -> map()).
-type call_handler_fn() :: fun((params_type()) -> any()).
-type reply_handler_fn() :: fun((reply_type()) -> any()).
-type call_mapper_fn() :: fun((list(params_type())) -> list(any())).
-type reply_mapper_fn() :: fun((list(reply_type())) -> list(any())).

-spec make_call(Method::string_type(), Params::params_type(), Encode::encoder_fn()) -> binary().
make_call(Method, Params, Encode) ->
  Encode().

-spec handle_call(Payload::binary(), Handler::call_handler_fn(), Decode::decoder_fn(), Encode::encoder_fn()) -> binary().
handle_call(Payload, Handler, Decode, Encode) ->
  Encode().

-spec handle_call(Payload::binary(), Handler::call_handler_fn(), Decode::decoder_fn(), Encode::encoder_fn(), MapFn::call_mapper_fn()) -> binary().
handle_call(Payload, Handler, Decode, Encode, MapFn) ->
  Encode().

-spec handle_reply(Payload::binary(), Handler::reply_handler_fn(), Decode::decoder_fn()) -> list(term()).
handle_reply(Payload, Handler, Decode) ->
  Decode().

-spec handle_reply(Payload::binary(), Handler::reply_handler_fn(), Decode::decoder_fn(), MapFn::reply_mapper_fn()) -> list(any()).
handle_reply(Payload, Handler, Decode, MapFn) ->
  Decode().
