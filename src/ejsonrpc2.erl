-module(ejsonrpc2).

-export([ make_call/4
        , make_notification/3
        , handle_call/4
        , handle_call/5
        , handle_reply/3
        , handle_reply/4
        ]).

-type positional_params() :: list(term()).
-type named_params() :: map().
-type string_type() :: string() | binary() | atom().
-type params_type() :: positional_params() | named_params() | undefined.
-type id_type() :: string() | integer().
-type reply_type() :: term().

-type encoder_fn() :: fun((map()) -> binary()).
-type decoder_fn() :: fun((binary()) -> map()).
-type call_handler_fn() :: fun((params_type()) -> any()).
-type reply_handler_fn() :: fun((id_type(), reply_type()) -> any()).
-type call_mapper_fn() :: fun((call_handler_fn(), list(params_type())) -> list(any())).
-type reply_mapper_fn() :: fun((reply_handler_fn(), list(reply_type())) -> list(any())).

-define(FLAT_FORMAT(Format, Data), lists:flatten(io_lib:format(Format, Data))).

-define(PARSE_ERROR_C, -32700).
-define(INVALID_REQUEST_C, -32600).
-define(METHOD_NOT_FOUND_C, -32601).
-define(INVALID_PARAMS_C, -32602).
-define(INTERNAL_ERROR_C, -32603).

-define(PARSE_ERROR_M, "Parse error").
-define(INVALID_REQUEST_M, "Invalid Request").
-define(METHOD_NOT_FOUND_M, "Method not found").
-define(INVAID_PARAMS_M, "Invalid params").
-define(INTERNAL_ERROR_M, "Internal error").

-define(ERROR_MAP(Code, Message), #{code => Code, message => Message}).
-define(ERROR_MAP(Code, Message, Data), #{code => Code, message => Message, data => Data}).

-ifdef(JSONRPC_DEBUG).
-define(PARSE_ERROR(Reply, Format, Data), maps:put(error, ?ERROR_MAP(?PARSE_ERROR_C, ?PARSE_ERROR_M, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(PARSE_ERROR(Reply, _Format, _Data), maps:put(error, ?ERROR_MAP(?PARSE_ERROR_C, ?PARSE_ERROR_M), Reply)).
-endif.

-ifdef(JSONRPC_DEBUG).
-define(INVALID_REQUEST(Reply, Format, Data), maps:put(error, ?ERROR_MAP(?INVALID_REQUEST_C, ?INVALID_REQUEST_M, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(INVALID_REQUEST(Reply, _Format, _Data), maps:put(error, ?ERROR_MAP(?INVALID_REQUEST_C, ?INVALID_REQUEST_M), Reply)).
-endif.

-ifdef(JSONRPC_DEBUG).
-define(METHOD_NOT_FOUND(Reply, Format, Data), maps:put(error, ?ERROR_MAP(?METHOD_NOT_FOUND_C, ?METHOD_NOT_FOUND_M, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(METHOD_NOT_FOUND(Reply, _Format, _Data), maps:put(error, ?ERROR_MAP(?METHOD_NOT_FOUND_C, ?METHOD_NOT_FOUND_M), Reply)).
-endif.

-ifdef(JSONRPC_DEBUG).
-define(INVALID_PARAMS(Reply, Format, Data), maps:put(error, ?ERROR_MAP(?INVALID_PARAMS_C, ?INVALID_PARAMS_M, Data, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(INVALID_PARAMS(Reply, _Format, _Data), maps:put(error, ?ERROR_MAP(?INVALID_PARAMS_C, ?INVALID_PARAMS_M), Reply)).
-endif.

-ifdef(JSONRPC_DEBUG).
-define(INTERNAL_ERROR(Reply, Format, Data), maps:put(error, ?ERROR_MAP(?INTERNAL_ERROR_C, ?INTERNAL_ERROR_M, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(INTERNAL_ERROR(Reply, _Format, _Data), maps:put(error, ?ERROR_MAP(?INTERNAL_ERROR_C, ?INTERNAL_ERROR_M), Reply)).
-endif.

-spec make_call(Method::string_type(), Params::params_type(), Id::id_type(), Encode::encoder_fn()) -> binary().
make_call(Method, Params, Id, Encode) when is_map(Params) orelse is_list(Params) ->
  Call = #{ jsonrpc => "2.0"
          , method  => Method
          , params  => Params
          , id      => Id
          },
  Encode(Call);

make_call(Method, undefined, Id, Encode) ->
  Call = #{ jsonrpc => "2.0"
          , method  => Method
          , id      => Id
          },
  Encode(Call);

make_call(_Method, _Params, _Id, _Encode) ->
  throw({badarg, params_arg_incorrect_type}).

-spec make_notification(Method::string_type(), Params::params_type(), Encode::encoder_fn()) -> binary().
make_notification(Method, Params, Encode) when is_map(Params) orelse is_list(Params) ->
  Call = #{ jsonrpc => "2.0"
          , method  => Method
          , params  => Params
          },
  Encode(Call);

make_notification(Method, undefined, Encode) ->
  Call = #{ jsonrpc => "2.0"
          , method  => Method
          },
  Encode(Call);

make_notification(_Method, _Params, _Encode) ->
  throw({badarg, params_arg_incorrect_type}).

-spec handle_call(Payload::binary(), Handler::call_handler_fn(), Decode::decoder_fn(), Encode::encoder_fn()) -> binary().
handle_call(Payload, Handler, Decode, Encode) ->
  handle_call(Payload, Handler, Decode, Encode, fun pmap/2).

-spec handle_call(Payload::binary(), Handler::call_handler_fn(), Decode::decoder_fn(), Encode::encoder_fn(), MapFn::call_mapper_fn()) -> binary().
handle_call(Payload, Handler, Decode, Encode, MapFn) ->
  ResponseStub = #{jsonrpc => "2.0"},
  Response = try Decode(Payload) of
               Decoded when is_list(Decoded)->
                 Responses = MapFn(fun(Req) ->
                                       wrapper(Req, ResponseStub, Handler)
                                   end, Decoded),
                 [ Response || Response <- Responses, maps:get(id, Response) /= null ];
               Decoded when is_map(Decoded) ->
                 case wrapper(Decoded, ResponseStub, Handler) of
                   #{id := Id} = Resp when Id /= null ->
                     Resp;
                   _otherwise -> []
                 end;
               _otherwise ->
                 PreResponse = maps:put(id, null, ResponseStub),
                 ?INVALID_REQUEST(PreResponse, "Request must be object or list of objects", [])
             catch
               Error ->
                ?PARSE_ERROR(ResponseStub, "~p", [Error])
             end,
  case Response of
    [] ->
      ignore;
    Response ->
      Encode(Response)
  end.

-spec handle_reply(Payload::binary(), Handler::reply_handler_fn(), Decode::decoder_fn()) -> list(term()).
handle_reply(Payload, Handler, Decode) ->
  Decode().

-spec handle_reply(Payload::binary(), Handler::reply_handler_fn(), Decode::decoder_fn(), MapFn::reply_mapper_fn()) -> list(any()).
handle_reply(Payload, Handler, Decode, MapFn) ->
  Decode().

%% Internal staff

wrapper(Req, ResponseStub, Hndlr) ->
  try
    begin
      Id = maps:get(<<"id">>, Req, null),
      PreResponse = maps:put(id, Id, ResponseStub),

      try map:get(<<"jsonrpc">>, Req) of
        Version when Version /= <<"2.0">> ->
          throw(?INVALID_REQUEST(PreResponse, "Value of 'jsonrpc' key must be \"2.0\"", []))
      catch
        error:{badkey, _} ->
          throw(?INVALID_REQUEST(PreResponse, "Request must contain 'jsornpc' key", []))
      end,

      Method = try maps:get(<<"method">>, Req) of
                 M when not is_binary(M) ->
                   throw(?INVALID_REQUEST(PreResponse, "Value of 'method' key must be a string", []));
                 M -> M
               catch
                 error:{badkey, _} ->
                   throw(?INVALID_REQUEST(PreResponse, "Request must contain 'method' key", []))
               end,

      Params = maps:get(<<"params">>, Req, []),

      try Hndlr(Method, Params) of
        method_not_found ->
          ?METHOD_NOT_FOUND(PreResponse, "Method ~p not found on this server", [Method]);
        Result ->
          maps:put(result, Result, PreResponse)
      catch
        HndlrError ->
          ?INTERNAL_ERROR(PreResponse, "~p", [HndlrError])
      end
    end
  catch
    throw:ErrorResponse ->
      ErrorResponse
  end.

pmap(Fun, List) ->
  Parent = self(),
  Pids = [ spawn(fun() -> Parent ! {self(), (catch Fun(Item))} end) || Item <- List ],
  gather(Pids).

gather([H | T]) ->
  receive
    {H, Ret} -> [Ret | gather(T)]
  end;

gather([]) -> [].

