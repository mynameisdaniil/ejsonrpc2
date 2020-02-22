-module(ejsonrpc2).

-export([ call/2
        , call/4
        , notification/3
        , handle_call_or_notification/4
        , handle_call_or_notification/5
        , handle_response/3
        , handle_response/4
        ]).

-type positional_params() :: list(term()).
-type named_params()      :: map().
-type string_type()       :: string() | binary() | atom().
-type params_type()       :: positional_params() | named_params() | undefined.
-type id_type()           :: string() | integer().
-type reply_type()        :: {reply, term()}.
-type reply_error()       :: {error, {parse_error, any()}
                                   | {invalid_response, string()
                                   | {call_error, #{binary() => any()}}}}.

-type encoder_fn()       :: fun((map())                                   -> binary()).
-type decoder_fn()       :: fun((binary())                                -> map()).
-type call_handler_fn()  :: fun((id_type(), string(), params_type())      -> any()).
-type reply_handler_fn() :: fun((id_type(), reply_type() | reply_error()) -> any()).
-type call_mapper_fn()   :: fun((call_handler_fn(), list(params_type()))  -> list(any())).
-type reply_mapper_fn()  :: fun((reply_handler_fn(), list(reply_type()))  -> list(any())).

-define(FLAT_FORMAT(Format, Data), lists:flatten(io_lib:format(Format, Data))).

-define(JSONRPC, <<"2.0">>).

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
-define(PARSE_ERROR(Reply, Format, Data),
        maps:put(error, ?ERROR_MAP(?PARSE_ERROR_C, ?PARSE_ERROR_M, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(PARSE_ERROR(Reply, _Format, _Data),
        maps:put(error, ?ERROR_MAP(?PARSE_ERROR_C, ?PARSE_ERROR_M), Reply)).
-endif.

-ifdef(JSONRPC_DEBUG).
-define(INVALID_REQUEST(Reply, Format, Data),
        maps:put(error, ?ERROR_MAP(?INVALID_REQUEST_C, ?INVALID_REQUEST_M, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(INVALID_REQUEST(Reply, _Format, _Data),
        maps:put(error, ?ERROR_MAP(?INVALID_REQUEST_C, ?INVALID_REQUEST_M), Reply)).
-endif.

-ifdef(JSONRPC_DEBUG).
-define(METHOD_NOT_FOUND(Reply, Format, Data),
        maps:put(error, ?ERROR_MAP(?METHOD_NOT_FOUND_C, ?METHOD_NOT_FOUND_M, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(METHOD_NOT_FOUND(Reply, _Format, _Data),
        maps:put(error, ?ERROR_MAP(?METHOD_NOT_FOUND_C, ?METHOD_NOT_FOUND_M), Reply)).
-endif.

-ifdef(JSONRPC_DEBUG).
-define(INVALID_PARAMS(Reply, Format, Data),
        maps:put(error, ?ERROR_MAP(?INVALID_PARAMS_C, ?INVALID_PARAMS_M, Data, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(INVALID_PARAMS(Reply, _Format, _Data),
        maps:put(error, ?ERROR_MAP(?INVALID_PARAMS_C, ?INVALID_PARAMS_M), Reply)).
-endif.

-ifdef(JSONRPC_DEBUG).
-define(INTERNAL_ERROR(Reply, Format, Data),
        maps:put(error, ?ERROR_MAP(?INTERNAL_ERROR_C, ?INTERNAL_ERROR_M, ?FLAT_FORMAT(Format, Data)), Reply)).
-else.
-define(INTERNAL_ERROR(Reply, _Format, _Data),
        maps:put(error, ?ERROR_MAP(?INTERNAL_ERROR_C, ?INTERNAL_ERROR_M), Reply)).
-endif.


-spec call( Batch::list({ Method::string_type()
                        , Params::params_type()
                        , Id::id_type()})
          , Encode::encoder_fn()
          ) -> binary().
call( Batch
    , Encode) ->
  Encode([ make_call(Method, Params, Id) || {Method, Params, Id} <- Batch ]).

-spec call( Method::string_type()
          , Params::params_type()
          , Id::id_type()
          , Encode::encoder_fn()
          ) -> binary().
call( Method
     , Params
     , Id
     , Encode
    ) ->
  CallObj = make_call(Method, Params, Id),
  % io:format(">>>\n~p\n", [CallObj]),
  Encode(CallObj).

-spec notification( Method::string_type()
                  , Params::params_type()
                  , Encode::encoder_fn()
                  ) -> binary().
notification( Method
            , Params
            , Encode
            ) ->
  Encode(make_notification(Method, Params)).

-spec handle_call_or_notification( Payload::binary()
                                 , Handler::call_handler_fn()
                                 , Decode::decoder_fn()
                                 , Encode::encoder_fn()
                                 ) -> binary().
handle_call_or_notification( Payload
                           , Handler
                           , Decode
                           , Encode
                           ) ->
  handle_call_or_notification(Payload, Handler, Decode, Encode, fun pmap/2).

-spec handle_call_or_notification( Payload::binary()
                                 , Handler::call_handler_fn()
                                 , Decode::decoder_fn()
                                 , Encode::encoder_fn()
                                 , MapFn::call_mapper_fn()
                                 ) -> binary().
handle_call_or_notification( Payload
                           , Handler
                           , Decode
                           , Encode
                           , MapFn
                           ) ->
  ResponseStub = #{jsonrpc => ?JSONRPC, id => null},
  Response = try Decode(Payload) of
               Decoded when is_list(Decoded)->
                 Responses = MapFn(fun(Req) ->
                                       call_or_notification_handler_wrapper(Req, ResponseStub, Handler)
                                   end, Decoded),
                 [ Response || Response <- Responses, maps:get(id, Response) /= notification ];
               Decoded when is_map(Decoded) ->
                 case call_or_notification_handler_wrapper(Decoded, ResponseStub, Handler) of
                   #{id := Id} = Resp when Id /= notification ->
                     Resp;
                   _otherwise -> []
                 end;
               Decoded ->
                 ?INVALID_REQUEST(ResponseStub, "Request must be object or list of objects, [~p] were given", [Decoded])
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

-spec handle_response( Payload::binary()
                     , Handler::reply_handler_fn()
                     , Decode::decoder_fn()
                     ) -> list(term() | reply_error()).
handle_response( Payload
               , Handler
               , Decode
               ) ->
  handle_response(Payload, Handler, Decode, fun pmap/2).

-spec handle_response( Payload::binary()
                     , Handler::reply_handler_fn()
                     , Decode::decoder_fn()
                     , MapFn::reply_mapper_fn()
                     ) -> list(term() | reply_error()).
handle_response( Payload
               , Handler
               , Decode
               , MapFn
               ) ->
  try Decode(Payload) of
    Decoded when is_list(Decoded) ->
      MapFn(fun(Response) ->
                response_handler_wrapper(Response, Handler)
            end, Decoded);
    Decoded when is_map(Decoded) ->
      [response_handler_wrapper(Decoded, Handler)];
    Decoded ->
      [{error,
        {invalid_response,
         ?FLAT_FORMAT("Response must be object or list of objects, [~p] were given", [Decoded])}}]
  catch
    Error ->
      [{error, {parse_error, Error}}]
  end.

%% --------------------------------------
%% Internals
%% --------------------------------------

make_call(Method, Params, Id) when Params == [] orelse Params == #{} orelse Params == undefined ->
  Call = #{ jsonrpc => ?JSONRPC
          , method  => Method
          , id      => Id
          },
  Call;

make_call(Method, Params, Id) when is_map(Params) orelse is_list(Params) ->
  Call = #{ jsonrpc => ?JSONRPC
          , method  => Method
          , params  => Params
          , id      => Id
          },
  Call;

make_call(_Method, _Params, _Id) ->
  throw({badarg, params_arg_incorrect_type}).


make_notification(Method, Params) when is_map(Params) orelse is_list(Params) ->
  Call = #{ jsonrpc => ?JSONRPC
          , method  => Method
          , params  => Params
          },
  Call;

make_notification(Method, Params) when Params == undefined orelse Params == [] ->
  Call = #{ jsonrpc => ?JSONRPC
          , method  => Method
          },
  Call;

make_notification(_Method, _Params) ->
  throw({badarg, params_arg_incorrect_type}).

response_handler_wrapper(Response, Handler) ->
  {Id, CallObj} = try
                    begin
                      Jsonrpc = try maps:get(<<"jsonrpc">>, Response) of
                                  Version when Version /= ?JSONRPC ->
                                    {error, {invalid_response, "Value of 'jsonrpc' key must be \"2.0\""}};
                                  _Otherwise ->
                                    true
                                catch
                                  error:{badkey, _} ->
                                    {error, {invalid_response, "Response must contain 'jsornpc' key"}}
                                end,

                      CallId = try maps:get(<<"id">>, Response) of
                                 Id_ when is_binary(Id_) orelse is_number(Id_) orelse Id_ == null ->
                                   Id_;
                                 Id_ ->
                                   throw({Id_,
                                          {error,
                                           {invalid_response,
                                            ?FLAT_FORMAT("'id' must be either String, Number or null, [~p] were given", [Id_])}}})
                               catch
                                 error:{badkey, _} ->
                                   throw({null, {error, {invalid_response, "Response must contain 'id' field"}}})
                               end,

                      if
                        Jsonrpc /= true ->
                          throw({CallId, Jsonrpc});
                        true -> ignore
                      end,

                      Result = maps:get(<<"result">>, Response, not_present),
                      CallError = maps:get(<<"error">>, Response, not_present),
                      ToHandle = case {Result, CallError} of
                                   {not_present, not_present} ->
                                     {error,
                                      {invalid_response,
                                       "Response must contain either 'result' or 'error', neither are present"}};
                                   {not_present, CallError} ->
                                     ErrorCode = maps:get(<<"code">>, CallError, not_present),
                                     ErrorMessage = maps:get(<<"message">>, CallError, not_present),
                                     if
                                       is_number(ErrorCode) andalso is_binary(ErrorMessage) ->
                                         {error, {call_error, CallError}};
                                       true ->
                                         {error,
                                          {invalid_response,
                                           "Response error object must contain 'code' and 'message' fields"}}
                                     end;
                                   {Result, not_present} ->
                                     {result, Result};
                                   {_Result, _CallError} ->
                                     {error,
                                      {invalid_response,
                                       "Response must contain either 'result' or 'error', both are present"}}
                                 end,
                      {CallId, ToHandle}
                    end
                  catch
                    throw:Error ->
                      Error
                  end,
  Handler(Id, CallObj).

call_or_notification_handler_wrapper(Req, ResponseStub, Handler) ->
  try
    begin
      Id = case maps:get(<<"id">>, Req, notification) of
             Id_ when is_number(Id_) orelse is_binary(Id_) orelse Id_ == null orelse Id_ == notification ->
               Id_;
             Id_ ->
               throw(?INVALID_REQUEST( ResponseStub
                                     , "If request contains 'id' key it should be either String, Number or null, [~p] were given"
                                     , [Id_]
                                     ))
           end,
      PreResponse = maps:put(id, Id, ResponseStub),

      try maps:get(<<"jsonrpc">>, Req) of
        Version when Version /= ?JSONRPC ->
          throw(?INVALID_REQUEST(PreResponse, "Value of 'jsonrpc' key must be \"2.0\"", []));
        _Otherwise -> ignore
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

      try Handler(Id, Method, Params) of
        Result ->
          maps:put(result, Result, PreResponse)
      catch
        throw:method_not_found ->
          ?METHOD_NOT_FOUND(PreResponse, "Method ~p not found on this server", [Method]);
        HndlrError ->
          ?INTERNAL_ERROR(PreResponse, "Internal error: ~p", [HndlrError])
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
