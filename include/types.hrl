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

