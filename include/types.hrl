-type item() :: string() | binary() | integer() | atom().
-type key() :: item().
-type val() :: item().
-type maybe():: {ok, val()} | {error, val()}.

-type ok(A) :: {ok, A}.
-type error(A) :: {error, A}.
-type maybe(A, B) :: {ok, A} | {error, B}.