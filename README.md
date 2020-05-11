erlang-json-lib
==========

An Erlang component on JSON.

Build
-----

```
    $ rebar3 compile
    $ rebar3 eunit
```

Exported Functions
-----

```
-spec ecjson:check_and_condense(JSON_string :: binary()) ->
        {ok, Condensed_JSON_string :: binary()} |
        {badsymbol, check_and_condense :: atom(), Args :: list()}.
```
