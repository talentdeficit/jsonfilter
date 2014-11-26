jsonfilter (v0.1.0)
===================

an otp approach to working with streaming json

**jsonfilter** is built via [mix][mix], and continuous integration testing provided by [travis-ci][travis]

current status: [![Build Status](https://travis-ci.org/talentdeficit/jsonfilter.svg?branch=mix)](https://travis-ci.org/talentdeficit/jsonfilter)

**jsonfilter** is released under the terms of the [MIT][MIT] license

copyright 2014 alisdair sullivan

## quickstart ##

#### to build library and run tests  ####

```bash
$ mix get.deps
$ mix compile
$ mix eunit
```

#### filtering a json stream ####

filter a json stream

```javascript
// JSON
{ "books": [
  { "title": "a wrinkle in time",
    "author": "madeleine l'engel",
    "editions": [1962, 1978, 2007]
  },
  { "title": "all creatures great and small",
    "author": "james herriot",
    "editions": [1972, 1992, 2004, 2014]
  }
]}
```

```erlang
%% a callback module

-module(authors).
-export([init/1, handle_value/3, finish/1]).

%% init's arg is the third argument passed to `filter/3`
init([]) -> [].

handle_value([<<"books">>, _, <<"author">>], Author, State) ->
  [Author] ++ State;
%% if you don't handle all paths filtering will fail with a `function_clause`
%%  error
handle_value(_, _, State) -> State.

finish(State) -> lists:reverse(State).
```

```erlang
1> jsonfilter:filter(JSON, authors, []).
[<<"james herriot">>, <<"madeleine l'engel">>].
```


[mix]: https://hex.pm
[travis]: https://travis-ci.org
[MIT]: http://www.opensource.org/licenses/mit-license.html