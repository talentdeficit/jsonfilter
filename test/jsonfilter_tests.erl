%% The MIT License

%% Copyright (c) 2014 alisdair sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.


-module(jsonfilter_tests).
-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_value/3, finish/1]).
-export([json/0]).

init(_State) -> [].

handle_value(Path, Value, State) ->
  [{Path, Value}] ++ State.

finish(State) -> lists:reverse(State).


json() ->
  <<"{ \"books\": [
    { \"title\": \"a wrinkle in time\",
      \"author\": \"madeleine l'engel\",
      \"editions\": [1962, 1978, 2007]
    },
    { \"title\": \"all creatures great and small\",
      \"author\": \"james herriot\",
      \"editions\": [1972, 1992, 2004, 2014]
    }
  ]}">>.

books_test_() ->
  [
    {"books test", ?_assertEqual(
      [
        {[<<"books">>, 0, <<"title">>], {string, <<"a wrinkle in time">>}},
        {[<<"books">>, 0, <<"author">>], {string, <<"madeleine l'engel">>}},
        {[<<"books">>, 0, <<"editions">>, 0], {integer, 1962}},
        {[<<"books">>, 0, <<"editions">>, 1], {integer, 1978}},
        {[<<"books">>, 0, <<"editions">>, 2], {integer, 2007}},
        {[<<"books">>, 1, <<"title">>], {string, <<"all creatures great and small">>}},
        {[<<"books">>, 1, <<"author">>], {string, <<"james herriot">>}},
        {[<<"books">>, 1, <<"editions">>, 0], {integer, 1972}},
        {[<<"books">>, 1, <<"editions">>, 1], {integer, 1992}},
        {[<<"books">>, 1, <<"editions">>, 2], {integer, 2004}},
        {[<<"books">>, 1, <<"editions">>, 3], {integer, 2014}}
      ],
      jsonfilter:filter(json(), ?MODULE, [])
    )}
  ].
