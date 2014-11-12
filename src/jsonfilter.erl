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


-module(jsonfilter).

-export([filter/3]).
-export([init/1, handle_event/2]).


filter(JSON, Module, InitialState) ->
  case JSON of
    JSON when is_binary(JSON) ->
      (jsx:decoder(?MODULE, {Module, InitialState}, []))(JSON);
    JSON when is_list(JSON); is_map(JSON) ->
      (jsx:encoder(?MODULE, {Module, InitialState}, []))(JSON)
  end.


init({Module, InitialState}) -> {[], Module, Module:init(InitialState)}.

handle_event(end_json, {_, Module, State}) ->
  Module:finish(State);
handle_event(start_object, {Path, Module, State}) ->
  {[key] ++ Path, Module, State};
handle_event(start_array, {Path, Module, State}) ->
  {[0] ++ Path, Module, State};
handle_event(end_object, {[_|Path], Module, State}) ->
  {advance(Path), Module, State};
handle_event(end_array, {[_|Path], Module, State}) ->
  {advance(Path), Module, State};
handle_event({key, Key}, {[key|Path], Module, State}) ->
  {[Key] ++ Path, Module, State};
handle_event(Value, {Path, Module, State}) ->
  NewState = Module:handle_value(lists:reverse(Path), Value, State),
  {advance(Path), Module, NewState}.

%% if there's no path (the base object/array is closed) do nothing
advance([]) -> [];
advance([N|Path]) when is_integer(N) -> [N+1] ++ Path;
advance([Bin|Path]) when is_binary(Bin) -> [key] ++ Path.