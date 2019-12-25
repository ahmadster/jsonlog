-module(jsonlog).

%% logger callbacks
-export([format/2, clean/1]).

-spec format(LogEvent, Config) -> unicode:chardata() when
	LogEvent :: logger:log_event(),
	Config :: logger:formatter_config().
format(#{level := Level, msg := {report, Msg}, meta := Meta}, Config)
	when is_map(Msg) ->
	encode(Msg, maps:put(level, Level, Meta), Config);
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
	format(Map#{msg := {report, maps:from_list(KeyVal)}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
	Msg = #{text => unicode:characters_to_binary(String)},
	format(Map#{msg := {report, Msg}}, Config);
format(Map = #{msg := {Format, Terms}}, Config) ->
	String = unicode:characters_to_binary(io_lib:format(Format, Terms)),
	Msg = #{text => String},
	format(Map#{msg := {report, Msg}}, Config).

encode(Map, Meta, #{jsone_options := Options}) when is_map(Map) andalso is_map(Meta) ->
%%	CMap = maps:with([what, file, line, time], maps:merge(Map, Meta)),
%%	io:format("ENCODING:~p~n~p~n~n", [Options, Map]),
	J = jsone:encode(clean(Map), Options),
	<<J/binary, <<"\n">>/binary>>.


%% clean
%% we'll get a FORMATTER CRASH if a value jsone can't handle makes it thru.
clean(Num) when is_number(Num) -> Num;
clean(Bin) when is_binary(Bin) -> Bin;
clean(Pid) when is_pid(Pid) -> list_to_binary(pid_to_list(Pid));
clean(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
clean(Port) when is_port(Port) -> clean(port_to_list(Port));
clean(Tuple) when is_tuple(Tuple) -> clean(tuple_to_list(Tuple));
clean(Str) when is_list(Str) ->
	case io_lib:printable_list(Str) of
		true -> list_to_binary(Str);
		false -> lists:map(fun clean/1, Str)
	end;
clean(Map) when is_map(Map) ->
	maps:fold(fun(Key, Value, Acc) ->
		maps:put(to_binary(Key), clean(Value), Acc)
	          end, #{}, Map);
clean(Other) -> to_binary(Other).

to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(Thing) -> erlang:iolist_to_binary(io_lib:format("~p", [Thing])).

%%Msg = #{file => "application_controller.erl",line => 1929,time => 1577294708050727}.
%%Msg = #{label => {application_controller, progress}, report => [{application, cowlib}, {started_at, nonode@nohost}]}.
%%Msg = #{major => 2, minor => 5, state => #{buffer => <<>>,digital => #{0 => #{0 => 0, 1 => 0, 2 => 0,3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0}, 1 => #{0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0}}, serial => {gen_serial, port, pid}}, what => "Firmata version"}.
%%Msg3 = #{1 => "Firmata version"}.
%%Config = #{jsone_options => []}.
%%Level = info.
%%Meta = #{}.
%%jsonlog:format(#{level => Level, msg => {report, Msg3}, meta => Meta}, Config).
%%
%%jsone:encode(jsonlog:clean(Msg)).