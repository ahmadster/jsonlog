-module(jsonlog).

%% logger callbacks
-export([format/2]).

-spec format(LogEvent, Config) -> unicode:chardata() when
	LogEvent :: logger:log_event(),
	Config :: logger:formatter_config().
format(#{level := Level, msg := {report, Msg}, meta := Meta}, Config) when is_map(
	Msg) ->
	Meta0 = maps:put(level, Level, Meta),
	format_msg(Msg, Meta0, Config);
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
	format(Map#{msg := {report, maps:from_list(KeyVal)}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
	Msg = #{text => unicode:characters_to_binary(String)},
	format(Map#{msg := {report, Msg}}, Config);
format(Map = #{msg := {Format, Terms}}, Config) ->
	String = unicode:characters_to_binary(io_lib:format(Format, Terms)),
	Msg = #{text => String},
	format(Map#{msg := {report, Msg}}, Config).

format_msg(Msg, Meta, _Config) when
	is_map(Msg) andalso
		is_map(Meta) andalso
		is_map(_Config) ->
	Msg0 = maps:merge(Msg, Meta),
	X = jsone:encode(Msg0),
	<<X/binary, <<"\n">>/binary>>.
