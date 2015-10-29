%% @author ramanagollamudi
%% @doc @todo Add description to logger.


-module(logger).

%% ====================================================================
%% API functions
%% ====================================================================
-export([log_msg/2, log_msg/1]).


log_msg(Format, List) ->
	io:format(Format, List).
log_msg(Format) ->
	io:format(Format).


%% ====================================================================
%% Internal functions
%% ====================================================================


