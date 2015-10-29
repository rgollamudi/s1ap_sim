%% @author rgollamudi
%% @doc @todo Add description to enb_s1_sup.


-module(enb_s1_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).




%% ====================================================================
%% Behavioural functions 
%% ====================================================================

-include("s1ap.hrl").


start_link() ->
	supervisor:start_link(?MODULE, []).



%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    AChild = {'enb',{'enb_s1',start_link,[{127,0,0,1}, 8000, self()]},
	      permanent,2000,worker,['enb_s1']},
    {ok,{{one_for_one,0,1}, []}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


