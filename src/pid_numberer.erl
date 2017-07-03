%%%-------------------------------------------------------------------
%%% @author Joel Ericson
%%% @copyright {@date}, Joel Ericson
%%% @doc
%%%
%%% The main purpose of this application is to work around Erlang having a limited atom name stack;
%%% It can be enlightening to have each running server from an application have a distinct name associated with
%%% the server module's name, but care must be taken to not exhaust the atom name stack. This app keeps track of
%%% the names of the modules it registers, reusing old names when their corresponding PID's die.
%%%
%%%
%%%
%%% @end
%%% Created : 2014-06-17 15:16:20.607252
%%%-------------------------------------------------------------------
%%% This is a special process. A special process is aware of system  
%%% messages and also implements some standard functions needed (and  
%%% standarized) in an OTP environment, so it can be supervised  
%%% (started/stop), upgraded in a hot-upgrade fashion, etc.  
%%%  
%%% See http://www.erlang.org/doc/design_principles/spec_proc.html  
%%% For system messages, see: http://www.erlang.org/doc/man/sys.html  
-module(pid_numberer).  
%%-------------------------------------------------------------------  
%% API Function Exports  
%%-------------------------------------------------------------------  
-export([start_link/0, init/1]).  
  
%%-------------------------------------------------------------------  
%% Required OTP Exports  
%%-------------------------------------------------------------------  
-export([  
    system_code_change/4, system_continue/3,  
    system_terminate/4, write_debug/3
]).  

-export([
    bind/0,bind/1,
    register/1,
    reg_name/1
    ]).
  
%%-------------------------------------------------------------------  
%% API Function Definitions  
%%-------------------------------------------------------------------  
%% @private
%% @doc Starts a new process synchronously. Spawns the process and  
%%% waits for it to start.  
%% See http://www.erlang.org/doc/man/proc_lib.html  
start_link() ->  
    proc_lib:start_link(?MODULE, init, [self()]).  
  
%%-------------------------------------------------------------------  
%% API Function Definitions  
%%-------------------------------------------------------------------  
%% @private
%% @doc Notifies the parent of a successful start and then runs the  
%% main loop. When the process has started, it must call  
%% init_ack(Parent,Ret) or init_ack(Ret), where Parent is the  
%% process that evaluates this function (see start_link/0 above).  
%% At this time, Ret is returned.  
init(Parent) ->  
    register(?MODULE, self()),  
    process_flag(trap_exit,true),
    Debug = sys:debug_options([]),  
    proc_lib:init_ack(Parent, {ok, self()}),  
    loop(Parent, Debug, []).  
  
%%----------------------------------------------
%% @doc binds a non-bound number from the general pool to the caller PID.
%%
%% All numbers bound to the PID become non-bound when the PID dies.
%% @todo
%% @spec () -> non_neg_integer()
%% @end
%%----------------------------------------------

bind() ->
    bind('_').

%%----------------------------------------------
%% @doc binds a non-bound number from the Module pool to the caller PID.
%%
%% All numbers bound to the PID become non-bound when the PID dies.
%% @todo
%% @spec (Module) -> non_neg_integer()
%% @end
%%----------------------------------------------

bind(Module) ->
    ?MODULE ! {bind,self(),Module,self()},
    receive
        {number,Number} ->
            Number;
        Error -> Error
    end.

%%----------------------------------------------
%% @doc Registers the calling PID to a not yet registered version of Module.
%%
%% The calling PID must not be registered before.
%%        
%% When the PID dies, the name will be reused for future registrations
%% @todo
%% @equiv erlang:register/2
%% @spec (Module) -> non_neg_integer()
%% @returns The name the caller PID got registered as.
%% @end
%%----------------------------------------------
register(Module) ->
    Name=reg_name(Module),
    register(Name,self()),
    Name.
        
%%----------------------------------------------
%% @doc Returns a not registered numbered version of the module name to be used for process registering
%%
%% erlang:register/2 throws an error badarg if the PID is already registered
%% @spec (Module) -> atom()
%% @end
%%----------------------------------------------
reg_name(Module) ->
    ?MODULE ! {bind,self(),Module,self()},
    Number=receive
        {number,N} -> N;
        % No errors implemented yet.
        Error -> throw(Error)
    end,
    list_to_atom(lists:concat([atom_to_list(Module),Number])).

%%-------------------------------------------------------------------  
%% Internal Functions  
%%-------------------------------------------------------------------  
%% @private
%% @doc Our main loop, designed to handle system messages.  
loop(Parent, Debug, State) ->  
    receive  
        {bind,PID,Module,Return} ->
            Number = case get(Module) of
                undefined ->
                    put(Module,{[],0}),
                    0;
                {[],Highest} ->
                    put(Module,{[],Highest+1}),
                    Highest+1;
                {[Free|Freed],Highest} ->
                    put(Module,{Freed,Highest}),
                    Free
                end,
            case get(PID) of
                undefined ->
                    monitor(process,PID),
                    put(PID,[{Module,Number}]);
                Bindings ->
                    put(PID,[{Module,Number}|Bindings])
            end,
            Return ! {number,Number},
            loop(Parent, Debug, State);
        {'EXIT',Parent,Reason} ->
            exit(Reason);
        {'DOWN',_,_,Child,_} ->
            case get(Child) of
                undefined ->
                    loop(Parent, Debug, State);
                Bindings ->
                    Unbind = 
                        fun({Module,Number}) ->
                            case get(Module) of
                                undefined ->
                                    ok;
                                {_,0} ->
                                    erase(Module);
                                {Freed,Highest} when Number == Highest ->
                                    put(Module,{Freed,Highest-1});
                                {Freed,Highest} ->
                                    put(Module,{[Number|Freed],Highest})
                            end
                        end,
                    lists:map(Unbind,Bindings),
                    erase(Child),
                    loop(Parent, Debug, State)
            end;
        {system, From, Request} ->  
            sys:handle_system_msg(  
                Request, From, Parent, ?MODULE, Debug, State
            );  
        Msg ->  
            sys:handle_debug(  
                Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}  
            ),  
            loop(Parent, Debug, State)  
    end.  
  
%% @hidden
%% @doc Called by sys:handle_debug().  
write_debug(Dev, Event, Name) ->  
    io:format(Dev, "~p event = ~p~n", [Name, Event]).  
  
%% @hidden
%% @doc http://www.erlang.org/doc/man/sys.html#Module:system_continue-3  
system_continue(Parent, Debug, State) ->  
    loop(Parent, Debug, State).  
  
%% @hidden
%% @doc http://www.erlang.org/doc/man/sys.html#Module:system_terminate-4  
system_terminate(Reason, _Parent, _Debug, _State) ->  
    exit(Reason).  
  
%% @hidden
%% @doc http://www.erlang.org/doc/man/sys.html#Module:system_code_change-4  
system_code_change(State, _Module, _OldVsn, _Extra) ->  
    {ok, State}.  


