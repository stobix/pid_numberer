# README

* [Summary](pid_numberer#markdown-header-summary)
* [Installation](pid_numberer#markdown-header-installation)
    * [Setup](pid_numberer#markdown-header-setup)
    * [Dependencies](pid_numberer#markdown-header-dependencies)
* [Usage](pid_numberer#markdown-header-usage)
    * [Functions](pid_numberer#markdown-header-functions)
    * [Examples](pid_numberer#markdown-header-examples)
* [TODO](pid_numberer#markdown-header-todo)
* [Meta info](pid_numberer#markdown-header-meta)

## Summary

An app for registering pids to unique names, keeping tracks of when pids are terminated and the names freed. (Basically, this enables one to have a nice tree of named processes in observer without being afraid of exhausting the atom definition stack.)

## Installation

### Setup
1. Use [rebar][] to fetch all dependencies and compile the app.
1. Place it somewhere in your erlang runtime path.

### Dependencies

none

[rebar]: https://github.com/basho/rebar "An erlang repository/dependency handler"

## Usage

The functions come in two flavours: 

3. A distinct counter that binds numers to pids. Bound numbers are returned iff the module that bound them exits.
7. A process namer that registers modules based on the module name + an integer. When a thusly named process exits, its name is reused for subsequent process registering by the module.

### Functions

Name | Arguments | Normal Return Value | Description
-- | -- | -- | --
`bind/0` | | `non_neg_integer()` | Binds a number from the generic number pool to the pid. The bind is broken and the number considered free for rebinding when the pid terminates.
`bind/1` | `module()` or `atom()` | `non_neg_integer()` | Returns a new number from the number pool associated with the argument. Any bindings are considered freed when the calling process exits.
`register/1` | `ProbablyModule::atom()` | `RegName::atom()` |Combines `reg_name/1` with `erlang:register/2`; Gets a new argument-specific name and registers the calling process to it. Returns the name under which the process was registered, or exits with an error.
`reg_name/1` | `ProbablyModule::atom()` | `RegName::atom()` |Returns a unique, unregistered, name for the atom provided. 

### Examples

A rather contrived example from the erl terminal.

```erlang
%after loading the app and all its dependencies
(node@somewhere)8> PrintReg = fun() -> io:format("~p~n",[pid_numberer:register(hej)]) end. % A function that registers itself using pid_numberer,prints its registered name and then exits, freeing up the registered name again.
...
(node@somewhere)9> spawn(PrintReg).
hej0
(node@somewhere)10> self().
<0.117.0>
(node@somewhere)11> pid_numberer:register(hej).
hej0
(node@somewhere)12> whereis(hej0).
<0.117.0>
(node@somewhere)9> spawn(PrintReg).
hej1
(node@somewhere)13>pid_numberer:register(hej). % each process can have only one name registered.
** exception error: bad argument
     in function  register/2
        called as register(hej1,<0.117.0>)
     in call from pid_numberer:register/1 (src/pid_numberer.erl, line 89)
(node@somewhere)14> self().
<0.123.0>
(node@somewhere)9> spawn(PrintReg).
hej0
(node@somewhere)15> pid_numberer:register(hej). % since the last process died, hej0 is now free again.
hej0
(node@somewhere)16> whereis(hej0).
<0.123.0>

```

Two ways to spawn several processes of the same module using a minimal pool of names:

* Directly inside an init function

```erlang
    ...
    init() ->
        MyName=pid_numberer:register(?MODULE),
        ....

```

* By giving a unique name as an argument to a gen server initiating function

```erlang
        ...
        gen_server:start_link({local,pid_numberer:reg_name(?MODULE)},?MODULE,StartArgs,Opts),
        ...
```

## TODO

## Meta

Created by Joel Ericson <http://bitbucket.org/volatile> under the GPL license.