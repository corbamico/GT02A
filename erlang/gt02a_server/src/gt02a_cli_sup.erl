%  gt02a_cli_sup.erl
%  
%  Copyright 2013  <corbamico@163.com>
%  
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%  MA 02110-1301, USA.
%  
%  
-module(gt02a_cli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD_TEMPORARY(Id,Mod, Type, Args), 
        {
         %%list_to_atom(binary_to_list(term_to_binary(id))), 
         Id,
         {Mod, start_link, Args},
         temporary,
         %%transient, 
         %%permanent,
         brutal_kill, 
         Type, 
         [Mod]
         }
        ).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, 
      {{simple_one_for_one,0,1}, 
       [ 
         ?CHILD_TEMPORARY(gt02a_cli_sock,gt02a_cli_sock, worker, [])]
      }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_child(ClientSock)->
    {ok,Pid} = supervisor:start_child(?MODULE,[]),
    gen_tcp:controlling_process(ClientSock,Pid),
    {ok,Pid}.





