%  gt02a_srv.erl
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
-module(gt02a_srv).

-behaviour(gen_server).

%% define
-define(SERVER,?MODULE).
-define(PORT,10550).
-define(DATA_PACKET,
        <<
         16#68:8,
         16#68:8,
         16#25:8,
         16#00:8,
         16#00:8,
         ID: 64,
         _:  16,         %%sequence
         16#10:8,
         ClientData:192, %%24 byte
         16#0D:8,
         16#0A:8
        >> ).
        
-define(HEART_PACKET,
        <<
         16#68:8,
         16#68:8,
         _:8,           %%len
         _:8,         %%power
         _:8,        %%signal
         _:64,          %%id
         _:16,          %%seq
         16#1A:8,
         _/binary
        >>).

-define(SERVER_HEART,<<16#54,16#68,16#1A,16#0D,16#0A>>).

%% API
-export([start_link/1,stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {port,lsock}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
%%  gen_server:start_link({local, ?MODULE}, ?MODULE, [LSock], []).
%%  @note
%%  Should not use {local,?MODULE} to register process name,
%%  if want more one gt02a_srv process running!
%%  if present {local,?MODULE}, got {error,{already_started,<0.38.0>} to
%%     gt02a_sup:start_child() call
    gen_server:start_link(?MODULE, [LSock], []).

stop() ->
    gen_server:cast(?SERVER,stop).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([LSock]) ->
    %% move listen to gt02a_app
    %% {ok,LSock} = gen_tcp:listen(Port,[{active,true}]),
    %% 0 -> timeout triger immediate, 
    %%  server async goes to handle_info(timeout)
    {ok, #state{port=?PORT,lsock=LSock},0}.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(stop, State) ->
    {stop,normal,State};
handle_cast(_Msg, State) ->
    {noreply, State}. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{lsock=LSock}=State) ->
    {ok,_} = gen_tcp:accept(LSock),
    %% Should spawn child again, here 
    {ok,_ }= 
       gt02a_sup:start_child(),
    {noreply, State};
    
handle_info({tcp_closed,Sock}, State) ->
    {stop,normal,State};
    
handle_info({tcp,Sock,Data}, State) ->
	%% @TODO get socket data here, should handle it.
	handle_data({tcp,Sock,Data}, State);  
    %%% {noreply, State};
  
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} 
%% @end 
%% Data Frm Client: 68 68 25  00 00 ID-8byte SEQ-2byte 10 ....... 0D 0A
%% Hear Frm Client: 68 68 len xx xx ID-8byte SEQ-2byte 1A ....... 0D 0A
%%--------------------------------------------------------------------
handle_data({tcp,Sock,Data}, State) ->
    case Data of 
        ?DATA_PACKET  ->
            %% get data here
            handle_clientdata(ID,ClientData);
        ?HEART_PACKET ->       
            %% get heart here
            gen_tcp:send(Sock,?SERVER_HEART)
    end,
    {noreplay,State}.
    
    
%%--------------------------------------------------------------------
%% @spec handle_cleintdata(ClientData) -> ok
%% @end 
%%% Data from Client (24byte):
%%% Date Lng Lat Speed Direction resevered Status
%%%  6    4   4     1     2          3        4
%%% 
%%% Date:     YY(1)MM(1)(DD)HH(1)MM(1)SS(1)
%%% Lat :     latitude = Lat/30000/60 
%%% Lng :     Long     = Lng/30000/60
%%% Speed:    0-255 km/h
%%% Direction:0~360, 0=North
%%% Status:   
%%%   BIT 0: 0/1 GPS located (No/Yes)
%%%   BIT 1: 0/1 North/South
%%%   BIT 2: 0/1 West/East
%%%   BIT 3: 0/1 Power Charge(No/Yes)
%%%   BIT 4: 0/1 Normal/SoS
%%%   BIT 5: 0/1 Normal/Force Poweroff alert
%%--------------------------------------------------------------------
handle_clientdata(ID,ClientData)->
    ok.
