%%
%% gt02a_app.app
%% 
%% Copyright 2013 corbamico <corbamico@163.com>
%% 
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%% MA 02110-1301, USA.
%% 
%% 
%%

{application,gt02a_app,
	[
    {description,"application for gt02a GPS tracker"},
	{vsn,"0.0.1"},
	{modules,[gt02a_srv,gt02a_sup,gt02a_cli_sock,gt02a_cli_sup]},
	{registered,[gt02a_srv,gt02a_sup,gt02a_cli_sup]},
	{applications,[kernel,stdlib]},
    {mod,{gt02a_app,[]}},
    {start_phases,[]}
    ]
}.
	
