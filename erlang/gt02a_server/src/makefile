#  makefile
#  
#  Copyright 2013 corbamico <corbamico@163.com>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#  
#  
.SUFFIXES: .erl .beam
.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = gt02a_cli_sock gt02a_cli_sup gt02a_srv gt02a_sup gt02a_app

all:compile

compile:${MODS:%=%.beam}

run:compile
	${ERL} -pa . -s application start gt02a_app
	
clean:
	rm -rf *.beam erl_crash.dump
