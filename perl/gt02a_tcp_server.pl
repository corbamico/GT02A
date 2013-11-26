#  @File gt02a.pl
#  @Desp perl tcp server for anchean GT02A+ GPS tracker
#  @author <corbamico@163.com>
#
#  Copyright 2013  <corbamico@163.com>
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

use strict;
use IO::Socket;


my $socket_port = 7789;

sub parsePacket($);
sub HEART(){return pack("C*",0x54,0x68,0x1A,0x0D,0x0A);}

my $client; #socket client
my $data;   #data from client one read
my $server = 
   IO::Socket::INET->new(LocalPort => $socket_port,
                         Type      => SOCK_STREAM,
                         Reuse     =>1,
                         Listen    =>3)
    or die "Counld creae tcp server ";            
        
while ($client = $server->accept()){
    while($data = <$client>){
        #we got data from here.
        parsePacket($data);
    }
    $client->close();
}

$server->close();

sub parsePacket($){
    my ($data) = unpack "H*", $_[0] ;  #converto '546681a0d0a' etc
    
    #send heart if we got heart bit from client
    if ($data =~ /^6868.{16}1a.*0d0a$/ )
    {
        $client->send(HEART)  if $data =~ m/^6868.{16}1a.*0d0a$/ ;
    }
    #we got gps data from client
    elsif ($data =~ /^6868250000.{10}10.*0d0a$/ )
    {
          my ($imei,$date,$lng,$lat,$speed,$direction,$resevered,$status) 
        = ($data =~ /^6868250000(.{8})..10(.{6})(.{4})(.{4})(.{1})(.{2})(.{3})(.{4})0d0a$/)
    }
    
    #we don't known other data format, if we get here.
}

=begin
==head 1.Packet
Data From Client: 68 68 25  00 00 ID-8byte SEQ-2byte 10 ....... 0D 0A
Hear From Client: 68 68 len xx xx ID-8byte SEQ-2byte 1A ....... 0D 0A

ID-8byte(IMEI): 01 23 45 67 89 12 34 50

==head 2.Heart beat
heart_from_server = (0x54,0x68,0x1A,0x0D,0x0A);
heart_from_client = (0x68,0x68,..., 0x1A,...,0x0D,0x0A);

==head 3.Data 
Data from Client (24byte):
Date Lng Lat Speed Direction resevered Status
 6    4   4     1     2          3        4

Date:     YY(1)MM(1)(DD)HH(1)MM(1)SS(1)
Lat :     latitude = Lat/30000/60 
Lng :     Long     = Lng/30000/60
Speed:    0-255 km/h
Direction:0~360, 0=North
Status:   
  BIT 0: 0/1 GPS located (No/Yes)
  BIT 1: 0/1 North/South
  BIT 2: 0/1 West/East
  BIT 3: 0/1 Power Charge(No/Yes)
  BIT 4: 0/1 Normal/SoS
  BIT 5: 0/1 Normal/Force Poweroff alert
=end
