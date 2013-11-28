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
use DBI;


my $socket_port = 7789;
my $dbfile = "gpstrack.db3";

sub parsePacket($);
sub HEART(){return pack("C*",0x54,0x68,0x1A,0x0D,0x0A);}


my $client; #socket client
my $data;   #data from client one read
my $server; #socket server
my $dbh;    #DB hancle
my $sth;    
#my $imei,$date,$lng,$lat,$speed,$direction,$resevered,$status;



#  
#  name: convertGPSData
#  convert string to GPS Data
#  @param  string format($imei,$date,$lng,$lat,$speed,$direction,$status)
#  @return db     format($imei,$date,$lng,$lat,$speed,$direction,$status)
#  
sub convertGPSData(@)
{
}

#($imei,$date,$lng,$lat,$speed,$direction,$status)
sub saveGPSData(@)
{
	$sth->execute(@_);
	
    #$sth->bind_param(1, $date, { TYPE => SQL_DATETIME });
	#$sth->bind_param(2, $imei, { TYPE => SQL_INTEGER });
	#$sth->bind_param(3, $lng, { TYPE => SQL_FLOAT });
	#$sth->bind_param(4, $lat, { TYPE => SQL_FLOAT });
	#$sth->bind_param(5, $speed, { TYPE => SQL_INTEGER });
	#$sth->bind_param(6, $direction, { TYPE => SQL_INTEGER });
	#$sth->bind_param(7, $status, { TYPE => SQL_INTEGER });
}

sub init
{
	$server= 
	   IO::Socket::INET->new(LocalPort => $socket_port,
							 Type      => SOCK_STREAM,
							 Reuse     =>1,
							 Listen    =>3)
		or die "Counld not create tcp server ";  
	
	$dbh = DBI->connect("dbi:SQLite:dbname=$dbfile","","") 
		or die "Counld not connect to sqlite3 DB"; 
	$sth = $dbh->prepare(qq{insert into gpstrack values(?,?,?,?,?,?,?)});
	
}
sub run
{
         
	while ($client = $server->accept()){
		while($data = <$client>){
			#we got data from here.
			parsePacket($data);
		}
		$client->close();
	}

	$server->close();
}

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
          = ($data =~ /^6868250000(.{8})..10(.{6})(.{4})(.{4})(.{1})(.{2})(.{3})(.{4})0d0a$/);
        my @param  = ($imei,$date,$lng,$lat,$speed,$direction,$status);
        my @result = convertGPS(@param);
        saveGPSData(@result);
    }
    
    #we don't known other data format, if we get here.
}


init();
run();


__END__
==head1 1.Packet
Data From Client: 68 68 25  00 00 ID-8byte SEQ-2byte 10 ....... 0D 0A
Hear From Client: 68 68 len xx xx ID-8byte SEQ-2byte 1A ....... 0D 0A

ID-8byte(IMEI): 01 23 45 67 89 12 34 50

==head1 2.Heart beat
heart_from_server = (0x54,0x68,0x1A,0x0D,0x0A);
heart_from_client = (0x68,0x68,..., 0x1A,...,0x0D,0x0A);

==head1 3.Data 
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

==head1 4.SQLite Table gpstrack
create table gpstrack 
(
  date datetime not null,
  imei char(16) not null,
  lng  float default 0.0,
  lat  float default 0.0,
  speed integer default 0,
  direction integer default 0,
  status integer default 0,
  primary key (date,imei)
);
