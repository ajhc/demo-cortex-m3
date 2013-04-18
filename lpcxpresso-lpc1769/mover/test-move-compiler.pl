#!/usr/bin/perl
use strict;
use warnings;
use Device::SerialPort;
#use Time::HiRes(

my $F = 50000;    # step frequency;
my $A = 57800/$F;  # step/s^2 Accelerate to full speed in 1 second  
my $OS = 1<<30;

my @moves;
sub mout {
    my ($v) = @_;
    push @moves, $v;
}

sub encodeMove {
    my @u32;
    for my $v (@_) {
	push @u32, $v & 0xffffffff;
    }

    my ($ticks, 
	$xs, $ys, $zs, $as,
	$xa, $ya, $za, $aa) = @u32;
        
    my $head = 0x05aa0000;
    $head |= 1<<1 if $xs;
    $head |= 1<<2 if $xa;

    $head |= 1<<3 if $ys;
    $head |= 1<<4 if $ya;

    $head |= 1<<5 if $zs;
    $head |= 1<<6 if $za;

    $head |= 1<<7 if $as;
    $head |= 1<<8 if $aa;

#    $head |= 1<<9;  # LASER
    if ($ticks > 15000) {
#	$head |= 1<<11; # Pixels
    }

    mout($head);
    mout($ticks);

    mout($xs) if $xs;
    mout($xa) if $xa;

    mout($ys) if $ys;
    mout($ya) if $ya;

    mout($zs) if $zs;
    mout($za) if $za;

    mout($as) if $as;
    mout($aa) if $aa;

    if ($head & (1<<9)) {
	mout(0x1A5E0000 | 127); # Full power

	if ($head & (1<<11)) {
	    my $ps = int(((4*32-10)*$OS/$ticks));
	    die "Pixel speed too high: $ps" if $ps > $OS;

	    my $pc = ($ps * $ticks) / $OS;

	    print "Pixel speed: $ps for $ticks check: $pc\n";

	    mout($ps); # Pixel speed
	    mout(4); # Pixel word count
	    mout(0xaaaaaaaa); 
	    mout(0xf0f0f0f0); 
	    mout(0x55aa55aa); 
	    mout(0xffff0000); 
	}
    }
}

# X delta steps, Y delta steps, start speed (steps / second), end speed
sub linexy {
    my ($xd, $yd, $s0, $s1) = @_;

    $s0 /= $F;
    
    my $dist = sqrt($xd**2 + $yd**2);
    my $xv = $xd/$dist;
    my $yv = $yd/$dist;

    my $ticks;
    my $a = 0;
    if (defined $s1) {
	$s1 /= $F;
	# distance = (1/2)*acceleration*time^2
       	# d = s0*t+0.5*a*t^2 and a = (s1-s0)/t =>
	# t = 2*d/(s1+s0)  and a = (s1^2-s0^2)/(2*d) 

	$a = (($s1**2-$s0**2)/(2*$dist));	
	print "$s0 -> $s1 in $dist -> $a\n";
	if (abs($a) > $A) { 
	    # TODO: This doesn't work because the end speed cannot be reached
	    die "Too high acceleration $a > $A\n";

	    $a = $a>0 ? $A : -$A;
	    $ticks = int(0.5+ ((sqrt($s0**2+2*$a*$dist)-$s0)/$a));
	} else {
	    $ticks = int(0.5+ 2*$dist/($s1+$s0));
	}

    } else {
	$ticks = int($dist / $s0 + 0.5);
	$a = 0;
    }

    my $xs = int($OS*$xv*$s0);
    my $ys = int($OS*$yv*$s0);
    my $xa = int($OS*$xv*$a);
    my $ya = int($OS*$yv*$a);

    print "  xs=$xs ys=$ys xa=$xa ya=$ya a=$a dist=$dist ticks=$ticks\n"; 

    encodeMove($ticks,
#	       $xs, $ys, 0, 0, $xa, $ya, 0, 0,
	       $xs, $ys, $xs, $xs, $xa, $ya, $xa, $xa
	);
}

sub linexys {
    my ($xd, $yd, $s0) = @_;

    my $ap = 0.4;

    linexy($ap*$xd, $ap*$yd, 0, $s0);
    linexy((1-2*$ap)*$xd, (1-2*$ap)*$yd, $s0);
    linexy($ap*$xd, $ap*$yd, $s0, 0);
}

my $portName = "/dev/ttyACM0";
my $port = new Device::SerialPort($portName, 0) or die "Can't open $portName: $!\n";
$port->read_const_time(100);

sub portCmd {
    my ($cmd) = @_;
#    print STDERR "Running: $cmd ";
    
    $port->write("$cmd\n");

    my $patience = 10;
    my $res = '';
    while ($patience-- > 0) {
	my $st = $port->read(10000);
	$res .= $st;
	if ($res =~ /[\r\n]Ready[\r\n]/) {
	    $res =~ s/^\s+//;
	    $res =~ s/\s+$//;
#	    print STDERR " got result $res\n";
	    return $res;
	}
	print STDERR ".";
    }
    die "Failed to get response to command: $cmd\nResult: $res\n";
}

sub strMoves {
    my $res = "bm";
    die "Too many moves!" if @moves > 4090;

    $res .= sprintf(" %x", scalar(@moves));

    for my $m (@moves) {
	$res .= sprintf(" %x", $m);
    }

    @moves = ();
    return $res;
}
portCmd("br");

portCmd("ai 10c");
portCmd(sprintf("me %d %d %d", 0, 350, 3));
portCmd(sprintf("me %d %d %d", 1, 1870, 3));
portCmd(sprintf("me %d %d %d", 2, 350, 3));
portCmd(sprintf("me %d %d %d", 3, 0, 3));


while (1) {
#for my $loops (1..1) {
    for my $i (0..0) {
	for my $j (1..8) {
	    linexys(200, 2*200, $j*1600*1.5);
	    linexys(-200, -2*200, $j*1600*1.5);
	}
	linexys(2*4*200, 20*8*200, 17*1600);
	linexys(-2*4*200, -20*8*200, 17*1600);
    }

#    linexys(400, 400, 10*1600);
#    linexys(400, 1600, 10*1600);
#    linexys(-800, -2000, 10*1600);

    my $res = portCmd(strMoves());
    
    while (1) {
	if ($res =~ /buffer\.free\s+(\d+)\s+words/) {
	    my $moves = $1;
	    if ($moves > 4095-100) {
		last;
	    }
	    sleep(1);	    
	    $res = portCmd("bs");
	} else {
	    die "Unable to find buffer.free in $res\n";
	}
    }
}

    
while (1) {
    my $res = portCmd("st");
    if ($res =~ /motion.active\s+No/) {
	last;
    } else {
	sleep(1);	    
    }
}

portCmd(sprintf("me %d %d %d", 0, 0, 3));
portCmd(sprintf("me %d %d %d", 1, 0, 3));
portCmd(sprintf("me %d %d %d", 2, 0, 3));
die "Done";
