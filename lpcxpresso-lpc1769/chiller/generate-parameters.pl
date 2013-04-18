#!/usr/bin/perl
use strict;
use warnings;
use FindBin qw($Bin);

my @param = ();
my $rw = 1;
my $rwCount = 0;
my $count = 0;
while (my $line = <DATA>) {
	chomp $line;
	next unless $line;
	my ($cn, $hn, $dv) = split /\s+/, $line;

	if ($cn eq '_readonly_') {
		$rw = 0;
		next;
	}
	$rwCount++ if $rw;
	
	push @param, {
		cn=>$cn,
		hn=>$hn,
		dv=>$dv,
		rw=>$rw,		
		id=>$count++,
	};
}

open O, ">$Bin/parameterlist.h" or die "Failed to write output file: $!";
print O "#pragma once\n\n";

print O "// Constants to use when acessing the array\n";
for my $p (@param) {
	print O "#define P_$p->{cn} $p->{id}\n";
}

print O "\n\n";
print O "#define P_RW_COUNT $rwCount\n";
print O "#define P_COUNT $count\n";
print O "\n\n";

for my $p (@param) {
	print O "char PN_$p->{cn}"."[] PROGMEM = \"$p->{hn}\";\n";
}
print O "\n\n";

print O "PGM_P PARAMETER_NAMES[P_COUNT] PROGMEM = {\n";
for my $p (@param) {
	print O "  [P_$p->{cn}] = PN_$p->{cn},\n";
}
print O "};\n\n";

print O "int DEFAULT_PARAMETERS[P_RW_COUNT] PROGMEM = {\n";
for my $p (@param) {
	print O "  [P_$p->{cn}] = $p->{dv},\n" if $p->{rw};
}
print O "};\n\n";

print O "int parameters[P_COUNT];\n";

close O;

# This is the list of parameters to generate constants for, columns are:
#  constant base name
#  human readable name
#  default value for read/write values

__DATA__
POWER             power       1
STORE_MAX_TEMP    store-max  150
STORE_MIN_TEMP    store-min  100
CIRCULATION_TEMP  circ-set   200
FAN_POST_RUN      fan-run    60
_readonly_
CURRENT_STATE     state
FAN_TIMER         fan-timer
COMPRESSOR_RELAY  comp-relay
TANK_RELAY        tank-relay
FAN_RELAY         fan-relay
CIRCULATION_CURRENT circ-temp
STORE_CURRENT     store-temp
CIRCULATION_PWM   circ-pwm
COOLING_PWM       cooling-pwm
