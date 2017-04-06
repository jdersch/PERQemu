#!/usr/bin/perl -w
#
# $Id: makerom.pl,v 1.2 2017/03/18 20:07:13 skeezics Exp $
# $Source: /home/skeezics/perq/shared/PERQemu-source/PERQemu/PROM/RCS/makerom.pl,v $
#
# makerom - a quick and dirty hack to build the rom files
#
# usage:
#           makerom.pl < mask_table.txt > rds00emu.rom
#           makerom.pl < edge_table.txt > rsc03emu.rom
#
# reads stdin, writes stdout.  any line that starts with three hex digits
# and ends with a valid combinermask or edgestrategy word is included; the
# rest are left as "invalid".  the last address determines the size of the
# output file, so 512 for the rds rom (mask table, 9-bit index) or 128 for
# the rsc (edge table, 7-bit index) is expected.
#

%words = (
    'invalid' => 0x0,           # combiner mask words
    'dontmask' => 0x1,
    'leftedge' => 0x2,
    'rightedge' => 0x4,
    'both' => 0x6,
    'fullword' => 0x08,
    'leftover' => 0x10,
    'nopopnopeek' => 0x0,       # edge strategy words
    'nopoppeek' => 0x1,
    'popnopeek' => 0x2,
    'poppeek' => 0x3,
    'unknown' => 0x7
);

@table = ();
foreach $i ( 0 .. 1023 ) { $table[$i] = 0; }

$hi = 0;

while (<>) {
    chomp;
    if ( /^([0-9a-f]{3})\s.*\s(\w{4,})$/i ) {
        ($addr, $val) = ($1, $2);
        $addr = hex($addr);
        $val =~ tr/[A-Z]/[a-z]/;
        do {
            print STDERR "addr $addr => value is $val (", $words{$val}, ")\n";
            $table[$addr] = $words{$val};
            $hi = $addr if $addr > $hi;
        } if defined($words{$val});
    }
    else {
        print STDERR "skipping: '", $_, "'\n";
    }
}

print STDERR "end of input reached, writing table...";

foreach $i ( 0 .. $hi - 1 ) { print chr($table[$i]); }

print STDERR "($hi) done.\n";

