#!/usr/bin/perl -w
#
# makebkm - a filthy hack to build the memory state machine rom
#
# the bookmark rom encodes the memory state machine inputs and produces a
# set of flags and state transition signals as outputs.  this boils
# down the complex memory state machine apparatus into a single lookup,
# distilled from a large number of special cases and rules, and is
# more like the actual hardware does it.  it also lets us dispense
# with the elaborate (and inefficient) queueing mechanism - which was
# fairly accurate, but very slow due to garbage collection overhead.
#
# usage:
#       makebkm.pl < memstate.txt > bkm16emu.rom
#
# reads stdin, writes stdout.  input format:
#
#       addr [text] | flags index state
#
# the 'addr' field, followed by whitespace, is an 8-bit encoding of the
# inputs, which may be included (and ignored by this program) as arbitrary
# text up to a single '|' char, which acts as a separator.  the outputs
# consist of a set of four boolean flags, a word index (0..3) and a state
# value (text).  the 8 bits of output are written as unsigned bytes, so no
# worries about endienness.
#

%words = (
    'invalid' => 0,  # same as idle since we don't do anything with it anyway
    'idle' => 0,
    'waitt3' => 1,
    'waitt2' => 2,
    'running' => 3
);

@table = ();
foreach $i ( 0 .. 256 ) { $table[$i] = 0; }

$hi = 0;

while (<>) {
    chomp;
    s/\s+/ /g;

    # this is hideous.  perl re syntax has devolved into complete lunacy.
    # simplify the crap out of things, making it brittle and ugly.  yay.
    if ( /^([0-9a-f]{3}) .*\Q|\E ([01] [01] [01] [01]) ([0-3]) (\w{4,}) {0,1}$/i ) {
        ($addr, $flags, $idx, $val) = ($1, $2, $3, $4);

        $addr = hex($addr);
        $val =~ tr/[A-Z]/[a-z]/;
        do {
            $out = 0;
            @bits = split(/ /, $flags);    	# split the text
            foreach $i ( 0..3 ) {          	# or in each bit and shift
               $out |= int($bits[$i]);
               $out <<= 1;
            }
            $out <<= 1;                     # shift and or in 2-bit idx val
            $out += int($idx);
            $out <<= 2;                     # shift and or in 2-bit state val
            $out += int($words{$val});

            print STDERR "addr $addr => value is $out\n";

            $table[$addr] = $out;
            $hi = $addr if $addr > $hi;
        } if defined($words{$val});
    }
    else {
        print STDERR "skipping: '", $_, "'\n";
    }
}

print STDERR "end of input reached, writing table...";

foreach $i ( 0 .. $hi ) { print chr($table[$i]); }

print STDERR "($hi) done.\n";
