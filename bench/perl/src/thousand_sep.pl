#!/usr/bin/env perl
use 5.012;
use warnings;
use Time::HiRes qw/time/;

# Idea: Split number into a pre-decimal separator part, and one where
# every 3rd digit needs to be followed by a ",".
# E.g.: 12345678901234 -> 12 and 345678901234

my $pre_compile = time;
my $splitRx   = qr/^(\d{1,3})((?:\d{3})*)\n$/;
my $replaceRx = qr/(\d{3})/;

# Start timing
my $start = time;

while (my $line = <>) {
    my ($first, $rest) = $line =~ $splitRx;

    print $first;
    if ($rest ne '') {
        $rest =~ s/$replaceRx/,$1/g;
        print $rest;
    }
    print "\n";
}

# End timing
my $end = time;
my $elaps = int(($end - $start) * 1000);
my $elaps_compile = int(($start - $pre_compile) * 1000);

print STDERR "\ncompilation (ms): ${elaps_compile}\n";
print STDERR "matching (ms):    ${elaps}\n"
