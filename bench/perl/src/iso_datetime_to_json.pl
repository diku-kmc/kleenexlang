#!/usr/bin/env perl
use strict;
use warnings;
use Time::HiRes qw/ time /;

my $regexprime = "((?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[0-1]|0[1-9]|[1-2][0-9])T(2[0-3]|[0-1][0-9]):([0-5][0-9]):([0-5][0-9])(Z|[+-](?:2[0-3]|[0-1][0-9]):[0-5][0-9])?";

my $regex = $regexprime . "\n";

my $pre_compile = time;

my $pattern = qr/$regex/;
my $lno = 0;

# Start timing
my $start = time;

while (<STDIN>) {
    $lno = $lno + 1;
    if ($_ =~ $pattern) {
        printf("{'year'='%s', 'month'='%s', 'day'='%s', 'hours'='%s', 'minutes'='%s', 'seconds'='%s', 'tz'='%s'}\n", 
               $1, $2, $3, $4, $5, $6, $7);
    } else {
        print STDERR "match error on line ${lno}\n";
        exit 1;
    }
}

# End timing
my $end = time;
my $elaps = int(($end - $start) * 1000);
my $elaps_compile = int(($start - $pre_compile) * 1000);

print STDERR "\ncompilation (ms): ${elaps_compile}\n";
print STDERR "matching (ms):    ${elaps}\n"
