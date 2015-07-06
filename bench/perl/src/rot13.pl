#! env perl
use strict;
use warnings;
use Time::HiRes qw/ time /;

# Perl variant of rot13

# Start timing
my $start = time;

for my $line (<STDIN>) {
    $line =~ tr/a-zA-Z/n-za-mN-ZA-M/;
    print $line;
}

# End timing
my $end = time;
my $elaps = int(($end - $start) * 1000);

print STDERR "matching (ms):    ${elaps}\n"
