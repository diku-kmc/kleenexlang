#!/usr/bin/env perl
use 5.012;
use warnings;
use Time::HiRes qw/time/;

my $pre_compile = time;

my $pattern = qr{^//};

# Start timing
my $start = time;

for my $line (<>) {
    print $line unless $line =~ $pattern;
}

# End timing
my $end = time;
my $elaps = int(($end - $start) * 1000);
my $elaps_compile = int(($start - $pre_compile) * 1000);

print STDERR "\ncompilation (ms): ${elaps_compile}\n";
print STDERR "matching (ms):    ${elaps}\n"
