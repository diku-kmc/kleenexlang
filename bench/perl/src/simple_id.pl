#! env perl
use strict;
use warnings;
use Time::HiRes qw/ time /;

# Perl variant of simple_id

my $regex = "(.*)";

my $pre_compile = time;

# Note the /s modifier.  This makes dot match newline.
my $pattern = qr/$regex/s;
my $lno = 0;

# Start timing
my $start = time;

while (<STDIN>) {
    $lno = $lno + 1;
    if ($_ =~ $pattern) {
        print STDOUT sprintf("%s", $1);
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
