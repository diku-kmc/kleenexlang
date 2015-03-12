#! env perl
use strict;
use warnings;
use Time::HiRes qw/ time /;

# Perl variant of patho2

my $regex = "^(?:([a-z]*a)|([a-z]*b))?(?:\n)?\$";

my $pre_compile = time;

# NOTE THE ADDED ANCHORS
my $pattern = qr/^$regex$/;
my $lno = 0;

# Start timing
my $start = time;

while (<STDIN>) {
    $lno = $lno + 1;
    if ($_ =~ $pattern) {
        print STDOUT sprintf("%s\n", defined $2? $2 : "");
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
