#! env perl
use strict;
use warnings;
use Time::HiRes qw/ time /;

# Perl variant of the email validator

# Differences from the Python and Kleenex version:
#   the $ sign in the first and second charclass is escaped with a \
#   the dot in the +-group after the @-sign is escaped with \\, not just \
my $regexprime = "[a-z0-9!#\$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#\$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?";
my $regex = "^(" . $regexprime . ")\$";

my $pre_compile = time;

my $pattern = qr/$regex/;

# Start timing
my $start = time;

while (<STDIN>) {
    if ($_ =~ $pattern) {
        print STDOUT "$1\n";
    }
}

# End timing
my $end = time;
my $elaps = int(($end - $start) * 1000);
my $elaps_compile = int(($start - $pre_compile) * 1000);

print STDERR "\ncompilation (ms): ${elaps_compile}\n";
print STDERR "matching (ms):    ${elaps}\n"
