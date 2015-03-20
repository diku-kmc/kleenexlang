#!/usr/bin/env perl
use 5.012;
use warnings;
use Time::HiRes qw/time/;

# Perl version of the INI 2 JSON conversion.
# Quite differently structured from the Kleenex version.

my $pre_compile = time;

my $commentRx  = qr/^\s*;|^\s*$/;
my $headingRx  = qr/^\s*\[([^\n\]]*)\]\s*$/;
my $keyValueRx = qr/^\s*([^;\s=\[]*)\s*=\s*([^\n]*?)\s*$/;
my $quotedRx   = qr/^".*"$/;
my $bslashRx   = qr/\\/;
my $quoteRx    = qr/"/;

# Start timing
my $start = time;

print "{\n";

my $firstSection = 1;
my $firstKeyValuePair = 1;
my $ind = "    ";

my $i = 0;
while (my $line = <>) {
    $i ++;
    next if ($line =~ $commentRx);

    if (my ($heading) = $line =~ $headingRx) {
        print "\n$ind},\n" unless $firstSection;
        $firstSection = 0;
        $firstKeyValuePair = 1;
        print "$ind\"$heading\": {";
        next;
    }

    die("Needs to start with a section before keys (line $i)") if $firstSection;

    if (my ($key, $value) = $line =~ $keyValueRx) {
        unless ($value =~ $quotedRx) {
            $value =~ s/$bslashRx/\\\\/g;
            $value =~ s/$quoteRx/\\"/g;
            $value = qq{"$value"};
        }

        print "," unless ($firstKeyValuePair);
        $firstKeyValuePair = 0;
        print qq{\n$ind$ind"$key": $value};
    }
}

print "\n$ind}" unless $firstSection;
print "\n}\n";

# End timing
my $end = time;
my $elaps = int(($end - $start) * 1000);
my $elaps_compile = int(($start - $pre_compile) * 1000);

print STDERR "\ncompilation (ms): ${elaps_compile}\n";
print STDERR "matching (ms):    ${elaps}\n"
