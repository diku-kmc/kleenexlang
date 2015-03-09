#!/usr/bin/env perl
use 5.012;
use warnings;

print "{\n";

my $firstSection = 1;
my $firstKeyValuePair = 1;
my $ind = "    ";

my $i = 0;
while (my $line = <>) {
    $i ++;
    next if ($line =~ qr/^\s*;|^\s*$/);

    if (my ($heading) = $line =~ qr/^\s*\[([^\n\]]*)\]\s*$/) {
        print "\n$ind},\n" unless $firstSection;
        $firstSection = 0;
        $firstKeyValuePair = 1;
        print "$ind\"$heading\": {";
        next;
    }

    die("Needs to start with a section before keys (line $i)") if $firstSection;

    if (my ($key, $value) = $line =~ qr/^\s*([^;\s=\[]*)\s*=\s*([^\n]*)\s*$/) {
        unless ($value =~ qr/^".*"$/) {
            $value =~ s/\\/\\\\/g;
            $value =~ s/"/\\"/g;
            $value = qq{"$value"};
        }

        print "," unless ($firstKeyValuePair);
        $firstKeyValuePair = 0;
        print qq{\n$ind$ind"$key": $value};
    }
}

print "\n$ind}" unless $firstSection;
print "\n}\n";
