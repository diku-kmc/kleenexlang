#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 NAME

C<gen_numbers.pl> - Generates lines of text consisting of only as and bs.

=head1 USAGE

Generate a 250mb file, with average line length of 1000 characters

    gen_numbers.pl -l 1000 -s 250000000

Generate a 1000 line file, with average line length of 10000 characters

    gen_numbers.pl -l 10000 -n 1000

Generate an endless stream of lines with average length of 500 characters

    gen_numbers.pl -l 500
=cut

# Average line length
my $avglen = 1000;
# Number of lines remaining 
my $n = undef;
# Number of bytes remaining (approximately)
my $b = undef;

GetOptions("length=i" => \$avglen,
           "num=i"    => \$n,
           "size_bytes=i"  => \$b);
my $lastnl = 0;

while (1) {
    last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));

    $b -= 1 if defined $b;

    my $i = int(rand() * ($avglen + 1));
    if ($i > 0 || $lastnl) {
        print int(rand() * 10);
        $lastnl = 0;
    } else {
        print "\n";
        $lastnl = 1;
        $n -= 1 if defined $n;
    }

    last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));
}

# Always add a newline at the end.
print "\n" if (!$lastnl);
