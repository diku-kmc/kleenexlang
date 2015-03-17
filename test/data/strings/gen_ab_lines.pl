#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 NAME

C<gen_ab_lines.pl> - Generates lines of text consisting of only as and bs.

=head1 USAGE

Generate a 250mb file, with average line length of 1000 characters

    gen_ab_lines.pl -l 1000 -b 250000000

Generate a 1000 line file, with average line length of 10000 characters

    gen_ab_lines.pl -l 10000 -n 1000

Generate an endless stream of lines with average length of 500 characters

    gen_ab_lines.pl -l 500

=cut

# Average line length
my $avglen = 1000;
# Number of lines emitted
my $n = undef;
# Number of bytes emitted (approximately)
my $b = undef;

GetOptions("length=i" => \$avglen,
           "num=i"    => \$n,
           "bytes=i"  => \$b);

while (1) {
    last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));

    my $i = int(rand() * (2 * $avglen + 2));

    $b -= 1 if defined $b;

    if ($i < $avglen) {
        print "a";
    } elsif ($i < 2*$avglen) {
        print "b";
    } else {
        print "\n";
        $n -= 1 if defined $n;

        last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));
    }
}
