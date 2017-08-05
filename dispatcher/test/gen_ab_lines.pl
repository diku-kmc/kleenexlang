#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 NAME

C<gen_ab_lines.pl> - Generates lines of text consisting of only as and bs.

=head1 USAGE

Generate a 250mb file, with average line length of 1000 characters

    gen_ab_lines.pl -l 1000 -s 250000000

Generate a 1000 line file, with average line length of 10000 characters

    gen_ab_lines.pl -l 10000 -n 1000

Generate an endless stream of lines with average length of 500 characters

    gen_ab_lines.pl -l 500

Generate 42 bytes on one line only with "a".
    
    gen_ab_lines.pl -s 42 -a -n 1

Generate 42 bytes with lines averaging 5 characters only with "b".
    
    gen_ab_lines.pl -s 42 -l 5 -b
=cut

# Average line length
my $avglen = 1000;
# Number of lines emitted
my $n = undef;
# Number of bytes emitted (approximately)
my $b = undef;

my $only_a = undef;
my $only_b = undef;

GetOptions("length=i" => \$avglen,
           "num=i"    => \$n,
           "size_bytes=i"  => \$b,
           "as_only"  => \$only_a,
           "bs_only"  => \$only_b);
my $lastnl = 0;

# Ignore line count if set to only one char.
$n = undef if (defined $only_a || defined $only_b);
# a takes precedence
$only_b = undef if (defined $only_a && defined $only_b);

while (1) {
    last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));

    $b -= 1 if defined $b;
    $lastnl = 0;
    
    if (defined $only_a || defined $only_b) {
        print "a" if (defined $only_a);
        print "b" if (defined $only_b);
    } else {
        my $i = int(rand() * (2 * $avglen + 2));
        if ($i < $avglen) {
            print "a";
        } elsif ($i < 2*$avglen) {
            print "b";
        } else {
            print "\n";
            $lastnl = 1;
            $n -= 1 if defined $n;
        }
    }
    last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));    
}

# Always add a newline at the end.
print "\n" if (!$lastnl);
