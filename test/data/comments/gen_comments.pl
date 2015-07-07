#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 NAME

C<gen_numbers.pl> - Generates lines of text consisting of only as and bs.

=head1 USAGE

Generate a 250mb file, with average line length of 1000 characters, with 20% of lines being comments

    gen_comments.pl -l 1000 -s 250000000 -c 20

Generate a 1000 line file, with average line length of 10000 characters

    gen_comments.pl -l 10000 -n 1000

Generate an endless stream of lines with average length of 500 characters

    gen_comments.pl -l 500
=cut

# Average line length
my $avglen = 1000;
# Number of lines remaining 
my $n = undef;
# Number of bytes remaining (approximately)
my $b = undef;
# Perecentage of lines that are comments
my $comm = 25;

GetOptions("length=i" => \$avglen,
           "num=i"    => \$n,
           "size_bytes=i"  => \$b,
           "comm_percent=i" => \$comm);
my $lastnl = 1;

my @chars = ('A' .. 'Z', 'a' .. 'z', ' ' x 5, '_', '.');

while (1) {
    last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));

    if ($lastnl && int(rand()*100) < $comm) {
        print "//";
        $b -= 2 if defined $b;
    }

    $b -= 1 if defined $b;

    my $i = int(rand() * ($avglen + 1));
    if ($i > 0 || $lastnl) {
        print $chars[rand @chars];
        $lastnl = 0;
    } else {
        print "\n";
        $n -= 1 if defined $n;
        $lastnl = 1;
    }

    last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));
}

# Always add a newline at the end.
print "\n" if (!$lastnl);
