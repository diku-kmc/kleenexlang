#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 NAME

C<gen_numbers.pl> - Generates lines of text consisting of only as and bs.

=head1 USAGE

Generate a 250mb file

    gen_numbers.pl -s 250000000

=cut

# Number of bytes remaining (approximately)
my $b = undef;
# Probability (in percent) of recursing when generating the tags. Affects depth.
my $r = 80;

GetOptions("size_bytes=i"  => \$b,
           "recursion_probability=i" => \$r);

die("Need to specify desired size of XML file.") unless $b && $b > 0;

sub gen_string {
    my ($chars, $min_length, $max_length) = @_;

    my $length = $min_length + rand($max_length - $min_length);
    my @res;
    push(@res, $chars->[rand @$chars]) for (1 .. $length);

    return join("", @res);
}

sub min { return $_[0] < $_[1] ? $_[0] : $_[1]; }

my $alpha = [ 'a' .. 'z', 'A' .. 'Z' ];
my $alphaspace = [ 'a' .. 'z', 'A' .. 'Z', ' ' x 5 ];

sub gen_xml {
    my $depth = shift;
    my $tag = gen_string($alpha, 1, 10);
    $b -= length($tag)*2 + 4 + 1 + $depth*2 + 2;
    print "\t"x$depth . "<$tag>\n";
    if ($b > 0) {
        if ((rand()*100 < $r)) {
            gen_xml($depth+1);
        } else {
            my $s = "\t"x($depth+1) . gen_string($alphaspace, 0, min(200, $b)) . "\n";
            $b -= length($s);
            print $s;
        }
    }
    print "\t"x$depth . "</$tag>\n";
}

print "\n<root>\n";
$b -= 7+8;

while (1) {
    gen_xml(1);
    last if $b <= 0;
}

print "</root>\n";
