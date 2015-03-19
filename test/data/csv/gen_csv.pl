#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 NAME

C<gen_csv.pl> - Generates a CSV file. 

=head1 USAGE

Generate a 250mb file of format 1

    gen_csv.pl -s 250000000 -f 1

Generate an infinite stream of lines of format 1

    gen_csv.pl -f 1

=cut

# Size in bytes
my $b = undef;
my $format = 1;
my @fields;

GetOptions("size_bytes=i" => \$b,
           "format=i"     => \$format);

my @alpha = ('a' .. 'z', 'A' .. 'Z');
my @digit = ('0' .. '9');

sub gen_int {
    my $min = shift // 0;
    my $max = shift // 10000000;

    return sub {
        return int($min + rand() * ($max-$min));
    };
}

sub gen_name {
    my $min = shift // 6;
    my $max = shift // 25;

    return sub {
        my $name = "";
        for my $i ($min .. $max) {
            $name .= $alpha[rand @alpha];
        }
        return $name;
    }
}

sub gen_mail {
    my $first = gen_name(5, 25);
    my $second = gen_name(10, 30);
    my $tld = gen_name(2, 4);

    return sub {
        return sprintf("%s@%s.%s", $first->(), $second->(), $tld->());
    }
}

sub gen_ip {
    my $segment = gen_int(0, 255);

    return sub {
        return sprintf("%d.%d.%d.%d", $segment->(), $segment->(), $segment->(), $segment->());
    }
}

if ($format == 1) {
    @fields = (gen_int, gen_name, gen_name, gen_mail, gen_name, gen_ip);
} else {
    die("Unknown format $format");
}

while (1) {
    last if (defined($b) && $b <= 0);

    my $line = join(",", map { $_->() } @fields) . "\n";
    print $line;
    $b -= length $line if defined($b);
}
