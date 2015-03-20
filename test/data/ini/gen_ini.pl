#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 USAGE

Generate approx. 10MB ini file.

    gen_ini.pl -s 10000000

Generate an endless stream of ini file.

    gen_ini.pl

=cut

my @comm_chars = ( 'a' .. 'z', 'A' .. 'Z', '0' .. '9', '=', ' ' );
my @sect_chars = ( 'a' .. 'z', 'A' .. 'Z', '0' .. '9', '=', '_', "-" );
my @key_chars = ( 'a' .. 'z', 'a' .. 'z', '0' .. '9', '_', "-" );
my @value_chars = ( 'a' .. 'z', 'a' .. 'z', '0' .. '9', '_', "-", ' ' );
my @value_chars_quot = ( 'a' .. 'z', 'a' .. 'z', '0' .. '9', '=', '_', "-", ' ');

sub comment {
    my $r = "";
    if (rand() < 0.8) {
        $r = $r . ";";
        for my $i (1 .. rand(200)) {
            $r = $r . $comm_chars[rand @comm_chars];
        }
        $r = $r . "\n";
    } else {
        $r = $r . " "x(rand(100)) . "\n";
    }
    return $r;
}

sub comments {
    my $r = "";
    if (rand() > 0.3) {
        for my $i (1 .. rand(5)) { $r = $r . comment; }
    }
    return $r;
}

sub section {
    my $r = "";
    $r = $r . "[";
    for my $i (1 .. 10+rand(50)) {
        $r = $r . $sect_chars[rand @sect_chars];
    }
    $r = $r . "]\n";
    return $r;
}

sub keyvalue {
    my $r = "";
    for my $i (1 .. 10+rand(30)) {
        $r = $r . $key_chars[rand @key_chars];
    }
    $r = $r . " "x(rand(3)) . "=" . " "x(rand(3));
    if (rand() < 0.7) {
        for my $i (1 .. rand(100)) {
            $r = $r . $value_chars[rand @value_chars];
        }
    } else {
        $r = $r . '"';
        for my $i (1 .. rand(100)) {
            $r = $r . $value_chars_quot[rand @value_chars_quot];
        }
        $r = $r . '"';
    }
    $r = $r . "\n";
    return $r;
}

# Approx. number of bytes to generate.
my $b = undef;
GetOptions("size_bytes=i" => \$b);

while (1) {
    my $r = comments;
    $r .= section;

    for my $i (1 .. rand(20)) {
        $r .= comment if (rand() < 0.3);
        $r .= keyvalue;
    }

    $b -= length($r) if (defined $b);
    print $r;

    last if (defined $b && $b <= 0);
}
