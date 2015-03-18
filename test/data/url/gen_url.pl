#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 NAME

C<gen_url.pl> - Generates lines consisting of URLs

=head1 USAGE

Generate 1000 urls 

    gen_url.pl -n 1000

Generate 250000000 bytes worth of URLs

    gen_url.pl -s 250000000

Generate an endless stream of URLs 

    gen_url.pl

=cut

# Number of lines emitted
my $n = undef;
# Number of bytes emitted (approximately)
my $b = undef;

GetOptions("num=i"    => \$n,
           "size_bytes=i"  => \$b);

sub gen_string {
    my ($chars, $min_length, $max_length) = @_;

    my $length = $min_length + rand($max_length - $min_length);
    my @res;
    push(@res, $chars->[rand @$chars]) for (1 .. $length);

    return join("", @res);
}

my $alpha = [ 'a' .. 'z', 'A' .. 'Z' ];
my $digit = [ '0' .. '9' ];
my $safe  = [ split //, '$_.+-' ];
my $extra = [ split //, "!*'()," ];
my $reserved = [ split //, ';/?:@&=' ];
my $hexdigit = [ '0' .. '9', 'a' .. 'f', 'A' .. 'F' ];
my $escape = [ ];

for my $x (@$hexdigit) {
    for my $y (@$hexdigit) {
        push(@$escape, "%$x$y");
    }
}

my $unreserved = [ @$alpha, @$digit, @$safe, @$extra ];
my $uchar = [ @$unreserved, @$escape ];
my $xchar = [ @$unreserved, @$reserved, @$escape ];
my $scheme_chars  = [ 'a' .. 'z', '0' .. '9', '.', '+', '-' ];
my $user_chars = [ @$uchar, qw/; ? & =/ ];
my $domain_chars = [ 'a' .. 'z', '0' .. '9', '-' ];

sub gen_domain_part {
    my ($min, $max) = @_;
    my $part;
    do {
        $part = gen_string($domain_chars, $min, $max);
    } while ($part =~ /^-|-$/);

    return $part;
}


sub gen_host {
    my $res = "";
    if (rand() < 0.2) {
        $res .= gen_string($digit, 1, 3);
        $res .= ".";
        $res .= gen_string($digit, 1, 3);
        $res .= ".";
        $res .= gen_string($digit, 1, 3);
        $res .= ".";
        $res .= gen_string($digit, 1, 3);
    } else {
        for my $i (1 .. rand(4)) {
            $res .= gen_domain_part(1, 20);
            $res .= ".";
        }
        $res .= gen_domain_part(2, 3);
    }
    return $res;
}

sub gen_login {
    my $res = "";
    if (rand() < 0.3) {
        $res .= gen_string($user_chars, 1, 20);
        if (rand() < 0.5) {
            $res .= ":";
            $res .= gen_string($user_chars, 1, 20);
        }
        $res .= "@";
    }
    $res .= gen_host;
    if (rand() < 0.3) {
        $res .= ":";
        $res .= gen_string($digit, 1, 5);
    }

    return $res;
}

sub gen_path {
    if (rand() < 0.2) {
        return "";
    }

    return "/" . gen_string($xchar, 1, 200);
}

sub gen_schemepart {
    if (rand() < 0.05) {
        return gen_string($xchar, 1, 200);
    }

    my $res = "";
    $res .= "//";
    $res .= gen_login;
    $res .= gen_path;

    return $res;
}

sub gen_url {
    my $res = "";

    $res .= gen_string($scheme_chars, 3, 6);
    $res .= ":";
    $res .= gen_schemepart;

    return $res;
}

while (1) {
    my $url = gen_url;
    print "$url\n";

    $n -= 1 if defined $n;
    $b -= 1+length $url if defined $b;

    last if ((defined $n && $n <= 0) || (defined $b && $b <= 0));
}
