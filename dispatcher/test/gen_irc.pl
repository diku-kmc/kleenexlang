#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;

=head1 NAME

C<gen_irc.pl> - Generates IRC commands.

=head1 USAGE

Generate 250MB IRC commands

    gen_irc.pl -b 250000000

=cut

# Size in bytes
my $b = undef;

GetOptions("size_bytes=i" => \$b);

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

sub gen_prefix {
    my $servername;
    if (rand() < 0.5) {
        $servername = gen_nickname();
    }
    else {
        $servername = gen_hostname();
    }

    if (rand() < 0.2) {
        return "";
    }
    else {
        return ":" . $servername . " ";
    }
}

sub gen_params {
    my $params;
    my $max = gen_int(0,14)->();
    for (my $i = 0; $i < $max; $i++) {
        $params .= " " . gen_name(5,20)->();
    }
    if (rand() < 0.5) {
        $params .= " :" . "asdf:foo:bar";
    }
}

sub gen_hostname {
    if (rand() < 0.5) {
        return gen_ip()->();
    }
    my $hostname = "";
    my $max = gen_int(1,5)->();
    for  (my $i = 0; $i < $max; $i++) {
        $hostname .= gen_name(2,8)->() . ".";
    }
    return $hostname . "dk";
}

sub gen_nickname {
    my $res = gen_name(1, 8)->();
    if (rand() < 0.75) {
        if (rand() < 0.5) {
            $res .= "!" . gen_name(5, 10)->();
        }
        $res .= "@" . gen_hostname();
    }
    return $res;
}

sub gen_message {
    my $prefix = gen_prefix();
    my $command;
    if (rand() < 0.5) {
        $command = $alpha[rand @alpha];
    }
    else {
        $command = sprintf("%d", gen_int(100,999)->());
    }
    my $params = gen_params();

    return sprintf("%s%s%s\r\n", $prefix, $command, $params);
}

sub gen_ip {
    my $segment = gen_int(0, 255);

    return sub {
        return sprintf("%d.%d.%d.%d", $segment->(), $segment->(), $segment->(), $segment->());
    }
}

while (1) {
    last if (defined($b) && $b <= 0);

    my $line = gen_message();
    print $line;
    $b -= length $line if defined($b);
}
