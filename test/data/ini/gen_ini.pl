#!/usr/bin/env perl
use 5.012;
use warnings;

my @comm_chars = ( 'a' .. 'z', 'A' .. 'Z', '0' .. '9', '=', ' ' );
my @sect_chars = ( 'a' .. 'z', 'A' .. 'Z', '0' .. '9', '=', '_', "-" );
my @key_chars = ( 'a' .. 'z', 'a' .. 'z', '0' .. '9', '=', '_', "-" );
my @value_chars = ( 'a' .. 'z', 'a' .. 'z', '0' .. '9', '=', '_', "-", ' ' );
my @value_chars_quot = ( 'a' .. 'z', 'a' .. 'z', '0' .. '9', '=', '_', "-", ' ');

sub comment {
    if (rand() < 0.9) {
        print ";";
        for my $i (1 .. rand(200)) {
            print $comm_chars[rand @comm_chars];
        }
        print "\n";
    } else {
        print " "x(rand(100)) . "\n";
    }
}

sub comments {
    if (rand() > 0.3) {
        for my $i (1 .. rand(5)) { comment; }
    }
}

sub section {
    print "[";
    for my $i (1 .. rand(50)) {
        print $sect_chars[rand @sect_chars];
    }
    print "]\n";
}

sub keyvalue {
    for my $i (1 .. rand(30)) {
        print $key_chars[rand @key_chars];
    }
    print " "x(rand(3)) . "=" . " "x(rand(3));
    if (rand() < 0.7) {
        for my $i (1 .. rand(100)) {
            print $value_chars[rand @value_chars];
        }
    } else {
        print '"';
        for my $i (1 .. rand(100)) {
            print $value_chars_quot[rand @value_chars_quot];
        }
        print '"';
    }
    print "\n";
}

while (1) {
    comments;
    section;

    for my $i (1 .. rand(20)) {
        comment if (rand() < 0.3);
        keyvalue;
    }
}
