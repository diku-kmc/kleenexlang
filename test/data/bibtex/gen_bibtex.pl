#!/usr/bin/env perl
use 5.012;
use warnings;
use Getopt::Long;
use List::Util;

=head1 NAME

C<gen_bibtex.pl> - Generates a CSV file.

=head1 USAGE

Generate a 250mb file

    gen_csv.pl -s 250000000

Generate an infinite stream of lines

    gen_csv.pl

=cut

# Size in bytes
my $b = undef;
my $format = 1;
my @fields;

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

sub gen_fields {
    my @fieldnames = ("year", "isbn", "series", "editor", "doi", "booktitle", "url", "publisher",
                      "author", "pages", "volume", "acmid", "address", "organization");
    my $min = shift // 4;
    my $max = shift // scalar @fieldnames;
    return sub {
        my $title = "title = {" . gen_name->() . "},\n";
        my @fields = ($title);
        for my $i ($min .. $max) {
            my $fieldname = $fieldnames[rand @fieldnames];
            push(@fields, $fieldname . " = {" . gen_name->() . "},\n");
        }
        @fields = List::Util::shuffle(@fields);
        return \@fields;
    }
}

sub gen_entry {
    my @headers = ("incollection", "inproceedings", "InProceedings",
                   "INPROCEEDINGS", "book", "article");

    my $header = "@" . $headers[rand @headers] ."{" . gen_name->() . ",\n";
    my @fields = @{gen_fields->()};
    my $res = $header;
    for my $field (@fields) {
        $res .= $field;
    }
    $res .= "}\n";
}

while (1) {
    last if (defined($b) && $b <= 0);

    my $entry = gen_entry();
    print $entry;
    $b -= length $entry if defined($b);
}
