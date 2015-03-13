#!/usr/bin/env perl
use 5.012;
use warnings;

=head1 NAME

C<gen_emails.pl> - Generates emails; some valid, some not valid.

=cut

sub gen_string {
    my ($chars, $min_length, $max_length) = @_;

    my $length = $min_length + rand($max_length - $min_length);
    my @res;
    push(@res, $chars->[rand @$chars]) for (1 .. $length);

    return join("", @res);
}

my $recip_chars  = [ 'a' .. 'z', '0' .. '9', split //, qq{!#\$%&'*+/=?^_`{|}~-} ];
my $domain_chars = [ 'a' .. 'z', '0' .. '9', '-' ];

sub gen_domain_part {
    my ($min, $max) = @_;
    my $part;
    do {
        $part = gen_string($domain_chars, $min, $max);
    } while ($part =~ /^-|-$/);

    return $part;
}

sub gen_email {
    my $email = gen_string($recip_chars, 1, 30);

    for my $i (1 .. rand(4)) {
        $email .= '.';
        $email .= gen_string($recip_chars, 1, 30);
    }

    $email .= '@';

    $email .= gen_domain_part(1, 30);

    for my $i (1 .. (rand(4)+1)) {
        $email .= '.';
        $email .= gen_domain_part(1, 30);
    }

    return $email;
}

my $any_char = [ map { chr($_) } grep { $_ != 13 && $_ != 10 } (0 .. 255) ];

sub mutate {
    my $mail = shift;

    # Make 1 out of 5 emails invalid
    if (rand() < 0.2) {
        # Mutate 1 out of 20 chars into something random
        $mail =~ s/(.)/rand() < 0.05 ? $any_char->[rand @$any_char] : $1/eg;
    }

    return $mail;
}

while (1) {
    printf("%s\n", mutate(gen_email));
}
