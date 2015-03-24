#!/usr/bin/env perl
use 5.012;
use warnings;
use Time::HiRes qw/ time /;

my $pre_compile = time;

my $ipRx = qr/\d+\.\d+\.\d+\.\d+/;
my $quotedRx = qr/"(?:[^"\n]|\\")*"/;

my $loglineRx = qr/^
    ($ipRx) \s+              # host
    - \s+                    # user
    [^\s\n]+ \s+             # authuser
    \[ ([^\n\]]*) \] \s+      # timestamp
    ($quotedRx) \s+          # request
    (\d+) \s+                # response code
    (\d+) \s+                # response size in bytes
    ($quotedRx) \s+          # referer
    ($quotedRx) \s*          # user agent
$/x;

# Start timing
my $start = time;

print "[";
my $first = 1;
my $i = 0;

for my $line (<STDIN>) {
    $i++;

    my ($host, $timestamp, $request, $code, $bytes, $referer, $useragent) =
        $line =~ $loglineRx;

    die("Error matching on line $i.\n") unless defined($host);

    print ",\n" unless $first;
    $first = 0;
    print qq#{"host":"$host","date":"$timestamp","request":$request,"status":"$code","size":"$bytes","url":$referer,"agent":$useragent}#;
}

print "]\n";

# End timing
my $end = time;
my $elaps = int(($end - $start) * 1000);
my $elaps_compile = int(($start - $pre_compile) * 1000);

print STDERR "\ncompilation (ms): ${elaps_compile}\n";
print STDERR "matching (ms):    ${elaps}\n"
