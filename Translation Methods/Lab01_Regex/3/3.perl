#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

my @lines = <STDIN>;
my $lin_pat = "<\\s*a.* href=\"\\s*([^\\ \"]*)\\s*\".*>";
# my $uri = "(([^:\\/?#]+):\\/\\/)?(([^\\/?\\s#:]+))(:(\\d+))?([^?\\s#]*)(\\?([^\\s#]*))?(#(.*))?";
my $uri_pat = "(([^:\\/?#]+):)?(\\/\\/([^\\/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?";
my $auth_pat = "(\\S+\\@)?([^\\/?#:]+)(:(\\d+))?";

my %hash = ();
foreach (@lines) {
    if (/$lin_pat/) {
        my $link = $1;
        if ($link =~ /$uri_pat/) {
            my $auth = $4 // $link;
            if ($auth =~ /^$auth_pat$/) {
                my $host = $2;
                # print "$auth -> $host\n";
                $hash{$host} = 1;
            }
        }
    }
}

my @res = ();
foreach my $key (keys %hash) {
    push(@res, $key);
}

@res = sort @res;
foreach (@res) {
    print "$_\n";
}

