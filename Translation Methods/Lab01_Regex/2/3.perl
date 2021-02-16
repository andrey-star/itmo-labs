#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    s/\b[aA]+\b/argh/;
    print;
}
