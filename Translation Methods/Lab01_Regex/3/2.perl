#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

my @lines = <STDIN>;
my $length = @lines;
my $i = 0;
for $i (0 .. $#lines) {
    my $line = $lines[$i];
    my $remove_html = $line =~ s/<[^>]*>//gr;
    $lines[$i] = $remove_html;
}
$i++ until ($i == $length || $lines[$i] =~ /\S/);
splice(@lines, 0, $i);

$length = @lines;
$i = $length - 1;
$i-- until ($i == -1 || $lines[$i] =~ /\S/);
if ($i != -1) {
    splice(@lines, $i + 1, $length - $i - 1);
}
my @res = ();
my $last_was_empty = 0;
foreach (@lines) {
    if ($_ =~ /^\s*$/) {
        if ($last_was_empty == 0) {
            $last_was_empty = 1;
            push(@res, "\n");
        }
    } else {
        $last_was_empty = 0;
        my $one_space = $_ =~ s/ +/ /gr;
        $one_space = $one_space =~ s/\n//r;
        push(@res, $one_space =~ s/^ ?(.*?) ?$/$1/r);
        push(@res, "\n");
    }
}
foreach (@res) {
    print $_;
}

