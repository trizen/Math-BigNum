#!/usr/bin/perl

# Author: Daniel "Trizen" Șuteu
# License: GPLv3
# Date: 19 September 2015
# Website: https://github.com/trizen

#
## Some simple approximations for the prime counting function.
#

# F(n) =  n^2 / ln(n!)
#      =~ n^2 / (n * ln(n) - n)
#      =~ n / (ln(n) - 1)

use 5.010;
use utf8;
use strict;
use warnings;

use lib qw(../lib);
use Math::BigNum qw(:constant);

#
## Logarithmic integral
#
sub Li {
    my ($n) = @_;
    log($n)->eint;
}

foreach my $n (1 .. 10) {
    my $x = 10**$n;

    my $f1 = ($x**2)->idiv(($x+1)->lngamma);
    my $f2 = Li($x)->int;

    say "PI($x) =~ ", $f1, ' =~ ', $f2;
}
