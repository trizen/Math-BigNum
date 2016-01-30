#!/usr/bin/perl

# Author: Daniel "Trizen" Șuteu
# License: GPLv3
# Date: 19 September 2015
# Website: https://github.com/trizen

#
## A naive approximation for the prime counting function.
#

# F(n) = n^2 / ln(n!)

use 5.010;
use utf8;
use strict;
use warnings;

use lib qw(../lib);
use Math::BigNum qw(:constant);

foreach my $n (1 .. 6) {
    my $x = 10**$n;
    my $f  = ($x**2)->idiv(int log $x->fac);
    say "PI($x) =~ $f";
}
