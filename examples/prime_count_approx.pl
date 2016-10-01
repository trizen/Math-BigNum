#!/usr/bin/perl

# Author: Daniel "Trizen" Șuteu
# License: GPLv3
# Date: 19 September 2015
# Website: https://github.com/trizen

#
## A naive approximation for the prime counting function.
#

# F(n) =  n^2 / ln(n!)
#      =~ n^2 / (n * ln(n) - n)
#      =~ n / (ln(n) - 1)

use 5.010;
use utf8;
use strict;
use warnings;

use lib qw(../lib);
use Math::BigNum qw(:constant e pi);

# pi(n) =~ n / (log(n) - log(n) / log(n/e))
sub pi_approx {
    my ($n) = @_;
    $n / ($n->log - $n->log($n / e));
}

# Ramanujan's approximation for ln(n!)
sub lnfac {
    my ($n) = @_;
    $n * log($n) - $n + log($n * (1 + 4 * $n * (1 + 2 * $n))) / 6 + log(pi) / 2;
}

foreach my $n (1 .. 26) {
    my $x = 10**$n;
    my $f = $n <= 6 ? ($x**2)->idiv(int log $x->fac) : int($x**2 / lnfac($x));
    say "PI($x) =~ ", pi_approx($x)->int, ' =~ ', $f;
}
