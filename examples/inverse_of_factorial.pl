#!/usr/bin/perl

# Daniel "Trizen" Șuteu
# License: GPLv3
# Date: 18 September 2016
# Website: https://github.com/trizen

# The inverse of n factorial, based on the inverse of Stirling approximation,
# computed with the `lgrt()` method, which calculates the logarithmic-root of n.

use 5.010;
use strict;
use warnings;

use Math::BigNum qw(:constant factorial pi e);

use constant S => (2 * pi)**(-1 / (2 * e));

sub inverse_of_factorial {
    (S * $_[0]->root(e))->lgrt * e - 0.5;
}

# Run some tests

foreach my $n (50 .. 60) {
    my $i = inverse_of_factorial(factorial($n));

    printf("F(%2s!) =~ %s\n", $n, $i);

    if ($i->round(0) != $n) {
        warn "However that is incorrect! (expected: $n)";
    }
}
