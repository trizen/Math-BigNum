#!/usr/bin/perl

# Author: Daniel "Trizen" Șuteu
# License: GPLv3
# Date: 21 September 2015
# Website: https://github.com/trizen

# Recursive computation of Bernoulli numbers.

# See: https://en.wikipedia.org/wiki/Bernoulli_number#Recursive_definition
#      https://en.wikipedia.org/wiki/Binomial_coefficient#Recursive_formula

use 5.010;
use strict;
use warnings;

use lib qw(../lib);
use Memoize qw(memoize);
use Math::BigNum qw(:constant);

memoize('bernoulli_number');

no warnings qw(recursion);

#sub binomial {
#    my ($n, $k) = @_;
#    $k == 0 || $n == $k ? 1.0 : binomial($n - 1, $k - 1) + binomial($n - 1, $k);
#}

sub bernoulli_number {
    my ($n) = @_;

    return 1 / 2 if $n == 1;
    return 0 / 1 if $n % 2;

    my $bern = 1;
    foreach my $k (0 .. $n - 1) {
        $bern -= bernoulli_number($k) * Math::BigNum::binomial($n, $k) / ($n - $k + 1);
    }
    $bern;
}

for (my $i = 0 ; $i <= 100 ; $i += 2) {
    printf "B%-2d = %s\n", $i, bernoulli_number($i)->as_rat;
}
