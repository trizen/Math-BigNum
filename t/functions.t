#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 14;

use Math::BigNum qw(
    factorial
    binomial
    fibonacci
    lucas
    ipow
);

is(factorial(0), 1);
is(factorial(5), 120);

is(binomial(42, 39), 11480);
is(binomial(-4, -42), 10660);
is(binomial(-4, 42), 14190);

is(fibonacci(12), 144);
is(lucas(15), 1364);

is(ipow(2, 10), 1024);
is(ipow(-2, 10), 1024);
is(ipow(-2, 11), -2048);
is(ipow(-2, -1), 0);
is(ipow(-2, -2), 0);
is(ipow(-1, -1), -1);
is(ipow(-1, 0), 1);
