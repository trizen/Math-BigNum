#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 5;

# Basic operations
{
    use Math::BigNum qw(:constant i);

    my $z = 3 + 4 * i;

    my $re = $z->re;
    my $im = $z->im;

    is("$re", "3");
    is("$im", "4");

    $z += 2;
    is("$z", "5+4i");

    my $z2 = 1 + 5 * i;
    my $z3 = $z + $z2;
    is("$z3", "6+9i");

    $z3 = $z - $z2;
    is("$z3", "4-i");
}
