#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 15;

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

# Trigonometric functions
{
    use Math::BigNum qw(:constant i);

    my $x = atan2(4 + 5 * i, 3 + 0 * i);
    my $y = atan2(4 + 5 * i, 3);

    ok($x == $y);
}

{
    use Math::BigNum qw(:constant i);

    my $x = 1 + 1 * i;
    my $y = 3.14159 + 1.25 * i;

    my $z = $x + $y;
    is("$z", "4.14159+2.25i");

    $z = $x * $y;
    is("$z", "1.89159+4.39159i");

    $z = -$x;
    is("$z", "-1-i");

    $z = 1 / $x;
    is("$z", "0.5-0.5i");

    $z = $x->conj;
    is("$z", "1-i");

    $z += 1;
    is("$z", "2-i");

    $z *= 3 + 4 * i;
    is("$z", "10+5i");

    $z /= 5;
    is("$z", "2+i");

    $z -= -9 + 3 * i;
    is("$z", "11-2i");
}
