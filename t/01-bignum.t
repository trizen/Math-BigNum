#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 11;

# Basic operations
{
    use Math::BigNum qw(:constant);

    # Division
    my $x = 1 / 3;
    my $y = $x * 3;
    is("$y", "1");

    # Factorial
    my $fac = ((100->fac + 1) / 2);
    is("$fac",
"46663107721972076340849619428133350245357984132190810734296481947608799996614957804470731988078259143126848960413611879125592605458432000000000000000000000000.5"
      );

    # Division by zero
    my $inf = $x / 0;
    is(ref($inf), 'Math::BigNum::Inf');

    # Negation
    ok($y->neg == -1);

    # Absolute values
    my $n     = 42;
    my $neg_n = -$n;

    is("$neg_n", "-42");
}

# Complex numbers
{
    use Math::BigNum qw(:constant i);

    my $z = 3 + 4 * i;
    is("$z", "3+4i");

    my $z2 = $z + 2;
    is("$z2", "5+4i");

    my $i = sqrt(-1);
    is("$i", 'i');
}

# Power
{
    use Math::BigNum qw();

    my $x = Math::BigNum->new(3);
    my $y = Math::BigNum->new(4);

    # Obj**Obj
    my $z = $x**$y;
    is("$z", 3**4);

    # Obj**Scalar
    my $z2 = $x**2;
    is("$z2", 3**2);

    # Scalar**Obj
    my $z3 = 2**$x;
    is("$z3", 2**3);
}
