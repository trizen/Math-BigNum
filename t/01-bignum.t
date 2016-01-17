#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 86;

# Initialization

{
    use Math::BigNum qw();
    my $x = Math::BigNum->new("1010", 2);
    is("$x", "10");

    $x = Math::BigNum->new("ff", 16);
    is("$x", "255");

    $x = Math::BigNum->new(255);
    is($x->in_base(16), "ff");
}

# Basic operations
{
    use Math::BigNum qw(:constant);

    # Division
    my $x = 1 / 3;
    my $y = $x * 3;
    is("$y", "1");

    # as_frac()
    is($x->as_frac, "1/3");

    # Factorial
    my $fac = ((100->fac + 1) / 2);
    is("$fac",
            "46663107721972076340849619428133350245357984132190810"
          . "734296481947608799996614957804470731988078259143126848"
          . "960413611879125592605458432000000000000000000000000.5");

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

    is(10->in_base(2), "1010");
}

# Float
{
    use Math::BigNum qw(:constant);

    my $x = 1.2;
    my $y = 3.4;
    my $z;

    # Addition
    $z = $x + $y;
    is("$z", "4.6");

    # Subtraction
    $z = $y - $x;
    is("$z", "2.2");

    # Multiplication
    $z = $x * $y;
    is("$z", "4.08");

    # Division
    $y += 0.2;
    $z = $y / $x;
    is("$z", "3");

    # Square root
    $z = sqrt(25);
    is("$z", "5");

    # Cube root
    $z = 125->cbrt;
    is("$z", "5");

    # Sqr
    $z = 3->sqr;
    is("$z", "9");

    # Root
    $z = 125->root(3);
    ok("$z" =~ /^5(?:\.000|\z)/);
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

# Comparisons
{
    use Math::BigNum qw(:constant);
    ok(3.2 < 4);
    ok(1.5 <= 1.5);
    ok(2.3 <= 3);
    ok(3 > 1.2);
    ok(3 >= 3);
    ok(9 >= 2.1);
    ok(9 == 9);
    ok(!(3 == 4));
    ok(8 != 3);
    ok(!(4 != 4));

    is(4 <=> 4,     "0");
    is(4.2 <=> 4.2, "0");
    is(3.4 <=> 6.4, "-1");
    is(9.4 <=> 2.3, "1");
}

{
    use Math::BigNum;

    is(4 <=> Math::BigNum->new(4), 0);
    is(3 <=> Math::BigNum->new(4), -1);
    is(4 <=> Math::BigNum->new(3), 1);

    is(Math::BigNum->new(2) <=> 3, -1);
    is(Math::BigNum->new(4) <=> 2, 1);
    is(Math::BigNum->new(3) <=> 3, 0);

    is(Math::BigNum->new(3.4) <=> 3.4, 0);
    is(Math::BigNum->new(8.3) <=> 2.3, 1);
    is(Math::BigNum->new(1.4) <=> 3,   -1);

    is(3.4 <=> Math::BigNum->new(3.4), 0);
    is(2.3 <=> Math::BigNum->new(8.3), -1);
    is(3.1 <=> Math::BigNum->new(1.4), 1);

    ok(Math::BigNum->new(3) > 1);
    ok(Math::BigNum->new(3.4) > 2.3);
    ok(!(Math::BigNum->new(4) > 5));
    ok(!(Math::BigNum->new(4.3) > 5.7));

    ok(3 > Math::BigNum->new(1));
    ok(3.4 > Math::BigNum->new(2.3));
    ok(!(4 > Math::BigNum->new(5)));
    ok(!(4.3 > Math::BigNum->new(5.7)));

    ok(Math::BigNum->new(9) >= 9);
    ok(Math::BigNum->new(4.5) >= 3.4);
    ok(Math::BigNum->new(5.6) >= 5.6);
    ok(!(Math::BigNum->new(4.3) >= 10.3));
    ok(!(Math::BigNum->new(3) >= 21));

    ok(9 >= Math::BigNum->new(9));
    ok(4.5 >= Math::BigNum->new(3.4));
    ok(5.6 >= Math::BigNum->new(5.6));
    ok(!(4.3 >= Math::BigNum->new(10.3)));
    ok(!(3 >= Math::BigNum->new(21)));

    ok(Math::BigNum->new(1) < 3);
    ok(Math::BigNum->new(2.3) < 3.4);
    ok(!(Math::BigNum->new(5) < 4));
    ok(!(Math::BigNum->new(5.7) < 4.3));

    ok(1 < Math::BigNum->new(3));
    ok(2.3 < Math::BigNum->new(3.4));
    ok(!(5 < Math::BigNum->new(4)));
    ok(!(5.7 < Math::BigNum->new(4.3)));

    ok(Math::BigNum->new(9) <= 9);
    ok(Math::BigNum->new(3.4) <= 4.5);
    ok(Math::BigNum->new(5.6) <= 5.6);
    ok(!(Math::BigNum->new(10.3) <= 4.3));
    ok(!(Math::BigNum->new(21) <= 3));

    ok(9 <= Math::BigNum->new(9));
    ok(3.4 <= Math::BigNum->new(4.5));
    ok(5.6 <= Math::BigNum->new(5.6));
    ok(!(12.3 <= Math::BigNum->new(4.3)));
    ok(!(21 <= Math::BigNum->new(3)));
}
