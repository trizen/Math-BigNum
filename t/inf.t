#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 56;

use Math::BigNum;

my $x = Math::BigNum->new(42);
my $y = Math::BigNum->new(-42);
my $z = Math::BigNum::Complex->new(3, 4);

my $inf  = Math::BigNum->inf;
my $ninf = Math::BigNum->ninf;

ok($inf->neg == $ninf);
ok($inf == $ninf->neg);
ok($inf + 3 == $inf);
ok(3 + $inf != $ninf);

like("$inf",  qr/^inf/i);
like("$ninf", qr/^-inf/i);

#
## atan(Inf) == pi/2
#
my $pio2 = $inf->atan;
is(ref($pio2), 'Math::BigNum');
like("$pio2", qr/^1\.570/);

my $p = $inf * $ninf;
like("$p", qr/^-inf/i);

$p = $inf * $inf;
like("$p", qr/^inf/i);

$p = $ninf * $ninf;
like("$p", qr/^inf/i);

like("$inf",  qr/^inf/i);
like("$ninf", qr/^-inf/i);

$p = $ninf * $inf;
like("$p", qr/^-inf/i);

{    # Infinity <=> Scalar
        # Scalar <=> Infinity
    ok($inf > 3);
    ok($inf >= 0);
    ok($ninf < 0);
    ok($ninf <= -1);
    ok($ninf < $inf);
    ok($inf >= $ninf);
    ok($inf > $ninf);
    ok(3 < $inf);
    ok(3 <= $inf);
    ok(3 >= $ninf);
    ok(-2 > $ninf);
    is($inf <=> $inf,  0);
    is($inf <=> $ninf, 1);
    is($ninf <=> $inf, -1);
    is($inf <=> 3,     1);
    is($ninf <=> -3,   -1);
    is($ninf <=> 3,    -1);
    is(3 <=> $inf,     -1);
    is(3 <=> $ninf,    1);
    is(-3 <=> $ninf,   1);
    is(-3 <=> $inf,    -1);
}

{    # Infinity <=> BigNum

    # BigNum <=> Infinity
    use Math::BigNum qw(:constant);

    ok(Inf > 3);
    ok(Inf >= 0);
    ok(-Inf < 0);
    ok(-Inf <= -1);
    ok(-Inf < Inf);
    ok(Inf >= -Inf);
    ok(Inf > -Inf);
    ok(3 < Inf);
    ok(3 <= Inf);
    ok(3 >= -Inf);
    ok(-2 > -Inf);
    is(Inf <=> Inf,  0);
    is(Inf <=> -Inf, 1);
    is(-Inf <=> Inf, -1);
    is(Inf <=> 3,    1);
    is(-Inf <=> -3,  -1);
    is(-Inf <=> 3,   -1);
    is(3 <=> Inf,    -1);
    is(3 <=> -Inf,   1);
    is(-3 <=> -Inf,  1);
    is(-3 <=> Inf,   -1);
}
