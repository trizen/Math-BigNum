#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 142;

require Math::BigNum;

my $x = Math::BigNum->new(42);

my $zero = Math::BigNum->zero;
my $nan  = Math::BigNum->nan;
my $ninf = Math::BigNum->ninf;
my $inf  = Math::BigNum->inf;

ok($x->is_div(2));
ok($x->is_div(-2));

is($x - $ninf, $inf);
is($x + $ninf, $ninf);

#
## (BigNum, Scalar)
#
is($x - 3, Math::BigNum->new(42 - 3));
is($x + 3, Math::BigNum->new(42 + 3));
is($x * 2, Math::BigNum->new(42 * 2));
is($x / 2, Math::BigNum->new(42 / 2));

is($x * 'inf',  $inf);
is($x * '-inf', $ninf);
is($x / 'inf',  $zero);
is($x / '-inf', $zero);

is($x + 'inf',  $inf);
is($x + '-inf', $ninf);
is($x - 'inf',  $ninf);
is($x - '-inf', $inf);

is($x**'inf',  $inf);
is($x**'-inf', $zero);

is($x->root('inf'),  Math::BigNum->one);
is($x->root('-inf'), Math::BigNum->one);

#
## (Scalar, BigNum)
#
is('inf' * $x,  $inf);
is('-inf' * $x, $ninf);
is('inf' / $x,  $inf);
is('-inf' / $x, $ninf);

is('inf' + $x,  $inf);
is('inf' - $x,  $inf);
is('-inf' + $x, $ninf);
is('-inf' - $x, $ninf);

is('inf'**$x,        $inf);
is('-inf'**$x,       $inf);     # even power
is('-inf'**($x + 1), $ninf);    # odd power

#
## badd()
#
{
    my $x = Math::BigNum->new(42);

    is($x->badd(3), Math::BigNum->new(42 + 3));
    is($x,          Math::BigNum->new(42 + 3));

    is($x->badd('inf'), $inf);
    is($x,              $inf);

    my $y = Math::BigNum->new(42);
    is($y->badd('-inf'), $ninf);
    is($y,               $ninf);
}

#
## bsub()
#
{
    my $x = Math::BigNum->new(42);

    is($x->bsub(3), Math::BigNum->new(42 - 3));
    is($x,          Math::BigNum->new(42 - 3));

    is($x->bsub('inf'), $ninf);
    is($x,              $ninf);

    my $y = Math::BigNum->new(42);
    is($y->bsub('-inf'), $inf);
    is($y,               $inf);
}

#
## bmul()
#
{
    my $x = Math::BigNum->new(42);

    is($x->bmul(3), Math::BigNum->new(42 * 3));
    is($x,          Math::BigNum->new(42 * 3));

    is($x->bmul('inf'), $inf);
    is($x,              $inf);

    my $y = Math::BigNum->new(42);
    is($y->bmul('-inf'), $ninf);
    is($y,               $ninf);
}

#
## bdiv()
#
{
    my $x = Math::BigNum->new(42);

    is($x->bdiv(3), Math::BigNum->new(42 / 3));
    is($x,          Math::BigNum->new(42 / 3));

    is($x->bdiv('inf'), $zero);
    is($x,              $zero);

    my $y = Math::BigNum->new(42);
    is($y->bdiv('-inf'), $zero);
    is($y,               $zero);
}

#
## bpow()
#
{
    my $x = Math::BigNum->new(42);

    is($x->bpow(2), Math::BigNum->new(42**2));
    is($x,          Math::BigNum->new(42**2));

    is($x->bpow('inf'), $inf);
    is($x,              $inf);

    my $y = Math::BigNum->new(42);
    is($y->bpow('-inf'), $zero);
    is($y,               $zero);
}

#
## Comparisons
#
ok($x < 'inf');
ok(not $x > 'inf');
ok($x <= 'inf');
ok(not $x >= 'inf');
ok(not $x == 'inf');

ok(not $x < '-inf');
ok($x > '-inf');
ok(not $x <= '-inf');
ok($x >= '-inf');
ok(not $x == '-inf');

ok($inf == 'inf');
ok('inf' == $inf);
ok($inf != '-inf');
ok('-inf' != $inf);
ok($ninf == '-inf');
ok('-inf' == $ninf);

ok(not $inf == '-inf');
ok(not '-inf' == $inf);
ok(not $ninf != '-inf');
ok(not '-inf' != $ninf);

is($x <=> 'inf',  -1);
is($x <=> '-inf', 1);

ok(not $inf < 'inf');
is($inf <=> 'inf',      0);
is($inf->acmp('inf'),   0);
is($ninf->acmp('inf'),  0);
is($ninf->acmp('-inf'), 0);
is($inf->acmp('-inf'),  0);
is($inf->acmp(42),      1);
is($ninf->acmp(42),     1);

ok($x == 42);
ok(42 == $x);

ok($inf == 'inf');
ok($ninf == '-inf');
ok('-inf' == $ninf);
ok(not 'inf' == $ninf);
ok(not $ninf == 'inf');
ok(not $x == 'inf');

ok($x != 'inf');
ok(not $x != 42);
ok('-inf' != $x);
ok($inf != '-inf');
ok(not 'inf' != $inf);
ok(not $ninf != '-inf');

ok($x < 'inf');
ok('-inf' < $x);
ok(not $x < '-inf');
ok(not 'inf' < $x);

ok($x > '-inf');
ok('inf' > $x);
ok(not $x > 'inf');
ok(not '-inf' > $x);

ok('-inf' <= $x);
ok($x <= 'inf');
ok(not $x <= '-inf');
ok(not 'inf' <= $x);

ok($x >= '-inf');
ok('inf' >= $x);
ok(not $x >= 'inf');
ok(not '-inf' >= $x);

#
## NaN
#

is($x + 'nan',    $nan);
is('nan' - $x,    $nan);
is('NaN' / $x,    $nan);
is($x / 'NaN',    $nan);
is($x * 'NaN',    $nan);
is('NaN' / $x,    $nan);
is('nan' * $inf,  $nan);
is('nan' / $inf,  $nan);
is($ninf / 'nan', $nan);
is('nan' + $nan,  $nan);
is('nan'**$inf,   $nan);
is($inf**'nan',   $nan);
is($ninf**'nan',  $nan);

#
## This is, somewhat, undefined behavior, so please don't rely on it!
#

is(Math::BigNum->new('abc'), $nan);

is($x + 'abc', $nan);
is('abc' + $x, $nan);
is('abc' - $x, $nan);
is('abc' * $x, $nan);
is($x * 'abc', $nan);
is($x / 'abc', $nan);
is('abc' / $x, $nan);

#is('abc'**$x, $nan);
#is($x**'abc', $nan);
