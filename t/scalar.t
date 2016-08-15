#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 202;

use Math::BigNum;

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

my $max_ui = Math::BigNum->new(Math::BigNum::MAX_UI);
my $min_si = Math::BigNum->new(Math::BigNum::MIN_SI);

is($min_si,                                     Math::BigNum::MIN_SI);
is(Math::BigNum->new(Math::BigNum::MIN_SI + 1), Math::BigNum::MIN_SI + 1);
is($max_ui,                                     Math::BigNum::MAX_UI);

is($x->iadd(Math::BigNum::MIN_SI),              $x + $min_si);
is($x->isub(Math::BigNum::MIN_SI),              $x - $min_si);
is($x->imul(Math::BigNum::MIN_SI),              $x * $min_si);
is($min_si->mul(3)->idiv(Math::BigNum::MIN_SI), 3);

is($x->iadd(Math::BigNum::MAX_UI),              $x + $max_ui);
is($x->isub(Math::BigNum::MAX_UI),              $x - $max_ui);
is($x->imul(Math::BigNum::MAX_UI),              $x * $max_ui);
is($max_ui->mul(3)->idiv(Math::BigNum::MAX_UI), 3);

is($x->mod(Math::BigNum::MAX_UI), $x);
is($x->mod(Math::BigNum::MIN_SI), $min_si + $x);

is($x->pow(2)->mod(Math::BigNum::MAX_UI), 1764);
is($x->pow(2)->mod(Math::BigNum::MIN_SI), $min_si + $x**2);

is($max_ui->and(Math::BigNum::MAX_UI), Math::BigNum::MAX_UI);
is($max_ui->ior(Math::BigNum::MAX_UI), Math::BigNum::MAX_UI);
is($max_ui->xor(Math::BigNum::MAX_UI), 0);

is($max_ui <=> Math::BigNum::MAX_UI,     0);
is($max_ui <=> Math::BigNum::MAX_UI- 1,  1);
is($min_si <=> Math::BigNum::MIN_SI,     0);
is($min_si <=> Math::BigNum::MIN_SI + 1, -1);

is(($min_si + 1)->binomial(Math::BigNum::MIN_SI), Math::BigNum::MIN_SI + 1);
is($max_ui->binomial(Math::BigNum::MAX_UI- 1),    Math::BigNum::MAX_UI);

#
## iadd()
#
{
    my $x = Math::BigNum->new(42);
    $x = $x->iadd(Math::BigNum::MAX_UI);
    is($x, 42 + $max_ui);

    $x = Math::BigNum->new(-42);
    $x = $x->iadd(Math::BigNum::MIN_SI);
    is($x, -42 + $min_si);

    $x = Math::BigNum->new(42);
    $x = $x->iadd("3.4");
    is($x, 45);
}

#
## biadd()
#
{
    my $x = Math::BigNum->new(42);
    $x->biadd(Math::BigNum::MAX_UI);
    is($x, 42 + $max_ui);

    $x = Math::BigNum->new(-42);
    $x->biadd(Math::BigNum::MIN_SI);
    is($x, -42 + $min_si);

    $x = Math::BigNum->new(42);
    $x->biadd("3.4");
    is($x, 45);
}

#
## isub()
#
{
    my $x = Math::BigNum->new(42);
    $x = $x->isub(Math::BigNum::MAX_UI);
    is($x, 42 - $max_ui);

    $x = Math::BigNum->new(-42);
    $x = $x->isub(Math::BigNum::MIN_SI);
    is($x, -42 - $min_si);

    $x = Math::BigNum->new(42);
    $x = $x->isub("3.4");
    is($x, 39);
}

#
## bisub()
#
{
    my $x = Math::BigNum->new(42);
    $x->bisub(Math::BigNum::MAX_UI);
    is($x, 42 - $max_ui);

    $x = Math::BigNum->new(-42);
    $x->bisub(Math::BigNum::MIN_SI);
    is($x, -42 - $min_si);

    $x = Math::BigNum->new(42);
    $x->bisub("3.4");
    is($x, 39);
}

#
## imul()
#
{
    my $x = Math::BigNum->new(42);
    $x = $x->imul(Math::BigNum::MAX_UI);
    is($x, 42 * $max_ui);

    $x = Math::BigNum->new(-42);
    $x = $x->imul(Math::BigNum::MIN_SI);
    is($x, -42 * $min_si);

    $x = Math::BigNum->new(2);
    $x = $x->imul(Math::BigNum::MIN_SI);
    is($x, 2 * $min_si);

    $x = Math::BigNum->new(42);
    $x = $x->imul("3.4");
    is($x, 126);
}

#
## bimul()
#
{
    my $x = Math::BigNum->new(42);
    $x->bimul(Math::BigNum::MAX_UI);
    is($x, 42 * $max_ui);

    $x = Math::BigNum->new(-42);
    $x->bimul(Math::BigNum::MIN_SI);
    is($x, -42 * $min_si);

    $x = Math::BigNum->new(2);
    $x->bimul(Math::BigNum::MIN_SI);
    is($x, 2 * $min_si);

    $x = Math::BigNum->new(42);
    $x->bimul("3.4");
    is($x, 126);
}

#
## idiv()
#
{
    my $x = Math::BigNum->new(Math::BigNum::MAX_UI)->mul(3);
    $x = $x->idiv(Math::BigNum::MAX_UI);
    is($x, 3);

    $x = Math::BigNum->new(Math::BigNum::MIN_SI)->mul(3);
    $x = $x->idiv(Math::BigNum::MIN_SI);
    is($x, 3);

    $x = Math::BigNum->new(42);
    $x = $x->idiv("3.4");
    is($x, 14);
}

#
## bidiv()
#
{
    my $x = Math::BigNum->new(Math::BigNum::MAX_UI)->mul(3);
    $x->bidiv(Math::BigNum::MAX_UI);
    is($x, 3);

    $x = Math::BigNum->new(Math::BigNum::MIN_SI)->mul(3);
    $x->bidiv(Math::BigNum::MIN_SI);
    is($x, 3);

    $x = Math::BigNum->new(42);
    $x->bidiv("3.4");
    is($x, 14);
}

#
## idiv()
#
{
    my $x = Math::BigNum->new(124);
    $x = $x->idiv(-4);
    is($x, -31);

    $x = Math::BigNum->new(124);
    $x = $x->idiv(4);
    is($x, 31);

    $x = Math::BigNum->new(-124);
    $x = $x->idiv(-4);
    is($x, 31);

    $x = Math::BigNum->new(-124);
    $x = $x->idiv(4);
    is($x, -31);
}

#
## bidiv()
#
{
    my $x = Math::BigNum->new(124);
    $x->bidiv(-4);
    is($x, -31);

    $x = Math::BigNum->new(124);
    $x->bidiv(4);
    is($x, 31);

    $x = Math::BigNum->new(-124);
    $x->bidiv(-4);
    is($x, 31);

    $x = Math::BigNum->new(-124);
    $x->bidiv(4);
    is($x, -31);
}

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
is('abc'**$x,  $nan);
is($x**'abc',  $nan);
