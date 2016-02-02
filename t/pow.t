#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 97;

use Math::BigNum;

my $int1 = Math::BigNum->new(3);
my $int2 = Math::BigNum->new(-4);

#################################################################
# integer

my $r = $int1**$int1;
is("$r", "27");

$r = $int1**4;
is("$r", "81");

$r = 4**$int1;
is("$r", "64");

$r = $int1**$int2;
ok($r == 1 / ($int1**abs($int2)));

$r = (-$int1)**($int2);
ok($r == 1 / ($int1**abs($int2)));

$r = (-$int1)**($int2 - 1);
ok($r == -(1 / ($int1**abs($int2 - 1))));

$r = $int2**(-$int1);
is("$r", "-0.015625");

$r = $int2**(-$int1 + 1);
is("$r", "0.0625");

#################################################################
# float + int

my $float1 = Math::BigNum->new(3.45);
my $float2 = Math::BigNum->new(-5.67);

$r = $float1**$int1;
is("$r", "41.063625");

$r = $float1**$int2;
like("$r", qr/^0\.00705868/);

$r = $float1**$float2;
like("$r", qr/^0\.0008924/);

$r = $float2**$int1;
is("$r", "-182.284263");

$r = $float2**$int2;
like("$r", qr/^0\.00096753/);

$r = $float2**abs($int2);
is("$r", "1033.55177121");

$r = $float1**4;
is("$r", "141.66950625");

$r = $float2**2;
is("$r", "32.1489");

$r = $float2**3;
is("$r", "-182.284263");

$r = $float1**2.34;
like("$r", qr/^18\.13412823/);

$r = $float2**2.25;
is(ref($r), 'Math::BigNum::Complex');
like("$r", qr/^35\.078974175.*?\+35\.078974175.*?i\z/);

$r = 3**$float1;
like("$r", qr/^44\.2658011/);

$r = 1.23**$float2;
like("$r", qr/^0\.309198955/);

$r = 0**$float2;
is(ref($r),  'Math::BigNum::Inf');
is(lc("$r"), 'inf');

$r = Math::BigNum->new(0)**$int2;
is(ref($r),  'Math::BigNum::Inf');
is(lc("$r"), 'inf');

$r = Math::BigNum->new(0)**$int1;
is("$r", "0");

$r = 0**($int2 - 1);
is(lc("$r"), 'inf');

#################################################################
# bpow() -- int

$r = $int1->copy->bpow($int1);
is("$r", "27");

$r = $int1->copy->bpow(4);
is("$r", "81");

$r = $int1->copy->bpow($int2);
ok($r == 1 / ($int1**abs($int2)));

$r = (-$int1)->copy->bpow($int2);
ok($r == 1 / ($int1**abs($int2)));

$r = (-$int1)->copy->bpow($int2 - 1);
ok($r == -(1 / ($int1**abs($int2 - 1))));

$r = $int2->copy->bpow(-$int1);
is("$r", "-0.015625");

$r = $int2->copy->bpow(-$int1 + 1);
is("$r", "0.0625");

#################################################################
# bpow() -- float + int

$r = $float1->copy;
$r->bpow($int1);
is("$r", "41.063625");

$r = $float1->copy;
$r->bpow($int2);
like("$r", qr/^0\.00705868/);

$r = $float1->copy;
$r->bpow($float2);
like("$r", qr/^0\.0008924/);

$r = $float2->copy;
$r->bpow($int1);
is("$r", "-182.284263");

$r = $float2->copy;
$r->bpow($int2);
like("$r", qr/^0\.00096753/);

$r = $float2->copy;
$r->bpow(abs($int2));
is("$r", "1033.55177121");

$r = $float1->copy;
$r->bpow(4);
is("$r", "141.66950625");

$r = $float2->copy;
$r->bpow(2);
is("$r", "32.1489");

$r = $float2->copy;
$r->bpow(3);
is("$r", "-182.284263");

$r = $float1->copy;
$r->bpow(2.34);
like("$r", qr/^18\.13412823/);

$r = $float2->copy;
$r->bpow(2.25);
is(ref($r), 'Math::BigNum::Complex');
like("$r", qr/^35\.078974175.*?\+35\.078974175.*?i\z/);

$r = Math::BigNum->new(0);
$r->bpow(-2);
is(ref($r),  'Math::BigNum::Inf');
is(lc("$r"), "inf");

##############################################################
# real test

{
    use Math::BigNum qw(:constant);

    sub round_nth {
        my ($orig, $nth) = @_;

        my $n = abs($orig);
        my $p = 10**$nth;

        $n *= $p;
        $n += 0.5;

        if ($n == int($n) and $n % 2 != 0) {
            $n -= 0.5;
        }

        $n = int($n);
        $n /= $p;
        $n = -$n if ($orig < 0);

        return $n;
    }

    my @tests = (

        # original | rounded | places
        [+1.6,      +2,        0],
        [+1.5,      +2,        0],
        [+1.4,      +1,        0],
        [+0.6,      +1,        0],
        [+0.5,      0,         0],
        [+0.4,      0,         0],
        [-0.4,      0,         0],
        [-0.5,      0,         0],
        [-0.6,      -1,        0],
        [-1.4,      -1,        0],
        [-1.5,      -2,        0],
        [-1.6,      -2,        0],
        [3.016,     3.02,      2],
        [3.013,     3.01,      2],
        [3.015,     3.02,      2],
        [3.045,     3.04,      2],
        [3.04501,   3.05,      2],
        [-1234.555, -1000,     -3],
        [-1234.555, -1200,     -2],
        [-1234.555, -1230,     -1],
        [-1234.555, -1235,     0],
        [-1234.555, -1234.6,   1],
        [-1234.555, -1234.56,  2],
        [-1234.555, -1234.555, 3],
    );

    foreach my $pair (@tests) {
        my ($n, $expected, $places) = @$pair;
        my $rounded = round_nth($n, $places);

        is(ref($rounded), 'Math::BigNum');
        ok($rounded == $expected);
    }
}
