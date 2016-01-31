#!perl -T

use 5.006;
use strict;
use warnings;

use Test::More tests => 17;

use Math::BigNum qw(:constant PI);

my $d = 45;
my $r = PI / 4;

sub rad2deg {
    180 / PI * $_[0];
}

sub deg2rad {
    PI / 180 * $_[0];
}

is(ref(sin(13)), 'Math::BigNum');
is(ref(cos(13)), 'Math::BigNum');

like(sin($r), qr/^0\.7071067811865/);
like(cos($r), qr/^0\.7071067811865/);

like(sin(deg2rad($d)), qr/^0\.7071067811865/);
like(cos(deg2rad($d)), qr/^0\.7071067811865/);

is($r->tan, "1");
is($r->cot, "1");

my $asin = $r->sin->asin;
is($asin,          PI / 4);
is(rad2deg($asin), $d);

my $acos = $r->cos->acos;
is($acos,          PI / 4);
is(rad2deg($acos), $d);

my $atan = $r->tan->atan;
is($atan,          PI / 4);
is(rad2deg($atan), $d);

my $acot = $r->cot->acot;
is($acot,          PI / 4);
is(rad2deg($acot), $d);

like(4 * atan2(1, 1), qr/^3.14159/);
