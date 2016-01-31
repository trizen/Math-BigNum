#!perl

use strict;
use warnings;

use Test::More tests => 14;

use Math::BigNum qw(:constant);

is(2**255, '578960446186580977117854925043439539266' . '34992332820282019728792003956564819968', '2 ** 255');

{
    no warnings 'portable';    # protect against "non-portable" warnings

    # hexadecimal constants
    is(0x123456789012345678901234567890, Math::BigNum->new('123456789012345678901234567890', '16'), 'hexadecimal constant');

    # binary constants
    is(0b01010100011001010110110001110011010010010110000101101101,
        Math::BigNum->new('01010100011001010110110001110011010010010110000101101101', '2'),
        'binary constant');

    # octal constants
    is(0443212636110022150531704401106425474220,
        Math::BigNum->new('443212636110022150531704401106425474220', '8'),
        'octal constant');

}

my $x = 2 + 4.5;    # BigNum 6.5
is("$x", "6.5");

$x = 2**512 * 0.1;    # really is what you think it is
is("$x",
        "1340780792994259709957402499820584612747936582"
      . "0592393377723561443721764030073546976801874298"
      . "1669034276900318581864860508537538828119465699"
      . "4643364900608409.6");

my $inf = Inf * Inf;
like($inf, qr/^inf/i);

my $ninf = Inf * -1;
like($ninf, qr/^-inf/i);

my $nan = 3 * NaN;
is("$nan", "NaN");

$nan = Inf / Inf;
is("$nan", "NaN");

$nan = NaN + 2;
is("$nan", "NaN");

$nan = NaN / 2;
is("$nan", "NaN");

$nan->bone;
is("$nan", "1");

like(1.0 / 3.0, qr/^0.333333333/, '1.0 / 3.0 = 0.333333333...');
