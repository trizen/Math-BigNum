#!perl

use strict;
use warnings;

use Test::More tests => 5;

use Math::BigNum qw(:constant);

is(2**255, '578960446186580977117854925043439539266' . '34992332820282019728792003956564819968', '2 ** 255');

{
    no warnings 'portable';    # protect against "non-portable" warnings

    # hexadecimal constants
    is(0x123456789012345678901234567890, Math::BigNum->new('123456789012345678901234567890', '16'), 'hexadecimal constant');

    # binary constants
    is(0b01010100011001010110110001110011010010010110000101101101,
        Math::BigNum->new('0101010001100101011011000111' . '0011010010010110000101101101', '2'),
        'binary constant');

    # octal constants
    is(0443212636110022150531704401106425474220,
        Math::BigNum->new('443212636110022150531704401106425474220', '8'),
        'octal constant');

}

like(1.0 / 3.0, qr/^0.333333333/, '1.0 / 3.0 = 0.333333333...');
