#!perl

use strict;
use warnings;

use Test::More tests => 6 * 8;

use Math::BigNum;

# 2 ** 240 =
# 1766847064778384329583297500742918515827483896875618958121606201292619776

test_broot('2', '240', 8,  qr/^1073741824\z/);
test_broot('2', '240', 9,  qr/^106528681.309990/);
test_broot('2', '120', 9,  qr/^10321.273240738/);
test_broot('2', '120', 17, qr/^133.32684936327/);

test_broot('2', '120', 8,  qr/^32768\z/);
test_broot('2', '60',  8,  qr/^181.01933598375616624/);
test_broot('2', '60',  9,  qr/^101.59366732596476638/);
test_broot('2', '60',  17, qr/^11.546724616239651532/);

sub test_broot {
    my ($x, $n, $y, $expected) = @_;

    # Test "bpow(BigNum, Scalar)" and "broot(BigNum, Scalar)"
    my $froot = Math::BigNum->new($x)->bpow($n)->broot($y);

    like($froot, $expected, "Try: Math::BigNum->new($x)->bpow($n)->broot($y) == $expected");

    # Test "pow(BigNum, Scalar)" and "root(BigNum, Scalar)"
    like(Math::BigNum->new($x)->pow($n)->root($y), $expected, "Try: Math::BigNum->new($x)->pow($n)->root($y) == $expected");

    # Test "pow(BigNum, BigNum)" and "root(BigNum, BigNum)"
    like(Math::BigNum->new($x)->pow(Math::BigNum->new($n))->root(Math::BigNum->new($y)), $expected);

    # Test "bpow(BigNum, BigNum)" and "broot(BigNum, BigNum)"
    like(Math::BigNum->new($x)->bpow(Math::BigNum->new($n))->broot(Math::BigNum->new($y)), $expected);

    $expected = "$froot";
    $expected =~ s/\..*//;
    $expected = qr/$expected/;

    # Test "bpow" and "biroot"
    like(Math::BigNum->new($x)->bpow($n)->biroot($y),
         $expected, "Try: Math::BigNum->new($x)->bpow($n)->biroot($y) == $expected");

    # Test "pow" and "iroot"
    like(Math::BigNum->new($x)->pow($n)->iroot($y), $expected, "Try: Math::BigNum->new($x)->pow($n)->iroot($y) == $expected");
}
