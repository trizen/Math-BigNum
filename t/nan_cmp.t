#!perl

# test that overloaded compare works when NaN are involved

use strict;
use warnings;

use Test::More tests => 13;

use Math::BigNum;

my $nan = Math::BigNum->nan();
my $one = Math::BigNum->new(1);

is($one, $one, "bone() == bone()");

ok($one != $nan, "bone() != bnan()");
ok($nan != $one, "bnan() != bone()");
ok($nan != $nan, "bnan() != bnan()");

ok(!($nan == $one), "bnan() == bone()");
ok(!($one == $nan), "bone() == bnan()");
ok(!($nan == $nan), "bnan() == bnan()");

ok(!($nan <= $one), "bnan() <= bone()");
ok(!($one <= $nan), "bone() <= bnan()");
ok(!($nan <= $nan), "bnan() <= bnan()");

ok(!($nan >= $one), "bnan() >= bone()");
ok(!($one >= $nan), "bone() >= bnan()");
ok(!($nan >= $nan), "bnan() >= bnan()");
