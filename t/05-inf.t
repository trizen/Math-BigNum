#!perl -T

use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 8;

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

ok("$inf" =~ /^inf/i);
ok("$ninf" =~ /^-inf/i);

#
## atan(Inf) == pi/2
#
my $pio2 = $inf->atan;
is(ref($pio2), 'Math::BigNum');
ok("$pio2" =~ /^1\.570/);
