#!/usr/bin/perl

use strict;
use warnings;

use 5.014;
use lib 'lib';

use 5.010;
use Math::BigNum qw(:constant);

# Integers and floating-points
say ((100->fac + 1) / 2);
  # prints: 46663107721972076340849619428133350245357984132190810734296481947608799996614957804470731988078259143126848960413611879125592605458432000000000000000000000000.5

# Rational numbers
my $x = 1/3;
say $x*3;         # prints: 1

use Math::BigNum qw(:constant i);

my $z = 3 + 4*i;
say $z;              # prints: "3+4i"
say $z + 2;          # prints: "5+4i"

use Math::BigNum qw(i);

say((Math::BigNum->new(100)->fac + 1) / 2);

my $x = Math::BigNum->new(10);

say $x - 3;
say 3 - $x;

say sqrt(42);

say -$x;

my $inf = $x / 0;
say $inf;

say Math::BigNum->i;
say(i);

my $y = 1 / 3;
say $y* 3;    # prints: 1

{
    use Math::BigNum qw(:constant);
    say(10->fac);
}
