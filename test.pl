#!/usr/bin/perl

use strict;
use warnings;

use 5.014;
use lib 'lib';

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
