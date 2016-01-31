#!perl -T

use 5.006;
use strict;
use warnings;

use Test::More tests => 49;

use Math::BigNum qw(:constant);

is(1.0 / 0.0,      Inf);
is(-1.0 / 0.0,     -Inf);
is(0.0 / 0.0,      NaN);
is(-0.0,           0);      # should be -0.0
is(Inf + 1,        Inf);
is(5 - Inf,        -Inf);
is(Inf * 5,        Inf);
is(Inf / 5,        Inf);
is(Inf * 0,        NaN);
is(1.0 / Inf,      0);
is(-1.0 / Inf,     0);      # should be -0.0
is(-Inf == -1 / 0, 1);
is(-Inf * 0,       NaN);
is(0 * -Inf,       NaN);
is(0 * 1 / 0,      NaN);
is(0 / 0,          NaN);
isnt(0 / 0 == 0 / 0, 1);    # NaN != NaN
is(Inf + Inf,     Inf);
is(Inf - Inf,     NaN);
is(Inf * Inf,     Inf);
is(Inf / Inf,     NaN);
is(Inf * 0.0,     NaN);
is(0 < Inf,       1);
is(Inf == Inf,    1);
is(-Inf == -Inf,  1);
is(-Inf <=> Inf,  -1);
is(Inf <=> -Inf,  1);
is(Inf <=> Inf,   0);
is(-Inf <=> -Inf, 0);
is(0 <=> -Inf,    1);
is(NaN + 1,       NaN);
is(NaN * 5,       NaN);
is(NaN - NaN,     NaN);
is(NaN * Inf,     NaN);
is(- NaN,         NaN);
isnt(NaN == NaN, 1);
isnt(NaN > 0,    1);
isnt(NaN < 0,    1);
isnt(NaN == 0,   1);
is(0.0 == -0.0, 1);     # should be false?
is(sin(Inf),    NaN);
is(Inf / -1, -Inf);
is(-Inf + 1e100, -Inf);
is(Inf + -Inf, NaN);
is(-Inf + Inf, NaN);
is(0 * +Inf, NaN);
is(NaN + 1.0, NaN);
is(NaN + NaN, NaN);
is(NaN != NaN, 1);
