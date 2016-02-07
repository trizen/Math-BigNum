#!perl -T

use 5.006;
use strict;
use warnings;

use Test::More tests => 108;

use Math::BigNum qw(:constant);

is(1.0 / 0.0,   Inf);
is(-1.0 / 0.0,  -Inf);
is(0.0 / 0.0,   NaN);
is(-0.0,        0);      # should be -0.0
is(Inf + 1,     Inf);
is(5 - Inf,     -Inf);
is("5" - Inf,   -Inf);
is(Inf * 5,     Inf);
is(Inf * "5",   Inf);
is((Inf) / 5,   Inf);
is((Inf) / "5", Inf);
is(0 * Inf,     NaN);
is(-2 - Inf,    -Inf);
is(-Inf - 2,    -Inf);
is(-Inf - "2",  -Inf);
is("-2" - Inf,  -Inf);
is(-2 + Inf,    Inf);
is(-Inf + 2,    -Inf);
is(-Inf + "2",  -Inf);
is(Inf + -Inf,  NaN);
is(-Inf + Inf,  NaN);
is(Inf + "-2",  Inf);
is("-2" + Inf,  Inf);
is(1 /     (Inf), 0);
is("1.0" / (Inf), 0);
is(-1.0 /  (Inf), 0);    # should be -0.0
is(-Inf == -1 / 0, 1);
is(-Inf * 0,       NaN);
is(0 * -Inf,       NaN);
is("0" * -Inf,     NaN);
is(0 * 1 / 0,      NaN);
is(0 / 0,          NaN);
isnt(0 / 0 == 0 / 0, 1);    # NaN != NaN
is(Inf + Inf,     Inf);
is(Inf - Inf,     NaN);
is(Inf * Inf,     Inf);
is(Inf / Inf,     NaN);
is(Inf * 0.0,     NaN);
is(0 < Inf,       1);
is("0" < Inf,     1);
is(Inf == Inf,    1);
is(-Inf == -Inf,  1);
is(-Inf <=> Inf,  -1);
is(Inf <=> -Inf,  1);
is(Inf <=> Inf,   0);
is(-Inf <=> -Inf, 0);
is(0 <=> -Inf,    1);
is("0" <=> -Inf,  1);
is(NaN + 1,       NaN);
is(NaN + "1",     NaN);
is(NaN * 5,       NaN);
is(NaN - NaN,     NaN);
is(NaN * Inf,     NaN);
is(-NaN,          NaN);
isnt(NaN == NaN, 1);
isnt(NaN > 0,    1);
isnt(NaN < 0,    1);
isnt(NaN == 0,   1);
is(0.0 == -0.0, 1);     # should be false?
is(sin(Inf),    NaN);
is(sin(-Inf),   NaN);
is(cos(Inf),    NaN);
is(cos(-Inf),   NaN);
is(Inf / (-1), -Inf);
is(-Inf + 1e100,  -Inf);
is(Inf + -Inf,    NaN);
is(-Inf + Inf,    NaN);
is(Inf - Inf,     NaN);
is(-Inf - -Inf,   NaN);
is(0 * +Inf,      NaN);
is("0" * +Inf,    NaN);
is(NaN + 1.0,     NaN);
is(NaN + NaN,     NaN);
is(NaN != NaN,    1);
is(abs(Inf),      Inf);
is(abs(-Inf),     Inf);
is(abs(NaN),      NaN);
is(sqrt(Inf),     Inf);
is(sqrt(-Inf),    Inf);    # should be NaN?
is((-Inf)->isqrt, Inf);    # =//=
is(Inf->erfc,     0);
is((-Inf)->erfc,  2);
is(Inf->fac,      Inf);
is((-Inf)->fac,   NaN);
is((-1.01)->acos, NaN);
is(1.01->acos,    NaN);
is((-1.01)->asin, NaN);
is(1.01->asin,    NaN);
is(sqrt(-1),      NaN);
is(Inf**NaN,      NaN);
is(NaN**Inf,      NaN);

# BigNum
is((Inf)->root(-12),   0);
is((Inf)->iroot(-12),  0);
is((-Inf)->root(-12),  0);
is((Inf)->root(2),     Inf);
is((Inf)->iroot(2),    Inf);
is((-Inf)->root(2),    Inf);    # sqrt(-Inf) -- shouldn't be NaN?
is((Inf)->root(Inf),   1);
is((-Inf)->root(Inf),  1);
is((Inf)->root(-Inf),  1);
is((-Inf)->root(-Inf), 1);

# Scalar
is((Inf)->root("-12"),  0);
is((Inf)->iroot("-12"), 0);
is((-Inf)->root("-12"), 0);
is((Inf)->root("2"),    Inf);
is((Inf)->iroot("2"),   Inf);
is((-Inf)->root("2"),   Inf);    # sqrt(-Inf) -- shouldn't be NaN?

#is(Math::BigNum->new('foo'), NaN);     # should be NaN?

like(Inf->asec, qr/^1\.5707963267/);
