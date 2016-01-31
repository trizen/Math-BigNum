package Math::BigNum::Inf;

use 5.010;
use strict;
use warnings;

use Math::GMPq qw();
use Math::MPFR qw();
use Math::BigNum qw();

use Class::Multimethods qw(multimethod);

=encoding utf8

=head1 NAME

Math::BigNum::Inf - Represents the +Infinity value.

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

    use Math::BigNum;
    say Math::BigNum->inf;         # => "inf"

    my $inf = Math::BigNum::Inf->new;
    say $inf->atan;                # => 1.57079632679489661923132169163975

=head1 DESCRIPTION

Math::BigNum::Inf is an abstract type that represents +/-Infinity.

=head1 SUBROUTINES/METHODS

=cut

use overload
  q{""} => \&stringify,
  q{0+} => \&numify,
  bool  => \&boolify,

  '=' => sub { $_[0]->copy },

  # Some shortcuts for speed
  '+='  => sub { $_[0]->badd($_[1]) },
  '-='  => sub { $_[0]->bsub($_[1]) },
  '*='  => sub { $_[0]->bmul($_[1]) },
  '/='  => sub { $_[0]->bdiv($_[1]) },
  '%='  => sub { $_[0]->bmod($_[1]) },
  '^='  => sub { $_[0]->bxor($_[1]) },
  '&='  => sub { $_[0]->band($_[1]) },
  '|='  => sub { $_[0]->bior($_[1]) },
  '**=' => sub { $_[0]->bpow($_[1]) },
  '<<=' => sub { $_[0]->blsft($_[1]) },
  '>>=' => sub { $_[0]->brsft($_[1]) },

  '+'  => sub { $_[0]->add($_[1]) },
  '*'  => sub { $_[0]->mul($_[1]) },
  '==' => sub { $_[0]->eq($_[1]) },
  '!=' => sub { $_[0]->ne($_[1]) },
  '&'  => sub { $_[0]->and($_[1]) },
  '|'  => sub { $_[0]->ior($_[1]) },
  '^'  => sub { $_[0]->xor($_[1]) },
  '~'  => sub { $_[0]->not },

  '++' => sub { $_[0]->binc },
  '--' => sub { $_[0]->bdec },

  '>'   => sub { Math::BigNum::Inf::gt($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '>='  => sub { Math::BigNum::Inf::ge($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<'   => sub { Math::BigNum::Inf::lt($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<='  => sub { Math::BigNum::Inf::le($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<=>' => sub { Math::BigNum::Inf::cmp($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  '>>' => sub { Math::BigNum::Inf::rsft($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<<' => sub { Math::BigNum::Inf::lsft($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  '**'  => sub { Math::BigNum::Inf::pow($_[2]   ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '-'   => sub { Math::BigNum::Inf::sub($_[2]   ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '/'   => sub { Math::BigNum::Inf::div($_[2]   ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '%'   => sub { Math::BigNum::Inf::mod($_[2]   ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  atan2 => sub { Math::BigNum::Inf::atan2($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  eq  => sub { "$_[0]" eq "$_[1]" },
  ne  => sub { "$_[0]" ne "$_[1]" },
  cmp => sub { $_[2] ? "$_[1]" cmp $_[0]->stringify : $_[0]->stringify cmp "$_[1]" },

  neg  => sub { $_[0]->neg },
  sin  => sub { $_[0]->sin },
  cos  => sub { $_[0]->cos },
  exp  => sub { $_[0]->exp },
  log  => sub { $_[0]->ln },
  int  => sub { $_[0]->int },
  abs  => sub { $_[0]->abs },
  sqrt => sub { $_[0]->sqrt };

=head2 new

    Inf->new        # => Inf
    Inf->new('-')   # => -Inf

Returns on objects representing the +/-Infinity abstract value.

=cut

sub new {
    my ($class, $sign) = @_;

    my $r = Math::GMPq::Rmpq_init();
    if (defined($sign)) {
        if ($sign eq '+') {
            Math::GMPq::Rmpq_set_ui($r, 1, 0);
        }
        elsif ($sign eq '-') {
            Math::GMPq::Rmpq_set_si($r, -1, 0);
        }
    }
    else {
        Math::GMPq::Rmpq_set_ui($r, 1, 0);
    }

    bless \$r, __PACKAGE__;
}

sub stringify {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0) ? 'Inf' : '-Inf';
}

sub numify {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0) ? +'inf' : -'inf';
}

sub _big2inf {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_set($$x, $$y);
    bless $x, __PACKAGE__;
    $x;
}

sub _big2ninf {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_set($$x, $$y);
    Math::GMPq::Rmpq_neg($$x, $$x);
    bless $x, __PACKAGE__;
    $x;
}

sub neg {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_neg($r, ${$_[0]});
    bless \$r, __PACKAGE__;
}

sub bneg {
    my ($x) = @_;
    Math::GMPq::Rmpq_neg($$x, $$x);
    $x;
}

sub abs {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_abs($r, ${$_[0]});
    bless \$r, __PACKAGE__;
}

sub babs {
    my ($x) = @_;
    Math::GMPq::Rmpq_abs($$x, $$x);
    $x;
}

*copy  = \&Math::BigNum::copy;
*one   = \&Math::BigNum::one;
*mone  = \&Math::BigNum::mone;
*zero  = \&Math::BigNum::zero;
*bone  = \&Math::BigNum::bone;
*bzero = \&Math::BigNum::bzero;
*bmone = \&Math::BigNum::bmone;

*nan  = \&Math::BigNum::Nan::nan;
*bnan = \&Math::BigNum::Nan::bnan;

sub binf {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_ui($$x, 1, 0);
    bless $x, __PACKAGE__;
    $x;
}

sub bninf {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_si($$x, -1, 0);
    bless $x, __PACKAGE__;
    $x;
}

*mod    = \&nan;
*modpow = \&nan;
*modinv = \&nan;
*and    = \&nan;
*ior    = \&nan;
*xor    = \&nan;
*gcd    = \&nan;
*lcm    = \&nan;

sub ninf {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_si($r, -1, 0);
    bless \$r, __PACKAGE__;
}

sub inf {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_ui($r, 1, 0);
    bless \$r, __PACKAGE__;
}

sub is_inf {
    my ($self, $sign) = @_;
    if (defined $sign) {
        if ($sign eq '+') {
            return $self->is_pos;
        }
        elsif ($sign eq '-') {
            return $self->is_neg;
        }
    }

    $self->is_pos;
}

sub is_ninf {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
}

sub is_neg {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
}

sub is_pos {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
}

sub is_nan   { 0 }
sub is_prime { }
sub is_psqr  { }
sub is_ppow  { }
sub is_div   { }
sub is_even  { }
sub is_odd   { }
sub is_real  { }

sub as_bin { '' }
*as_oct = \&as_bin;
*as_hex = \&as_bin;

=head2 add


=cut

sub add {
    $_[0]->copy->badd($_[1]);
}

=head2 badd

=cut

multimethod badd => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    my ($x, $y) = @_;
    $x->eq($y) ? $x : $x->bnan;
};

multimethod badd => qw(Math::BigNum::Inf $)                     => sub { $_[0] };
multimethod badd => qw(Math::BigNum::Inf Math::BigNum)          => sub { $_[0] };
multimethod badd => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub { $_[0] };
multimethod badd => qw(Math::BigNum::Inf Math::BigNum::Nan)     => sub { $_[0]->bnan };

=head2 sub


=cut

sub sub { $_[0]->copy->bsub($_[1]) }

=head2 bsub

=cut

sub bsub {
    $_[0]->badd($_[1]->neg);
}

=head2 mul


=cut

sub mul {
    $_[0]->copy->bmul($_[1]);
}

multimethod bmul => qw(Math::BigNum::Inf Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $sgn = Math::GMPq::Rmpq_sgn($$y);
    $sgn < 0 ? $x->bneg : $sgn > 0 ? $x : $x->bnan;
};

multimethod bmul => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    my ($x, $y) = @_;
    my $xsgn = Math::GMPq::Rmpq_sgn($$x);
    my $ysgn = Math::GMPq::Rmpq_sgn($$y);

    # Inf * Inf = Inf
    if ($xsgn > 0 and $ysgn > 0) {
        $x;
    }

    # Inf * -Inf = -Inf
    elsif ($xsgn > 0 and $ysgn < 0) {
        $x->bneg;
    }

    # -Inf * Inf = -Inf
    elsif ($xsgn < 0 and $ysgn > 0) {
        $x;
    }

    # -Inf * -Inf = Inf
    else {
        $x->bneg;
    }
};

multimethod bmul => qw(Math::BigNum::Inf Math::BigNum::Nan) => sub { $_[0]->bnan };

=head2 div

=cut

sub div {
    $_[0]->copy->bdiv($_[1]);
}

=head2 bdiv

=cut

multimethod bdiv => qw(Math::BigNum::Inf Math::BigNum) => sub {
    my ($x, $y) = @_;
    $y->is_neg ? $x->bneg : $x;
};

multimethod bdiv => qw(Math::BigNum::Inf Math::BigNum::Inf) => \&bnan;
multimethod bdiv => qw(Math::BigNum::Inf Math::BigNum::Nan) => sub { $_[0]->bnan };

#
## Trigonometric functions
#

=head2 atan

=cut

sub atan {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0)
      ? Math::BigNum->pi->div(2)
      : Math::BigNum->pi->div(-2);
}

*atan2 = \&atan;

#
## atanh(+inf) = -pi/2*i
## atanh(-inf) = pi/2*i
#
sub atanh {
    (
     (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0)
     ? Math::BigNum->pi->div(-2)
     : Math::BigNum->pi->div(2)
      ) *
      Math::BigNum::i();
}

#
## asec(+inf) = pi/2
## asec(-inf) = pi/2
#
sub asec {
    Math::BigNum->pi->div(2);
}

#
## asech(+inf) = pi/2*i
## asech(-inf) = pi/2*i
#
sub asech {
    Math::BigNum->pi->div(2) * Math::BigNum::i();
}

#
## asin(+inf) = -inf*i
## asin(-inf) = inf*i
#
sub asin {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0)
      ? Math::BigNum::Complex->new(0, '-@Inf@')
      : Math::BigNum::Complex->new(0, '@Inf@');
}

#
## acos(+inf) = inf*i
## acos(-inf) = -inf*i
#
sub acos {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0)
      ? Math::BigNum::Complex->new(0, '@Inf@')
      : Math::BigNum::Complex->new(0, '-@Inf@');
}

#
## tanh(+inf) = coth(+inf) = erf(+inf) = +1
## tanh(-inf) = coth(-inf) = erf(-inf) = -1
#
sub tanh {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0)
      ? one()
      : mone();
}

*coth = \&tanh;
*erf  = \&tanh;

#
## sin(+inf) = sin(-inf) = 0
## cos(+inf) = cos(-inf) = 0
## sech(+inf) = sech(-inf) = 0
## ...
#

*sin   = \&zero;
*cos   = \&zero;
*sech  = \&zero;
*csch  = \&zero;
*acsc  = \&zero;
*acsch = \&zero;
*acot  = \&zero;
*acoth = \&zero;

#
## acosh(+inf) = acosh(-inf) = inf
#

*acosh = \&inf;

#
## Other functions
#

#
## binomial(inf, x) = 0       | with x < 0
## binomial(inf, 0) = 1
## binomial(inf, inf) = 1
## binomial(inf, x) = inf     | with x > 0
##
#
## binomial(-inf, x) = 0        | with x < 0
## binomial(-inf, 0) = 1
## binomial(-inf, inf) = 1
## binomial(-inf, x) = -inf     | with x > 0
##
#

multimethod binomial => qw(Math::BigNum::Inf Math::BigNum) => sub {
        $_[1]->is_neg  ? zero()
      : $_[1]->is_zero ? one()
      :                  $_[0]->copy;
};

multimethod binomial => qw(Math::BigNum::Inf Math::BigNum::Inf)     => sub { one() };
multimethod binomial => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub { nan() };

=head2 inv

    $x->inv     # => BigNum

Inverse value of +/-Infinity. Always returns zero.

=cut

*inv = \&zero;

#
## Comparisons
#

=head2 eq

    $x->eq($y)      # => Bool
    $x == $y        # => Bool


Equality test:

    Inf == Inf      # true
    Inf == -Inf     # false
    Inf == 0        # false
    Inf == MaN      # false

=cut

multimethod eq => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) == Math::GMPq::Rmpq_sgn(${$_[1]});
};

multimethod eq => qw(Math::BigNum::Inf Math::BigNum)          => sub { };
multimethod eq => qw(Math::BigNum::Inf Math::BigNum::Nan)     => sub { };
multimethod eq => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub { };    # this may equal

=head2 ne

    $x->ne($y)      # => Bool
    $x != $y        # => Bool

Inequality test:

    Inf != Inf      # false
    Inf != -Inf     # true
    Inf != 0        # true
    Inf != MaN      # true

=cut

multimethod ne => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) != Math::GMPq::Rmpq_sgn(${$_[1]});
};

multimethod ne => qw(Math::BigNum::Inf Math::BigNum)          => sub { 1 };
multimethod ne => qw(Math::BigNum::Inf Math::BigNum::Nan)     => sub { 1 };
multimethod ne => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub { 1 };    # this may equal

=head2 cmp

    $x->cmp(Inf)            # => Scalar
    $x->cmp(BigNum)         # => Scalar
    $x->cmp(Complex)        # => Scalar
    $x->cmp(Nan)            # => undef

Compares C<$x> to C<$y> and returns a positive value when C<$x> is greater than C<$y>,
a negative value when C<$x> is lower than C<$y>, or zero when C<$x> and C<$y> are equal.

=cut

multimethod cmp => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) <=> Math::GMPq::Rmpq_sgn(${$_[1]});
};

multimethod cmp => qw(Math::BigNum::Inf Math::BigNum) => sub {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0) ? 1 : -1;
};

multimethod cmp => qw(Math::BigNum::Inf $) => sub {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0) ? 1 : -1;
};

multimethod cmp => qw($ Math::BigNum::Inf) => sub {
    (Math::GMPq::Rmpq_sgn(${$_[1]}) > 0) ? -1 : 1;
};

multimethod cmp => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) > 0) ? 1 : -1;
};

multimethod cmp => qw(Math::BigNum::Inf Math::BigNum::Nan) => sub {
    ## undef for Inf <=> NaN
};

=head2 gt

    $x->gt(BigNum)      # => Bool
    $x->gt(Inf)         # => Bool
    $x->gt(Complex)     # => Bool
    $x->gt(Nan)         # => undef
    $x > $y             # => Bool

Returns true if C<$x> is greater than C<$y>.

=cut

multimethod gt => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > Math::GMPq::Rmpq_sgn(${$_[1]});
};

multimethod gt => qw(Math::BigNum::Inf Math::BigNum) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
};

multimethod gt => qw(Math::BigNum::Inf $) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
};

multimethod gt => qw($ Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[1]}) < 0;
};

multimethod gt => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
};

multimethod gt => qw(Math::BigNum::Inf Math::BigNum::Nan) => sub {
    ## undef for Inf > NaN
};

=head2 ge

    $x->ge(BigNum)      # => Bool
    $x->ge(Inf)         # => Bool
    $x->ge(Complex)     # => Bool
    $x->ge(Nan)         # => undef
    $x >= $y            # => Bool

Returns true if C<$x> is greater or equal to C<$y>.

=cut

multimethod ge => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) >= Math::GMPq::Rmpq_sgn(${$_[1]});
};

multimethod ge => qw(Math::BigNum::Inf Math::BigNum) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
};

multimethod ge => qw(Math::BigNum::Inf $) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
};

multimethod ge => qw($ Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[1]}) < 0;
};

multimethod ge => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
};

multimethod ge => qw(Math::BigNum::Inf Math::BigNum::Nan) => sub {
    ## undef for Inf > NaN
};

=head2 lt

    $x->lt(BigNum)      # => Bool
    $x->lt(Inf)         # => Bool
    $x->lt(Complex)     # => Bool
    $x->lt(Nan)         # => undef
    $x < $y             # => Bool

Returns true if C<$x> is less than C<$y>.

=cut

multimethod lt => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < Math::GMPq::Rmpq_sgn(${$_[1]});
};

multimethod lt => qw(Math::BigNum::Inf Math::BigNum) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
};

multimethod lt => qw(Math::BigNum::Inf $) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
};

multimethod lt => qw($ Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[1]}) > 0;
};

multimethod lt => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
};

multimethod lt => qw(Math::BigNum::Inf Math::BigNum::Nan) => sub {
    ## undef for Inf > NaN
};

=head2 le

    $x->le(BigNum)      # => Bool
    $x->le(Inf)         # => Bool
    $x->le(Complex)     # => Bool
    $x->le(Nan)         # => undef
    $x <= $y            # => Bool

Returns true if C<$x> is less than or equal to C<$y>.

=cut

multimethod le => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) <= Math::GMPq::Rmpq_sgn(${$_[1]});
};

multimethod le => qw(Math::BigNum::Inf Math::BigNum) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
};

multimethod le => qw(Math::BigNum::Inf $) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
};

multimethod le => qw($ Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[1]}) > 0;
};

multimethod le => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
};

multimethod le => qw(Math::BigNum::Inf Math::BigNum::Nan) => sub {
    ## undef for Inf > NaN
};

1;
