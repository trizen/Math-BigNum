package Math::BigNum;

use 5.010;
use strict;
use warnings;

no warnings qw(qw);

use Math::GMPq qw();
use Math::GMPz qw();
use Math::MPFR qw();
use Math::BigRat qw(try GMP);

use Class::Multimethods qw(multimethod);

=head1 NAME

Math::BigNum - Arbitrary size precision for integer, complex and floating-point numbers

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

    use Math::BigNum qw(:constant);
    print 1/2 * (100->fac + 1);

=head1 DESCRIPTION

Math::BigNum provides a transparent interface to Math::GMPz, Math::GMPq, Math::MPFR and Math::MPC.

=head1 SUBROUTINES/METHODS

=cut

our $VERSION = '0.01';

our ($ROUND, $PREC);

BEGIN {
    $ROUND = Math::MPFR::MPFR_RNDN();
    $PREC  = 128;
}

use Math::BigNum::Inf qw();
use Math::BigNum::Ninf qw();
use Math::BigNum::Nan qw();

use constant {
              NAN  => Math::BigNum::Nan->new,
              INF  => Math::BigNum::Inf->new,
              NINF => Math::BigNum::Ninf->new,
              ONE  => bless(\Math::GMPq->new(1), __PACKAGE__),
              ZERO => bless(\Math::GMPq->new(0), __PACKAGE__),
              MONE => bless(\Math::GMPq->new(-1), __PACKAGE__),
             };

use Math::BigNum::Complex qw();
use constant {i => Math::BigNum::Complex->new(0, 1)};

use overload
  '""' => \&stringify,
  '0+' => \&numify,
  bool => \&boolify,

  neg  => sub { $_[0]->neg },
  '+'  => sub { $_[0]->add($_[1]) },
  '*'  => sub { $_[0]->mul($_[1]) },
  '==' => sub { $_[0]->eq($_[1]) },
  '!=' => sub { $_[0]->ne($_[1]) },
  '&'  => sub { $_[0]->and($_[1]) },
  '|'  => sub { $_[0]->or($_[1]) },
  '^'  => sub { $_[0]->xor($_[1]) },

  '>'   => sub { Math::BigNum::gt($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '>='  => sub { Math::BigNum::ge($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<'   => sub { Math::BigNum::lt($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<='  => sub { Math::BigNum::le($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<=>' => sub { Math::BigNum::cmp($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  '>>' => sub { Math::BigNum::rsft($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<<' => sub { Math::BigNum::lsft($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  '**'  => sub { Math::BigNum::pow($_[2]   ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '-'   => sub { Math::BigNum::sub($_[2]   ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '/'   => sub { Math::BigNum::div($_[2]   ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '%'   => sub { Math::BigNum::mod($_[2]   ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  atan2 => sub { Math::BigNum::atan2($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  sin  => sub { $_[0]->sin },
  cos  => sub { $_[0]->cos },
  exp  => sub { $_[0]->exp },
  log  => sub { $_[0]->ln },
  int  => sub { $_[0]->int },
  abs  => sub { $_[0]->abs },
  sqrt => sub { $_[0]->sqrt };

sub import {
    shift;

    foreach my $name (@_) {
        if ($name eq ':constant') {
            overload::constant
              integer => sub { _new_uint(shift) },
              float   => sub { Math::BigNum->new(shift, 10) };
        }
        elsif ($name eq 'i') {
            no strict 'refs';
            *{caller(0) . '::' . 'i'} = \&i;
        }
        else {
            die "unknown import: $name";
        }
    }
    return;
}

sub _new_int {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_si($r, $_[0], 1);
    bless \$r, __PACKAGE__;
}

sub _new_uint {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_ui($r, $_[0], 1);
    bless \$r, __PACKAGE__;
}

multimethod new => qw($ Math::BigNum) => sub {
    $_[1]->copy;
};

multimethod new => qw($ #) => sub {
    bless(\_str2mpq($_[1]), $_[0]);
};

multimethod new => qw($ $) => sub {
    my $r   = Math::GMPq::Rmpq_init();
    my $rat = _str2rat($_[1]);
    Math::GMPq::Rmpq_set_str($r, $rat, 10);
    Math::GMPq::Rmpq_canonicalize($r) if (index($rat, '/') != -1);
    bless(\$r, $_[0]);
};

multimethod new => qw($ # #) => sub {
    if ($_[2] == 10) {
        (bless \_str2mpq($_[1]), $_[0]);
    }
    else {
        my $r = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set_str($r, $_[1], $_[2]);
        Math::GMPq::Rmpq_canonicalize($r) if (index($_[1], '/') != -1);
        bless(\$r, $_[0]);
    }
};

multimethod new => qw($ $ $) => sub {
    my $r = Math::GMPq::Rmpq_init();
    if ($_[2] == 10) {
        Math::GMPq::Rmpq_set_str($r, _str2rat($_[1]), 10);
        Math::GMPq::Rmpq_canonicalize($r);
    }
    else {
        Math::GMPq::Rmpq_set_str($r, $_[1], $_[2]);
        Math::GMPq::Rmpq_canonicalize($r) if (index($_[1], '/') != -1);
    }
    bless(\$r, $_[0]);
};

# TODO: find a better solution
sub _str2rat {
    my ($str) = @_;

    my $sign = substr($str, 0, 1);
    if ($sign eq '-') {
        substr($str, 0, 1, '');
        $sign = '-';
    }
    else {
        substr($str, 0, 1, '') if ($sign eq '+');
        $sign = '';
    }

    my $i;
    if (($i = index($str, 'e')) != -1) {

        my $exp = substr($str, $i + 1);
        my ($before, $after) = split(/\./, substr($str, 0, $i));
        my $numerator = $before . $after;

        my $denominator = 1;
        if ($exp < 1) {
            $denominator .= '0' x (abs($exp) + length($after));
        }
        else {
            my $diff = ($exp - length($after));
            if ($diff >= 0) {
                $numerator .= '0' x $diff;
            }
            else {
                my $s = $before . $after;
                substr($s, $exp + length($before), 0, '.');
                return _str2rat("$sign$s");
            }
        }

        "$sign$numerator/$denominator";
    }
    elsif (($i = index($str, '.')) != -1) {
        my ($before, $after) = (substr($str, 0, $i), substr($str, $i + 1));
        if ($after =~ tr/0// == length($after)) {
            return "$sign$before";
        }
        $sign . ("$before$after/1" =~ s/^0+//r) . ('0' x length($after));
    }
    else {
        $sign . $str;
    }
}

sub _str2mpfr {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_set_str($r, $_[0], 10, $ROUND);
    $r;
}

sub _str2mpq {
    my $r = Math::GMPq::Rmpq_init();

    if ((~$_[0] & $_[0]) eq '0' and CORE::int($_[0]) == $_[0]) {
        if ($_[0] >= 0) {
            Math::GMPq::Rmpq_set_ui($r, $_[0], 1);
        }
        else {
            Math::GMPq::Rmpq_set_si($r, $_[0], 1);
        }
    }
    else {
        my $rat = _str2rat($_[0]);
        Math::GMPq::Rmpq_set_str($r, $rat, 10);
        Math::GMPq::Rmpq_canonicalize($r) if (index($rat, '/') != -1);
    }

    $r;
}

sub _as_float {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_set_q($r, ${$_[0]}, $ROUND);
    $r;
}

sub _as_int {
    my $i = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($i, ${$_[0]});
    $i;
}

sub _mpfr2rat {

    if (Math::MPFR::Rmpfr_inf_p($_[0])) {
        if (Math::MPFR::Rmpfr_sgn($_[0]) > 0) {
            return INF;
        }
        else {
            return NINF;
        }
    }

    if (Math::MPFR::Rmpfr_nan_p($_[0])) {
        return NAN;
    }

    my $r = Math::GMPq::Rmpq_init();
    Math::MPFR::Rmpfr_get_q($r, $_[0]);
    bless \$r, __PACKAGE__;
}

sub _mpz2rat {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_z($r, $_[0]);
    bless \$r, __PACKAGE__;
}

sub _big2cplx {
    my ($x, $z) = @_;
    $$x = $$z;
    bless $x, 'Math::BigNum::Complex';
    $x;
}

sub stringify {
    my $v = Math::GMPq::Rmpq_get_str(${$_[0]}, 10);

    if (index($v, '/') != -1) {
        my $br = Math::BigRat->new($v);
        local $Math::BigFloat::precision = -CORE::int(CORE::int($PREC) / 3.321923);
        $br->as_float->bstr =~ s/0+$//r;
    }
    else {
        $v;
    }
}

sub numify {
    Math::GMPq::Rmpq_get_d(${$_[0]});
}

sub boolify {
    !!Math::GMPq::Rmpq_get_d(${$_[0]});
}

sub as_frac {
    Math::GMPq::Rmpq_get_str(${$_[0]}, 10);
}

multimethod in_base => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if ($y < 2 or $y > 36) {
        die "base must be between 2 and 36, got $y";
    }

    Math::GMPq::Rmpq_get_str(${$_[0]}, $y);
};

multimethod in_base => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->in_base(CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]})));
};

sub copy {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set($r, ${$_[0]});
    bless \$r, __PACKAGE__;
}

#
## Constants
#

sub pi {
    my $pi = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_pi($pi, $ROUND);
    _mpfr2rat($pi);
}

sub tau {
    my $tau = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_pi($tau, $ROUND);
    Math::MPFR::Rmpfr_mul_ui($tau, $tau, 2, $ROUND);
    _mpfr2rat($tau);
}

sub ln2 {
    my $ln2 = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_log2($ln2, $ROUND);
    _mpfr2rat($ln2);
}

sub Y {
    my $euler = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_euler($euler, $ROUND);
    _mpfr2rat($euler);
}

sub G {
    my $catalan = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_catalan($catalan, $ROUND);
    _mpfr2rat($catalan);
}

sub e {
    state $one_f = (Math::MPFR::Rmpfr_init_set_ui(1, $ROUND))[0];
    my $e = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_exp($e, $one_f, $ROUND);
    _mpfr2rat($e);
}

sub phi {
    state $one_f  = (Math::MPFR::Rmpfr_init_set_ui(1, $ROUND))[0];
    state $two_f  = (Math::MPFR::Rmpfr_init_set_ui(2, $ROUND))[0];
    state $five_f = (Math::MPFR::Rmpfr_init_set_ui(5, $ROUND))[0];

    my $phi = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_sqrt($phi, $five_f, $ROUND);
    Math::MPFR::Rmpfr_add($phi, $phi, $one_f, $ROUND);
    Math::MPFR::Rmpfr_div($phi, $phi, $two_f, $ROUND);

    _mpfr2rat($phi);
}

#
## Special values
#

multimethod binf => qw(Math::BigNum) => sub {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_ui($$x, 1, 0);
    bless $x, 'Math::BigNum::Inf';
    $x;
};

multimethod binf => qw(Math::BigNum $) => sub {
    my ($x, $sign) = @_;

    if ($sign eq '-') {
        Math::GMPq::Rmpq_set_si($$x, -1, 0);
        bless $x, 'Math::BigNum::Ninf';
    }
    elsif ($sign eq '+') {
        Math::GMPq::Rmpq_set_ui($$x, 1, 0);
        bless $x, 'Math::BigNum::Inf';
    }
    else {
        die "Invalid argument provided to binf(). Expected '+' or '-', but got: $sign";
    }

    $x;
};

multimethod bninf => qw(Math::BigNum) => sub {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_si($$x, -1, 0);
    bless $x, 'Math::BigNum::Ninf';
    $x;
};

multimethod bnan => qw(Math::BigNum) => sub {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_ui($$x, 0, 0);
    bless $x, 'Math::BigNum::Nan';
    $x;
};

#
## Arithmetic
#

=head2 add

    $x->add(BigNum)       # => BigNum
    $x->add(Scalar)       # => BigNum
    $x->add(Complex)      # => Complex
    $x->add(Inf)          # => Inf
    $x->add(Ninf)         # => Ninf

Adds $x to $y and returns the result.

=cut

multimethod add => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_add($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod add => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_add($r, $$x, _str2mpq($y));
    bless \$r, __PACKAGE__;
};

multimethod add => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->add($_[1]);
};

multimethod add => qw(Math::BigNum Math::BigNum::Inf) => sub {
    $_[1];
};

multimethod add => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    $_[1];
};

=head2 badd

    $x->badd(BigNum)      # => BigNum
    $x->badd(Scalar)      # => BigNum
    $x->badd(Inf)         # => Inf
    $x->badd(Ninf)        # => Ninf

Add $y to $x by changing $x in-place.

=cut

multimethod badd => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_add($$x, $$x, $$y);
    $x;
};

multimethod badd => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_add($$x, $$x, _str2mpq($y));
    $x;
};

multimethod badd => qw(Math::BigNum Math::BigNum::Inf) => sub {
    $_[0]->binf;
};

multimethod badd => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    $_[0]->bninf;
};

multimethod badd => qw(Math::BigNum Math::BigNum::Nan) => sub {
    $_[0]->bnan;
};

=head2 sub

    $x->sub(BigNum)       # => BigNum
    $x->sub(Scalar)       # => BigNum
    $x->sub(Complex)      # => Complex
    $x->sub(Inf)          # => Ninf
    $x->sub(Ninf)         # => Inf

Subtracts $y from $x and returns the result.

=cut

multimethod sub => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_sub($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod sub => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_sub($r, $$x, _str2mpq($y));
    bless \$r, __PACKAGE__;
};

multimethod sub => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->sub($_[1]);
};

multimethod sub => qw(Math::BigNum Math::BigNum::Inf) => sub {
    $_[1]->neg;
};

multimethod sub => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    $_[1]->neg;
};

=head2 bsub

    $x->bsub(BigNum)      # => BigNum
    $x->bsub(Scalar)      # => BigNum
    $x->bsub(Inf)         # => Ninf
    $x->bsub(Ninf)        # => Inf

Subtracts $y from $x by changing $x in-place.

=cut

multimethod bsub => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_sub($$x, $$x, $$y);
    $x;
};

multimethod bsub => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_sub($$x, $$x, _str2mpq($y));
    $x;
};

multimethod bsub => qw(Math::BigNum Math::BigNum::Inf) => sub {
    $_[0]->bninf;
};

multimethod bsub => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    $_[0]->binf;
};

multimethod bsub => qw(Math::BigNum Math::BigNum::Nan) => sub {
    $_[0]->bnan;
};

=head2 mul

    $x->mul(BigNum)       # => BigNum
    $x->mul(Scalar)       # => BigNum
    $x->mul(Complex)      # => Complex
    $x->mul(Inf)          # => Ninf | Inf | Nan
    $x->mul(Ninf)         # => Ninf | Inf | Nan

Multiplies $x by $y and returns the result.

=cut

multimethod mul => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_mul($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod mul => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_mul($r, $$x, _str2mpq($y));
    bless \$r, __PACKAGE__;
};

multimethod mul => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->mul($_[1]);
};

multimethod mul => qw(Math::BigNum Math::BigNum::Inf) => sub {
    my ($x) = @_;
    my $sign = Math::GMPq::Rmpq_sgn($$x);
    $sign < 0 ? NINF : $sign > 0 ? INF : NAN;
};

multimethod mul => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    my ($x) = @_;
    my $sign = Math::GMPq::Rmpq_sgn($$x);
    $sign < 0 ? INF : $sign > 0 ? NINF : NAN;
};

multimethod mul => qw(Math::BigNum Math::BigNum::Nan) => sub { NAN };

=head2 bmul

    $x->bmul(BigNum)        # => BigNum
    $x->bmul(Scalar)        # => BigNum
    $x->bmul(Inf)           # => Inf | Ninf | Nan
    $x->bmul(Ninf)          # => Inf | Ninf | Nan

Multiply $x by $y, changing $x in-place.

=cut

multimethod bmul => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_mul($$x, $$x, $$y);
    $x;
};

multimethod bmul => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_mul($$x, $$x, _str2mpq($y));
    $x;
};

multimethod bmul => qw(Math::BigNum Math::BigNum::Inf) => sub {
    my ($x) = @_;
    my $sign = Math::GMPq::Rmpq_sgn($$x);

        $sign < 0 ? $x->bninf
      : $sign > 0 ? $x->binf
      :             $x->bnan;
};

multimethod bmul => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    my ($x) = @_;
    my $sign = Math::GMPq::Rmpq_sgn($$x);

        $sign > 0 ? $x->bninf
      : $sign < 0 ? $x->binf
      :             $x->bnan;
};

multimethod bmul => qw(Math::BigNum Math::BigNum::Nan) => sub {
    $_[0]->bnan;
};

=head2 div

    $x->div(BigNum)       # => BigNum | Nan | Inf | Ninf
    $x->div(Scalar)       # => BigNum | Nan | Inf | Ninf
    $x->div(Complex)      # => Complex
    $x->div(Inf)          # => BigNum(0)
    $x->div(Ninf)         # => BigNum(0)

Divides $x by $y and returns the result. Returns Nan when $x and $y are 0,
Inf when $y is $zero and $x is positive, Ninf when $y is zero and $x is negative.

=cut

multimethod div => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (!Math::GMPq::Rmpq_sgn($$y)) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? NAN : $sign > 0 ? INF : NINF);
    }

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_div($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod div => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if (!$y) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? NAN : $sign > 0 ? INF : NINF);
    }

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_div($r, $$x, _str2mpq($y));
    bless \$r, __PACKAGE__;
};

multimethod div => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->div($_[1]);
};

multimethod div => qw(Math::BigNum Math::BigNum::Inf) => sub {
    ZERO;
};

multimethod div => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    ZERO;
};

=head2 bdiv

    $x->bdiv(BigNum)        # => BigNum | Nan | Inf | Ninf
    $x->bdiv(Scalar)        # => BigNum | Nan | Inf | Ninf
    $x->bdiv(Inf)           # => BigNum(0)
    $x->bdiv(Ninf)          # => BigNum(0)

Divide $x by $y, changing $x in-place. The return values are the same as for C<div()>.

=cut

multimethod bdiv => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (!Math::GMPq::Rmpq_sgn($$y)) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? NAN : $sign > 0 ? INF : NINF);
    }

    Math::GMPq::Rmpq_div($$x, $$x, $$y);
    $x;
};

multimethod bdiv => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if (!$y) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? NAN : $sign > 0 ? INF : NINF);
    }

    Math::GMPq::Rmpq_div($$x, $$x, _str2mpq($y));
    $x;
};

=head2 neg

    $x->neg

Negative value of $x. Returns abs($x) when $x is negative, and -$x when $x is positive.

=cut

sub neg {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_neg($r, $$x);
    bless \$r, __PACKAGE__;
}

=head2 abs

    $x->abs     # => BigNum

Absolute value of $x. Returns -$x when $x is negative and $x otherwise.

=cut

sub abs {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_abs($r, $$x);
    bless \$r, __PACKAGE__;
}

=head2 inv

    $x->inv     # => BigNum

Inverse value of $x. (1/$x)

=cut

sub inv {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_inv($r, $$x);
    bless \$r, __PACKAGE__;
}

=head2 sqr

    $x->sqr    # => BigNum

Raise $x to the power of 2 and return the result. ($x**2)

=cut

sub sqr {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_mul($r, $$x, $$x);
    bless \$r, __PACKAGE__;
}

=head2 sqrt

    $x->sqrt    # => BigNum or Complex

Square root of $x. Returns a Complex number when $x is negative.

=cut

sub sqrt {
    my ($x) = @_;

    # Return a complex number for x < 0
    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        return Math::BigNum::Complex->new($x)->sqrt;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_sqrt($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

=head2 cbrt

    $x->cbrt    # => BigNum | Complex

Cube root of $x. Returns a Complex number when $x is negative.

=cut

sub cbrt {
    my ($x) = @_;

    # Return a complex number for x < 0
    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        return Math::BigNum::Complex->new($x)->cbrt;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_cbrt($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

=head2 root

    $x->root(BigNum)      # => BigNum | Complex
    $x->root(Complex)     # => Complex
    $x->root(Inf)         # => BigNum(1)
    $x->root(Ninf)        # => BigNum(1)

Nth root of $x. Returns a Complex number when is $x is negative.

=cut

multimethod root => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->pow($_[1]->inv);
};

multimethod root => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->pow($_[1]->inv);
};

multimethod root => qw(Math::BigNum $) => sub {
    $_[0]->root(Math::BigNum->new($_[1]));
};

multimethod root => qw(Math::BigNum Math::BigNum::Inf) => sub {
    ONE;
};

multimethod root => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    ONE;
};

multimethod root => qw(Math::BigNum Math::BigNum::Nan) => sub {
    NAN;
};

=head2 pow

    $x->pow(BigNum)     # => BigNum | Complex
    $x->pow(Complex)    # => Complex
    $x->pow(Inf)        # => Inf
    $x->pow(Ninf)       # => BigNum(0)

Raise $x to power $y.

=cut

multimethod pow => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_sgn($$y) >= 0 and Math::GMPq::Rmpq_integer_p($$x) and Math::GMPq::Rmpq_integer_p($$y)) {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_pow_ui($z, $z, Math::GMPq::Rmpq_get_d($$y));
        return _mpz2rat($z);
    }

    if (Math::GMPq::Rmpq_sgn($$x) < 0 and !Math::GMPq::Rmpq_integer_p($$y)) {
        return Math::BigNum::Complex->new($x)->pow($y);
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_pow($r, _as_float($x), _as_float($y), $ROUND);
    _mpfr2rat($r);
};

multimethod pow => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->pow($_[1]);
};

# TODO: optimize this for Math::GMPz::Rmpz_pow_ui()
multimethod pow => qw(Math::BigNum $) => sub {
    $_[0]->pow(Math::BigNum->new($_[1]));
};

multimethod pow => qw($ Math::BigNum) => sub {
    Math::BigNum->new($_[0])->pow($_[1]);
};

multimethod pow => qw(Math::BigNum Math::BigNum::Inf)  => sub { $_[1] };
multimethod pow => qw(Math::BigNum Math::BigNum::Ninf) => sub { ZERO };
multimethod pow => qw(Math::BigNum Math::BigNum::Nan)  => sub { NAN };

=head2 bpow

    $x->bpow(BigNum)     # => BigNum | Complex
    $x->bpow(Complex)    # => Complex
    $x->bpow(Inf)        # => Inf
    $x->bpow(Ninf)       # => BigNum(0)

Raise $x to power $y, changing $x in-place.

=cut

multimethod bpow => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_sgn($$y) >= 0 and Math::GMPq::Rmpq_integer_p($$x) and Math::GMPq::Rmpq_integer_p($$y)) {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_pow_ui($z, $z, Math::GMPq::Rmpq_get_d($$y));
        Math::GMPq::Rmpq_set_z($$x, $z);
        return $x;
    }

    if (Math::GMPq::Rmpq_sgn($$x) < 0 and !Math::GMPq::Rmpq_integer_p($$y)) {
        my $z = Math::BigNum::Complex->new($x)->pow($y);
        _big2cplx($x, $z);
        return $x;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_pow($r, _as_float($x), _as_float($y), $ROUND);
    Math::MPFR::Rmpfr_get_q($$x, $r);
    $x;
};

multimethod bpow => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $y_is_int = CORE::int($y) == $y;

    if ($y >= 0 and Math::GMPq::Rmpq_integer_p($$x) and $y_is_int) {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_pow_ui($z, $z, $y);
        Math::GMPq::Rmpq_set_z($$x, $z);
        return $x;
    }

    if (Math::GMPq::Rmpq_sgn($$x) < 0 and !$y_is_int) {
        my $z = Math::BigNum::Complex->new($x)->pow($y);
        _big2cplx($x, $z);
        return $x;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    if ($y_is_int) {
        if ($y >= 0) {
            Math::MPFR::Rmpfr_pow_ui($r, _as_float($x), $y, $ROUND);
        }
        else {
            Math::MPFR::Rmpfr_pow_ui($r, _as_float($x), $y, $ROUND);
        }
    }
    else {
        Math::MPFR::Rmpfr_pow($r, _as_float($x), _str2mpfr($y), $ROUND);
    }

    Math::MPFR::Rmpfr_get_q($$x, $r);
    $x;
};

=head2 ln

    $x->ln          # => BigNum | Complex

Logarithm of $x in base e. Returns a Complex number when $x is negative.

=cut

sub ln {
    my ($x) = @_;

    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        return Math::BigNum::Complex->new($x)->ln;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_log($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

=head2 log

    $x->log              # => BigNum | Complex
    $x->log(BigNum)      # => BigNum | Complex
    $x->log(Scalar)      # => BigNum | Complex

Logarithm of $x in base $y. When $y is not specified, it defaults to base e.

=cut

multimethod log => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        return Math::BigNum::Complex->new($x)->log($y);
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_log($r, _as_float($x), $ROUND);
    my $baseln = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_log($baseln, _as_float($y), $ROUND);
    Math::MPFR::Rmpfr_div($r, $r, $baseln, $ROUND);

    _mpfr2rat($r);
};

multimethod log => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        return Math::BigNum::Complex->new($x)->log($y);
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);

    if ($y == 2) {
        Math::MPFR::Rmpfr_log2($r, _as_float($x), $ROUND);
    }
    elsif ($y == 10) {
        Math::MPFR::Rmpfr_log10($r, _as_float($x), $ROUND);
    }
    else {
        Math::MPFR::Rmpfr_log($r, _as_float($x), $ROUND);
        my $baseln = Math::MPFR::Rmpfr_init2($PREC);
        Math::MPFR::Rmpfr_log($baseln, _str2mpfr($y), $ROUND);
        Math::MPFR::Rmpfr_div($r, $r, $baseln, $ROUND);
    }

    _mpfr2rat($r);
};

multimethod log => qw(Math::BigNum) => sub {
    $_[0]->ln;
};

=head2 blog

    $x->blog            # => BigNum | Complex
    $x->blog(BigNum)    # => BigNum | Complex
    $x->log(Scalar)     # => BigNum | Complex

Logarithm of $x in base $y, changing the $x in-place.
When $y is not specified, it defaults to base e.

=cut

multimethod blog => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        my $z = Math::BigNum::Complex->new($x)->log($y);
        return _big2cplx($x, $z);
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);

    if ($y == 2) {
        Math::MPFR::Rmpfr_log2($r, _as_float($x), $ROUND);
    }
    elsif ($y == 10) {
        Math::MPFR::Rmpfr_log10($r, _as_float($x), $ROUND);
    }
    else {
        Math::MPFR::Rmpfr_log($r, _as_float($x), $ROUND);
        my $baseln = Math::MPFR::Rmpfr_init2($PREC);
        Math::MPFR::Rmpfr_log($baseln, _str2mpfr($y), $ROUND);
        Math::MPFR::Rmpfr_div($r, $r, $baseln, $ROUND);
    }

    Math::MPFR::Rmpfr_get_q($$x, $r);
    $x;
};

multimethod blog => qw(Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        my $z = Math::BigNum::Complex->new($x)->log($y);
        return _big2cplx($x, $z);
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_log($r, _as_float($x), $ROUND);
    Math::MPFR::Rmpfr_get_q($$x, $r);

    $x;
};

=head2 log2

    $x->log2        # => BigNum | Complex

Logarithm of $x in base 2. Returns a Complex number when $x is negative.

=cut

sub log2 {
    my ($x) = @_;

    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        return Math::BigNum::Complex->new($x)->log2;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_log2($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

=head2 log10

    $x->log10       # => BigNum | Complex

Logarithm of $x in base 10. Returns a Complex number when $x is negative.

=cut

sub log10 {
    my ($x) = @_;

    if (Math::GMPq::Rmpq_sgn($$x) < 0) {
        return Math::BigNum::Complex->new($x)->log10;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_log10($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

=head2 exp

    $x->exp         # => BigNum

Exponential of $x in base e. (e**$x)

=cut

sub exp {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_exp($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

=head2 exp2

    $x->exp2        # => BigNum

Exponential of $x in base 2. (2**$x)

=cut

sub exp2 {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_exp2($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

=head2 exp10

    $x->exp10       # => BigNum

Exponential of $x in base 10. (10**$x)

=cut

sub exp10 {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_exp10($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

#
## Trigonometric functions
#

sub sin {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_sin($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub asin {
    my ($x) = @_;

    # Return a complex number for x < -1 or x > 1
    if (Math::GMPq::Rmpq_cmp_ui($$x, 1, 1) > 0 or Math::GMPq::Rmpq_cmp_si($$x, -1, 1) < 0) {
        return Math::BigNum::Complex->new($x)->asin;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_asin($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub sinh {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_sinh($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub asinh {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_asinh($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub cos {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_cos($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub acos {
    my ($x) = @_;

    # Return a complex number for x < -1 or x > 1
    if (Math::GMPq::Rmpq_cmp_ui($$x, 1, 1) > 0 or Math::GMPq::Rmpq_cmp_si($$x, -1, 1) < 0) {
        return Math::BigNum::Complex->new($x)->acos;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_acos($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub cosh {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_cosh($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub acosh {
    my ($x) = @_;

    # Return a complex number for x < 1
    if (Math::GMPq::Rmpq_cmp_ui($$x, 1, 1) < 0) {
        return Math::BigNum::Complex->new($x)->acosh;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_acosh($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub tan {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_tan($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub atan {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_atan($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub tanh {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_tanh($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub atanh {
    my ($x) = @_;

    # Return a complex number for x <= -1 or x >= 1
    if (Math::GMPq::Rmpq_cmp_ui($$x, 1, 1) >= 0 or Math::GMPq::Rmpq_cmp_si($$x, -1, 1) <= 0) {
        return Math::BigNum::Complex->new($x)->atanh;
    }

    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_atanh($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

sub sec {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_sec($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

#
## asec(x) = acos(1/x)
#
sub asec {
    my ($x) = @_;

    # Return a complex number for x > -1 and x < 1
    if (Math::GMPq::Rmpq_cmp_ui($$x, 1, 1) < 0 and Math::GMPq::Rmpq_cmp_si($$x, -1, 1) > 0) {
        return Math::BigNum::Complex->new($x)->asec;
    }

    state $one = Math::MPFR->new(1);
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_div($r, $one, _as_float($x), $ROUND);
    Math::MPFR::Rmpfr_acos($r, $r, $ROUND);
    _mpfr2rat($r);
}

sub sech {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_sech($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

#
## asech(x) = acosh(1/x)
#
sub asech {
    my ($x) = @_;

    # Return a complex number for x < 0 or x > 1
    if (Math::GMPq::Rmpq_cmp_ui($$x, 1, 1) > 0 or Math::GMPq::Rmpq_cmp_ui($$x, 0, 1) < 0) {
        return Math::BigNum::Complex->new($x)->asech;
    }

    state $one = Math::MPFR->new(1);
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_div($r, $one, _as_float($x), $ROUND);
    Math::MPFR::Rmpfr_acosh($r, $r, $ROUND);
    _mpfr2rat($r);
}

sub csc {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_csc($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

#
## acsc(x) = asin(1/x)
#
sub acsc {
    my ($x) = @_;

    # Return a complex number for x > -1 and x < 1
    if (Math::GMPq::Rmpq_cmp_ui($$x, 1, 1) < 0 and Math::GMPq::Rmpq_cmp_si($$x, -1, 1) > 0) {
        return Math::BigNum::Complex->new($x)->acsc;
    }

    state $one = Math::MPFR->new(1);
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_div($r, $one, _as_float($x), $ROUND);
    Math::MPFR::Rmpfr_asin($r, $r, $ROUND);
    _mpfr2rat($r);
}

sub csch {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_csch($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

#
## acsch(x) = asinh(1/x)
#
sub acsch {
    my ($x) = @_;
    state $one = Math::MPFR->new(1);
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_div($r, $one, _as_float($x), $ROUND);
    Math::MPFR::Rmpfr_asinh($r, $r, $ROUND);
    _mpfr2rat($r);
}

sub cot {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_cot($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

#
## acot(x) = atan(1/x)
#
sub acot {
    my ($x) = @_;
    state $one = Math::MPFR->new(1);
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_div($r, $one, _as_float($x), $ROUND);
    Math::MPFR::Rmpfr_atan($r, $r, $ROUND);
    _mpfr2rat($r);
}

sub coth {
    my ($x) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_coth($r, _as_float($x), $ROUND);
    _mpfr2rat($r);
}

#
## acoth(x) = atanh(1/x)
#
sub acoth {
    my ($x) = @_;
    state $one = Math::MPFR->new(1);
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_div($r, $one, _as_float($x), $ROUND);
    Math::MPFR::Rmpfr_atanh($r, $r, $ROUND);
    _mpfr2rat($r);
}

=head2 atan2

    $x->atan2(BigNum)           # => BigNum
    $x->atan2(Inf)              # => BigNum(0)
    $x->atan2(Ninf)             # => BigNum
    $x->atan2(Scalar)           # => BigNum
    atan2(Scalar, BigNum)       # => BigNum

Arctangent of $x and $y. When $y is Ninf returns PI when $x>=0 or -PI when $x < 0.

=cut

multimethod atan2 => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_atan2($r, _as_float($x), _as_float($y), $ROUND);
    _mpfr2rat($r);
};

multimethod atan2 => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_atan2($r, _as_float($x), _str2mpfr($y), $ROUND);
    _mpfr2rat($r);
};

multimethod atan2 => qw($ Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_atan2($r, _str2mpfr($x), _as_float($y), $ROUND);
    _mpfr2rat($r);
};

multimethod atan2 => qw(Math::BigNum Math::BigNum::Inf) => sub {
    ZERO;
};

multimethod atan2 => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    (Math::GMPq::Rmpq_sgn(${$_[0]}) >= 0) ? pi() : (pi()->neg);
};

#
## Comparisons
#

=head2 eq

    $x->eq($y)

Equality check: returns true when $x and $y are equal.

=cut

multimethod eq => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPq::Rmpq_equal(${$_[0]}, ${$_[1]});
};

multimethod eq => qw(Math::BigNum $) => sub {
    my $y = Math::BigNum->new($_[1]);
    Math::GMPq::Rmpq_equal(${$_[0]}, $$y);
};

multimethod eq => qw(Math::BigNum Math::BigNum::Complex) => sub {
    my ($x, $y) = @_;
    $y->im->is_zero && Math::GMPq::Rmpq_equal($$x, ${$y->re});
};

multimethod eq => qw(Math::BigNum Math::BigNum::Inf)  => sub { 0 };
multimethod eq => qw(Math::BigNum Math::BigNum::Ninf) => sub { 0 };
multimethod eq => qw(Math::BigNum Math::BigNum::Nan)  => sub { };

=head2 ne

    $x->ne($y)

Inequality check: returns true when $x and $y are not equal.

=cut

multimethod ne => qw(Math::BigNum Math::BigNum) => sub {
    !Math::GMPq::Rmpq_equal(${$_[0]}, ${$_[1]});
};

multimethod ne => qw(Math::BigNum $) => sub {
    my $y = Math::BigNum->new($_[1]);
    !Math::GMPq::Rmpq_equal(${$_[0]}, $$y);
};

multimethod ne => qw(Math::BigNum Math::BigNum::Complex) => sub {
    my ($x, $y) = @_;
    !($y->im->is_zero && Math::GMPq::Rmpq_equal($$x, ${$y->re}));
};

multimethod ne => qw(Math::BigNum Math::BigNum::Inf)  => sub { 1 };
multimethod ne => qw(Math::BigNum Math::BigNum::Ninf) => sub { 1 };
multimethod ne => qw(Math::BigNum Math::BigNum::Nan)  => sub { 1 };

=head2 gt

    $x->gt(BigNum)             # => Bool
    $x->gt(Complex)            # => Bool
    $x->gt(Scalar)             # => Bool
    gt(Scalar, BigNum)         # => Bool

Returns a true value when $x is greater than $y.

=cut

multimethod gt => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPq::Rmpq_cmp(${$_[0]}, ${$_[1]}) > 0;
};

multimethod gt => qw(Math::BigNum $) => sub {
    $_[0]->cmp($_[1]) > 0;
};

multimethod gt => qw($ Math::BigNum) => sub {
    $_[1]->cmp($_[0]) < 0;
};

multimethod gt => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->lt($_[0]);
};

multimethod gt => qw(Math::BigNum Math::BigNum::Inf)  => sub { 0 };
multimethod gt => qw(Math::BigNum Math::BigNum::Ninf) => sub { 1 };
multimethod gt => qw(Math::BigNum Math::BigNum::Nan)  => sub { };

=head2 ge

    $x->ge(BigNum)               # => Bool
    $x->ge(Complex)              # => Bool
    $x->ge(Scalar)               # => Bool
    ge(Scalar, BigNum)           # => Bool

Returns a true value when $x is equal or greater than $y.

=cut

multimethod ge => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPq::Rmpq_cmp(${$_[0]}, ${$_[1]}) >= 0;
};

multimethod ge => qw(Math::BigNum $) => sub {
    $_[0]->cmp($_[1]) >= 0;
};

multimethod ge => qw($ Math::BigNum) => sub {
    $_[1]->cmp($_[0]) <= 0;
};

multimethod ge => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->le($_[0]);
};

multimethod ge => qw(Math::BigNum Math::BigNum::Inf)  => sub { 0 };
multimethod ge => qw(Math::BigNum Math::BigNum::Ninf) => sub { 1 };
multimethod ge => qw(Math::BigNum Math::BigNum::Nan)  => sub { };

=head2 lt

    $x->lt(BigNum)             # => Bool
    $x->lt(Complex)            # => Bool
    $x->lt(Scalar)             # => Bool
    lt(Scalar, BigNum)         # => Bool

Returns a true value when $x is less than $y.

=cut

multimethod lt => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPq::Rmpq_cmp(${$_[0]}, ${$_[1]}) < 0;
};

multimethod lt => qw(Math::BigNum $) => sub {
    $_[0]->cmp($_[1]) < 0;
};

multimethod lt => qw($ Math::BigNum) => sub {
    $_[1]->cmp($_[0]) > 0;
};

multimethod lt => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->gt($_[0]);
};

multimethod lt => qw(Math::BigNum Math::BigNum::Inf)  => sub { 1 };
multimethod lt => qw(Math::BigNum Math::BigNum::Ninf) => sub { 0 };
multimethod lt => qw(Math::BigNum Math::BigNum::Nan)  => sub { };

=head2 le

    $x->le(BigNum)                # => Bool
    $x->le(Complex)               # => Bool
    $x->le(Scalar)                # => Bool
    le(Scalar, BigNum)            # => Bool

Returns a true value when $x is equal or less than $y.

=cut

multimethod le => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPq::Rmpq_cmp(${$_[0]}, ${$_[1]}) <= 0;
};

multimethod le => qw(Math::BigNum $) => sub {
    $_[0]->cmp($_[1]) <= 0;
};

multimethod le => qw($ Math::BigNum) => sub {
    $_[1]->cmp($_[0]) >= 0;
};

multimethod le => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->ge($_[0]);
};

multimethod le => qw(Math::BigNum Math::BigNum::Inf)  => sub { 1 };
multimethod le => qw(Math::BigNum Math::BigNum::Ninf) => sub { 0 };
multimethod le => qw(Math::BigNum Math::BigNum::Nan)  => sub { };

=head2 cmp

    $x->cmp(BigNum)         # => Scalar
    $x->cmp(Complex)        # => Scalar
    $x->cmp(Scalar)         # => Scalar
    cmp(Scalar, BigNum)     # => Scalar

Compares $x to $y and returns a negative value when $x is less than $y,
0 when $x and $y are equal, and a positive value when $x is greater than $y.

=cut

multimethod cmp => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPq::Rmpq_cmp(${$_[0]}, ${$_[1]});
};

multimethod cmp => qw(Math::BigNum Math::BigNum::Inf) => sub {
    -1;
};

multimethod cmp => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    1;
};

multimethod cmp => qw(Math::BigNum Math::BigNum::Nan) => sub {

};

multimethod cmp => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if (CORE::int($y) == $y) {
        $y > 0
          ? Math::GMPq::Rmpq_cmp_ui($$x, $y, 1)
          : Math::GMPq::Rmpq_cmp_si($$x, $y, 1);
    }
    else {
        Math::GMPq::Rmpq_cmp($$x, _str2mpq($_[1]));
    }
};

multimethod cmp => qw($ Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (CORE::int($x) == $x) {
        my $cmp =
          $x > 0
          ? Math::GMPq::Rmpq_cmp_ui($$y, $x, 1)
          : Math::GMPq::Rmpq_cmp_si($$y, $x, 1);
        $cmp < 0 ? 1 : $cmp > 0 ? -1 : 0;
    }
    else {
        Math::GMPq::Rmpq_cmp(_str2mpq($_[0]), $$y);
    }
};

=head2 acmp

    $x->acmp(BigNum)         # => Scalar
    $x->acmp(Complex)        # => Scalar
    cmp(Scalar, BigNum)      # => Scalar

Compares the absolute values of $x and $y. Returns a negative value
when the absolute value of $x is less than the absolute value of $y,
0 when the absolute value of $x is equal with the absolute value of $y,
and a positive value when the absolute value of $x is greater than the
absolute value of $y.

=cut

multimethod acmp => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $xn = $$x;
    my $yn = $$y;

    if (Math::GMPq::Rmpq_sgn($xn) < 0) {
        my $r = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_abs($r, $xn);
        $xn = $r;
    }

    if (Math::GMPq::Rmpq_sgn($yn) < 0) {
        my $r = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_abs($r, $yn);
        $yn = $r;
    }

    Math::GMPq::Rmpq_cmp($xn, $yn);
};

=head2 mod

    $x->mod(BigNum)      # BigNum | Nan

Remainder of $x divided by $y. ($x % $y)

=cut

multimethod mod => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_integer_p($$x) and Math::GMPq::Rmpq_integer_p($$y)) {
        my $r      = Math::GMPz::Rmpz_init();
        my $yz     = _as_int($y);
        my $sign_y = Math::GMPz::Rmpz_sgn($yz);
        return NAN if !$sign_y;
        Math::GMPz::Rmpz_mod($r, _as_int($x), $yz);
        Math::GMPz::Rmpz_add($r, $r, $yz) if ($sign_y < 0);
        _mpz2rat($r);
    }
    else {
        my $r  = Math::MPFR::Rmpfr_init2($PREC);
        my $yf = _as_float($y);
        Math::MPFR::Rmpfr_fmod($r, _as_float($x), $yf, $ROUND);
        my $sign = Math::MPFR::Rmpfr_sgn($r);
        if (!$sign) {
            return (ZERO);
        }
        elsif (($sign > 0) ne (Math::MPFR::Rmpfr_sgn($yf) > 0)) {
            Math::MPFR::Rmpfr_add($r, $r, $yf, $ROUND);
        }
        _mpfr2rat($r);
    }
};

multimethod mod => qw(Math::BigNum $) => sub {
    $_[0]->mod(Math::BigNum->new($_[1]));
};

#
## Miscellaneous
#

sub is_zero {
    CORE::not Math::GMPq::Rmpq_sgn(${$_[0]});
}

sub is_one {
    defined($_[1]) && $_[1] eq '-'
      ? Math::GMPq::Rmpq_equal(${$_[0]}, ${(MONE)})
      : Math::GMPq::Rmpq_equal(${$_[0]}, ${(ONE)});
}

sub is_pos {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
}

sub is_neg {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
}

sub sign {
    my $sign = Math::GMPq::Rmpq_sgn(${$_[0]});
    $sign > 0 ? '+' : $sign < 0 ? '-' : '';
}

sub is_int {
    Math::GMPq::Rmpq_integer_p(${$_[0]});
}

sub is_real { 1 }
sub is_inf  { 0 }
sub is_nan  { 0 }
sub is_ninf { 0 }

sub is_even {
    my ($x) = @_;

    if (CORE::not Math::GMPq::Rmpq_integer_p($$x)) {
        return 0;
    }

    my $nz = Math::GMPz::Rmpz_init();
    Math::GMPq::Rmpq_get_num($nz, $$x);

    Math::GMPz::Rmpz_even_p($nz);
}

sub is_odd {
    my ($x) = @_;

    if (CORE::not Math::GMPq::Rmpq_integer_p($$x)) {
        return 0;
    }

    my $nz = Math::GMPz::Rmpz_init();
    Math::GMPq::Rmpq_get_num($nz, $$x);

    Math::GMPz::Rmpz_odd_p($nz);
}

multimethod max => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_cmp($$x, $$y) > 0 ? $x : $y;
};

multimethod min => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_cmp($$x, $$y) < 0 ? $x : $y;
};

sub int {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    _mpz2rat($z);
}

*as_int = \&int;

sub bint {
    my ($x) = @_;
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, $$x);
    Math::GMPq::Rmpq_set_z($$x, $z);
    $x;
}

sub float {
    my $f = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_set_q($f, ${$_[0]}, $ROUND);
    _mpfr2rat($f);
}

*as_float = \&float;

sub as_rat {
    my $rat = Math::GMPq::Rmpq_get_str(${$_[0]}, 10);
    index($rat, '/') == -1 ? "$rat/1" : $rat;
}

sub as_bin {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_get_str($z, 2);
}

sub as_oct {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_get_str($z, 8);
}

sub as_hex {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_get_str($z, 16);
}

sub digits {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_abs($z, $z);
    my @digits = (map { _new_uint($_) } split(//, Math::GMPz::Rmpz_get_str($z, 10)));
    @digits;
}

sub length {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_abs($z, $z);
    _new_uint(Math::GMPz::Rmpz_snprintf(my $buf, 0, "%Zd", $z, 0));
}

sub floor {
    my ($x) = @_;
    Math::GMPq::Rmpq_integer_p($$x) && return $x;

    if (Math::GMPq::Rmpq_sgn($$x) > 0) {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        _mpz2rat($z);
    }
    else {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_sub_ui($z, $z, 1);
        _mpz2rat($z);
    }
}

sub ceil {
    my ($x) = @_;
    Math::GMPq::Rmpq_integer_p($$x) && return $x;

    if (Math::GMPq::Rmpq_sgn($$x) > 0) {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_add_ui($z, $z, 1);
        _mpz2rat($z);
    }
    else {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        _mpz2rat($z);
    }
}

sub inc {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_add($r, $$x, ${(ONE)});
    bless \$r, __PACKAGE__;
}

sub binc {
    my ($x) = @_;
    Math::GMPq::Rmpq_add($$x, $$x, ${(ONE)});
    $x;
}

sub dec {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_sub($r, $$x, ${(ONE)});
    bless \$r, __PACKAGE__;
}

sub bdec {
    my ($x) = @_;
    Math::GMPq::Rmpq_sub($$x, $$x, ${(ONE)});
    $x;
}

#
## Integer operations
#

multimethod expmod => qw(Math::BigNum Math::BigNum Math::BigNum) => sub {
    my ($x, $y, $z) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_powm($r, _as_int($x), _as_int($y), _as_int($z));
    _mpz2rat($r);
};

multimethod and => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_and($r, _as_int($x), _as_int($y));
    _mpz2rat($r);
};

multimethod or => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_ior($r, _as_int($x), _as_int($y));
    _mpz2rat($r);
};

multimethod xor => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_xor($r, _as_int($x), _as_int($y));
    _mpz2rat($r);
};

sub not {
    my ($x) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_com($r, _as_int($x));
    _mpz2rat($r);
}

multimethod lsft => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_mul_2exp($r, _as_int($x), CORE::int(Math::GMPq::Rmpq_get_d($$y)));
    _mpz2rat($r);
};

multimethod lsft => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_mul_2exp($r, _as_int($x), CORE::int($y));
    _mpz2rat($r);
};

multimethod rsft => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_div_2exp($r, _as_int($x), CORE::int(Math::GMPq::Rmpq_get_d($$y)));
    _mpz2rat($r);
};

multimethod rsft => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_div_2exp($r, _as_int($x), CORE::int($y));
    _mpz2rat($r);
};

=head2 fac

    $x->fac                  # => BigNum | Nan
    BigNum::fac(Scalar)      # => BigNum | Nan

Factorial of $x. Returns Nan when $x is negative. (1*2*3*...*$x)

=cut

multimethod fac => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return NAN if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fac_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2rat($r);
};

multimethod fac => qw($) => sub {
    my ($x) = @_;
    return NAN if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fac_ui($r, CORE::int($x));
    _mpz2rat($r);
};

=head2 dfac

    $x->dfac                 # => BigNum | Nan
    BigNum::dfac(Scalar)     # => BigNum | Nan

Double factorial of $x. Returns Nan when $x is negative.

=cut

multimethod dfac => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return NAN if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_2fac_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2rat($r);
};

multimethod dfac => qw($) => sub {
    my ($x) = @_;
    return NAN if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_2fac_ui($r, CORE::int($x));
    _mpz2rat($r);
};

=head2 prim

    $x->prim            # => BigNum | Nan
    prim(Scalar)        # => BigNum | Nan

Primorial of $x. Returns Nan when $x is negative. (2*3*5*7*11*...*$x)

=cut

multimethod prim => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return NAN if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_primorial_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2rat($r);
};

multimethod prim => qw($) => sub {
    my ($x) = @_;
    return NAN if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_primorial_ui($r, CORE::int($x));
    _mpz2rat($r);
};

=head2 fib

    $n->fib          # => BigNum | Nan
    fib(Scalar)      # => BigNum | Nan

The $n'th Fibonacci number. Returns Nan when $n is negative.

=cut

multimethod fib => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return NAN if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fib_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2rat($r);
};

multimethod fib => qw($) => sub {
    my ($x) = @_;
    return NAN if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fib_ui($r, CORE::int($x));
    _mpz2rat($r);
};

=head2 binomial

    $n->binomial(BigNum)    # => BigNum
    $n->binomial(Scalar)    # => BigNum

=cut

multimethod binomial => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_bin_si($r, _as_int($x), CORE::int(Math::GMPq::Rmpq_get_d($$y)));
    _mpz2rat($r);
};

multimethod binomial => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_bin_si($r, _as_int($x), CORE::int($y));
    _mpz2rat($r);
};

#
## Special methods
#

=head2 agm

    $x->agm(BigNum)                 # => BigNum
    $x->agm(Scalar)                 # => BigNum
    BigNum::agm(Scalar, Scalar)     # => BigNum

Arithmetic-geometric mean of $x and $y.

=cut

multimethod agm => qw(Math::BigNum Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_agm($r, _as_float($_[0]), _as_float($_[1]), $ROUND);
    _mpfr2rat($r);
};

multimethod agm => qw(Math::BigNum $) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_agm($r, _as_float($_[0]), _str2mpfr($_[1]), $ROUND);
    _mpfr2rat($r);
};

multimethod agm => qw($ $) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_agm($r, _str2mpfr($_[0]), _str2mpfr($_[1]), $ROUND);
    _mpfr2rat($r);
};

multimethod hypot => qw(Math::BigNum Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_hypot($r, _as_float($_[0]), _as_float($_[1]), $ROUND);
    _mpfr2rat($r);
};

multimethod hypot => qw(Math::BigNum $) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_hypot($r, _as_float($_[0]), _str2mpfr($_[1]), $ROUND);
    _mpfr2rat($r);
};

multimethod hypot => qw($ $) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_hypot($r, _str2mpfr($_[0]), _str2mpfr($_[1]), $ROUND);
    _mpfr2rat($r);
};

multimethod gamma => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_gamma($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod gamma => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_gamma($r, _str2mpfr($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod lngamma => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_lngamma($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod lngamma => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_lngamma($r, _str2mpfr($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod lgamma => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_lgamma($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod lgamma => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_lgamma($r, _str2mpfr($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod digamma => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_digamma($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod digamma => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_digamma($r, _str2mpfr($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod zeta => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_zeta($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod zeta => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_zeta($r, _str2mpfr($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod erf => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_erf($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod erf => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_erf($r, _str2mpfr($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod erfc => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_erfc($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod erfc => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_erfc($r, _mpfr2rat($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod eint => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_eint($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod eint => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_eint($r, _str2mpfr($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod li2 => qw(Math::BigNum) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_li2($r, _as_float($_[0]), $ROUND);
    _mpfr2rat($r);
};

multimethod li2 => qw($) => sub {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_li2($r, _str2mpfr($_[0]), $ROUND);
    _mpfr2rat($r);
};

=head1 AUTHOR

Daniel Șuteu, C<< <trizenx at gmail.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-math-bignum at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Math-BigNum>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Math::BigNum


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Math-BigNum>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Math-BigNum>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Math-BigNum>

=item * Search CPAN

L<http://search.cpan.org/dist/Math-BigNum/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2016 Daniel Șuteu.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

1;    # End of Math::BigNum
