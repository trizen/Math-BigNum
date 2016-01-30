package Math::BigNum::Inf;

use 5.010;
use strict;
use warnings;

no warnings qw(qw);

use Math::GMPq qw();
use Math::MPFR qw();
use Math::BigNum qw();

use Class::Multimethods qw(multimethod);

=encoding utf8

=head1 NAME

Math::BigNum::Inf - An abstraction for the +Infinity value.

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
  # (although, a benchmark has never been done)
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
  atan2 => sub { Math::BigNum::Inf::atan2($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) };

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
    $_[0]->is_pos ? 'inf' : '-inf';
}

sub numify {
    $_[0]->is_pos ? +'inf' : -'inf';
}

sub copy {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set($r, ${$_[0]});
    bless \$r, __PACKAGE__;
}

*nan = \&Math::BigNum::nan;

*mod    = \&nan;
*expmod = \&nan;
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

sub is_pos {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
}

sub is_neg {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
}

sub neg {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_neg($r, ${$_[0]});
    bless \$r, __PACKAGE__;
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

multimethod add => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    my ($x, $y) = @_;
    $x->eq($y) ? $x->copy : nan();
};

multimethod add => qw(Math::BigNum::Inf $)                     => sub { $_[0]->copy };
multimethod add => qw(Math::BigNum::Inf Math::BigNum)          => sub { $_[0]->copy };
multimethod add => qw(Math::BigNum::Inf Math::BigNum::Complex) => sub { $_[0]->copy };
multimethod add => qw(Math::BigNum::Inf Math::BigNum::Nan)     => sub { nan() };

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

=head2 div

=cut

multimethod div => qw(Math::BigNum::Inf Math::BigNum) => sub {
    my ($x, $y) = @_;
    $y->is_neg ? $x->neg : $x->copy;
};

multimethod div => qw(Math::BigNum::Inf Math::BigNum::Inf) => \&nan;

#
## Trigonometric functions
#

=head2 atan

=cut

sub atan {
    $_[0]->is_neg
      ? Math::BigNum->pi->div(-2)
      : Math::BigNum->pi->div(2);
}

#
## Comparisons
#

=head eq

=cut

multimethod eq => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) == Math::GMPq::Rmpq_sgn(${$_[1]});
};

=head ne

=cut

multimethod ne => qw(Math::BigNum::Inf Math::BigNum::Inf) => sub {
    Math::GMPq::Rmpq_sgn(${$_[0]}) != Math::GMPq::Rmpq_sgn(${$_[1]});
};

1;
