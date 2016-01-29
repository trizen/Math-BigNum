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
  q{""} => sub { $_[0]->is_pos ? 'inf'  : '-inf' },
  q{0+} => sub { $_[0]->is_pos ? +'inf' : -'inf' };

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

multimethod div => qw(Math::BigNum::Inf Math::BigNum) => sub {
    my ($x, $y) = @_;
    $y->is_neg ? $x->neg : $x->copy;
};

multimethod div => qw(Math::BigNum::Inf Math::BigNum::Inf) => \&nan;

1;
