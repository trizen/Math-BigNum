package Math::BigNum::Complex;

use 5.010;
use strict;
use warnings;

no warnings qw(qw);

use Math::MPC qw();
use Math::MPFR qw();
use Math::BigNum qw();

use Class::Multimethods qw(multimethod);

our $ROUND = Math::MPC::MPC_RNDNN();

our ($PREC);
*PREC = \$Math::BigNum::PREC;

use overload q{""} => \&stringify;

use constant {
              NAN  => Math::BigNum::NAN,
              INF  => Math::BigNum::INF,
              NINF => Math::BigNum::NINF,
             };

sub new {
    my (undef, $x, $y) = @_;

    if (ref($x) eq 'Math::BigNum') {
        $x = $$x;
    }
    elsif (ref($x) eq __PACKAGE__) {
        return $x if not defined $y;
        if (ref($y) eq __PACKAGE__) {
            return $x->add($y);
        }
        else {
            return $x->add(__PACKAGE__->new($y));
        }
    }
    elsif (ref($x) eq '') {
        if ($x eq 'i' or $x eq '+i') {
            return __PACKAGE__->new(__PACKAGE__->new(0, 1), $y);
        }
        elsif ($x eq '-i') {
            return __PACKAGE__->new(__PACKAGE__->new(0, -1), $y);
        }
        elsif (substr($x, -1) eq 'i') {
            if ($x =~ /^(.+?)([+-].*?)i\z/) {
                my ($re, $im) = ($1, $2);
                if ($im eq '+') {
                    $im = 1;
                }
                elsif ($im eq '-') {
                    $im = -1;
                }
                return __PACKAGE__->new(__PACKAGE__->new($re, $im), $y);
            }
            else {
                return __PACKAGE__->new(__PACKAGE__->new(0, $x), $y);
            }
        }
    }

    if (not defined($y)) {
        my $r = Math::MPC::Rmpc_init2($PREC);
        if (ref($x) eq 'Math::GMPq') {
            Math::MPC::Rmpc_set_q($r, $x, $ROUND);
        }
        else {
            Math::MPC::Rmpc_set_str($r, $x, 10, $ROUND);
        }

        return (bless \$r, __PACKAGE__);
    }
    elsif (ref($y) eq 'Math::BigNum') {
        $y = $$y;
    }
    elsif (ref($y) eq __PACKAGE__) {
        return $y->add(__PACKAGE__->new($x));
    }
    elsif (ref($y) eq '') {
        if ($y eq 'i' or $y eq '+i') {
            return __PACKAGE__->new($x, __PACKAGE__->new(0, 1));
        }
        elsif ($y eq '-i') {
            return __PACKAGE__->new($x, __PACKAGE__->new(0, -1));
        }
        elsif (substr($y, -1) eq 'i') {
            if ($y =~ /^(.+?)([+-].*?)i\z/) {
                my ($re, $im) = ($1, $2);
                if ($im eq '+') {
                    $im = 1;
                }
                elsif ($im eq '-') {
                    $im = -1;
                }
                return __PACKAGE__->new($x, __PACKAGE__->new($re, $im));
            }
            else {
                return __PACKAGE__->new($x, __PACKAGE__->new(0, $y));
            }
        }
    }

    my $r = Math::MPC::Rmpc_init2($PREC);

    if (ref($x) eq 'Math::GMPq') {
        if (ref($y) eq 'Math::GMPq') {
            Math::MPC::Rmpc_set_q_q($r, $x, $y, $ROUND);
        }
        else {
            my $y_fr = Math::MPFR::Rmpfr_init2($PREC);
            Math::MPFR::Rmpfr_set_str($y_fr, $y, 10, $Math::BigNum::ROUND);
            Math::MPC::Rmpc_set_q_fr($r, $x, $y_fr, $ROUND);
        }
    }
    elsif (ref($y) eq 'Math::GMPq') {
        my $x_fr = Math::MPFR::Rmpfr_init2($PREC);
        Math::MPFR::Rmpfr_set_str($x_fr, $x, 10, $Math::BigNum::ROUND);
        Math::MPC::Rmpc_set_fr_q($r, $x_fr, $y, $ROUND);
    }
    else {
        my $x_fr = Math::MPFR::Rmpfr_init2($PREC);
        Math::MPFR::Rmpfr_set_str($x_fr, $x, 10, $Math::BigNum::ROUND);

        my $y_fr = Math::MPFR::Rmpfr_init2($PREC);
        Math::MPFR::Rmpfr_set_str($y_fr, $y, 10, $Math::BigNum::ROUND);

        Math::MPC::Rmpc_set_fr_fr($r, $x_fr, $y_fr, $ROUND);

        #my $x_q = Math::GMPq->new(Math::BigNum::_str2rat($x), 10);
        #my $y_q = Math::GMPq->new(Math::BigNum::_str2rat($y), 10);
        #Math::MPC::Rmpc_set_q_q($r, $x_q, $y_q, $ROUND);
    }

    bless \$r, __PACKAGE__;
}

sub stringify {
    my $re = $_[0]->real;
    my $im = $_[0]->imag;

    $re = "$re";
    $im = "$im";

    return $re if $im eq '0';
    my $sign = '+';

    if (substr($im, 0, 1) eq '-') {
        $sign = '-';
        substr($im, 0, 1, '');
    }

    $im = '' if $im eq '1';
    $re eq '0' ? $sign eq '+' ? "${im}i" : "$sign${im}i" : "$re$sign${im}i";
}

sub real {
    my $mpfr = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPC::RMPC_RE($mpfr, ${$_[0]});
    Math::BigNum::_mpfr2rat($mpfr);
}

sub imag {
    my $mpfr = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPC::RMPC_IM($mpfr, ${$_[0]});
    Math::BigNum::_mpfr2rat($mpfr);
}

#
## Arithmetic
#

=head2 add

    $x->add(Complex)        # Complex
    $x->add(BigNum)         # BigNum

Adds $x to $y and returns the result.

=cut

multimethod add => qw(Math::BigNum::Complex Math::BigNum::Complex) => sub {
    my ($x, $y) = @_;

    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_add($r, $$x, $$y, $ROUND);

    bless \$r, __PACKAGE__;
};

multimethod add => qw(Math::BigNum::Complex Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_add_fr($r, $$x, $y->_as_float(), $ROUND);

    bless \$r, __PACKAGE__;
};

=head2 sub

    $x->sub(Complex)        # Complex
    $x->sub(BigNum)         # BigNum

Subtracts $y from $x and returns the result.

=cut

multimethod sub => qw(Math::BigNum::Complex Math::BigNum::Complex) => sub {
    my ($x, $y) = @_;

    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_sub($r, $$x, $$y, $ROUND);

    bless \$r, __PACKAGE__;
};

multimethod sub => qw(Math::BigNum::Complex Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_sub_fr($r, $$x, $y->_as_float(), $ROUND);

    bless \$r, __PACKAGE__;
};

=head2 mul

    $x->mul(Complex)        # Complex
    $x->mul(BigNum)         # BigNum

Multiplies $x by $y and returns the result.

=cut

multimethod mul => qw(Math::BigNum::Complex Math::BigNum::Complex) => sub {
    my ($x, $y) = @_;

    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_mul($r, $$x, $$y, $ROUND);

    bless \$r, __PACKAGE__;
};

multimethod mul => qw(Math::BigNum::Complex Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_mul_fr($r, $$x, $y->_as_float(), $ROUND);

    bless \$r, __PACKAGE__;
};

1;
