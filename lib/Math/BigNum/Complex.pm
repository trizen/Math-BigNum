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

  '>'  => sub { $_[2] ? Math::BigNum::Complex->new($_[1])->gt($_[0]) : $_[0]->gt($_[1]) },
  '>=' => sub { $_[2] ? Math::BigNum::Complex->new($_[1])->ge($_[0]) : $_[0]->ge($_[1]) },
  '<'  => sub { $_[2] ? Math::BigNum::Complex->new($_[1])->lt($_[0]) : $_[0]->lt($_[1]) },
  '<=' => sub { $_[2] ? Math::BigNum::Complex->new($_[1])->le($_[0]) : $_[0]->le($_[1]) },

  '**'  => sub { $_[2] ? Math::BigNum::Complex->new($_[1])->pow($_[0])   : $_[0]->pow($_[1]) },
  '-'   => sub { $_[2] ? Math::BigNum::Complex->new($_[1])->sub($_[0])   : $_[0]->sub($_[1]) },
  '/'   => sub { $_[2] ? Math::BigNum::Complex->new($_[1])->div($_[0])   : $_[0]->div($_[1]) },
  atan2 => sub { $_[2] ? Math::BigNum::Complex->new($_[1])->atan2($_[0]) : $_[0]->atan2($_[1]) },

  sin  => sub { $_[0]->sin },
  cos  => sub { $_[0]->cos },
  exp  => sub { $_[0]->exp },
  log  => sub { $_[0]->ln },
  int  => sub { $_[0]->int },
  abs  => sub { $_[0]->abs },
  sqrt => sub { $_[0]->sqrt };

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
    my $re = $_[0]->re;
    my $im = $_[0]->im;

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

sub re {
    my $mpfr = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPC::RMPC_RE($mpfr, ${$_[0]});
    Math::BigNum::_mpfr2rat($mpfr);
}

sub im {
    my $mpfr = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPC::RMPC_IM($mpfr, ${$_[0]});
    Math::BigNum::_mpfr2rat($mpfr);
}

#
## Arithmetic
#

=head2 add

    $x->add(Complex)        # Complex
    $x->add(BigNum)         # Complex

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
    $x->sub(BigNum)         # Complex

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
    Math::MPC::Rmpc_add_fr($r, $$x, -$y->_as_float(), $ROUND);

    bless \$r, __PACKAGE__;
};

=head2 mul

    $x->mul(Complex)        # Complex
    $x->mul(BigNum)         # Complex

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

=head2 div

    $x->div(Complex)        # Complex
    $x->div(BigNum)         # Complex

Divides $x by $y and returns the result.

=cut

multimethod div => qw(Math::BigNum::Complex Math::BigNum::Complex) => sub {
    my ($x, $y) = @_;

    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_div($r, $$x, $$y, $ROUND);

    bless \$r, __PACKAGE__;
};

multimethod div => qw(Math::BigNum::Complex Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_div_fr($r, $$x, $y->_as_float(), $ROUND);

    bless \$r, __PACKAGE__;
};

=head2 sqrt

    $x->sqrt        # => Complex

Square root of $x. ($x**(1/2))

=cut

sub sqrt {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_sqrt($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

=head2 cbrt

    $x->cbrt        # => Complex

Cube root of $x. ($x**(1/3))

=cut

sub cbrt {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    state $three_inv = do {
        my $r = Math::MPC::Rmpc_init2($PREC);
        Math::MPC::Rmpc_set_ui($r, 3, $ROUND);
        Math::MPC::Rmpc_ui_div($r, 1, $r, $ROUND);
        $r;
    };
    Math::MPC::Rmpc_pow($r, $$x, $three_inv, $ROUND);
    bless(\$r, __PACKAGE__);
}

=head2 ln

    $x->ln       # => Complex

Natural logarithm of $x.

=cut

sub ln {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_log($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

=head2 log2

    $x->log2     # => Complex

Logarithm to the base 2 of $x.

=cut

sub log2 {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_log($r, $$x, $ROUND);

    state $two = (Math::MPFR::Rmpfr_init_set_ui(2, $Sidef::Types::Number::Number::ROUND))[0];

    my $baseln = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_log($baseln, $two, $Sidef::Types::Number::Number::ROUND);
    Math::MPC::Rmpc_div_fr($r, $r, $baseln, $ROUND);

    bless(\$r, __PACKAGE__);
}

=head2 log10

    $x->log10     # => Complex

Logarithm to the base 10 of $x.

=cut

sub log10 {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_log10($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

=head2 exp

    $x->exp     # => Complex

Exponential of $x in base e. (e**$x)

=cut

sub exp {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_exp($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

=head2 exp2

    $x->exp2     # => Complex

Exponential of $x in base 2. (2**$x)

=cut

sub exp2 {
    my ($x) = @_;
    state $two = Math::MPC->new(2);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_pow($r, $two, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

=head2 exp2

    $x->exp10     # => Complex

Exponential of $x in base 10. (10**$x)

=cut

sub exp10 {
    my ($x) = @_;
    state $ten = Math::MPC->new(10);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_pow($r, $ten, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

=head2 dec

    $x->dec     # => Complex

Subtract one from the real part of $x. ($x - 1)

=cut

sub dec {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_sub($r, $$x, $one, $ROUND);
    bless(\$r, __PACKAGE__);
}

=head2 inc

    $x->inc     # => Complex

Add one to the real part of $x. ($x + 1)

=cut

sub inc {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_add($r, $$x, $one, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## Trigonometric
#

sub sin {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_sin($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub asin {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_asin($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub sinh {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_sinh($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub asinh {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_asinh($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub cos {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_cos($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub acos {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_acos($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub cosh {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_cosh($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub acosh {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_acosh($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub tan {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_tan($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub atan {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_atan($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub tanh {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_tanh($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

sub atanh {
    my ($x) = @_;
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_atanh($r, $$x, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## csc(x) = 1/sin(x)
#
sub csc {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_sin($r, $$x, $ROUND);
    Math::MPC::Rmpc_div($r, $one, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## acsc(x) = asin(1/x)
#
sub acsc {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_div($r, $one, $$x, $ROUND);
    Math::MPC::Rmpc_asin($r, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## csch(x) = 1/sinh(x)
#
sub csch {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_sinh($r, $$x, $ROUND);
    Math::MPC::Rmpc_div($r, $one, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## acsch(x) = asinh(1/x)
#
sub acsch {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_div($r, $one, $$x, $ROUND);
    Math::MPC::Rmpc_asinh($r, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## sec(x) = 1/cos(x)
#
sub sec {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_cos($r, $$x, $ROUND);
    Math::MPC::Rmpc_div($r, $one, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## asec(x) = acos(1/x)
#
sub asec {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_div($r, $one, $$x, $ROUND);
    Math::MPC::Rmpc_acos($r, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## sech(x) = 1/cosh(x)
#
sub sech {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_cosh($r, $$x, $ROUND);
    Math::MPC::Rmpc_div($r, $one, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## asech(x) = acosh(1/x)
#
sub asech {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_div($r, $one, $$x, $ROUND);
    Math::MPC::Rmpc_acosh($r, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## cot(x) = 1/tan(x)
#
sub cot {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_tan($r, $$x, $ROUND);
    Math::MPC::Rmpc_div($r, $one, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## acot(x) = atan(1/x)
#
sub acot {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_div($r, $one, $$x, $ROUND);
    Math::MPC::Rmpc_atan($r, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## coth(x) = 1/tanh(x)
#
sub coth {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_tanh($r, $$x, $ROUND);
    Math::MPC::Rmpc_div($r, $one, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

#
## acoth(x) = atanh(1/x)
#
sub acoth {
    my ($x) = @_;
    state $one = Math::MPC->new(1);
    my $r = Math::MPC::Rmpc_init2($PREC);
    Math::MPC::Rmpc_div($r, $one, $$x, $ROUND);
    Math::MPC::Rmpc_atanh($r, $r, $ROUND);
    bless(\$r, __PACKAGE__);
}

1;
