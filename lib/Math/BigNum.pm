package Math::BigNum;

use 5.010;
use strict;
use warnings;

no warnings qw(qw);

use Math::GMPq qw();
use Math::GMPz qw();
use Math::MPFR qw();
use Math::BigRat qw();

use Class::Multimethods qw(multimethod);

=head1 NAME

Math::BigNum - Arbitrary size precision for integer, complex and floating-point numbers

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

    use Math::BigNum qw(:constant);
    print ((100->fac + 1)/2);

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
              ONE  => bless(\Math::GMPq->new('1'), __PACKAGE__),
              ZERO => bless(\Math::GMPq->new('0'), __PACKAGE__),
              MONE => bless(\Math::GMPq->new('-1'), __PACKAGE__),
             };

use Math::BigNum::Complex qw();

use constant {i => Math::BigNum::Complex->new(0, 1),};

use overload
  '""'  => \&stringify,
  '0+'  => \&numify,
  bool  => \&boolify,
  neg   => sub { $_[0]->neg },
  '+'   => sub { $_[0]->add($_[1]) },
  '*'   => sub { $_[0]->mul($_[1]) },
  '-'   => sub { $_[2] ? Math::BigNum->new($_[1])->sub($_[0]) : $_[0]->sub($_[1]) },
  '/'   => sub { $_[2] ? Math::BigNum->new($_[1])->div($_[0]) : $_[0]->div($_[1]) },
  atan2 => sub { $_[2] ? Math::BigNum->new($_[1])->atan2($_[0]) : $_[0]->atan2($_[1]) },
  abs  => sub { $_[0]->abs },
  sqrt => sub { $_[0]->sqrt };

sub import {
    shift;

    my $callback = caller(0);

    foreach my $name (@_) {
        if ($name eq ':constant') {
            overload::constant
              integer => sub { _new_uint(shift) },
              float   => sub { Math::BigNum->new(shift, 10) };
        }
        elsif ($name eq 'i') {
            no strict 'refs';
            *{$callback . '::' . 'i'} = \&i;
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
    $_[1];
};

multimethod new => qw($ #) => sub {
    bless(\Math::GMPq->new(_str2rat($_[1]), 10), $_[0]);
};

multimethod new => qw($ $) => sub {
    bless(\Math::GMPq->new(_str2rat($_[1]), 10), $_[0]);
};

multimethod new => qw($ $ #) => sub {
    bless(\Math::GMPq->new($_[2] == 10 ? _str2rat($_[1]) : $_[1], $_[2]), $_[0]);
};

multimethod new => qw($ # #) => sub {
    bless(\Math::GMPq->new(_str2rat($_[1]), $_[2]), $_[0]);
};

multimethod new => qw(Math::BigFloat #) => sub {
    bless(\Math::GMPq->new(_str2rat($_[1]), 10), __PACKAGE__);
};

multimethod new => qw(Math::BigFloat # #) => sub {
    bless(\Math::GMPq->new(_str2rat($_[1]), $_[2]), __PACKAGE__);
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

    $PREC = $PREC->get_value if ref($PREC);

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

*as_float = \&numify;

sub boolify {
    !!Math::GMPq::Rmpq_get_d(${$_[0]});
}

sub as_frac {
    Math::GMPq::Rmpq_get_str(${$_[0]}, 10);
}

sub in_base {
    my ($x, $y) = @_;

    $y = 0 + $y if ref($y);

    state $min = Math::GMPq->new(2);
    state $max = Math::GMPq->new(36);

    if (Math::GMPq::Rmpq_cmp($y, $min) < 0 or Math::GMPq::Rmpq_cmp($y, $max) > 0) {
        die "base must be between 2 and 36, got $y";
    }

    Math::GMPq::Rmpq_get_str(${$_[0]}, $y);
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
## Arithmetic
#

=head2 add

    $x->add(BigNum)       # => BigNum
    $x->add(SCALAR)       # => BigNum
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

multimethod add => qw(Math::BigNum #) => sub {
    $_[0]->add(Math::BigNum->new($_[1]));
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

=head2 sub

    $x->sub(BigNum)       # => BigNum
    $x->sub(SCALAR)       # => BigNum
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

multimethod sub => qw(Math::BigNum #) => sub {
    $_[0]->sub(Math::BigNum->new($_[1]));
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

=head2 mul

    $x->mul(BigNum)       # => BigNum
    $x->mul(SCALAR)       # => BigNum
    $x->mul(Complex)      # => Complex
    $x->mul(Inf)          # => BigNum or Nan
    $x->mul(Ninf)         # => BigNum or Nan

Multiplies $x by $y and returns the result.

=cut

multimethod mul => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_mul($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod mul => qw(Math::BigNum #) => sub {
    $_[0]->mul(Math::BigNum->new($_[1]));
};

multimethod mul => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->mul($_[1]);
};

multimethod mul => qw(Math::BigNum Math::BigNum::Inf) => sub {
    my ($x, $y) = @_;
    my $sign = Math::GMPq::Rmpq_sgn($$x);
    $sign < 0 ? NINF : $sign > 0 ? $y : NAN;
};

multimethod mul => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    my ($x, $y) = @_;
    my $sign = Math::GMPq::Rmpq_sgn($$x);
    $sign < 0 ? INF : $sign > 0 ? $y : NAN;
};

=head2 div

    $x->div(BigNum)       # => BigNum
    $x->div(SCALAR)       # => BigNum
    $x->div(Complex)      # => Complex
    $x->div(Inf)          # => BigNum
    $x->div(Ninf)         # => BigNum

Divides $x by $y and returns the result.

=cut

multimethod div => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (CORE::not Math::GMPq::Rmpq_sgn($$y)) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? NAN : $sign > 0 ? INF : NINF);
    }

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_div($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod div => qw(Math::BigNum #) => sub {
    $_[0]->div(Math::BigNum->new($_[1]));
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

=head2 neg

    $x->neg

Return the negative value of a number.

=cut

sub neg {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_neg($r, $$x);
    bless \$r, __PACKAGE__;
}

=head2 fac

    $x->fac     # => BigNum

Factorial of $x. (1*2*3*...*$x)

=cut

sub fac {
    my ($x) = @_;
    return NAN if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fac_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2rat($r);
}

=head2 abs

    $x->abs     # => BigNum

Absolute value of $x

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

    $x->cbrt    # => BigNum or Complex

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

    $x->root(Inf)         # => BigNum
    $x->root(Ninf)        # => BigNum
    $x->root(BigNum)      # => BigNum or Complex
    $x->root(Complex)     # => Complex

Nth root of $x. Returns a Complex number when is $x is negative.

=cut

multimethod root => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->pow($_[1]->inv);
};

multimethod root => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->root($_[1]);
};

multimethod root => qw(Math::BigNum Math::BigNum::Inf) => sub {
    ONE;
};

multimethod root => qw(Math::BigNum Math::BigNum::Ninf) => sub {
    ONE;
};

multimethod root => qw(Math::BigNum NAN) => sub {
    NAN;
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
