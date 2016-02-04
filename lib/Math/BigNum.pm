package Math::BigNum;

use 5.014;
use strict;
use warnings;

no warnings qw(qw);

use Math::GMPq qw();
use Math::GMPz qw();
use Math::MPFR qw();

use Class::Multimethods qw(multimethod);

=encoding utf8

=head1 NAME

Math::BigNum - Arbitrary size precision for integers, rationals and floating-point numbers

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

    use Math::BigNum qw(:constant);
    print 1/2 * (100->fac + 1);

=head1 DESCRIPTION

Math::BigNum provides a transparent interface to Math::GMPz, Math::GMPq and Math::MPFR.

In most cases, I<Math::BigNum> can be used as a drop-in replacement for the I<bignum>
and I<bigrat> pragmas.

I<Math::BigNum> provides an arbitrary size precision for integers, rationals and floating-point
numbers, focusing on performance and transparency.

=head1 MOTIVATION

This module came into existence as a response to Dana Jacobsen's request for a transparent
interface to I<Math::GMPz> and I<Math::MPFR>, that he talked about at the YAPC NA, in 2015.
See he's great presentation at: L<https://www.youtube.com/watch?v=Dhl4_Chvm_g>.

The main aim of this module is to provide a fast and correct alternative to I<Math::Big{Float,Int,Rat}>.

=head1 HOW IT WORKS

I<Math::BigNum> tries really hard to do the right thing and as efficiently as possible.
For example, if you say C<$x**$y>, it first checks to see if C<$x> and <$y> are integers,
so it can optimize the operation to integer exponentiation, by calling the corresponding
I<mpz> function. Otherwise, it will fallback to the corresponding C<mpfr> function.

All numbers in I<Math::BigNum> are stored as rational I<Math::GMPq> objects. Each operation
outside the functions provided by I<Math::GMPq>, is done by converting the internal objects to
I<Math::GMPz> or I<Math::MPFR> objects and calling the corresponding functions, converting
the results back to I<Math::GMPq> objects, without loosing any precision in the process.

=head1 IMPORT/EXPORT

I<Math::BigNum> does not export anything by default, but it recognizes the following list of words:

    :constant       # will make any number a Math::BigNum object
                    # it will also export the "Inf" and "NaN" constants,
                    # which represent +Infinity and NaN special values

    e               # "e" constant (2.7182...)
    pi              # "pi" constant (3.1415...)
    tau             # "tau" constant (which is: 2*pi)
    phi             # Golden ratio constant (1.618...)
    G               # Catalan's constant (0.91596...)
    Y               # Euler-Mascheroni constant (0.57721...)
    Inf             # +Infinity constant
    NaN             # Not-a-Number constant

The syntax for importing something, is:

    use Math::BigNum qw(:constant pi);
    say cos(2*pi);

B<NOTE:> C<:constant> is lexical to the current scope only.

=head1 PRECISION

The default precision of floating-point numbers is 128 bits, which is equivalent with about 32
digits of precision in base 10.

The precision can be changed by modifying the C<$Math::BigNum::PREC> variable, such as:

    local $Math::BigNum::PREC = 1024;

However, a very important thing to take into account, it's the fact that an individual number
do B<*NOT*> have a specific precision stored inside, therefore it's not possible to create
I<Math::BigNum> objects with a specific precision. All numbers can grow or shrink dynamically,
regardless of the global precision.

The global precision controls only the precision of the floating-point functions and the stringification
of floating-point numbers.

For example, let's change the precision to 3 decimal digits (where C<4> is the conversion factor):

    local $Math::BigNum::PREC = 3*4
    say sqrt(2);                   # => 1.414
    say 98**7;                     # => 86812553324672
    say 1 / 98**7                  # => 1.15e-14

As shown above, integers do not obey the global precision, as they can grow or shrink, virtually, without any limit.
This is true for rational numbers as well.

A rational number never losses precision in rational operations, therefore if we say:

    my $x = 1 / 3;
    say $x * 3;                    # => 1
    say 1 / $x;                    # => 3
    say 3 / $x;                    # => 9

...the results are exactly what we expect.

=head1 NOTATIONS

Methods that begin with a B<b> followed by the actual name (such as C<bsqrt>), are mutable methods that change the self
object in-place, while their counter-parts (such as C<sqrt>) do not. Instead, they will create and return a new object.

The returned types are noted as follows:

    BigNum      #-> a "Math::BigNum" object
    Inf         #-> a "Math::BigNum::Inf" object
    Nan         #-> a "Math::BigNum::Nan" object
    Scalar      #-> a Perl number or a string
    Bool        #-> true or false (actually: 1 or 0)

When two or more types are separated with pipe characters (B<|>), it means that the corresponding function can
return any of the specified types.

=head1 SUBROUTINES/METHODS

=cut

our $VERSION = '0.01';

our ($ROUND, $PREC);

BEGIN {
    $ROUND = Math::MPFR::MPFR_RNDN();
    $PREC  = 128;                       # too little?
}

use Math::BigNum::Inf qw();
use Math::BigNum::Nan qw();

my $MONE = do {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_si($r, -1, 1);
    $r;
};

my $ZERO = do {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_ui($r, 0, 1);
    $r;
};

my $ONE = do {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_ui($r, 1, 1);
    $r;
};

my $ONE_Z = Math::GMPz::Rmpz_init_set_ui(1);

use overload
  '""' => \&stringify,
  '0+' => \&numify,
  bool => \&boolify,

  '=' => \&copy,

  # Some shortcuts for speed
  '+='  => sub { $_[0]->badd($_[1]) },
  '-='  => sub { $_[0]->bsub($_[1]) },
  '*='  => sub { $_[0]->bmul($_[1]) },
  '/='  => sub { $_[0]->bdiv($_[1]) },
  '%='  => sub { $_[0]->bmod($_[1]) },
  '**=' => sub { $_[0]->bpow($_[1]) },

  '^='  => sub { $_[0]->bxor($_[1]) },
  '&='  => sub { $_[0]->band($_[1]) },
  '|='  => sub { $_[0]->bior($_[1]) },
  '<<=' => sub { $_[0]->blsft($_[1]) },
  '>>=' => sub { $_[0]->brsft($_[1]) },

  '+' => sub { $_[0]->add($_[1]) },
  '*' => sub { $_[0]->mul($_[1]) },

  '==' => sub { $_[0]->eq($_[1]) },
  '!=' => sub { $_[0]->ne($_[1]) },
  '&'  => sub { $_[0]->and($_[1]) },
  '|'  => sub { $_[0]->ior($_[1]) },
  '^'  => sub { $_[0]->xor($_[1]) },
  '~'  => \&not,

  '++' => \&binc,
  '--' => \&bdec,

  '>'   => sub { Math::BigNum::gt($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '>='  => sub { Math::BigNum::ge($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<'   => sub { Math::BigNum::lt($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<='  => sub { Math::BigNum::le($_[2]  ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<=>' => sub { Math::BigNum::cmp($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  '>>' => sub { Math::BigNum::rsft($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '<<' => sub { Math::BigNum::lsft($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  '**' => sub { Math::BigNum::pow($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '-'  => sub { Math::BigNum::sub($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '/'  => sub { Math::BigNum::div($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },
  '%'  => sub { Math::BigNum::mod($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  atan2 => sub { Math::BigNum::atan2($_[2] ? ($_[1], $_[0]) : ($_[0], $_[1])) },

  eq => sub { "$_[0]" eq "$_[1]" },
  ne => sub { "$_[0]" ne "$_[1]" },

  cmp => sub { $_[2] ? "$_[1]" cmp $_[0]->stringify : $_[0]->stringify cmp "$_[1]" },

  neg  => \&neg,
  sin  => \&sin,
  cos  => \&cos,
  exp  => \&exp,
  log  => \&ln,
  int  => \&int,
  abs  => \&abs,
  sqrt => \&sqrt;

{
    my %constants = (
                     e   => \&e,
                     phi => \&phi,
                     tau => \&tau,
                     pi  => \&pi,
                     Y   => \&Y,
                     G   => \&G,
                     Inf => \&inf,
                     NaN => \&nan,
                    );

    sub import {
        shift;

        my $caller = caller(0);

        foreach my $name (@_) {
            if ($name eq ':constant') {
                overload::constant
                  integer => sub { _new_uint(shift) },
                  float   => sub { Math::BigNum->new(shift, 10) },
                  binary => sub {
                    my ($const) = @_;
                    my $prefix = substr($const, 0, 2);
                        $prefix eq '0x' ? Math::BigNum->new(substr($const, 2), 16)
                      : $prefix eq '0b' ? Math::BigNum->new(substr($const, 2), 2)
                      :                   Math::BigNum->new(substr($const, 1), 8);
                  },
                  ;

                # Export 'Inf' and 'NaN' as constants
                no strict 'refs';

                my $inf_sub = $caller . '::' . 'Inf';
                if (!defined &$inf_sub) {
                    my $inf = inf();
                    *$inf_sub = sub () { $inf };
                }

                my $nan_sub = $caller . '::' . 'NaN';
                if (!defined &$nan_sub) {
                    my $nan = nan();
                    *$nan_sub = sub () { $nan };
                }
            }
            elsif (exists $constants{$name}) {
                no strict 'refs';
                my $caller_sub = $caller . '::' . $name;
                if (!defined &$caller_sub) {
                    my $sub   = $constants{$name};
                    my $value = Math::BigNum->$sub;
                    *$caller_sub = sub() { $value }
                }
            }
            else {
                require Carp;
                Carp::croak("unknown import: $name");
            }
        }
        return;
    }
}

# Converts a string representing a floating-point number into a rational representation
# Example: "1.234" is converted into "1234/1000"
# TODO: find a better solution (maybe)
# This solution is very slow for literals with absolute big exponents, such as: "1e-10000000"
sub _str2rat {
    my $str = lc($_[0] || "0");

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

        # Handle specially numbers with very big exponents
        # (it's not a very good solution, but I hope it's only temporary)
        if (CORE::abs($exp) >= 1000000) {
            my $mpfr = Math::MPFR::Rmpfr_init2($PREC);
            Math::MPFR::Rmpfr_set_str($mpfr, "$sign$str", 10, $ROUND);
            my $mpq = Math::GMPq::Rmpq_init();
            Math::MPFR::Rmpfr_get_q($mpq, $mpfr);
            return Math::GMPq::Rmpq_get_str($mpq, 10);
        }

        my ($before, $after) = split(/\./, substr($str, 0, $i));

        if (!defined($after)) {    # return faster for numbers like "13e2"
            if ($exp >= 0) {
                return ("$sign$before" . ('0' x $exp));
            }
            else {
                $after = '';
            }
        }

        my $numerator   = "$before$after";
        my $denominator = "1";

        if ($exp < 1) {
            $denominator .= '0' x (CORE::abs($exp) + length($after));
        }
        else {
            my $diff = ($exp - length($after));
            if ($diff >= 0) {
                $numerator .= '0' x $diff;
            }
            else {
                my $s = "$before$after";
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
        "$sign$str";
    }
}

# Converts a string into an mpfr object
sub _str2mpfr {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_set_str($r, $_[0], 10, $ROUND);
    $r;
}

# Converts a string into an mpq object
sub _str2mpq {
    my $r = Math::GMPq::Rmpq_init();

    if ((~$_[0] & $_[0]) eq '0' and CORE::int($_[0]) eq $_[0]) {
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

# Converts a string into an mpz object
sub _str2mpz {
    Math::GMPz::Rmpz_init_set_str($_[0], 10);
}

# Converts a BigNum object to mpfr
sub _big2mpfr {
    my $r = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_set_q($r, ${$_[0]}, $ROUND);
    $r;
}

# Converts a BigNum object to mpz
sub _big2mpz {
    my $i = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($i, ${$_[0]});
    $i;
}

# Converts an mpfr object to BigNum
sub _mpfr2big {

    if (Math::MPFR::Rmpfr_inf_p($_[0])) {
        if (Math::MPFR::Rmpfr_sgn($_[0]) > 0) {
            return inf();
        }
        else {
            return ninf();
        }
    }

    if (Math::MPFR::Rmpfr_nan_p($_[0])) {
        return nan();
    }

    my $r = Math::GMPq::Rmpq_init();
    Math::MPFR::Rmpfr_get_q($r, $_[0]);
    bless \$r, __PACKAGE__;
}

# Converts an mpfr object to mpq and puts it in $x
sub _mpfr2x {

    if (Math::MPFR::Rmpfr_inf_p($_[1])) {
        if (Math::MPFR::Rmpfr_sgn($_[1]) > 0) {
            return $_[0]->binf;
        }
        else {
            return $_[0]->bninf;
        }
    }

    if (Math::MPFR::Rmpfr_nan_p($_[1])) {
        return $_[0]->bnan;
    }

    Math::MPFR::Rmpfr_get_q(${$_[0]}, $_[1]);
    $_[0];
}

# Converts an mpz object to BigNum
sub _mpz2big {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_z($r, $_[0]);
    bless \$r, __PACKAGE__;
}

*_big2inf  = \&Math::BigNum::Inf::_big2inf;
*_big2ninf = \&Math::BigNum::Inf::_big2ninf;

#*_big2cplx = \&Math::BigNum::Complex::_big2cplx;

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

=head2 new

    BigNum->new(Scalar)            # => BigNum
    BigNum->new(Scalar, Scalar)    # => BigNum

Returns a new BigNum object with the value specified in the first argument,
which can be a Perl numerical value, a string representing a number in a
rational form, such as C<"1/2">, a string holding a floating-point number,
such as C<"0.5">, or a string holding an integer, such as C<"255">.

The second argument specifies the base of the number, which can range from 2
to 36 inclusive and defaults to 10.

For setting an hexadecimal number, we can say:

    my $x = Math::BigNum->new("deadbeef", 16);

B<NOTE:> no prefix, such as C<"0x"> or C<"0b">, is allowed as part of the number.

=cut

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
        my $rat = _str2rat($_[1]);
        Math::GMPq::Rmpq_set_str($r, $rat, 10);
        Math::GMPq::Rmpq_canonicalize($r) if (index($rat, '/') != -1);
    }
    else {
        Math::GMPq::Rmpq_set_str($r, $_[1], $_[2]);
        Math::GMPq::Rmpq_canonicalize($r) if (index($_[1], '/') != -1);
    }
    bless(\$r, $_[0]);
};

multimethod new => qw($ Math::BigNum) => sub {
    $_[1]->copy;
};

multimethod new => qw($ Math::BigNum::Inf) => sub {
    $_[1]->copy;
};

multimethod new => qw($ Math::BigNum::Nan) => sub {
    $_[1]->copy;
};

multimethod new => qw($ Math::BigInt) => sub {
    my ($x, $mb) = @_;

    if ($mb->is_nan) {
        return nan();
    }
    elsif ($mb->is_inf('-')) {
        return ninf();
    }
    elsif ($mb->is_inf('+')) {
        return inf();
    }

    my $value = $mb->bstr;
    my $r     = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_str($r, $value, 10);
    bless \$r, $_[0];
};

multimethod new => qw($ Math::BigFloat) => sub {
    my ($x, $mb) = @_;

    if ($mb->is_nan) {
        return nan();
    }
    elsif ($mb->is_inf('-')) {
        return ninf();
    }
    elsif ($mb->is_inf('+')) {
        return inf();
    }

    my $value = $mb->bstr;
    my $r     = Math::GMPq::Rmpq_init();
    my $rat   = _str2rat($value);
    Math::GMPq::Rmpq_set_str($r, $rat, 10);
    Math::GMPq::Rmpq_canonicalize($r) if (index($rat, '/') != -1);
    bless \$r, $_[0];
};

multimethod new => qw($ Math::BigRat) => sub {
    my ($x, $mb) = @_;

    if ($mb->is_nan) {
        return nan();
    }
    elsif ($mb->is_inf('-')) {
        return ninf();
    }
    elsif ($mb->is_inf('+')) {
        return inf();
    }

    my $value = $mb->bstr;
    my $r     = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_str($r, $value, 10);
    bless \$r, $_[0];
};

multimethod new => qw($ Math::GMPz) => sub {
    _mpz2big($_[1]);
};

multimethod new => qw($ Math::MPFR) => sub {
    _mpfr2big($_[1]);
};

multimethod new => qw($ Math::GMPq) => sub {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set($r, $_[1]);
    bless \$r, $_[0];
};

=head2 nan

    BigNum->nan                    # => Nan

Returns a new Nan object.

=cut

BEGIN { *nan = \&Math::BigNum::Nan::nan }

=head2 inf

    BigNum->inf                    # => Inf

Returns a new Inf object to represent positive Infinity.

=cut

BEGIN { *inf = \&Math::BigNum::Inf::inf }

=head2 ninf

    BigNum->ninf                   # => -Inf

Returns an Inf object to represent negative Infinity.

=cut

BEGIN { *ninf = \&Math::BigNum::Inf::ninf }

=head2 one

    BigNum->one                    # => BigNum

Returns a BigNum object containing the value C<1>.

=cut

sub one {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set($r, $ONE);
    bless \$r, __PACKAGE__;
}

=head2 zero

    BigNum->zero                   # => BigNum

Returns a BigNum object containing the value C<0>.

=cut

sub zero {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set($r, $ZERO);
    bless \$r, __PACKAGE__;
}

=head2 mone

    BigNum->mone                   # => BigNum

Returns a BigNum object containing the value C<-1>.

=cut

sub mone {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set($r, $MONE);
    bless \$r, __PACKAGE__;
}

=head2 stringify

    $x->stringify                  # => Scalar

Returns a string representing the value of C<$x>, either as an integer
or as a floating-point number. For C<$x=1/2>, it returns C<"0.5">.

=cut

sub stringify {
    my $v = Math::GMPq::Rmpq_get_str(${$_[0]}, 10);

    if (index($v, '/') != -1) {
        my ($x) = @_;
        $PREC = "$$PREC" if ref($PREC);    # make sure $PREC is not a BigNum

        my $prec = CORE::int($PREC / 4);        # the exact value should be 3.3...?
        my $sgn  = Math::GMPq::Rmpq_sgn($$x);

        my $n = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set($n, $$x);
        Math::GMPq::Rmpq_abs($n, $n) if $sgn < 0;

        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_ui_pow_ui($z, 10, CORE::abs($prec));

        my $p = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set_z($p, $z);

        if ($prec < 0) {
            Math::GMPq::Rmpq_div($n, $n, $p);
        }
        else {
            Math::GMPq::Rmpq_mul($n, $n, $p);
        }

        state $half = do {
            my $q = Math::GMPq::Rmpq_init();
            Math::GMPq::Rmpq_set_ui($q, 1, 2);
            $q;
        };

        Math::GMPq::Rmpq_add($n, $n, $half);
        Math::GMPz::Rmpz_set_q($z, $n);

        # Too much rounding... Give up and return an MPFR stringified number.
        Math::GMPz::Rmpz_sgn($z) || do {
            my $mpfr = Math::MPFR::Rmpfr_init2($PREC);
            Math::MPFR::Rmpfr_set_q($mpfr, $$x, $ROUND);
            return Math::MPFR::Rmpfr_get_str($mpfr, 10, $prec, $ROUND);
        };

        if (Math::GMPz::Rmpz_odd_p($z) and Math::GMPq::Rmpq_integer_p($n)) {
            Math::GMPz::Rmpz_sub_ui($z, $z, 1);
        }

        Math::GMPq::Rmpq_set_z($n, $z);

        if ($prec < 0) {
            Math::GMPq::Rmpq_mul($n, $n, $p);
        }
        else {
            Math::GMPq::Rmpq_div($n, $n, $p);
        }

        my $num = Math::GMPz::Rmpz_init();
        my $den = Math::GMPz::Rmpz_init();

        Math::GMPq::Rmpq_get_num($num, $n);
        Math::GMPq::Rmpq_get_den($den, $n);

        my @r;
        my $c = 0;

        while (1) {

            Math::GMPz::Rmpz_div($z, $num, $den);
            push @r, Math::GMPz::Rmpz_get_str($z, 10);

            Math::GMPz::Rmpz_mul($z, $z, $den);
            last if Math::GMPz::Rmpz_divisible_p($num, $den);
            Math::GMPz::Rmpz_sub($num, $num, $z);

            my $s = -1;
            while (Math::GMPz::Rmpz_cmp($den, $num) > 0) {
                last if !Math::GMPz::Rmpz_sgn($num);
                Math::GMPz::Rmpz_mul_ui($num, $num, 10);
                ++$s;
            }

            push(@r, '0' x $s) if ($s > 0);
        }

        my $before = shift(@r);      # before the decimal point
        my $after = join('', @r);    # after the decimal point
        $after =~ s/0+$//;           # remove trailing zeros

        # Maybe we should return "$before.0" when $after eq ''?
        ($sgn < 0 ? "-" : '') . ($after eq '' ? $before : "$before.$after");
    }
    else {
        $v;
    }
}

=head2 numify

    $x->numify                     # => Scalar

Returns a Perl numerical scalar with the value of C<$x>, truncated if needed.

=cut

sub numify {
    Math::GMPq::Rmpq_get_d(${$_[0]});
}

=head2 boolify

    $x->boolify                    # => Bool

Returns a true value when the number is not zero. False otherwise.

=cut

sub boolify {
    !!Math::GMPq::Rmpq_sgn(${$_[0]});
}

=head2 copy

    $x->copy                       # => BigNum

Returns a deep copy of C<$x>.

=cut

sub copy {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set($r, ${$_[0]});
    bless \$r, ref($_[0]);
}

#
## Constants
#

=head2 pi

    BigNum->pi                     # => BigNum

Returns the number PI, which is C<3.1415...>.

=cut

sub pi {
    my $pi = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_pi($pi, $ROUND);
    _mpfr2big($pi);
}

=head2 tau

    BigNum->tau                    # => BigNum

Returns the number TAU, which is C<2*PI>.

=cut

sub tau {
    my $tau = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_pi($tau, $ROUND);
    Math::MPFR::Rmpfr_mul_ui($tau, $tau, 2, $ROUND);
    _mpfr2big($tau);
}

=head2 ln2

    BigNum->ln2                    # => BigNum

Returns the natural logarithm of C<2>.

=cut

sub ln2 {
    my $ln2 = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_log2($ln2, $ROUND);
    _mpfr2big($ln2);
}

=head2 Y

    BigNum->Y                      # => BigNum

Returns the Euler-Mascheroni constant, which is C<0.57721...>.

=cut

sub Y {
    my $euler = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_euler($euler, $ROUND);
    _mpfr2big($euler);
}

=head2 G

    BigNum->G                      # => BigNum

Returns the value of Catalan's constant, also known
as Beta(2) or G, and starts as: C<0.91596...>.

=cut

sub G {
    my $catalan = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_const_catalan($catalan, $ROUND);
    _mpfr2big($catalan);
}

=head2 e

    BigNum->e                      # => BigNum

Returns the e mathematical constant, which is C<2.718...>.

=cut

sub e {
    state $one_f = (Math::MPFR::Rmpfr_init_set_ui(1, $ROUND))[0];
    my $e = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_exp($e, $one_f, $ROUND);
    _mpfr2big($e);
}

=head2 phi

    BigNum->phi                    # => BigNum

Returns the value of the golden ratio, which is C<1.61803...>.

=cut

sub phi {
    state $one_f  = (Math::MPFR::Rmpfr_init_set_ui(1, $ROUND))[0];
    state $two_f  = (Math::MPFR::Rmpfr_init_set_ui(2, $ROUND))[0];
    state $five_f = (Math::MPFR::Rmpfr_init_set_ui(5, $ROUND))[0];

    my $phi = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_sqrt($phi, $five_f, $ROUND);
    Math::MPFR::Rmpfr_add($phi, $phi, $one_f, $ROUND);
    Math::MPFR::Rmpfr_div($phi, $phi, $two_f, $ROUND);

    _mpfr2big($phi);
}

#
## Special methods
#

=head2 bzero

    $x->bzero                      # => BigNum

Changes C<$x> in-place to hold the value 0.

=cut

sub bzero {
    my ($x) = @_;
    Math::GMPq::Rmpq_set($$x, $ZERO);
    if (ref($x) ne __PACKAGE__) {
        bless $x, __PACKAGE__;
    }
    $x;
}

=head2 bone

    $x->bone                       # => BigNum

Changes C<$x> in-place to hold the value +1.

=cut

sub bone {
    my ($x) = @_;
    Math::GMPq::Rmpq_set($$x, $ONE);
    if (ref($x) ne __PACKAGE__) {
        bless $x, __PACKAGE__;
    }
    $x;
}

=head2 bmone

    $x->bmone                      # => BigNum

Changes C<$x> in-place to hold the value -1.

=cut

sub bmone {
    my ($x) = @_;
    Math::GMPq::Rmpq_set($$x, $MONE);
    if (ref($x) ne __PACKAGE__) {
        bless $x, __PACKAGE__;
    }
    $x;
}

=head2 binf

    $x->binf                       # => Inf

Changes C<$x> in-place to positive Infinity.

=cut

*binf = \&Math::BigNum::Inf::binf;

=head2 bninf

    $x->bninf                      # => -Inf

Changes C<$x> in-place to negative Infinity.

=cut

*bninf = \&Math::BigNum::Inf::bninf;

=head2 bnan

    $x->bnan                       # => Nan

Changes C<$x> in-place to the special Not-A-Number value.

=cut

*bnan = \&Math::BigNum::Nan::bnan;

#
## Arithmetic
#

=head2 add

    $x->add(BigNum)                # => BigNum
    $x->add(Scalar)                # => BigNum

    BigNum + BigNum                # => BigNum
    BigNum + Scalar                # => BigNum
    Scalar + BigNum                # => BigNum

Adds C<$y> to C<$x> and returns the result.

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

=for comment
multimethod add => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->add($_[1]);
};
=cut

multimethod add => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->copy };
multimethod add => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 badd

    $x->badd(BigNum)               # => BigNum
    $x->badd(Scalar)               # => BigNum

    BigNum += BigNum               # => BigNum
    BigNum += Scalar               # => BigNum

Adds C<$y> to C<$x>, changing C<$x> in-place.

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

multimethod badd => qw(Math::BigNum Math::BigNum::Inf) => \&_big2inf;
multimethod badd => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 iadd

    $x->iadd(BigNum)               # => BigNum
    $x->iadd(Scalar)               # => BigNum

Integer addition of C<$y> to C<$x>. Both values
are truncated to integers before addition.

=cut

multimethod iadd => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_add($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

multimethod iadd => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $y = CORE::int($_[1]);
    $y < 0
      ? Math::GMPz::Rmpz_sub_ui($r, $r, CORE::abs($y))
      : Math::GMPz::Rmpz_add_ui($r, $r, $y);
    _mpz2big($r);
};

multimethod iadd => qw(Math::BigNum Math::BigNum::Inf) => \&_big2inf;
multimethod iadd => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 biadd

    $x->biadd(BigNum)              # => BigNum
    $x->biadd(Scalar)              # => BigNum

Integer addition of C<$y> from C<$x>, changing C<$x> in-place.
Both values are truncated to integers before addition.

=cut

multimethod biadd => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_add($r, $r, _big2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod biadd => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $y = CORE::int($_[1]);
    $y < 0
      ? Math::GMPz::Rmpz_sub_ui($r, $r, CORE::abs($y))
      : Math::GMPz::Rmpz_add_ui($r, $r, $y);
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod biadd => qw(Math::BigNum Math::BigNum::Inf) => \&_big2inf;
multimethod biadd => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 sub

    $x->sub(BigNum)                # => BigNum
    $x->sub(Scalar)                # => BigNum

    BigNum - BigNum                # => BigNum
    BigNum - Scalar                # => BigNum
    Scalar - BigNum                # => BigNum

Subtracts C<$y> from C<$x> and returns the result.

=cut

multimethod sub => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_sub($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod sub => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    my $r = _str2mpq($y);
    Math::GMPq::Rmpq_sub($r, $$x, $r);
    bless \$r, __PACKAGE__;
};

multimethod sub => qw($ Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = _str2mpq($x);
    Math::GMPq::Rmpq_sub($r, $r, $$y);
    bless \$r, __PACKAGE__;
};

=for comment
multimethod sub => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->sub($_[1]);
};
=cut

multimethod sub => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->neg };
multimethod sub => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bsub

    $x->bsub(BigNum)               # => BigNum
    $x->bsub(Scalar)               # => BigNum

    BigNum -= BigNum               # => BigNum
    BigNum -= Scalar               # => BigNum

Subtracts C<$y> from C<$x> by changing C<$x> in-place.

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

multimethod bsub => qw(Math::BigNum Math::BigNum::Inf) => \&_big2ninf;
multimethod bsub => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 isub

    $x->isub(BigNum)               # => BigNum
    $x->isub(Scalar)               # => BigNum

Integer subtraction of C<$y> from C<$x>. Both values
are truncated to integers before subtraction.

=cut

multimethod isub => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_sub($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

multimethod isub => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $y = CORE::int($_[1]);
    $y < 0
      ? Math::GMPz::Rmpz_add_ui($r, $r, CORE::abs($y))
      : Math::GMPz::Rmpz_sub_ui($r, $r, $y);
    _mpz2big($r);
};

multimethod isub => qw($ Math::BigNum) => sub {
    my $r = _str2mpz($_[0]);
    Math::GMPz::Rmpz_sub($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

multimethod isub => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->neg };
multimethod isub => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bisub

    $x->bisub(BigNum)              # => BigNum
    $x->bisub(Scalar)              # => BigNum

Integer subtraction of C<$y> from $x, changing C<$x> in-place.
Both values are truncated to integers before subtraction.

=cut

multimethod bisub => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_sub($r, $r, _big2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod bisub => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $y = CORE::int($_[1]);
    $y < 0
      ? Math::GMPz::Rmpz_add_ui($r, $r, CORE::abs($y))
      : Math::GMPz::Rmpz_sub_ui($r, $r, $y);
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod bisub => qw(Math::BigNum Math::BigNum::Inf) => \&_big2ninf;
multimethod bisub => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 mul

    $x->mul(BigNum)                # => BigNum
    $x->mul(Scalar)                # => BigNum

    BigNum * BigNum                # => BigNum
    BigNum * Scalar                # => BigNum
    Scalar * BigNum                # => BigNum

Multiplies C<$x> by C<$y> and returns the result.

=cut

multimethod mul => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_mul($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod mul => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    my $r = _str2mpq($y);
    Math::GMPq::Rmpq_mul($r, $$x, $r);
    bless \$r, __PACKAGE__;
};

=for comment
multimethod mul => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->mul($_[0]);
};
=cut

multimethod mul => qw(Math::BigNum Math::BigNum::Inf) => sub {
    my $sign = Math::GMPq::Rmpq_sgn(${$_[0]});
    $sign < 0 ? ninf : $sign > 0 ? inf : nan;
};

multimethod mul => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bmul

    $x->bmul(BigNum)               # => BigNum
    $x->bmul(Scalar)               # => BigNum

    BigNum *= BigNum               # => BigNum
    BigNum *= Scalar               # => BigNum

Multiply C<$x> by C<$y>, changing C<$x> in-place.

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

        $sign < 0 ? _big2ninf(@_)
      : $sign > 0 ? _big2inf(@_)
      :             $x->bnan;
};

multimethod bmul => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 imul

    $x->imul(BigNum)               # => BigNum
    $x->imul(Scalar)               # => BigNum

Integer multiplication of C<$x> by C<$y>. Both values
are truncated to integers before multiplication.

=cut

multimethod imul => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_mul($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

multimethod imul => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $y = CORE::int($_[1]);
    $y < 0
      ? Math::GMPz::Rmpz_mul_si($r, $r, $y)
      : Math::GMPz::Rmpz_mul_ui($r, $r, $y);
    _mpz2big($r);
};

multimethod imul => qw(Math::BigNum Math::BigNum::Inf) => sub {
    my $sign = Math::GMPq::Rmpq_sgn(${$_[0]});
    $sign < 0 ? $_[1]->neg : $sign > 0 ? $_[1]->copy : nan;
};

multimethod imul => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bimul

    $x->bimul(BigNum)              # => BigNum
    $x->bimul(Scalar)              # => BigNum

Integer multiplication of C<$x> by C<$y>, changing C<$x> in-place.
Both values are truncated to integers before multiplication.

=cut

multimethod bimul => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_mul($r, $r, _big2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod bimul => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $y = CORE::int($_[1]);
    $y < 0
      ? Math::GMPz::Rmpz_mul_si($r, $r, $y)
      : Math::GMPz::Rmpz_mul_ui($r, $r, $y);
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod bimul => qw(Math::BigNum Math::BigNum::Inf) => sub {
    my ($x) = @_;
    my $sign = Math::GMPq::Rmpq_sgn($$x);
    $sign < 0 ? _big2ninf(@_) : $sign > 0 ? _big2inf(@_) : $x->bnan;
};

multimethod bimul => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 div

    $x->div(BigNum)                # => BigNum | Inf | Nan
    $x->div(Scalar)                # => BigNum | Inf | Nan

    BigNum / BigNum                # => BigNum | Inf | Nan
    BigNum / Scalar                # => BigNum | Inf | Nan
    Scalar / BigNum                # => BigNum | Inf | Nan

Divides C<$x> by C<$y> and returns the result. Returns Nan when C<$x> and C<$y> are 0,
Inf when C<$y> is $zero and C<$x> is positive, -Inf when C<$y> is zero and C<$x> is negative.

=cut

multimethod div => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    Math::GMPq::Rmpq_sgn($$y) || do {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? nan : $sign > 0 ? inf : ninf);
    };

    # Unsure optimization: set the numerator and denominator manually for integers.
    # Doing this, we get about the same performance as we would do integer division,
    # and this is because we prevent the expensive mpq_canonicalize() to get called,
    # but this may have some other, nasty consequences. So far, I haven't found any.
    # See also `Rational Arithmetic` on: https://gmplib.org/manual/Efficiency.html

    #~ if (Math::GMPq::Rmpq_integer_p($$x) and Math::GMPq::Rmpq_integer_p($$y)) {
    #~      my $num_z = Math::GMPz::Rmpz_init();
    #~      my $den_z = Math::GMPz::Rmpz_init();

    #~      Math::GMPq::Rmpq_numref($num_z, $$x);
    #~      Math::GMPq::Rmpq_numref($den_z, $$y);

    #~      my $r = Math::GMPq::Rmpq_init();
    #~      Math::GMPq::Rmpq_set_num($r, $num_z);
    #~      Math::GMPq::Rmpq_set_den($r, $den_z);

    #~      return bless \$r, __PACKAGE__;
    #~ }

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_div($r, $$x, $$y);
    bless \$r, __PACKAGE__;
};

multimethod div => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    $y || do {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? nan : $sign > 0 ? inf : ninf);
    };

    my $r = _str2mpq($y);
    Math::GMPq::Rmpq_div($r, $$x, $r);
    bless \$r, __PACKAGE__;
};

multimethod div => qw($ Math::BigNum) => sub {
    my ($x, $y) = @_;

    Math::GMPq::Rmpq_sgn($$y)
      || return (!$x ? nan : $x > 0 ? inf : ninf);

    my $r = _str2mpq($x);
    Math::GMPq::Rmpq_div($r, $r, $$y);
    bless \$r, __PACKAGE__;
};

=for comment
multimethod div => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->div($_[1]);
};
=cut

multimethod div => qw(Math::BigNum Math::BigNum::Inf) => \&zero;
multimethod div => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bdiv

    $x->bdiv(BigNum)               # => BigNum | Nan | Inf
    $x->bdiv(Scalar)               # => BigNum | Nan | Inf

    BigNum /= BigNum               # => BigNum | Nan | Inf
    BigNum /= Scalar               # => BigNum | Nan | Inf

Divide C<$x> by C<$y>, changing C<$x> in-place. The return values are the same as for C<div()>.

=cut

multimethod bdiv => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    Math::GMPq::Rmpq_sgn($$y) || do {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return
            $sign > 0 ? $x->binf
          : $sign < 0 ? $x->bninf
          :             $x->bnan;
    };

    Math::GMPq::Rmpq_div($$x, $$x, $$y);
    $x;
};

multimethod bdiv => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    $y || do {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return
            $sign > 0 ? $x->binf
          : $sign < 0 ? $x->bninf
          :             $x->bnan;
    };

    Math::GMPq::Rmpq_div($$x, $$x, _str2mpq($y));
    $x;
};

multimethod bdiv => qw(Math::BigNum Math::BigNum::Inf) => \&bzero;
multimethod bdiv => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 idiv

    $x->idiv(BigNum)               # => BigNum | Nan | Inf
    $x->idiv(Scalar)               # => BigNum | Nan | Inf

Integer division of C<$x> by C<$y>.

=cut

multimethod idiv => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (!Math::GMPq::Rmpq_sgn($$y)) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? nan : $sign > 0 ? inf : ninf);
    }

    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_div($r, $r, _big2mpz($y));
    _mpz2big($r);
};

multimethod idiv => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if (!$y) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return (!$sign ? nan : $sign > 0 ? inf : ninf);
    }

    if (CORE::int($y) eq $y and $y >= 0) {
        my $r = _big2mpz($x);
        Math::GMPz::Rmpz_div_ui($r, $r, $y);
        return _mpz2big($r);
    }

    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_div($r, $r, _str2mpz($y));
    _mpz2big($r);
};

multimethod idiv => qw($ Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (!Math::GMPq::Rmpq_sgn($$y)) {
        return ($x == 0 ? nan : $x > 0 ? inf : ninf);
    }

    my $r = _str2mpz($x);
    Math::GMPz::Rmpz_div($r, $r, _big2mpz($y));
    _mpz2big($r);
};

multimethod idiv => qw(Math::BigNum Math::BigNum::Inf) => \&zero;
multimethod idiv => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bidiv

    $x->bidiv(BigNum)              # => BigNum | Nan | Inf
    $x->bidiv(Scalar)              # => BigNum | Nan | Inf

Integer division of C<$x> by C<$y>, changing C<$x> in-place.

=cut

multimethod bidiv => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (!Math::GMPq::Rmpq_sgn($$y)) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return
            $sign > 0 ? $x->binf
          : $sign < 0 ? $x->bninf
          :             $x->bnan;
    }

    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_div($r, $r, _big2mpz($y));
    Math::GMPq::Rmpq_set_z($$x, $r);
    $x;
};

multimethod bidiv => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if (!$y) {
        my $sign = Math::GMPq::Rmpq_sgn($$x);
        return
            $sign > 0 ? $x->binf
          : $sign < 0 ? $x->bninf
          :             $x->bnan;
    }

    if (CORE::int($y) eq $y and $y >= 0) {
        my $r = _big2mpz($x);
        Math::GMPz::Rmpz_div_ui($r, $r, $y);
        Math::GMPq::Rmpq_set_z($$x, $r);
        return $x;
    }

    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_div($r, $r, _str2mpz($y));
    Math::GMPq::Rmpq_set_z($$x, $r);
    $x;
};

multimethod bidiv => qw(Math::BigNum Math::BigNum::Inf) => \&bzero;
multimethod bidiv => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 neg

    $x->neg                        # => BigNum
    -$x                            # => BigNum

Negative value of C<$x>. Returns C<abs($x)> when C<$x> is negative, and C<-$x> when C<$x> is positive.

=cut

sub neg {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_neg($r, $$x);
    bless \$r, __PACKAGE__;
}

=head2 bneg

    $x->bneg                       # => BigNum

Negative value of C<$x>, changing C<$x> in-place.

=cut

sub bneg {
    my ($x) = @_;
    Math::GMPq::Rmpq_neg($$x, $$x);
    $x;
}

=head2 abs

    $x->abs                        # => BigNum
    abs($x)                        # => BigNum

Absolute value of C<$x>.

=cut

sub abs {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_abs($r, $$x);
    bless \$r, __PACKAGE__;
}

=head2 babs

    $x->babs                       # => BigNum

Absolute value of C<$x>, changing C<$x> in-place.

=cut

sub babs {
    my ($x) = @_;
    Math::GMPq::Rmpq_abs($$x, $$x);
    $x;
}

=head2 inv

    $x->inv                        # => BigNum | Inf

Inverse value of C<$x>. Return Inf when C<$x> is zero. (C<1/$x>)

=cut

sub inv {
    my ($x) = @_;

    # Return Inf when $x is zero.
    if (!Math::GMPq::Rmpq_sgn($$x)) {
        return inf;
    }

    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_inv($r, $$x);
    bless \$r, __PACKAGE__;
}

=head2 sqr

    $x->sqr                        # => BigNum

Raise C<$x> to the power of 2 and return the result. (C<$x**2>)

=cut

sub sqr {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_mul($r, $$x, $$x);
    bless \$r, __PACKAGE__;
}

=head2 sqrt

    $x->sqrt                       # => BigNum | Nan
    sqrt($x)                       # => BigNum | Nan

Square root of C<$x>. Returns Nan when C<$x> is negative.

=cut

sub sqrt {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_sqrt($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 bsqrt

    $x->bsqrt                      # => BigNum | Nan

Square root of C<$x>, changing C<$x> in-place. Promotes C<$x> to Nan when C<$x> is negative.

=cut

sub bsqrt {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_sqrt($r, $r, $ROUND);
    _mpfr2x($x, $r);
}

=head2 isqrt

    $x->isqrt                      # => BigNum | Nan

Integer square root of C<$x>. Returns Nan when C<$x> is negative.

=cut

sub isqrt {
    my $r = _big2mpz($_[0]);
    return nan() if Math::GMPz::Rmpz_sgn($r) < 0;
    Math::GMPz::Rmpz_sqrt($r, $r);
    _mpz2big($r);
}

=head2 bisqrt

    $x->bisqrt                     # => BigNum | Nan

Integer square root of C<$x>, changing C<$x> in-place. Promotes C<$x> to Nan when C<$x> is negative.

=cut

sub bisqrt {
    my ($x) = @_;
    my $r = _big2mpz($x);
    return $x->bnan() if Math::GMPz::Rmpz_sgn($r) < 0;
    Math::GMPz::Rmpz_sqrt($r, $r);
    Math::GMPq::Rmpq_set_z($$x, $r);
    $x;
}

=head2 cbrt

    $x->cbrt                       # => BigNum | Nan

Cube root of C<$x>. Returns Nan when C<$x> is negative.

=cut

sub cbrt {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_cbrt($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 root

    $x->root(BigNum)               # => BigNum | Nan
    $x->root(Scalar)               # => BigNum | Nan

Nth root of C<$x>. Returns Nan when C<$x> is negative.

=cut

multimethod root => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->pow($_[1]->inv);
};

=for comment
multimethod root => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->pow($_[1]->inv);
};
=cut

multimethod root => qw(Math::BigNum $) => sub {
    $_[0]->pow(Math::BigNum->new($_[1])->inv);
};

multimethod root => qw(Math::BigNum Math::BigNum::Inf) => \&one;
multimethod root => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 broot

    $x->broot(BigNum)              # => BigNum | Nan
    $x->broot(Scalar)              # => BigNum(1)

Nth root of C<$x>, changing C<$x> in-place. Promotes
C<$x> to Nan when C<$x> is negative.

=cut

multimethod broot => qw(Math::BigNum $) => sub {
    $_[0]->bpow(Math::BigNum->new($_[1])->inv);
};

multimethod broot => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->bpow($_[1]->inv);
};

=for comment
multimethod broot => qw(Math::BigNum Math::BigNum::Complex) => sub {
    my $complex = Math::BigNum::Complex->new($_[0])->bpow($_[1]->inv);
    _big2cplx($_[0], $complex);
};
=cut

multimethod broot => qw(Math::BigNum Math::BigNum::Inf) => \&bone;
multimethod broot => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 iroot

    $x->iroot(BigNum)              # => BigNum | Nan
    $x->iroot(Scalar)              # => BigNum | Nan

Nth integer root of C<$x> (C<$x**(1/$n)>). Returns
Nan when C<$x> is negative and C<$y> is even.

=cut

multimethod iroot => qw(Math::BigNum $) => sub {
    $_[0]->copy->biroot($_[1]);
};

multimethod iroot => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->copy->biroot(CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]})));
};

multimethod iroot => qw(Math::BigNum Math::BigNum::Inf) => \&one;
multimethod iroot => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 biroot

    $x->biroot(BigNum)             # => BigNum | Nan
    $x->biroot(Scalar)             # => BigNum | Nan

Nth integer root of C<$x>, changing C<$x> in-place. Promotes
C<$x> to Nan when C<$x> is negative and C<$y> is even.

=cut

multimethod biroot => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $z    = _big2mpz($x);
    my $root = CORE::int($y);

    if ($root % 2 == 0 and Math::GMPz::Rmpz_sgn($z) < 0) {
        return $x->bnan;
    }

    Math::GMPz::Rmpz_root($z, $z, $root);
    Math::GMPq::Rmpq_set_z($$x, $z);
    $x;
};

multimethod biroot => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->biroot(CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]})));
};

=head2 pow

    $x->pow(BigNum)                # => BigNum | Nan
    $x->pow(Scalar)                # => BigNum | Nan

    BigNum ** BigNum               # => BigNum | Nan
    BigNum ** Scalar               # => BigNum | Nan
    Scalar ** BigNum               # => BigNum | Nan

Raises C<$x> to power C<$y>. Returns Nan when C<$x> is negative
and C<$y> is not an integer.

=cut

multimethod pow => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    # Do integer exponentiation when both are integers
    if (Math::GMPq::Rmpq_integer_p($$x) and Math::GMPq::Rmpq_integer_p($$y)) {

        my $pow = Math::GMPq::Rmpq_get_d($$y);

        my $z = _big2mpz($x);
        Math::GMPz::Rmpz_pow_ui($z, $z, CORE::abs($pow));

        my $q = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set_z($q, $z);

        if ($pow < 0) {
            if (!Math::GMPq::Rmpq_sgn($q)) {
                return inf();
            }
            Math::GMPq::Rmpq_inv($q, $q);
        }

        return bless \$q, __PACKAGE__;
    }

    # Floating-point exponentiation otherwise
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_pow($r, $r, _big2mpfr($y), $ROUND);
    _mpfr2big($r);
};

=for comment
multimethod pow => qw(Math::BigNum Math::BigNum::Complex) => sub {
    Math::BigNum::Complex->new($_[0])->pow($_[1]);
};
=cut

multimethod pow => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    # Optimization for when both are integers
    if (CORE::int($y) eq $y and Math::GMPq::Rmpq_integer_p($$x)) {
        my $z = _big2mpz($x);
        Math::GMPz::Rmpz_pow_ui($z, $z, CORE::abs($y));
        my $q = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set_z($q, $z);

        if ($y < 0) {
            if (!Math::GMPq::Rmpq_sgn($q)) {
                return inf();
            }
            Math::GMPq::Rmpq_inv($q, $q);
        }

        return bless \$q, __PACKAGE__;
    }
    else {
        $x->pow(Math::BigNum->new($y));
    }
};

# This will happen rarely, so no special optimization.
multimethod pow => qw($ Math::BigNum) => sub {
    Math::BigNum->new($_[0])->bpow($_[1]);
};

# (+/-1) ** (+/-Inf) = 1
# x ** (-Inf) = 0
# x ** Inf = Inf

multimethod pow => qw(Math::BigNum Math::BigNum::Inf) => sub {
        $_[0]->is_one || $_[0]->is_mone ? one()
      : $_[1]->is_neg ? zero
      :                 inf;
};

multimethod pow => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bpow

    $x->bpow(BigNum)               # => BigNum | Nan
    $x->bpow(Scalar)               # => BigNum | Nan

Raises C<$x> to power C<$y>, changing C<$x> in-place.

=cut

multimethod bpow => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    # Both are integers
    if (Math::GMPq::Rmpq_integer_p($$x) and Math::GMPq::Rmpq_integer_p($$y)) {

        my $pow = Math::GMPq::Rmpq_get_d($$y);

        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_pow_ui($z, $z, CORE::abs($pow));
        Math::GMPq::Rmpq_set_z($$x, $z);

        if ($pow < 0) {
            if (!Math::GMPq::Rmpq_sgn($$x)) {
                return $x->binf;
            }
            Math::GMPq::Rmpq_inv($$x, $$x);
        }

        return $x;
    }

    # A floating-point otherwise
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_pow($r, $r, _big2mpfr($y), $ROUND);
    _mpfr2x($x, $r);
};

multimethod bpow => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $y_is_int = CORE::int($y) eq $y;

    # Both are integers
    if ($y_is_int and Math::GMPq::Rmpq_integer_p($$x)) {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_pow_ui($z, $z, CORE::abs($y));
        Math::GMPq::Rmpq_set_z($$x, $z);

        if ($y < 0) {
            if (!Math::GMPq::Rmpq_sgn($$x)) {
                return $x->binf;
            }
            Math::GMPq::Rmpq_inv($$x, $$x);
        }

        return $x;
    }

    # A floating-point otherwise
    my $r = _big2mpfr($x);
    if ($y_is_int) {
        if ($y >= 0) {
            Math::MPFR::Rmpfr_pow_ui($r, $r, $y, $ROUND);
        }
        else {
            Math::MPFR::Rmpfr_pow_si($r, $r, $y, $ROUND);
        }
    }
    else {
        Math::MPFR::Rmpfr_pow($r, $r, _str2mpfr($y), $ROUND);
    }

    _mpfr2x($x, $r);
};

multimethod bpow => qw(Math::BigNum Math::BigNum::Inf) => sub {
        $_[0]->is_one || $_[0]->is_mone ? $_[0]->bone()
      : $_[1]->is_neg ? $_[0]->bzero
      :                 $_[0]->binf;
};

multimethod bpow => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 ipow

    $x->ipow(BigNum)               # => BigNum
    $x->ipow(Scalar)               # => BigNum

Raises C<$x> to power C<$y>, truncating C<$x> and C<$y> to integers, if necessarily.

=cut

multimethod ipow => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $pow = CORE::int(Math::GMPq::Rmpq_get_d($$y));

    my $z = _big2mpz($x);
    Math::GMPz::Rmpz_pow_ui($z, $z, CORE::abs($pow));

    if ($pow < 0) {
        return inf() if !Math::GMPz::Rmpz_sgn($z);
        Math::GMPz::Rmpz_div($z, $ONE_Z, $z);
    }

    _mpz2big($z);
};

multimethod ipow => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    $y = CORE::int($y);

    my $z = _big2mpz($x);
    Math::GMPz::Rmpz_pow_ui($z, $z, CORE::abs($y));

    if ($y < 0) {
        return inf() if !Math::GMPz::Rmpz_sgn($z);
        Math::GMPz::Rmpz_div($z, $ONE_Z, $z);
    }

    _mpz2big($z);
};

# This will happen rarely, so no special optimization.
multimethod ipow => qw($ Math::BigNum) => sub {
    Math::BigNum->new($_[0])->bipow($_[1]);
};

multimethod ipow => qw(Math::BigNum Math::BigNum::Inf) => sub {
    $_[0]->int->pow($_[1]);
};

multimethod ipow => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bipow

    $x->bipow(BigNum)              # => BigNum
    $x->bipow(Scalar)              # => BigNum

Raises C<$x> to power C<$y>, changing C<$x> in-place.

=cut

multimethod bipow => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $pow = CORE::int(Math::GMPq::Rmpq_get_d($$y));

    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, $$x);
    Math::GMPz::Rmpz_pow_ui($z, $z, CORE::abs($pow));
    if ($pow < 0) {
        return $x->binf if !Math::GMPz::Rmpz_sgn($z);
        Math::GMPz::Rmpz_div($z, $ONE_Z, $z);
    }
    Math::GMPq::Rmpq_set_z($$x, $z);
    return $x;
};

multimethod bipow => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    $y = CORE::int($y);

    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, $$x);
    Math::GMPz::Rmpz_pow_ui($z, $z, CORE::abs($y));
    if ($y < 0) {
        return $x->binf if !Math::GMPz::Rmpz_sgn($z);
        Math::GMPz::Rmpz_div($z, $ONE_Z, $z);
    }
    Math::GMPq::Rmpq_set_z($$x, $z);
    return $x;
};

multimethod bipow => qw(Math::BigNum Math::BigNum::Inf) => sub {
    $_[0]->bint->bpow($_[1]);
};

multimethod bipow => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 ln

    $x->ln                         # => BigNum | Nan

Logarithm of C<$x> in base e. Returns Nan when C<$x> is negative.

=cut

sub ln {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_log($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 log

    $x->log                        # => BigNum | Nan
    $x->log(BigNum)                # => BigNum | Nan
    $x->log(Scalar)                # => BigNum | Nan
    log(BigNum)                    # => BigNum | Nan

Logarithm of C<$x> in base C<$y>. When C<$y> is not specified, it defaults to base e.
Returns Nan when C<$x> is negative and -Inf when C<$x> is zero.

=cut

multimethod log => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    # log(x,base) = log(x)/log(base)
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_log($r, $r, $ROUND);
    my $baseln = _big2mpfr($y);
    Math::MPFR::Rmpfr_log($baseln, $baseln, $ROUND);
    Math::MPFR::Rmpfr_div($r, $r, $baseln, $ROUND);

    _mpfr2big($r);
};

multimethod log => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $r = _big2mpfr($x);

    if ($y == 2) {
        Math::MPFR::Rmpfr_log2($r, $r, $ROUND);
    }
    elsif ($y == 10) {
        Math::MPFR::Rmpfr_log10($r, $r, $ROUND);
    }
    else {
        Math::MPFR::Rmpfr_log($r, $r, $ROUND);
        my $baseln = _str2mpfr($y);
        Math::MPFR::Rmpfr_log($baseln, $baseln, $ROUND);
        Math::MPFR::Rmpfr_div($r, $r, $baseln, $ROUND);
    }

    _mpfr2big($r);
};

multimethod log => qw(Math::BigNum) => \&ln;

=head2 blog

    $x->blog                       # => BigNum | Nan
    $x->blog(BigNum)               # => BigNum | Nan
    $x->log(Scalar)                # => BigNum | Nan

Logarithm of C<$x> in base C<$y>, changing the C<$x> in-place.
When C<$y> is not specified, it defaults to base e.

=cut

multimethod blog => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $r = _big2mpfr($x);

    if ($y == 2) {
        Math::MPFR::Rmpfr_log2($r, $r, $ROUND);
    }
    elsif ($y == 10) {
        Math::MPFR::Rmpfr_log10($r, $r, $ROUND);
    }
    else {
        Math::MPFR::Rmpfr_log($r, $r, $ROUND);
        my $baseln = _str2mpfr($y);
        Math::MPFR::Rmpfr_log($baseln, $baseln, $ROUND);
        Math::MPFR::Rmpfr_div($r, $r, $baseln, $ROUND);
    }

    _mpfr2x($x, $r);
};

multimethod blog => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->blog(Math::GMPq::Rmpq_get_d(${$_[1]}));
};

multimethod blog => qw(Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_log($r, $r, $ROUND);
    _mpfr2x($x, $r);
};

=head2 log2

    $x->log2                       # => BigNum | Nan

Logarithm of C<$x> in base 2. Returns Nan when C<$x> is negative.

=cut

sub log2 {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_log2($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 log10

    $x->log10                      # => BigNum | Nan

Logarithm of C<$x> in base 10. Returns Nan when C<$x> is negative.

=cut

sub log10 {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_log10($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 exp

    $x->exp                        # => BigNum

Exponential of C<$x> in base e. (C<e**$x>)

=cut

sub exp {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_exp($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 bexp

    $x->bexp                       # => BigNum

Exponential of C<$x> in base e, changing C<$x> in-place.

=cut

sub bexp {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_exp($r, $r, $ROUND);
    Math::MPFR::Rmpfr_get_q($$x, $r);
    $x;
}

=head2 exp2

    $x->exp2                       # => BigNum

Exponential of C<$x> in base 2. (C<2**$x>)

=cut

sub exp2 {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_exp2($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 exp10

    $x->exp10                      # => BigNum

Exponential of C<$x> in base 10. (C<10**$x>)

=cut

sub exp10 {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_exp10($r, $r, $ROUND);
    _mpfr2big($r);
}

#
## Trigonometric functions
#

=head2 sin

    $x->sin                        # => BigNum

Returns the sine of C<$x>.

=cut

sub sin {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_sin($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 asin

    $x->asin                       # => BigNum | Nan

Returns the inverse sine of C<$x>.
Returns Nan for C<<$x < -1>> or C<<$x > 1>>.

=cut

sub asin {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_asin($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 sinh

    $x->sinh                       # => BigNum

Returns the hyperbolic sine of C<$x>.

=cut

sub sinh {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_sinh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 asinh

    $x->asinh                      # => BigNum

Returns the inverse hyperbolic sine of C<$x>.

=cut

sub asinh {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_asinh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 cos

    $x->cos                        # => BigNum

Returns the cosine of C<$x>.

=cut

sub cos {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_cos($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 acos

    $x->acos                       # => BigNum | Nan

Returns the inverse cosine of C<$x>.
Returns Nan for C<<$x < -1>> or C<<$x > 1>>.

=cut

sub acos {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_acos($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 cosh

    $x->cosh                       # => BigNum

Returns the hyperbolic cosine of C<$x>.

=cut

sub cosh {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_cosh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 acosh

    $x->acosh                      # => BigNum | Nan

Returns the inverse hyperbolic cosine of C<$x>.
Returns Nan for C<<$x < 1>>.

=cut

sub acosh {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_acosh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 tan

    $x->tan                        # => BigNum

Returns the tangent of C<$x>.

=cut

sub tan {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_tan($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 atan

    $x->atan                       # => BigNum

Returns the inverse tangent of C<$x>.

=cut

sub atan {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_atan($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 tanh

    $x->tanh                       # => BigNum

Returns the hyperbolic tangent of C<$x>.

=cut

sub tanh {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_tanh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 atanh

    $x->atanh                      # => BigNum | Nan

Returns the inverse hyperbolic tangent of C<$x>.
Returns Nan for C<<$x <= -1>> or C<<$x >= 1>>.

=cut

sub atanh {
    my ($x) = @_;
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_atanh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 sec

    $x->sec                        # => BigNum

Returns the secant of C<$x>.

=cut

sub sec {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_sec($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 asec

    $x->asec                       # => BigNum | Nan

Returns the inverse secant of C<$x>.
Returns Nan for C<<$x > -1>> and C<<$x < 1>>.

=cut

#
## asec(x) = acos(1/x)
#
sub asec {
    my ($x) = @_;
    state $one = Math::MPFR->new(1);
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_div($r, $one, $r, $ROUND);
    Math::MPFR::Rmpfr_acos($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 sech

    $x->sech                       # => BigNum

Returns the hyperbolic secant of C<$x>.

=cut

sub sech {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_sech($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 asech

    $x->asech                      # => BigNum | Nan

Returns the inverse hyperbolic secant of C<$x>.
Returns a Nan for C<<$x < 0>> or C<<$x > 1>>.

=cut

#
## asech(x) = acosh(1/x)
#
sub asech {
    my ($x) = @_;
    state $one = Math::MPFR->new(1);
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_div($r, $one, $r, $ROUND);
    Math::MPFR::Rmpfr_acosh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 csc

    $x->csc                        # => BigNum

Returns the cosecant of C<$x>.

=cut

sub csc {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_csc($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 acsc

    $x->acsc                       # => BigNum | Nan

Returns the inverse cosecant of C<$x>.
Returns Nan for C<<$x > -1>> and C<<$x < 1>>.

=cut

#
## acsc(x) = asin(1/x)
#
sub acsc {
    my ($x) = @_;
    state $one = Math::MPFR->new(1);
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_div($r, $one, $r, $ROUND);
    Math::MPFR::Rmpfr_asin($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 csch

    $x->csch                       # => BigNum

Returns the hyperbolic cosecant of C<$x>.

=cut

sub csch {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_csch($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 acsch

    $x->acsch                      # => BigNum

Returns the inverse hyperbolic cosecant of C<$x>.

=cut

#
## acsch(x) = asinh(1/x)
#
sub acsch {
    my ($x) = @_;
    state $one = Math::MPFR->new(1);
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_div($r, $one, $r, $ROUND);
    Math::MPFR::Rmpfr_asinh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 cot

    $x->cot                        # => BigNum

Returns the cotangent of C<$x>.

=cut

sub cot {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_cot($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 acot

    $x->acot                       # => BigNum

Returns the inverse cotangent of C<$x>.

=cut

#
## acot(x) = atan(1/x)
#
sub acot {
    my ($x) = @_;
    state $one = Math::MPFR->new(1);
    my $r = _big2mpfr($x);
    Math::MPFR::Rmpfr_div($r, $one, $r, $ROUND);
    Math::MPFR::Rmpfr_atan($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 coth

    $x->coth                       # => BigNum

Returns the hyperbolic cotangent of C<$x>.

=cut

sub coth {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_coth($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 acoth

    $x->acoth                      # => BigNum

Returns the inverse hyperbolic cotangent of C<$x>.

=cut

#
## acoth(x) = atanh(1/x)
#
sub acoth {
    state $one = Math::MPFR->new(1);
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_div($r, $one, $r, $ROUND);
    Math::MPFR::Rmpfr_atanh($r, $r, $ROUND);
    _mpfr2big($r);
}

=head2 atan2

    $x->atan2(BigNum)              # => BigNum
    $x->atan2(Scalar)              # => BigNum

    atan2(BigNum, BigNum)          # => BigNum
    atan2(BigNum, Scalar)          # => BigNum
    atan2(Scalar, BigNum)          # => BigNum

Arctangent of C<$x> and C<$y>. When C<$y> is -Inf returns PI when C<<$x >= 0>>, or C<-PI> when C<<$x < 0>>.

=cut

multimethod atan2 => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_atan2($r, $r, _big2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

multimethod atan2 => qw(Math::BigNum $) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_atan2($r, $r, _str2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

multimethod atan2 => qw($ Math::BigNum) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_atan2($r, $r, _big2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

multimethod atan2 => qw(Math::BigNum Math::BigNum::Inf) => sub {
    $_[1]->is_neg
      ? ((Math::GMPq::Rmpq_sgn(${$_[0]}) >= 0) ? pi() : (pi()->neg))
      : zero;
};
multimethod atan2 => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

#
## Comparisons
#

=head2 eq

    $x->eq(BigNum)                 # => Bool
    $x->eq(Scalar)                 # => Bool

    $x == $y                       # => Bool

Equality check: returns a true value when C<$x> and C<$y> are equal.

=cut

multimethod eq => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPq::Rmpq_equal(${$_[0]}, ${$_[1]});
};

multimethod eq => qw(Math::BigNum $) => sub {
    Math::GMPq::Rmpq_equal(${$_[0]}, _str2mpq($_[1]));
};

=for comment
multimethod eq => qw(Math::BigNum Math::BigNum::Complex) => sub {
    my ($x, $y) = @_;
    $y->im->is_zero && Math::GMPq::Rmpq_equal($$x, ${$y->re});
};
=cut

multimethod eq => qw(Math::BigNum Math::BigNum::Inf) => sub { 0 };
multimethod eq => qw(Math::BigNum Math::BigNum::Nan) => sub { 0 };

=head2 ne

    $x->ne(BigNum)                 # => Bool
    $x->ne(Scalar)                 # => Bool

    $x != $y                       # => Bool

Inequality check: returns a true value when C<$x> and C<$y> are not equal.

=cut

multimethod ne => qw(Math::BigNum Math::BigNum) => sub {
    !Math::GMPq::Rmpq_equal(${$_[0]}, ${$_[1]});
};

multimethod ne => qw(Math::BigNum $) => sub {
    my $y = Math::BigNum->new($_[1]);
    !Math::GMPq::Rmpq_equal(${$_[0]}, $$y);
};

=for comment
multimethod ne => qw(Math::BigNum Math::BigNum::Complex) => sub {
    my ($x, $y) = @_;
    !($y->im->is_zero && Math::GMPq::Rmpq_equal($$x, ${$y->re}));
};
=cut

multimethod ne => qw(Math::BigNum Math::BigNum::Inf) => sub { 1 };
multimethod ne => qw(Math::BigNum Math::BigNum::Nan) => sub { 1 };

=head2 gt

    $x->gt(BigNum)                 # => Bool
    $x->gt(Scalar)                 # => Bool

    BigNum > BigNum                # => Bool
    BigNum > Scalar                # => Bool
    Scalar > BigNum                # => Bool

Returns a true value when C<$x> is greater than C<$y>.

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

=for comment
multimethod gt => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->lt($_[0]);
};
=cut

multimethod gt => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->is_neg };
multimethod gt => qw(Math::BigNum Math::BigNum::Nan) => sub { 0 };

=head2 ge

    $x->ge(BigNum)                 # => Bool
    $x->ge(Scalar)                 # => Bool

    BigNum >= BigNum               # => Bool
    BigNum >= Scalar               # => Bool
    Scalar >= BigNum               # => Bool

Returns a true value when C<$x> is equal or greater than C<$y>.

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

=for comment
multimethod ge => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->le($_[0]);
};
=cut

multimethod ge => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->is_neg };
multimethod ge => qw(Math::BigNum Math::BigNum::Nan) => sub { 0 };

=head2 lt

    $x->lt(BigNum)                 # => Bool
    $x->lt(Scalar)                 # => Bool

    BigNum < BigNum                # => Bool
    BigNum < Scalar                # => Bool
    Scalar < BigNum                # => Bool

Returns a true value when C<$x> is less than C<$y>.

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

=for comment
multimethod lt => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->gt($_[0]);
};
=cut

multimethod lt => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->is_pos };
multimethod lt => qw(Math::BigNum Math::BigNum::Nan) => sub { 0 };

=head2 le

    $x->le(BigNum)                 # => Bool
    $x->le(Scalar)                 # => Bool

    BigNum <= BigNum               # => Bool
    BigNum <= Scalar               # => Bool
    Scalar <= BigNum               # => Bool

Returns a true value when C<$x> is equal or less than C<$y>.

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

=for comment
multimethod le => qw(Math::BigNum Math::BigNum::Complex) => sub {
    $_[1]->ge($_[0]);
};
=cut

multimethod le => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->is_pos };
multimethod le => qw(Math::BigNum Math::BigNum::Nan) => sub { 0 };

=head2 cmp

    $x->cmp(BigNum)                # => Scalar
    $x->cmp(Scalar)                # => Scalar

    BigNum <=> BigNum              # => Scalar
    BigNum <=> Scalar              # => Scalar
    Scalar <=> BigNum              # => Scalar

Compares C<$x> to C<$y> and returns a negative value when C<$x> is less than C<$y>,
0 when C<$x> and C<$y> are equal, and a positive value when C<$x> is greater than C<$y>.

=cut

multimethod cmp => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPq::Rmpq_cmp(${$_[0]}, ${$_[1]});
};

multimethod cmp => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if (CORE::int($y) eq $y) {
        $y >= 0
          ? Math::GMPq::Rmpq_cmp_ui($$x, $y, 1)
          : Math::GMPq::Rmpq_cmp_si($$x, $y, 1);
    }
    else {
        Math::GMPq::Rmpq_cmp($$x, _str2mpq($_[1]));
    }
};

multimethod cmp => qw($ Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (CORE::int($x) eq $x) {
        my $cmp =
          $x >= 0
          ? Math::GMPq::Rmpq_cmp_ui($$y, $x, 1)
          : Math::GMPq::Rmpq_cmp_si($$y, $x, 1);
        $cmp < 0 ? 1 : $cmp > 0 ? -1 : 0;
    }
    else {
        Math::GMPq::Rmpq_cmp(_str2mpq($_[0]), $$y);
    }
};

multimethod cmp => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->is_pos ? -1 : 1 };
multimethod cmp => qw(Math::BigNum Math::BigNum::Nan) => sub { 0 };

=head2 acmp

    $x->acmp(BigNum)               # => Scalar
    cmp(Scalar, BigNum)            # => Scalar

Compares the absolute values of C<$x> and C<$y>. Returns a negative value
when the absolute value of C<$x> is less than the absolute value of C<$y>,
0 when the absolute value of C<$x> is equal to the absolute value of C<$y>,
and a positive value when the absolute value of C<$x> is greater than the
absolute value of C<$y>.

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

multimethod acmp => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    my $xn = $$x;

    if (Math::GMPq::Rmpq_sgn($xn) < 0) {
        my $r = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_abs($r, $xn);
        $xn = $r;
    }

    if (CORE::int($y) eq $y) {
        Math::GMPq::Rmpq_cmp_ui($xn, CORE::abs($y), 1);
    }
    else {
        my $q = _str2mpq($y);
        Math::GMPq::Rmpq_abs($q, $q);
        Math::GMPq::Rmpq_cmp($xn, $q);
    }
};

=head2 rand

    $x->rand                       # => BigNum
    $x->rand(BigNum)               # => BigNum
    $x->rand(Scalar)               # => BigNum

Returns a random floating-point number. If an argument is provided,
it returns a number between C<$x> and C<$y>, otherwise returns a number lower
than C<$x>.

Example:

    10->rand;       # a random number between 0 and 10 (exclusive)
    10->rand(20);   # a random number between 10 and 20 (exclusive)

=cut

{
    state $state = Math::MPFR::Rmpfr_randinit_mt();
    state $seed  = do {
        my $seed = srand();
        Math::MPFR::Rmpfr_randseed_ui($state, $seed);
    };

    multimethod rand => qw(Math::BigNum) => sub {
        my ($x) = @_;

        my $rand = Math::MPFR::Rmpfr_init2($PREC);
        Math::MPFR::Rmpfr_urandom($rand, $state, $ROUND);

        my $q = Math::GMPq::Rmpq_init();
        Math::MPFR::Rmpfr_get_q($q, $rand);

        Math::GMPq::Rmpq_mul($q, $q, $$x);
        bless \$q, __PACKAGE__;
    };

    multimethod rand => qw(Math::BigNum Math::BigNum) => sub {
        my ($x, $y) = @_;

        my $rand = Math::MPFR::Rmpfr_init2($PREC);
        Math::MPFR::Rmpfr_urandom($rand, $state, $ROUND);

        my $q = Math::GMPq::Rmpq_init();
        Math::MPFR::Rmpfr_get_q($q, $rand);

        my $diff = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_sub($diff, $$y, $$x);
        Math::GMPq::Rmpq_mul($q, $q, $diff);
        Math::GMPq::Rmpq_add($q, $q, $$x);

        bless \$q, __PACKAGE__;
    };

    multimethod rand => qw(Math::BigNum $) => sub {
        $_[0]->rand(Math::BigNum->new($_[1]));
    };
}

=head2 mod

    $x->mod(BigNum)                # => BigNum | Nan
    $x->mod(Scalar)                # => BigNum | Nan

    BigNum % BigNum                # => BigNum | Nan
    BigNum % Scalar                # => BigNum | Nan
    Scalar % BigNum                # => BigNum | Nan

Remainder of C<$x> when is divided by C<$y>. Returns Nan when C<$y> is zero.

=cut

multimethod mod => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_integer_p($$x) and Math::GMPq::Rmpq_integer_p($$y)) {

        my $yz     = _big2mpz($y);
        my $sign_y = Math::GMPz::Rmpz_sgn($yz);
        return nan if !$sign_y;

        my $r = _big2mpz($x);
        Math::GMPz::Rmpz_mod($r, $r, $yz);
        if (!Math::GMPz::Rmpz_sgn($r)) {
            return (zero);    # return faster
        }
        elsif ($sign_y < 0) {
            Math::GMPz::Rmpz_add($r, $r, $yz);
        }
        _mpz2big($r);
    }
    else {
        my $r  = _big2mpfr($x);
        my $yf = _big2mpfr($y);
        Math::MPFR::Rmpfr_fmod($r, $r, $yf, $ROUND);
        my $sign_r = Math::MPFR::Rmpfr_sgn($r);
        if (!$sign_r) {
            return (zero);    # return faster
        }
        elsif ($sign_r > 0 xor Math::MPFR::Rmpfr_sgn($yf) > 0) {
            Math::MPFR::Rmpfr_add($r, $r, $yf, $ROUND);
        }
        _mpfr2big($r);
    }
};

multimethod mod => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    return nan if ($y == 0);

    if (Math::GMPq::Rmpq_integer_p($$x) and CORE::int($y) eq $y) {
        my $r     = _big2mpz($x);
        my $neg_y = $y < 0;
        $y = CORE::abs($y) if $neg_y;
        Math::GMPz::Rmpz_mod_ui($r, $r, $y);
        if (!Math::GMPz::Rmpz_sgn($r)) {
            return (zero);    # return faster
        }
        elsif ($neg_y) {
            Math::GMPz::Rmpz_sub_ui($r, $r, $y);
        }
        _mpz2big($r);
    }
    else {
        my $r  = _big2mpfr($x);
        my $yf = _str2mpfr($y);
        Math::MPFR::Rmpfr_fmod($r, $r, $yf, $ROUND);
        my $sign = Math::MPFR::Rmpfr_sgn($r);
        if (!$sign) {
            return (zero);    # return faster
        }
        elsif ($sign > 0 xor Math::MPFR::Rmpfr_sgn($yf) > 0) {
            Math::MPFR::Rmpfr_add($r, $r, $yf, $ROUND);
        }
        _mpfr2big($r);
    }
};

# This will happen rarely, so no special optimization.
multimethod mod => qw($ Math::BigNum) => sub {
    Math::BigNum->new($_[0])->bmod($_[1]);
};

multimethod mod => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[0]->copy };
multimethod mod => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bmod

    $x->bmod(BigNum)               # => BigNum | Nan
    $x->bmod(Scalar)               # => BigNum | Nan

    BigNum %= BigNum               # => BigNum | Nan
    BigNum %= Scalar               # => BigNum | Nan

Sets C<$x> to the remainder of C<$x> when is divided by C<$y>. Sets C<$x> to Nan when C<$y> is zero.

=cut

multimethod bmod => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    if (Math::GMPq::Rmpq_integer_p($$x) and Math::GMPq::Rmpq_integer_p($$y)) {

        my $yz     = _big2mpz($y);
        my $sign_y = Math::GMPz::Rmpz_sgn($yz);
        return $x->bnan if !$sign_y;

        my $r = _big2mpz($x);
        Math::GMPz::Rmpz_mod($r, $r, $yz);
        if ($sign_y < 0 and Math::GMPz::Rmpz_sgn($r)) {
            Math::GMPz::Rmpz_add($r, $r, $yz);
        }
        Math::GMPq::Rmpq_set_z($$x, $r);
    }
    else {
        my $r  = _big2mpfr($x);
        my $yf = _big2mpfr($y);
        Math::MPFR::Rmpfr_fmod($r, $r, $yf, $ROUND);
        my $sign = Math::MPFR::Rmpfr_sgn($r);
        if (!$sign) {
            ## ok
        }
        elsif ($sign > 0 xor Math::MPFR::Rmpfr_sgn($yf) > 0) {
            Math::MPFR::Rmpfr_add($r, $r, $yf, $ROUND);
        }
        _mpfr2x($x, $r);
    }

    $x;
};

multimethod bmod => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    return $x->bnan if ($y == 0);

    if (Math::GMPq::Rmpq_integer_p($$x) and CORE::int($y) eq $y) {
        my $r     = _big2mpz($x);
        my $neg_y = $y < 0;
        $y = CORE::abs($y) if $neg_y;
        Math::GMPz::Rmpz_mod_ui($r, $r, $y);
        if ($neg_y and Math::GMPz::Rmpz_sgn($r)) {
            Math::GMPz::Rmpz_sub_ui($r, $r, $y);
        }
        Math::GMPq::Rmpq_set_z($$x, $r);
    }
    else {
        my $r  = _big2mpfr($x);
        my $yf = _str2mpfr($y);
        Math::MPFR::Rmpfr_fmod($r, $r, $yf, $ROUND);
        my $sign_r = Math::MPFR::Rmpfr_sgn($r);
        if (!$sign_r) {
            ## ok
        }
        elsif ($sign_r > 0 xor Math::MPFR::Rmpfr_sgn($yf) > 0) {
            Math::MPFR::Rmpfr_add($r, $r, $yf, $ROUND);
        }
        _mpfr2x($x, $r);
    }

    $x;
};

multimethod bmod => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[0] };
multimethod bmod => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 imod

    $x->imod(BigNum)               # => BigNum | Nan
    $x->imod(Scalar)               # => BigNum | Nan

Integer remainder of C<$x> when is divided by C<$y>. If necessary, C<$x> and C<$y>
are implicitly truncated to integers. Nan is returned when C<$y> is zero.

=cut

multimethod imod => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $yz     = _big2mpz($y);
    my $sign_y = Math::GMPz::Rmpz_sgn($yz);
    return nan if !$sign_y;

    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_mod($r, $r, $yz);
    if (!Math::GMPz::Rmpz_sgn($r)) {
        return (zero);    # return faster
    }
    elsif ($sign_y < 0) {
        Math::GMPz::Rmpz_add($r, $r, $yz);
    }
    _mpz2big($r);
};

multimethod imod => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    $y = CORE::int($y);
    return nan if ($y == 0);

    my $r     = _big2mpz($x);
    my $neg_y = $y < 0;
    $y = CORE::abs($y) if $neg_y;
    Math::GMPz::Rmpz_mod_ui($r, $r, $y);
    if (!Math::GMPz::Rmpz_sgn($r)) {
        return (zero);    # return faster
    }
    elsif ($neg_y) {
        Math::GMPz::Rmpz_sub_ui($r, $r, $y);
    }
    _mpz2big($r);
};

# This will happen rarely, so no special optimization.
multimethod imod => qw($ Math::BigNum) => sub {
    Math::BigNum->new($_[0])->bimod($_[1]);
};

multimethod imod => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[0]->copy };
multimethod imod => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 bimod

    $x->bimod(BigNum)              # => BigNum | Nan
    $x->bimod(Scalar)              # => BigNum | Nan

Sets C<$x> to the remainder of C<$x> divided by C<$y>. If necessary, C<$x> and C<$y>
are implicitly truncated to integers. Sets C<$x> to Nan when C<$y> is zero.

=cut

multimethod bimod => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $yz     = _big2mpz($y);
    my $sign_y = Math::GMPz::Rmpz_sgn($yz);
    return $x->bnan if !$sign_y;

    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_mod($r, $r, $yz);
    if ($sign_y < 0 and Math::GMPz::Rmpz_sgn($r)) {
        Math::GMPz::Rmpz_add($r, $r, $yz);
    }
    Math::GMPq::Rmpq_set_z($$x, $r);
    $x;
};

multimethod bimod => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    $y = CORE::int($y);
    return $x->bnan if ($y == 0);

    my $r     = _big2mpz($x);
    my $neg_y = $y < 0;
    $y = CORE::abs($y) if $neg_y;
    Math::GMPz::Rmpz_mod_ui($r, $r, $y);
    if ($neg_y and Math::GMPz::Rmpz_sgn($r)) {
        Math::GMPz::Rmpz_sub_ui($r, $r, $y);
    }
    Math::GMPq::Rmpq_set_z($$x, $r);

    $x;
};

multimethod bimod => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[0] };
multimethod bimod => qw(Math::BigNum Math::BigNum::Nan) => \&bnan;

=head2 divmod

    $x->divmod(BigNum)             # => (BigNum, BigNum) | (Nan, Nan)
    $x->divmod(Scalar)             # => (BigNum, BigNum) | (Nan, Nan)

Returns the quotient and the remainder from division of C<$x> by C<$y>,
where both are integers. When C<$y> is zero, it returns two Nan values.

=cut

multimethod divmod => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;

    my $r1 = _big2mpz($x);
    my $r2 = _big2mpz($y);

    Math::GMPz::Rmpz_sgn($$y) || return (nan, nan);

    Math::GMPz::Rmpz_divmod($r1, $r2, $r1, $r2);
    (_mpz2big($r1), _mpz2big($r2));
};

multimethod divmod => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    CORE::int($y) || return (nan, nan);

    my $r1 = _big2mpz($x);
    my $r2 = Math::GMPz::Rmpz_init();

    Math::GMPz::Rmpz_divmod_ui($r1, $r2, $r1, CORE::int($y));
    (_mpz2big($r1), _mpz2big($r2));
};

=head2 modinv

    $x->modinv(BigNum)             # => BigNum | Nan
    $x->modinv(Scalar)             # => BigNum | Nan

Computes the inverse of C<$x> modulo C<$y> and returns the result.
If an inverse doesn't exist the Nan value is returned.

=cut

multimethod modinv => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_invert($r, $r, _big2mpz($y)) || return nan;
    _mpz2big($r);
};

multimethod modinv => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_invert($r, $r, _str2mpz($y)) || return nan;
    _mpz2big($r);
};

=head2 modpow

    $x->modpow(BigNum, BigNum)     # => BigNum

Calculates C<($x ** $y) % $z>, where all three values are integers.

=cut

multimethod modpow => qw(Math::BigNum Math::BigNum Math::BigNum) => sub {
    my ($x, $y, $z) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_powm($r, $r, _big2mpz($y), _big2mpz($z));
    _mpz2big($r);
};

multimethod modpow => qw(Math::BigNum Math::BigNum $) => sub {
    my ($x, $y, $z) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_powm($r, $r, _big2mpz($y), _str2mpz($z));
    _mpz2big($r);
};

multimethod modpow => qw(Math::BigNum $ $) => sub {
    my ($x, $y, $z) = @_;
    my $r = _big2mpz($x);
    if ($y >= 0) {
        Math::GMPz::Rmpz_powm_ui($r, $r, CORE::int($y), _str2mpz($z));
    }
    else {
        Math::GMPz::Rmpz_powm($r, $r, _str2mpz($y), _str2mpz($z));
    }
    _mpz2big($r);
};

multimethod modpow => qw(Math::BigNum $ Math::BigNum) => sub {
    my ($x, $y, $z) = @_;
    my $r = _big2mpz($x);
    if ($y >= 0) {
        Math::GMPz::Rmpz_powm_ui($r, $r, CORE::int($y), _big2mpz($z));
    }
    else {
        Math::GMPz::Rmpz_powm($r, $r, _str2mpz($y), _big2mpz($z));
    }
    _mpz2big($r);
};

#
## Miscellaneous
#

=head2 is_zero

    $x->is_zero                    # => Bool

Returns a true value when C<$x> is 0.

=cut

sub is_zero {
    !Math::GMPq::Rmpq_sgn(${$_[0]});
}

=head2 is_one

    $x->is_one                     # => Bool

Returns a true value when C<$x> is +1.

=cut

sub is_one {
    Math::GMPq::Rmpq_equal(${$_[0]}, $ONE);
}

=head2 is_mone

    $x->is_mone                    # => Bool

Returns a true value when C<$x> is -1.

=cut

sub is_mone {
    Math::GMPq::Rmpq_equal(${$_[0]}, $MONE);
}

=head2 is_pos

    $x->is_pos                     # => Bool

Returns a true value when C<$x> is greater than zero.

=cut

sub is_pos {
    Math::GMPq::Rmpq_sgn(${$_[0]}) > 0;
}

=head2 is_neg

    $x->is_neg                     # => Bool

Returns a true value when C<$x> is less than zero.

=cut

sub is_neg {
    Math::GMPq::Rmpq_sgn(${$_[0]}) < 0;
}

=head2 is_int

    $x->is_int                     # => Bool

Returns a true value when C<$x> is an integer.

=cut

sub is_int {
    Math::GMPq::Rmpq_integer_p(${$_[0]});
}

=head2 is_real

    $x->is_real                    # => Bool

Always returns a true value when invoked on a Math::BigNum object.

=cut

sub is_real { 1 }

=head2 is_inf

    $x->is_inf                     # => Bool

Always returns a false value when invoked on a Math::BigNum object.

=cut

sub is_inf { 0 }

=head2 is_nan

    $x->is_nan                     # => Bool

Always returns a false value when invoked on a Math::BigNum object.

=cut

sub is_nan { 0 }

=head2 is_ninf

    $x->is_ninf                    # => Bool

Always returns a false value when invoked on a Math::BigNum object.

=cut

sub is_ninf { 0 }

=head2 is_even

    $x->is_even                    # => Bool

Returns a true value when C<$x> is divisible by 2. Returns C<0> if C<$x> is NOT an integer.

=cut

sub is_even {
    my ($x) = @_;
    Math::GMPq::Rmpq_integer_p($$x) || return 0;
    my $nz = Math::GMPz::Rmpz_init();
    Math::GMPq::Rmpq_get_num($nz, $$x);
    Math::GMPz::Rmpz_even_p($nz);
}

=head2 is_odd

    $x->is_odd                     # => Bool

Returns a true value when C<$x> is NOT divisible by 2. Returns C<0> if C<$x> is NOT an integer.

=cut

sub is_odd {
    my ($x) = @_;
    Math::GMPq::Rmpq_integer_p($$x) || return 0;
    my $nz = Math::GMPz::Rmpz_init();
    Math::GMPq::Rmpq_get_num($nz, $$x);
    Math::GMPz::Rmpz_odd_p($nz);
}

=head2 is_div

    $x->is_div(BigNum)             # => Bool
    $x->is_div(Scalar)             # => Bool

Returns a true value if C<$x> is divisible by C<$y>. False otherwise.
If C<$y> is zero, returns C<0>.

=cut

multimethod is_div => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_sgn($$y) || return 0;
    my $q = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_div($q, $$x, $$y);
    Math::GMPq::Rmpq_integer_p($q);
};

multimethod is_div => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    $y == 0 and return 0;

    # Use a faster method when both $x and $y are integers
    if ($y > 0 and CORE::int($y) eq $y and Math::GMPq::Rmpq_integer_p($$x)) {
        Math::GMPz::Rmpz_divisible_ui_p(_big2mpz($x), $y);
    }

    # Otherwise, do the division and check the result
    else {
        my $q = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_div($q, $$x, _str2mpq($y));
        Math::GMPq::Rmpq_integer_p($q);
    }
};

multimethod is_div => qw(Math::BigNum Math::BigNum::Inf) => sub { 0 };
multimethod is_div => qw(Math::BigNum Math::BigNum::Nan) => sub { 0 };

=head2 is_psqr

    $n->is_psqr                    # => Bool

Returns a true value when C<$n> is a perfect square. False otherwise.
When C<$n> is not an integer, returns C<0>.

=cut

sub is_psqr {
    my ($x) = @_;
    Math::GMPq::Rmpq_integer_p($$x) || return 0;
    my $nz = Math::GMPz::Rmpz_init();
    Math::GMPq::Rmpq_get_num($nz, $$x);
    Math::GMPz::Rmpz_perfect_square_p($nz);
}

=head2 is_ppow

    $n->is_ppow                    # => Bool

Returns a true value when C<$n> is a perfect power. False otherwise.
When C<$n> is not an integer, returns C<0>.

=cut

sub is_ppow {
    my ($x) = @_;
    Math::GMPq::Rmpq_integer_p($$x) || return 0;
    my $nz = Math::GMPz::Rmpz_init();
    Math::GMPq::Rmpq_get_num($nz, $$x);
    Math::GMPz::Rmpz_perfect_power_p($nz);
}

=head2 sign

    $x->sign                       # => Scalar

Returns C<'-'> when C<$x> is negative, C<'+'> when C<$x> is positive, and C<''> when C<$x> is zero.

=cut

sub sign {
    my $sign = Math::GMPq::Rmpq_sgn(${$_[0]});
    $sign > 0 ? '+' : $sign < 0 ? '-' : '';
}

=head2 min

    $x->min(BigNum)                # => BigNum

Returns C<$x> if C<$x> is lower than C<$y>. Returns C<$y> otherwise.

=cut

multimethod min => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_cmp($$x, $$y) < 0 ? $x : $y;
};

multimethod min => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->is_pos ? $_[0] : $_[1] };
multimethod min => qw(Math::BigNum Math::BigNum::Nan) => sub { $_[1] };

=head2 max

    $x->max(BigNum)                # => BigNum

Returns C<$x> if C<$x> is greater than C<$y>. Returns C<$y> otherwise.

=cut

multimethod max => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    Math::GMPq::Rmpq_cmp($$x, $$y) > 0 ? $x : $y;
};

multimethod max => qw(Math::BigNum Math::BigNum::Inf) => sub { $_[1]->is_pos ? $_[1] : $_[0] };
multimethod max => qw(Math::BigNum Math::BigNum::Nan) => sub { $_[1] };

=head2 gcd

    $x->gcd(BigNum)                # => BigNum
    $x->gcd(Scalar)                # => BigNum

The greatest common divisor of C<$x> and C<$y>.

=cut

multimethod gcd => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_gcd($r, $r, _big2mpz($y));
    _mpz2big($r);
};

multimethod gcd => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_gcd($r, $r, _str2mpz($y));
    _mpz2big($r);
};

multimethod gcd => qw(Math::BigNum Math::BigNum::Inf) => \&nan;
multimethod gcd => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 lcm

    $x->lcd(BigNum)                # => BigNum
    $x->lcd(Scalar)                # => BigNum

The least common multiple of C<$x> and C<$y>.

=cut

multimethod lcm => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_lcm($r, $r, _big2mpz($y));
    _mpz2big($r);
};

multimethod lcm => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_lcm($r, $r, _str2mpz($y));
    _mpz2big($r);
};

multimethod lcm => qw(Math::BigNum Math::BigNum::Inf) => \&nan;
multimethod lcm => qw(Math::BigNum Math::BigNum::Nan) => \&nan;

=head2 int

    $x->int                        # => BigNum
    int($x)                        # => BigNum

Returns a truncated integer from the value of C<$x>.

=cut

sub int {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    _mpz2big($z);
}

=head2 bint

    $x->bint                       # => BigNum

Truncates C<$x> to an integer in-place.

=cut

sub bint {
    my ($x) = @_;
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, $$x);
    Math::GMPq::Rmpq_set_z($$x, $z);
    $x;
}

=head2 float

    $x->float                      # => BigNum

Returns a truncated number that fits inside
number of bits specified in C<$Math::BigNum::PREC>.

=cut

sub float {
    my $f = Math::MPFR::Rmpfr_init2($PREC);
    Math::MPFR::Rmpfr_set_q($f, ${$_[0]}, $ROUND);
    _mpfr2big($f);
}

=head2 as_frac

    $x->as_frac                    # => Scalar

Returns a string representing the number as a fraction.
For C<$x=0.5>, it returns C<"1/2">. For C<$x=3>, it returns C<"3/1">.

=cut

sub as_frac {
    my $rat = Math::GMPq::Rmpq_get_str(${$_[0]}, 10);
    index($rat, '/') == -1 ? "$rat/1" : $rat;
}

=head2 as_rat

    $x->as_rat                     # => Scalar

Almost the same as C<as_frac()>, except that integers are returned as they are,
without adding the "1" denominator. For C<$x=0.5>, it returns C<"1/2">. For
C<$x=3>, it simply returns C<"3">.

=cut

sub as_rat {
    Math::GMPq::Rmpq_get_str(${$_[0]}, 10);
}

=head2 as_float

    $x->as_float                   # => Scalar
    $x->as_float(Scalar)           # => Scalar
    $x->as_float(BigNum)           # => Scalar

Returns the self-number as a floating-point scalar. The method also accepts
an optional argument for precision after the decimal point. When no argument
is provided, it uses the default precision.

Example for C<$x = 1/3>:

    $x->as_float(4);        # returns "0.3333"

If the self number is an integer, it will be returned as it is.

=cut

multimethod as_float => qw(Math::BigNum) => sub {
    $_[0]->stringify;
};

multimethod as_float => qw(Math::BigNum $) => sub {
    local $Math::BigNum::PREC = 4 * $_[1];
    $_[0]->stringify;
};

multimethod as_float => qw(Math::BigNum Math::BigNum) => sub {
    local $Math::BigNum::PREC = 4 * Math::GMPq::Rmpq_get_d(${$_[1]});
    $_[0]->stringify;
};

=head2 as_int

    $x->as_int                     # => Scalar
    $x->as_int(Scalar)             # => Scalar
    $x->as_int(BigNum)             # => Scalar

Returns the self-number as an integer in a given base. When the base is omitted, it
defaults to 10.

Example for C<$x = 255>:

    $x->as_int          # returns: "255"
    $x->as_int(16)      # returns: "ff"

=cut

multimethod as_int => qw(Math::BigNum) => sub {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_get_str($z, 10);
};

multimethod as_int => qw(Math::BigNum $) => sub {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});

    my $base = CORE::int($_[1]);
    if ($base < 2 or $base > 36) {
        require Carp;
        Carp::croak("base must be between 2 and 36, got $base");
    }

    Math::GMPz::Rmpz_get_str($z, $base);
};

multimethod as_int => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->as_int(Math::GMPq::Rmpq_get_d(${$_[1]}));
};

=head2 as_bin

    $x->as_bin                     # => Scalar

Returns a string representing the value of C<$x> in binary.
For C<$x=42>, it returns C<"101010">.

=cut

sub as_bin {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_get_str($z, 2);
}

=head2 as_oct

    $x->as_oct                     # => Scalar

Returns a string representing the value of C<$x> in octal.
For C<$x=42>, it returns C<"52">.

=cut

sub as_oct {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_get_str($z, 8);
}

=head2 as_hex

    $x->as_hex                     # => Scalar

Returns a string representing the value of C<$x> in hexadecimal.
For C<$x=42>, it returns C<"2a">.

=cut

sub as_hex {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_get_str($z, 16);
}

=head2 in_base

    $x->in_base(BigNum)            # => Scalar
    $x->in_base(Scalar)            # => Scalar

Returns a string with the value of C<$x> in a given base,
where the base can range from 2 to 36 inclusive. If C<$x>
is not an integer, the result is returned in rationalized
form.

=cut

multimethod in_base => qw(Math::BigNum $) => sub {
    my ($x, $y) = @_;

    if ($y < 2 or $y > 36) {
        require Carp;
        Carp::croak("base must be between 2 and 36, got $y");
    }

    Math::GMPq::Rmpq_get_str(${$_[0]}, $y);
};

multimethod in_base => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->in_base(CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]})));
};

=head2 digits

    $x->digits                     # => List of scalars

Returns a list with the digits of C<$x> in base 10 before the decimal point.
For C<$x=-1234.56>, it returns C<(1,2,3,4)>

=cut

sub digits {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_abs($z, $z);
    split(//, Math::GMPz::Rmpz_get_str($z, 10));
}

=head2 length

    $x->length                     # => Scalar

Returns the number of digits of C<$x> in base 10 before the decimal point.
For C<$x=-1234.56>, it returns C<4>.

=cut

sub length {
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_set_q($z, ${$_[0]});
    Math::GMPz::Rmpz_abs($z, $z);
    Math::GMPz::Rmpz_snprintf(my $buf, 0, "%Zd", $z, 0);
}

=head2 numerator

    $x->numerator                  # => BigNum

Returns a copy of the numerator as signed BigNum.

=cut

sub numerator {
    my ($x) = @_;
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPq::Rmpq_get_num($z, $$x);
    _mpz2big($z);
}

=head2 denominator

    $x->denominator                # => BigNum

Returns a copy of the denominator as positive BigNum.

=cut

sub denominator {
    my ($x) = @_;
    my $z = Math::GMPz::Rmpz_init();
    Math::GMPq::Rmpq_get_den($z, $$x);
    _mpz2big($z);
}

=head2 floor

    $x->floor                      # => BigNum

Returns C<$x> if C<$x> is an integer, otherwise it rounds C<$x> towards -Infinity.
For C<$x=2.5>, returns C<2>, and for C<$x=-2.5>, returns C<-3>.

=cut

sub floor {
    my ($x) = @_;
    Math::GMPq::Rmpq_integer_p($$x) && return $x;

    if (Math::GMPq::Rmpq_sgn($$x) > 0) {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        _mpz2big($z);
    }
    else {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_sub_ui($z, $z, 1);
        _mpz2big($z);
    }
}

=head2 ceil

    $x->ceil                       # => BigNum

Returns C<$x> if C<$x> is an integer, otherwise it rounds C<$x> towards +Infinity.
For C<$x=2.5>, returns C<3>, and for C<$x=-2.5>, returns C<-2>.

=cut

sub ceil {
    my ($x) = @_;
    Math::GMPq::Rmpq_integer_p($$x) && return $x;

    if (Math::GMPq::Rmpq_sgn($$x) > 0) {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        Math::GMPz::Rmpz_add_ui($z, $z, 1);
        _mpz2big($z);
    }
    else {
        my $z = Math::GMPz::Rmpz_init();
        Math::GMPz::Rmpz_set_q($z, $$x);
        _mpz2big($z);
    }
}

=head2 round

    $x->round(BigNum)              # => BigNum
    $x->round(Scalar)              # => BigNum

Rounds C<$x> to the nth place. A negative argument rounds that many digits
after the decimal point, while a positive argument rounds before the decimal
point. This method uses the "round half to even" algorithm, which is the
default rounding mode used in IEEE 754 computing functions and operators.

=cut

multimethod round => qw(Math::BigNum $) => sub {
    $_[0]->copy->bround($_[1]);
};

multimethod round => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->copy->bround(Math::GMPq::Rmpq_get_d(${$_[1]}));
};

=head2 bround

    $x->bround(BigNum)             # => BigNum
    $x->bround(Scalar)             # => BigNum

Rounds C<$x> in-place to nth places.

=cut

multimethod bround => qw(Math::BigNum $) => sub {
    my ($x, $prec) = @_;

    my $n   = $$x;
    my $nth = -CORE::int($prec);
    my $sgn = Math::GMPq::Rmpq_sgn($n);

    Math::GMPq::Rmpq_abs($n, $n) if $sgn < 0;

    my $z = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_ui_pow_ui($z, 10, CORE::abs($nth));

    my $p = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_z($p, $z);

    if ($nth < 0) {
        Math::GMPq::Rmpq_div($n, $n, $p);
    }
    else {
        Math::GMPq::Rmpq_mul($n, $n, $p);
    }

    state $half = do {
        my $q = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set_ui($q, 1, 2);
        $q;
    };

    Math::GMPq::Rmpq_add($n, $n, $half);
    Math::GMPz::Rmpz_set_q($z, $n);

    if (Math::GMPz::Rmpz_odd_p($z) and Math::GMPq::Rmpq_integer_p($n)) {
        Math::GMPz::Rmpz_sub_ui($z, $z, 1);
    }

    Math::GMPq::Rmpq_set_z($n, $z);

    if ($nth < 0) {
        Math::GMPq::Rmpq_mul($n, $n, $p);
    }
    else {
        Math::GMPq::Rmpq_div($n, $n, $p);
    }

    if ($sgn < 0) {
        Math::GMPq::Rmpq_neg($n, $n);
    }

    $x;
};

multimethod bround => qw(Math::BigNum Math::BigNum) => sub {
    $_[0]->bround(Math::GMPq::Rmpq_get_d(${$_[1]}));
};

=head2 inc

    $x->inc                        # => BigNum

Returns C<$x + 1>.

=cut

sub inc {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_add($r, $$x, $ONE);
    bless \$r, __PACKAGE__;
}

=head2 binc

    $x->binc                       # => BigNum
    ++$x                           # => BigNum
    $x++                           # => BigNum

Increments C<$x> in-place by 1.

=cut

sub binc {
    my ($x) = @_;
    Math::GMPq::Rmpq_add($$x, $$x, $ONE);
    $x;
}

=head2 dec

    $x->dec                        # => BigNum

Returns C<$x - 1>.

=cut

sub dec {
    my ($x) = @_;
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_sub($r, $$x, $ONE);
    bless \$r, __PACKAGE__;
}

=head2 bdec

    $x->bdec                       # => BigNum
    --$x                           # => BigNum
    $x--                           # => BigNum

Decrements C<$x> in-place by 1.

=cut

sub bdec {
    my ($x) = @_;
    Math::GMPq::Rmpq_sub($$x, $$x, $ONE);
    $x;
}

#
## Integer operations
#

=head2 and

    $x->and(BigNum)                # => BigNum
    $x->and(Scalar)                # => BigNum

    BigNum & BigNum                # => BigNum
    BigNum & Scalar                # => BigNum
    Scalar & BigNum                # => BigNum

Integer logical-and operation.

=cut

multimethod and => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_and($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

multimethod and => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_and($r, $r, _str2mpz($_[1]));
    _mpz2big($r);
};

multimethod and => qw($ Math::BigNum) => sub {
    my $r = _str2mpz($_[0]);
    Math::GMPz::Rmpz_and($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

=head2 band

    $x->band(BigNum)               # => BigNum
    $x->band(Scalar)               # => BigNum

    BigNum &= BigNum               # => BigNum
    BigNum &= Scalar               # => BigNum

Integer logical-and operation, changing C<$x> in-place.

=cut

multimethod band => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_and($r, $r, _big2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod band => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_and($r, $r, _str2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

=head2 ior

    $x->ior(BigNum)                # => BigNum
    $x->ior(Scalar)                # => BigNum

    BigNum | BigNum                # => BigNum
    BigNum | Scalar                # => BigNum
    Scalar | BigNum                # => BigNum

Integer logical inclusive-or operation.

=cut

multimethod ior => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_ior($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

multimethod ior => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_ior($r, $r, _str2mpz($_[1]));
    _mpz2big($r);
};

multimethod ior => qw($ Math::BigNum) => sub {
    my $r = _str2mpz($_[0]);
    Math::GMPz::Rmpz_ior($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

=head2 bior

    $x->bior(BigNum)               # => BigNum
    $x->bior(Scalar)               # => BigNum

    BigNum |= BigNum               # => BigNum
    BigNum |= Scalar               # => BigNum

Integer logical inclusive-or operation, changing C<$x> in-place.

=cut

multimethod bior => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_ior($r, $r, _big2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod bior => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_ior($r, $r, _str2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

=head2 xor

    $x->xor(BigNum)                # => BigNum
    $x->xor(Scalar)                # => BigNum

    BigNum ^ BigNum                # => BigNum
    BigNum ^ Scalar                # => BigNum
    Scalar ^ BigNum                # => BigNum

Integer logical exclusive-or operation.

=cut

multimethod xor => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_xor($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

multimethod xor => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_xor($r, $r, _str2mpz($_[1]));
    _mpz2big($r);
};

multimethod xor => qw($ Math::BigNum) => sub {
    my $r = _str2mpz($_[0]);
    Math::GMPz::Rmpz_xor($r, $r, _big2mpz($_[1]));
    _mpz2big($r);
};

=head2 bxor

    $x->bxor(BigNum)               # => BigNum
    $x->bxor(Scalar)               # => BigNum

    BigNum ^= BigNum               # => BigNum
    BigNum ^= Scalar               # => BigNum

Integer logical exclusive-or operation, changing C<$x> in-place.

=cut

multimethod bxor => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_xor($r, $r, _big2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod bxor => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_xor($r, $r, _str2mpz($_[1]));
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

=head2 not

    $x->not                        # => BigNum
    ~BigNum                        # => BigNum

Integer logical-not operation. (The one's complement of $x).

=cut

sub not {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_com($r, $r);
    _mpz2big($r);
}

=head2 bnot

    $x->bnot                       # => BigNum

Integer logical-not operation, changing C<$x> in-place.

=cut

sub bnot {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_com($r, $r);
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
}

=head2 lsft

    $x->lsft(BigNum)               # => BigNum
    $x->lsft(Scalar)               # => BigNum

    BigNum << BigNum               # => BigNum
    BigNum << Scalar               # => BigNum
    Scalar << BigNum               # => BigNum

Integer left-shift operation. (C<$x * (2 ** $y)>)

=cut

multimethod lsft => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    my $i = CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]}));
    if ($i < 0) {
        Math::GMPz::Rmpz_div_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_mul_2exp($r, $r, $i);
    }
    _mpz2big($r);
};

multimethod lsft => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $i = CORE::int($_[1]);
    if ($i < 0) {
        Math::GMPz::Rmpz_div_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_mul_2exp($r, $r, $i);
    }
    _mpz2big($r);
};

multimethod lsft => qw($ Math::BigNum) => sub {
    my $r = _str2mpz($_[0]);
    my $i = CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]}));
    if ($i < 0) {
        Math::GMPz::Rmpz_div_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_mul_2exp($r, $r, $i);
    }
    _mpz2big($r);
};

=head2 blsft

    $x->blsft(BigNum)              # => BigNum
    $x->blsft(Scalar)              # => BigNum

    BigNum <<= BigNum              # => BigNum
    BigNum <<= Scalar              # => BigNum

Integer left-shift operation, changing C<$x> in-place. Promotes C<$x> to Nan when C<$y> is negative.
(C<$x * (2 ** $y)>)

=cut

multimethod blsft => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    my $i = CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]}));
    if ($i < 0) {
        Math::GMPz::Rmpz_div_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_mul_2exp($r, $r, $i);
    }
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod blsft => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $i = CORE::int($_[1]);
    if ($i < 0) {
        Math::GMPz::Rmpz_div_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_mul_2exp($r, $r, $i);
    }
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

=head2 rsft

    $x->rsft(BigNum)               # => BigNum
    $x->rsft(Scalar)               # => BigNum

    BigNum >> BigNum               # => BigNum
    BigNum >> Scalar               # => BigNum
    Scalar >> BigNum               # => BigNum

Integer right-shift operation. (C<$x / (2 ** $y)>)

=cut

multimethod rsft => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    my $i = CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]}));
    if ($i < 0) {
        Math::GMPz::Rmpz_mul_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_div_2exp($r, $r, $i);
    }
    _mpz2big($r);
};

multimethod rsft => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $i = CORE::int($_[1]);
    if ($i < 0) {
        Math::GMPz::Rmpz_mul_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_div_2exp($r, $r, $i);
    }
    _mpz2big($r);
};

multimethod rsft => qw($ Math::BigNum) => sub {
    my $r = _str2mpz($_[0]);
    my $i = CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]}));
    if ($i < 0) {
        Math::GMPz::Rmpz_mul_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_div_2exp($r, $r, $i);
    }
    _mpz2big($r);
};

=head2 brsft

    $x->brsft(BigNum)              # => BigNum
    $x->brsft(Scalar)              # => BigNum

    BigNum >>= BigNum              # => BigNum
    BigNum >>= Scalar              # => BigNum

Integer right-shift operation, changing C<$x> in-place. (C<$x / (2 ** $y)>)

=cut

multimethod brsft => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpz($_[0]);
    my $i = CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]}));
    if ($i < 0) {
        Math::GMPz::Rmpz_mul_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_div_2exp($r, $r, $i);
    }
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

multimethod brsft => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    my $i = CORE::int($_[1]);
    if ($i < 0) {
        Math::GMPz::Rmpz_mul_2exp($r, $r, CORE::abs($i));
    }
    else {
        Math::GMPz::Rmpz_div_2exp($r, $r, $i);
    }
    Math::GMPq::Rmpq_set_z(${$_[0]}, $r);
    $_[0];
};

=head2 fac

    $n->fac                        # => BigNum | Nan
    fac(Scalar)                    # => BigNum | Nan

Factorial of C<$n>. Returns Nan when C<$n> is negative. (C<1*2*3*...*$n>)

=cut

multimethod fac => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return nan if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fac_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2big($r);
};

multimethod fac => qw($) => sub {
    my ($x) = @_;
    return nan if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fac_ui($r, CORE::int($x));
    _mpz2big($r);
};

=head2 bfac

    $n->bfac                       # => BigNum | Nan

Factorial of C<$n>, by changing C<$n> in-place.

=cut

sub bfac {
    my ($x) = @_;
    return $x->bnan if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fac_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    Math::GMPq::Rmpq_set_z($$x, $r);
    $x;
}

=head2 dfac

    $n->dfac                       # => BigNum | Nan
    dfac(Scalar)                   # => BigNum | Nan

Double factorial of C<$n>. Returns Nan when C<$n> is negative.

=cut

multimethod dfac => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return nan if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_2fac_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2big($r);
};

multimethod dfac => qw($) => sub {
    my ($x) = @_;
    return nan if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_2fac_ui($r, CORE::int($x));
    _mpz2big($r);
};

=head2 primorial

    $n->primorial                  # => BigNum | Nan
    primorial(Scalar)              # => BigNum | Nan

Returns the product of all the primes less than or equal to C<$n>.

=cut

multimethod primorial => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return nan if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_primorial_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2big($r);
};

multimethod primorial => qw($) => sub {
    my ($x) = @_;
    return nan if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_primorial_ui($r, CORE::int($x));
    _mpz2big($r);
};

=head2 fib

    $n->fib                        # => BigNum | Nan
    fib(Scalar)                    # => BigNum | Nan

The $n'th Fibonacci number. Returns Nan when C<$n> is negative.

=cut

multimethod fib => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return nan if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fib_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2big($r);
};

multimethod fib => qw($) => sub {
    my ($x) = @_;
    return nan if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_fib_ui($r, CORE::int($x));
    _mpz2big($r);
};

=head2 lucas

    $n->lucas                      # => BigNum | Nan
    lucas(Scalar)                  # => BigNum | Nan

The $n'th Lucas number. Returns Nan when C<$n> is negative.

=cut

multimethod lucas => qw(Math::BigNum) => sub {
    my ($x) = @_;
    return nan if Math::GMPq::Rmpq_sgn($$x) < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_lucnum_ui($r, CORE::int(Math::GMPq::Rmpq_get_d($$x)));
    _mpz2big($r);
};

multimethod lucas => qw($) => sub {
    my ($x) = @_;
    return nan if $x < 0;
    my $r = Math::GMPz::Rmpz_init();
    Math::GMPz::Rmpz_lucnum_ui($r, CORE::int($x));
    _mpz2big($r);
};

=head2 binomial

    $n->binomial(BigNum)           # => BigNum
    $n->binomial(Scalar)           # => BigNum

Calculates the binomial coefficient n over k, also called the
"choose" function. The result is equivalent to:

           ( n )       n!
           |   |  = -------
           ( k )    k!(n-k)!

=cut

multimethod binomial => qw(Math::BigNum Math::BigNum) => sub {
    my ($x, $y) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_bin_si($r, $r, CORE::int(Math::GMPq::Rmpq_get_d($$y)));
    _mpz2big($r);
};

multimethod binomial => qw(Math::BigNum $) => sub {
    my $r = _big2mpz($_[0]);
    Math::GMPz::Rmpz_bin_si($r, $r, CORE::int($_[1]));
    _mpz2big($r);
};

=head2 is_prime

    $n->is_prime                   # => Scalar
    $x->is_prime(BigNum)           # => Scalar
    $n->is_prime(Scalar)           # => Scalar

Returns 2 if $n is definitely prime, 1 if $n is probably prime (without
being certain), or 0 if $n is definitely composite. This method does some
trial divisions, then some Miller-Rabin probabilistic primality tests. It
also accepts an optional argument for specifying the accuracy of the test.
By default, it uses an accuracy value of 12, which guarantees correctness
up to C<2**78>.

See also: L<https://en.wikipedia.org/wiki/Miller–Rabin_primality_test>

=cut

multimethod is_prime => qw(Math::BigNum) => sub {
    Math::GMPz::Rmpz_probab_prime_p(_big2mpz($_[0]), 12);
};

multimethod is_prime => qw(Math::BigNum $) => sub {
    Math::GMPz::Rmpz_probab_prime_p(_big2mpz($_[0]), CORE::abs(CORE::int($_[1])));
};

multimethod is_prime => qw(Math::BigNum Math::BigNum) => sub {
    Math::GMPz::Rmpz_probab_prime_p(_big2mpz($_[0]), CORE::abs(CORE::int(Math::GMPq::Rmpq_get_d(${$_[1]}))));
};

=head2 next_prime

    $n->next_prime                 # => BigNum

Returns the next prime after $n.

=cut

sub next_prime {
    my ($x) = @_;
    my $r = _big2mpz($x);
    Math::GMPz::Rmpz_nextprime($r, $r);
    _mpz2big($r);
}

#
## Special methods
#

=head2 agm

    $x->agm(BigNum)                # => BigNum
    $x->agm(Scalar)                # => BigNum
    agm(Scalar, Scalar)            # => BigNum

Arithmetic-geometric mean of C<$x> and C<$y>.

=cut

multimethod agm => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_agm($r, $r, _big2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

multimethod agm => qw(Math::BigNum $) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_agm($r, $r, _str2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

multimethod agm => qw($ $) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_agm($r, $r, _str2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

=head2 hypot

    $x->hypot(BigNum)              # => BigNum
    $x->hypot(Scalar)              # => BigNum
    hypot(Scalar, Scalar)          # => BigNum

The value of the hypotenuse for catheti C<$x> and C<$y>. (C<sqrt($x**2 + $y**2)>)

=cut

multimethod hypot => qw(Math::BigNum Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_hypot($r, $r, _big2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

multimethod hypot => qw(Math::BigNum $) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_hypot($r, $r, _str2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

multimethod hypot => qw($ $) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_hypot($r, $r, _str2mpfr($_[1]), $ROUND);
    _mpfr2big($r);
};

=head2 gamma

    $x->gamma                      # => BigNum | Inf | Nan
    gamma(Scalar)                  # => BigNum | Inf | Nan

The Gamma function on C<$x>. Returns Inf when C<$x> is zero, and Nan when C<$x> is negative.

=cut

multimethod gamma => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_gamma($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod gamma => qw($) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_gamma($r, $r, $ROUND);
    _mpfr2big($r);
};

=head2 lngamma

    $x->lngamma                    # => BigNum | Inf
    lngamma(Scalar)                # => BigNum | Inf

The natural logarithm of the Gamma function on C<$x>.
Returns Inf when C<$x> is negative or equal to zero.

=cut

multimethod lngamma => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_lngamma($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod lngamma => qw($) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_lngamma($r, $r, $ROUND);
    _mpfr2big($r);
};

=head2 lgamma

    $x->lgamma                     # => BigNum | Inf
    lgamma(Scalar)                 # => BigNum | Inf

The logarithm of the absolute value of the Gamma function.
Returns Inf when C<$x> is negative or equal to zero.

=cut

multimethod lgamma => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_lgamma($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod lgamma => qw($) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_lgamma($r, $r, $ROUND);
    _mpfr2big($r);
};

=head2 digamma

    $x->digamma                    # => BigNum | Inf | Nan
    digamma(Scalar)                # => BigNum | Inf | Nan

The Digamma function (sometimes also called Psi).
Returns Nan when C<$x> is negative, and -Inf when C<$x> is 0.

=cut

multimethod digamma => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_digamma($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod digamma => qw($) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_digamma($r, $r, $ROUND);
    _mpfr2big($r);
};

=head2 zeta

    $x->zeta                       # => BigNum | Inf
    zeta(Scalar)                   # => BigNum | Inf

The zeta function on C<$x>. Returns Inf when C<$x> is 1.

=cut

multimethod zeta => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_zeta($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod zeta => qw($) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_zeta($r, $r, $ROUND);
    _mpfr2big($r);
};

=head2 erf

    $x->erf                        # => BigNum
    erf(Scalar)                    # => BigNum

The error function on C<$x>.

=cut

multimethod erf => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_erf($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod erf => qw($) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_erf($r, $r, $ROUND);
    _mpfr2big($r);
};

=head2 erfc

    $x->erfc                       # => BigNum
    erfc(Scalar)                   # => BigNum

Complementary error function on C<$x>.

=cut

multimethod erfc => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_erfc($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod erfc => qw($) => sub {
    my $r = _mpfr2big($_[0]);
    Math::MPFR::Rmpfr_erfc($r, $r, $ROUND);
    _mpfr2big($r);
};

=head2 eint

    $x->eint                       # => BigNum | Inf | Nan
    eint(Scalar)                   # => BigNum | Inf | Nan

Exponential integral of C<$x>. Returns -Inf when C<$x> is zero, and Nan when C<$x> is negative.

=cut

multimethod eint => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_eint($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod eint => qw($) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_eint($r, $r, $ROUND);
    _mpfr2big($r);
};

=head2 li2

    $x->li2                        # => BigNum
    li(Scalar)                     # => BigNum

The dilogarithm function, defined as the integral of C<-log(1-t)/t> from 0 to C<$x>.

=cut

multimethod li2 => qw(Math::BigNum) => sub {
    my $r = _big2mpfr($_[0]);
    Math::MPFR::Rmpfr_li2($r, $r, $ROUND);
    _mpfr2big($r);
};

multimethod li2 => qw($) => sub {
    my $r = _str2mpfr($_[0]);
    Math::MPFR::Rmpfr_li2($r, $r, $ROUND);
    _mpfr2big($r);
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
