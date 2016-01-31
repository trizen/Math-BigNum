package Math::BigNum::Nan;

use 5.010;
use strict;
use warnings;

use Math::GMPq qw();
use Math::MPFR qw();
use Math::BigNum qw();

use Class::Multimethods qw(multimethod);

=encoding utf8

=head1 NAME

Math::BigNum::Nan - Represents the Not-A-Number value.

=head1 VERSION

Version 0.01

=head1 SYNOPSIS

    use Math::BigNum;
    say Math::BigNum->nan;         # => "NaN"

    my $nan = Math::BigNum::Nan->new;
    say $nan != 0;                 # => 1

=head1 DESCRIPTION

Math::BigNum::Nan is an abstract type that represents C<NaN>.

=head1 SUBROUTINES/METHODS

=cut

use overload
  q{""} => \&stringify,
  q{0+} => \&numify,
  bool  => \&boolify,

  '=' => sub { $_[0]->copy },

  # Some shortcuts for speed
  '+='  => sub { $_[0] },
  '-='  => sub { $_[0] },
  '*='  => sub { $_[0] },
  '/='  => sub { $_[0] },
  '%='  => sub { $_[0] },
  '^='  => sub { $_[0] },
  '&='  => sub { $_[0] },
  '|='  => sub { $_[0] },
  '**=' => sub { $_[0] },
  '<<=' => sub { $_[0] },
  '>>=' => sub { $_[0] },

  '+'  => \&nan,
  '*'  => \&nan,
  '&'  => \&nan,
  '|'  => \&nan,
  '^'  => \&nan,
  '~'  => \&nan,
  '>>' => \&nan,
  '<<' => \&nan,

  '++' => sub { $_[0] },
  '--' => sub { $_[0] },

  eq  => sub { "$_[0]" eq "$_[1]" },
  ne  => sub { "$_[0]" ne "$_[1]" },
  cmp => sub { $_[2] ? "$_[1]" cmp $_[0]->stringify : $_[0]->stringify cmp "$_[1]" },

  '!='  => sub { 1 },
  '=='  => sub { },
  '>'   => sub { },
  '>='  => sub { },
  '<'   => sub { },
  '<='  => sub { },
  '<=>' => sub { },

  '**'  => \&nan,
  '-'   => \&nan,
  '/'   => \&nan,
  '%'   => \&nan,
  atan2 => \&nan,

  sin  => \&nan,
  cos  => \&nan,
  exp  => \&nan,
  log  => \&nan,
  int  => \&nan,
  abs  => \&nan,
  sqrt => \&nan;

sub new {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_ui($r, 0, 0);
    bless \$r, 'Math::BigNum::Nan';
}

BEGIN { *nan = \&new }

sub boolify   { }
sub stringify { 'NaN' }
sub numify    { 'NaN' + 0 }

*copy  = \&Math::BigNum::copy;
*inf   = \&Math::BigNum::inf;
*binf  = \&Math::BigNum::binf;
*bninf = \&Math::BigNum::bninf;

sub bnan {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_ui($$x, 0, 0);
    bless $x, __PACKAGE__;
    $x;
}

sub bone {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_ui($$x, 1, 1);
    bless $x, 'Math::BigNum';
    $x;
}

sub bzero {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_ui($$x, 0, 1);
    bless $x, 'Math::BigNum';
    $x;
}

sub bmone {
    my ($x) = @_;
    Math::GMPq::Rmpq_set_si($$x, -1, 1);
    bless $x, 'Math::BigNum';
    $x;
}

=head2 eq

    $x->eq($y)          # => Bool
    $x == $y            # => Bool

Equality test: always returns a false value.

=cut

sub eq { }

=head2 ne

    $x->ne($y)        # => Bool
    $x != $y          # => Bool

Inequality test: always returns a true value.

=cut

sub ne { 1 }

=head2 add / badd

    $x->add(Any)        # => Nan
    $x->badd(Any)       # => Nan

Always returns Nan.

=cut

sub add  { nan() }
sub badd { $_[0] }

=head2 sub / bsub

    $x->sub(Any)        # => Nan
    $x->bsub(Any)       # => Nan

Always returns Nan.

=cut

sub sub  { nan() }
sub bsub { $_[0] }

=head2 div / bdiv

    $x->div(Any)        # => Nan
    $x->bdiv(Any)       # => Nan

Always returns Nan.

=cut

sub div  { nan() }
sub bdiv { $_[0] }

1;
