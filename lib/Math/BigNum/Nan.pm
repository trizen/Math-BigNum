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

  '==' => sub { $_[0]->eq($_[1]) },
  '!=' => sub { $_[0]->ne($_[1]) },

  '>'  => sub { },
  '>=' => sub { },
  '<'  => sub { },
  '<=' => sub { };

sub new {
    my $r = Math::GMPq::Rmpq_init();
    Math::GMPq::Rmpq_set_ui($r, 0, 0);
    bless \$r, 'Math::BigNum::Nan';
}

sub boolify   { }
sub stringify { 'NaN' }
sub numify    { 'NaN' }

=head2 eq

    $x->eq($y)          # => Bool
    $x == $y            # => Bool

Equality test: always returns a false value.

=cut

multimethod eq => qw(Math::BigNum::Nan Math::BigNum::Nan)     => sub { };    # should return true?
multimethod eq => qw(Math::BigNum::Nan Math::BigNum)          => sub { };
multimethod eq => qw(Math::BigNum::Nan Math::BigNum::Complex) => sub { };
multimethod eq => qw(Math::BigNum::Nan Math::BigNum::Inf)     => sub { };
multimethod eq => qw(Math::BigNum::Nan $)                     => sub { };

=head2 ne

    $x->ne($y)        # => Bool
    $x != $y          # => Bool

Inequality test: always returns a true value.

=cut

multimethod ne => qw(Math::BigNum::Nan Math::BigNum::Nan)     => sub { 1 };    # should return false?
multimethod ne => qw(Math::BigNum::Nan Math::BigNum)          => sub { 1 };
multimethod ne => qw(Math::BigNum::Nan Math::BigNum::Complex) => sub { 1 };
multimethod ne => qw(Math::BigNum::Nan Math::BigNum::Inf)     => sub { 1 };
multimethod ne => qw(Math::BigNum::Nan $)                     => sub { 1 };

1;
