# Math-BigNum

Transparent interface to Math::GMPq, Math::GMPz, Math::MPFR and Math::MPC

# DESCRIPTION

Math::BigNum provides a correct, intuitive and transparent interface to the GMP, MPFR and MPC numerical libraries.

# SYNOPSIS

```perl
use 5.010;
use Math::BigNum qw(:constant);

# Integers and floating-points
say ((100->fac + 1) / 2);
  # prints: 46663107721972076340849619428133350245357984132190810734296481947608799996614957804470731988078259143126848960413611879125592605458432000000000000000000000000.5

# Rational numbers
my $x = 1/3;
say $x*3;         # prints: 1

# Complex numbers
say sqrt(-1);     # prints: i
```

Importing the `i` constant to explicitly create complex numbers:

```perl
use Math::BigNum qw(:constant i);

my $z = 3 + 4*i;
say $z;              # prints: "3+4i"
say $z + 2;          # prints: "5+4i"
```
