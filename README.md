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

# INSTALLATION

To install this module, run the following commands:

    perl Build.PL
    ./Build
    ./Build test
    ./Build install

# SUPPORT AND DOCUMENTATION

After installing, you can find documentation for this module with the
perldoc command.

    perldoc Math::BigNum

You can also look for information at:

    RT, CPAN's request tracker (report bugs here)
        http://rt.cpan.org/NoAuth/Bugs.html?Dist=Math-BigNum

    AnnoCPAN, Annotated CPAN documentation
        http://annocpan.org/dist/Math-BigNum

    CPAN Ratings
        http://cpanratings.perl.org/d/Math-BigNum

    Search CPAN
        http://search.cpan.org/dist/Math-BigNum/


# LICENSE AND COPYRIGHT

Copyright (C) 2016 Daniel Șuteu

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

http://www.perlfoundation.org/artistic_license_2_0

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
