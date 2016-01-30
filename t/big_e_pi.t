#!perl

use strict;
use warnings;

use Test::More tests => 4;

use Math::BigNum qw(e PI);    # import 'e' and 'PI'

my $e  = Math::BigNum->e;
my $pi = Math::BigNum->pi;

is(e,  $e);
is(PI, $pi);

ok("$e" =~ /^2\.71828182/);
ok("$pi" =~ /^3\.14159265/);
