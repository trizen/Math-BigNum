#!perl -T
use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Math::BigNum' ) || print "Bail out!\n";
}

diag( "Testing Math::BigNum $Math::BigNum::VERSION, Perl $], $^X" );
