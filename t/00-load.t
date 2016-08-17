#!perl -T
use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    use_ok('Math::BigNum') || print "Bail out!\n";
}

diag("Testing Math::BigNum $Math::BigNum::VERSION, Perl $], $^X");

warn "-" x 40, "\n";
warn "# INT_MIN  : ", Math::GMPq::_int_min(),   "\n";
warn "# INT_MAX  : ", Math::GMPq::_int_max(),   "\n";
warn "# UINT_MAX : ", Math::GMPq::_uint_max(),  "\n";
warn "# LONG_MIN : ", Math::GMPq::_long_min(),  "\n";
warn "# LONG_MAX : ", Math::GMPq::_long_max(),  "\n";
warn "# ULONG_MAX: ", Math::GMPq::_ulong_max(), "\n";
warn "-" x 40, "\n";
