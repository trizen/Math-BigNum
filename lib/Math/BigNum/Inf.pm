package Math::BigNum::Inf;

use 5.010;
use strict;
use warnings;

no warnings qw(qw);
use Math::GMPq qw();

use overload
  q{""} => sub { 'inf' },
  q{0+} => sub { +'inf' };

use constant {
    INF => do {
        my $r = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set_ui($r, 1, 0);
        bless \$r, __PACKAGE__;
      }
};

sub new { INF }

1;
