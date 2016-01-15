package Math::BigNum::Ninf;

use 5.010;
use strict;
use warnings;

use overload
  q{""} => sub { '-inf' },
  q{0+} => sub { -'inf' };

use constant {
    NINF => do {
        my $r = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set_ui($r, -1, 0);
        bless \$r, 'Math::BigNum::Ninf';
    },
};

sub new { NINF }

1;
