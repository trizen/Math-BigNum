package Math::BigNum::Nan;

use 5.010;
use strict;
use warnings;

use overload
  q{""} => sub { 'NaN' },
  q{0+} => sub { 'nan' };

use constant {
    NAN => do {
        my $r = Math::GMPq::Rmpq_init();
        Math::GMPq::Rmpq_set_ui($r, 0, 0);
        bless \$r, 'Math::BigNum::Nan';
    },
};

sub new { NAN }

1;
