#!/usr/bin/perl

use utf8;
use 5.006;
use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
                                 module_name        => 'Math::BigNum',
                                 license            => 'artistic_2',
                                 dist_author        => q{Daniel Șuteu <trizenx@gmail.com>},
                                 dist_version_from  => 'lib/Math/BigNum.pm',
                                 release_status     => 'unstable',
                                 configure_requires => {
                                                        'Module::Build' => 0,
                                                       },
                                 build_requires => {
                                                    'Test::More' => 0,
                                                   },
                                 requires => {
                                              'perl'           => '5.10.0',
                                              'Math::BigInt'   => 0,
                                              'Math::BigRat'   => 0,
                                              'Math::BigFloat' => 0,
                                              'Math::MPFR'     => 3.29,
                                              'Math::MPC'      => 0,
                                              'Math::GMPq'     => 0.39,
                                              'Math::GMPz'     => 0,
                                             },
                                 add_to_cleanup     => ['Math-BigNum-*'],
                                 create_makefile_pl => 'traditional',
                                );

$builder->create_build_script();
