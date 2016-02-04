#!/usr/bin/perl

# RSA Encryption example by Phil Massyn (www.massyn.net)
# July 10th 2013

use strict;
use lib qw(../lib);
use Math::BigNum qw(:constant);
use Math::Prime::Util ':all';

# == key generation
my $p = Math::BigNum->new(random_strong_prime(256));    # Note that BigNum understands BigInt
my $q = Math::BigNum->new(random_strong_prime(256));

my $n = $p * $q;

my $phi = ($p - 1) * ($q - 1);

my $e = 257;                                            # need to figure out how to calculate it

my $x = $e**1;
my $d = $x->modinv($phi);

# == encryption
my $message = "hello world";
my $m = (join('', map (sprintf("%03d", ord), split(//, $message)))) + 1 - 1;

my $c = $m->modpow($e, $n);

# == decryption
my $M = $c->modpow($d, $n);

print join('', map(sprintf("%s", chr), ($M =~ /\d{3}/g))) . "\n";

exit(0);