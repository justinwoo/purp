#!/usr/bin/perl

use warnings;
use strict;

print "running stack install\n";
print `stack install`;

my $stack_local_install_root = `stack path --local-install-root`;
chomp $stack_local_install_root;

my $bin_path = "$stack_local_install_root/bin/purp";

my $target_path = "purp";

print "copying built bin: $bin_path -> $target_path\n";
print `cp $bin_path $target_path`;

my $zip_path = "$ENV{TRAVIS_OS_NAME}.tar.gz";

print "zipping to $zip_path\n";
print `tar -zcvf $zip_path $target_path`;
