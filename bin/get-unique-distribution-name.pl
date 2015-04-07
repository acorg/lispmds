#!/usr/bin/perl -w

BEGIN {
	use strict;
	$0 =~ s/.*\///;

	die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
		exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use strict;

my $today = `date '+%Y%m%d'`;
chomp($today);
my $base = $ENV{MDS_ROOT} . '/data/data/distributions/' . $today;
my @suffixes = ('', '.zip');
my $result = undef;

foreach (0 .. 25){
  my $insert = sprintf "%c", ord('a') + $_;
  my $exist = 0;
  for my $suffix (@suffixes) {
    if (-e "$base$insert$suffix") {
      $exist = 1;
      last;
    }
  }
  if (! $exist) {
    $result = $today . $insert;
    last;
  }
}

die "$0: could not find an available dir name letter to follow $base\n" unless defined $result;

print "$result\n";
exit(0);
