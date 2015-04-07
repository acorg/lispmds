#!/usr/bin/perl -w

use strict;

$0 =~ s/.*\///;

my $count = 0;
my $tmp = "/tmp/$0.$$.tmp";
my $ps;
my $wish_exec = "./wish-expect";

open(TMP, ">$tmp") || die "$0: could not open tmp file '$tmp'.\n";

while (<>){
	if ($count == 2){
		if (/^\.c postscript -file (.*)/){
			$ps = $1;
			chomp($ps);
			last;
		}
		print TMP;
	}
	else {
		$count++ if /^\s*set\s+lisp_tk_stream_number/;
		print TMP if $count == 2;
	}
}
	
close(TMP) || die "$0: could not close tmp file '$tmp'.\n";

my $status = system("$wish_exec '$tmp' '$ps'") >> 8;

die "$0: invocation of '$wish_exec' exited with non-zero ($status) status.\n" if $status;
unlink($tmp) || die "$0: could not unlink tmp file '$tmp'.\n";

exit($status);
