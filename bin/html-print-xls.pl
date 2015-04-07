#!/usr/bin/perl -w

BEGIN {
	use strict;
	$0 =~ s/.*\///;

	die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
		exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use strict;
use lib "$ENV{MDS_ROOT}/perl";
use Getopt::Long;
use MDS::Excel;

sub usage {
    print STDERR "Usage: $0 [-sheet-num n] file\n";
    exit(1);
}

my $sheet_num = 0;

&GetOptions('sheet-num=i' => \$sheet_num);

usage() if $Getopt::Long::error;
usage() unless @ARGV == 1;

my $xls = MDS::Excel->new($ARGV[0]);

$xls->html_print($sheet_num, 1);

exit(0);
