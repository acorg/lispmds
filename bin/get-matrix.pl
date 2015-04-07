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
use MDS::DB;

my $matrix;

&GetOptions('matrix=i' => \$matrix);

usage() if $Getopt::Long::error;
usage() unless defined $matrix;

sub usage {
    print STDERR "Usage: $0 -matrix n\n";
    exit(1);
}

my $db = MDS::DB->new();
$db->read(1);

exit(0);
