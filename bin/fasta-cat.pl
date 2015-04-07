#!/usr/bin/perl -w

BEGIN {
    use strict;
    $0 =~ s/.*\///;

    die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
      exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use strict;
use lib "$ENV{MDS_ROOT}/perl";
use Bio::AlignIO;

sub usage() {
    print STDERR "Usage: $0 file.fas\n";
    exit(1);
}

for my $file (@ARGV) {
    my $in = Bio::AlignIO->new(-file => $file, -format => 'fasta');

    while (my $aln = $in->next_aln()) {
	for my $s ($aln->each_seq) {
	    printf "%s\t%s\n", $s->id(), $s->seq();
	}
    }
}

exit(0);
