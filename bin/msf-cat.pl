#!/usr/bin/perl -w

use strict;
use Bio::AlignIO;
# use Getopt::Long;

for my $file (@ARGV){
	my $in = Bio::AlignIO->new(-file => $file, -format => 'msf');

	while (my $aln = $in->next_aln()){
		for my $s ($aln->each_seq){
			my $seq = $s->seq();
			#$seq =~ s/-+$//;
			printf "%s\t%s\n", $s->id(), uc($seq);
		}
	}
}

exit(0);
