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
    print STDERR "Usage: $0 [-sheet n] [-noignore-empty-cells] file\n";
    exit(1);
}

my $sheet_num = 0;
my $ignore_empty_cells = 1;

&GetOptions('sheet=i' => \$sheet_num,
			'ignore-empty-cells!' => \$ignore_empty_cells);

usage() if $Getopt::Long::error;
usage() unless @ARGV == 1;

my $ss = MDS::Excel->new($ARGV[0]);

my $sheet = $ss->get_sheet($sheet_num);
die "$0: could not get sheet $sheet_num.\n" unless defined $sheet;

my ($from_row, $from_col, $to_row, $to_col) = $ss->sheet_bounds($sheet, $ignore_empty_cells);

print "Sheet bounds:\nrow min = $from_row, col min = $from_col\nrow max = $to_row, col max = $to_col\n";

die "$0: sheet number $sheet_num in file '$ARGV[0]' does not define all of MinRow, MaxRow, etc."
	unless defined $from_row;

for (my $row = $from_row; $row <= $to_row; $row++){

	for (my $col = $from_col; $col <= $to_col; $col++){

		my $cell = $sheet->{Cells}[$row][$col];
		my $label = $ss->col_index_to_col_label($col);

		#print "looking at $row $col. ig = $ignore_empty_cells\n";
		if ($cell){

			if ($cell->{Type} eq 'Text'){
				#print "here\n";
				if ($ss->empty_cell($cell, $ignore_empty_cells)){
					printf("TEXT: (%d, %s/%d) => ''\n", $row + 1, $label, $col + 1)
						unless $ignore_empty_cells;
				}
				else {
					printf("TEXT: (%d, %s/%d) => '%s'\n", $row + 1, $label, $col + 1, $cell->{Val});
				}
			}
			elsif ($cell->{Type} eq 'Numeric') {
				printf("NUM:  (%d, %s/%d) => %d\n", $row + 1, $label, $col + 1, $cell->{Val});
			}
			elsif ($cell->{Type} eq 'Date') {
				printf("DATE: (%d, %s/%d) => '%s'\n", $row + 1, $label, $col + 1, $cell->Value);
			}
			else {
				die;
			}
		}
		else {
			printf("NULL: (%d, %s/%d)\n", $row + 1, $label, $col + 1) unless $ignore_empty_cells;
		}
	}
}
	 

exit(0);
