#!/usr/bin/perl -w

use strict;
use Spreadsheet::ParseExcel;

# Prototypes
sub empty_row($$;$);
sub empty($$;$$$$);


for my $file (@ARGV){
	my $xls = Spreadsheet::ParseExcel::Workbook->Parse($file);

	unless ($xls){
		warn "$0: could not parse excel file '$file'.\n";
		next;
	}

	warn "$0: File '$file' has $xls->{SheetCount} worksheets. Trying to proceed.\n"
		unless $xls->{SheetCount} == 1;

	for my $sheet (@{$xls->{Worksheet}}){
		
		unless (defined $sheet->{MinRow} && defined $sheet->{MaxRow} &&
				defined $sheet->{MinCol} && defined $sheet->{MaxCol}){
			# warn "$0: Sheet $sheet->{Name} in file '$file' does not have both Min/Max Col defined.\n";
			# warn "$0: Sheet $sheet->{Name} in file '$file' does not have both Min/Max Row defined.\n";
			next;
		}

		# print STDERR "File '$file'; Sheet '", $sheet->{Name}, "'\n";

		my $row = $sheet->{MinRow};
		my $col = $sheet->{MinCol};

		for (; $row <= $sheet->{MaxRow}; $row++){
			printf("EMPTY ROW %d\n", $row + 1) if empty_row($sheet, $row);
			for ($col = $sheet->{MinCol}; $col <= $sheet->{MaxCol}; $col++){
				my $cell = $sheet->{Cells}[$row][$col];
				next unless $cell;
				# die unless $cell->{Val};
				
				if ($cell->{Type} eq 'Text'){
					printf("TEXT: (%d, %d) => '%s' = '%s'\n", $row + 1, $col + 1, $cell->{Val}, $cell->Value) if $cell->{Val} ne '';
				}
				elsif ($cell->{Type} eq 'Numeric') {
					printf("NUM:  (%d, %d) => %d = %s\n", $row + 1, $col + 1, $cell->{Val}, $cell->Value);
				}
				elsif ($cell->{Type} eq 'Date') {
					printf("DATE: (%d, %d) => '%s' = '%s'\n", $row + 1, $col + 1, $cell->{Val}, $cell->Value);
				}
				else {
					die;
				}
			
				
			}
		}
	}
}

sub empty_row ($$;$) {
	my ($sheet, $row, $ignore_empty_strings) = @_;
	return defined empty($sheet, $row, $ignore_empty_strings) ? 0 : 1;
}

sub empty ($$;$$$$) {
	my ($sheet, $from_row, $ignore_empty_strings, $to_row, $from_col, $to_col) = @_;
	die "$0: sheet undefined in empty()\n" unless defined $sheet;
	die "$0: from_row undefined in empty()\n" unless defined $from_row;
	$ignore_empty_strings = 1 unless defined $ignore_empty_strings;
	$to_row = $from_row unless defined $to_row;
	$from_col = $sheet->{MinCol} unless defined $from_col;
	$to_col = $sheet->{MaxCol} unless defined $to_col;

	for (my $row = $from_row; $row <= $to_row; $row++){
		for (my $col = $from_col; $col <= $to_col; $col++){
			my $cell = $sheet->{Cells}[$row][$col];

			return ($row, $col) if
				$cell && !($ignore_empty_strings && $cell->Value eq '');
		}
	}

	return undef;
}

exit(0);
