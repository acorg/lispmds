#!/usr/bin/perl -w

use strict;
use GD::Graph::lines;

my $graph = new GD::Graph::lines(800, 600);
my @data;
my @legend;

#
# The following attributes may be adjusted in the input, e.g. as
# ; title = blah
# Everything following the = will be taken as the value, with leading and
# trailing whitespace dropped.
#
my %attrs = (
	title             => 'Ag/Sr frequency over time',
	x_label           => 'Date',
	x_labels_vertical => 1,
	x_label_position  => 0.5,
	y_label           => 'Frequency',
	y_tick_number     => 10,
	y_max_value       => 1.0,
	x_all_ticks       => 1,
	x_first_date      => undef, # Must have the form 'month/year', e.g., 6/2005. Must be set in the input.
	x_interval        => 1,     # An integer number of months.
	lab               => undef, # The name of the lab. Must be set in the input.
);

# Attributes we'll zap after using them for our own purposes.
my @private_attrs = qw(x_first_date x_interval lab);

# printf STDERR "can%s do truetype fonts.\n", GD::Graph::can_do_ttf() ? '' : 'not';

while (<>){
	next if /^\s*$/;
	my $line = $_;
	chomp($line);

	if ($line =~ /(\S+)\s*=\s*(.*)/){
		# In-file graph attribute setting.
		my ($attr, $value) = (lc($1), $2);
		$value =~ s/\s+$//;
		$attrs{$attr} = $value;
		next;
	}

	my ($name, @values) = split(' ', $line);
	push @legend, $name;
	my $values = join(' ', @values);
	$values =~ tr/()//d;
	@values = split(' ', $values);
    # print "name => $name, values => $values\n";

	if (@values % 3 == 2 && $values[$#values - 1] eq '0' && $values[$#values] eq '0'){
		splice(@values, -2);
	}

	if (@values % 3){
		print STDERR "Input line '$line' (line num $.) does not have 0 mod 3 values. Skipped.\n";
		next;
	}

	my @fractions = grep { /\./ } @values;
	# printf STDERR "fractions => %s\n", join(' ', @fractions);

	unless (@data){
		push @data, x_labels($attrs{x_first_date}, $attrs{x_interval}, scalar(@fractions))
	}

	push @data, \@fractions;
}

# Put the lab name at the start of the title.
$attrs{title} = $attrs{lab} . ' ' . $attrs{title};

# Remove attributes that are unknown to GD.
map { delete $attrs{$_} } @private_attrs;

$graph->set(%attrs) || die $graph->error;
$graph->set_legend(@legend);
my $gd = $graph->plot(\@data);
print $gd->png();

exit(0);

sub x_labels {
	my ($first_date, $interval, $n) = @_;
	die "$0: first_date undefined" unless defined $first_date;
	die "$0: non-positive number of months" unless $n > 0;
	die "$0: could not parse first date '$first_date'" unless $first_date =~ /^(\d+)\/(\d+)$/;
	my ($month, $year) = ($1, $2);
	die "$0: bad month" unless $month >= 1 && $month <= 12;
	my @labels;
	my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
	
	do {
		push @labels, $months[$month - 1] . ' ' . $year;
		$month += $interval;

		if ($month > 12){
			$year++;
			$month -= 12;
		}
		
	} while (--$n > 0);

	\@labels;
}
