#!/usr/bin/perl -w

BEGIN {
	use strict;
	$0 =~ s/.*\///;

	die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
		exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use strict;
use lib "$ENV{MDS_ROOT}/perl";
use MDS::Report::RTF;

my $want_four_on_a_page           = 0;
my $want_four_on_a_page_frontpage = 0;
my $want_six_on_a_page            = 1;

my $blank_image = "$ENV{MDS_ROOT}/etc/blank-hi-table.png";

my $report = MDS::Report::RTF->new(file => $ARGV[0] || 'fred.doc',
								  title => "WHO Feb-2005 consultation on the compostion of influenza vaccine.");

die "$0: could not find blank HI image '$blank_image'.\n" unless -f $blank_image;

$report->open('paragraph', 'center');
$report->huge_skip_before();
$report->big_skip_after();
$report->huge();
$report->small_caps();
$report->printnl("Information for WHO consultation");
$report->printnl("on the composition of influenza vaccines");
$report->close_all();

$report->open('paragraph', 'center');
$report->large();
$report->big_skip_after();
$report->printnl("February 1, 2005 Teleconference");
$report->close_all();

$report->open('paragraph', 'center');
$report->large();
$report->small_caps();
$report->printnl("antigenic cartography report");
$report->close_all();

$report->open('paragraph', 'center');
$report->large();
$report->printnl("H3 from July 2002 to January 2005");
$report->close_all();


if ($want_six_on_a_page){
	for my $lab (qw(cdc melb niid nimr)){

		my @files = glob "$lab/*.png";

		if (@files){
			$report->main_page($lab, undef, filename => shift(@files));

			while (@files){
				my @these = splice(@files, 0, 6);
				$report->six_image_page(undef,
										filenames => \@these,
										blank_image => $blank_image);
			}
		}
		else {
			warn "$0: no image files found for lab $lab.\n";
		}
	}
}

if ($want_four_on_a_page){
	my @months = qw(January February March April May June July August September October November December);
	my %files;
	my $first = 1;

	for my $lab (qw(cdc melb nimr niid)){
		my @f = glob "$lab/*.png";
		$files{$lab} = \@f;
	}

	while (@{$files{cdc}} || @{$files{melb}} || @{$files{niid}} || @{$files{nimr}}){
		my @these = (shift @{$files{cdc}}, shift @{$files{melb}}, shift @{$files{niid}}, shift @{$files{nimr}});

		if ($first && ! $want_four_on_a_page_frontpage){
			$first = 0;
			next;
		}
		
		my $name = $these[0] || $these[1] || $these[2] || $these[3];
		die unless defined $name;
		# die "$0: file name '$name' can't be parsed" unless 

		$report->four_image_page($name =~ /\S+\/(\d\d\d\d)-(\d\d)\.png/ ? $1 . ' ' . $months[$2 - 1] : '',
								 filenames => \@these,
								 blank_image => $blank_image);
	}
}		 

exit(0);
