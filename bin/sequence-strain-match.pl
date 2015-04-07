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

my $sequence_file;
my $strain_file;

sub usage {
    print STDERR "Usage: $0 -sequence-file file -strain-file file\n";
    exit(1);
}

GetOptions('sequence-file=s'   => \$sequence_file,
		   'strain-file=s'     => \$strain_file);

usage() if $Getopt::Long::error;

die "$0: you must give a sequence file via -sequence-file\n" unless defined $sequence_file;
die "$0: you must give a strain file via -strain-file\n" unless defined $strain_file;

my %sequence_typo =
	(
	 # These from NIMR. It doesn't matter which way we fix them, I can't see
	 # a match against any strain (poland or warsaw).
	 'POLAND/WARSAW/10/02'          => 'WARSAW/10/02',
	 'POLAND/WARSAW/6/02'           => 'WARSAW/6/02',
	 );

my %strain_typo =
	(
     # These don't help with any match (at least on the original CDC data),
	 # it just avoids errors as the left sides have 4 slashes.
	 'A/BEIJING/FANGSHAN/4/03'      => 'A/BEIJING-FANGSHAN/4/03',
	 'A/SW/MN/593/99'               => 'A/SW-MN/593/99',
	 'A/SW/ONT/41848/97'            => 'A/SW-ONT/41848/97',

	 # More things with 4 slashes.
	 'A/LYON/CHU/4011/03'           => 'A/LYON/4011/03',
	 'A/LYON/CHU/24648/03'          => 'A/LYON/24648/03',
	 'A/LYON/CHU/24722/03'          => 'A/LYON/24722/03',
	 'A/LYON/CHU/26367/03'          => 'A/LYON/26367/03',
	 'A/LYON/DESGENETTES/2210/03'   => 'A/LYON/2210/03',
	 'A/LYON/CHU/24381/03'          => 'A/LYON/24381/03',
	 'MD-D40/04 RG-A/WYOMING/03/03' => 'WYOMING/03/03',
	 'A/PHILIPPINES/472//02'        => 'A/PHILIPPINES/472/02',
	 'A/LYON/CHU/41523/04'          => 'A/LYON/41523/04',
	 'A/LYON/TRS/720/04'            => 'A/LYON/720/04',
	 'A/JIANGSU/FU/106/04'          => 'A/JIANGSU/106/04',

	 # 5 slashes
	 'A/WYOMING/03/03X A/PR/8'       => 'A/WYOMING/03/03 X-A-PR-8',
	 'A/WYOMING/03/03X A/PR/8 X-149' => 'A/WYOMING/03/03 X-A-PR-8-X-149',

	 # 6 slashes
	 'A/WYOMING/03/2003 X A/PR/8/34' => 'A/WYOMING/03/2003 X-A-PR-8-34',
	 );

my %sequence_location_corrections =
	('CC'                 => 'CHRISTCHURCH',
	 'FUKUOKAC'           => 'FUKUOKA-C',
	 'GERONA'             => 'GIRONA',
	 'GIFUC'              => 'GIFU',
	 'GUANZHOU'           => 'GUANGZHOU',
	 'HAMAMATU'           => 'HAMAMATU-C',
	 'HIROSHIMAC'         => 'HIROSHIMA-C',
	 'HUNNAN'             => 'HUNAN',
	 'KUMAMOTOC'          => 'KUMAMOTO-C',
	 'KYOTO'              => 'KYOTO-C',
	 'MAEHONGSON'         => 'MAE-HONG-SON',
	 'MAEHONGSORN'        => 'MAE-HONG-SON',
	 'MASSACHUSETTS'      => 'MASSACHUSETS',
	 'NAKHONRATCHASIMA'   => 'NAKHON-RATCHASIMA',
	 'NEPAL'              => 'NAPAL',
	 'NEWCALEDONIA'       => 'NEW-CALEDONIA',
	 'NEWJERSEY'          => 'NEW-JERSEY',
	 'NEWYORK'            => 'NEW-YORK',
	 'NIIGATAC'           => 'NIIGATA-C',
	 'NORTHCAROLINA'      => 'NORTH-CAROLINA',
	 'PALENCIA'           => 'VALENCIA',
	 'RUSSIA'             => 'USSR/RUSSIA',
	 'SENDAIH'            => 'SENDAI-H',
	 'SHIZUOKAC'          => 'SHIZUOKA-C',
	 'SOLOMONISLANDS'     => 'SOLOMON-ISLANDS',
	 'SOUTHAUSTRALIA'     => 'SOUTH-AUSTRALIA',
	 'SOUTHCAROLINA'      => 'SOUTH-CAROLINA',
	 'SOUTHDAKOTA'        => 'SOUTH-DAKOTA',
	 'ST.PETERSBURG'      => 'ST.-PETERSBURG');

# Take everything in the sequence file and attempt to match it
# against as many things as we can in the strain file.
#
# The sequence file has lines like
#
#   cdc24   BANGLADESH/1164729/03
#
# and the strain file has lines like this
#
#   FU/411/02 A/FUJIAN/411/02
#
# and we want to match the second field of the sequence file
# with the second of the strain file.

sub slurp_file($){
	my ($file) = @_;
	my @lines;
	open(F, $file) || die "$0: could not open '$file' ($!).\n";

	while (<F>){
		chomp;
		my ($first, $second) = split(' ', $_, 2);
		push @lines, [ uc($first), uc($second) ];
	}

	close(F) || die "$0: could not close '$file' ($!).\n";
	
	\@lines;
}

sub canonicalize_year($){
	my ($year) = @_;
	return '' if $year eq '';
	return $year - 2000 if $year >= 2000;
	return $year - 1900 if $year >= 1900;
	$year;
}

sub match($$$){
	my ($sequence_name, $strain_abbr, $strain_name) = @_;

	$strain_name = $strain_typo{$strain_name} if exists $strain_typo{$strain_name};
	$sequence_name = $sequence_typo{$sequence_name} if exists $sequence_typo{$sequence_name};
	
	substr($strain_name, -1, 1, '') if substr($strain_name, -1) =~ /[*\/]/;
	my ($sequence_location, $sequence_middle, $sequence_year) = split('/', $sequence_name, 3);
	my $sequence_letter = 'A';

	my $n_slashes = $strain_name =~ y/\//\//;
	my ($strain_letter, $strain_location, $strain_middle, $strain_year) = ('', '', '', '');

	if ($n_slashes == 3){
		($strain_letter, $strain_location, $strain_middle, $strain_year) = split('/', $strain_name);
	}
	elsif ($n_slashes == 2){
		($strain_letter, $strain_location, $strain_year) = split('/', $strain_name);
	}
	elsif ($n_slashes == 1){

		if ($strain_name =~ /^\s*(\S+)\s*\(A\/([^\)]+)\)\s*$/){
			# Something like IVR-134 (A/WYOM)
			$strain_location = $2;
			$strain_middle = $1;
		}
		else {
			($strain_location, $strain_year) = split('/', $strain_name);
		}
	}
	elsif ($n_slashes == 0){
		$strain_location = $strain_name;
	}
	else {
		die "$0: strain name '$strain_name' has $n_slashes slashes.\n";
	}

	#$sequence_location = $sequence_location_corrections{$sequence_location} if exists
	#$sequence_location_corrections{$sequence_location};

	$sequence_location =~ y/ -//d;
	$strain_location =~ y/ -//d;

	my $strain_rest;

	($strain_year, $strain_rest) = split(' ', $strain_year) if $strain_year ne '';
	$strain_rest = '' unless defined $strain_rest;

	if ($strain_year =~ /((?:original|box1|case|V-\d).*)/i){
		$strain_year = '';
		$strain_rest = $1 . ' ' . $strain_rest;
	}
	elsif ($strain_year =~ /^(\d+)([^\d].*)/){
		$strain_year = $1;
		$strain_rest = $2 . ' ' . $strain_rest;

		if (length($strain_year) > 4){
			$strain_rest = substr($strain_year, 4) . $strain_rest;
			substr($strain_year, 4) = '';
		}
	}

	$sequence_middle =~ s/^0+//;
	$strain_middle =~ s/^0+//;

	die "$0: undef sequence year in sequence '$sequence_name'.\n" unless defined $sequence_year;
	die "$0: undef strain year in strain '$strain_name' ($n_slashes slashes).\n" unless defined $strain_year;

	unless ($sequence_year =~ /^\d*$/){
		warn "non-numeric year in sequence '$sequence_name'. year = '$sequence_year'.\n";
		return 0;
	}

	unless ($strain_year =~ /^\d*$/){
		# warn "non-numeric year in strain '$strain_name'.\n";
		return 0;
	}

	$sequence_year = canonicalize_year($sequence_year);


	$strain_year = canonicalize_year($strain_year);

	return 1 if
		$sequence_letter eq $strain_letter &&
		$sequence_location eq $strain_location &&
		$sequence_middle eq $strain_middle &&
		$sequence_year == $strain_year;

	return 0;
}


my $sequences = slurp_file($sequence_file);
my $strains   = slurp_file($strain_file);

my $n_sequences = @$sequences;
my $n_strains = @$strains;

print ";; All strain names, given as abbreviation (longname):\n";

for (my $strain = 0; $strain < $n_strains; $strain++){
	my ($strain_abbr, $strain_name) = ($strains->[$strain]->[0], $strains->[$strain]->[1]);

	print ";;     $strain_abbr ($strain_name)\n";
}

my $n_matches = 0;

for (my $sequence = 0; $sequence < $n_sequences; $sequence++){
	my $matched = 0;
	my ($sequence_abbr, $sequence_name) = ($sequences->[$sequence]->[0], $sequences->[$sequence]->[1]);

	for (my $strain = 0; $strain < $n_strains; $strain++){
		my ($strain_abbr, $strain_name) = ($strains->[$strain]->[0], $strains->[$strain]->[1]);

		if (match($sequence_name, $strain_abbr, $strain_name)){
			$n_matches++ unless $matched; # Only count the first match of a sequence.
			$matched = 1;
			print "$sequence_abbr $sequence_name matches $strain_abbr ($strain_name)\n";
		}
	}

	print "Sequence $sequence_abbr ($sequence_name) matched no strains.\n" unless $matched;
}

print "\nMatched $n_matches/$n_sequences sequences.\n";

exit(0);
