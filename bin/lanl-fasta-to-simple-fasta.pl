#!/usr/bin/perl -w

BEGIN {
	use strict;
	$0 =~ s/.*\///;

	die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
		exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use strict;
use lib "$ENV{MDS_ROOT}/perl";
use MDS::Config;
use MDS::Location;
use MDS::DB;
use Getopt::Long;
use Bio::SeqIO;

my $HA_LENGTH = 987;
my $start_base = 1;
my $sequence_len = $HA_LENGTH;
my $verbose = 0;
my $skip_incomplete = 0;
my $config_file;
my $db_name;

my $n_ok_sequences = 0;
my $n_short_sequences = 0;
my $n_incomplete_sequences = 0;

sub usage() {
  print STDERR "Usage: $0 [-start-base S] [-length L] [-config-file file] [-db-name db] < file\n";
  exit(1);
}

GetOptions('start-base=i' => \$start_base,
	   'length=i'     => \$sequence_len,
	   'verbose!'     => \$verbose,
	   'config-file=s' => \$config_file,
	   'db-name=s'     => \$db_name
	  );

usage() if $Getopt::Long::error;

my $config = MDS::Config->new($config_file);

$db_name = 'default' unless defined $db_name;
my $db = $config->{config}->{$db_name};

die sprintf("$0: cannot find database '$db_name' in your config file '%s'",
			$config->{file}) unless $db;

$db = MDS::DB::->new($db);
my $locations = MDS::Location->new($db);

my $in = Bio::SeqIO->new(-fh => \*STDIN, -format => 'fasta');
my $out = Bio::SeqIO->new(-fh => \*STDOUT, -format => 'fasta');

while (my $s = $in->next_seq()) {
    my $id = $s->desc();

    if ($id =~ /^(.*)\s+H3(N2)?\s/i){
	$id = $locations->abbreviate_designation($1);
	$id =~ s/\s+\(egg passaged|reassortant\)$//;
	$id =~ s/\s+\(//;

	# Zap initial A/
	substr($id, 0, 2, '') if $id =~ /^a\//i;

	# Change year to have two digits.
	if ($id =~ /(\d\d)\d\d$/){
	    substr($id, -4, 2, '') if $1 == 19 || $1 == 20;
	}

	# Watch for problems in which the year has whitespace after the final slash
	# and before the year.
	$id =~ s:/\s+(\d+)$:/$1:;
    }
    else {
	printf STDERR "$0: skipping strain with unusual name '%s'.\n", $id;
	next;
    }

    if ($s->length() < $start_base + $sequence_len - 1) {
	warn sprintf("$0: sequence '%s' is too short, ignored.\n", $s->display_id) if $verbose;
	$n_short_sequences++;
	next;
    }

    $s->seq(uc($s->subseq($start_base, $start_base + $sequence_len - 1)));

    die sprintf("$0: unexpected incorrect length in truncated sequence '%s'.\n", $s->display_id)
      unless $s->length() == $sequence_len;

    if ($skip_incomplete && $s->seq !~ /^[ACGT]*$/) {
	if ($verbose) {
	    warn sprintf("$0: sequence '%s' is not composed of ACGT, ignoring.\n", $s->display_id);
	}
	$n_incomplete_sequences++;
	next;
    }

    my $seqobj = Bio::Seq->new(-seq => $s->seq,
			       -display_id  => $id,
			      );
    
    $out->write_seq($seqobj);
}

exit(0);
