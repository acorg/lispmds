#!/usr/bin/perl -w

BEGIN {
    use strict;
    $0 =~ s/.*\///;

    die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
      exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use lib "$ENV{MDS_ROOT}/perl";
use MDS::Misc;

my @msf = @ARGV;
my $n_tables = @msf;

map { print "Warning: MSF file '$_' is empty.\n" if -z $_ } @msf;


my $ag_ref = msf_process(':ag', \@msf);
my $n_ag = scalar(keys %$ag_ref);

my $sr_ref = msf_process(':sr', \@msf);
my $n_sr = scalar(keys %$sr_ref);

my $hi_ref = hi_process(\@msf);

print "Titer Summary\n\n\tTiter\tFrequency\n";

# Summarize HI values
my $n_hi = 0;

sub titer_cmp {
    my ($x, $y) = ($a, $b);
        
    if (substr($x, 0, 1) eq '<') {
        $x = substr($x, 1) - 0.1;
    }
    elsif (substr($x, 0, 1) eq '>') {
        $x = substr($x, 1) + 0.1;
    }
    elsif ($x eq '*') {
        $x = -1;
    }

    if (substr($y, 0, 1) eq '<') {
        die "$0: titer string '$y' is not followed by digits" unless substr($y, 1) =~ /^\d+/;
        $y = substr($y, 1) - 0.1;
    }
    elsif (substr($y, 0, 1) eq '>') {
        $y = substr($y, 1) + 0.1;
    }
    elsif ($y eq '*') {
        $y = -1;
    }

    $x <=> $y;
}

for my $titer (sort titer_cmp (keys %$hi_ref)) {
    printf "\t%6s  %6d\n", $titer, $hi_ref->{$titer};
    $n_hi += $hi_ref->{$titer};
}

# Summarize number of tables by lab

print "\nHI table summary by lab.\n\n";

my %tables;
my $total_tables = 0;
my $n_shipments = 0;

map { my @F = split('/', $_); $tables{$F[1]}->{$F[2]}++ } @msf;

for my $lab (sort keys %tables) {
    my $lab_total = 0;
    printf "Lab $lab, received %d shipments:\n", scalar(keys %{$tables{$lab}});
    $n_shipments += scalar(keys %{$tables{$lab}});
    for my $shipment (sort { $a <=> $b } keys %{$tables{$lab}}) {
        printf "\t$shipment\t%3d table%s\n", $tables{$lab}->{$shipment}, $tables{$lab}->{$shipment} > 1 ? 's' : '';
        $total_tables += $tables{$lab}->{$shipment};
        $lab_total += $tables{$lab}->{$shipment};
    }
    printf "\tTotal\t\t%3d tables\n\n", $lab_total;
}

die "$0: unequal table counts ($total_tables != $n_tables)" unless $total_tables == $n_tables;

printf "\n";

printf "%d HI tables.\n", $n_tables;
printf "%d HI titers.\n", $n_hi;
printf "%d sera.\n", $n_sr;
printf "%d antigens.\n", $n_ag;
printf "%d data shipments.\n", $n_shipments;


exit(0);

sub msf_process {
    my ($tag, $files) = @_;
    return unless @$files;
    my $rm_ac_marker = 1;
    my $rm_rowcol_marker = 1;
    my $quoted_files = MDS::Misc::single_quote_list_for_shell($files);
    # The -H in the grep prints the filename (this is omitted when there is only one file).
    my @lines = `grep -H ' $tag ' $quoted_files`;
    chomp(@lines);
    my %matches;

    for my $line (@lines) {
        my $error = 0;
        my ($file, $name, $semicolons, $rest) = split(' ', $line, 4);
        die "$0: found name '$name' on line $. containing whitespace.\n" if $name =~ /\s/;
        # Drop trailing : from file.
        substr($file, -1, 1, '') if substr($file, -1) eq ":";
        # Drop the special uniquing marker
        $name =~ s/-ac-(ag|sr)\d+// if $rm_ac_marker;
        $name =~ s/\s+\S+\s+\((?:row|col) \d+\)\s*$// if $rm_rowcol_marker;
        $matches{$name} = undef;
    }

    return \%matches;
}


sub hi_process {
    my ($files) = @_;
    return unless @$files;
    my $quoted_files = MDS::Misc::single_quote_list_for_shell($files);
    # The -H in the grep prints the filename (this is omitted when there is only one file).
    my @lines = `grep -H ' :hi ' $quoted_files`;
    chomp(@lines);
    my %hi;

    for my $line (@lines) {
        $line =~ s/^.*?://; # Drop file name.
        $line =~ s/;;.*//; # Drop comment.
        $line =~ tr/()//d; # Drop braces.
        map { $hi{$_}++ } split(' ', $line);
    }

    return \%hi;
}
