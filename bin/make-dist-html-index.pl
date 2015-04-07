#!/usr/bin/perl -w

BEGIN {
    use strict;
    $0 =~ s/.*\///;

    die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
      exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use lib "$ENV{MDS_ROOT}/perl";
use MDS::Misc;
use Getopt::Long;

my $show_shipments = 0;

sub usage() {
    print STDERR "Usage: $0 -show-shipments\n";
    exit(1);
}

GetOptions('show-shipments!' => \$show_shipments,
	  );

usage() if $Getopt::Long::error;

my @msf = @ARGV;
my $n_tables = @msf;

map { print "Warning: MSF file '$_' is empty.\n" if -z $_ } @msf;

my $ag_ref = msf_process(':ag', \@msf);
my $n_ag = scalar(keys %$ag_ref);

my $sr_ref = msf_process(':sr', \@msf);
my $n_sr = scalar(keys %$sr_ref);

print "<html>
<style type=\"text/css\">
body { 
    color: black; 
    background: white;
    margin-left: 2%;
    margin-right: 2%;
}
body,td {
    font-family: Georgia,Arial,Tahoma,Verdana,Geneva,sans-serif; 
}

h1,h2,h3,h4,h5 { 
    font-family: Garamond,Arial,\"Times New Roman\",sans-serif; 
}
th {
    border: 1px solid;
    background-color: #aaaaff;
    padding: 5px;
}

td {
    border: 1px solid;
    padding: 5px;
    vertical-align: top;
    /*
    writing-mode: tb-rl;
    filter: flipv fliph;
    */
}

table {
    border-collapse: collapse;
    border: 1px solid;
    border-width: 1px 1px;
    # border-color: #ccccaa;
    # padding: 3px;
}

</style>
<head><title>H3N2 HI table summary</title></head><body>
";

# Summarize number of tables by lab

my %tables;
my $total_tables = 0;
my $n_shipments = 0;

my $out = '';

map { my @F = split('/', $_); $tables{$F[1]}->{$F[2]}++ } @msf;

for my $lab (sort keys %tables) {
    my $lab_total = 0;
    my $labout = '';
    if ($show_shipments){
        $n_shipments += scalar(keys %{$tables{$lab}});
    }
    $labout .= "<table>\n";
    if ($show_shipments){
        $labout .= "<tr><td>Shipment<br>Date</td><td>#&nbsp;Tables</td><td>Table links</td></tr>\n";
    }
    else {
        $labout .= "<tr><td>";
    }
    for my $shipment (sort { $a <=> $b } keys %{$tables{$lab}}) {
        if ($show_shipments){
            $labout .= sprintf "<tr><td>$shipment</td><td>%d</td>\n", $tables{$lab}->{$shipment};
        }
        $total_tables += $tables{$lab}->{$shipment};
        $lab_total += $tables{$lab}->{$shipment};
        if ($show_shipments){
            $labout .= "<td>\n";
        }
        my @tables = glob("labs/$lab/$shipment/*.htm");
        for my $table (@tables){
            $base = $table;
            $base =~ s:labs/$lab/$shipment/(.+)\.htm$:$1:;
            $labout .= "<a href=\"$table\">$base</a>\n";
        }
        if ($show_shipments){
            $labout .= "</td></tr>\n";
        }
    }
    if (!$show_shipments){
        $labout .= "</td></tr>";
    }
    $labout .= "</table>\n";
    $out .= "<h2>$lab summary</h2><p>Received $lab_total H3N2 HI tables";
    if ($show_shipments){
        $out .= sprintf ", in %d data shipments.</p>\n", scalar(keys %{$tables{$lab}});
    }
    $out .= $labout;
}

die "$0: unequal table counts ($total_tables != $n_tables)" unless $total_tables == $n_tables;

print "<h1>H3N2 HI table summary by laboratory</h1>";

printf "%d HI tables.<br>\n", $n_tables;
printf "%d sera.<br>\n", $n_sr;
printf "%d antigens.<br>\n", $n_ag;

if ($show_shipments){
    printf "%d data shipments.<br>\n", $n_shipments;
}

print $out;

printf "</p></body></html>\n";

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
