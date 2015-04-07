#!/usr/bin/perl -w

# Given a tag (ag or sr) to use to identify HI lines of interest
# (from one of our .msf (make save form) files, pull out the HI
# abbrev and the date. Don't get empty or 00000000 dates.

BEGIN {
    use strict;
    $0 =~ s/.*\///;

    die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
        exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use strict;
use lib "$ENV{MDS_ROOT}/perl";
use Getopt::Long;
use MDS::Misc;

my $BAD_DATE = '00000000';

my $tag               = ':ag';
my $show_file_name    = 0;
my $rm_ac_marker      = 0;
my $uniq              = 0;
my $sort              = 0;
my $variable          = 'date'; # The var=xxx variable whose value we want to pull out of the .msf file.

GetOptions('show-file-name!'     => \$show_file_name,
           'rm-ac-marker!'       => \$rm_ac_marker,
           'uniq!'               => \$uniq,
           'sort!'               => \$sort,
           'tag=s'               => \$tag,
           'variable=s'          => \$variable);

usage() if $Getopt::Long::error;
die "$0: can't sort unless uniq is also true.\n" if $sort && !$uniq;

my @lines;

if (@ARGV){
    $tag = ":$tag" unless substr($tag, 0, 1) eq ':';

    my $files = MDS::Misc::single_quote_list_for_shell(\@ARGV);
    
    @lines = `grep -H -e ' $tag ' $files`;
    chomp(@lines);
}
else {
    @lines = <>;
}

my %names;
my $e = 0;

for my $line (@lines){
    my ($file, $abbrev, $semicolons, $rest) = split(' ', $line, 4);
    die "$0: found abbrev '$abbrev' on line $. containing whitespace.\n" if $abbrev =~ /\s/;
    # Drop leading '( from the abbrev, if any.
    substr($abbrev, 0, 2, '') if substr($abbrev, 0, 2) eq "'(";
    # Drop trailing ) from the abbrev, if any.
    substr($abbrev, -1, 1, '') if substr($abbrev, -1) eq ")";
    # Drop the special uniquing marker
    $abbrev =~ s/-ac-(ag|sr)\d+// if $rm_ac_marker;
    # Drop trailing : from file.
    substr($file, -1, 1, '') if substr($file, -1) eq ":";

    next unless $rest =~ /\s:$variable=(\S+)/o;
    my $value = $1;

    next if $variable eq 'date' && $value eq $BAD_DATE;
    
    if ($uniq){
        if (exists $names{$abbrev}){
            unless ($names{$abbrev}->[0] eq $value){
                $e++;
                warn "$0: $e: found (and ignoring) abbrev '$abbrev' in file '$file' with $variable '$value' that does not match '$names{$abbrev}->[0]', previously seen in file '$names{$abbrev}->[1]'.\n";
            }
            ;
        }
        else {
            $names{$abbrev} = [ $value, $file ];
        }
    }
    else {
        print "$file\t" if $show_file_name;
        print "$abbrev\t$value\n";
    }
}

if ($uniq){
    for my $abbrev ($sort ?
                    sort { $names{$a}->[0] cmp $names{$b}->[0] } keys %names :
                    keys %names){
        
        print "$names{$abbrev}->[1]\t" if $show_file_name;
        print "$abbrev\t$names{$abbrev}->[0]\n";
    }
}


exit(0);

sub usage {
    print STDERR "Usage: $0 files OR cat *.msf | grep :ag | $0\n";
    exit(1);
}
