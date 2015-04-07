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
use MDS::Misc;

my $tag               = ':ag';
my $show_file_name    = 0;
my $rm_ac_marker      = 0;
my $rm_rowcol_marker  = 0;
my $uniq              = 0;
my $sort              = 0;
my $fiddle_long_names = 0;
        
GetOptions('show-file-name!'     => \$show_file_name,
           'rm-ac-marker!'       => \$rm_ac_marker,
           'rm-rowcol-marker!'   => \$rm_rowcol_marker,
           'uniq!'               => \$uniq,
           'fiddle-long-names!'  => \$fiddle_long_names,
           'sort!'               => \$sort,
           'tag=s'               => \$tag);

usage() if $Getopt::Long::error;
die "$0: can't sort unless uniq is also true.\n" if $sort && !$uniq;

my @lines;

if (@ARGV) {
    $tag = ":$tag" unless substr($tag, 0, 1) eq ':';

    my $files = MDS::Misc::single_quote_list_for_shell(\@ARGV);
        
    @lines = `grep -H -e ' $tag ' $files`;
}
else {
    @lines = <>;
}

chomp(@lines);

my %names;
my $e = 0;

for my $line (@lines) {
    my ($file, $abbrev, $semicolons, $rest) = split(' ', $line, 4);
    die "$0: found abbrev '$abbrev' on line $. containing whitespace.\n" if $abbrev =~ /\s/;
    # Drop leading '( from the abbrev, if any.
    substr($abbrev, 0, 2, '') if substr($abbrev, 0, 2) eq "'(";
    # Drop trailing ) from the abbrev, if any.
    substr($abbrev, -1, 1, '') if substr($abbrev, -1) eq ")";
    # Drop the special uniquing marker
    $abbrev =~ s/-ac-(ag|sr)\d+// if $rm_ac_marker;
    $rest =~ s/\s+\Q$tag\E.*//;
    $rest =~ s/\s+\S+\s+\((?:row|col) \d+\)\s*$// if $rm_rowcol_marker;
    # Drop trailing : from file.
    substr($file, -1, 1, '') if substr($file, -1) eq ":";

    if ($fiddle_long_names) {
        $rest =~ s/hong[ -]kong/HONGKONG/i;
        $rest =~ s/a\/hk\//A\/HONGKONG\//i;
        $rest =~ s/\(?new\)?/NEW/i;
        $rest =~ y/ /-/;
        $rest =~ y/*//d;
    }

    if ($rest =~ m|/(\d\d)\d\d$|) {
        substr($rest, -4, 2, '') if $1 == 19 || $1 == 20;
    }
        
    if ($uniq) {
        if (exists $names{$abbrev}) {
            unless ($names{$abbrev}->[0] eq $rest) {
                $e++;
                warn "$0: $e: found abbrev '$abbrev' in file '$file' with name '$rest' that does not match '$names{$abbrev}->[0]', previously seen in file '$names{$abbrev}->[1]'.\n";
            }
            ;
        }
        else {
            $names{$abbrev} = [ $rest, $file ];
        }
    }
    else {
        print "$file " if $show_file_name;
        print "$abbrev $rest\n";
    }
}

if ($uniq) {
    for my $abbrev ($sort ?
                    sort { $names{$a}->[0] cmp $names{$b}->[0] } keys %names :
                    keys %names) {
                
        print "$names{$abbrev}->[1] " if $show_file_name;
        print "$abbrev $names{$abbrev}->[0]\n";
    }
}


exit(0);

sub usage {
    print STDERR "Usage: $0 files OR cat *.msf | grep :ag | $0\n";
    exit(1);
}
