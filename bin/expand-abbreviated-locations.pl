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

my $countryLocationsFile = "$ENV{MDS_ROOT}/data/data/cdc/meta/country-locations.csv";
my $stateLocationsFile = "$ENV{MDS_ROOT}/data/data/cdc/meta/state-locations.csv";
my $locationAbbrFile = "$ENV{MDS_ROOT}/data/data/summary/locations.by-name";
my %cdcLocations;
my %locations;
my $doCDC = 0;

&GetOptions('country-file=s' => \$countryLocationsFile,
            'state-file=s' => \$stateLocationsFile,
            'location-file=s' => \$locationAbbrFile);

usage() if $Getopt::Long::error;

open(CL, $countryLocationsFile) or die;

while (<CL>){
    next if $. == 1;
    my @columns = map { $_ =~ s/^\s+//; $_ =~ s/\s+$//; $_; } split(',', $_);
    my $location = uc($columns[0]);
    my $abbrev = uc($columns[2]);

    if (length($abbrev) == 2){
        if (exists $cdcLocations{$abbrev}){
            print STDERR "Duplicate abbrev found for '$abbrev'.\n";
            die unless $cdcLocations{$abbrev} eq $location;
        }
        else {
            $cdcLocations{$abbrev} = $location;
        }
    }
    else {
        print STDERR "Got non-2-length abbrev ($abbrev) on line $. of $countryLocationsFile\n" if length($abbrev);
    }
}

close(CL) or die;

# State locations.
open(SL, $stateLocationsFile) or die;

while (<SL>){
    next if $. == 1;
    my @columns = map { $_ =~ s/^\s+//; $_ =~ s/\s+$//; $_; } split(',', $_);
    my $location = uc($columns[0]);
    next unless length($location);
    my $abbrev = uc($columns[2]);

    if (length($abbrev) == 2){
        if (exists $cdcLocations{$abbrev}){
            print STDERR "Duplicate abbrev found for '$abbrev' ($cdcLocations{$abbrev} and $location).\n";
            die unless $cdcLocations{$abbrev} eq $location;
        }
        else {
            # print "State '$location' = '$abbrev'.\n";
            $cdcLocations{$abbrev} = $location;
        }
    }
    else {
        print STDERR "Got non-2-length abbrev ($abbrev) on line $. of $stateLocationsFile\n" if length($abbrev);
    }
}

close(SL) or die;

# State locations (16th field).
open(SL, $stateLocationsFile) or die;

while (<SL>){
    next if $. == 1;
    my @columns = map { $_ =~ s/^\s+//; $_ =~ s/\s+$//; $_; } split(',', $_);
    my $location = uc($columns[0]);
    next unless length($location);
    my $abbrev = uc($columns[16]);

    if (length($abbrev) == 2){
        unless (exists $cdcLocations{$abbrev}){
            $cdcLocations{$abbrev} = $location;
        }
    }
}

close(SL) or die;

# Our locations.
open(L, $locationAbbrFile) or die;

while (<L>){
    next if $. == 1;
    my @columns = map { $_ =~ s/^\s+//; $_ =~ s/\s+$//; $_; } split('=', $_);
    my $location = uc($columns[0]);
    my $abbrev = uc($columns[1]);

    if (length($abbrev) != 2){
        print STDERR "Got non-2-length abbrev ($abbrev) on line $. of $locationAbbrFile\n" if length($abbrev);
        exit(1);
    }

    if (exists $locations{$abbrev}){
        print STDERR "Duplicate abbrev found for '$abbrev'.\n";
        if ($locations{$abbrev} ne $location){
            print STDERR "$locations{$abbrev} != $location. ";
            if (length($location) > length($locations{$abbrev})){
                $locations{$abbrev} = $location;
            }
            print STDERR "Going with $locations{$abbrev}\n";
        }
    }
    else {
        $locations{$abbrev} = $location;
    }
}

close(L) or die;

while (my $line = <>){
    chomp($line);
    $line =~ s/ac-(ag|sr)\d\d//i;
    my $first = uc(substr($line, 0, 1));
    my $second = uc(substr($line, 1, 1));
    my $third = uc(substr($line, 2, 1));
    my $first2 = uc(substr($line, 0, 2));

    if ($doCDC && $third eq '-'){
        # This is a CDC name.
        if (exists $cdcLocations{$first2}){
            printf "$line $cdcLocations{$first2}%s\n", substr($line, 2);
        }
        else {
            printf STDERR "$line is CDC-line, but no abbrev found for '%s'.\n", $first2;
            printf "$line $line\n";
        }
    }
    elsif ($third eq '/'){
        if (exists $locations{$first2}){
            printf "$line $locations{$first2}%s\n", substr($line, 2);
        }
        else {
            printf STDERR "$line is designation-line, but no abbrev found for '%s'.\n", $first2;
            printf "$line $line\n";
        }
    }
    elsif ($first eq 'A' && $second eq '/'){
        printf "$line $line\n";
    }
    else {
        printf STDERR "UNKNOWN: $line\n";
        printf "$line $line\n";
    }
}

sub usage {
    print STDERR "Usage: $0 [-country-file file] [-state-file file] [-location-file file] < abbrs\n";
    exit(1);
}
