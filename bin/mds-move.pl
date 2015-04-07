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
use MDS::Classes;
use MDS::Misc;

my $NONE     = 'none';
my $options  = {};

my $default_options = {
    'read-format'  => 'text',
    'write-format' => 'text',
};


sub usage() {
    print STDERR "Usage: $0 -file filename [-store] [-read format] [-write format | $NONE] [-noabbrevs] [-config file | -noconfig] [-file file]\n";
    exit(1);
}

GetOptions($options, 'file=s', 'read=s', 'write=s', 'drop-rows-all-lt=i',
           'drop-rows-exact-lt=i', 'drop-cols-all-lt=i',
           'drop-cols-exact-lt=i', 'config-file=s', 'noconfig', 'nohtml',
           'noxml', 'store!');

usage() if $Getopt::Long::error;

MDS::Misc::set_defaults($options, $default_options);

my ($type, $date) = MDS::Misc::get_type_and_date_from_pwd();

# Read.

my $in_format = MDS::Classes::reader_class($type, $date);
my $read_pkg = "MDS::MatrixReader::$in_format";
eval "require $read_pkg" || die "$0: could not load $read_pkg ($@).\n";

my $n_tables;
my $name;

if ($in_format eq 'MELB'){
    $n_tables = 1;
    die unless exists $options->{file};
    $name = $options->{file};
    $name =~ s/\.xls$//;
}
else {
    my $reader = $read_pkg->new($options);
    $n_tables = $reader->n_tables();
    $name = $reader->name();
}

# Add xml production, unless it's already there or we've been told not to.
for my $html qw(xml){
    $options->{write} .= " $html" if exists $options->{write} &&
        $options->{write} !~ /\b$html\b/ &&
        ! exists $options->{noxml};
}

# Add html production, unless it's already there or we've been told not to.
for my $html qw(html htm){
    $options->{write} .= " $html" if exists $options->{write} &&
        $options->{write} !~ /\b$html\b/ &&
        ! exists $options->{nohtml};
}
	
my @out_formats = map { MDS::Classes::writer_class($_) } split(' ', $options->{write});

# Require all the appropriate perl output format classes.
for my $fmt (@out_formats){
    my $pkg = "MDS::MatrixWriter::$fmt";
    eval "require $pkg" || die "$0: could not load $pkg ($@).\n";
}

printf "%s%s table%s: ", ($name eq '' ? '' : "$name "), $n_tables, ($n_tables > 1 ? 's' : '');

for (my $table = 0; $table < $n_tables; $table++){
    my $reader2 = $read_pkg->new($options, $table);
    $reader2->process_options();

    # Write.

    printf "%d", $table + 1;

    for my $fmt (@out_formats){
        my $pkg = "MDS::MatrixWriter::$fmt";
        my $writer = $pkg->new($options, $reader2);
        $writer->process_options();
        $writer->write();
        printf ":%s", lc($fmt);
    }

    print $table == $n_tables - 1 ? '.' : ' ';
}

printf "\n";

exit(0);
