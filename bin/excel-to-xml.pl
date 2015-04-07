#!/usr/bin/perl -w

use strict;

use XML::Excel;

my $excel_obj = XML::Excel->new();
# $excel_obj = XML::Excel->new(\%attr);

my $status = $excel_obj->parse_doc($ARGV[0]);
# $status = $excel_obj->parse_doc(file_name, \%attr);

# $excel_obj->declare_xml(\%attr);
# $excel_obj->declare_doctype(\%attr);

$excel_obj->print_xml("fred.xml");

exit(0);
