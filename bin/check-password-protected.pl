#!/usr/bin/perl -w

# Try to http get various web pages that are supposed to be
# password protected. Complain if any of them are fetched
# without error.

use strict;
use LWP::UserAgent;

my $page_display_bytes = 2000;
my $default_protocol = 'http';

my @pages = qw(www.santafe.edu/~dsmith/mds/index.html
	       www.santafe.edu/~asl/index.html
	       www.santafe.edu/~fouchier/mds/index.htm
	       www.antigenic-cartography.org/asl/index.html
	       www.antigenic-cartography.org/dsmith/mds/index.html
	       www.antigenic-cartography.org/dsmith/release/index.html
	       www.antigenic-cartography.org/fouchier/mds/index.html
	       www.antigenic-cartography.org/terry/mds/index.html
	       www.antigenic-cartography.org/nicola/index.html
	       www.antigenic-cartography.org/margaret/index.html
	       www.antigenic-cartography.org/eu/index.html
	       www.antigenic-cartography.org/colin/index.html
	       www.antigenic-cartography.org/bjorn/index.html
	       www.antigenic-cartography.org/dan/index.html
	       www.antigenic-cartography.org/pipermail/regression/2004-November/thread.html
	       www.antigenic-cartography.org/wiki/index.php/Main_Page
	       www.antigenic-cartography.org/wikis/regression/index.php/Main_Page);

my $ua = LWP::UserAgent->new;
my $error = 0;

for my $page (@pages) {
    $page = "$default_protocol://" . $page unless $page =~ m{^[^:/]+://};
    my $req = HTTP::Request->new(GET => $page);
    my $res = $ua->request($req);

    if ($res->is_success && $res->message eq 'OK') {
	print "Warning: $page is accessible without a password.\n";
	printf "Response: %s %s\n", $res->code, $res->message;

	my $page = $res->as_string;

	# Truncate if too long.
	$page = (substr($page, 0, $page_display_bytes) . "\n[Output truncated after $page_display_bytes bytes]\n") if
	  length($page) > $page_display_bytes;

	printf "Page contents = '%s'.\n", $page;
	$error = 1;
    }
}

exit($error);
