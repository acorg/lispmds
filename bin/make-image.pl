#!/usr/bin/perl -w

BEGIN {
	use strict;
	$0 =~ s/.*\///;

	die "$0: non-existent or invalid MDS_ROOT environment variable.\n" unless
		exists $ENV{MDS_ROOT} && -d $ENV{MDS_ROOT};
}

use lib "$ENV{MDS_ROOT}/perl";
use Getopt::Long;
use IO::File;
use GD;
use GD::Polyline;
use MDS::Misc;

my $default_height       = 530;
my $default_width        = 530;
my $default_small_height = 300;
my $default_small_width  = 300;
my $suffix               = '.png';
my %colors;
my $rgb;
my $small_width;
my $small_height;

# Command line options
my $batch_file;
my $transparent          = 1;
my $background_color     = 'white';
my $png_compression      = 2; # 0 is best/biggest.
my $save_alpha           = 0;
my $interlaced           = 1;
my $true_color           = 0;
my $font_file            = "$ENV{MDS_ROOT}/etc/arialuni.ttf";
my $make_small           = 1;
my $small_suffix         = '-small';
my $small_dimensions;

sub usage(;$) {
	my ($msg) = @_;
	print STDERR "$0: $msg\n" if defined $msg;
    print STDERR "Usage: $0 [-batch-file file ] [filenames]\n";
    exit(1);
}

sub new_image(;$$){
	my ($width, $height) = @_;
	$width ||= $default_width;
	$height ||= $default_height;
	GD::Image->trueColor(1) if $true_color;
	my $im = new GD::Image($width, $height);
	%colors = ();
	my $bg = allocate_color($im, $background_color);
	$im->transparent($bg) if $transparent;
	$im->interlaced(1) if $interlaced;

	if ($save_alpha){
		$im->saveAlpha(1);
		$im->alphaBlending(0);
	}
	
	$im;
}

sub allocate_color($$){
	my ($im, $color) = @_;
	return $colors{$color} if exists $colors{$color};

	my $new;
	
	if ($color =~ /^\#([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})$/){
		$new = $im->colorAllocate(hex($1), hex($2), hex($3));
	}
	else {
		$color = lc($color);
		$rgb = MDS::Misc::read_rgb() unless defined $rgb;
		die "$0: could not locate color '$color' in RGB hash.\n" unless exists $rgb->{$color};
		my ($r, $g, $b) = ($rgb->{$color}->[0], $rgb->{$color}->[1], $rgb->{$color}->[2]);
		$new = $im->colorAllocate($r, $g, $b);
	}

	die "failed to allocate color '$color'.\n" if $new == -1;

	$colors{$color} = $new;
	return $new;
}

sub parse_line($$){
	my ($im, $line) = @_;
	
	if ($line =~ /^\s*wm\s+geometry\s+\.\s+(\d+)x(\d+)\s*$/){
		$im = new_image($1, $2);
	}

	elsif ($line =~ /^\s*mk(Circle|Rectangle|(?:Down)?Triangle)Color\s+\S+\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s*$/){
		my ($shape, $outline_color, $fill_color, $x, $y, $radius) =
			($1, allocate_color($im, $2), allocate_color($im, $3), $4, $5, $6);

		$im->setAntiAliased($outline_color);
		
		if ($shape eq 'Circle'){
			$radius <<= 1;
			$im->filledArc($x, $y, $radius, $radius, 0, 360, $fill_color);
			$im->arc($x, $y, $radius, $radius, 0, 360, gdAntiAliased);
		}
		elsif ($shape eq 'Rectangle'){
			$im->filledRectangle($x - $radius, $y - $radius, $x + $radius, $y + $radius, $fill_color);
			$im->rectangle($x - $radius, $y - $radius, $x + $radius, $y + $radius, gdAntiAliased);
		}
		elsif ($shape eq 'Triangle'){
			my $poly = new GD::Polygon;
			$poly->addPt($x - $radius, $y + $radius);
			$poly->addPt($x + $radius, $y + $radius);
			$poly->addPt($x, $y - $radius);
			$poly->addPt($x - $radius, $y + $radius);
			$im->filledPolygon($poly, $fill_color);
			$im->polygon($poly, gdAntiAliased);
		}
		elsif ($shape eq 'DownTriangle'){
			my $poly = new GD::Polygon;
			$poly->addPt($x - $radius, $y - $radius);
			$poly->addPt($x + $radius, $y - $radius);
			$poly->addPt($x, $y + $radius);
			$poly->addPt($x - $radius, $y - $radius);
			$im->filledPolygon($poly, $fill_color);
			$im->polygon($poly, gdAntiAliased);
		}
		else {
			die "Huh?";
		}
	}

	elsif ($line =~ /^\s*mkText \"([^\"]*)\"\s+(\S+)\s+(\S+)\s+\S+\s+(\S+)\s+(\S+)\s+\{(\S+)\s+(\d+)\}\s*$/){
		my ($str, $x, $y, $anchor, $color, $font, $font_size) = ($1, $2, $3, $4, allocate_color($im, $5), $6, $7);
		#$im->stringFT($color, $font_file, $font_size, 0, $x, $y, $str);
		$im->string(gdSmallFont, $x, $y, $str, $color);
	}

	elsif ($line =~ /^\s*mkColored(Arrow|Line)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+\S+\s+(\S+)\s*$/){
		my ($shape, $x1, $y1, $x2, $y2, $color) = ($1, $2, $3, $4, $5, allocate_color($im, $6));
		# $shape is currently unused.
		$im->setAntiAliased($color);
		$im->line($x1, $y1, $x2, $y2, gdAntiAliased);
	}

	elsif ($line =~ /^\s*mkPolygonColor\s+\S+\s+(\S+)\s+(\S+)\s+\{([^\}]+)\}\s*$/){
		my ($outline_color, $fill_color, $coords) =
			(allocate_color($im, $1), allocate_color($im, $2), $3);
		$im->setAntiAliased($outline_color);
		my @coords = split(' ', $coords);
		my $poly = new GD::Polygon;

		while (@coords){
			my $x = shift(@coords);
			my $y = shift(@coords);
			die "undefined x or y in polygon coords" unless defined $x && defined $y;
			$poly->addPt($x, $y);
		}
		
		$im->filledPolygon($poly, $fill_color);
		$im->polygon($poly, gdAntiAliased);
	}
	else {
		# print "Did not parse '$line'.\n" unless $line =~ /^set itemColor/;
	}

	return $im;
}


GetOptions('batch-file=s'            => \$batch_file,
		   'background-color|bg=s'   => \$background_color,
		   'small-dimensions=s'      => \$small_dimensions,
		   'make-small!'             => \$make_small,
		   'small-suffix=s'          => \$small_suffix,
		   'font|ttf=s'              => \$font_file,
		   'png-compression=i'       => \$png_compression,
		   'true-color!'             => \$true_color,
		   'save-alpha!'             => \$save_alpha,
		   'interlaced!'             => \$interlaced,
		   'transparent!'            => \$transparent);

usage() if $Getopt::Long::error;
usage() if @ARGV > 0 && $batch_file;


if ($make_small){
	die "$0: -small-suffix must be of non-zero length.\n" unless length($small_suffix);
	
	if ($small_dimensions){
		die "$0: -small-dimensions needs an argument in the form HxW.\n"
			unless $small_dimensions =~ /^\s*(\d+)\s*x\s*(\d+)\s*$/;
		($small_height, $small_width) = ($1, $2);
	}
	else {
		($small_height, $small_width) = ($default_small_height, $default_small_width);
	}
}


if ($batch_file){
	open(B, $batch_file) || die "$0: could not open batch file '$batch_file' ($!).\n";
	@ARGV = <B>;
	close(B) || die "$0: could not close batch file '$batch_file' ($!).\n";
}

for my $file (@ARGV){

	my $fh = new IO::File $file;

	unless ($fh){
		warn "$0: could not open '$file' ($!). Skipping.\n";
		next;
	}

	my $im;
	
	while (<$fh>){
		chomp;
		$im = parse_line($im, $_);
	}

	undef $fh;

	my $orig_file = $file;
	$file = $file . $suffix;
    open(F, ">$file") || die "$0: could not open image file '$file' for writing ($!).\n";
    binmode F;
    print F $im->png($png_compression);
    close(F) || die "$0: could not close image file '$file': $!.\n";

	if ($make_small){
		$file = $orig_file . $small_suffix . $suffix;
		open(F, ">$file") || die "$0: could not open (small) image file '$file' for writing ($!).\n";
		binmode F;

		my $new = new_image($small_width, $small_height);
		my ($width, $height) = $im->getBounds();
		$new->copyResized($im, 0, 0, 0, 0, $small_width - 1, $small_height - 1, $width - 1, $height - 1);
			
		print F $new->png($png_compression);
		close(F) || die "$0: could not close (small) image file '$file': $!.\n";
	}
}

exit(0);
