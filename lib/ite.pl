# ite.pl, the Perl parser for .log and .ps file.
#
# Copyright (C) 2000 by Wolfgang Kuehn <wolfgang@decatur.de>
#
# Version: _ITEVERSION_
# Date: Sun May 28 17:56:26 CEST 2000
# Syntax          : Perl
#
#    This program is free software; you can redistribute it and/or
#    modify it under the terms of the GNU General Public License
#    as published by the Free Software Foundation. See the copyright
#    notice included in the iTe distribution for more details.


# Program expects on standard input 4 lines, for example
#   _ITEVERSION_
#   slave.tex
#   master.tex
#   /tmp/test
#   dvips -q %s -o %s 2>&1
#
# 1. line: Version
# 2. line: TeX master file
# 3. line: TeX slave file
# 4. line: Temporary directory
# 5. line: Command to launch dvips
#
# Expects the following files in the working directory:
# 1) master.dvi
# 2) master.log
# 
# Program generates the following files in the temporary directory:
# 1) script.ps:	The output of dvips.
# 2) prolog.ps:	The prolog of script.ps.
# 3) pageXY.ps:	For each page that contains an iTe block all
#		script between '%%Page: XY..' and the next page
#		or the trailer '%%Trailer'.
# 4) docuXY.eps:	For each included eps file which is part of an
#			iTe object.
# 5) objXY.ps:	For each large enough iTe object.
# 6) info.el:   List containing information about iTe objects.
#               This list is evaluated by the Emacs session.


# use strict;

# Definition of global variables.

my $debug = 0;
my $version = '_ITEVERSION_';
my $decimal = '-?\d*.?\d*';
my $savedobjcount = 0;
my $saveddocucount = 0;
my $blockIterator=0;
my $objcount = 0;
my @blocks = ();
my $lispVersion;
my $tmp;
my $name;
my $master;
my $dvips;
my $FN;

# Definition of subroutines.

sub check_version {
    my($v, $msg) = @_;
    $v =~ s/\s+//g;
    die "Wrong version $v of $msg. Version $version is needed.\n" if $v ne $version;
}

# Subroutine extract_docu writes all lines between here ($_="%%BeginDocument")
# and "%%EndDocument" to a file. On return $_ is next line after "%%EndDocument".
sub extract_docu () {
  $FN = "docu$saveddocucount.eps";
  $saveddocucount++;
  open (DOCU, ">$FN") or die
    "Can't write to file $FN: $!\n";

  until (/%%EndDocument/) {
    $_ = <IN>;
    print DOCU $_;		# Last loop writes "%%EndDocument".
  }
  $_ = <IN>;
  close DOCU;
  $FN;
}

sub parse_object () {
  print ".";
  /%iTeBeginObject ($decimal)pt/o;
# $1 is unit.
  print PAGE 
    "ITEDict /obj$objcount [ /info$objcount cvx $1 delta gstate 
\{gsave ITEDict begin BeginObject end\n";
  
  # Skip lines up to "%iTeInObject".
  until (/%iTeInObject/) {
    $_ = <IN>;
  }
  
  # Check if object is too big to be directly generated into page.
  # Read about 2000 bytes into buffer.
  my @buffer = ();
  my $objectsize = 0;
  my $fh;

  while (!/%iTeEndObject/ && $objectsize<2000) {
    $objectsize += length($_);
    push @buffer, $_;	# First loop pushes "%iTeInObject".
    $_ = <IN>;
    if (/%%BeginDocument/) {
      push @buffer, "%%BeginDocument\n";
      $FN = do extract_docu ();
      push @buffer, "($FN) ITEDict /Run get exec\n%%EndDocument\n";
    }
  }
  
  # If object is too big, write it to file.

  if ( $objectsize>=2000 ) {
    $FN = "obj$savedobjcount.ps";
    $savedobjcount++;
    print PAGE "\n($FN) run\n";
    open (OBJECT, ">$FN") or die
      "Can't write to file $FN: $!\n";
    $fh = \*OBJECT;
  } else {
    $fh = \*PAGE;
  }
  
  # Write the buffer.
  my $line;
  foreach $line (@buffer) {
    print $fh $line;
  }
  
  # Write remaining object.
  until (/%iTeEndObject/) {
    print $fh $_;
    $_ = <IN>;
    if (/%%BeginDocument/) {
      print $fh $_;
      $FN = do extract_docu ();
      print $fh "($FN) ITEDict /Run get exec\n%%EndDocument\n";
    }
  }
  
  if ($fh == \*OBJECT) {
    close $fh;
    $fh = \*PAGE;
  }
  print PAGE 
    "\} cvlit aload pop ] cvx put ITEDict /obj$objcount get exec\n";
  print PAGE $_;		# Write "%iTeEndObject".
  $objcount++;
}

sub parse_block () {
  print "Processing block $blocks[$blockIterator] ";
  if (/\s($decimal)pt\s($decimal)pt/o) {
# $1 is width, $2 is height of box.
      print PAGE "ITEDict begin $1 $2 MakeBox end\n";
  }
  
  $_ = <IN>;
  
  until (/%iTeEndBlock/) {
    print PAGE $_;
    do parse_object() if /%iTeBeginObject/;
    $_ = <IN>;
  }

  print PAGE $_;		# $_ = "%iTeEndBlock".
  print "\n";
  $blockIterator++;
}


# Body.

print "Executing Perl script ite.pl.\n";

$lispVersion = <STDIN>;  # Version number of lisp code.
chop $lispVersion;
$master = <STDIN>;       # Name of TeX master file.
chop $master;
$name   = <STDIN>;       # Name of active TeX file. 
chop $name;
$tmp    = <STDIN>;       # The temporary directory. 
chop $tmp;
$dvips = <STDIN>;        # Command to start dvips.
chop $dvips;

$dvips = $dvips.' -q %s -o %s 2>&1';

print "
Lisp version: $lispVersion
Master file: $master
Active file: $name
Temporary directory: $tmp\n
dvips command: $dvips\n";

check_version($lispVersion, "lisp file \'ite.el\'");
$master =~ s/\.tex$//;

open IN, "$master.log" or die "Can't read file $master.log.";

my @names = ();
my $cf = "$master.tex";		# current file
my $versionOk = 0;

# Array contains the block numbers of all blocks in the active file.
$#blocks==-1;

print "Parsing file $master.log for blocks defined in file $name.\n";
while (<IN>) {
  while ( $_ ) {
      if ( s/^.*?(ITEKEYOPEN|ITEKEYCLOSE|ITEKEYBLOCK|ITEKEYVERSION)// ) {
	  if ( $1 eq "ITEKEYOPEN" ) {
	      push @names, $cf;
	      $cf=0;
	      do {
		  if (/\(([^)\s]+)/) {
		      $cf = $1;
	          } else { $_ = <IN>; }
	      } while (!$cf);
	      print "Looking into file $cf.\n";
          } elsif ( $1 eq "ITEKEYCLOSE" ) {
	      $cf = pop @names;
	      print "In file: <$cf>\n" if $debug;
          } elsif ( $1 eq "ITEKEYBLOCK" ) {
	      if ($name eq $cf) {
	          /(\d+)/;
	          print "Found block $1 in file $name.\n";
	          push @blocks, $1;
	      }
          } elsif ( $1 eq "ITEKEYVERSION" ) {
	      my $texversion = $';
	      chop $a;
	      if ($debug) { print "(La)TeX Version is $texversion\n"; }
	      check_version($texversion, "TeX file \'ite.sty\' or \'ite.tex\'");
	      $versionOk = 1;
          }
      } else {
          $_ = '';
      }
   }
}

close IN;

die "You forgot to load the 'ite' package, or you use a pre 1.2 version.
You need to load version $version.\n" if !$versionOk;

die "No iTe blocks found in $master.log.\n" if $#blocks==-1;

print "Successfully parsed file $master.log.
List of blocks in active file: [@blocks].\n";

my $cmd = sprintf $dvips, $master, "$tmp/script.ps";
print "Generating PostScript with\n    $cmd\n";
my $error = `$cmd`;
#die "$error\nCould not successfully execute dvips!\n" if ($error =~ /dvips/);
# TODO: How to determine if dvips was successful?
open (IN, "$tmp/script.ps") or die "$error\n"; 

print "Finding all pages containing relevant iTe blocks.\n";

my $inDocument = 0;
my $page;
my $blockfound = 0;
my @pages = ();

while (<IN>) {
    if (/%%BeginDocument/) {
	$inDocument = 1;
    } elsif (/%%EndDocument/) {
	$inDocument = 0;
    } elsif (!$inDocument && /%%Trailer/) {
	if ($blockfound) {
	    $page->{ENDLINE} = $. - 1;
	    push (@pages, $page);
	}
    } elsif (!$inDocument && /%%Page:\s?(\S+)\s(\d+)/) {
	if ($blockfound) {
	    $page->{ENDLINE} = $. - 1;
	    push (@pages, $page);
	}
	
	last if $blockIterator > $#blocks;

	$blockfound = 0;
	$page = {
	    NUMBER => $2,
	    LABEL => $1,
	    BEGINLINE   => $.,
	    ENDLINE    => -1     # We don't know yet on which line the page ends.
       }
    } elsif (/%iTeBeginBlock $blocks[$blockIterator]/) {
	$blockfound = 1;
	print "Found block $blocks[$blockIterator++] on page $page->{NUMBER} with label $page->{LABEL}.\n";
    }
}

close IN;

#print "List of pages: [@pages].\n";   

# Parse the output of dvips in file "$tmp/script.ps". First
# write prolog to file.

chdir $tmp || die "Can't cd to $tmp: $!\n";

open (IN, "script.ps") or die "Can't read file script.ps.\n";

$FN = "prolog.ps";
open (PROLOG, ">$FN") or die
  "Can't write to file $FN: $!\n";

$_ = <IN>;

until ( /%%Page:/ ) {       # Last line of prolog should be %%EndProlog
  print PROLOG $_;
  $_ = <IN>;
}

close PROLOG;


# Now write each page to a different file. If you maintain this code, make sure
# not too loose any lines.

open (OUT, ">info.el") or die "Can't write to file info.el.\n";
print OUT "(setq ite-info-list \'(\n";

$blockIterator = 0;

foreach $page (@pages) {
    $objcount = 0;
  
    open (PAGE, ">page$page->{NUMBER}.ps") or die "Can't write to file $!\n";
    print "Generate page $page->{NUMBER} starting at line $page->{BEGINLINE}.\n";

    # We should be at beginning of a new page. In case we are not:
    until ( $. >= $page->{BEGINLINE} ) {
        $_ = <IN>;
    }

    print PAGE $_;  # $_ = "%%Page:..."
    $_ = <IN>;
  
    until ( $. > $page->{ENDLINE} ) {
      print PAGE $_;
      
      do parse_block() if $blockIterator <= $#blocks &&
	/%iTeBeginBlock $blocks[$blockIterator]/;
      
      $_ = <IN>;
    }
    close PAGE;
    print OUT "($page->{NUMBER} $objcount)";
}

print OUT "))";
close OUT;

print "\niTe_PerlFinished\n";






