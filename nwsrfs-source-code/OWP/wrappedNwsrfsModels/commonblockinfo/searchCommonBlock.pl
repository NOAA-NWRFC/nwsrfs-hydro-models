#!/usr/bin/perl

if (@ARGV < 1) {
    print "$0 path/file_name\n";
    exit 2;
}

unless (open(OPENFILE, $ARGV[0])) {
    die("Can't open $ARGV[0]\n");
}

$newCommon = "";

$line = <OPENFILE>; # read a line
$lineNumber = 1;
$commonLineNumber = -1;

while($line ne "") {
    $line =~ s/\s//g; # filtered out all white spaces and \r\n\t etc.
    $index = index($line, "COMMON", 0); # get COMMON index at position/column 0
    if ($index > -1) {
        @list = split(/\//, $newCommon, 3); # use '/' as delimiter for array of 3
#        print "$list[0] $list[1]\n";
        @variables = split(/,/, $list[2]); #  use ',' as delimiter for array of N
        foreach $variable (@variables) {
            print "$variable ";
        }
        $newCommon = $line;
        $commonLineNumber = $lineNumber;
    }
#   get line begin with one of those character
    if ($line =~ /^[\&\*\+\$\%\@\#\^\!]/ &&
        ($commonLineNumber + 1) == $lineNumber) { # the line number must in sequency order
            $line =~ s/^[\&\*\+\$\%\@\#\^\!]//g; # filtered them out if there is one
            $newCommon .= $line; # concatenate to newCommon
            $commonLineNumber++;
    }
    $line = <OPENFILE>;
    $lineNumber++;
}
#print "\n";
# print the last newCommon which has out of above while loop
@list = split(/\//, $newCommon, 3);
#print "$list[0] $list[1]\n";
@variables = split(/,/, $list[2]);
foreach $variable (@variables) {
    print "$variable ";
}
close(OPENFILE);
print "\n";
