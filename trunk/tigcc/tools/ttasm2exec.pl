#!/usr/bin/perl -W
use strict;

# Trivial program to convert a TI-68k assembly file (.??z) to a TI-BASIC Exec string.
# Copyright (C) 2009 Lionel Debroux.

package main;
    my $length;
    my $filecontents;
    my $unpackedfilecontents;

    (   ($#ARGV == 0) # A single argument...
     && (($ARGV[0] =~ m#.89z$#) || ($ARGV[0] =~ m#.9xz$#) || ($ARGV[0] =~ m#.v2z$#)) # ... which ends with {89z,9xz,v2z} and ...
     && (-f $ARGV[0]) # ... which is a path representing an actual file.
    )
    or die "Usage: ttasm2exec.pl <infile.{89z,9xz,v2z}>\n";

    open(INFILE, $ARGV[0]) or die "Can't open $ARGV[0]: $!";
    read(INFILE, $filecontents, -s INFILE);
    close(INFILE);

    if ((index ($filecontents, "**TI89**") != 0) && (index ($filecontents, "**TI92P*") != 0)) {
        die "$ARGV[0] doesn't seem to be a file suitable for TI-68k calculators";
    }

    do {
        use bytes;
        # Get file size in bytes.
        $length = length($filecontents);
        print "Input file is $length bytes long (of which 91 bytes will be stripped).\n";

        if ($length <= 93) { # 86 bytes for the header, 2 size bytes, 2 bytes for the empty relocation, 1 byte for the tag and 2 bytes for the trailing checksum.
            die "$ARGV[0] is too short to be a valid ASM file";
        }
        $length -= 90;
        
        $filecontents = substr($filecontents, 88, $length);

        $length *= 2; # Number of hex digits is twice that of bytes.

        $unpackedfilecontents = unpack ("H[$length]", $filecontents);
        if (rindex($unpackedfilecontents, "f3") != $length - 2) {
            warn "$ARGV[0] doesn't seem to be an assembly file, the Exec string that will be generated is unlikely to work !";
        }
        $length = length($unpackedfilecontents);
        $unpackedfilecontents = substr($unpackedfilecontents, 0, $length - 2);
        print "Exec \"$unpackedfilecontents\"\n";
    } while (0);
