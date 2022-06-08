##################################################################################
# Copyright (c) 2012 - 2021 Xilinx, Inc.  All rights reserved.
# SPDX-License-Identifier: MIT
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is furnished
# to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
# THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#####################################################################################

proc smartcable_install_version {{file /BOOT.BIN}} {
    if { [catch {set fhandle [tfile open $file]}] } {
	error "cannot open installed boot image file"
    }
    set err [catch {
	binary scan [tfile read -offset 0x20 $fhandle 0x80] iu* boothdr
	if { [lindex $boothdr 0] != 0xaa995566 ||
	     [lindex $boothdr 1] != 0x584c4e58 } {
	    error "unknown boot image format"
	}
	set csum [expr [join [lrange $boothdr 0 9] +] & 0xffffffff ^ 0xffffffff]
	if { $csum != [lindex $boothdr 10] } {
	    error "invalid boot header checksum"
	}
	set ihtoffset [lindex $boothdr 30]
	binary scan [tfile read -offset $ihtoffset $fhandle 0x20] iu* iht
	if { [lindex $iht 0] != 0x01020000 } {
	    error "unknown image header table version: [format 0x%08x [lindex $iht 0]]"
	}
	set ihoffset [expr [lindex $iht 3] * 4]
	set version "unknown"
	while { $ihoffset } {
	    binary scan [tfile read -offset $ihoffset $fhandle 256] iu* ih
	    binary scan [binary format I* [lrange $ih 4 end]] a* name
	    set namelen [string first \0 $name]
	    if { $namelen >= 0 } {
		set name [string range $name 0 $namelen-1]
	    }
	    if { [string range $name 0 5] == "probe-" &&
		 [string range $name end-3 end] == ".elf"} {
		set version [string range $name 6 end-4]
		break
	    }
	    set ihoffset [expr [lindex $ih 0] * 4]
	}
    } msg opt]
    tfile close $fhandle
    if { $err } {
	return -options $opt $msg
    }
    return [list $version [build_date $version]]
}

proc smartcable_file_version {file} {
    set fhandle [::open $file rb]
    set err [catch {
	::seek $fhandle 0x20
	binary scan [::read $fhandle 0x80] iu* boothdr
	if { [lindex $boothdr 0] != 0xaa995566 ||
	     [lindex $boothdr 1] != 0x584c4e58 } {
	    error "unknown boot image format"
	}
	set csum [expr [join [lrange $boothdr 0 9] +] & 0xffffffff ^ 0xffffffff]
	if { $csum != [lindex $boothdr 10] } {
	    error "invalid boot header checksum"
	}
	set ihtoffset [lindex $boothdr 30]
	::seek $fhandle $ihtoffset
	binary scan [::read $fhandle 0x20] iu* iht
	if { [lindex $iht 0] != 0x01020000 } {
	    error "unknown image header table version: [format 0x%08x [lindex $iht 0]]"
	}
	set ihoffset [expr [lindex $iht 3] * 4]
	set version "unknown"
	while { $ihoffset } {
	    ::seek $fhandle $ihoffset
	    binary scan [::read $fhandle 256] iu* ih
	    binary scan [binary format I* [lrange $ih 4 end]] a* name
	    set namelen [string first \0 $name]
	    if { $namelen >= 0 } {
		set name [string range $name 0 $namelen-1]
	    }
	    if { [string range $name 0 5] == "probe-" &&
		 [string range $name end-3 end] == ".elf"} {
		set version [string range $name 6 end-4]
		break
	    }
	    set ihoffset [expr [lindex $ih 0] * 4]
	}
    } msg opt]
    ::close $fhandle
    if { $err } {
	return -options $opt $msg
    }
    return [list $version [build_date $version]]
}

proc build_date {version} {
    set fields [split $version -]
    if { [llength $fields] != 2 ||
	 [catch {clock format [lindex $fields 1]} date] } {
	return ""
    }
    return $date
}
