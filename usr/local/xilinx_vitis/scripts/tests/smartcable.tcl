#######################################################################
# Copyright (c) 2016-2021 Xilinx, Inc.  All rights reserved.
# Copyright (c) 2022-2023 Advanced Micro Devices, Inc. All rights reserved.
#
# This   document  contains  proprietary information  which   is
# protected by  copyright. All rights  are reserved. No  part of
# this  document may be photocopied, reproduced or translated to
# another  program  language  without  prior written  consent of
# XILINX Inc., San Jose, CA. 95124
#
# Xilinx, Inc.
# XILINX IS PROVIDING THIS DESIGN, CODE, OR INFORMATION "AS IS" AS A
# COURTESY TO YOU.  BY PROVIDING THIS DESIGN, CODE, OR INFORMATION AS
# ONE POSSIBLE   IMPLEMENTATION OF THIS FEATURE, APPLICATION OR
# STANDARD, XILINX IS MAKING NO REPRESENTATION THAT THIS IMPLEMENTATION
# IS FREE FROM ANY CLAIMS OF INFRINGEMENT, AND YOU ARE RESPONSIBLE
# FOR OBTAINING ANY RIGHTS YOU MAY REQUIRE FOR YOUR IMPLEMENTATION.
# XILINX EXPRESSLY DISCLAIMS ANY WARRANTY WHATSOEVER WITH RESPECT TO
# THE ADEQUACY OF THE IMPLEMENTATION, INCLUDING BUT NOT LIMITED TO
# ANY WARRANTIES OR REPRESENTATIONS THAT THIS IMPLEMENTATION IS FREE
# FROM CLAIMS OF INFRINGEMENT, IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.
#
#######################################################################

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
