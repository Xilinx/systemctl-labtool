#######################################################################
# Copyright (c) 2015-2021 Xilinx, Inc.  All rights reserved.
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

proc fpga5 {file} {
    set jprogram 0b1111001011
    set jconfig  0b1111000101
    set jstart   0b1111001100

    set map {}
    for {set old 0} {$old < 256} {incr old} {
	set new $old
	set new [expr (($new & 0x55) << 1) | (($new & 0xaa) >> 1)]
	set new [expr (($new & 0x33) << 2) | (($new & 0xcc) >> 2)]
	set new [expr (($new & 0x0f) << 4) | (($new & 0xf0) >> 4)]
	lappend map [binary format c $old] [binary format c $new]
    }

    if {[catch {
	set props [targets -target-properties -filter is_current]
	jtag targets [dict get [lindex $props 0] jtag_device_id]
    } msg]} {
	error "unable to determine jtag target id: $msg"
    }

    if {[catch {
	set jtagprops [jtag targets -target-properties -filter is_current]
	set irlen [dict get [lindex $jtagprops 0] irlen]
    } msg]} {
	puts stderr "warning: unable to determine irlen, assuming 10"
	set irlen 10
    }
    if { $irlen != 10 } {
	error "unexpected irlen $irlen, expected 10"
    }

    set f [::open $file rb]
    jtag lock
    set seq [jtag sequence]
    set code [catch {
	$seq state RESET
	$seq irshift -state IDLE -int $irlen $jprogram
	$seq run

	puts -nonewline "  Shutdown..."
	flush stdout
	$seq clear
	$seq irshift -state IDLE -capture -tdi 1 $irlen
	set start [clock milliseconds]
	while 1 {
	    if {[string range [$seq run -bits] 4 4] == 1} break 
	    if {[clock milliseconds] - $start > 1000} {
		error "timeout waiting for INIT"
	    }
	}
	puts "done"
	
	puts -nonewline "  Configure..."
	flush stdout
	$seq clear
	$seq irshift -state IDLE -int $irlen $jconfig
	while {![eof $f]} {
	    set bindata [::read $f 100000]
	    set bindata [string map $map $bindata]
	    $seq drshift -state DRSHIFT -binary [expr [string length $bindata] * 8] $bindata
	    $seq run -bits
	    $seq clear
	    puts -nonewline "."
	    flush stdout
	}
	$seq state RESET 5
	$seq run -bits
	puts "done"

	puts -nonewline "  Start..." 
	flush stdout
	$seq clear
	$seq irshift -state IDLE -capture -int $irlen $jstart
	$seq state IDLE 1000
	set start [clock milliseconds]
	while 1 {
	    if {[string range [$seq run -bits] 5 5] == 1} break 
	    if {[clock milliseconds] - $start > 1000} {
		error "timeout waiting for DONE"
	    }
	}
	$seq irshift -state IDLE -tdi 1 $irlen
	$seq run -bits
	puts "done"
    } msg opt]
    $seq delete
    jtag unlock
    ::close $f
    if { $code } {
	error $msg
    }
    return
}
