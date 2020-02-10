#######################################################################
# Copyright (c) 2014-2018 Xilinx, Inc.  All rights reserved.
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

namespace eval ::xsdb::jtag::sequence {
    variable version 0.1

    # Create command sequence object
    proc create {} {
	return [dict create fmt {} cmds {} inbuf {}]
    }
    namespace export create
    
    # Validate command sequence object
    proc check_valid {seq} {
	if {![dict exists $seq fmt] ||
	    ![dict exists $seq cmds] ||
	    ![dict exists $seq inbuf]} {
	    error "invalid sequence object"
	}
    }
    
    # Add state change command to sequence
    proc state {seqvar state count} {
	upvar $seqvar seq
	check_valid $seq
	dict append seq fmt "a{ssi}"
	dict lappend seq cmds [list state $state $count]
    }
    namespace export state
    
    # Add shift command to sequence
    proc shift {seqvar to_ir return_tdo bits params args} {
	upvar $seqvar seq
	check_valid $seq
	if {$to_ir} {
	    set reg i
	} else {
	    set reg d
	}
	dict append seq fmt "a{ssbio{value i state s bitswap b detect b compare b}}"
	dict lappend seq cmds [list shift $reg $return_tdo $bits $params]
	if {![dict exists $params value]} {
	    set tdidata [lindex $args 0]
	    if {[string length $tdidata] != ($bits + 7)/8} {
		error "invalid TDI buffer length"
	    }
	    dict append seq inbuf [lindex $args 0]
	} else {
	    if {[llength $args] != 0} {
		error "wrong # args: should either specify TDI data or value property"
	    }
	}
    }
    namespace export shift
    
    proc delay {seqvar usec} {
	upvar $seqvar seq
	check_valid $seq
	dict append seq fmt "a{si}"
	dict lappend seq cmds [list delay $usec]
    }
    namespace export delay
    
    proc get_pin {seqvar pin} {
	upvar $seqvar seq
	check_valid $seq
	dict append seq fmt "a{ss}"
	dict lappend seq cmds [list getPin $pin]
    }
    namespace export get_pin
    
    proc set_pin {seqvar pin value} {
	upvar $seqvar seq
	check_valid $seq
	dict append seq fmt "a{ssi}"
	dict lappend seq cmds [list setPin $pin $value]
    }
    namespace export set_pin
    
    proc atomic {seqvar enable} {
	upvar $seqvar seq
	check_valid $seq
	dict append seq fmt "a{sb}"
	dict lappend seq cmds [list atomic $enable]
    }
    namespace export atomic
    
    proc progress {seqvar id} {
	upvar $seqvar seq
	check_valid $seq
	dict append seq fmt "a{si}"
	dict lappend seq cmds [list progress $id]
    }
    namespace export progress

    # TODO: unify with other copies
    proc i2bin {i bits} {
	set l {}
	while { $bits >= 8 } {
	    lappend l [expr $i & 255]
	    set i [expr $i >> 8]
	    incr bits -8
	}
	if { $bits > 0 } {
	    lappend l [expr $i & ((1 << $bits) - 1)]
	}
	return [binary format c* $l]
    }

    # Return format, commands and data for sequence
    proc fmt_cmds_data {seqvar {irlen 0} {devprops {}}} {
	upvar $seqvar seq
	check_valid $seq
	set fmt [dict get $seq fmt]
	set cmds [dict get $seq cmds]
	set inbuf [dict get $seq inbuf]

	set o 0
	for {set i 0} {$i < [llength $cmds]} {incr i} {
	    set cmd [lindex $cmds $i]
	    switch -- [lindex $cmd 0] {
		shift {
		    set b [lindex $cmd 3]
		    set l [expr ($b + 7)/8]
		    set props [lindex $cmd 4]
		    if { [dict exists $props register] } {
			control::assert {$b == 1}
			lset cmd 3 $irlen
			set b [lindex $cmd 3]
			set l [expr ($b + 7)/8]
			set name [dict get $props register]
			dict unset props register
			lset cmd 4 $props

			set names [lsearch -all -inline -dict $devprops "reg.$name*"]
			if { [llength $names] != 1 } {
			    if { [llength $names] == 0 } {
				set names [lsearch -all -inline -dict $devprops "reg.*"]
				if { [llength $names] == 0 } {
				    error "unknown register name \"$name\""
				}
			    }
			    set l {}
			    foreach n $names {
				lappend l [string range $n 4 end]
			    }
			    error "unknown or ambiguous register name \"$name\": must be [join $l {, }]"
			}
			set name [lindex $names 0]

			set v [i2bin [dict get $devprops $name] $b]
			set inbuf [string replace $inbuf $o $o $v]
			lset cmds $i $cmd
		    }
		    incr o $l
		    if { [dict exists $props compare] } {
			set compare [i2bin [dict get $props compare] $b]
			set mask [i2bin [dict get $props mask] $b]
			dict set props compare 1
			dict unset props mask
			lset cmd 4 $props
			lset cmds $i $cmd
			set inbuf "[string range $inbuf 0 $o-1]$compare$mask[string range $inbuf $o end]"
			incr o $l
			incr o $l
		    }
		}
		getPin {
		    incr o 1
		}
		default continue
	    }
	}

	return [list "so{detect b type s}a{$fmt}B" $cmds $inbuf]
    }

    namespace ensemble create -command ::xsdb::jtag::sequence
}

package provide xsdb::jtag::sequence $::xsdb::jtag::sequence::version
