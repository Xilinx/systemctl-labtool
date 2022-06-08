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

source [file dirname [info script]]/huddle.tcl
source [file dirname [info script]]/yaml.tcl

namespace eval ::xsdb {
    proc set_profile_config {chan ctx fmt config} {
	set script [list $chan send_command Profiler configure [list command_callback $chan ea{o{}}]]
	append script " ; " [list ::tcf::write $chan "so{}" [list $ctx $config]]
	return [::tcf::eval $script]
    }

    proc _huddle2fd_value {h} {
	switch -- [lindex $h 0] {
	    "!!map" {
		set f [dict create]
		set d [dict create]
		foreach {k v} [lindex $h 1] {
		    set fd [_huddle2fd_value $v]
		    dict set f $k [lindex $fd 0]
		    dict set d $k [lindex $fd 1]
		}
		return [list "o{$f}" $d]
	    }

	    "!!seq" {
		set f {}
		set d [list]
		foreach {v} [lindex $h 1] {
		    set fd [_huddle2fd_value $v]
		    append f [lindex $fd 0]
		    lappend d [lindex $fd 1]
		}
		return [list "a{$f}" $d]
	    }

	    "!!str" {
		return [list s [lindex $h 1]]
	    }

	    "!!int" {
		return [list i [lindex $h 1]]
	    }

	    "!!float" {
		return [list f [lindex $h 1]]
	    }

	    "!!true" -
	    "!!false" {
		return [list b [lindex $h 1]]
	    }

	    "!!null" {
		return [list n [lindex $h 1]]
	    }

	    default {
		error "unexpected huddle type: [lindex $h 0]"
	    }
	}
    }

    proc _huddle2fd {h} {
	if { [lindex $h 0] != "HUDDLE" } {
	    error "no a huddle object: $h"
	}
	return [_huddle2fd_value [lindex $h 1]]
    }

    proc pconfig {config} {
	set chan [getcurchan]
	set ctx [getcurtarget]
	set h [::xsdb::yaml::yaml2huddle $config]
	set fd [_huddle2fd $h]
	puts "format: [lindex $fd 0], value: [lindex $fd 1]"

	puts [::tcf::send_command $chan Profiler configure "s[lindex $fd 0]" "e" [list $ctx [lindex $fd 1]]]
    }
    namespace export pconfig

    proc pread {} {
	set chan [getcurchan]
	set ctx [getcurtarget]
	return [lindex [::tcf::send_command $chan Profiler read "s" "ea{o{}}" [list $ctx]] 1]
    }
    namespace export pread

    proc decode_event_counters { pdata } {
	if { [dict get $pdata Format] != "EventCounters" } {
	    error "invalid data format"
	}
	set column_names [dict get $pdata ColumnNames]
	set columns [llength $column_names]
	set rows [dict get $pdata NumRows]
	set data [dict get $pdata Data]
	if { [string length $data] != $rows * $columns * 4 } {
	    error "unexpected data length: [string length $data], expected [expr {$rows * $columns * 4}]"
	}
	set result {}
	lappend result $column_names
	set pos 0
	for {set row 0} {$row < $rows} {incr row} {
	    binary scan $data "@[set pos]iu[set columns]" values
	    lappend result $values
	    incr pos [expr {$columns * 4}]
	}
	return $result
    }
    namespace export decode_event_counters
}
