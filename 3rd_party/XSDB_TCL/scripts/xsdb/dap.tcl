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

namespace eval dap {
    variable daps {}
    variable commands {getapinfo abort dpread dpwrite apread apwrite memread memwrite run clear delete}
    variable log_mode 0

    proc log args {
	variable log_mode
	if { $log_mode != 0 } {
	    puts "===> $args"
	}
	set result [uplevel 1 $args]
	if { $log_mode != 0 && $result != {} } {
	    puts $result
	}
	return $result
    }

    proc _cmd_getapinfo {name args} {
	variable daps
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"getapinfo\""
	}
	dict set daps $name aps {}
	dict with daps $name {
	    set old_cmds $cmds
	    set cmds {}
	    for {set i 0} {$i < 16} {incr i} {
		_cmd_apread $name $i 0xf0
		_cmd_apread $name $i 0xf4
		_cmd_apread $name $i 0xf8
		_cmd_apread $name $i 0xfc
	    }
	    foreach {result err} [_cmd_run $name] break
	    if { $err != "" } {
		set cmds $old_cmds
		error $err
	    }
	    set cmds {}
	    for {set i 0} {$i < 16} {incr i} {
		foreach { f0 f4 f8 idr } $result break
		set result [lrange $result 4 end]
		if { $idr == 0 } continue
		set ap [dict create idr $idr name [format "0x%08x" $idr] mem 0]
		switch -- [format "0x%08x" [expr {$idr & 0x0fffffff}]] {
		    0x04770001 {
			dict set ap name "AHB-AP"
			dict set ap mem 1
			dict set ap cfg $f4
			dict set ap ctrl 0
			dict set ap size {8 16 32}
		    }
		    0x04770002 {
			dict set ap name "APB-AP"
			dict set ap mem 1
			dict set ap cfg $f4
			dict set ap ctrl 0x80000000
			dict set ap size {32}
			if { ($f4 & 2) == 0 } {
			    dict set ap base $f8
			} else {
			    dict set ap base [expr {(0xf0 << 32) | $f8}]
			}
		    }
		    0x04770004 {
			dict set ap name "AXI-AP"
			dict set ap mem 1
			dict set ap cfg $f4
			dict set ap ctrl 0x30006000
			if { ($f4 & 4) == 0 } {
			    dict set ap size {8 16 32}
			} else {
			    dict set ap size {8 16 32 64}
			}
		    }
		    0x04760010 {
			dict set ap name "JTAG-AP"
		    }
		}
		dict set aps $i $ap
	    }
	    set cmds $old_cmds
	}
	return $aps
    }

    proc _cmd_abort {name args} {
	variable daps
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"abort\""
	}
	dict with daps $name {
	    lappend cmds [list ABORT]
	}
	return
    }

    proc _cmd_dpread {name args} {
	variable daps
	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"dpread \[addr\]\""
	}
	set addr [lindex $args 0]
	dict with daps $name {
	    lappend cmds [list DPREAD $addr]
	}
	return
    }

    proc _cmd_dpwrite {name args} {
	variable daps
	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"dpwrite \[addr\] \[value\]\""
	}
	set addr [lindex $args 0]
	set value [lindex $args 1]
	dict with daps $name {
	    lappend cmds [list DPWRITE $addr $value]
	}
	return
    }

    proc _cmd_apread {name args} {
	variable daps
	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"apread \[ap\] \[addr\]\""
	}
	set ap [lindex $args 0]
	set addr [lindex $args 1]
	dict with daps $name {
	    lappend cmds [list APREAD $ap $addr]
	}
	return
    }

    proc _cmd_apwrite {name args} {
	variable daps
	if { [llength $args] != 3 } {
	    error "wrong # args: should be \"apwrite \[ap\] \[addr\] \[value\]\""
	}
	set ap [lindex $args 0]
	set addr [lindex $args 1]
	set value [lindex $args 2]
	dict with daps $name {
	    lappend cmds [list APWRITE $ap $addr $value]
	}
	return
    }

    proc _csw_size {size} {
	switch -- $size {
	    1 { return 0 }
	    2 { return 1 }
	    4 { return 2 }
	    8 { return 3 }
	}
	error "unsupported CSW size: $size"
    }

    proc _cmd_memread {name args} {
	set options {
	    {size "access size" {args 1 default 4}}
	    {ctrl "AP ctrl value" {args 1}}
	}
	array set params [::xsdb::get_options args $options 0]

	variable daps
	set size $params(size)
	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"memread \[ap\] \[addr\]\""
	}
	if { ![dict exists $daps $name aps] } {
	    _cmd_getapinfo $name
	}
	set ap [lindex $args 0]
	set addr [lindex $args 1]
	if { ![dict exists $daps $name aps $ap] ||
	     ![dict get $daps $name aps $ap mem] } {
	    error "invalid AP index: $ap"
	}
	if { ($addr & ($size - 1)) != 0 } {
	    error "invalid AP address alignment: $addr"
	}
	if { ([dict get $daps $name aps $ap cfg] & 2) == 0 } {
	    set cmd MEMREAD32
	    if { $addr < 0 || $addr > 0xffffffff } {
		error "invalid AP address range: $addr"
	    }
	} else {
	    set cmd MEMREAD64
	    if { $addr < 0 || $addr > 0xffffffffffffffff } {
		error "invalid AP address range: $addr"
	    }
	}
	if { [info exists params(ctrl)] } {
	    set apctrl $params(ctrl)
	} else {
	    set apctrl [dict get $daps $name aps $ap ctrl]
	}
	set apctrl [expr {($apctrl & ~7) | [_csw_size $size]}]
	dict with daps $name {
	    lappend cmds [list $cmd $ap $apctrl $addr]
	}
	return
    }

    proc _cmd_memwrite {name args} {
	set options {
	    {size "access size" {args 1 default 4}}
	    {ctrl "AP ctrl value" {args 1}}
	}
	array set params [::xsdb::get_options args $options 0]

	variable daps
	set size $params(size)
	if { [llength $args] != 3 } {
	    error "wrong # args: should be \"memwrite \[ap\] \[addr\] \[value\]\""
	}
	if { ![dict exists $daps $name aps] } {
	    _cmd_getapinfo $name
	}
	set ap [lindex $args 0]
	set addr [lindex $args 1]
	set value [lindex $args 2]
	if { ![dict exists $daps $name aps $ap] ||
	     ![dict get $daps $name aps $ap mem] } {
	    error "invalid AP index: $ap"
	}
	if { ($addr & ($size - 1)) != 0 } {
	    error "invalid AP address alignment: $addr"
	}
	if { ([dict get $daps $name aps $ap cfg] & 2) == 0 } {
	    set cmd MEMWRITE32
	    if { $addr < 0 || $addr > 0xffffffff } {
		error "invalid AP address range: $addr"
	    }
	} else {
	    set cmd MEMWRITE64
	    if { $addr < 0 || $addr > 0xffffffffffffffff } {
		error "invalid AP address range: $addr"
	    }
	}
	if { [info exists params(ctrl)] } {
	    set apctrl $params(ctrl)
	} else {
	    set apctrl [dict get $daps $name aps $ap ctrl]
	}
	set apctrl [expr {($apctrl & ~7) | [_csw_size $size]}]
	dict with daps $name {
	    lappend cmds [list $cmd $ap $apctrl $addr $value]
	}
	return
    }

    variable DAP_ABORT 0x8
    variable DAP_DPACC 0xa
    variable DAP_APACC 0xb

    proc _run_set_abort {} {
	variable DAP_ABORT
	upvar 1 ir ir
	upvar 1 seq seq
	if { $ir != $DAP_ABORT } {
	    set ir $DAP_ABORT
	    log $seq irshift -state IRUPDATE -int 4 $ir
	}
    }

    proc _run_set_dpacc {} {
	variable DAP_DPACC
	upvar 1 ir ir
	upvar 1 seq seq
	if { $ir != $DAP_DPACC } {
	    set ir $DAP_DPACC
	    log $seq irshift -state IRUPDATE -int 4 $ir
	}
    }

    proc _run_set_apacc {} {
	variable DAP_APACC
	upvar 1 ir ir
	upvar 1 seq seq
	if { $ir != $DAP_APACC } {
	    set ir $DAP_APACC
	    log $seq irshift -state IRUPDATE -int 4 $ir
	}
    }

    proc _run_set_dpselect {addr} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_dpacc
	set diff [expr {($select ^ ($addr >> 4)) & 0xf}]
	if { $diff != 0 } {
	    lappend resptypes $resptype
	    set resptype i
	    set select [expr {$select ^ $diff}]
	    log $seq drshift -state DRUPDATE -capture -int 35 [expr {($select << 3) | 4}]
	}
    }

    proc _run_set_apselect {ap addr} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	set diff [expr {($select ^ (($ap << 24) | $addr)) & 0xff0000f0}]
	if { $diff != 0 } {
	    lappend resptypes $resptype
	    set resptype i
	    set select [expr {$select ^ $diff}]
	    _run_set_dpacc
	    log $seq drshift -state DRUPDATE -capture -int 35 [expr {($select << 3) | 4}]
	}
	_run_set_apacc
    }

    proc _run_clear_stickybits {} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 ctrl ctrl
	upvar 1 select select
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	upvar 1 stickybits stickybits
	if { $stickybits } {
	    # Clear CTRL/STAT.SICKYORUN
	    set addr 4
	    set data [expr {$ctrl | $stickybits}]
	    _run_set_dpselect $addr
	    lappend resptypes $resptype
	    set resptype i
	    log $seq drshift -state $endstate -capture -int 35 [expr {($data << 3) | (($addr & 0xc) >> 1)}]
	    set stickybits 0
	}
    }

    proc _run_dpread {addr} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_dpselect $addr
	lappend resptypes $resptype
	set resptype r
	log $seq drshift -state $endstate -capture -int 35 [expr {(($addr & 0xc) >> 1) | 1}]
    }

    proc _run_dpwrite {addr data} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_dpselect $addr
	lappend resptypes $resptype
	set resptype w
	log $seq drshift -state $endstate -capture -int 35 [expr {($data << 3) | (($addr & 0xc) >> 1)}]
    }

    proc _run_apread {ap addr} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 apdelay apdelay
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_apselect $ap $addr
	lappend resptypes $resptype
	set resptype r
	log $seq drshift -state $endstate -capture -int 35 [expr {(($addr & 0xc) >> 1) | 1}]
	if { $apdelay > 0 } {
	    log $seq state IDLE $apdelay
	}
    }

    proc _run_apwrite {ap addr data} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 apdelay apdelay
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_apselect $ap $addr
	lappend resptypes $resptype
	set resptype w
	log $seq drshift -state $endstate -capture -int 35 [expr {($data << 3) | (($addr & 0xc) >> 1)}]
	if { $apdelay > 0 } {
	    log $seq state IDLE $apdelay
	}
    }

    proc _cmd_run {name args} {
	variable daps
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"clear\""
	}

	dict with daps $name {
	    jtag target $node_id
	    if { [catch {jtag lock} msg opt] } {
		if { $msg != "already locked" } {
		    return -options $opt $msg
		}
		set unlock 0
	    } else {
		set unlock 1
	    }
	    set err ""
	    if { [catch {
		set ir 15
		set select 0xff0000ff
		set ncmds [llength $cmds]
		set complete 0
		set result {}
		while { $complete < $ncmds } {
		    $seq clear
		    set resptype i
		    set resptypes {}
		    set stickybits 2

		    for {set i $complete} {$i < $ncmds} {incr i} {
			set cmd [lindex $cmds $i]
			switch -- [lindex $cmd 0] {
			    ABORT {
				_run_set_abort
				log $seq drshift -state IDLE -int 35 8
				_run_set_dpacc
				lappend resptypes $resptype
				set resptype r
				log $seq drshift -state IDLE -capture -int 35 7
			    }

			    DPREAD {
				_run_dpread [lindex $cmd 1]
			    }

			    DPWRITE {
				_run_dpwrite [lindex $cmd 1] [lindex $cmd 2]
			    }

			    APREAD {
				_run_clear_stickybits
				_run_apread [lindex $cmd 1] [lindex $cmd 2]
			    }

			    APWRITE {
				_run_clear_stickybits
				_run_apwrite [lindex $cmd 1] [lindex $cmd 2] [lindex $cmd 3]
			    }

			    MEMREAD32 {
				_run_clear_stickybits
				set ap [lindex $cmd 1]
				_run_apwrite $ap 0x00 [lindex $cmd 2]
				set resptype i
				_run_apwrite $ap 0x04 [lindex $cmd 3]
				set resptype i
				_run_apread $ap 0x0c
			    }

			    MEMREAD64 {
				_run_clear_stickybits
				set ap [lindex $cmd 1]
				_run_apwrite $ap 0x00 [lindex $cmd 2]
				set resptype i
				_run_apwrite $ap 0x04 [expr [lindex $cmd 3] & 0xffffffff]
				set resptype i
				_run_apwrite $ap 0x08 [expr [lindex $cmd 3] >> 32]
				set resptype i
				_run_apread $ap 0x0c
			    }

			    MEMWRITE32 {
				_run_clear_stickybits
				set ap [lindex $cmd 1]
				_run_apwrite $ap 0x00 [lindex $cmd 2]
				set resptype i
				_run_apwrite $ap 0x04 [lindex $cmd 3]
				set resptype i
				_run_apwrite $ap 0x0c [lindex $cmd 4]
			    }

			    MEMWRITE64 {
				_run_clear_stickybits
				set ap [lindex $cmd 1]
				_run_apwrite $ap 0x00 [lindex $cmd 2]
				set resptype i
				_run_apwrite $ap 0x04 [expr [lindex $cmd 3] & 0xffffffff]
				set resptype i
				_run_apwrite $ap 0x08 [expr [lindex $cmd 3] >> 32]
				set resptype i
				_run_apwrite $ap 0x0c [lindex $cmd 4]
			    }

			    default {
				error "invalid DAP command: $cmd"
			    }
			}
		    }
		    # RDBUFF
		    _run_set_dpacc
		    lappend resptypes $resptype
		    log $seq drshift -state IDLE -capture -int 35 7

		    set respdata [log $seq run -int]
		    set overrun 0
		    while { $respdata != {} } {
			set data [lindex $respdata 0]
			set status [expr {$data & 7}]
			if { $status == 1 } {
			    set overrun 1
			    set resptypes [lrange $resptypes 0 end-1]
			    set respdata [lrange $respdata 1 end]
			    if { $respdata != {} } continue

			    $seq clear
			    _run_set_dpacc
			    log $seq drshift -state IDLE -capture -int 35 7
			    set stime [clock milliseconds]
			    while { $status == 1 && [clock milliseconds] - $stime < 100 } {
				# WAIT
				set respdata [log $seq run -int]
				set data [lindex $respdata 0]
				set status [expr {$data & 7}]
			    }
			}
			if { $status != 2 } {
			    # not OK/FAULT
			    if { $status == 1 } {
				error "timeout"
			    }
			    error "invalid DAP response: [format %x $data]"
			}
			if { [lindex $resptypes 0] == "r" } {
			    lappend result [expr {$data >> 3}]
			    incr complete
			} elseif { [lindex $resptypes 0] == "w" } {
			    incr complete
			}
			set resptypes [lrange $resptypes 1 end]
			set respdata [lrange $respdata 1 end]
			if { $overrun } break
		    }
		}
	    } msg] } {
		set err $msg
	    }
	    if { $unlock } {
		jtag unlock
	    }
	    set cmds {}
	    return [list $result $err]
	}
    }

    proc _cmd_clear {name args} {
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"clear\""
	}
	dict set daps $name cmds {}
	return
    }

    proc _cmd_delete {name args} {
	variable daps
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"delete\""
	}
	[dict get $daps $name seq] delete
	dict unset daps $name
	interp alias {} ::$name {}
	return
    }

    proc _dap_dispatch {name {cmd ""} args} {
	variable daps
	variable commands
	set matches [lsearch -all -inline -glob  $commands $cmd*]
	if { [llength $matches] == 0 } {
	    error "unknown subcommand \"$cmd\": must be [join $commands {, }]"
	}
	if { [llength $matches] == 1 } {
	    set cmd [lindex $matches 0]
	} else {
	    set match [lsearch $matches $cmd]
	    if { $match < 0 } {
		error "ambiguous subcommand \"$cmd\": must be [join $matches {, }]"
	    }
	    set cmd [lindex $matches $match]
	}
	if { ![dict exists $daps $name] } {
	    error "dap object \"$name\" is not valid"
	}
	return [_cmd_$cmd $name {*}$args]
    }

    proc open {} {
	variable daps
	set props [lindex [jtag targets -target-properties -filter is_current] 0]
	set node_id [dict get $props node_id]
	set name "dap$node_id"
	if { [dict exists $daps $name] } {
	    error "DAP is already open"
	}

	set idcode "0x[dict get $props idcode]"
	if { ($idcode & 0x0fffffff) != 0x0ba00477 } {
	    error "target is not a DAP"
	}

	set seq [jtag sequence]
	set ctrl 0x50000001
	dict set daps $name [dict create node_id $node_id seq $seq cmds {} ctrl $ctrl apdelay 100 endstate DRUPDATE]
	set dap [interp alias {} ::$name {} [namespace current]::_dap_dispatch $name]

	if { [catch {jtag lock} msg opt] } {
	    if { $msg != "already locked" } {
		_cmd_delete $name
		return -options $opt $msg
	    }
	    set unlock 0
	} else {
	    set unlock 1
	}

	# reset jtag in case polling is not doing it
	$seq state RESET 5
	$seq run

	_cmd_dpwrite $name 4 $ctrl
	_cmd_dpread $name 4
	foreach {result err} [_cmd_run $name] break
	if { $err != "" } { error $err }
	set status [lindex $result 0]

	set stime [clock milliseconds]
	while { ($status & 0x20000000) == 0 && [clock milliseconds] - $stime < 100 } {
	    _cmd_dpread $name 4
	    foreach {result err} [_cmd_run $name] break
	    if { $err != "" } { error $err }
	    set status [lindex $result 0]
	}

	if { $unlock } {
	    jtag unlock
	}

	if { ($status & 0x20000000) == 0 } {
	    _cmd_delete $name
	    error [format "no debug power ack: %08x" $status]
	}
	return $dap
    }
}
