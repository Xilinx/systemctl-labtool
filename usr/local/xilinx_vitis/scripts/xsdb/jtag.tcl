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

package require tcf
package require xsdb::jtag::sequence

namespace eval ::xsdb::jtag {
    variable version 0.1
    variable curnode ""

    ::xsdb::setcmdmeta jtag brief {JTAG Access}

    proc getcurnode {} {
	variable curnode
	if { $curnode == "" } {
	    error "Invalid target. Use \"jtag targets\" command to select a target"
	}
	return $curnode
    }

    proc setcurnode {node} {
	variable curnode
	set curnode $node
    }

    proc print_jtag_nodes {nodes} {
	set result ""
	foreach node $nodes {
	    if { $result != "" } {
		append result "\n"
	    }

	    set node_id [dict get $node node_id]
	    set name [dict get $node name]
	    set level [dict get $node level]
	    set info ""
	    if { [dict exists $node state] && [dict get $node state] != "" } {
		lappend info [dict get $node state]
	    }
	    if { [dict exists $node idcode] } {
		lappend info idcode [dict get $node idcode]
	    }
	    if { [dict exists $node irlen] } {
		lappend info irlen [dict get $node irlen]
	    }
	    if { [dict exists $node is_fpga] } {
		lappend info fpga
	    }
	    if { [dict exists $node is_pdi_programmable] } {
		lappend info pdi_programmable
	    }

	    if { ![dict get $node is_active] } {
		lappend info inactive
	    } elseif { $level == 0 && ![dict get $node is_open] } {
		lappend info closed
	    }
	    if { [llength $info] > 0 } {
		set info " ([join $info { }])"
	    }
	    if { [dict get $node is_current] } {
		set iscur "*"
	    } else {
		set iscur " "
	    }
	    append result [format "%*d%s %s%s" [expr {$level*3+3}] $node_id $iscur $name $info]
	}
	return $result
    }

    proc get_jtag_targets_properties {targets {parent ""} {level 0} {parentprops {}}} {
	variable curnode
	if { ![dict exists $targets $parent] } {
	    return
	}

	set result {}
	foreach ctx [dict get $targets $parent children] {
	    set jc [dict get $targets $ctx Jtag:context]
	    if { [lindex $jc 0] != "" } continue
	    set jc [lindex $jc 1]
	    set nid [dict get $targets $ctx node_id]
	    set props $parentprops
	    dict set props target_ctx $ctx
	    dict set props level $level
	    dict set props node_id $nid

	    if { [dict exists $targets $ctx JtagCable:context] } {
		set jcc [lindex [dict get $targets $ctx JtagCable:context] 1]
	    } else {
		set jcc {}
	    }

	    set open 0
	    set active 0
	    if { ([dict exists $jc isActive] && [dict get $jc isActive]) } {
		set open 1
		set active 1
	    }
	    if { ([dict exists $jcc isActive] && [dict get $jcc isActive]) ||
		 ([dict exists $jcc isInitializing] && [dict get $jcc isInitializing]) } {
		set active 1
	    }
	    dict set props is_open $open
	    dict set props is_active $active
	    dict set props is_current [expr {$curnode == $ctx}]

	    set name ""
	    if { ([dict exists $jcc Description] && [dict get $jcc Description] != "") &&
		 !([dict exists $jcc isError] && [dict get $jcc isError]) &&
		 !([dict exists $jcc isInitializing] && [dict get $jcc isInitializing]) } {
		set name [dict get $jcc Description]
		if { [dict exists $jcc Serial] && [dict get $jcc Serial] != "" } {
		    append name " [dict get $jcc Serial]"
		}
	    } else {
		if { [dict exists $jcc Manufacturer] && [dict get $jcc Manufacturer] != "" } {
		    set name [dict get $jcc Manufacturer]
		    if { [dict exists $jcc ProductID] && [dict get $jcc ProductID] != "" } {
			append name " [dict get $jcc ProductID]"
		    } else {
			append name " cable"
		    }
		    if { [dict exists $jcc Serial] && [dict get $jcc Serial] != "" } {
			append name " [dict get $jcc Serial]"
		    }
		} elseif { [dict exists $jcc ProductID] && [dict get $jcc ProductID] != "" } {
		    set name [dict get $jcc ProductID]
		}
	    }

	    set jtag_cable_name $name
	    if { $name == "" } {
		if { [dict exists $jc Name] } {
		    set name [dict get $jc Name]
		} else {
		    set name unknown
		}
	    }
	    set state ""
	    if { [dict exists $jcc isInitializing] && [dict get $jcc isInitializing] } {
		append state "initializing"
		if { [dict exists $jcc Description] && [dict get $jcc Description] != "" } {
		    append state ": [dict get $jcc Description]"
		}
	    } elseif { [dict exists $jcc isActive] && [dict get $jcc isActive] &&
		       [dict exists $jcc isError] && [dict get $jcc isError] } {
		append state "error"
		if { [dict exists $jcc Description] && [dict get $jcc Description] != "" } {
		    append state ": [dict get $jcc Description]"
		}
	    } elseif { $open && [dict exists $jc Status] && [dict get $jc Status] != "" } {
		append state "error [dict get $jc Status]"
	    }
	    dict set props name $name
	    if { $jtag_cable_name != "" } {
		dict set props jtag_cable_name $jtag_cable_name
	    }
	    dict set props state $state

	    if { [dict exists $jcc Manufacturer] && [dict get $jcc Manufacturer] != "" } {
		dict set props jtag_cable_manufacturer [dict get $jcc Manufacturer]
	    }
	    if { [dict exists $jcc ProductID] && [dict get $jcc ProductID] != "" } {
		dict set props jtag_cable_product [dict get $jcc ProductID]
	    }
	    if { [dict exists $jcc Serial] && [dict get $jcc Serial] != "" } {
		dict set props jtag_cable_serial [dict get $jcc Serial]
	    }
	    if { [dict exists $jc idCode] && [dict get $jc idCode] != 255 } {
		dict set props idcode [format "%08x" [dict get $jc idCode]]
	    }
	    if { [dict exists $jc irLen] && [dict get $jc irLen] > 0 } {
		dict set props irlen [dict get $jc irLen]
	    }
	    if { [dict exists $targets $ctx JtagDevice:properties] } {
		set devprops [lindex [dict get $targets $ctx JtagDevice:properties] 1]
		if { [dict exists $devprops reg.jprogram] } {
		    dict set props is_fpga 1
		}
		if { [dict exists $devprops is_pdi_program_supported] && [dict get $devprops is_pdi_program_supported] } {
		    dict set props is_pdi_programmable 1
		}
	    }

	    lappend result $props
	    lappend result {*}[get_jtag_targets_properties $targets $ctx [expr {$level+1}] $props]
	}
	return $result
    }

    # Print or select jtag target node
    proc targets {args} {
	set options {
	    {verbose "show verbose information"}
	    {set "set active target"}
	    {regexp "match using regexp"}
	    {nocase "case insensitive match"}
	    {filter "filter target based on properties" {args 1}}
	    {target-properties "return properties for each target"}
	    {timeout "timeout for polling when filter option is used" {args 1}}
	    {open "open target"}
	    {close "close target"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [info exists params(filter)] && ![info exists params(timeout)] } {
	    set params(timeout) 3
	}
	if { ![info exists params(filter)] && [info exists params(timeout)] } {
	    error "timeout option is valid only with filter option"
	}

	if { $params(set) + $params(open) + $params(close) + $params(target-properties) > 1 } {
	    error "ambiguous options, use only one of -set, -open, -close, -target-properties"
	}

	set chan [::xsdb::getcurchan]
	set arg [array get params]
	dict set arg chan $chan
	dict set arg services {}
	dict set arg nodes {}
	dict lappend arg actions {
	    set services [::tcf::get_services $chan]
	    incr curaction
	}
	dict lappend arg actions {
	    set nodes {}
	    set cache_misses 0
	    if { [lsearch $services Jtag] >= 0 } {
		incr cache_misses [get_context_datatypes nodes $chan {Jtag:children} {Jtag:context} "" -1]
		incr cache_misses [get_context_datatypes nodes $chan {Jtag:children} {Jtag:capabilities JtagCable:context} "" 2]
	    }
	    foreach ctx [dict keys $nodes] {
		if { [catch {
		    set jc [lindex [dict get $nodes $ctx Jtag:context] 1]
		    dict set nodes $ctx JtagDevice:properties [get_ctx_data $chan [dict get $jc idCode] JtagDevice:properties]
		} msg] } {
		    if { $msg == $::cache_miss_err } {
			incr cache_misses
		    }
		}
	    }
	    if { $cache_misses > 0 } {
		error $::cache_miss_err
	    }
	    if { $numreq > 0 } {
		cache wait
	    }
	    incr curaction
	}
	dict set arg result {
	    set_jtag_node_id $chan nodes
	    set nodes
	}
	set targets [::xsdb::process_tcf_actions $arg]
	set targets [get_jtag_targets_properties $targets]

	if { [llength $args] > 0 } {
	    if { $params(set) + $params(open) + $params(close) + $params(target-properties) == 0 } {
		set params(set) 1
	    }
	    set new_targets {}
	    foreach target $targets {
		set node_id [dict get $target node_id]
		set i [lsearch -exact $args $node_id]
		if { $i >= 0 } {
		    lappend new_targets $target
		    set args [lreplace $args $i $i]
		}
	    }
	    if { [llength $args] > 0 } {
		error "no target(s) with id: $args"
	    }
	    set targets $new_targets
	}

	if { !$params(verbose) } {
	    set new_targets {}
	    foreach target $targets {
		if { [dict get $target is_active] } {
		    lappend new_targets $target
		}
	    }
	    set targets $new_targets
	}

	if { [info exists params(filter)] } {
	    set filter [::xsdb::parse_filter $params(filter) 2]
	    set new_targets {}
	    foreach target $targets {
		if { [::xsdb::match_filter $filter $target $params(regexp) $params(nocase)] } {
		    lappend new_targets $target
		}
	    }
	    set targets $new_targets

	    if { [llength $targets] == 0 } {
		set start [clock milliseconds]
		set end $start
		set params(timeout) [expr $params(timeout) * 1000]
		while { [expr $end - $start ] < $params(timeout) } {
		    set targets [::tcf::cache_eval $chan [list get_jtag_nodes $chan]]
		    set targets [get_jtag_targets_properties $targets]
		    set target_list $targets
		    set new_targets {}
		    foreach target $targets {
			if { [::xsdb::match_filter $filter $target $params(regexp) $params(nocase)] && [dict get $target is_active] } {
			    lappend new_targets $target
			}
		    }
		    set targets $new_targets
		    if { [llength $targets] } break
		    set end [clock milliseconds]
		}
	    }
	}

	if { $params(open) } {
	    foreach target $targets {
		if { [dict get $target level] == 0 &&
		     [dict get $target is_active] &&
		     ![dict get $target is_open] } {
		    ::tcf::send_command $chan JtagCable openPort s es [list [dict get $target target_ctx]]
		}
	    }
	    return ""
	}
	if { $params(close) } {
	    foreach target $targets {
		if { [dict get $target level] == 0 &&
		     [dict get $target is_active] &&
		     [dict get $target is_open] } {
		    ::tcf::send_command $chan JtagCable closePort s e [list [dict get $target target_ctx]]
		}
	    }
	    return ""
	}
	if { $params(set) } {
	    if { [llength $targets] == 0 } {
		error "target list is empty"
	    }
	    if { [llength $targets] != 1 } {
		error "target list contains more than one entry"
	    }
	    setcurnode [dict get [lindex $targets 0] target_ctx]
	    return ""
	}
	if { $params(target-properties) } {
	    return $targets
	}
	print_jtag_nodes $targets
    }
    namespace export targets
    ::xsdb::setcmdmeta {jtag targets} categories {jtag}
    ::xsdb::setcmdmeta {jtag targets} brief {List JTAG targets or switch between JTAG targets.}
    ::xsdb::setcmdmeta {jtag targets} description {
SYNOPSIS {
    jtag targets
        List available JTAG targets.

    jtag targets <target id>
        Select <target id> as active JTAG target.
}
OPTIONS {
    -set
        Set current target to entry single entry in list.  This is
        useful in comibination with -filter option.  An error will be
        generated if list is empty or contains more than one entry.

    -regexp
        Use regexp for filter matching.

    -nocase
        Use case insensitive filter matching.

    -filter <filter-expression>
        Specify filter expression to control which targets are
        included in list based on its properties.  Filter expressions
        are similar to Tcl expr syntax.  Target properties are
        referenced by name, while Tcl variables are accessed using the
        $ syntax, string must be quoted.  Operators ==, !=, <=, >=, <,
        >, &&, and || are supported as well as ().  There operators
        behave like Tcl expr operators.  String matching operator =~
        and !~ match LHS string with RHS pattern using either regexp
        or string match.

    -target-properties
        Returns a Tcl list of dictionaries containing target properties.

    -open
        Open all targets in list.  List can be shorted by specifying
        target-ids and using filters.

    -close
        Close all targets in list.  List can be shorted by specifying
        target-ids and using filters.

    -timeout <sec>
        Poll until the targets specified by filter option are found
        on the scan chain, or until timeout. This option is valid only
        with filter option.
        The timeout value is in seconds. Default timeout is three seconds.
}
RETURNS {
    The return value depends on the options used.

    <none>
        JTAG targets list when no options are used.

    -filter
        Filtered JTAG targets list.

    -target-properties
        Tcl list consisting of JTAG target properties.

    An error is returned when JTAG target selection fails.
}
EXAMPLE {
    jtag targets
        List all targets.

    jtag targets -filter {name == "arm_dap"}
        List targets with name "arm_dap".

    jtag targets 2
        Set target with id 2 as the current target.

    jtag targets -set -filter {name =~ "arm*"}
        Set current target to target with name starting with "arm".

    jtag targets -set -filter {level == 0}
        List JTAG cables.
}
}

    variable sequences {}
    variable sequenceid 0
    proc sequence { args } {
	variable sequences
	variable sequenceid

	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set name "jtagseq#$sequenceid"
	incr sequenceid

	dict set sequences $name [::xsdb::jtag::sequence::create]

	interp alias {} ::$name {} [namespace current]::_sequence_dispatch $name
    }
    namespace export sequence
    ::xsdb::setcmdmeta {jtag sequence} categories {jtag}
    ::xsdb::setcmdmeta {jtag sequence} brief {Create JTAG sequence object.}
    ::xsdb::setcmdmeta {jtag sequence} description {
SYNOPSIS {
    jtag sequence
        Create JTAG sequence object.

DESCRIPTION
    The jtag sequence command creates a new sequence object.  After
    creation the sequence is empty.  The following sequence object
    commands are available:

    sequence state new-state [count]
        Move JTAG state machine to <new-state> and then generate
        <count> JTAG clocks.  If <clock> is given and <new-state> is
        not a looping state (RESET, IDLE, IRSHIFT, IRPAUSE, DRSHIFT or
        DRPAUSE), the state machine will move towards RESET state.

    sequence irshift [options] [bits [data]]
    sequence drshift [options] bits [data]
        Shift data in IRSHIFT or DRSHIFT state.  Data is either given
        as the last argument or if -tdi option is given then data will
        be all zeros or all ones depending on the argument given to
        -tdi.  The <bits> and <data> arguments are not used for
        irshift when the -register option is specified.  Available
        options:

        -register <name>
            Select instruction register by name.  This option is only
            supported for irshift.
        -tdi <value>
            TDI value to use for all clocks in SHIFT state.
        -binary
            Format of <data> is binary, for example data from a file
            or from binary format.
        -integer
            Format of <data> is an integer.  The least significant bit
            of data is shifted first.
        -bits
            Format of <data> is a binary text string.  The first bit
            in the string is shifted first.
        -hex
            Format of <data> is a hexadecimal text string.  The least
            significant bit of the first byte in the string is shifted
            first.
        -capture
            Capture TDO data during shift and return from sequence run
            command.
        -state <new-state>
            State to enter after shift is complete.  The default is RESET.

    sequence delay usec
        Generate delay between sequence commands.  No JTAG clocks will
        be generated during the delay.  The delay is guaranteed to be
        at least <usec> microseconds, but can be longer for cables
        that do not support delays without generating JTAG clocks.

    sequence get_pin pin
        Get value of <pin>.  Supported pins are cable specific.

    sequence set_pin pin value
        Set value of <pin> to <value>.  Supported pins are cable
        specific.

    sequence atomic enable
        Set or clear atomic sequences.  This is useful to creating
        sequences that are guaranteed to run with precise timing or
        fail.  Atomic sequences should be as short as possible to
        minimize the risk of failure.

    sequence run [options]
        Run JTAG operations in sequence for the currently selected
        jtag target.  This command will return the result from shift
        commands using -capture option and from get_pin commands.
        Available options:

        -binary
            Format return value(s) as binary.  The first bit shifted
            out is the least significant bit in the first byte
            returned.
        -integer
            Format return values(s) as integer.  The first bit shifted
            out is the least significant bit of the integer.
        -bits
            Format return value(s) as binary text string.  The first
            bit shifted out is the first character in the string.
        -hex
            Format return value(s) as hexadecimal text string.  The
            first bit shifted out is the least significant bit of the
            first byte of the in the string.
        -single
            Combine all return values as a single piece of data.
            Without this option the return value is a list with one
            entry for every shift with -capture and every get_pin.

    sequence clear
        Remove all commands from sequence.

    sequence delete
        Delete sequence.
}
RETURNS {
    JTAG sequence object.
}
EXAMPLE {
    set seqname [jtag sequence]
    $seqname state RESET
    $seqname drshift -capture -tdi 0 256
    set result [$seqname run]
    $seqname delete
}
}

    variable sequence_commands {state irshift drshift delay get_pin set_pin atomic run clear delete}
    proc _sequence_dispatch {name {cmd ""} args} {
	variable sequences
	variable sequence_commands
	set matches [lsearch -all -inline -glob  $sequence_commands $cmd*]
	if { [llength $matches] == 0 } {
	    error "unknown subcommand \"$cmd\": must be [join $sequence_commands {, }]"
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
	if { ![dict exists $sequences $name] } {
	    error "sequence object \"$name\" is not valid"
	}

	dict with sequences {
	    switch -- $cmd {
		state {
		    if { [llength $args] < 1 || [llength $args] > 2 } {
			error "wrong # args: should be \"state \[count\]\""
		    }
		    set state [lindex $args 0]
		    if { [llength $args] > 1 } {
			set count [lindex $args 1]
		    } else {
			set count 0
		    }
		    ::xsdb::jtag::sequence::state $name $state $count
		    return
		}

		irshift -
		drshift {
		    set options {
			{register "instruction register name" {args 1}}
			{tdi "TDI value" {args 1}}
			{binary "binary data"}
			{integer "integer data"}
			{bits "binary text data"}
			{hex "hexadecimal data"}
			{capture "capture tdo"}
			{state "post shift state" {args 1}}
			{compare "compare value" {args 1}}
			{mask "compare mask" {args 1}}
		    }
		    array set params [::xsdb::get_options args $options 0]

		    if { [info exists params(register)] } {
			if { $cmd != "irshift" } {
			    error "option -register is only available for irshift"
			}
			if { [info exists params(tdi)] ||
			     $params(binary) || $params(integer) || $params(bits) ||
			     $params(hex) } {
			    error "option -register cannot be combined with -tdi, -binary, -integer, -bits or -hex"
			}
		    }
		    if { [info exists params(tdi)] + $params(binary) +
			 $params(integer) + $params(bits) + $params(hex) > 1 } {
			error "conflicting data, use only one of -tdi, -binary, -integer, -bits or -hex"
		    }
		    if { [info exists params(register)] + [info exists params(tdi)] +
			 $params(binary) + $params(integer) + $params(bits) + $params(hex) == 0 } {
			set params(tdi) 0
		    }

		    if { $cmd == "irshift" } {
			set to_ir 1
		    } else {
			set to_ir 0
		    }

		    set props {}
		    if { [info exists params(state)] } {
			dict set props state $params(state)
		    }

		    if { [info exists params(compare)] || [info exists params(mask)] } {
			if { [info exists params(compare)] } {
			    dict set props compare $params(compare)
			} else {
			    dict set props compare 0
			}
			if { [info exists params(mask)] } {
			    dict set props mask $params(mask)
			} else {
			    dict set props mask -1
			}
		    }

		    if { [info exists params(tdi)] } {
			if { [llength $args] != 1 } {
			    error "wrong # args: should be \"shift \[options\] bits\""
			}
			set bits [lindex $args 0]
			dict set props value $params(tdi)
			::xsdb::jtag::sequence::shift $name $to_ir $params(capture) $bits $props
		    } elseif { [info exists params(register)] } {
			if { [llength $args] != 0 } {
			    error "wrong # args: should be \"shift \[options\]\""
			}
			dict set props register $params(register)
			::xsdb::jtag::sequence::shift $name $to_ir $params(capture) 1 $props [i2bin 0 1]
		    } else {
			if { [llength $args] != 2 } {
			    error "wrong # args: should be \"shift \[options\] bits data\""
			}
			set bits [lindex $args 0]
			set data [lindex $args 1]
			if { $params(integer) } {
			    set data [i2bin $data $bits]
			} elseif { $params(bits) } {
			    set data [binary format b* $data]
			} elseif { $params(hex) } {
			    set data [binary format H* $data]
			} else {
			    control::assert {$params(binary)}
			}
			::xsdb::jtag::sequence::shift $name $to_ir $params(capture) $bits $props $data
		    }
		    return
		}

		delay {
		    if { [llength $args] != 1 } {
			error "wrong # args: should be \"delay usec\""
		    }
		    set usec [lindex $args 0]
		    ::xsdb::jtag::sequence::delay $name $usec
		    return
		}

		get_pin {
		    if { [llength $args] != 1 } {
			error "wrong # args: should be \"get_pin pin\""
		    }
		    set pin [lindex $args 0]
		    ::xsdb::jtag::sequence::get_pin $name $pin
		    return
		}

		set_pin {
		    if { [llength $args] != 2 } {
			error "wrong # args: should be \"set_pin pin value\""
		    }
		    set pin [lindex $args 0]
		    set value [lindex $args 1]
		    ::xsdb::jtag::sequence::set_pin $name $pin $value
		    return
		}

		atomic {
		    if { [llength $args] != 1 } {
			error "wrong # args: should be \"atomic enable\""
		    }
		    set enable [lindex $args 0]
		    ::xsdb::jtag::sequence::atomic $name $enable
		    return
		}

		run {
		    set options {
			{binary "return binary data"}
			{integer "integer data"}
			{bits "return binary text data"}
			{hex "return hexadecimal data"}
			{single "return a single data value"}
			{detect "force scan chain detection on next unlock"}
			{type "type of sequence" {args 1}}
			{current-target "run sequence on current target"}
			{node "jtag node context id" {args 1}}
		    }
		    array set params [::xsdb::get_options args $options]

		    set formats [expr $params(binary) + $params(integer) + $params(bits) + $params(hex)]
		    if { $formats > 1 } {
			error "conflicting return format, use only one of -binary, -integer, -bits or -hex"
		    } elseif { $formats == 0 } {
			set params(hex) 1
		    }
		    set chan [::xsdb::getcurchan]

		    if { $params(current-target) + [info exists params(node)] > 1 } {
			error "conflicting options, use only one of -current-target or -node"
		    }
		    if { $params(current-target) } {
			set jc [::xsdb::current_target_jtag_context]
			set node [dict get $jc ID]
		    } else {
			if { [info exists params(node)] } {
			    set node $params(node)
			} else {
			    set node [getcurnode]
			}
			set jc [lindex [::tcf::cache_eval $chan [list get_ctx_data $chan $node Jtag:context]] 1]
		    }
		    if { [dict exists $jc irLen] } {
			set irlen [dict get $jc irLen]
		    } else {
			set irlen 0
		    }
		    if { $irlen > 0 && [dict exists $jc idCode] } {
			set devprops [lindex [::tcf::cache_eval $chan [list get_ctx_data $chan [dict get $jc idCode] JtagDevice:properties]] 1]
		    } else {
			set devprops {}
		    }
		    foreach {fmt cmds data} [::xsdb::jtag::sequence::fmt_cmds_data $name $irlen $devprops] break
		    set sparams {}
		    if { $params(detect) } {
			dict set sparams detect 1
		    }
		    if { [info exists params(type)] } {
			dict set sparams type $params(type)
		    }
		    set result [lindex [::tcf::send_command $chan Jtag sequence $fmt "eB" [list $node $sparams $cmds $data]] 1]

		    if { !$params(single) } {
			set r {}
			set o 0
			foreach cmd $cmds {
			    switch -- [lindex $cmd 0] {
				shift {
				    if { ![lindex $cmd 2] } continue
				    set b [lindex $cmd 3]
				    set l [expr ($b + 7)/8]
				    set v [string range $result $o [expr $o + $l - 1]]
				    incr o $l
				}
				getPin {
				    set b 1
				    set v [string range $result $o $o]
				    incr o 1
				}
				default continue
			    }
			    if { !$params(binary) } {
				if { $params(integer) } {
				    binary scan $v cu* vlist
				    set v 0
				    foreach c [lreverse $vlist] {
					set v [expr {($v << 8) + $c}]
				    }
				} elseif { $params(bits) } {
				    binary scan $v b* v
				    set v [string range $v 0 $b-1]
				} else {
				    control::assert {$params(hex)}
				    binary scan $v H* v
				}
			    }
			    lappend r $v
			}
			if { [string length $result] != $o } {
			    error "unrecognized result value (got [string length $result] handled $o), use -single option"
			}
			set result $r
		    } elseif { !$params(binary) } {
			if { $params(integer) } {
			    binary scan $result cu* vlist
			    set v 0
			    foreach c [lreverse $vlist] {
				set v [expr {($v << 8) + $c}]
			    }
			} elseif { $params(bits) } {
			    binary scan $result b* v
			} else {
			    control::assert {$params(hex)}
			    binary scan $result H* v
			}
			set result $v
		    }
		    return $result
		}

		clear {
		    if { [llength $args] != 0 } {
			error "wrong # args: should be \"clear\""
		    }
		    set $name [::xsdb::jtag::sequence::create]
		    return
		}

		delete {
		    if { [llength $args] != 0 } {
			error "wrong # args: should be \"delete\""
		    }
		    unset $name
		    interp alias {} ::$name {}
		    return
		}
	    }
	}
    }

    # Get/set JTAG properties assoctated for given idcode
    proc device_properties {args} {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # of args: should be \"jtag device_properties idcode or dict\""
	}
	set value [lindex $args 0]
	set chan [::xsdb::getcurchan]
	if { [llength $value] == 1 } {
	    return [lindex [::tcf::send_command $chan JtagDevice getProperties i eo{} [list $value]] 1]
	}
	if { ![dict exists $value idcode] } {
	    error "missing idcode property"
	}
	::tcf::send_command $chan JtagDevice setProperties "o{name s arch_name s bitstream_revisions s i}" e [list $value]
    }
    namespace export device_properties
    ::xsdb::setcmdmeta {jtag device_properties} categories {jtag}
    ::xsdb::setcmdmeta {jtag device_properties} brief {Get/set device properties.}
    ::xsdb::setcmdmeta {jtag device_properties} description {
SYNOPSIS {
    jtag device_properties idcode
        Get JTAG device properties associated with <idcode>.

    jtag device_properties key value ...
        Set JTAG device properties.
}
RETURNS {
    Jtag device properties for the given idcode, or nothing, if the idcode is
    unknown.
}
EXAMPLE {
    jtag device_properties 0x4ba00477
        Return Tcl dict containing device properties for idcode 0x4ba00477.

    jtag device_properties {idcode 0x4ba00477 mask 0xffffffff name dap irlen 4}
        Set device properties for idcode 0x4ba00477.
}
}

    # Lock scan chain
    proc lock {args} {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set chan [::xsdb::getcurchan]
	set node [getcurnode]

	if { [llength $args] > 1 } {
	    error "wrong # args: should be \"jtag lock \[timeout\]\""
	}

	if { [llength $args] == 0 } {
	    ::tcf::send_command $chan Jtag lock s e [list $node]
	} else {
	    ::xsdb::checkint [lindex $args 0]
	    ::tcf::send_command $chan Jtag lock si e [list $node [lindex $args 0]]
	}
    }
    namespace export lock
    ::xsdb::setcmdmeta {jtag lock} categories {jtag}
    ::xsdb::setcmdmeta {jtag lock} brief {Lock JTAG scan chain.}
    ::xsdb::setcmdmeta {jtag lock} description {
SYNOPSIS {
    jtag lock [timeout]
        Lock JTAG scan chain containing current JTAG target.

DESCRIPTION
    Wait for scan chain lock to be available and then lock it.  If
    <timeout> is specified the wait time is limited to <timeout>
    milliseconds.

    The JTAG lock prevents other clients from performing any JTAG
    shifts or state changes on the scan chain.  Other scan chains can
    be used in parallel.

    The jtag run_sequence command will ensure that all commands in the
    sequence are performed in order so the use of jtag lock is only
    needed when multiple jtag run_sequence commands needs to be done
    without interruption.
}
NOTE {
    A client should avoid locking more than one scan chain since this
    can cause dead-lock.
}
RETURNS {
    Nothing.
}
}

    # Unlock scan chain
    proc unlock { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set chan [::xsdb::getcurchan]
	set node [getcurnode]
	::tcf::send_command $chan Jtag unlock s e [list $node]
    }
    namespace export unlock
    ::xsdb::setcmdmeta {jtag unlock} categories {jtag}
    ::xsdb::setcmdmeta {jtag unlock} brief {Unlock JTAG scan chain.}
    ::xsdb::setcmdmeta {jtag unlock} description {
SYNOPSIS {
    jtag unlock
        Unlock JTAG scan chain containing current JTAG target.
}
RETURNS {
    Nothing.
}
}

    # Claim ownership of node
    proc claim {args} {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"jtag claim mask\""
	}
	set mask [lindex $args 0]
	set chan [::xsdb::getcurchan]
	set node [getcurnode]
	::tcf::send_command $chan Jtag claim si e [list $node $mask]
    }
    namespace export claim
    ::xsdb::setcmdmeta {jtag claim} categories {jtag}
    ::xsdb::setcmdmeta {jtag claim} brief {Claim JTAG device.}
    ::xsdb::setcmdmeta {jtag claim} description {
SYNOPSIS {
    jtag claim <mask>
        Set claim mask for current JTAG device.

DESCRIPTION
    This command will attept to set the claim mask for the current
    JTAG device.  If any set bits in <mask> are already set in the
    claim mask then this command will return error "already claimed".

    The claim mask allow clients to negotiate control over JTAG
    devices.  This is different from jtag lock in that 1) it is
    specific to a device in the scan chain, and 2) any clients can
    perform JTAG operations while the claim is in effect.
}
NOTE {
    Currently claim is used to disable the hw_server debugger from
    controlling microprocessors on ARM DAP devices and FPGA devices
    containing Microblaze processors.
}
RETURNS {
    Nothing.
}
}

    # Disclaim ownership of node
    proc disclaim {args} {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"jtag disclaim mask\""
	}
	set mask [lindex $args 0]
	set chan [::xsdb::getcurchan]
	set node [getcurnode]
	::tcf::send_command $chan Jtag disclaim si e [list $node $mask]
    }
    namespace export disclaim
    ::xsdb::setcmdmeta {jtag disclaim} categories {jtag}
    ::xsdb::setcmdmeta {jtag disclaim} brief {Disclaim JTAG device.}
    ::xsdb::setcmdmeta {jtag disclaim} description {
SYNOPSIS {
    jtag disclaim <mask>
        Clear claim mask for current JTAG device.
}
RETURNS {
    Nothing.
}
}

    proc i2bin {i bits} {
	::xsdb::checkint $i
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

    # Get/set JTAG frequency
    proc frequency {args} {
	set options {
	    {list "show supported frequencies"}
	    {test "test frequency" {args 1}}
	    {instructions "IR values use with frequency test" {default {} args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set chan [::xsdb::getcurchan]
	set node [getcurnode]
	if { $params(list) } {
	    if { [llength $args] != 0 } {
		error "wrong # args with -list option"
	    }
	    set freqs [::tcf::send_command $chan Jtag getOption ss eai [list $node frequency_list]]
	    return [lindex $freqs 1]
	}
	if { [info exists params(test)] } {
	    set targets [::tcf::cache_eval $chan [list get_jtag_nodes $chan]]
	    while 1 {
		if { ![dict exists $targets $node Jtag:context] } {
		    error "current JTAG target is accessible"
		}
		set jc [lindex [dict get $targets $node Jtag:context] 1]
		if { ![dict exists $jc isActive] || ![dict get $jc isActive] } {
		    error "current JTAG target is accessible"
		}
		if { ![dict exists $jc ParentID] || [dict get $jc ParentID] == "" } {
		    break
		}
		set node [dict get $jc ParentID]
	    }

	    ::tcf::send_command $chan Jtag lock si e [list $node 2000]
	    catch {
		set curfreq [lindex [::tcf::send_command $chan Jtag getOption ss ei [list $node frequency]] 1]
		set i 0
		set seq [::xsdb::jtag::sequence::create]
		::xsdb::jtag::sequence::state seq RESET 0
		foreach ctx [dict get $targets $node children] {
		    set jc [lindex [dict get $targets $ctx Jtag:context] 1]
		    if { ![dict exists $jc isActive] || ![dict get $jc isActive] } {
			continue
		    }
		    set irlen [dict get $jc irLen]
		    set irval [lindex $params(instructions) $i]
		    if { $irval == "" } {
			set irval -1
		    }
		    ::xsdb::jtag::sequence::shift seq 1 0 $irlen {state IRPAUSE} [i2bin $irval $irlen]
		    incr i
		}
		if { $i == 0 } {
		    error "no active devices on scan chain"
		}
		::xsdb::jtag::sequence::state seq DRSHIFT 0
		foreach {fmt cmds data} [::xsdb::jtag::sequence::fmt_cmds_data seq] break
		::tcf::send_command $chan Jtag sequence $fmt "eB" [list $node {} $cmds $data]

		::tcf::send_command $chan Jtag setOption ssi e [list $node frequency $params(test)]

		set pattern {}
		append pattern [string repeat 0000 1000]
		append pattern [string repeat 1010 1000]
		append pattern [string repeat 1100 1000]
		append pattern [string repeat 1000 1000]
		append pattern [string repeat 1110 1000]
		append pattern [string repeat 1111 1000]
		for {set i 0} {$i < 1000} {incr i} {
		    binary scan [binary format i [expr int(0xffffffff*rand())]] b* val
		    append pattern $val
		}
		set data $pattern[string repeat 0000 1000]
		set seq [::xsdb::jtag::sequence::create]
		::xsdb::jtag::sequence::shift seq 0 1 [string length $data] {state DRSHIFT} [binary format b* $data]
		foreach {fmt cmds data} [::xsdb::jtag::sequence::fmt_cmds_data seq] break
		set result [lindex [::tcf::send_command $chan Jtag sequence $fmt "eB" [list $node {} $cmds $data]] 1]
		binary scan $result b* result

		::tcf::send_command $chan Jtag setOption ssi e [list $node frequency $curfreq]
		if { [string first $pattern $result] < 0 } {
		    error "test pattern missmatch"
		}
		return
	    } msg opt
	    ::tcf::send_command $chan Jtag unlock s e [list $node]
	    return -options $opt $msg
	}
	if { [llength $args] > 1 } {
	    error "wrong # args: should be \"frequency \[frequency\]\""
	}
	if { [llength $args] > 0 } {
	    set freq [lindex $args 0]
	    if { [catch { expr $freq <= 0 }] } {
		error "invalid frequency value: $freq"
	    }
	    set freq [::tcf::send_command $chan Jtag setOption ssi e [list $node frequency $freq]]
	}
	set freq [::tcf::send_command $chan Jtag getOption ss ei [list $node frequency]]
	return [lindex $freq 1]
    }
    namespace export frequency
    ::xsdb::setcmdmeta {jtag frequency} categories {jtag}
    ::xsdb::setcmdmeta {jtag frequency} brief {Get/set JTAG frequency.}
    ::xsdb::setcmdmeta {jtag frequency} description {
SYNOPSIS {
    jtag frequency
        Get JTAG clock frequency for current scan chain.

    jtag frequency -list
        Get list of supported JTAG clock frequencies for current scan chain.

    jtag frequency <frequency>
        Set JTAG clock frequency for current scan chain. This frequency is
        persistent as long as the hw_server is running, and is reset to the
        default value when a new hw_server is started.
}
RETURNS {
    Current JTAG frequency, if no arguments are specified, or if JTAG frequency
    is successfully set.
    Supported JTAG frequencies, if -list option is used.
    Error string, if invalid frequency is specified or frequency cannot be set.
}
}

    # Get/set JTAG skew
    proc skew {args} {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set chan [::xsdb::getcurchan]
	set node [getcurnode]
	if { [llength $args] > 1 } {
	    error "wrong # args: should be \"skew \[clock-skew\]\""
	}
	if { [llength $args] > 0 } {
	    set skew [lindex $args 0]
	    if { [catch { expr $skew <= 0 }] } {
		error "invalid skew value: $skew"
	    }
	    set skew [::tcf::send_command $chan Jtag setOption ssi e [list $node skew $skew]]
	}
	set skew [::tcf::send_command $chan Jtag getOption ss ei [list $node skew]]
	return [lindex $skew 1]
    }
    namespace export skew
    ::xsdb::setcmdmeta {jtag skew} categories {jtag}
    ::xsdb::setcmdmeta {jtag skew} brief {Get/set JTAG skew.}
    ::xsdb::setcmdmeta {jtag skew} description {
SYNOPSIS {
    jtag skew
        Get JTAG clock skew for current scan chain.

    jtag skew <clock-skew>
        Set JTAG clock skew for current scan chain.
}
NOTE {
    Clock skew property is not supported by some JTAG cables.
}
RETURNS {
    Current JTAG clock skew, if no arguments are specified, or if JTAG skew is
    successfully set.
    Error string, if invalid skew is specified or skew cannot be set.
}
}

    # Manange jtag servers
    proc servers {args} {
	set options {
	    {list "list opened servers"}
	    {format "list format of supported servers"}
	    {open "open JTAG server" {args 1}}
	    {close "close JTAG server" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help jtag [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { $params(list) + $params(format) + [info exists params(open)] + [info exists params(close)] > 1 } {
	    error "ambiguous options, use only one of -list, -format, -open, -close"
	}

	set chan [::xsdb::getcurchan]
	set arg [array get params]
	dict set arg chan $chan
	dict set arg nodes {}
	dict lappend arg actions {
	    set nodes {}
	    incr cache_misses [get_context_datatypes nodes $chan {} {JtagCable:serverDescriptions} "" 1]
	    incr cache_misses [get_context_datatypes nodes $chan {JtagCable:openServers} {JtagCable:serverContext} "" 2]
	    incr cache_misses [get_context_datatypes nodes $chan {JtagCable:children} {JtagCable:context} "" 2]
	    if { $cache_misses > 0 } {
		error $::cache_miss_err
	    }
	    incr curaction
	}
	dict set arg result {
	    set nodes
	}
	set nodes [::xsdb::process_tcf_actions $arg]

	if { [info exists params(open)] } {
	    set fields [split $params(open) :]
	    set servernames {}
	    foreach descr [lindex [dict get $nodes "" JtagCable:serverDescriptions] 1] {
		if { ![dict exists $descr ServerID] } continue
		set servername [dict get $descr ServerID]
		lappend servernames $servername
		if { $servername != [lindex $fields 0] } continue
		set paramnames {}
		if { [dict exists $descr ParamNames] } {
		    set paramnames [dict get $descr ParamNames]
		}
		if { [llength $fields] - 1 != [llength $paramnames] } {
		    set format $servername
		    foreach name $paramnames {
			append format ":<$name>"
		    }
		    error "invalid server format, must be $format"
		}
		set arguments {}
		for {set i 0} {$i < [llength $paramnames]} {incr i} {
		    dict set arguments [lindex $paramnames $i] [lindex $fields $i+1]
		}
		::tcf::send_command $chan JtagCable openServer sos es [list $servername $arguments ]
		return ""
	    }
	    error "unknown server, must be [join $servernames {, }]"
	}

	set descrs {}
	foreach descr [lindex [dict get $nodes "" JtagCable:serverDescriptions] 1] {
	    if { ![dict exists $descr ServerID] } continue
	    set serverid [dict get $descr ServerID]
	    dict set descrs $serverid $descr
	}

	if { [info exists params(close)] } {
	    set cableservers {}
	    dict for {ctxid value} $nodes {
		if { ![dict exists $value JtagCable:serverContext] } continue
		set server [lindex [dict get $value JtagCable:serverContext] 1]
		if { ![dict exists $server ID] ||
		     ![dict exists $server ServerID] ||
		     ![dict exists $server isActive] ||
		     ![dict get $server isActive] } continue
		set serverid [dict get $server ServerID]
		set cableserver $serverid
		if { [dict exists $descrs $serverid ParamNames] } {
		    foreach name [dict get $descrs $serverid ParamNames] {
			append cableserver ":[dict get $server Parameters $name]"
		    }
		}
		lappend cableservers $cableserver
		if { $params(close) != $cableserver } continue
		::tcf::send_command $chan JtagCable closeServer s e [list [dict get $server ID]]
		return ""
	    }
	    error "unknown server, must be [join $cableservers {, }]"
	}

	if { $params(format) } {
	    set result ""
	    dict for {serverid descr} $descrs {
		if { [string length $result] != 0 } {
		    append result "\n"
		}
		append result "  $serverid"
		if { [dict exists $descr ParamNames] } {
		    foreach name [dict get $descr ParamNames] {
			append result ":<$name>"
		    }
		}
	    }
	    return $result
	}

	set cables {}
	dict for {ctxid value} $nodes {
	    if { ![dict exists $value JtagCable:context] } continue
	    set cable [lindex [dict get $value JtagCable:context] 1]
	    if { ![dict exists $cable ServerID] ||
		 ![dict exists $cable isActive] ||
		 ![dict get $cable isActive] } continue
	    set serverctx [dict get $cable ServerID]
	    if { ![dict exists $nodes $serverctx JtagCable:serverContext] } continue
	    set server [lindex [dict get $nodes $serverctx JtagCable:serverContext] 1]
	    if { ![dict exists $server ServerID] ||
		 ![dict exists $server isActive] ||
		 ![dict get $server isActive] } continue
	    set serverid [dict get $server ServerID]

	    set cableserver $serverid
	    if { [dict exists $descrs $serverid ParamNames] } {
		foreach name [dict get $descrs $serverid ParamNames] {
		    append cableserver ":[dict get $server Parameters $name]"
		}
	    }
	    dict incr cables $cableserver 1
	}

	set result {}
	dict for {ctxid value} $nodes {
	    if { ![dict exists $value JtagCable:serverContext] } continue
	    set server [lindex [dict get $value JtagCable:serverContext] 1]
	    if { ![dict exists $server ServerID] ||
		 ![dict exists $server isActive] ||
		 ![dict get $server isActive] } continue
	    set serverid [dict get $server ServerID]
	    set cableserver $serverid
	    if { [dict exists $descrs $serverid ParamNames] } {
		foreach name [dict get $descrs $serverid ParamNames] {
		    append cableserver ":[dict get $server Parameters $name]"
		}
	    }
	    set n 0
	    if { [dict exists $cables $cableserver] } {
		set n [dict get $cables $cableserver]
	    }
	    if { [string length $result] != 0 } {
		append result "\n"
	    }
	    append result "  $cableserver cables $n"
	}
	return $result
    }
    namespace export servers
    ::xsdb::setcmdmeta {jtag servers} categories {jtag}
    ::xsdb::setcmdmeta {jtag servers} brief {List, open or close JTAG servers.}
    ::xsdb::setcmdmeta {jtag servers} description {
SYNOPSIS {
    jtag servers [options]
        List, open, and close JTAG servers.  JTAG servers are use to
        implement support for different types of JTAG cables.  An open
        JTAG server will enumerate or connect to available JTAG ports.
}
OPTIONS {
    -list
        List opened servers.  This is the default if no other option
        is given.

    -format
        Return the format of each supported server string.

    -open <server>
        Specifies server to open.

    -close <server>
        Specifies server to close.
}
RETURNS {
    Depends on the options specified.

    <none>, -list
	List of open JTAG servers.

    -format
	List of supported JTAG servers.

    -close
	Nothing if the server is closed, or an error string, if invalid server
        is specified.
}
EXAMPLE {
    jtag servers
        List opened servers and number of associated ports.

    jtag servers -open xilinx-xvc:localhost:10200
        Connect to XVC server on host localhost port 10200.

    jtag servers -close xilinx-xvc:localhost:10200
        Close XVC server for host localhost port 10200.
}
}

    namespace export sequence

    namespace ensemble create -command ::jtag
}

package provide xsdb::jtag $::xsdb::jtag::version
