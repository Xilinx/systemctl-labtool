##################################################################################
# Copyright (c) 2012 - 2022 Xilinx, Inc.  All rights reserved.
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
package require xsdb::device
package require xsdb::tcfinterp

namespace eval ::xsdb::stapl {
    variable version 0.1
    ::xsdb::setcmdmeta stapl brief {STAPL Operations}
    variable stapltable [dict create]
    proc config {args} {
	variable stapltable
	set options {
	    {part "device name" {args 1}}
	    {handle "file handle" {args 1}}
	    {out "output file path" {args 1}}
	    {scan-chain "scan chain info" {args 1}}
	    {checksum "calclates checksum of the stapl data"}
	    {help "command help"}
	}

	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}
	if { !([info exists params(scan-chain)] ^ [info exists params(part)]) || \
	    !([info exists params(handle)] ^ [info exists params(out)]) } {
	    error "invalid arguments, specify -scan-chain/-part and -handle/-out options."
	}
	if { [info exists params(scan-chain)] } {
	    foreach dev $params(scan-chain) {
		if { ![dict exists $dev name] && ![dict exists $dev idcode] } {
			error "missing device parameters, specify name or idcode for all devices"
		}
	    }
	}
	if { [info exists params(out)] } {
	    dict set stapltable $xsdb::curchan "out" $params(out)
	} else {
	    dict set stapltable $xsdb::curchan "handle" $params(handle)
	}
	if { $params(checksum) } {
	    dict set stapltable $xsdb::curchan "checksum" 1
	} else {
	    dict set stapltable $xsdb::curchan "checksum" 0
	}

	set target [format "target%03d" [lindex [split $xsdb::curchan "#"] 1 ]]
	::tcf::send_command $xsdb::curchan svf reqAddTarget "o{name s}" eA [list [dict create name $target]]
	set target_ctx [dict get [lindex [::xsdb::jtag::targets -target-properties -filter \
			{name == "Xilinx Null Cable $target"}] 0] target_ctx]

	if { [info exists params(scan-chain)] } {
	    foreach dev $params(scan-chain) {
		if { [dict exists $dev name] } {
		    if { ![dict exists $dev idcode] } {
			dict set dev idcode 0
		    }
		    if { ![dict exists $dev irlen] } {
			dict set dev irlen 0
		    }
		    if { ![dict exists $dev idcode2] } {
			dict set dev idcode2 0
		    }
		    if { ![dict exists $dev mask] } {
			dict set dev mask 0
		    }
		} else {
		    dict set dev name ""
		    if { ![dict exists $dev irlen] } {
			dict set dev irlen 0
		    }
		    if { ![dict exists $dev idcode2] } {
			dict set dev idcode2 0
		    }
		    if { ![dict exists $dev mask] } {
			dict set dev mask 0
		    }
		}
		::tcf::send_command $xsdb::curchan svf reqAddDevice "o{ctx s name s idcode i irlen i idcode2 i mask i}" eA \
		    [list [dict create ctx $target_ctx name [dict get $dev name] idcode [dict get $dev idcode] irlen [dict get $dev irlen] \
		    idcode2 [dict get $dev idcode2] mask [dict get $dev mask]]]
	    }
	}

	if { [info exists params(part)] } {
	    foreach part $params(part) {
		::tcf::send_command $xsdb::curchan svf reqAddDevice "o{ctx s name s idcode i irlen i idcode2 i mask i}" eA \
		    [list [dict create ctx $target_ctx name $part idcode 0 irlen 0 idcode2 0 mask 0]]
	    }
	}
	return
    }
    namespace export config
    ::xsdb::setcmdmeta {stapl config} categories {stapl}
    ::xsdb::setcmdmeta {stapl config} brief {Configure stapl target.}
    ::xsdb::setcmdmeta {stapl config} description {
SYNOPSIS {
    stapl config <options>
        Create a hw_target (jtag chain) and add all the hw_devices given in
        the scan-chain list to the hw_target. It also configures the stapl
        output file where the stapl data is recorded.
}
OPTIONS {
    -out <filepath>
        Output file path. Only one of the -out and -handle options should be
        used. If the -out option is provided, the file will be explicitly opened
        in a+ mode.

    -handle <filehandle>
        File handle returned by open command for output. Only one of the
        -out and -handle options should be used.

    -scan-chain <list-of-dicts>
        List of devices in the scan-chain. Each list element must be a dict of
        device properties in the format {name <string> idcode <int> irlen <int>
        idcode2 <int> mask <int>}. For example:
            [list [dict create name <device1_name> idcode <idcode> \
            irlen <irlen> idcode2 <idcode2> mask <mask>] [dict create \
            name <device2_name> idcode <idcode> irlen <irlen> idcode2 \
            <idcode2> mask <mask>]]
        The order of devices specified with scan-chain option should match the
        order of devices on the physical hardware where the stapl file is played
        back.
        Only one of the -scan-chain and -part options should be used.

    -part <device-name list>
        List of part names of the Xilinx devices to add to the scan-chain.
        This option works only with Xilinx devices. This option can be
        used instead of the -scan-chain option.

    -checksum
        Calculate stapl-data CRC and append it to the stapl file. If not
        specified, CRC 0 is appended.
}
NOTE {
    For Xilinx devices, if the device_name or idcode is specified in the
    scan-chain information, the other parameters are optional. All the JTAG
    TAPs are added automatically to the scan-chain for Xilinx devices.
}
RETURNS {
    None.
}
EXAMPLE {
    stapl config -handle $fp -scan-chain [list [dict create name xcvc1902 \
          idcode 0 irlen 0 idcode2 0 mask 0] [dict create name xcvm1802 \
          idcode 0 irlen 0 idcode2 0 mask 0]]
        Add xcvc1902 and xcvm1802 devices to scan-chain and use the file
        handle returned by Tcl open command, to record stapl commands.

    stapl config -out mystapl.stapl -scan-chain [list [dict create \
          name xcvc1902 idcode 0 irlen 0 idcode2 0 mask 0] [dict create \
          name xcvm1802 idcode 0 irlen 0 idcode2 0 mask 0]]
        Same as the previous example, but using the stapl file path as input,
            instead of the file handle returned by Tcl open command.

    stapl config -out mystapl.stapl -part xcvc1902
        Add xcvc1902 device to scan-chain, using -part option.

    stapl config -out mystapl.stapl -scan-chain [list [dict create \
          idcode 0x14CA8093 idcode2 1]]
        Same as previous example, but specifying idcode and idcode2, instead of
        the part name.

    stapl config -out mystapl.stapl -part [list xcvc1902 xcvm1802]
        Add xcvc1902 and xcvm1802 devices to scan-chain, using the -part option.

    connect
    stapl config -out mystapl.stapl -scan-chain [list [dict create \
        name xcvc1902 idcode 0 irlen 0 idcode2 0 mask 0]]
    jtag targets -set -filter {name == "xcvc1902"}
    stapl start
    device program <pdipath>
    stapl stop
        The above example demonstrate the correct order for creating a stapl
            file for a single device on a stapl target.

    connect
    stapl config -out mystapl.stapl -scan-chain [list [dict create \
        name xcvc1902 idcode 0 irlen 0 idcode2 0 mask 0] [dict create \
        xcvm1802 idcode 0 irlen 0 idcode2 0 mask 0]]
    jtag targets -set -filter {name == "xcvc1902"}
    targets -set -filter {jtag_device_name == "xcvc1902"}
    stapl start
    device program <pdipath>
    jtag targets -set -filter {name == "xcvm1802"}
    targets -set -filter {jtag_device_name == "xcvm1802"}
    stapl start
    device program <pdipath>
    stapl stop
        The above example demonstrate the correct order for creating a stapl
            file for multiple devices on a stapl target.
}
}

    proc start {args} {
	variable stapltable
	set options {
	    {help "command help"}
	}

	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { $xsdb::curchan == "" } {
	    error "invalid target, use connect command to connect to hw_server"
	}

	if { ![dict exists $stapltable $xsdb::curchan] } {
	    error "run stapl config prior to this command"
	}

	if { [dict exists $stapltable $xsdb::curchan "out"] && ![dict exists $stapltable $xsdb::curchan "handle"] } {
	    dict set stapltable $xsdb::curchan "handle" [open [dict get $stapltable $xsdb::curchan "out"] a+]
	}

	dict set stapltable $xsdb::curchan "done" 0
	::tcf::send_command $xsdb::curchan stapl start s eA [list $::xsdb::jtag::curnode]
	dict set stapltable $xsdb::curchan "started" 1
	::tcf::send_command $xsdb::curchan Xicom configReset so{} e [list $::xsdb::jtag::curnode {}]
	return
    }
    namespace export start
    ::xsdb::setcmdmeta {stapl start} categories {stapl}
    ::xsdb::setcmdmeta {stapl start} brief {Start stapl recording.}
    ::xsdb::setcmdmeta {stapl start} description {
SYNOPSIS {
    stapl start
        Start stapl recording.
}
NOTE {
    It is mandatory to call 'stapl start' before programming each device
    on the scan-chain, and call 'stapl stop' after programming all the devices
    to generate stapl data properly.
}
OPTIONS {
    None.
}
RETURNS {
    None.
}
}

    proc stop {args} {
	variable stapltable
	set options {
	    {help "command help"}
	}

	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [lindex [info level 0] 0]]
	}

	if { $xsdb::curchan == "" } {
	    error "invalid target, use connect command to connect to hw_server"
	}

	if { (![dict exists $stapltable $xsdb::curchan "started"]) || ([dict get $stapltable $xsdb::curchan "started"] == 0) } {
	    error "run stapl start prior to this command"
	}

	dict set stapltable $xsdb::curchan "started" 0

	::tcf::send_command $xsdb::curchan stapl stop s eA [list $::xsdb::jtag::curnode]
	::tcf::send_command $xsdb::curchan stapl close s eA [list $::xsdb::jtag::curnode]
	::xsdb::event_table_handler

	while { [dict get $stapltable $xsdb::curchan "done"] == 0} {
	    after 50
	    ::xsdb::event_table_handler
	}

	if { [dict exists $stapltable $xsdb::curchan "out"] } {
	    close [dict get $stapltable $xsdb::curchan "handle"]
	}
	return
    }
    namespace export stop
    ::xsdb::setcmdmeta {stapl stop} categories {stapl}
    ::xsdb::setcmdmeta {stapl stop} brief {Stop stapl recording.}
    ::xsdb::setcmdmeta {stapl stop} description {
SYNOPSIS {
    stapl stop
        Stop stapl recording.
}
NOTE {
    It is mandatory to call 'stapl start' before programming each device
    on the scan-chain, and call 'stapl stop' after programming all the devices
    to generate stapl data properly.
}
OPTIONS {
    None.
}
RETURNS {
    None.
}
}
    namespace ensemble create -command ::stapl
}

package provide xsdb::stapl $::xsdb::stapl::version
