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

package require tcf

namespace eval ::xsdb::gdbremote {
    variable version 0.1

    # Connect to gdbremote server
    proc connect {args} {
	set options {
	    {multiprocess "enable/disable multiprocess mode" {args 1}}
	    {extended "enable/disable extended mode" {args 1}}
	    {auto_attach "enable/disable auto attach mode" {args 1}}
	    {architecture "specify default architecture" {args 1}}
	    {osabi "specify default osabi" {args 1}}
	    {log "specify log level" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help gdbremote [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"gdbremote connect \[options\] <server>\""
	}

	if { [catch {
	    set chan [::xsdb::getcurchan]
	    set services [::tcf::sync_eval [list ::tcf::get_services $chan]]
	    if { [lsearch $services GdbClient] < 0 } {
		error "gdbremote connect not supported"
	    }
	}] } {
	    # No connection or current connection does not support
	    # GdbClient service - launch and connect to xrt_server
	    # process to get access to GdbClient service.
	    set chan [::xsdb::connect -server xrt_server -port 0]
	}

	set fields [split [lindex $args 0] :]
	if { [string compare -nocase tcp [lindex $fields 0]] == 0 } {
	    set fields [lrange $fields 1 end]
	}
	set host [lindex $fields 0]
	set port [lindex $fields 1]
	if { $host == "" } {
	    set host "127.0.0.1"
	}

	set arg [array get params]
	dict set arg host $host
	dict set arg port $port
	if {[dict exists $arg help]} {
	    set arg [dict remove $arg help]
	}
	::tcf::send_command $chan GdbClient connect "o{host s port s multiprocess b extended b auto_attach b architecture s osabi s log i}" es [list $arg]
	return ""
    }
    namespace export connect
    ::xsdb::setcmdmeta {gdbremote connect} categories {connections}
    ::xsdb::setcmdmeta {gdbremote connect} brief {Connect to GDB remote server.}
    ::xsdb::setcmdmeta {gdbremote connect} description {
SYNOPSIS {
    gdbremote connect [options] server
        Connect to a GDB remote server, for example qemu.
        xrt_server is used to connect to remote GDB server.
}
OPTIONS {
    -architecture <name>
        Specify default architecture is remote server does not provide it.
}
DESCRIPTION {
    This command attempts to connect the GDB Remote Server.
}
RETURNS {
    Nothing, if the connection is successful.
    Error string, if the connection failed.
}
}

    # Disconnect to gdbremote server
    proc disconnect {args} {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help gdbremote [lindex [split [lindex [info level 0] 0] ::] end]]
	}
	if { [llength $args] > 1 } {
	    error "wrong # args: should be \"gdbremote disconnect \[target-id\]\""
	}

	set chan [::xsdb::getcurchan]
	set targets [targets -target-properties]
	set match {}
	set l0target {}
	if { [llength $args] == 0 } {
	    foreach target $targets {
		if { [dict exists $target level] && [dict get $target level] == 0 } {
		    set l0target $target
		}
		if { [dict exists $target is_current] && [dict get $target is_current] } {
		    set match $l0target
		    break
		}
	    }
	} else {
	    set tid [lindex $args 0]
	    foreach target $targets {
		if { [dict exists $target level] && [dict get $target level] == 0 } {
		    set l0target $target
		}
		if { [dict exists $target target_id] && [dict get $target target_id] == $tid } {
		    set match $l0target
		    break
		}
	    }
	}
	if { $match == {} || ![dict exists $match target_ctx] || [dict get $match target_ctx] == "" } {
	    error "Invalid target. Use \"targets\" command to select a target"
	}
	::tcf::send_command $chan GdbClient disconnect "s" e [list [dict get $match target_ctx]]
	return ""
    }
    namespace export disconnect
    ::xsdb::setcmdmeta {gdbremote disconnect} categories {connections}
    ::xsdb::setcmdmeta {gdbremote disconnect} brief {Disconnect from GDB remote server.}
    ::xsdb::setcmdmeta {gdbremote disconnect} description {
SYNOPSIS {
    gdbremote disconnect [target-id]
        Disconnect from GDB remote server, for example qemu.
}
DESCRIPTION {
    This command disconnects GDB Remote Server for current or specified target.
}
RETURNS {
    Nothing, if the connection is close.
    Error string, if there is no active connection.
}
}

    namespace ensemble create -command ::gdbremote
}

package provide xsdb::gdbremote $::xsdb::gdbremote::version
