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

# This script can be sourced in xsdb to start a TCF server supporting
# an xsdb service.  TCF clients can connect to this server and issue
# xsdb service commands, like xsdb/eval, to evaluate xsdb commands.
#
# The default behavior when sourcing this file is to block forever in
# vwait to service incomming requests.  When no client connection is
# active for 10 seconds, i.e. the xsdb server is idle, exit is called
# to terminate this xsdb process.  This behavior can be modified by
# setting the variable xsdb_idle_timeout before sourcing this file.
#
# When set to -1, vwait is not called and no idle timer is used.  This
# enables the server while also allowing interactive user commands.
#
# When set to 0, no idle timer is used and the server runs until
# killed.
#
# When set to any value greater than 0 this value is used as the idle
# timeout in seconds.

if { [info exists xsdb_num_connections] } {
    error "xsdb-server already started"
}

::tcf::sync_eval {
    proc get_xsdb_command_table {} {
	variable xsdb_command_table
	if { [info exists xsdb_command_table] } {
	    set ret $xsdb_command_table
	    unset xsdb_command_table
	} else {
	    set ret {}
	}
	return $ret
    }

    proc clear_abort_pending {} {
	variable xsdb_abort_pending
	if { [info exists xsdb_abort_pending] } {
	    unset xsdb_abort_pending
	}
    }

    proc xsdb_eval {chan token} {
	variable xsdb_command_table
	if { [catch {::tcf::read $chan s} data] } {
	    puts "xsdb_eval error $token $data"
	    ::tcf::disconnect $chan [list eval_event {puts "Channel closed"}]
	    return
	}

	if { ![info exists xsdb_command_table] } {
	    eval_event xsdb_command_handler
	}
	lappend xsdb_command_table [list $chan $token [lindex $data 0]]
    }

    proc xsdb_after {chan token} {
	if { [catch {::tcf::read $chan i} data] } {
	    puts "xsdb_eval error $token $data"
	    ::tcf::disconnect $chan [list eval_event {puts "Channel closed"}]
	    return
	}

	::tcf::post_event [list ::tcf::send_reply $chan $token "e" [list {}]] $data
    }

    proc xsdb_abort {chan token} {
	variable xsdb_abort_pending
	if { [catch {::tcf::read $chan {}} data] } {
	    puts "xsdb_eval error $token $data"
	    ::tcf::disconnect $chan [list eval_event {puts "Channel closed"}]
	    return
	}

	::tcf::generate_interrupt
	::tcf::send_reply $chan $token "e" [list {}]
	if { ![info exists xsdb_abort_pending] } {
	    eval_event xsdb_abort_clear
	}
	set xsdb_abort_pending 1
    }

    proc tcf_connection_callback {type data} {
	switch $type {
	    start {
		eval_event [list xsdb_connect_handler $data]
		::tcf::on_command $data xsdb eval [list xsdb_eval $data]
		::tcf::on_command $data xsdb after [list xsdb_after $data]
		::tcf::on_command $data xsdb abort [list xsdb_abort $data]
		::tcf::on_disconnect $data [list eval_event [list xsdb_disconnect_handler $data]]
	    }
	    error {
	    }
	    connect {
	    }
	}
    }

    proc server_connect {} {
	set serverid [::tcf::server_start tcp:localhost:0 tcf_connection_callback]
	set props [::tcf::get_server_properties $serverid]
	if { [dict exists $props Host] && [dict get $props Host] != "" } {
	    set host [dict get $props Host]
	} else {
	    set host [info hostname]
	}
	return "[dict get $props TransportName]:$host:[dict get $props Port]"
    }
}

proc xsdb_abort_clear {} {
    if { [catch [list namespace eval :: ::xsdb::abort_clear] msg] } {
	puts "err: $msg"
    }
    ::tcf::sync_eval clear_abort_pending
}

proc xsdb_command_handler {} {
    foreach command [::tcf::sync_eval get_xsdb_command_table] {
	set chan [lindex $command 0]
	set token [lindex $command 1]
	set script [lindex $command 2]
	if { [catch [list namespace eval :: $script] msg] } {
	    ::tcf::sync_eval [list ::tcf::send_reply $chan $token "es" [list $msg {}]]
	} else {
	    ::tcf::sync_eval [list ::tcf::send_reply $chan $token "es" [list {} $msg]]
	}
    }
}

proc xsdb_connect_handler {chan} {
    variable xsdb_num_connections
    incr xsdb_num_connections 1
    puts "XSDB Server Channel: $chan"
}

proc xsdb_disconnect_handler {chan} {
    variable xsdb_num_connections
    incr xsdb_num_connections -1
}

proc xsdb_check_idle_timeout {} {
    variable xsdb_num_connections
    variable xsdb_idle_timeout
    variable xsdb_idle_count
    if { $xsdb_num_connections == 0 } {
	incr xsdb_idle_count 1
        if { $xsdb_idle_count >= $xsdb_idle_timeout} {
	    eval {exit}
	}
    } else {
	set xsdb_idle_count 0
    }
    after 1000 xsdb_check_idle_timeout
}

variable xsdb_num_connections 0
variable xsdb_idle_timeout
variable xsdb_idle_count 0
if { ![info exists xsdb_idle_timeout] } {
    # default to 10 seconds if not set by user
    variable xsdb_idle_timeout 10
}
if { $xsdb_idle_timeout > 0 } {
    after 1000 xsdb_check_idle_timeout
}

puts "XSDB Server URL: [::tcf::sync_eval server_connect]"

if { $xsdb_idle_timeout >= 0 } {
    vwait forever
}
