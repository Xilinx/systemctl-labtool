#######################################################################
# Copyright (c) 2015-2018 Xilinx, Inc.  All rights reserved.
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

if { $argc != 1 } {
    error "wrong # args: should be \"xsdb-reverse-server.tcl url\""
}

set url [lindex $argv 0]

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

    proc xsdb_eval {chan token} {
	variable xsdb_command_table
	if { [catch {::tcf::read $chan s} data] } {
	    puts "xsdb_eval error $token $data"
	    ::tcf::disconnect $chan
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
	    ::tcf::disconnect $chan
	    return
	}

	::tcf::post_event [list ::tcf::send_reply $chan $token "e" [list {}]] $data
    }

    proc xsdb_abort {chan token} {
	variable xsdb_command_table
	if { [catch {::tcf::read $chan {}} data] } {
	    puts "xsdb_eval error $token $data"
	    ::tcf::disconnect $chan
	    return
	}

	::tcf::generate_interrupt

	if { ![info exists xsdb_command_table] } {
	    eval_event xsdb_command_handler
	}
	lappend xsdb_command_table [list $chan $token ::xsdb::abort_clear]
    }

    proc reverse_server_connection_callback {url type data} {
	puts [list reverse_server_connection_callback $url $type $data]
	switch $type {
	    start {
		puts "connection start $data"
		::tcf::on_command $data xsdb eval [list xsdb_eval $data]
		::tcf::on_command $data xsdb after [list xsdb_after $data]
		::tcf::on_command $data xsdb abort [list xsdb_abort $data]
	    }
	}
	connect_tcf_callback $url $type $data
    }

    proc reverse_server_connect {url} {
	::tcf::connect $url [list reverse_server_connection_callback $url]
    }
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

::tcf::eval [list reverse_server_connect $url]

vwait forever
