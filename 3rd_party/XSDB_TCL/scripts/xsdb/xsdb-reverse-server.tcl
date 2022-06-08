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
