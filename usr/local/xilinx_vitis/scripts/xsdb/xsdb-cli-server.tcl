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

set cli_server_url [::tcf::sync_eval {
    variable cli_server_channel

    proc cli_server_connection_callback {type data} {
	switch $type {
	    start {
		if { [info exists cli_server_channel] } {
		    puts "CLI already connected"
		    ::tcf::disconnect $data {apply {{} {}}}
		    return
		}
		set cli_server_channel $data
		::tcf::on_disconnect $data {
		    variable cli_server_channel
		    unset cli_server_channel
		}
	    }
	    error {
		puts "connection error $data"
	    }
	    connect {
		puts "connection okay $data"
	    }
	}
    }
    set serverid [::tcf::server_start tcp:localhost:0 cli_server_connection_callback]
    set props [::tcf::get_server_properties $serverid]
    if { [dict exists $props Host] && [dict get $props Host] != "" } {
	set host [dict get $props Host]
    } else {
	set host [info hostname]
    }
    return "[dict get $props TransportName]:$host:[dict get $props Port]"
}]

