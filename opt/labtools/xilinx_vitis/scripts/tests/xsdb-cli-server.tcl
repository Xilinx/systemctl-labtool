#######################################################################
# Copyright (c) 2015-2021 Xilinx, Inc.  All rights reserved.
# Copyright (c) 2022-2023 Advanced Micro Devices, Inc. All rights reserved.
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

