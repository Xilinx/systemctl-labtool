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

::tcf::sync_eval {
    proc sdk_command_callback {err} {
	puts "sdk_command_callback"
	variable sdk_cmd_arg
	set arg $sdk_cmd_arg
	unset sdk_cmd_arg
	if { [string length $err] > 0 } {
	    set data [list $err]
	} elseif { [catch {::tcf::read [dict get $arg chan] "es"} data] } {
	    set data [list $data]
	}
	eval_event [list sdk_command_handler $data]
    }

    proc sdk_command_eval_client {arg} {
	puts "sdk_command_eval_client"
	variable sdk_cmd_arg
	if { [info exists sdk_cmd_arg] } {
	    error "sdk command already in progress"
	}
	set chan [dict get $arg chan]
	if { [catch {
	    ::tcf::send_command $chan xsdk eval [list sdk_command_callback]
	    ::tcf::write $chan "s" [list [dict get $arg script]]
	} msg opt] } {
	    puts [list $msg $opt]
	    dict set arg err $msg
	}
	set sdk_cmd_arg $arg
    }
}

proc sdk_command_handler { data } {
    puts "sdk_cmd_result"
    variable sdk_cmd_result
    set sdk_cmd_result $data
}

proc xsdk_eval {script} {
    variable sdk_cmd_result
    if { [info exists sdk_cmd_result] } {
	error "sdk command already in progress"
    }
    catch {
	set sdk_cmd_result pending
	set arg [dict create chan $xsdb::curchan script $script]
	::tcf::sync_eval [list sdk_command_eval_client $arg]
	vwait sdk_cmd_result
	set sdk_cmd_result
    } msg opt
    unset sdk_cmd_result
    return -options $opt $msg
}
