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
