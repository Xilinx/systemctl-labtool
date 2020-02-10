#######################################################################
# Copyright (c) 2013-2018 Xilinx, Inc.  All rights reserved.
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

package require Tcl 8.5

namespace eval tcf {
    variable version 0.1

    set loaded 0
    foreach d $::auto_path {
	if { ![catch {load [file join $d libtcltcf[info sharedlibextension]]}] } {
	    set loaded 1
	    break
	}
    }
    if { !$loaded } {
	load libtcltcf[info sharedlibextension]
    }

    ::tcf::eval {
        proc tcf_connect_callback {type data} {
            switch $type {
                error {eval_error $data}
                connect {eval_done $data}
            }
        }

        proc tcf_redirect_callback {chan err} {
	    if { [string length $err] > 0 } {
		eval_error $err
	    } else {
		eval_done
	    }
        }

        proc tcf_command_callback {chan fmt err} {
	    if { [string length $err] > 0 } {
		eval_error $err
	    } elseif { [catch {::tcf::read $chan $fmt} data] } {
		eval_error $data
	    } else {
		eval_done $data
	    }
        }

        proc tcf_send_command {chan service name argfmt resfmt arglist} {
            ::tcf::send_command $chan $service $name [list tcf_command_callback $chan $resfmt]
	    if { [catch {::tcf::write $chan $argfmt $arglist} msg] } {
		# generate EOM if write failed to avoid assert
		::tcf::write $chan "" {}
		eval_progress "channel write error: $msg"
	    }
        }

        proc tcf_async_command_callback {chan fmt callback err} {
	    if { [string length $err] > 0 } {
		set data {}
	    } elseif { [catch {::tcf::read $chan $fmt} data] } {
		set err $data
		set data {}
	    }
	    eval $callback $err $data
        }

        proc tcf_async_send_command {chan service name argfmt resfmt arglist callback} {
            ::tcf::send_command $chan $service $name [list tcf_async_command_callback $chan $resfmt $callback]
	    if { [catch {::tcf::write $chan $argfmt $arglist} msg] } {
		# generate EOM if write failed to avoid assert
		::tcf::write $chan "" {}
		eval_progress "channel write error: $msg"
	    }
	    eval_done
        }

        proc tcf_sync_eval {_script} {
            if { [catch $_script msg] != 1 } {
		eval_done $msg
	    } else {
                eval_error $msg
            }
        }

        proc tcf_cache_eval {_script} {
            if { [catch $_script msg opt] != 1 } {
		eval_done $msg
	    } else {
                if { $msg == "CACHE_MISS" } {
                    return -options $opt $msg
                }
                eval_error $msg
            }
        }

        eval_done
    }

    proc sync_eval {script} {
        ::tcf::eval [list tcf_sync_eval $script]
    }
    namespace export sync_eval

    proc sync_eval_with_progress {script progress} {
        ::tcf::eval -progress $progress [list tcf_sync_eval $script]
    }
    namespace export sync_eval_with_progress

    proc cache_eval {chan script} {
        ::tcf::eval [list ::tcf::cache_enter $chan [list tcf_cache_eval $script]]
    }
    namespace export cache_eval

    proc cache_eval_with_progress {chan script progress} {
        ::tcf::eval -progress $progress [list ::tcf::cache_enter $chan [list tcf_cache_eval $script]]
    }
    namespace export cache_eval_with_progress

    proc connect {peer} {
	::tcf::eval [list ::tcf::connect $peer tcf_connect_callback]
    }
    namespace export connect

    proc redirect {chan peer} {
	::tcf::eval [list ::tcf::redirect $chan $peer [list tcf_redirect_callback $chan]]
    }

    proc disconnect {chan} {
	::tcf::eval [list ::tcf::disconnect $chan eval_done]
    }
    namespace export disconnect

    proc send_command {chan service name argfmt resfmt arglist} {
	::tcf::eval -progress {apply {{msg} {puts $msg}}} \
	    [list tcf_send_command $chan $service $name $argfmt $resfmt $arglist]
    }
    namespace export send_command

    proc async_send_command {chan service name argfmt resfmt arglist callback} {
	::tcf::eval -progress {apply {{msg} {puts $msg}}} \
	    [list tcf_async_send_command $chan $service $name $argfmt $resfmt $arglist $callback]
    }
    namespace export async_send_command

    namespace ensemble create -command ::tcf
    namespace export tcf
}

package provide tcf $::tcf::version
