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

package require Tcl 8.5

namespace eval tcf {
    variable version 0.1

    # Temp changes to support gradle builds, which creates the libs with
    # libxv_ prefix instead of lib prefix. gradle build scripts search and
    # replace "set GRADLE_BUILD 0" with "set GRADLE_BUILD 1" during build,
    # so libxv_* libs are loaded when the tool is run from gradle output
    set GRADLE_BUILD 0
    if { $GRADLE_BUILD } {
	if { $::tcl_platform(platform) == "windows" } {
	    set lib_name [join xv_tcltcf[info sharedlibextension]]
	} else {
	    set lib_name [join libxv_tcltcf[info sharedlibextension]]
        }
    } else {
	set lib_name [join libtcltcf[info sharedlibextension]]
    }

    set loaded 0
    set pkg_name "Tcltcf"
    foreach d $::auto_path {
	if { ![catch {load [file join $d $lib_name] $pkg_name}] } {
	    set loaded 1
	    break
	}
    }
    if { !$loaded } {
	load $lib_name $pkg_name
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
