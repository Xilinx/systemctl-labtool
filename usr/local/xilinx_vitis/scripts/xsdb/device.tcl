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

package require tcf
package require xsdb::tcfinterp


namespace eval ::xsdb::device {
    variable version 0.1
    variable device_status_defs {}
    variable add_plm_log_msg 1

    ::xsdb::setcmdmeta device brief {Device Configuration System}

    variable get_device_action {
    lassign [get_debug_targets_cache_client $chan] targets cache_misses
    lassign [get_jtag_nodes_cache_client $chan] jtag_targets jtag_cache_misses
    incr cache_misses $jtag_cache_misses
    dict for {ctx2 ctx2data} $targets {
        if { [catch {
        set rc [lindex [dict get $ctx2data RunControl:context] 1]
        if { [dict exists $rc JtagDeviceID] } {
            dict set targets $ctx2 JtagDevice:properties [lindex [get_ctx_data $chan [dict get $rc JtagDeviceID] JtagDevice:properties] 1]
        }
        } msg] } {
        if { $msg == $::cache_miss_err } {
            incr cache_misses
        }
        }
    }
    if { $cache_misses > 0 } {
        error $::cache_miss_err
    }

    set rc {}
    if {$ctx ne ""} {
        set rc [lindex [dict get $targets $ctx RunControl:context] 1]
    }

    if {$rc ne {} && [string first "DAP" [dict get $rc Name]] != -1} {
        if { [catch {
        set jtag_ctx [dict get $rc JtagNodeID]
        set jc [lindex [dict get $jtag_targets $jtag_ctx Jtag:context] 1]
        set parent  [dict get $jtag_targets [dict get $jc ParentID]]
        set children [dict get $parent children]
        set index [lsearch $children $jtag_ctx]
        switch [dict get $rc JtagDeviceID] {
            1268778103 {incr index -1}
            1537213559 {incr index}
            1805649015 {incr index}
            default {set index -1}
        }
        if {$index >= 0 && $index < [llength $children]} {
            set ctx2 [lindex $children $index]
            set jc [lindex [dict get $jtag_targets $ctx2 Jtag:context] 1]
            set dp [lindex [get_ctx_data $chan [dict get $jc idCode] JtagDevice:properties] 1]

        }
        } msg] } {
        if { $msg == $::cache_miss_err } {
            incr cache_misses
        }
        }
        if { $cache_misses > 0 } {
            error $::cache_miss_err
        }
        if {[dict exists $dp reg.jconfig]} {
            set node $ctx2
        }
    } elseif { [dict exists $targets $ctx RunControl:context] &&
         [dict exists [lindex [dict get $targets $ctx RunControl:context] 1] DpcTargetID] } {
        set rc [lindex [dict get $targets $ctx RunControl:context] 1]
        if { [dict exists $rc DpcDriverName] && [dict get $rc DpcDriverName] == "dpc-jtag" } {
            set err "JTAG-based DPC target incompatible"
            break
        }
        set node [dict get $rc DpcTargetID]
    } elseif { ![dict exists $targets $ctx JtagDevice:properties] ||
         ![dict exists [dict get $targets $ctx JtagDevice:properties] reg.jconfig] } {
        set devices {}
        dict for {ctx2 ctx2data} $targets {
        if { [dict exists $ctx2data JtagDevice:properties] &&
             [dict exists [dict get $ctx2data JtagDevice:properties] reg.jconfig] } {
            lappend devices $ctx2
        }
        }
        if { [llength $devices] == 1 } {
        set ctx [lindex $devices 0]
        } else {
        if { [llength $devices] == 0 } {
            set err "No supported device found"
        } else {
            set ids {}
            foreach ctx2 $devices {
            lappend ids [dict get $targets $ctx2 target_id]
            }
            set ids [lsort $ids]
            set err "Multiple devices found, please use targets command to select one of: [join $ids {, }]"
        }
        break
        }
    }

    if { ![info exists node] } {
        set rc [lindex [dict get $targets $ctx RunControl:context] 1]
        set jtaggroup [dict get $rc JtagGroup]
        set rc [lindex [dict get $targets $jtaggroup RunControl:context] 1]
        set node [dict get $rc JtagNodeID]
    }

    dict set arg node $node
    incr curaction
    }

    proc program {args} {
        variable curtarget
        variable get_device_action
        variable add_plm_log_msg

        set options {
            {file "program file" {args 1}}
            {partial "partial config"}
            {maxreq "max number pending requests" {default 16 args 1}}
            {chunksize "chuck size" {default 0x4000 args 1}}
            {maxwait "max wait in ms for SBI"}
            {state "return done status"}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options 0]

        if { $params(help) } {
            return [help device [lindex [split [lindex [info level 0] 0] ::] end]]
        }

        # check file param
        if { ![info exists params(file)]} {
            if { [llength $args] > 0 } {
                set params(file) [lindex $args 0]
                set args [lrange $args 1 end]
            } else {
                error "no pdi file specified"
            }
        }

        if { [llength $args] != 0 } {
            error "wrong # args: should be \"pdi device ?options?\""
        }

        set arg [array get params]
        dict set arg chan [::xsdb::getcurchan]
        dict set arg ctx $::xsdb::curtarget
        dict set arg current_bytes 0
        dict set arg total_bytes 0
        dict set arg transfers {}
        dict set arg aborting ""
        dict set arg cancelled 0
        dict set arg state ""

        # Find the 1st device, if the current context is not device and
        # map target context to jtag context
        dict lappend arg actions $get_device_action

        # Open pdi file
        dict lappend arg actions {
            set f [::open $file rb]
            dict set arg f $f
            set total_bytes [::file size $file]
            if { $total_bytes == 0 } {
                set err "empty configuration file"
            }
            incr curaction
        }

        # Initialize device for programming
        dict lappend arg actions {
            if { $numreq > 0 } {
        cache wait
            }
            if { !$partial } {
        if { [catch {eval_progress [list info "initializing"]} msg] } {
            dict set arg err $msg
            break
        }

        send_action_command $argvar Xicom configBegin so{} e [list $node {}] {
            if { [lindex $data 0] != "" } {
            error [lindex $data 0]
            }
        }
        incr numreq 1
            }
            incr curaction
        }

        # Start progress
        dict lappend arg actions {
            if { $numreq > 0 } {
                cache wait
            }
            set ts [clock milliseconds]
            dict set arg start_time $ts
            dict set arg progress_time $ts
            if { [catch {eval_progress [list info [format "%3u%% %4uMB %5.1fMB/s  %s" 0 0 0 "??:?? ETA"]]} msg] } {
                dict set arg err $msg
                break
            }
            incr curaction
        }

        # Program the device
        dict lappend arg actions {
            variable device_program_stage
            if { $aborting != "" } {
                if { !$cancelled } {
                    set cancelled 1
                    send_action_command $argvar Xicom cancel s ei [list $node]
                    incr numreq 1
                }
                if { $numreq > 0 } {
                    cache wait
                }
                set err $aborting
                set device_program_stage ""
                break
            }

            set data [::read $f $chunksize]
            set len [string length $data]
            if { $len == 0 } {
                set err "premature end of file"
                set device_program_stage ""
                incr curaction
                break
            }

            set device_program_stage "config_data"
            send_action_command $argvar Xicom configData sB e [list $node $data] {
                set transfers [dict get $arg transfers]
                foreach {cb len ts} [lindex $transfers 0] break
                dict set arg transfers [lrange $transfers 1 end]

                set progress_delta [expr {$ts - [dict get $arg progress_time]}]
                set complete [expr {100 * $cb / [dict get $arg total_bytes]}]
                set remaining_bytes [expr {[dict get $arg total_bytes] - $cb}]
                if { $progress_delta > 500 || $remaining_bytes == 0 } {
                    dict set arg progress_time $ts
                    set total_time [expr {$ts - [dict get $arg start_time]}]
                    set throughput [expr {$cb / ($total_time / 1000.0)}]
                    if { $remaining_bytes == 0 } {
                        set eta [format "%02u:%02u    " [expr {$total_time / 1000 / 60}] [expr {$total_time / 1000 % 60}]]
                    } elseif { $total_time > 3000 && $throughput > 0 } {
                        set remaining_time [expr {int($remaining_bytes / $throughput)}]
                        set eta [format "%02u:%02u ETA" [expr {$remaining_time / 60}] [expr {$remaining_time % 60}]]
                    } else {
                        set eta [format "??:?? ETA" ]
                    }
                    if { [catch {eval_progress [list info [format "%3u%% %4uMB %5.1fMB/s  %s" $complete [expr {$cb / 1048576}] [expr {$throughput / 1048576}] $eta]]} msg] } {
                        dict set arg aborting $msg
                    }
                }
            }
            incr numreq 1
            incr current_bytes $len
            lappend transfers [list $current_bytes $len [clock milliseconds]]
            if { $total_bytes == $current_bytes } {
                incr curaction
            }
        }

        # Finish the configuration
        dict lappend arg actions {
            send_action_command $argvar Xicom configEnd s e [list $node] {
                if { [lindex $data 0] != "" } {
                    error [lindex $data 0]
                }
                dict set arg init_status 1
            }
            incr numreq 1
            incr curaction
        }

        dict set arg result {
            if { [info exist f] } {
                ::close $f
            }
            #set status
        }

        if { [catch {::xsdb::process_tcf_actions $arg ::xsdb::print_progress} msg] } {
            set config_stage [::tcf::sync_eval {
                variable device_program_stage
                if { ![info exists device_program_stage] } {
                    return ""
                }
                set ret $device_program_stage
                unset device_program_stage
                return $ret
            }]
            if { $add_plm_log_msg && $config_stage != "" } {
                set add_plm_log_msg 0
                append msg "\nRun \"plm log\" command to see PDI boot messages."
                append msg "\n\nThis extra message about \"plm log\" command will not be"
                append msg "\ndisplayed for subsequent \"device program\" errors during"
                append msg "\nthis session.\n"
            }
            error "$msg"
        }
        ::tcf::sync_eval {
            variable device_program_stage
            if { [info exists device_program_stage] } {
                unset device_program_stage
            }
        }
        return
    }
    namespace export program
    ::xsdb::setcmdmeta {device program} categories {device}
    ::xsdb::setcmdmeta {device program} brief {Program PDI/BIT.}
    ::xsdb::setcmdmeta {device program} description {
SYNOPSIS {
    device program <file>
        Program PDI or BIT file into the device.
}
NOTE {
    If no target is selected or if the current target is not a configurable
    device, and only one supported device is found in the targets list, then
    this device will be configured. Otherwise, users will have to select a
    device using targets command.

    device program command is currently supported for Versal devices only. Other
    devices will be supported in future releases.

    For Versal devices, users can run "plm log" to retrieve plm log from memory.
}
RETURNS {
    Nothing, if device is configured, or an error if the configuration failed.
}
}

    proc status {args} {
        variable curtarget
        variable get_device_action

        set options {
            {jreg-name "JTAG register name" {args 1}}
            {hex "Format data in hex"}
            {jtag-target "Jtag target to use instead of current target" {args 1}}
            {slr "Select which SLR to read" {args 1}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options 0]

        if { $params(help) } {
            return [help device [lindex [split [lindex [info level 0] 0] ::] end]]
        }

        # check jreg name param
        if { ![info exists params(jreg-name)]} {
            if { [llength $args] > 0 } {
                set params(jreg-name) [lindex $args 0]
                set args [lrange $args 1 end]
            } else {
                set params(jreg-name) ""
            }
        }

        set arg [array get params]
        dict set arg chan [::xsdb::getcurchan]
        dict set arg ctx $::xsdb::curtarget
        dict set arg current_bytes 0
        dict set arg total_bytes 0
        dict set arg transfers {}
        dict set arg aborting ""
        dict set arg cancelled 0
        dict set arg state ""
        dict set arg slr 0

        set node ""
        if {[info exists params(jtag-target)]} {
            set targets [::xsdb::jtag::targets -target-properties]
            foreach target $targets {
                if {[dict get $target node_id] == $params(jtag-target)} {
                    set node [dict get $target target_ctx]
                    dict set arg node $node
                    break
                }
            }
            if {$node == ""} {
                error "no JTAG target with id $params(jtag-target)"
            }
        }

        if {[info exists params(slr)]} {
            dict set arg slr $params(slr)
        }

        if {$node == ""} {
            # Find the 1st device, if the current context is not device and
            # map target context to jtag context
            dict lappend arg actions $get_device_action
        }

        # Get the list of registers or register status
        dict lappend arg actions {
            set list [lindex [get_ctx_data $chan $node JtagReg:list] 1]

            if { ${jreg-name} == "" } {
                set status {}
                foreach reg $list {
                    set def [lindex [get_ctx_data $chan $reg JtagReg:def] 1]
                    lappend status [dict get $def name]
                }
                dict set arg status $status
            } else {
                set regdef {}
                set regctx ""
                foreach reg $list {
                    set def [lindex [get_ctx_data $chan $reg JtagReg:def] 1]
                    if {${jreg-name} == [dict get $def name]} {
                        set regdef $def
                        set regctx $reg
                        break
                    }
                }
                if {$regdef == {}} {
                    error [format "Could not find JTAG register %s" ${jreg-name}]
                }
                dict set arg regdef $regdef
                send_action_command $argvar Xicom jtagRegGet ssi EB [list $node $regctx $slr] {
                    if { [lindex $data 0] != "" } {
                        dict set arg err [lindex $data 0]
                        return
                    }
                    proc fix_name {name} {
                        return [string toupper [string map {_ { }} $name]]
                    }
                    set regdef [dict get $arg regdef]
                    set bytes [split [lindex $data 1] {}]
                    set status [format "%s: 0x" [fix_name [dict get $regdef name]]]
                    foreach byte [lreverse $bytes] {
                        binary scan $byte H* value
                        append status $value
                    }
                    if {[dict exists $regdef fields]} {
                        set fields [dict get $regdef fields]
                        foreach field $fields {
                            set bits [dict get $field bits]
                            set value 0
                            foreach bit $bits {
                                binary scan [lindex $bytes [expr $bit / 8]] c byte
                                set value [expr ($value << 1) | (($byte >> ($bit % 8)) & 1)]
                            }
                            set name [fix_name [dict get $field name]]
                            set bit_range [lindex $bits 0]
                            set bit_len [llength $bits]
                            if {$bit_len > 1} {
                                set bit_range "[lindex $bits 0]:[lindex $bits end]"
                            }
                            if {[dict get $arg hex]} {
                                set fs "\%0[expr ($bit_len + 3) / 4]x"
                                set value [format $fs $value]
                            } else {
                                binary scan [i2bin $value $bit_len] b${bit_len} bitval
                                set value [string reverse $bitval]
                            }
                            dict set props "$name (Bits \[$bit_range\])" $value
                        }

                        set max_len 0
                        dict for {name value} $props {
                            if { $max_len < [string length $name] } {
                                set max_len [string length $name]
                            }
                        }
                        dict for {name value} $props {
                            if { $status != "" } {
                                append status "\n"
                            }
                            append status [format "  %*s: %s" $max_len $name $value]
                        }
                    }
                    dict set arg status $status
                }
                incr numreq 1
            }
            incr curaction
        }

        dict set arg result {
            set status
        }
        return [::xsdb::process_tcf_actions $arg]
    }
    namespace export status
    ::xsdb::setcmdmeta {device status} categories {device}
    ::xsdb::setcmdmeta {device status} brief {Return JTAG register status.}
    ::xsdb::setcmdmeta {device status} description {
SYNOPSIS {
    device status [options] <jtag-register-name>
        Return device JTAG Register status, or list of available registers
        if no name is given.
}
OPTIONS {
    -jreg-name <jtag-register-name>
        Specify jtag register name to read. This is the default option, so
        register name can be directly specified as an argument without using
        this option.

    -jtag-target <jtag-target-id>
        Specify jtag target id to use instead of the current target.  This is primarily 
        used when there isn't a valid target option.

    -hex
        Format the return data in hexadecimal.
}
RETURNS {
    Status report.
}
}

    proc authjtag {args} {
        variable curtarget
        variable get_device_action

        set options {
            {file "secure debug file" {args 1}}
            {jtag-target "Jtag target to use instead of current target" {args 1}}
            {help "command help"}
        }
        array set params [::xsdb::get_options args $options 0]

        if { $params(help) } {
            return [help device [lindex [split [lindex [info level 0] 0] ::] end]]
        }

        # check file param
        if { ![info exists params(file)]} {
            if { [llength $args] > 0 } {
                set params(file) [lindex $args 0]
                set args [lrange $args 1 end]
            } else {
                error "no secure debug file specified"
            }
        }

        if { [llength $args] != 0 } {
            error "wrong # args: should be \"device authjtag ?options? <file>\""
        }

        set arg [array get params]
        dict set arg chan [::xsdb::getcurchan]
        dict set arg ctx $::xsdb::curtarget
        dict set arg current_bytes 0
        dict set arg total_bytes 0
        dict set arg transfers {}
        dict set arg aborting ""
        dict set arg cancelled 0
        dict set arg state ""

        set node ""
        if {[info exists params(jtag-target)]} {
            set targets [::xsdb::jtag::targets -target-properties]
            foreach target $targets {
                if {[dict get $target node_id] == $params(jtag-target)} {
                    set node [dict get $target target_ctx]
                    dict set arg node $node
                    break
                }
            }
            if {$node == ""} {
                error "no JTAG target with id $params(jtag-target)"
            }
        }

        if {$node == ""} {
            # Find the 1st device, if the current context is not device and
            # map target context to jtag context
            dict lappend arg actions $get_device_action
        }

        # Open file
        dict lappend arg actions {
            set f [::open $file rb]
            dict set arg f $f
            set total_bytes [::file size $file]
            if { $total_bytes == 0 } {
                set err "empty secure debug file"
            }
            incr curaction
        }

        # Secure debug the device
        dict lappend arg actions {
            set data [::read $f $total_bytes]
            set len [string length $data]
            if { $len == 0 } {
                set err "premature end of file"
                incr curaction
                break
            }

            send_action_command $argvar Xicom secureDebug sB e [list $node $data] {}
            incr numreq 1
            incr current_bytes $len
            lappend transfers [list $current_bytes $len [clock milliseconds]]
            if { $total_bytes == $current_bytes } {
                incr curaction
            }
        }

        dict set arg result {
            if { [info exist f] } {
                ::close $f
            }
            #set status
        }
        return [::xsdb::process_tcf_actions $arg ::xsdb::print_progress]
    }
    namespace export authjtag
    interp alias {} ::xsdb::device::secdebug {} ::xsdb::device::authjtag
    namespace export secdebug
    ::xsdb::setcmdmeta {device authjtag} categories {device}
    ::xsdb::setcmdmeta {device authjtag} brief {Secure debug BIN.}
    ::xsdb::setcmdmeta {device authjtag} description {
SYNOPSIS {
    device authjtag <file>
        Unlock device for secure debug.
}
OPTIONS {
    -jtag-target <jtag-target-id>
        Specify jtag target id to use instead of the current target.  This is primarily 
        used when there isn't a valid target option.
}
NOTE {
    If no target is selected or if the current target is not a configurable
    device, and only one supported device is found in the targets list, then
    this device will be configured. Otherwise, users will have to select a
    device using targets command.

    device authjtag command is currently supported for Versal devices only.
}
RETURNS {
    Nothing, if secure debug is successful, or an error if failed.
}
}


    namespace ensemble create -command ::device
}

package provide xsdb::device $::xsdb::device::version
