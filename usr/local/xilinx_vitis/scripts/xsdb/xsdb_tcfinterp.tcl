##################################################################################
# Copyright (c) 2012 - 2022 Xilinx, Inc.  All rights reserved.
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

#
# xsdb code running in TCF thread interpretor
#
# All code in the TCF interp honour the asynchronous programming model
# used by TCF.
#

namespace eval ::xsdb::tcfinterp {
    variable version 0.1
}

variable ::cache_miss_err "CACHE_MISS"

proc event_notify {ctx type state} {
    set tcfclients [regexp -all -inline "::tcfclient#\[0-9\]*" [namespace children ::]]
    foreach ns $tcfclients {
	namespace eval $ns {
	    variable event_table
	    if { ![info exists event_table] } {
		eval_event {::xsdb::event_table_handler}
	    }
	    dict set event_table [uplevel 1 {set ctx}] [uplevel 1 {set type}] [uplevel 1 {set state}]
	}
    }
}

proc dispose_exprs {chan ctx scope} {
    set tcfclients [regexp -all -inline "::tcfclient#\[0-9\]*" [namespace children ::]]
    foreach ns $tcfclients {
	namespace eval $ns {
	    global [namespace current]::expr_list

	    set chan [uplevel 1 {set chan}]
	    set ctx [uplevel 1 {set ctx}]
	    set scope [uplevel 1 {set scope}]
	    if { $scope == "all" } { dict set expr_list $chan $ctx {} }
	    set ids [get_expr_ids $chan $ctx "" $scope]

	    if { [llength $ids] } {
		set expr_arg [dict create chan $chan ctx $ctx ids $ids maxreq 256 numreq 0 curpos 0 err ""]
		set argvar "[namespace current]::expr_arg"
		if { [catch {::tcf::cache_enter [dict get $expr_arg chan] [list expr_dispose_cache_client $argvar]} msg opt] } {
		    catch {unset $argvar}
		    puts stderr $opt
		}
	    }
	}
    }
}

proc notify_newctxs {chan newctxs} {
    set tcfclients [regexp -all -inline "::tcfclient#\[0-9\]*" [namespace children ::]]
    foreach ns $tcfclients {
	namespace eval $ns {
	    variable newctxs_chanlist
	    if { ![info exists newctxs_chanlist] } {
		eval_event {::xsdb::ctx_added_handler}
	    }
	    dict lappend newctxs_chanlist [uplevel 1 {set chan}] [uplevel 1 {set newctxs}]
	}
    }
}

proc ::add_channel {chan url} {
    if { [namespace exists ::channels::$chan] } {
	error "channel '$chan' already added"
    }

    namespace eval ::channels::$chan {
	# Dictionary of context information keyed by context id.
	# Value for each context id is a dictionary keyed by data
	# type.  Value for each data type is specific to each data
	# type.  If context id or data type is not in the cache
	# then it means that it is invalid.

	namespace eval [namespace current]::datatypes {}

	variable chan [uplevel 1 {set chan}]
	variable url [uplevel 1 {set url}]
	variable ctxs [dict create "" [dict create children {} ]]
	variable next_target_id 0
	variable target_ctx_map {}
	variable ctx_target_map {}
	variable next_process_id 0
	variable process_ctx_map {}
	variable ctx_process_map {}
	variable next_node_id 0
	variable node_ctx_map {}
	variable ctx_node_map {}
	variable invalidate_level 0
	variable invalidate_done {}
	variable invalidate_types {}
	variable invalidate_pending {}

	# This function evaluates a body of cache invalidation code.
	# Grouping cache invalidation improves performance by only
	# performing cleanup functions at the end of the outer group.
	proc invalidate_group {body} {
	    variable chan
	    variable ctxs
	    variable invalidate_level
	    variable invalidate_done
	    variable invalidate_types
	    variable invalidate_pending

	    if { $invalidate_level == 0 } {
		if { [catch {
		    control::assert {[dict size $invalidate_done] == 0}
		    control::assert {[dict size $invalidate_types] == 0}
		    control::assert {[dict size $invalidate_pending] == 0}
		} msg opt] } {
		    puts stderr $opt
		}
	    }
	    incr invalidate_level
	    set code [uplevel 1 [list catch $body _invalidate_body_result _invalidate_body_options]]
	    upvar _invalidate_body_result result
	    upvar _invalidate_body_options options

	    if { $invalidate_level == 1 } {
		if { [catch {
		    while 1 {
			set pending $invalidate_pending
			set invalidate_pending {}
			if { [dict size $pending] > 0 } {
			    dict for {ctx types} $pending {
				invalidate_datatypes $ctx [dict keys $types] 1
			    }
			} else {
			    dict for {ctx data} $ctxs {
				if { [dict exists $data parent] } continue
				invalidate_datatypes $ctx [dict keys $invalidate_types] 1
			    }
			    if { [dict size $invalidate_pending] == 0 } break
			}
		    }
		    dict for {ctx dummy} $invalidate_done {
			if { [dict exists $ctxs $ctx children] &&
			     [dict size [dict get $ctxs $ctx]] == 1 &&
			     [dict size [dict get $ctxs $ctx children]] == 0 &&
			     $ctx != "" } {
			    dict unset ctxs $ctx
			}
		    }
		} msg opt] } {
		    puts stderr $opt
		}
		set invalidate_done {}
		set invalidate_types {}
		set invalidate_pending {}
	    }
	    incr invalidate_level -1

	    return -options $options -code $code $result
	}

	# Mark children of ctx to be invalidated as part of
	# invalidation group.
	proc invalidate_children {ctx type} {
	    variable ctxs
	    variable invalidate_level
	    variable invalidate_done
	    variable invalidate_types
	    variable invalidate_pending

	    control::assert {$invalidate_level > 0}

	    dict set invalidate_types $type 1
	    if { [dict exists $ctxs $ctx children] } {
		dict for {childctx types} [dict get $ctxs $ctx children] {
		    if { ![dict exists $invalidate_done $childctx $type] } {
			dict set invalidate_pending $childctx $type 1
		    }
		}
	    }
	}

	# Invalidate datatypes for ctx and possibly its children.
	proc invalidate_datatypes {ctx datatypes deep} {
	    variable chan
	    variable ctxs
	    variable invalidate_done

	    invalidate_group {
		foreach type $datatypes {
		    if { $deep } {
			invalidate_children $ctx $type
		    }
		    dict set invalidate_done $ctx $type 1
		    ::channels::[set chan]::datatypes::[set type]::invalidate $ctx
		}
	    }
	}

	# Update list of children for type and invalidate any child
	# contexts that are no longer referenced.
	proc update_relations {ctx type invtypes children} {
	    global [namespace current]::ctxs

	    set oldchildren {}
	    if { [dict exists $ctxs $ctx children] } {
		dict for {childctx types} [dict get $ctxs $ctx children] {
		    if { [dict exists $types $type] } {
			dict set oldchildren $childctx 1
		    }
		}
	    }
	    foreach childctx $children {
		if { ![dict exists $ctxs $childctx] } {
		    dict set ctxs $childctx parent $ctx
		    dict set ctxs $childctx children {}
		} else {
		    dict set ctxs $childctx parent $ctx
		}
		dict set ctxs $ctx children $childctx $type 1
		dict unset oldchildren $childctx
	    }

	    invalidate_group {
		dict for {childctx dummy} $oldchildren {
		    invalidate_datatypes $childctx $invtypes 1
		    dict unset ctxs $ctx children $childctx $type
		    if { [dict size [dict get $ctxs $ctx children $childctx]] == 0 } {
			dict unset ctxs $ctx children $childctx
			dict unset ctxs $childctx parent
		    }
		}
	    }
	}
	namespace export update_relations

	proc update_children {ctx type invtypes fmt err} {
	    global [namespace current]::chan
	    global [namespace current]::ctxs
	    variable datatypes::[set type]::pending

	    if { $err != "" } {
		set data [list $err]
	    } elseif { [catch {::tcf::read $chan $fmt} data] } {
		if { $data == "" } {
		    set data "unknown read error"
		}
		set data [list $data]
	    }
	    update_relations $ctx $type $invtypes [lindex $data 1]

	    dict set ctxs $ctx $type data $data

	    control::assert {[dict exists $pending $ctx]}
	    dict unset pending $ctx
	    if { [dict size $pending] == 0 } {
		datatypes::[set type]::cache notify
	    }
	}
	namespace export update_children

	proc update_stacktrace_children {ctx type invtypes maxframes fmt err} {
	    global [namespace current]::chan
	    global [namespace current]::ctxs
	    variable datatypes::[set type]::pending

	    if { $err != "" } {
		set data [list $err]
	    } elseif { [catch {::tcf::read $chan $fmt} data] } {
		if { $data == "" } {
		    set data "unknown read error"
		}
		set data [list $data]
	    }

	    dict set ctxs $ctx $type:$maxframes data $data

	    control::assert {[dict exists $pending $ctx]}
	    dict unset pending $ctx
	    if { [dict size $pending] == 0 } {
		datatypes::[set type]::cache notify
	    }
	}
	namespace export update_stacktrace_children

	proc update_cache {ctx type fmt err} {
	    global [namespace current]::chan
	    global [namespace current]::ctxs
	    variable datatypes::[set type]::pending

	    if { $err != "" } {
		set data [list $err]
	    } elseif { [catch {::tcf::read $chan $fmt} data] } {
		if { $data == "" } {
		    set data "unknown read error"
		}
		set data [list $data]
	    }

	    dict set ctxs $ctx $type data $data

	    control::assert {[dict exists $pending $ctx]}
	    dict unset pending $ctx
	    if { [dict size $pending] == 0 } {
		datatypes::[set type]::cache notify
	    }
	}
	namespace export update_cache

	proc update_cache_entry {ctx type data} {
	    global [namespace current]::ctxs

	    dict set ctxs $ctx $type data $data
	}
	namespace export update_cache_entry

	proc add_contexts {ctxobjlist datatype childtype } {
	    global [namespace current]::ctxs
	    variable chan

	    set newctxs {}
	    foreach ctxobj $ctxobjlist {
		if { ![dict exists $ctxobj ID] } {
		    puts stderr "missing ID: $ctxobj"
		    continue
		}
		set ctx [dict get $ctxobj ID]

		if { [dict exists $ctxobj ParentID] } {
		    set parent [dict get $ctxobj ParentID]
		} else {
		    set parent ""
		}

		if { [dict exists $ctxs $parent $childtype data] } {
		    set childdata [dict get $ctxs $parent $childtype data]
		    if { [lindex $childdata 0] == "" } {
			set newctxlist [lindex $childdata 1]
			lappend newctxlist $ctx
			dict set ctxs $parent $childtype data [list "" $newctxlist]
		    }
		}

		if { ![dict exists $ctxs $ctx] } {
		    dict set ctxs $ctx [dict create parent $parent children {}]
		    lappend newctxs $ctx
		}
		dict set ctxs $parent children $ctx $childtype 1
		dict set ctxs $ctx $datatype data [list "" $ctxobj]
	    }
	    notify_newctxs $chan $newctxs
	}

	proc change_contexts {ctxobjlist datatype} {
	    global [namespace current]::ctxs

	    foreach ctxobj $ctxobjlist {
		if { ![dict exists $ctxobj ID] } {
		    puts stderr "missing ID: $ctxobj"
		    continue
		}
		set ctx [dict get $ctxobj ID]

		if { ![dict exists $ctxs $ctx $datatype data] } {
		    continue
		}
		dict set ctxs $ctx $datatype data [list "" $ctxobj]
	    }
	}

	proc remove_contexts {ctxlist childtype invtypes} {
	    global [namespace current]::ctxs

	    invalidate_group {
		foreach ctx $ctxlist {
		    if { ![dict exists $ctxs $ctx parent] } {
			continue
		    }
		    set parent [dict get $ctxs $ctx parent]

		    invalidate_datatypes $ctx $invtypes 1

		    if { [dict exists $ctxs $parent $childtype data] } {
			set data [dict get $ctxs $parent $childtype data]
			if { [lindex $data 0] == "" } {
			    set newctxlist {}
			    foreach ctx2 [lindex $data 1] {
				if { $ctx == $ctx2 } {
				    continue
				}
				lappend newctxlist $ctx2
			    }
			    dict set ctxs $parent $childtype data [list "" $newctxlist]
			    update_relations $parent $childtype $invtypes $newctxlist
			}
		    }
		}
	    }
	}

	proc processes_get_children {service invtypes} {
	    namespace eval datatypes::$service:children {
		::tcf::cache_create cache
		variable pending {}
		variable service [uplevel 1 set service]
		variable invtypes [uplevel 1 set invtypes]
		namespace import [namespace parent [namespace parent]]::update_children
		namespace import [namespace parent [namespace parent]]::update_relations
		proc get {ctx} {
		    global [namespace parent [namespace parent]]::chan
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::pending
		    global [namespace current]::service
		    global [namespace current]::invtypes

		    set type $service:children
		    if { ![dict exists $pending $ctx] } {
			::tcf::send_command $chan $service getChildren [list update_children $ctx $type $invtypes "Ea{}"]
			::tcf::write $chan "sb" [list $ctx 0]
			dict set pending $ctx 1
		    }
		    cache wait
		}
		proc invalidate {ctx} {
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::service
		    global [namespace current]::invtypes

		    set type $service:children
		    dict unset ctxs $ctx $type
		    update_relations $ctx $type $invtypes {}
		}
	    }
	}

	proc jtagcable_get_open_servers {service invtypes} {
	    namespace eval datatypes::$service:openServers {
		::tcf::cache_create cache
		variable pending {}
		variable service [uplevel 1 set service]
		variable invtypes [uplevel 1 set invtypes]
		namespace import [namespace parent [namespace parent]]::update_children
		namespace import [namespace parent [namespace parent]]::update_relations
		proc get {ctx} {
		    global [namespace parent [namespace parent]]::chan
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::pending
		    global [namespace current]::service
		    global [namespace current]::invtypes

		    set type $service:openServers
		    if { ![dict exists $pending $ctx] } {
			if { $ctx != "" } {
			    return [list "Invalid context"]
			}
			::tcf::send_command $chan $service getOpenServers [list update_children $ctx $type $invtypes "o{}a{}"]
			::tcf::write $chan "" {}
			dict set pending $ctx 1
		    }
		    cache wait
		}
		proc invalidate {ctx} {
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::service
		    global [namespace current]::invtypes

		    set type $service:openServers
		    dict unset ctxs $ctx $type
		    update_relations $ctx $type $invtypes {}
		}
	    }
	}

	proc stacktrace_get_context {service resfmt} {
	    namespace eval datatypes::$service:context {
		::tcf::cache_create cache
		variable pending {}
		variable service [uplevel 1 set service]
		variable resfmt [uplevel 1 set resfmt]
		namespace import [namespace parent [namespace parent]]::update_cache
		proc get {ctx} {
		    global [namespace parent [namespace parent]]::chan
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::pending
		    global [namespace current]::service
		    global [namespace current]::resfmt

		    set type $service:context
		    if { ![dict exists $pending $ctx] } {
			::tcf::send_command $chan $service getContext [list update_cache $ctx $type $resfmt]
			::tcf::write $chan "a{s}" [list [list $ctx]]
			dict set pending $ctx 1
		    }
		    cache wait
		}
		proc invalidate {ctx} {
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::service

		    set type $service:context
		    dict unset ctxs $ctx $type
		}
	    }
	}

	proc stacktrace_get_children {service invtypes} {
	    namespace eval datatypes::$service:children {
		::tcf::cache_create cache
		variable pending {}
		variable service [uplevel 1 set service]
		variable invtypes [uplevel 1 set invtypes]
		namespace import [namespace parent [namespace parent]]::update_stacktrace_children
		proc get {ctx frames} {
		    global [namespace parent [namespace parent]]::chan
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::pending
		    global [namespace current]::service
		    global [namespace current]::invtypes

		    set type $service:children
		    if { ![dict exists $pending $ctx] } {
			if { $frames == -1} {
			    ::tcf::send_command $chan $service getChildren [list update_stacktrace_children $ctx $type $invtypes -1 "Ea{}"]
			    ::tcf::write $chan "s" [list $ctx]
			} else {
			    ::tcf::send_command $chan $service getChildrenRange [list update_stacktrace_children $ctx $type $invtypes $frames "Ea{}"]
			    ::tcf::write $chan "sii" [list $ctx 0 $frames]
			}

			dict set pending $ctx 1
		    }
		    cache wait
		}
		proc invalidate {ctx} {
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::service
		    global [namespace current]::invtypes

		    dict for {key name} [dict get $ctxs $ctx] {
			if { [string first "StackTrace:children" $key] == 0 } {
			    dict unset ctxs $ctx $key
			}
		    }
		}
	    }
	}

	proc std_get_children {service invtypes} {
	    namespace eval datatypes::$service:children {
		::tcf::cache_create cache
		variable pending {}
		variable service [uplevel 1 set service]
		variable invtypes [uplevel 1 set invtypes]
		namespace import [namespace parent [namespace parent]]::update_children
		namespace import [namespace parent [namespace parent]]::update_relations
		proc get {ctx} {
		    global [namespace parent [namespace parent]]::chan
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::pending
		    global [namespace current]::service
		    global [namespace current]::invtypes

		    set type $service:children
		    if { ![dict exists $pending $ctx] } {
			::tcf::send_command $chan $service getChildren [list update_children $ctx $type $invtypes "Ea{}"]
			::tcf::write $chan "s" [list $ctx]
			dict set pending $ctx 1
		    }
		    cache wait
		}
		proc invalidate {ctx} {
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::service
		    global [namespace current]::invtypes

		    set type $service:children
		    dict unset ctxs $ctx $type
		    update_relations $ctx $type $invtypes {}
		}
	    }
	}

	proc std_get_context {service resfmt} {
	    namespace eval datatypes::$service:context {
		::tcf::cache_create cache
		variable pending {}
		variable service [uplevel 1 set service]
		variable resfmt [uplevel 1 set resfmt]
		namespace import [namespace parent [namespace parent]]::update_cache
		proc get {ctx} {
		    global [namespace parent [namespace parent]]::chan
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::pending
		    global [namespace current]::service
		    global [namespace current]::resfmt

		    set type $service:context
		    if { ![dict exists $pending $ctx] } {
			::tcf::send_command $chan $service getContext [list update_cache $ctx $type $resfmt]
			::tcf::write $chan "s" [list $ctx]
			dict set pending $ctx 1
		    }
		    cache wait
		}
		proc invalidate {ctx} {
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::service

		    set type $service:context
		    dict unset ctxs $ctx $type
		}
	    }
	}

	proc std_get_data {type retrieve} {
	    namespace eval datatypes::$type {
		::tcf::cache_create cache
		variable pending {}
		variable type [uplevel 1 set type]
		variable retrieve [uplevel 1 set retrieve]
		if { [llength $retrieve] == 2 } {
		    lappend retrieve [namespace current]
		}
		namespace import [namespace parent [namespace parent]]::update_cache
		proc get {ctx} {
		    global [namespace parent [namespace parent]]::chan
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::pending
		    global [namespace current]::type
		    global [namespace current]::retrieve

		    if { ![dict exists $pending $ctx] } {
			apply $retrieve $ctx
			dict set pending $ctx 1
		    }
		    cache wait
		}
		proc invalidate {ctx} {
		    global [namespace parent [namespace parent]]::ctxs
		    global [namespace current]::type

		    dict unset ctxs $ctx $type
		}
	    }
	}

	foreach service [::tcf::get_services $chan] {
	    switch $service {
		LineNumbers {
		    std_get_data $service:mapToSource [subst -noc {{ctx} {
			::tcf::send_command {$chan} LineNumbers mapToSource [list update_cache \$ctx LineNumbers:mapToSource "Ea{o{}}"]
			::tcf::write {$chan} sii [list [lindex \$ctx 0] [lindex \$ctx 1] [lindex \$ctx 2]]
		    }}]
		}
		Symbols {
		    std_get_data $service:findByAddr [subst -noc {{ctx} {
			::tcf::send_command {$chan} Symbols findByAddr [list update_cache \$ctx Symbols:findByAddr "Es"]
			::tcf::write {$chan} si [list [lindex \$ctx 0] [lindex \$ctx 1]]
		    }}]
		    std_get_children $service {Symbols:context Symbols:children}
		    std_get_context $service "AA"
		}
		StackTrace {
		    stacktrace_get_children $service {StackTrace:context StackTrace:children}
		    stacktrace_get_context $service "a{o{}}E"
		}
		Expressions {
		    std_get_children $service {Expressions:context Expressions:children}
		    std_get_context $service "eA"
		}
		MemoryMap {
		    ::tcf::on_event $chan MemoryMap changed [subst -noc {
			set reply [::tcf::read {$chan} s]
			invalidate_datatypes [lindex \$reply 0] [list LineNumbers:mapToSource Symbols:findByAddr Symbols:context Symbols:children StackTrace:context StackTrace:children \
								 Expressions:children Expressions:context] 1
			dispose_exprs {$chan} [lindex \$reply 0] "all"
		    }]
		}
		Breakpoints {
		    ::tcf::on_event $chan Breakpoints status [subst -noc {
			set reply [::tcf::read {$chan} so{}]
			event_notify [lindex \$reply 0] "breakpoint" [lindex \$reply 1]
		    }]
		}
		RunControl {
		    std_get_children $service {RunControl:context RunControl:children RunControl:state StackTrace:context StackTrace:children}
		    std_get_context $service "AA"
		    std_get_data $service:state [subst -noc {{ctx} {
			::tcf::send_command {$chan} RunControl getState [list update_cache \$ctx RunControl:state {o{}biso{PCError o{}}}]
			::tcf::write {$chan} s [list \$ctx ]
		    }}]

		    ::tcf::on_event $chan RunControl contextAdded [subst -noc {
			set reply [::tcf::read {$chan} a{o{}}]
			set ctx [lindex \$reply 0]
			add_contexts \$ctx RunControl:context RunControl:children
			event_notify \$ctx "target" "added"
		    }]

		    ::tcf::on_event $chan RunControl contextChanged [subst -noc {
			set reply [::tcf::read {$chan} a{o{}}]
			set ctx [lindex \$reply 0]
			change_contexts \$ctx RunControl:context
			event_notify \$ctx "target" "changed"
		    }]

		    ::tcf::on_event $chan RunControl contextRemoved [subst -noc {
			set reply [::tcf::read {$chan} a{s}]
			set ctx [lindex \$reply 0]
			remove_contexts \$ctx RunControl:children {RunControl:context RunControl:children RunControl:state StackTrace:context StackTrace:children \
										Expressions:children Expressions:context}
			event_notify \$ctx "target" "removed"
			dispose_exprs {$chan} \$ctx "local"
		    }]

		    ::tcf::on_event $chan RunControl contextSuspended [subst -noc {
			set reply [::tcf::read {$chan} siso{}]
			set ctx [lindex \$reply 0]
			set state [list {} 1 [lindex \$reply 1] [lindex \$reply 2] [lindex \$reply 3]]
			update_cache_entry \$ctx RunControl:state \$state
			invalidate_datatypes \$ctx [list StackTrace:context StackTrace:children Expressions:children Expressions:context] 1
			event_notify \$ctx "target" \$state
			dispose_exprs {$chan} \$ctx "local"
		    }]

		    ::tcf::on_event $chan RunControl contextResumed [subst -noc {
			set reply [::tcf::read {$chan} s]
			set ctx [lindex \$reply 0]
			set state [list {} 0]
			invalidate_datatypes \$ctx [list RunControl:state] 0
			invalidate_datatypes \$ctx [list StackTrace:context StackTrace:children Expressions:children Expressions:context] 1
			event_notify \$ctx "target" \$state
			dispose_exprs {$chan} \$ctx "local"
		    }]

		    ::tcf::on_event $chan RunControl contextException [subst -noc {
			set reply [::tcf::read {$chan} ss]
			set ctx [lindex \$reply 0]
			invalidate_datatypes \$ctx [list RunControl:state StackTrace:context StackTrace:children Expressions:children Expressions:context] 1
			dispose_exprs {$chan} \$ctx "local"
		    }]

		    ::tcf::on_event $chan RunControl contextStateChanged [subst -noc {
			set reply [::tcf::read {$chan} s]
			set ctx [lindex \$reply 0]
			invalidate_datatypes \$ctx [list RunControl:state] 0
		    }]

		    ::tcf::on_event $chan RunControl containerSuspended [subst -noc {
			set reply [::tcf::read {$chan} siso{}a{s}]
			set ctx [lindex \$reply 0]
			set state [list {} 1 [lindex \$reply 1] [lindex \$reply 2] [lindex \$reply 3]]
			update_cache_entry \$ctx RunControl:state \$state
			event_notify \$ctx "target" \$state
			foreach ctx2 [lindex \$reply 4] {
			    if { \$ctx != \$ctx2 } {
				event_notify \$ctx2 "target" \$state
				invalidate_datatypes \$ctx2 [list RunControl:state StackTrace:context StackTrace:children Expressions:children Expressions:context] 1
				dispose_exprs {$chan} \$ctx2 "local"
			    }
			}
		    }]

		    ::tcf::on_event $chan RunControl containerResumed [subst -noc {
			set reply [::tcf::read {$chan} a{s}]
			set state [list {} 0]
			foreach ctx [lindex \$reply 0] {
			    invalidate_datatypes \$ctx [list StackTrace:context StackTrace:children Expressions:children Expressions:context] 1
			    dispose_exprs {$chan} \$ctx "local"
			    invalidate_datatypes \$ctx [list RunControl:state] 0
			    event_notify \$ctx "target" \$state
			}
		    }]
		}
		Memory {
		    std_get_children $service {Memory:context Memory:children}
		    std_get_context $service "o{}o{}"

		    ::tcf::on_event $chan Memory contextAdded [subst -noc {
			set reply [::tcf::read {$chan} a{o{}}]
			add_contexts [lindex \$reply 0] Memory:context Memory:children
		    }]

		    ::tcf::on_event $chan Memory contextRemoved [subst -noc {
			set reply [::tcf::read {$chan} a{s}]
			remove_contexts [lindex \$reply 0] Memory:children {Memory:context Memory:children}
		    }]
		}
		Registers {
		    std_get_children $service {Registers:context Registers:children}
		    std_get_context $service "o{}o{}"
		    std_get_data $service:value [subst -noc {{ctx} {
			::tcf::send_command {$chan} Registers get [list update_cache \$ctx Registers:value {o{}B}]
			::tcf::write {$chan} s [list \$ctx ]
		    }}]

		    ::tcf::on_event $chan Registers registerChanged [subst -noc {
			set reply [::tcf::read {$chan} s]
			add_contexts  Memory:context Memory:children
			invalidate_datatypes [lindex \$reply 0] [list Registers:value] 0
		    }]

		    ::tcf::on_event $chan Registers contextChanged [subst -noc {
			::tcf::read {$chan} o{}
			invalidate_datatypes "" [list Registers:context Registers:children] 1
		    }]
		}
		Jtag {
		    std_get_children $service {Jtag:context Jtag:children Jtag:capabilities}
		    std_get_context $service "o{}o{}"
		    std_get_data $service:capabilities [subst -noc {{ctx} {
			::tcf::send_command {$chan} Jtag getCapabilities [list update_cache \$ctx Jtag:capabilities {o{}o{SequenceCommands a{s} Options a{s} Pins a{s}}}]
			::tcf::write {$chan} s [list \$ctx ]
		    }}]

		    ::tcf::on_event $chan Jtag contextAdded [subst -noc {
			set reply [::tcf::read {$chan} a{o{}}]
			add_contexts [lindex \$reply 0] Jtag:context Jtag:children
		    }]

		    ::tcf::on_event $chan Jtag contextChanged [subst -noc {
			set reply [::tcf::read {$chan} a{o{}}]
			change_contexts [lindex \$reply 0] Jtag:context
		    }]

		    ::tcf::on_event $chan Jtag contextRemoved [subst -noc {
			set reply [::tcf::read {$chan} a{s}]
			remove_contexts [lindex \$reply 0] Jtag:children {Jtag:context Jtag:children Jtag:capabilities}
		    }]
		}
		JtagCable {
		    std_get_data $service:serverDescriptions [subst -noc {{ctx} {
			::tcf::send_command {$chan} JtagCable getServerDescriptions [list update_cache \$ctx JtagCable:serverDescriptions {ea{oA}}]
			::tcf::write {$chan} "" {}
		    }}]

		    jtagcable_get_open_servers $service {JtagCable:openServers JtagCable:serverContext}

		    std_get_data $service:serverContext [subst -noc {{ctx} {
			::tcf::send_command {$chan} JtagCable getServerContext [list update_cache \$ctx JtagCable:serverContext {o{}oA}]
			::tcf::write {$chan} s [list \$ctx ]
		    }}]

		    std_get_children $service {JtagCable:context JtagCable:children}
		    std_get_context $service "o{}o{}"

		    ::tcf::on_event $chan JtagCable serverAdded [subst -noc {
			set reply [::tcf::read {$chan} a{o{}}]
			add_contexts [lindex \$reply 0] JtagCable:serverContext JtagCable:openServers
		    }]

		    ::tcf::on_event $chan JtagCable serverRemoved [subst -noc {
			set reply [::tcf::read {$chan} a{s}]
			remove_contexts [lindex \$reply 0] JtagCable:openServers {JtagCable:serverContext JtagCable:openServers}
		    }]

		    ::tcf::on_event $chan JtagCable contextAdded [subst -noc {
			set reply [::tcf::read {$chan} a{o{}}]
			add_contexts [lindex \$reply 0] JtagCable:context JtagCable:children
		    }]

		    ::tcf::on_event $chan JtagCable contextChanged [subst -noc {
			set reply [::tcf::read {$chan} a{o{}}]
			change_contexts [lindex \$reply 0] JtagCable:context
		    }]

		    ::tcf::on_event $chan JtagCable contextRemoved [subst -noc {
			set reply [::tcf::read {$chan} a{s}]
			remove_contexts [lindex \$reply 0] JtagCable:children {JtagCable:context JtagCable:children}
		    }]
		}
		JtagDevice {
		    std_get_data $service:devices [subst -noc {{ctx} {
			::tcf::send_command {$chan} JtagDevice getDevices [list update_cache \$ctx JtagDevice:devices {eai}]
			::tcf::write {$chan} "" {}
		    }}]

		    std_get_data $service:properties [subst -noc {{ctx} {
			if { ![string is int -strict \$ctx] } {
			    return [list "Invalid context"]
			}
			::tcf::send_command {$chan} JtagDevice getProperties [list update_cache \$ctx JtagDevice:properties {eoA}]
			::tcf::write {$chan} "i" [list \$ctx ]
		    }}]

		    ::tcf::on_event $chan JtagDevice devicesChanged [subst -noc {
			set reply [::tcf::read {$chan} {} ]
			invalidate_datatypes "" {JtagDevice:devices JtagDevice:properties} 1
		    }]
		}
		ProcessesV1 {
		    processes_get_children $service {ProcessesV1:context ProcessesV1:children}
		    std_get_context $service "o{}o{}"

		    ::tcf::on_event $chan ProcessesV1 exited [subst -noc {
			set reply [::tcf::read {$chan} s]
			remove_contexts \$reply Processes:children {Processes:context Processes:children}
		    }]
		}
		Processes {
		    processes_get_children $service {Processes:context Processes:children}
		    std_get_context $service "o{}o{}"

		    ::tcf::on_event $chan Processes exited [subst -noc {
			set reply [::tcf::read {$chan} s]
			remove_contexts \$reply Processes:children {Processes:context Processes:children}
		    }]
		}
		Xicom {
		    std_get_data JtagReg:list [subst -noc {{ctx} {
			::tcf::send_command {$chan} Xicom jtagRegList [list update_cache \$ctx JtagReg:list {Ea{s}}]
			::tcf::write {$chan} s [list \$ctx ]
		    }}]
		    std_get_data JtagReg:def [subst -noc {{ctx} {
			::tcf::send_command {$chan} Xicom jtagRegDef [list update_cache \$ctx JtagReg:def {EoA}]
			::tcf::write {$chan} s [list \$ctx ]
		    }}]
		}
		stapl {
		    ::tcf::on_event $chan stapl staplData [subst -noc {
			set reply [::tcf::read {$chan} iB]
			set data [lindex \$reply 1]
			event_notify \$data "stapl" "Data"
		    }]
		    ::tcf::on_event $chan stapl staplNotes [subst -noc {
			set reply [::tcf::read {$chan} iB]
			set data [lindex \$reply 1]
			event_notify \$data "stapl" "Notes"
		    }]
		}
	    }
	}
    }
    ::tcf::on_disconnect $chan [list ::delete_channel $chan]
}

proc ::redirect_channel {chan url} {
    if { ![namespace exists ::channels::$chan] } {
	error "channel '$chan' already deleted"
    }
    foreach ns [namespace children ::channels::[set chan]::datatypes] {
	[set ns]::cache delete
    }
    set url [set ::channels::[set chan]::url]
    namespace delete ::channels::$chan
    add_channel $chan $url
}

proc ::delete_channel {chan} {
    if { ![namespace exists ::channels::$chan] } {
	error "channel '$chan' already deleted"
    }
    foreach ns [namespace children ::channels::[set chan]::datatypes] {
	[set ns]::cache delete
    }
    namespace delete ::channels::$chan
    event_notify $chan "channel" "closed"
}

proc ::get_ctx_data {chan ctx type} {
    global ::channels::[set chan]::ctxs
    if { [dict exists $ctxs $ctx $type data] } {
	return [dict get $ctxs $ctx $type data]
    }
    if { [catch {::channels::[set chan]::datatypes::[set type]::get $ctx} msg opt] } {
	# TODO: Change to work for type returning error in different position.
	if { $msg == $::cache_miss_err } {
	    return -options $opt $msg
	} elseif { [info procs ::channels::[set chan]::datatypes::[set type]::get] == "" } {
	    return [list "$type not supported"]
	} else {
	    return [list $msg]
	}
    }
}

proc ::get_context_cache_client {chan ctx type} {
    set ret [get_ctx_data $chan $ctx $type]
    ::tcf::cache_exit
    return $ret
}

proc ::get_context_list {resultvar chan service parent} {
    upvar $resultvar result

    set ret [get_ctx_data $chan $parent $service:children]
    if {[lindex $ret 0] != ""} {
	return
    }
    foreach child [lindex $ret 1] {
	lappend result $child
	get_context_list result $chan $service $child
    }
}

proc ::get_context_pair {resultvar chan service parent} {
    upvar $resultvar result

    set ret [get_ctx_data $chan $parent $service:children]
    if {[lindex $ret 0] != ""} {
	return
    }
    foreach child [lindex $ret 1] {
	lappend result $parent $child
	get_context_pair result $chan $service $child
    }
}

proc ::get_context_properties {resultvar chan service idlist} {
    upvar $resultvar result

    foreach id $idlist {
	set ret [get_ctx_data $chan $id $service:context]
	if {[lindex $ret 0] == ""} {
	    lappend result [lindex $ret 1]
	}
    }
}

proc ::get_context_data {statevar ctx type} {
    upvar $statevar state
    if { ![catch {get_ctx_data [dict get $state chan] $ctx $type} data] } {
	dict set state result $ctx $type $data
    } elseif { $data == $::cache_miss_err } {
	dict incr state cache_misses
    }
    return $data
}

proc ::get_context_children {statevar ctx type} {
    upvar $statevar state
    if { ![catch {get_ctx_data [dict get $state chan] $ctx $type} data] } {
	dict lappend state children {*}[lindex $data 1]
    } elseif { $data == $::cache_miss_err } {
	dict incr state cache_misses
    }
    return $data
}

proc ::get_context_hierarchy {statevar {ctx ""} {level 0}} {
    upvar $statevar state

    dict set state children {}
    apply [dict get $state get] state $ctx $level

    # Make unique list of children without reordering the list
    set unique {}
    foreach child [dict get $state children] {
	dict set unique $child 1
    }
    set children [dict keys $unique]
    dict set state result $ctx children $children

    incr level
    foreach child $children {
	get_context_hierarchy state $child $level
    }
}

proc ::get_debug_targets_cache_client {chan {ctx ""} {level 0}} {
    set state {}
    dict set state chan $chan
    dict set state result {}
    dict set state cache_misses 0
    dict set state get {{statevar ctx level} {
	upvar $statevar state

	set rc [get_context_data state $ctx RunControl:context]
	get_context_data state $ctx Memory:context
	if { [lindex $rc 0] != $::cache_miss_err } {
	    set rc [lindex $rc 1]
	    if { ![dict exists $rc HasState] || ![dict get $rc HasState] } {
		get_context_children state $ctx RunControl:children
		get_context_children state $ctx Memory:children
	    } else {
		get_context_data state $ctx RunControl:state
	    }
	}
    }}
    get_context_hierarchy state $ctx $level
    set result [dict get $state result]
    if { [dict get $state cache_misses] == 0 } {
	set_debug_target_id $chan result
    }
    return [list $result [dict get $state cache_misses]]
}

proc ::get_context_datatypes {resultvar chan childtypes datatypes ctx depth} {
    upvar $resultvar result
    global cache_miss_err
    set cache_misses 0

    foreach type $datatypes {
	if { [catch {dict set result $ctx $type [get_ctx_data $chan $ctx $type]} message] } {
	    if { $message == $cache_miss_err } {
		incr cache_misses
	    } else {
		puts $message
	    }
	}
    }

    set children {}
    foreach type $childtypes {
	if { [catch {
	    set ret [get_ctx_data $chan $ctx $type]
	    if {[lindex $ret 0] == ""} {
		lappend children {*}[lindex $ret 1]
	    }
	} message] } {
	    if { $message == $cache_miss_err } {
		incr cache_misses
	    } else {
		puts $message
	    }
	}
    }

    # Make unique list of children without reordering the list
    set unique {}
    foreach child $children {
	dict set unique $child 1
    }
    set children [dict keys $unique]
    dict set result $ctx children $children

    incr depth -1
    if { $depth != 0 } {
	foreach child $children {
	    incr cache_misses [get_context_datatypes result $chan $childtypes $datatypes $child $depth]
	}
    }
    return $cache_misses
}

proc ::set_debug_target_id {chan targetsVar} {
    upvar 1 $targetsVar targets
    global ::channels::[set chan]::next_target_id
    global ::channels::[set chan]::target_ctx_map
    global ::channels::[set chan]::ctx_target_map

    foreach ctx [dict keys $targets] {
	if { ![dict exists $ctx_target_map $ctx] } {
	    dict set targets $ctx target_id $next_target_id
	    dict set ctx_target_map $ctx $next_target_id
	    dict set target_ctx_map $next_target_id $ctx
	    incr next_target_id
	} else {
	    dict set targets $ctx target_id [dict get $ctx_target_map $ctx]
	}
    }
}

proc ::get_debug_targets {chan} {
    lassign [get_debug_targets_cache_client $chan] targets cache_misses
    ::tcf::cache_exit
    return $targets
}

proc ::set_jtag_node_id {chan nodesVar} {
    upvar 1 $nodesVar nodes
    global ::channels::[set chan]::next_node_id
    global ::channels::[set chan]::node_ctx_map
    global ::channels::[set chan]::ctx_node_map

    foreach ctx [dict keys $nodes] {
	if { ![dict exists $ctx_node_map $ctx] } {
	    dict set nodes $ctx node_id $next_node_id
	    dict set ctx_node_map $ctx $next_node_id
	    dict set node_ctx_map $next_node_id $ctx
	    incr next_node_id
	} else {
	    dict set nodes $ctx node_id [dict get $ctx_node_map $ctx]
	}
    }
}

proc ::get_jtag_nodes {chan} {
    get_context_datatypes nodes $chan {Jtag:children} {Jtag:context} "" -1
    get_context_datatypes nodes $chan {Jtag:children} {Jtag:capabilities JtagCable:context} "" 2

    ::tcf::cache_exit

    set_jtag_node_id $chan nodes
    return $nodes
}

proc ::get_jtag_nodes_cache_client {chan} {
    set cache_misses 0
    incr cache_misses [get_context_datatypes nodes $chan {Jtag:children} {Jtag:context} "" -1]
    incr cache_misses [get_context_datatypes nodes $chan {Jtag:children} {Jtag:capabilities JtagCable:context} "" 2]

    if {$cache_misses == 0 } {
	set_jtag_node_id $chan nodes
    }
    return [list $nodes $cache_misses]
}

proc ::get_processes {chan} {
    set services [::tcf::get_services $chan]
    set processes {}
    if { [lsearch $services ProcessesV1] >= 0 } {
	get_context_datatypes processes $chan {ProcessesV1:children} {ProcessesV1:context} "" 2
    } elseif { [lsearch $services Processes] >= 0 } {
	get_context_datatypes processes $chan {Processes:children} {Processes:context} "" 2
    }

    ::tcf::cache_exit

    apply [list {processesVar} {
	upvar 1 $processesVar processes
	variable next_process_id
	variable process_ctx_map
	variable ctx_process_map

	set processes [uplevel 1 {set processes}]
	foreach ctx [dict keys $processes] {
	    if { ![dict exists $ctx_process_map $ctx] } {
		dict set processes $ctx process_id $next_process_id
		dict set ctx_process_map $ctx $next_process_id
		dict set process_ctx_map $next_process_id $ctx
		incr next_process_id
	    } else {
		dict set processes $ctx process_id [dict get $ctx_process_map $ctx]
	    }
	}
	dict set processes process_ctx_map $process_ctx_map
    } ::channels::$chan] processes
    return $processes
}

proc ::get_regs {chan parent flags} {
    global cache_miss_err
    set cache_misses 0
    if { ![catch {
	foreach name [dict get $flags regpath] {
	    incr cache_misses [get_context_datatypes regs $chan {Registers:children} {Registers:context} $parent 2]
	    foreach ctx [dict get $regs $parent children] {
		set reg [dict get $regs $ctx Registers:context]
		if { [dict get [lindex $reg 1] Name] == $name } {
		    set parent $ctx
		    break
		}
	    }
	}
    }]} {
	incr cache_misses [get_context_datatypes regs $chan {Registers:children} {Registers:context} $parent 2]
    }

    if { $cache_misses == 0 && ![dict get $flags defs] } {
	foreach ctx [dict keys $regs] {
	    set reg [lindex [dict get $regs $ctx Registers:context] 1]
	    if { ![dict exists $reg Readable] || ![dict get $reg Readable] } {
		continue
	    }
	    if { [catch {dict set regs $ctx Registers:value [get_ctx_data $chan $ctx Registers:value]} message] } {
		if { $message != $cache_miss_err } {
		    puts $message
		}
	    }
	}
    }

    ::tcf::cache_exit

    return $regs
}

proc ::get_stacktrace_children {chan ctx frames } {
    global ::channels::[set chan]::ctxs
    if { $frames > 0} {
	incr frames -1
    } else {
	set frames -1
    }
    set type StackTrace:children

    dict for {key name} [dict get $ctxs $ctx] {
	if { [string first "StackTrace:children" $key] == 0 } {
	    set cachedframes [lindex [split $key ":"] 2 ]
	    if { $cachedframes != -1 && $frames != -1} {
		if { $frames <= $cachedframes } {
		    return [list [lindex [dict get $ctxs $ctx $type:$cachedframes data] 0] \
			    [lrange [lindex [dict get $ctxs $ctx $type:$cachedframes data] 1] 0 $frames]]
		}
	    } elseif { $cachedframes == -1 && $frames == -1} {
		return [dict get $ctxs $ctx $type:$cachedframes data]
	    }
	}
    }

    if { [catch {::channels::[set chan]::datatypes::[set type]::get $ctx $frames} msg opt] } {
	# TODO: Change to work for type returning error in different position.
	if { $msg == $::cache_miss_err } {
	    return -options $opt $msg
	} elseif { [info procs ::channels::[set chan]::datatypes::[set type]::get] == "" } {
	    return [list "$type not supported"]
	} else {
	    return [list $msg]
	}
    }
}

package provide xsdb::tcfinterp $::xsdb::tcfinterp::version
