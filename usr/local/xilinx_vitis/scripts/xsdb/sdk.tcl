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
package require scw

namespace eval ::sdk {
    variable version 0.1
    variable sdk_workspace ""
    variable sdk_chan ""
    variable server_props ""
    variable sdk_conn_status ""
    variable help_prefix ""
    variable plx_install ""
    if { [string first "xsdb" [file tail [info nameofexecutable]]] != -1 } {
	set help_prefix "sdk "
    }

    proc sdk_conn_handler { conn } {
	variable sdk_chan
	variable sdk_conn_status

	set sdk_conn_status [dict get $conn state]
	set sdk_chan [dict get $conn chan]
    }

    proc sdk_disconn_handler {} {
	variable sdk_chan
	variable server_props
	variable sdk_conn_status

	set sdk_conn_status ""
	set sdk_chan ""
	set server_props ""
    }

    proc sdk_command_handler { data } {
	variable sdk_cmd_result
	set sdk_cmd_result $data
    }

    proc xsdk_eval { chan service cmd argfmt resfmt arglist } {
	variable sdk_cmd_result
	if { [info exists sdk_cmd_result] } {
	    error "vitis command already in progress, cannot run the command $cmd $arglist."
	}
	catch {
	    set sdk_cmd_result pending
	    set arg [dict create chan $chan service $service cmd $cmd argfmt $argfmt resfmt $resfmt arglist $arglist]
	    ::tcf::sync_eval [list sdk_command_eval_client $arg]
	    vwait ::sdk::sdk_cmd_result
	    set sdk_cmd_result
	} msg opt
	unset sdk_cmd_result
	if { [::xsdb::dict_get_safe $msg err] != "" } {
	    error [dict get $msg err]
	}
	return -options $opt [::xsdb::dict_get_safe $msg data]
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

    proc getsdkchan {} {
	variable sdk_chan
	variable sdk_workspace
	variable server_props
	variable sdk_conn_status

	check_sdk_workspace
	if { $sdk_chan == "" } {
	    if { [catch {
		set server_props [::tcf::sync_eval {
		    variable server_connection

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
			    ::tcf::disconnect $chan [list eval_event {puts "Channel closed"}]
			    return
			}

			if { ![info exists xsdb_command_table] } {
			    eval_event ::sdk::xsdb_command_handler
			}
			lappend xsdb_command_table [list $chan $token [lindex $data 0]]
		    }

		    proc xsdb_after {chan token} {
			if { [catch {::tcf::read $chan i} data] } {
			    puts "xsdb_eval error $token $data"
			    ::tcf::disconnect $chan [list eval_event {puts "Channel closed"}]
			    return
			}

			::tcf::post_event [list ::tcf::send_reply $chan $token "e" [list {}]] $data
		    }

		    proc xsdb_abort {chan token} {
			if { [catch {::tcf::read $chan {}} data] } {
			    puts "xsdb_eval error $token $data"
			    ::tcf::disconnect $chan [list eval_event {puts "Channel closed"}]
			    return
			}

			::tcf::generate_interrupt
			::tcf::send_reply $chan $token "e" [list {}]
		    }

		    proc server_connection_callback {type data} {
			variable server_connection
			switch $type {
			    start {
				if { [info exists server_connection] } {
				    ::tcf::on_command $data xsdb eval [list xsdb_eval $data]
				    ::tcf::on_command $data xsdb after [list xsdb_after $data]
				    ::tcf::on_command $data xsdb abort [list xsdb_abort $data]
				    ::tcf::on_disconnect $data {
				    }
				} else {
				    dict set server_connection chan $data
				    ::tcf::on_command $data xsdb eval [list xsdb_eval $data]
				    ::tcf::on_command $data xsdb after [list xsdb_after $data]
				    ::tcf::on_command $data xsdb abort [list xsdb_abort $data]
				    ::tcf::on_disconnect $data {
					variable server_connection
					unset server_connection
					eval_event {::sdk::sdk_disconn_handler}
				    }
				}
			    }
			    error -
			    connect {
				dict set server_connection state $type
				eval_event [list ::sdk::sdk_conn_handler $server_connection]
			    }
			}
		    }
		    set serverid [::tcf::server_start tcp:localhost:0 server_connection_callback]
		    set props [::tcf::get_server_properties $serverid]
		    return $props
		}]
	    } msg] } {
		error $msg
	    }
	    set xsdk_exec "vitis"

	    set eclipseargs [::xsdb::get_eclipseargs]
	    set vmargs [::xsdb::get_vmargs]

	    catch {string first "Windows" $::tcl_platform(os)} res
	    if { $res == 0 } {
		set xsdk_exec "vitis.bat"
	    }
	    puts -nonewline "Starting $xsdk_exec. This could take few seconds..."
	    flush stdout
	    if {[string first "Linux" $::tcl_platform(os)] == -1} {
		exec $xsdk_exec -eclipseargs $eclipseargs --launcher.suppressErrors -nosplash -application com.xilinx.sdx.cmdline.service \
		 [dict get $server_props Port] -data $sdk_workspace -vmargs $vmargs -Dorg.eclipse.cdt.core.console=org.eclipse.cdt.core.systemConsole &
	    } else {
	    exec $xsdk_exec -eclipseargs $eclipseargs --launcher.suppressErrors --launcher.GTK_version 2 -nosplash -application com.xilinx.sdx.cmdline.service \
		 [dict get $server_props Port] -data $sdk_workspace -vmargs $vmargs -Dorg.eclipse.cdt.core.console=org.eclipse.cdt.core.systemConsole &
	    }

	    set command_id [after $::xsdb::sdk_launch_timeout {set ::sdk::sdk_conn_status "timeout"}]
	    vwait ::sdk::sdk_conn_status
	    switch $sdk_conn_status {
		connect {
		    puts "done"
		    after cancel $command_id
		}
		error {
		    sdk_disconn_handler
		    error "TCF connection error"
		}
		timeout {
		    sdk_disconn_handler
		    error "Timeout while establishing a connection with Vitis"
		}
	    }
	}
	return $sdk_chan
    }

    proc scw_clear_open_sw_db {} {
	::scw::clear_open_swdb
    }

    proc append_app_compiler_flags {flags} {
	set curval [hsi get_property APP_COMPILER_FLAGS [hsi current_sw_design]]
	hsi set_property -name APP_COMPILER_FLAGS -value $curval$flags -objects [hsi current_sw_design]
    }

    proc append_app_linker_flags {flags} {
	set curval [hsi get_property APP_LINKER_FLAGS [hsi current_sw_design]]
	hsi set_property -name APP_LINKER_FLAGS -value $curval$flags -objects [hsi current_sw_design]
    }

    proc check_sdk_workspace {} {
	variable sdk_workspace
	if { [string compare $sdk_workspace ""] == 0 } {
	    error "Invalid workspace. set workspace using setws command"
	}
    }

    proc setws { args } {
	variable sdk_workspace
	variable sdk_chan
	variable help_prefix

	set options {
	    {switch "switch to new workspace"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}
	if { ![info exists params(path)] } {
	    if { [llength $args] > 0 } {
		set params(path) [lindex $args 0]
		set args [lrange $args 1 end]
	    } else {
		set params(path) [pwd]
	    }
	}
	if { [llength $args] } {
	    error "Unexpected arguments: $args"
	}

	if {[file isfile $params(path)]} {
	    error "$params(path) is a file"
	} elseif {![file isdirectory $params(path)]} {
	    if { [catch { file mkdir $params(path) } msg ] } {
		error $msg
	    }
	}

	set params(path) [file normalize $params(path)]
	if { $sdk_workspace != "" && $sdk_chan != "" && $sdk_workspace != $params(path) } {
	    if { !$params(switch) } {
		error "Workspace is already set. use -switch option to switch to new workspace"
	    }
	    ::xsdb::disconnect $sdk_chan
	    vwait ::sdk::sdk_conn_status
	}
	set sdk_workspace $params(path)
	::scw::clearplatforms
	builtin_scwutil -setws $sdk_workspace
	return
    }
    namespace export setws
    ::xsdb::setcmdmeta setws categories {projects} [subst $help_prefix]
    ::xsdb::setcmdmeta setws brief {Set Vitis workspace} [subst $help_prefix]
    ::xsdb::setcmdmeta setws description [subst {
SYNOPSIS {
    [concat $help_prefix setws] \[OPTIONS\] \[path\]
        Set Vitis workspace to <path>, for creating projects.
        If <path> does not exist, then the directory is created.
        If <path> is not specified, then current directory is used.
}
OPTIONS {
    -switch <path>
        Close existing workspace and switch to new workspace.
}
RETURNS {
    Nothing if the workspace is set successfully.
    Error string, if the path specified is a file.
}
EXAMPLE {
    setws /tmp/wrk/wksp1
       Set the current workspace to /tmp/wrk/wksp1.

    setws -switch /tmp/wrk/wksp2
       Close the current workspace and switch to new workspace /tmp/wrk/wksp2.
}
}] [subst $help_prefix]

    proc getws { args } {
	variable sdk_workspace
	variable help_prefix
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	return $sdk_workspace
    }
    namespace export getws
    ::xsdb::setcmdmeta getws categories {projects} [subst $help_prefix]
    ::xsdb::setcmdmeta getws brief {Get Vitis workspace.} [subst $help_prefix]
    ::xsdb::setcmdmeta getws description [subst {
SYNOPSIS {
    [concat $help_prefix getws]
        Return the current vitis workspace.
}
RETURNS {
    Current workspace.
}
}] [subst $help_prefix]

    proc set_user_repo_path_sdk {args} {
	variable help_prefix
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "Wrong # args: should be \"sdk set_user_repo_path path\""
	}

	set repo_path [lindex $args 0]
	set file_list [glob -nocomplain -directory $repo_path *]
	if { [llength $file_list] == 0 } {
	    puts "WARNING: repository path given is empty"
	}

	set chan [getsdkchan]
	xsdk_eval $chan "XSDx" "setRepo" "o{Path s}" e [list [dict create Path [file normalize $repo_path]]]
	return
    }

    proc get_user_repo_path_sdk { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set chan [getsdkchan]
	return [xsdk_eval $chan "XSDx" getRepo "" s [list]]
    }

    proc createhw {args} {
	puts "\nNote:: \"createhw\" command is deprecated. Use \"platform\" command"
	variable sdk_workspace
	variable help_prefix

	set options {
	    {name "project name" {args 1}}
	    {hwspec "hw specification file" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(name)] } {
	    error "Project name not specified"
	}

	if { ![info exists params(hwspec)] } {
	    error "HW specification file not specified"
	}

	set params(hwspec) [file normalize $params(hwspec)]
	check_sdk_workspace
	return [::scw::platform create -name $params(name) -hw $params(hwspec)]
    }
    namespace export createhw

    proc updatehw {args} {
	variable sdk_workspace
	variable help_prefix

	set options {
	    {hw "hardware project" {args 1}}
	    {newhwspec "new hw specification file" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(hw)] } {
	    error "HW project not specified"
	}

	if { ![info exists params(newhwspec)] } {
	    error "New hw specification file not specified"
	}
	set chan [getsdkchan]
	::scw::platform active $params(hw)
	return [::scw::platform config -updatehw $params(newhwspec)]
    }
    namespace export updatehw

    proc changebsp {args} {
	variable sdk_workspace
	variable help_prefix

	set options {
	    {app "application project" {args 1}}
	    {newbsp "new BSP to be referenced" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(app)] } {
	    error "Application project not specified"
	}

	if { ![info exists params(newbsp)] } {
	    set fmt [dict create Name s]
	    set data [dict create Name $params(app)]
	    set chan [getsdkchan]
	    set retvals [xsdk_eval $chan "Xsdk" changeBsp "o{$fmt}" es [list $data]]
	    set retval [lindex $retvals 1]
	    set retlist [split $retval ","]
	    if { [llength $retlist] == 0 } {
		puts "No other valid bsp exists"
		return
	    }
	    puts "valid bsp references for application \"$params(app)\":"
	    foreach entry $retlist {
		puts "   $entry"
	    }

	} else {
	    set fmt [dict create Name s NewBsp s]
	    set data [dict create Name $params(app) NewBsp $params(newbsp)]
	    set chan [getsdkchan]
	    xsdk_eval $chan "Xsdk" changeBsp "o{$fmt}" e [list $data]
	}
	return
    }
    namespace export changebsp

    proc createbsp {args} {
	puts "\nNote:: \"createbsp\" command is deprecated. Use \"platform\" command"
	variable sdk_workspace
	variable help_prefix

	set options {
	    {name "project name" {args 1}}
	    {proc "target processor" {args 1}}
	    {hwproject "hwproject name" {args 1}}
	    {mss "MSS File Path" {args 1}}
	    {os "project OS" {default "standalone" args 1}}
	    {arch "32/64 bit" {default "" args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(name)] } {
	    error "Project name not specified"
	}

	if { ![info exists params(hwproject)] } {
	    error "Specify a hw project to create a BSP project"
	}

	check_sdk_workspace
	if { [info exists params(mss)] } {
	    set params(mss) [file normalize $params(mss)]
	    if { [file isfile $params(mss)] == 0} {
		error "Invalid mss file path"
	    }
	    if { [info exists params(proc)] || [info exists params(arch)] || [info exists params(os)] } {
		puts "WARNING: ignoring proc/arch/os details, when mss file is passed"
	    }
	} else {
	    if { ![info exists params(proc)] } {
		error "Specify a processor instance or a mss file to create a BSP project"
	    } else {
		if { ($params(arch) == 32) || ($params(arch) == 64) || ($params(arch) == "") } {
		    if { [string match "psu_cortexa53*" $params(proc)] } {
			if { $params(arch) == "" } {
			    set params(arch) 64
			}
		    } else {
			if { $params(arch) == 64 } {
			    puts "WARNING: $params(arch)-bit not supported for the processor type - $params(proc). Setting 32-bit"
			}
			set params(arch) 32
		    }
		} else {
		    error "Illegal arch type $params(arch). must be 32/64"
		}
	    }
	}
	if { [info exists params(hwproject)] } {
	    set retval [builtin_platform -list]
	    if { $retval == "" } {
		error "HW project $params(hwproject) does not exist"
	    }
	    set platform_set 0
	    set retdict [::json::json2dict $retval]
	    dict for {key value} $retdict {
		if { $params(hwproject) == $key} {
		    builtin_platform -active $key
		    set platform_set 1
		}
	    }
	    if { $platform_set == 0 } {
		error "HW project $params(hwproject) does not exist"
	    }
	}
	if { [info exists params(mss)] } {
	    ::scw::domain create -name "$params(name)" -os $params(os) -mss $params(mss) -proc $params(proc)
	} else {
	    ::scw::domain create -name "$params(name)" -os $params(os) -proc $params(proc)
	}
	return [::scw::platform generate -quick]
    }
    namespace export createbsp

#----------------------------------------------------------------------------------#
#  Name: create app
#  ----------------------------------------------------------------------------
    proc app {args} {
	if { [llength $args] == 0 } {
	    error "Wrong # of args: should be \"app <sub-command> \[options]\""
	}

	if { [string index [lindex $args 0] 0] == "-" } {
	    if { [lsearch $args "-help"] != -1 } {
		return [help app]
	    } elseif { [lsearch $args "-report"] != -1 } {
		set args [lrange $args 1 end]
		return [::scw::linker report $args]
	    } elseif { [lsearch $args "-list-mem"] != -1 } {
		return [::scw::linker list-mem]
	    } else {
		return [::scw::linker config {*}$args]
	    }
	} else {
	    set subcmd [lindex $args 0]
	    set args [lrange $args 1 end]
	}

	switch -- $subcmd {
	    build {
		set options {
		    {name "application name" {args 1}}
		    {all "all application projects"}
		    {help "command help"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { $params(all) } {
		    set params(name) "all"
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] != 1 } {
			error "Invalid arguments, specify name of the application"
		    }
		}

		set chan [getsdkchan]
		xsdk_eval $chan XSDx build "o{[dict create Type s Name s]}" e [list [dict create Type "app" Name $params(name)]]
		return
	    }
	    clean {
		set options {
		    {name "application name" {args 1}}
		    {help "command help"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] != 1 } {
			error "Invalid arguments, specify name of the application"
		    }
		}

		set chan [getsdkchan]
		xsdk_eval $chan XSDx clean "o{[dict create Type s Name s]}" e [list [dict create Type "app" Name $params(name)]]
		return
	    }
	    config {
		set options {
		    {name "name of the application" {args 1}}
		    {set "set a param value" {default 0}}
		    {get "get the param value"}
		    {add "add to a param value"}
		    {remove "delete a param value"}
		    {info "more info of param value"}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { $params(set) + $params(add) + $params(remove) + $params(info) + $params(get) > 1 } {
		   error "Conflicting options specified"
		}

		set chan [getsdkchan]
		if { [isAIEApp $params(name) ] == "1" } {
		    set defs [lindex [xsdk_eval $chan SDxBuildSettings getAIEDefinitions s "eA" [list ""]] 1]
		} else {
		    set defs [lindex [xsdk_eval $chan SDxBuildSettings getDefinitions s "eA" [list ""]] 1]
		}
		set pnames [lsort [dict keys $defs]]
		if { [llength $args] == 0 } {
		    set result ""
		    foreach pname $pnames {
			if { $result != "" } {
			    append result "\n"
			}
			append result [format "  %-30s %s" $pname [::xsdb::dict_get_safe $defs $pname description]]
		    }
		    return $result
		}

		if { [llength $args] > 2 } {
		    error "Unexpected arguments: $args. should be \"app config \[name\] \[options\] \[value\]\""
		}
		set pname [lsearch -all -inline -glob $pnames "[lindex $args 0]*"]
		if { [llength $pname] != 1 } {
		    if { [llength $pname] == 0 } {
			set pname $pnames
		    }
		    error "Unknown or ambiguous parameter \"[lindex $args 0]\": must be [join $pname {, }]"
		}
		set pname [lindex $pname 0]

		if { [info exists params(name)] } {
		    if { [lsearch [::sdk::getprojects] $params(name)] == -1 } {
			error "Application project '$params(name)' doesn't exist in the workspace\nuse 'app list' to get a list of application projects in workspace"
		    }
		} else {
		    error "Application name not specified"
		}

		if { [llength $args] == 2 } {
		    if { $params(info) || $params(get) } {
			error "'-info' and '-get' options are not supported while setting a parameter value"
		    }

		    set value [lindex $args 1]
		    set props [::xsdb::dict_get_safe $defs $pname props]
		    if { $params(set) + $params(add) + $params(remove) == 0 } {
			set params([lindex $props 0]) 1
		    }

		    foreach prop [array names params] {
			if { $params($prop) == 1 } {
			    if { [lsearch $props $prop] == -1 } {
				error "Parameter $pname doesn't support $prop operation"
			    }
			    xsdk_eval $chan SDxBuildSettings $prop sss e [list $params(name) $pname $value]
			    return
			}
		    }
		}

		if { $params(info) } {
		    set result [format "  %-20s : %s\n" "Possible Values" [::xsdb::dict_get_safe $defs $pname values]]
		    append result [format "  %-20s : %s\n" "Possible Operations" [regsub -all { } [::xsdb::dict_get_safe $defs $pname props] {, }]]
		    append result [format "  %-20s : %s" "Default Operation" [lindex [::xsdb::dict_get_safe $defs $pname props] 0]]
		    return $result
		}

		return [lindex [xsdk_eval $chan SDxBuildSettings get ss eA [list $params(name) $pname]] 1]
	    }
	    create {
		set options {
		    {name "project name" {args 1}}
		    {template "application template" {default {Hello World} args 1}}
		    {proc "target processor" {args 1}}
		    {platform "platform project name" {args 1}}
		    {domain "domain name" {args 1}}
		    {arch "architecture type" {args 1}}
		    {sysproj "system project name" {args 1}}
		    {os "project OS" {default "standalone" args 1}}
		    {lang "project launguage" {default "c" args 1}}
		    {hw "hw specification file path" {args 1}}
		    {plnx "app creation for petalinux"}
		    {out "output directory location" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}
		if { [info exists params(hw)] } {
		    if { ![info exists params(name)] || ![info exists params(proc)] } {
			error "Invalid arguments, name or processor not specified"
		    }
		    if { $params(proc) == "ai_engine" && $params(os) != "aie_runtime"  } {
			puts "WARNING: $params(os) not supported for the processor type - $params(proc). Setting to aie_runtime"
			set params(os) "aie_runtime"
		    }
		    set params(proc) [::scw::get_processor_name $params(proc) $params(hw)]
		} elseif { [info exists params(name)] } {
		    if { [info exists params(sysproj)] } {
			if { ![info exists params(domain)] } {
			    set activedom [builtin_domain -active]
			    if { $activedom == ""} {
				error "No active domain exists, specify a domain"
			    } else {
				set params(domain) $activedom
			    }
			}
			set chan [getsdkchan]
			set ret_list [xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type system]]]
			if { [lindex $ret_list 1] != "" } {
			    set projlist [split [lindex $ret_list 1] ";"]
			    if { [lsearch $projlist $params(sysproj)] == -1 } {
				puts "System project doesnt exist, creating new system project."
			    }
			} else {
			    puts "No system project exists, creating new system project."
			}
		    } else {
			if { ![info exists params(platform)] } {
			    set activeplat [builtin_platform -active]
			    if { $activeplat == ""} {
				error "No active platform exists, specify a platform"
			    } else {
				set params(platform) $activeplat
			    }
			}
			if { ![info exists params(domain)] } {
			    set activedom [builtin_domain -active]
			    if { $activedom == ""} {
				error "No active domain exists, specify a domain"
			    } else {
				set params(domain) $activedom
			    }
			}
			if { $params(plnx) != "1" } {
			    check_sdk_workspace
			    set params(os) [getosfromrepo $params(platform) "" $params(domain)]
			}
			if { ![info exists params(out)] } {
			    if { $params(plnx) != "1" } {
				    check_sdk_workspace
				    set params(os) [getosfromrepo $params(platform) "" $params(domain)]
			    }
			}
		    }
		} else {
		    error "Invalid arguments, should be \"app create \[options\]\""
		}

		if { [string compare -nocase $params(os) "linux"] == 0 } {
		    if { $params(template) == "Hello World" } {
			set params(template) "Linux Hello World"
		    }
		}

		if { [string match -nocase "freertos*" $params(os)] } {
		    if { $params(template) == "Hello World" } {
			set params(template) "FreeRTOS Hello World"
		    }
		}
		if { $params(plnx) != "1" } {
		    if { ![info exists params(proc)] && [isAIEDomain $params(platform) $params(domain)] == "1" } {
			if { $params(template) == "Hello World" } {
			    set params(template) "Empty Application(C)"
			}
		    } elseif { [info exists params(proc)] && $params(proc) == "ai_engine" } {
			# $params(template)  != "Empty Application"
			# puts "WARNING: $params(template) not supported for the processor type - $params(proc). Setting to Empty Application"
			set params(template) "Empty Application(C)"
		    } elseif { [string compare -nocase $params(lang) "C"] == 0 } {
			set appfound 0
			set applist [::hsi::utils::get_all_app_details -dict]
			if { $params(template) == "Empty Application" } {
			    # to provide backward compatibility for previous version templates
			    set params(template) "Empty Application(C)"
			}
			dict for {app details} $applist {
			    if { $params(template) == [dict get $details userdefname] } {
				set appfound 1
				break;
			    }
			}
			if { $appfound == 0 } {
			    error "$params(template) is not valid application template name\n\
				    use repo -apps to get the available templates"
			}
		    } elseif { [string compare -nocase $params(lang) "c++"] == 0 } {
			if { $params(template) == "Hello World" || $params(template) == "Linux Hello World" || $params(template) == "Empty Application" } {
			    set params(template) "Empty Application (C++)"
			}
		    }
		} else {
		    if { [string compare -nocase $params(lang) "C"] == 0 } {
			set appfound 0
			set applist [::hsi::utils::get_all_app_details -dict]
			if { $params(template) == "Empty Application" } {
			    # to provide backward compatibility for previous version templates
			    set params(template) "Empty Application(C)"
			}
			dict for {app details} $applist {
			    if { $params(template) == [dict get $details userdefname] } {
				set appfound 1
				break;
			    }
			}
			if { $appfound == 0 } {
			    error "$params(template) is not valid application template name\n\
				    use repo -apps to get the available templates"
			}
		    } elseif { [string compare -nocase $params(lang) "c++"] == 0 } {
			if { $params(template) == "Hello World" || $params(template) == "Linux Hello World" || $params(template) == "Empty Application" } {
			    set params(template) "Empty Application (C++)"
			}
		    }
		}

		if { [info exists params(name)] } {
		    if { [info exists params(hw)] } {
			set archtype ""
			if { [info exists params(arch)]  &&  $params(arch) != "" } {
			    if { $params(arch) == "32-bit" || $params(arch) == "32" } {
				set archtype 32
			    } elseif { $params(arch) == "64-bit" || $params(arch) == "64" } {
				if { [string match "ps7_cortexa9*" $params(proc)] || [string match "psu_cortexr5*" $params(proc)] || \
				    [string match "psv_cortexr5*" $params(proc)] } {
				    puts "WARNING: -arch $params(arch) is not supported for the processor type - $params(proc). Setting 32-bit"
				    set archtype 32
				} else {
				    set archtype 64
				}
			    } else {
				error "Illegal arch type $params(arch). must be 32/64"
			    }
			} elseif { [string match "psu_cortexa53*" $params(proc)] || [string match "*psv_cortexa72*" $params(proc)] || \
			    [string match "*psu_cortexa72*" $params(proc)] || [string match "*cortexa78*" $params(proc)] } {
			    set archtype 64
			} else {
			    set archtype 32
			}

			if { [string compare -nocase $params(os) "linux"] != 0  && $params(proc) != "ai_engine" } {
			    set ipname [hsi get_property IP_NAME [ hsi get_cells -filter "IP_TYPE == PROCESSOR" -hierarchical $params(proc) ]]
			    if { $ipname == "microblaze"} {
			        set datasize [hsi get_property CONFIG.C_DATA_SIZE  [ hsi get_cells -filter "IP_TYPE == PROCESSOR" -hierarchical  $params(proc) ]]
			        if { $datasize == "64" } {
				    set archtype 64
			        }
			    }
			}

			set fmt [dict create Name s Hw s Proc s Os s Language s TemplateApp s ArchType s OutPutFormat s]
			set data [dict create Name $params(name) Hw $params(hw) Proc $params(proc) Os $params(os) \
				  Language [string toupper $params(lang)] TemplateApp $params(template) ArchType $archtype OutPutFormat elf]
			set chan [getsdkchan]
			xsdk_eval $chan "XSDx" createAppFromDsa "o{$fmt}" e [list $data]
		    } elseif { [info exists params(platform)] && [info exists params(domain)]} {
			if { [info exists params(arch)] } {
			    error "Invalid argument, '-arch' is not supported for\n\
				   application creation using platform/domain"
			}

			if { $params(plnx) || [info exists params(out)] } {
			    set outdir [pwd]
			    if { [info exists params(out)] } {
				    set outdir $params(out)
			    }
			    set currdir [pwd]
			    ::scw::get_hw_path
			    ::scw::get_mss_path
			    set hsitemplate [::scw::get_app_template $params(template)]
			    cd $outdir
			    set params(os) [hsi get_os]
			    set params(proc) [hsi get_sw_processor]

			    if { $params(os) == "standalone" &&  [string match "*_cortexr5*" $params(proc)] } {
				    append_app_compiler_flags "-mcpu=cortex-r5  -mfloat-abi=hard  -mfpu=vfpv3-d16 -DARMR5"

			    } elseif { $params(os) != "standalone" &&  [string match "*_cortexr5*" $params(proc)] } {
				    append_app_compiler_flags "-mcpu=cortex-r5 -DARMR5"
			    }
			    if { [file isdir $params(name)] && [file exists $params(name)] } {
				    puts "Note: updating the existing app sources with modified platform."
				    file mkdir "$params(name).new"
				    cd "$params(name).new"
				    ::hsi::generate_app -app $hsitemplate
				    cd ../
				    append hsitemplate _bsp
				    file delete -force $params(name)/$hsitemplate
				    file delete $params(name)/Makefile
				    #file delete $params(name)/lscript.ld
				    file copy $params(name).new/Makefile $params(name)/Makefile
				    #file copy $params(name).new/lscript.ld $params(name)/lscript.ld
				    file copy $params(name).new/$hsitemplate $params(name)/
				    file delete -force $params(name).new
			    } else {
				    file mkdir $params(name)
				    cd $params(name)
				    ::hsi::generate_app -app $hsitemplate
			    }
			    cd $currdir
			    return "Created app at $outdir"
			} else {
			    if { [info exists params(sysproj)] } {
				set fmt [dict create Name s Platform s System s Domain s Language s TemplateApp s OutPutFormat s SysProject s]
				set data [dict create Name $params(name) Platform $params(platform) System "" Domain $params(domain) \
				Language [string toupper $params(lang)] TemplateApp $params(template) OutPutFormat elf SysProject $params(sysproj)]

			    } else {
				set fmt [dict create Name s Platform s System s Domain s Language s TemplateApp s OutPutFormat s]
				set data [dict create Name $params(name) Platform $params(platform) System "" Domain $params(domain) \
				Language [string toupper $params(lang)] TemplateApp $params(template) OutPutFormat elf]
			    }
			    set chan [getsdkchan]
			    xsdk_eval $chan "XSDx" createAppFromPlatform "o{$fmt}" e [list $data]
			}
		    } elseif { [info exists params(sysproj)] } {
			set fmt [dict create Name s Sysproj s Domain s Language s TemplateApp s OutPutFormat s]
			set data [dict create Name $params(name) Sysproj $params(sysproj) Domain $params(domain) \
				  Language [string toupper $params(lang)] TemplateApp $params(template) OutPutFormat elf]
			set chan [getsdkchan]
			xsdk_eval $chan "XSDx" createAppToSysProj "o{$fmt}" e [list $data]
		    }
		}
		return
	    }
	    list {
		set options {
		    {dict "returns the app list in tcl dict format"}
		}
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}
		array set params [::xsdb::get_options args $options 0]
		if { [llength $args] > 0 } {
		    error "Unexpected arguments: $args, should be \"app list\""
		}

		set chan [getsdkchan]
		set ret_list [xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type app]]]

		if { [lindex $ret_list 1] != "" } {
		    regsub -all {\;} [lindex $ret_list 1] "\n" projs
		} else {
		    error "No application exist"
		}

		set app_list {}
		foreach item $projs {
		    set chan [getsdkchan]
		    set retval [lindex [xsdk_eval $chan "XSDx" "reportApp" "o{AppName s}" eA [list [dict create AppName $item ]]] 1]
		    dict for {key value} $retval {
			if { $key == "fsblpath" || $key == "Platform Path" } { continue }
			if { $key == "Domain" } {
			    dict set app_dict $item "domain" $value
			    continue
			}
			dict set app_dict $item $key $value
		    }
		}
		if { $params(dict) } {
		    return $app_dict
		}
		set formatstr { %-15s %-20s %-15s }
		set border "[string repeat "=" 55]\n"
		set output $border
		append output "[format $formatstr "NAME" "DOMAIN" "PLATFORM" ]\n"
		append output $border
		dict for {key value} $app_dict {
		    dict with value {
			append output "[format $formatstr "$key" $domain $platform]\n"
		    }
		}
		return $output
	    }
	    remove {
		set options {
		    {name "project name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } else {
			error "Invalid arguments, should be \"app remove \<options>\""
		    }
		}

		set wksp "false"
		set chan [getsdkchan]
		set retval [xsdk_eval $chan "XSDx" deleteProjects "o{[dict create Name s Workspace s]}" e [list [dict create Name $params(name) Workspace $wksp Type app]]]
		if { [lindex $retval 0] != "" } {
		    error $retval
		}
	    }
	    report {
		set options {
		    {name "project name" {args 1}}
		    {dict "dictionary format"}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } else {
			error "Invalid arguments, should be \"app report <app-name>\""
		    }
		}

		set chan [getsdkchan]
		set retval [lindex [xsdk_eval $chan "XSDx" "reportApp" "o{AppName s}" eA [list [dict create AppName $params(name) Type app]]] 1]

		if { $params(dict) } {
		    return $retval
		}

		set formatstr {%-20s     %s}
		set border "[string repeat "=" 80]\n"
		set output $border
		append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		append output $border
		dict for {key value} $retval {
		    append output "[format $formatstr $key [string range $value 0 54]]\n"
		    set len [string length $value]
		    set pos 55
		    while { $len - $pos > 0 } {
			append output "[format $formatstr "" [string range $value $pos [expr $pos + 55]]]\n"
			incr pos 56
		    }
		}
		return $output
	    }
	    switch {
		set options {
		    {name "name of the application project" {args 1}}
		    {platform "platform name or xpfm path" {args 1}}
		    {domain "domain name" {default "" args 1}}
		    {help "command help"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] || ![info exists params(platform)] } {
		    error "Invalid arguments, should be \"app switch -name <app-name> -platform <platform-name>\""
		}

		set chan [getsdkchan]
		set retval [xsdk_eval $chan "XSDx" appSwitch "o{[dict create Name s Platform s Domain s]}" e [list [dict create Name $params(name) Platform $params(platform) Domain $params(domain)]]]
		if { [lindex $retval 0] != "" } {
		    error $retval
		}
	    }
	    default {
		error "Wrong sub-command, use \"help app\" for the list of sub-commands"
	    }
	}
    }
    namespace export app
    ::xsdb::setcmdmeta app categories {projects}
    ::xsdb::setcmdmeta app brief {Application project management.}
    ::xsdb::setcmdmeta app description {
SYNOPSIS {
    app <sub-command> [options]
        Create an application project, or perform various other operations on
        the application project, based on <sub-command> specified.
        Following sub-commands are supported.
            build  - Build the application project.
            clean  - Clean the application project.
            config - Configure C/C++ build settings of the application project.
            create - Create an application project.
            list   - List all the application projects in workspace.
            remove - Delete the application project.
            report - Report the details of the application project.
            switch - Switch application project to refer another platform.
        Type "help" followed by "app sub-command", or "app sub-command" followed
        by "-help" for more details.
}
OPTIONS {
    Depends on the sub-command. Refer to the sub-command help for details.
}
RETURNS {
    Depends on the sub-command. Refer to the sub-command help for details.
}
EXAMPLE {
    Refer to the sub-command help for examples.
}
SUBCMDS {
    build clean config create list remove report switch
}
}

    ::xsdb::setcmdmeta {app build} brief {Build application.}
    ::xsdb::setcmdmeta {app build} description {
SYNOPSIS {
    app build -name <app-name>
        Build the application specified by <app-name> in the workspace. "-name"
        switch is optional, so <app-name> can be specified directly, without
        using -name.
}
OPTIONS {
    -name <app-name>
        Name of the application to be built.

    -all
        Option to Build all the application projects.
}
RETURNS {
    Nothing. Build log will be printed on the console.
}
EXAMPLE {
    app build -name helloworld
        Build Hello World application.

    app build -all
        Build all the application projects in the workspace.
}
}

    ::xsdb::setcmdmeta {app clean} brief {Clean application.}
    ::xsdb::setcmdmeta {app clean} description {
SYNOPSIS {
    app clean -name <app-name>
        Clean the application specified by <app-name> in the workspace. "-name"
        switch is optional, so <app-name> can be specified directly, without
        using -name.
}
OPTIONS {
    -name <app-name>
        Name of the application to be clean built.
}
RETURNS {
    Nothing. Build log will be printed on the console.
}
EXAMPLE {
    app clean -name helloworld
        Clean Hello World application.
}
}

    ::xsdb::setcmdmeta {app config} brief {Configure C/C++ build settings of the application.}
    ::xsdb::setcmdmeta {app config} description {
SYNOPSIS {
    Configure C/C++ build settings for the specified application.
    Following settings can be configured for applications:
          assembler-flags         : Miscellaneous flags for assembler
          build-config            : Get/set build configuration
          compiler-misc           : Compiler miscellaneous flags
          compiler-optimization   : Optimization level
          define-compiler-symbols : Define symbols. Ex. MYSYMBOL
          include-path            : Include path for header files
          libraries               : Libraries to be added while linking
          library-search-path     : Search path for the libraries added
          linker-misc             : Linker miscellaneous flags
          linker-script           : Linker script for linking
          undef-compiler-symbols  : Undefine symbols. Ex. MYSYMBOL

    app config -name <app-name> <param-name>
        Get the value of configuration parameter <param-name> for the
        application specified by <app-name>.

    app config [OPTIONS] -name <app-name> <param-name> <value>
        Set/modify/remove the value of configuration parameter <param-name>
        for the application specified by <app-name>.
}
OPTIONS {
    -name
        Name of the application.

    -set
        Set the configuration parameter value to new <value>.

    -get
        Get the configuration parameter value.

    -add
        Append the new <value> to configuration parameter value.
        Add option is not supported for ,compiler-optimization

    -info
        Displays more information like possible values and possible
        operations about the configuration parameter. A parameter name
        must be specified when this option is used.

    -remove
        Remove <value> from the configuration parameter value.
        Remove option is not supported for assembler-flags, build-config,
        compiler-misc, compiler-optimization, linker-misc and linker-script.
}
RETURNS {
    Depends on the arguments specified.
    <none>
        List of parameters available for configuration and description of each
        parameter.

    <parameter name>
        Parameter value, or error, if unsupported parameter is specified.

    <parameter name> <paramater value>
        Nothing if the value is set successfully, or error, if unsupported
        parameter is specified.
}
EXAMPLE {
    app config -name test build-config
        Return the current build configuration for the application named test.

    app config -name test define-compiler-symbols FSBL_DEBUG_INFO
        Add -DFSBL_DEBUG_INFO to the compiler options, while building the test
        application.

    app config -name test -remove define-compiler-symbols FSBL_DEBUG_INFO
        Remove -DFSBL_DEBUG_INFO from the compiler options, while building the test
        application.

    app config -name test -set compiler-misc {-c -fmessage-length=0 -MT"$@"}
       Set {-c -fmessage-length=0 -MT"$@"} as compiler miscellaneous flags for
       the test application.

     app config -name test -append compiler-misc {-pg}
       Add {-pg} to compiler miscellaneous flags for
       the test application.

    app config -name test -info compiler-optimization
       Display more information about possible values and default values for
       compiler optimization level.
}
}

    ::xsdb::setcmdmeta {app create} brief {Create an application.}
    ::xsdb::setcmdmeta {app create} description {
SYNOPSIS {
    app create [options] -platform <platform> -domain <domain>
                         -sysproj <system-project>

        Create an application using an existing platform and domain,
        and add it to a system project. If <platform> and <domain> are not specified,
        then active platform and domain are used for creating the application.
        If <system-project> is not specified, then a system project is created
        with name appname_system. For creating applications and adding them
        to existing system project, refer to next use case.

        Supported options are: -name, -template.

    app create [options] -sysproj <system-project> -domain <domain>

        Create an application for domain specified by <domain> and add it to
        system project specified by <system-project>.
        If <system-project> exists, platform corresponding to
        this system project are used for creating the application.
        If <domain> is not specified, then active domain is used.

        Supported options are: -name, -template.

    app create [options] -hw <hw-spec> -proc <proc-instance>

        Create an application for processor core specified <proc-instance> in
        HW platform specified by <hw-spec>.

        Supported options are: -name, -template, -os, -lang, -arch.
}
OPTIONS {
    -name <application-name>
        Name of the application to be created.

    -platform <platform-name>
        Name of the platform.
        Use "repo -platforms" to list available pre-defined platforms.

    -domain <domain-name>
        Name of the domain.
        Use "platform report <platform-name>" to list the available
        system configurations in a platform.

    -hw <hw-spec>
        HW specification file exported from Vivado (XSA).

    -sysproj <system-project>
        Name of the system project.
        Use "sysproj list" to know available system projects in the workspace.

    -proc <processor>
        Processor core for which the application should be created.

    -template <application template>
        Name of the template application. Default is "Hello World".
        Use "repo -apps" to list available template applications.

    -os <os-name>
        OS type. Default type is standalone.

    -lang <programming language>
        Programming language can be c or c++.

    -arch <arch-type>
        Processor architecture, <arch-type> can be 32 or 64 bits.
        This option is used to build the project with 32/64 bit toolchain.
}
RETURNS {
    Nothing, if the application is created successfully.
    Error string, if the application creation fails.
}
EXAMPLE {
    app create -name test -platform zcu102 -domain a53_standalone
        Create Hello World application named test, for the platform zcu102,
        with a domain named a53_standalone.

    app create -name zqfsbl -hw zc702 -proc ps7_cortexa9_0 -os standalone
               -template "Zynq FSBL"
        Create Zynq FSBL application named zqfsbl for ps7_cortexa9_0
        processor core, in zc702 HW platform.

    app create -name memtest -hw /path/zc702.xsa -proc ps7_cortexa9_0 -os standalone
               -template "Memory Tests"
        Create Memory Test application named memtest for ps7_cortexa9_0
        processor core, in zc702.xsa HW platform.

    app create -name test -sysproj test_system -domain test_domain
        Create Hello World application project with name test and add it to
        system project test_system.
}
}

    ::xsdb::setcmdmeta {app list} brief {List applications.}
    ::xsdb::setcmdmeta {app list} description {
SYNOPSIS {
    app list
        List all applications for in the workspace.
}
OPTIONS {
    -dict
        List all the applications for the workspace in Tcl dictionary format.
        Without this option, applications are listed in tabular format.
}
RETURNS {
    List of applications in the workspace. If no applications exist,
    "No application exist" string is returned.
}
EXAMPLE {
    app list
        Lists all the applications in the workspace in tabular format.

    app list -dict
        Lists all the applications in the workspace in Tcl dictionary format.
}
}

    ::xsdb::setcmdmeta {app remove} brief {Delete application.}
    ::xsdb::setcmdmeta {app remove} description {
SYNOPSIS {
    app remove <app-name>
        Delete an application from the workspace.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing, if the application is deleted successfully.
    Error string, if the application deletion fails.
}
EXAMPLE {
    app remove zynqapp
        Removes zynqapp from workspace.
}
}

    ::xsdb::setcmdmeta {app report} brief {Report details of the application.}
    ::xsdb::setcmdmeta {app report} description {
SYNOPSIS {
    app report <app-name>
        Return details such as the platform, domain, processor
        core, and OS of an application.
}
OPTIONS {
    None.
}
RETURNS {
    Details of the application, or error string, if application does not exist.
}
EXAMPLE {
    app report test
        Return all the details of application test.
}
}

    ::xsdb::setcmdmeta {app switch} brief {Switch the application to use another domain/platform.}
    ::xsdb::setcmdmeta {app switch} description {
SYNOPSIS {
    app switch -name <app-name> -platform <platform-name> -domain <domain-name>
        Switch the application to use another platform and domain.
        If the domain name is not specified, application will be moved to the first
        domain which is created for the same processor as current domain.
        This option is supported if there is only one application under this platform.

    app switch -name <app-name> -domain <domain-name>
        Switch the application to use another domain within the same platform.
        New domain should be created for the same processor as current domain.
}
OPTIONS {
    -name <application-name>
        Name of the application to be switched.

    -platform <platform-name>
        Name of the new Platform.
        Use "platform -list" to list the available platforms.

    -domain <domain-name>
        Name of the new domain.
        Use "domain -list" to list avaliable domain in the active platform.
}
RETURNS {
    Nothing if application is switched successfully, or error string,
    if given platform project does not exist or given platform
    project does not have valid domain.
}
EXAMPLE {
    app switch -name helloworld -platform zcu102
        Switch the Hello World application to use zcu102 platform.
}
}

    #---------------------------------------------------------------------------------------#
    # Create a system project
    # Description:  Creates a system project  with the given arguments
    # Arguments  :
    # Type       :  SDx command
    #---------------------------------------------------------------------------------------#
    proc sysproj { args } {
	if { [llength $args] == 0 } {
	    error "Wrong # of args: should be \"sysproj <sub-command> \[options]\""
	}

	set subcmd [lindex $args 0]

	if { [lindex $args 0] == "-help" } {
	    return [help sysproj]
	}

	set args [lrange $args 1 end]
	switch -- $subcmd {
	    build {
		set options {
		    {name "system project name" {args 1}}
		    {all "all system projects"}
		    {help "command help"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { $params(all) } {
		    set params(name) "all"
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] != 1 } {
			error "Invalid arguments, specify name of the system project"
		    }
		}

		set chan [getsdkchan]
		xsdk_eval $chan XSDx build "o{[dict create Type s Name s]}" e [list [dict create Type "system" Name $params(name)]]
		return
	    }
	    clean {
		set options {
		    {name "system project name" {args 1}}
		    {help "command help"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } elseif { [llength $args] != 1 } {
			error "Invalid arguments, specify name of the system project"
		    }
		}

		set chan [getsdkchan]
		xsdk_eval $chan XSDx clean "o{[dict create Type s Name s]}" e [list [dict create Type "system" Name $params(name)]]
		return
	    }
	    list {
		if { [lsearch $args "-help"] != -1 } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		set params(dict) 0
		if { [lsearch $args "-dict"] != -1 } {
		    set params(dict) 1
		} elseif { [llength $args] > 0 } {
		    error "Unexpected arguments: $args, should be \"sysproj list\""
		}

		set chan [getsdkchan]
		set ret_list [xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type system]]]
		if { [lindex $ret_list 1] != "" } {
		    regsub -all {\;} [lindex $ret_list 1] "\n" projs
		} else {
		    error "No system project exist"
		}

		set sysproj_list {}
		if { $params(dict) } {
		    foreach item $projs {
			set sysproj_dict [dict create "Name" $item]
			lappend sysproj_list $sysproj_dict
		    }
		    return $sysproj_list
		}
		set formatstr {%15s     %s}
		set border "[string repeat "=" 40]\n"
		set output $border
		append output "[format $formatstr "SYSTEM NAME" ""]\n"
		append output $border
		append output "[format $formatstr $projs ""]\n"
		return $output
	    }
	    remove {
		set options {
		    {name "project name" {args 1}}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } else {
			error "Invalid arguments, should be \"sysproj remove <options>\""
		    }
		}

		set wksp "false"
		set chan [getsdkchan]
		set retval [xsdk_eval $chan "XSDx" deleteProjects "o{[dict create Name s Workspace s]}" e [list [dict create Name $params(name) Workspace $wksp Type system]]]
		if { [lindex $retval 0] != "" } {
		    error $retval
		}
	    }
	    report {
		set options {
		    {name "project name" {args 1}}
		    {dict "dictionary format"}
		    {help "help command"}
		}
		array set params [::xsdb::get_options args $options 0]

		if { $params(help) } {
		    return [help [lindex [split [lindex [info level 0] 0] ::] end] \
			    [lindex [split [lindex [info level 0] 1] ::] end]]
		}

		if { ![info exists params(name)] } {
		    if { [llength $args] == 1 } {
			set params(name) [lindex $args 0]
		    } else {
			error "Invalid arguments, should be \"sysproj report <options>\""
		    }
		}

		set chan [getsdkchan]
		set retval [lindex [xsdk_eval $chan "XSDx" "reportApp" "o{AppName s}" eA [list [dict create AppName $params(name) Type system]]] 1]

		if { $params(dict) } {
		    return $retval
		}

		set formatstr {%-20s     %s}
		set border "[string repeat "=" 80]\n"
		set output $border
		append output "[format $formatstr "PROPERTY" "VALUE"]\n"
		append output $border
		dict for {key value} $retval {
		    append output "\n[format $formatstr $key [string range $value 0 59]]"
		    set len [string length $value]
		    set pos 60
		    while { $len - $pos > 0 } {
			append output "\n[format $formatstr "" [string range $value $pos [expr $pos + 60]]]"
			incr pos 61
		    }
		}
		return $output
	    }
	    default {
		error "Wrong sub-command, use \"help sysproj\" for the list of sub-commands"
	    }
	}
    }
    namespace export sysproj
    ::xsdb::setcmdmeta sysproj categories {projects}
    ::xsdb::setcmdmeta sysproj brief {System project management.}
    ::xsdb::setcmdmeta sysproj description {
SYNOPSIS {
    sysproj <sub-command> [options]
        Build, list and report system project, based on <sub-command> specified.
        Following sub-commands are supported.
            build  - Build the system project.
            clean  - Clean the system project.
            list   - List all system projects in workspace.
            remove - Delete the system project.
            report - Report the details of the system project.
        Type "help" followed by "sysproj sub-command", or "sysproj sub-command" followed
        by "-help" for more details.
}
OPTIONS {
    Depends on the sub-command.
}
RETURNS {
    Depends on the sub-command.
}
EXAMPLE {
    See sub-command help for examples.
}
SUBCMDS {
    build clean list remove report
}
}

    ::xsdb::setcmdmeta {sysproj build} brief {Build system project.}
    ::xsdb::setcmdmeta {sysproj build} description {
SYNOPSIS {
    sysproj build -name <sysproj-name>
        Build the application specified by <sysproj-name> in the workspace. "-name"
        switch is optional, so <sysproj-name> can be specified directly, without
        using -name.
}
OPTIONS {
    -name <sysproj-name>
        Name of the system project to be built.

    -all
        Option to build all the system projects.
}
EXAMPLE {
    sysproj build -name helloworld_system
        Build the system project specified.

    sysproj build -all
        Build all the system projects in the workspace.
}
}

    ::xsdb::setcmdmeta {sysproj clean} brief {Clean application.}
    ::xsdb::setcmdmeta {sysproj clean} description {
SYNOPSIS {
    sysproj clean -name <app-name>
        Clean the application specified by <sysproj-name> in the workspace. "-name"
        switch is optional, so <sysproj-name> can be specified directly, without
        using -name.
}
OPTIONS {
    -name <sysproj-name>
        Name of the application to be clean built.
}
RETURNS {
    Nothing, if the application is cleaned suceessfully.
    Error string, if the application build clean fails.
}
EXAMPLE {
    sysproj clean -name helloworld_system
        Clean-build the system project specified.
}
}

    ::xsdb::setcmdmeta {sysproj list} brief {List system projects.}
    ::xsdb::setcmdmeta {sysproj list} description {
SYNOPSIS {
    sysproj list
        List all system projects in the workspace.
}
OPTIONS {
    None.
}
RETURNS {
    List of system projects in the workspace. If no system project exist, an empty
    string is returned.
}
EXAMPLE {
    sysproj list
        List all system projects in the workspace.
}
}

    ::xsdb::setcmdmeta {sysproj remove} brief {Delete system project.}
    ::xsdb::setcmdmeta {sysproj remove} description {
SYNOPSIS {
    sysproj remove [options]
        Delete a system project from the workspace.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing, if the system project is deleted successfully.
    Error string, if the system project deletion fails.
}
EXAMPLE {
    sysproj remove test_system
        Delete test_system from workspace.
}
}

    ::xsdb::setcmdmeta {sysproj report} brief {Report details of the system project.}
    ::xsdb::setcmdmeta {sysproj report} description {
SYNOPSIS {
    sysproj report <sysproj-name>
        Return the details such as the platform and domain
        of a system project.
}
OPTIONS {
    None.
}
RETURNS {
    Details of the system project, or error string, if system project does not exist.
}
EXAMPLE {
    sysproj report test_system
        Return all the details of the system project test_system.
}
}

    proc createapp {args} {
	puts "\nNote:: \"createapp\" command is deprecated. Use \"app create\" command"
	variable sdk_workspace
	variable help_prefix
	set options {
	    {name "project name" {args 1}}
	    {app "application template" {default {Hello World} args 1}}
	    {proc "target processor" {args 1}}
	    {hwproject "hwproject name" {args 1}}
	    {bsp "bsp project name" {default "" args 1}}
	    {os "project OS" {default "standalone" args 1}}
	    {lang "project launguage" {default "c" args 1}}
	    {arch "32/64 bit" {default "" args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(name)] } {
	    error "project name not specified"
	}

	if { ![info exists params(hwproject)] } {
	    error "specify a hwproject name to create an application project"
	}
	if { ![info exists params(proc)] } {
	    error "specify a processor instance to create an application project"
	}
	set platform_set 0
	set chan [getsdkchan]
	if { [info exists params(hwproject)] } {
	    set predef_hw [dict create ZC702_hw_platform zc702 ZC706_hw_platform zc706 ZCU102_hw_platform zcu102 zed_hw_platform zed]
	    dict for {key value} $predef_hw {
		if { $key == $params(hwproject) } {
		    set xsa_path "$::env(XILINX_VITIS)/data/embeddedsw/lib/fixed_hwplatforms/$value.xsa"
		    ::scw::platform create -name $params(hwproject) -hw $xsa_path
		    set platform_set 1
		    break
		}
	    }
	    set retval [builtin_platform -list]
	    if { $retval == "" } {
		error "HW project $params(hwproject) does not exist"
	    }
	    set retdict [::json::json2dict $retval]
	    dict for {key value} $retdict {
		if { $params(hwproject) == $key} {
		    builtin_platform -active $key
		    set platform_name $key
		    set platform_set 1
		}
	    }
	    if { $platform_set == 0 } {
		error "HW project $params(hwproject) does not exist"
	    }
	}

	if { [string compare -nocase $params(os) "Linux"] == 0 } {
	    error "createapp command cannot be used for linux applications"
	} else {
	    if { $params(bsp) == "" } {
		#create domain
		#need to change this, if sysconfig active gives success message
		if { [catch { [builtin_domain -active "$params(name)_bsp"] } msg]} {
			set app_name [::scw::get_app_template $params(app)]
			::scw::domain create -name "$params(name)_bsp" -proc $params(proc) -os $params(os) -support-app $app_name
			set domain_name $params(name)_bsp
		}
	    } else {
		::scw::domain active "$params(bsp)"
		set domain_name $params(bsp)
	    }
	    ::scw::platform generate
	}

	if { ($params(arch) == 32) || ($params(arch) == 64) || ($params(arch) == "") } {
	    if { [string match "psu_cortexa53*" $params(proc)] } {
		if { $params(arch) == "" } {
		    set params(arch) 64
		}
	    } else {
		if { $params(arch) == 64 } {
		    puts "WARNING: $params(arch)-bit not supported for the processor type - $params(proc). Setting 32-bit"
		}
		set params(arch) 32
	    }
	} else {
	    error "illegal arch type $params(arch). must be 32/64"
	}
	return [app create -name $params(name) -lang $params(lang) -template $params(app) -platform $platform_name -domain $domain_name]

    }
    namespace export createapp

    proc createlib {args} {
	puts "\nNote:: \"createlib\" command is deprecated. Use \"library create\" command"
	variable sdk_workspace
	variable help_prefix

	set options {
	    {name "library name" {args 1}}
	    {type "library type" {default "" args 1}}
	    {proc "target processor" {args 1}}
	    {os "project OS" {default "linux" args 1}}
	    {lang "project launguage" {default "c" args 1}}
	    {arch "32/64 bit" {default "" args 1}}
	    {flags "compiler flags" {default "" args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set plat_name ""
	if { ![info exists params(name)] } {
	    error "Project name not specified"
	}
	if { ![info exists params(proc)] } {
	    error "Processor type not specified"
	}
	if { ($params(arch) == 32) || ($params(arch) == 64) || ($params(arch) == "") } {
	    if { [string match "psu_cortexa53" $params(proc)] } {
		if { $params(arch) == "" } {
		    set params(arch) 64
		}
	    } else {
		if { $params(arch) == 64 } {
		    puts "WARNING: $params(arch)-bit not supported for the processor type - $params(proc). Setting 32-bit"
		}
		set params(arch) 32
	    }
	} else {
	    error "illegal arch type $params(arch). must be 32/64"
	}
	if { $params(type) == "" } {
	    if { $params(os) == "linux" } {
		set params(type) "shared"
	    } else {
		set params(type) "static"
	    }
	}

	set chan [getsdkchan]

	if { $params(os) == "linux" } {
	    error "createlib command cannot be used for linux projects"
	}

	if { $plat_name != "" } {
	    if { $params(os) == "linux" } {
		scw::library create -name $params(name) -type $params(type) -platform $plat_name -domain $dom_name
	    } else {
		scw::platform create -name $params(name)_p -hw $params(hw)
		scw::domain create -name $params(name)_d -proc $params(proc) -os $params(os)
		scw:platform generate
		scw::library create -name $params(name) -type $params(type) -platform $params(name)_p -domain $params(name)_d
	    }
	} else {
	    error "Cannot create library project for this platform"
	}
	return
    }
    namespace export createlib

    proc projects { args } {
	puts "\nNote:: \"projects\" command is deprecated. Use \"app build\" or \"app clean\" command"
	variable sdk_workspace
	variable help_prefix

	set options {
	    {build "builds the project"}
	    {clean "cleans the project"}
	    {type "build type" {default "all" args 1}}
	    {name "project name" {default "" args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { $params(name) != "" } {
	    if { $params(type) == "" || $params(type) == "all" } {
		error "Specify project type \"bsp\" or \"app\", when project name is specified"
	    }
	}

	if { $params(type) == "all" } {
		set params(name) "all"
	} else {
	    if { $params(type) != "bsp" && $params(type) != "app" } {
		error "Unknown project type $params(type). should be \"all\", \"bsp\" or \"app\""
	    }
	    if { $params(name) == ""} {
		error "Invalid project name"
	    }
	}

	set chan [getsdkchan]
	if { $params(build) } {
		if { $params(type) == "bsp" } {
			return [builtin_platform -generate]
		} else {
			xsdk_eval $chan XSDx build "o{[dict create Type s Name s]}" e [list [dict create Type $params(type) Name $params(name)]]
		}
		return
	}
	if { $params(clean) } {
	    xsdk_eval $chan XSDx clean "o{[dict create Type s Name s]}" e [list [dict create Type $params(type) Name $params(name)]]
	    return
	}
    }
    namespace export projects

    proc importprojects { args } {
	variable sdk_workspace
	variable help_prefix

	set options {
	    # leave this for backward compatibility
	    {path "project(s) path" {args 1}}
	    {silent "do not update the progress"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}
	if { ![info exists params(path)] } {
	    if { [llength $args] > 0 } {
		set params(path) [lindex $args 0]
		set params(path) [file normalize $params(path)]
		set args [lrange $args 1 end]
	    } else {
		error "Project path not specified"
	    }
	}

	if { [llength $args] } {
	    error "Unexpected arguments: $args"
	}

	set chan [getsdkchan]
	if { $params(silent)  == 1 } {
	    xsdk_eval $chan "XSDx" importProjects "o{[dict create Path s Silent s]}" e [list [dict create  Path $params(path) Silent yes]]
	} else {
	    xsdk_eval $chan "XSDx" importProjects "o{[dict create Path s  Silent s]}" e [list [dict create Path $params(path) Silent no]]
	}
	return
    }
    namespace export importprojects
    ::xsdb::setcmdmeta importprojects categories {projects} [subst $help_prefix]
    ::xsdb::setcmdmeta importprojects brief {Import projects to workspace.} [subst $help_prefix]
    ::xsdb::setcmdmeta importprojects description [subst {
SYNOPSIS {
    [concat $help_prefix importprojects] <path>
        Import all the Vitis projects from <path> to workspace.
}
RETURNS {
    Nothing, if the projects are imported successfully.
    Error string, if project path is not specified or if the projects cannot be
    imported.
}
EXAMPLE {
    importprojects /tmp/wrk/wksp1/hello1
        Import Vitis project(s) into the current workspace.
}
}] [subst $help_prefix]

    proc importsources { args } {
	variable sdk_workspace
	variable help_prefix

	set options {
	    {name "project name" {args 1}}
	    {path "sources path" {args 1}}
	    {target-path "target directory path" {default "src" args 1}}
	    {soft-link "do not copy sources, create only links" {args 0}}
	    {linker-script "copy linker script" {args 0}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(name)] } {
	    error "Project name not specified"
	}

	if { ![info exists params(path)] } {
	    error "sources path not specified"
	}

	check_sdk_workspace
	if { $params(soft-link) } {
	    set fmt [dict create ProjName s Path s Target s]
	    set data [dict create ProjName $params(name) Path $params(path) Target $params(target-path)]
	    set chan [getsdkchan]
	    xsdk_eval $chan "XSDx" ImportSources "o{$fmt}" e [list $data]
	    return
	}

	if { ![file exists $sdk_workspace/$params(name)/$params(target-path)] } {
	    file mkdir $sdk_workspace/$params(name)/$params(target-path)
	}

	if { [file isfile $params(path)] } {
	    if { [catch {file copy -force $params(path) $sdk_workspace/$params(name)/$params(target-path)} msg ] } { error $msg }
	} elseif { [file isdirectory $params(path)] } {
	    # Copy all the files except linker
	    foreach filelist [glob -dir $params(path) *] {
		if { [file extension $filelist] != ".ld" } {
		    if { [catch {file copy -force $filelist $sdk_workspace/$params(name)/$params(target-path)} msg ] } { error $msg }
		}
	    }
	    if { $params(linker-script) } {
		foreach lfile [glob -dir $params(path) "*.ld"] {
		    set ld_file [file normalize $lfile]
		    if { [catch {file copy -force $ld_file $sdk_workspace/$params(name)/$params(target-path)} msg ] } { error $msg }
		}
	    }
	} else {
	    error "cannot read $params(path)"
	}
    }
    namespace export importsources
    ::xsdb::setcmdmeta importsources categories {projects} [subst $help_prefix]
    ::xsdb::setcmdmeta importsources brief {Import sources to an application project.} [subst $help_prefix]
    ::xsdb::setcmdmeta importsources description [subst {
SYNOPSIS {
    [concat $help_prefix importsources] \[OPTIONS\]
        Import sources from a path to application project in workspace.
}
OPTIONS {
    -name <project-name>
        Application Project to which the sources should be imported.

    -path <source-path>
        Path from which the source files should be imported.
        If <source-path> is a file, it is imported to application project.
        If <source-path> is a directory, all the files/sub-directories from
        the <source-path> are imported to application project. All existing
        source files will be overwritten in the application, and new files
        will be copied. Linker script will not be copied to the application
        directory, unless -linker-script option is used.

    -soft-link
        Links the sources from source-path and does not copy the source.

    -target-path <dir-path>
        Directory to which the sources have to be linked or copied.
        If target-path option is not used, source files will be linked
        or copied to "src" directory.

    -linker-script
        Copies the linker script as well.
}
RETURNS {
    Nothing, if the project sources are imported successfully.
    Error string, if invalid options are used or if the project sources cannot
    be read/imported.
}
EXAMPLE {
    importsources -name hello1 -path /tmp/wrk/wksp2/hello2
        Import the 'hello2' project sources to 'hello1' application project
        without the linker script.

    importsources -name hello1 -path /tmp/wrk/wksp2/hello2 -linker-script
        Import the 'hello2' project sources to 'hello1' application project
        along with the linker script.

    importsources -name hello1 -path /tmp/wrk/wksp2/hello_app -soft-link
        Create a soft-link to hello1 application project from hello_app
        application project.
}
}] [subst $help_prefix]

    proc export_to_ds5 {args } {
	variable help_prefix
	set options {
	    {name "project name" {args 1}}
	    {path "target path" {args 1}}
	}
	array set params [::xsdb::get_options args $options]

	if { ![info exists params(name)] } {
	    error "Project name not specified"
	}
	if { ![info exists params(path)] } {
	    error "Target path not specified"
	}

	set chan [getsdkchan]
	xsdk_eval $chan "Xsdk" migrate2Ds5 "o{[dict create Name s Path s]}" e [list [dict create Name $params(name) Path $params(path)]]
	return
    }
    namespace export export_to_ds5

    proc getprojects { args } {
	variable sdk_workspace
	variable help_prefix
	set options {
	    {type "project type" {default "all" args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	set projs ""
	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { $params(type) != "all" && $params(type) != "system" && $params(type) != "platform"  &&  $params(type) != "app" } {
	    error "Unknown project type $params(type). should be \"all\", \"system\", \"platform\" or \"app\""
	}

	set chan [getsdkchan]
	set ret_list [xsdk_eval $chan "XSDx" getProjects "o{[dict create Type s]}" eA [list [dict create Type $params(type)]]]
	if { [lindex $ret_list 1] != "" } {
	    regsub -all {\;} [lindex $ret_list 1] "\n" projs
	}
	return $projs
    }
    namespace export getprojects

    proc getreport { app } {
	set chan [getsdkchan]
	return [xsdk_eval $chan "XSDx" "reportApp" "o{AppName s}" eA [list [dict create AppName $app]]]
    }

    proc list_platforms_dir { } {
	set chan [getsdkchan]
	set res [xsdk_eval $chan "XSDx" "listPlatformRepo" "" s [list]]
	if { [string length $res ]  == "2" } {
	    puts "No custom platform directory is added. Please add using repo -add-platforms command."
	    return
	}
	if { [string index $res  0]  == "\{" } {
	    set res [string range $res 1 [expr [string length $res] -2 ]]
	}
	set dirs [split $res ";"]
	set i 0
	set formatstr {%15s}
	set border "[string repeat "-" 80]\n"
	set output $border
	append output "[format $formatstr "Custom platform directories"]\n"
	append output $border
	foreach direntry $dirs {
	    append output "[format $formatstr $direntry]\n"
	}
	return $output
    }

    proc remove_platforms_dir { dir } {
	set chan [getsdkchan]
	return [xsdk_eval $chan "XSDx" "removePlatformRepo" "o{Paths s}" eA [list [dict create Paths $dir]]]
    }

    proc get_apps_supported { plat } {
	set chan [getsdkchan]
	set retval [xsdk_eval $chan "XSDx" "listAppsForPlatform" "o{Platform s}" eA [list [dict create Platform $plat]]]
	return $retval
    }

    proc reportplatform { plat } {
	set projs ""
	set chan [getsdkchan]
	set retval [xsdk_eval $chan "XSDx" "reportPlatform" "o{Platform s}" eA [list [dict create Platform $plat]]]
	set formatstr {%-15s     %s}
	set border "[string repeat "=" 45]\n"
	set output $border
	append output "[format $formatstr "PROPERTY" "VALUE"]\n"
	dict for {keys plat} $retval {
	    dict for {syscfg dom} $plat {
		append output $border
		append output "[format $formatstr "sysconfig name" $syscfg]\n"
		dict for {domname dominfo} $dom {
		    dict for {key value} $dominfo {
			append output "[format $formatstr $key $value]\n"
		    }
		}
	    }
	}
	return $output
    }

    proc isAIEApp { app } {
	set chan [getsdkchan]
	set retval [xsdk_eval $chan "XSDx" "reportApp" "o{AppName s}" eA [list [dict create AppName $app Type app]]]
	set plat ""
	set sysconfig ""
	set domain ""
	dict for {mainkey mainvalue} $retval {
	    dict for {key value} $mainvalue {
		if { $key == "platform"} {
		    set plat $value
		}
		if { $key == "Domain"} {
		    set domain $value
		}
	    }
	}
	if { $plat == "" || $domain == ""  } {
	    return 0
	} else {
	    if { [isAIEDomain $plat $domain ] == "1" } {
		return 1
	    } else {
		return 0
	    }
	}
	return 0
    }

    proc isAIEDomain { plat domain } {
	set chan [getsdkchan]
	set retval [xsdk_eval $chan "XSDx" "reportPlatform" "o{Platform s}" eA [list [dict create Platform $plat]]]
	dict for {keys plat} $retval {
	    dict for {syscfg dom} $plat {
		dict for {domname dominfo} $dom {
		    if { $domname == $domain } {
			dict for {key value} $dominfo {
			    if { $key == "CpuType"  && $value == "ai_engine"} {
				return 1
			    }
			}
		    }
		}
	    }
	}
	return 0
    }

    proc getplatforms { args } {
	set projs ""
	set chan [getsdkchan]
	set ret_list [xsdk_eval $chan "XSDx" getPlatforms "o{[dict create Type s]}" eA [list [dict create Type "sdk"]]]
	if { [lindex $ret_list 1] != "" } {
	    set projs [split [lindex $ret_list 1] ";"]
	}
	set formatstr {%-15s     %s}
	set border "[string repeat "=" 80]\n"
	set output $border
	append output "[format $formatstr "PLATFORM NAME" "PATH"]\n"
	append output $border
	if { $projs != "" } {
	    foreach detail $projs {
		set projdetails [split $detail "|"]
		append output "[format $formatstr [lindex $projdetails 0] [string range [lindex $projdetails 1] 0 59]]\n"
		set len [string length [lindex $projdetails 1]]
		set pos 60
		while { $len - $pos > 0 } {
		    append output "[format $formatstr "" [string range [lindex $projdetails 1] $pos [expr $pos + 60]]]\n"
		    incr pos 61
		}
	    }
	}
	return $output
    }

    proc addplatforms { args } {
	# To enforce a call to get the list of platforms using the Platform API, from GUI.
	set platformslist [::scw::platform list]
	set projs ""
	set chan [getsdkchan]
	if { [llength $args] != 1 } {
	    error "Wrong # args: should be \"sdk addplatforms path\""
	}
	set repo_path [lindex $args 0]
	xsdk_eval $chan "XSDx" "addPlatformRepo" "o{Paths s}" e [list [dict create Paths [file normalize $repo_path]]]
	return
    }

    proc getosfromrepo { args } {
	set projs ""
	set chan [getsdkchan]
	set retval [xsdk_eval $chan "XSDx" "reportPlatform" "o{Platform s}" eA [list [dict create Platform [lindex $args 0]]]]
	dict for {keys plat} $retval {
	    dict for {syscfg dom} $plat {
		if { [lindex $args 1] == $syscfg } {
		    dict for {domname dominfo} $dom {
			if { [lindex $args 2] == $domname } {
			    dict for {key value} $dominfo {
				if { [string compare -nocase $key "os"] == 0 } {
				    return $value
				}
			    }
			}
		    }
		}
	    }

	}
	return
    }

    proc deleteprojects { args } {
	puts "\nNote:: \"deleteprojects\" command is deprecated. Use \"app remove\" command"
	variable sdk_workspace
	variable help_prefix
	set options {
	    {name "project name" {args 1}}
	    {workspace-only "delete from workspace only" {default 0}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(name)] } {
	    error "Project name not specified"
	}

	set wksp "false"
	if { $params(workspace-only) == 1 } {
	    set wksp "true"
	}

	set chan [getsdkchan]
	set retval [xsdk_eval $chan "XSDx" deleteProjects "o{[dict create Name s Workspace s]}" e [list [dict create Name $params(name) Workspace $wksp]]]
	if { [lindex $retval 0] != "" } {
	    error $retval
	}
	return
    }
    namespace export deleteprojects

    proc configapp { args } {
	puts "\nNote:: \"configapp\" command is deprecated. Use \"app config\" command"
	variable sdk_workspace
	variable help_prefix
	set options {
	    {app "application name" {args 1}}
	    {set "set a param value" {default 0}}
	    {add "add to a param value"}
	    {remove "delete a param value"}
	    {info "more info of param value"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { $params(set) + $params(add) + $params(remove) + $params(info) > 1 } {
	    error "Conflicting options; use only one of -info, -add, -set, or -remove"
	}

	set chan [getsdkchan]
	if { [isAIEApp $params(app) ] == "1" } {
	    set defs [lindex [xsdk_eval $chan SDxBuildSettings getAIEDefinitions s "eA" [list ""]] 1]
	} else {
	    set defs [lindex [xsdk_eval $chan SDxBuildSettings getDefinitions s "eA" [list ""]] 1]
	}

	set names [lsort [dict keys $defs]]
	if { [llength $args] == 0 } {
	    if { $params(set) + $params(add) + $params(remove) + $params(info) != 0 } {
		error "Conflicting options specified"
	    }
	    set result ""
	    foreach name $names {
		if { $result != "" } {
		    append result "\n"
		}
		append result [format "  %-30s %s" $name [::xsdb::dict_get_safe $defs $name description]]
	    }
	    return $result
	}

	if { [llength $args] > 2 } {
	    error "Unexpected arguments: $args. should be \"configapp \[options\] \[name \[value\]\]\""
	}
	set name [lsearch -all -inline -glob $names "[lindex $args 0]*"]
	if { [llength $name] != 1 } {
	    if { [llength $name] == 0 } {
		set name $names
	    }
	    error "Unknown or ambiguous parameter \"[lindex $args 0]\": must be [join $name {, }]"
	}
	set name [lindex $name 0]

	if { [info exists params(app)] } {
	    if { [lsearch [::sdk::getprojects] $params(app)] == -1 } {
		error "Application project '$params(app)' doesn't exist in the workspace\nuse 'getprojects -type app' to get a list of application projects in workspace"
	    }
	} else {
	    error "Application name not specified"
	}

	if { [llength $args] == 2 } {
	    if { $params(info) } {
		error "-info is not supported while setting a parameter value"
	    }
	    set value [lindex $args 1]
	    set props [::xsdb::dict_get_safe $defs $name props]
	    if { $params(set) + $params(add) + $params(remove) == 0 } {
		set params([lindex $props 0]) 1
	    }

	    foreach prop [array names params] {
		if { $params($prop) == 1 } {
		    if { [lsearch $props $prop] == -1 } {
			error "parameter $name doesn't support $prop operation"
		    }
		    xsdk_eval $chan SDxBuildSettings $prop sss e [list $params(app) $name $value]
		    return
		}
	    }
	}

	if { $params(info) } {
	    set result [format "  %-20s : %s\n" "Possible Values" [::xsdb::dict_get_safe $defs $name values]]
	    append result [format "  %-20s : %s\n" "Possible Operations" [regsub -all { } [::xsdb::dict_get_safe $defs $name props] {, }]]
	    append result [format "  %-20s : %s" "Default Operation" [lindex [::xsdb::dict_get_safe $defs $name props] 0]]
	    return $result
	}

	return [lindex [xsdk_eval $chan SDxBuildSettings get ss eA [list $params(app) $name]] 1]
    }
    namespace export configapp

    proc toolchain { args } {
	variable sdk_workspace
	variable help_prefix
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help [subst $help_prefix][lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set chan [getsdkchan]
	set defs [lindex [xsdk_eval $chan XSDx getAvailableToolchains s "eA" [list ""]] 1]
	set names [lsort [dict keys $defs]]
	if { [llength $args] == 0 } {
	    set fmt "  %-15s %s"
	    set result [format $fmt Name {Supported CPU Types}]
	    append result "\n[format $fmt "====" "==================="]"
	    foreach name $names {
		append result "\n"
		set desc [join [::xsdb::dict_get_safe $defs $name {cpu types}] {, }]
		append result [format $fmt $name $desc]
	    }
	    return $result
	}

	set proc_type [lindex $args 0]
	if { [llength $args] == 2 } {
	    set name [lsearch -all -inline -glob $names "[lindex $args 1]"]
	    if { [llength $name] != 1 } {
		error "Unknown toolchain \"[lindex $args 1]\": must be [join $names {, }]"
	    }
	    set name [lindex $args 1]
	    if { [lsearch [::xsdb::dict_get_safe $defs $name "cpu types"] $proc_type] == -1 } {
		error "$name is not a supported toolchain for processor type $proc_type"
	    }
	    return [lindex [xsdk_eval $chan XSDx setToolchain ss e [list $proc_type $name]] 1]
	}

	if { [llength $args] != 1 } {
	    error {"Wrong # args: should be \"toolchain \[processor-type \[tool-chain\]\]"}
	}

	return [lindex [xsdk_eval $chan XSDx getToolchain s eA [list $proc_type]] 1]
    }
    namespace export toolchain
    ::xsdb::setcmdmeta toolchain categories {projects} [subst $help_prefix]
    ::xsdb::setcmdmeta toolchain brief {Set or get toolchain used for building projects.} [subst $help_prefix]
    ::xsdb::setcmdmeta toolchain description [subst {
SYNOPSIS {
    [concat $help_prefix toolchain]
        Return a list of available toolchains and supported processor types.

    [concat $help_prefix toolchain] <processor-type>
        Get the current toolchain for <processor-type>.

    [concat $help_prefix toolchain] <processor-type> <tool-chain>
        Set the <toolchain> for <processor-type>. Any new projects created
        will use the new toolchain during build.
}
RETURNS {
    Depends on the arguments specified.

    <none>
        List of available toolchains and supported processor types.

    <processor-type>
        Current toolchain for processor-type.

    <processor-type> <tool-chain>
        Nothing if the tool-chain is set, or error, if unsupported tool-chain
        is specified.
}
}] [subst $help_prefix]

    proc petalinux-install-path { args } {
	variable plx_install

	if {[string first "Linux" $::tcl_platform(os)] == -1} {
	    error "Petalinux commands are supported only on Linux platform"
	}

	if { [llength $args] == 0 } {
	    if { $plx_install == "" } {
		error "Petalinux installation path not set. Set the \n\
			installation path using \"petalinux-install-path <path>\""
	    }
	    return $plx_install
	} elseif { [llength $args] > 1 } {
	    error "Wrong # of args: should be \"petalinux-install-path \[path\]\""
	}

	set path [lindex $args 0]
	if { ![file isdirectory $path] || ![file exists $path/settings.sh] } {
	    error "$path is not a valid Petalinux installation"
	}
	# Source the script for sanity
	exec >&@stdout /bin/bash -c "source $path/settings.sh"
	set plx_install $path
	return
    }
    namespace export petalinux-install-path
    ::xsdb::setcmdmeta petalinux-install-path categories {petalinux} [subst $help_prefix]
    ::xsdb::setcmdmeta petalinux-install-path brief {Set or get PetaLinux installation path.} [subst $help_prefix]
    ::xsdb::setcmdmeta petalinux-install-path description [subst {
SYNOPSIS {
    [concat $help_prefix petalinux-install-path] <path>
        Set PetaLinux installation path. XSCT uses this installation path to
        run the Petalinux commands.
        The following Petalinux commands are available:
            petalinux-boot
            petalinux-build
            petalinux-config
            petalinux-create
            petalinux-package
            petalinux-util

        Help for these Petalinux commands is available by running
        <petalinux-command> -help, after setting the installation path.
}
RETURNS {
    Installation path, if no arguments are specified.
    Nothing, if a valid installtion path is specified.
    Error string, if path is not a valid installation or Wrong # of args are
    specified.
}
}] [subst $help_prefix]

    proc plx { args } {
	variable plx_install

	if {[string first "Linux" $::tcl_platform(os)] == -1} {
	    error "Petalinux commands are supported only on Linux platform"
	}

	if { $plx_install == "" } {
	    error "Petalinux installation path not set. Set the\n\
		    installation path using \"petalinux-install-path <path>\""
	}
	exec >&@stdout /bin/bash -c "source $plx_install/settings.sh > /dev/null; [list {*}$args]"
    }

    proc petalinux-boot { args } {
	return [plx {*}[linsert $args 0 petalinux-boot]]
    }
    namespace export petalinux-boot

    proc petalinux-build { args } {
	return [plx {*}[linsert $args 0 petalinux-build]]
    }
    namespace export petalinux-build

    proc petalinux-config { args } {
	return [plx {*}[linsert $args 0 petalinux-config]]
    }
    namespace export petalinux-config

    proc petalinux-create { args } {
	return [plx {*}[linsert $args 0 petalinux-create]]
    }
    namespace export petalinux-create

    proc petalinux-package { args } {
	return [plx {*}[linsert $args 0 petalinux-package]]
    }
    namespace export petalinux-package

    proc petalinux-util { args } {
	return [plx {*}[linsert $args 0 petalinux-util]]
    }
    namespace export petalinux-util

    namespace ensemble create -command ::sdk
}

package provide sdk $::sdk::version
