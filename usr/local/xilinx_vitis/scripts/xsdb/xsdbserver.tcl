#######################################################################
# Copyright (c) 2014-2018 Xilinx, Inc.  All rights reserved.
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

namespace eval ::xsdb::server {
    variable version 0.1
    variable map {\0 \\000 \n \\n \r \\r \\ \\\\}

    proc quote {s} {
	variable map
	return [string map $map $s]
    }

    proc connect_handler {newchannel clientaddr clientport} {
	variable channel

	puts "new connection $newchannel $clientaddr:$clientport"
	if { [info exists channel] } {
	    close $newchannel
	    return
	}
	if { [catch {
	    fconfigure $newchannel -block 0
	    fileevent $newchannel readable ::xsdb::server::command_handler
	    set channel $newchannel
	} msg] } {
	    puts "server connect error: $msg"
	    close $newchannel
	}
    }

    proc command_handler {} {
	variable channel

	set count [gets $channel line]
	if { $count < 0 } {
	    if { [eof $channel] } {
		close $channel
		unset channel
	    }
	    return
	}

	fconfigure $channel -block 1
	fileevent $channel readable {}

	set code [uplevel 1 [list catch $line _xsdbserver_result _xsdbserver_options]]
	upvar _xsdbserver_result result
	upvar _xsdbserver_options options

	if { ![info exists channel] } return

	if { $code == 0 || $code == 2 } {
	    puts $channel "okay [quote $result]"
	} else {
	    if { $code == 3 } {
		set result {invoked "break" outside of a loop}
	    } elseif { $code == 4 } {
		set result {invoked "continue" outside of a loop}
	    }
	    puts $channel "error [quote $result]"
	}
	flush $channel

	fconfigure $channel -block 0
	fileevent $channel readable ::xsdb::server::command_handler
    }

    proc start {args} {
	variable server

	set options {
	    {host "host name or ip address" {args 1}}
	    {port "port number" {default 0 args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help xsdbserver [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [info exists server] } {
	    error "XSDB server already started"
	}

	set opts {}
	if { [info exists params(host)] } {
	    lappend opts -myaddr $params(host)
	}
	set server [socket -server ::xsdb::server::connect_handler {*}$opts $params(port)]

	puts "Connect to this XSDB server use host [info hostname] and port [lindex [fconfigure $server -sockname] 2]"

	# wait only in non-interactive mode
	# vwait requires global variable
	if { $::tcl_interactive == 0 } {
	    vwait [namespace current]::server
	}
    }
    namespace export start
    ::xsdb::setcmdmeta {xsdbserver start} categories {miscellaneous}
    ::xsdb::setcmdmeta {xsdbserver start} brief {Start XSDB command server.}
    ::xsdb::setcmdmeta {xsdbserver start} description {
SYNOPSIS {
    xsdbserver start [options]
        Start XSDB command server listener.  XSDB command server
        allows external processes to connect to XSDB to evaluate
        commands.  The XSDB server reads commands from the
        connected socket one line at the time.  After evaluation, a
        line is sent back starting with 'okay' or 'error' followed
        by the result or error as a backslash quoted string.
}
OPTIONS {
    -host <addr>
        Limits the network interface on which to listen for incomming
        connections.

    -port <port>
        Specifies port to listen on.  If this option is not specified
        or if the port is zero then a dynamically allocated port
        number is used.
}
RETURNS {
    Server details are disaplayed on the console if server is started.
    successfully, or error string, if a server has been already started.
}
EXAMPLE {
    xsdbserver start
        Start XSDB server listener using dynamically allocated port.

    xsdbserver start -host localhost -port 2000
        Start XSDB server listener using port 2000 and only allow
        incomming connections on this host.
}
}

    proc stop {args} {
	variable server
	variable channel

	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help xsdbserver [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists server] } {
	    error "XSDB server not started"
	}

	if { [info exists channel] } {
	    close $channel
	    unset channel
	}

	close $server
	unset server
    }
    namespace export stop
    ::xsdb::setcmdmeta {xsdbserver stop} categories {miscellaneous}
    ::xsdb::setcmdmeta {xsdbserver stop} brief {Stop XSDB command server.}
    ::xsdb::setcmdmeta {xsdbserver stop} description {
SYNOPSIS {
    xsdbserver stop
        Stop XSDB command server listener and disconnect connected
        client if any.
}
RETURNS {
    Nothing, if the server is closed successfully.
    Error string, if the server has not been started already.
}
}

    proc disconnect {args} {
	variable channel

	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help xsdbserver [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists channel] } {
	    error "XSDB server not connected"
	}

	close $channel
	unset channel
    }
    namespace export disconnect
    ::xsdb::setcmdmeta {xsdbserver disconnect} categories {miscellaneous}
    ::xsdb::setcmdmeta {xsdbserver disconnect} brief {Disconnect active XSDB server connection.}
    ::xsdb::setcmdmeta {xsdbserver disconnect} description {
SYNOPSIS {
    xsdbserver disconnect
        Disconnect current XSDB server connection.
}
RETURNS {
    Nothing, if the connection is closed.
    Error string, if there is no active connection.
}
}

    proc version {args} {
	variable channel
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help xsdbserver [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists channel] } {
	    error "XSDB server not connected"
	}

	return "XSDB Server Protocol Version 0.1"
    }
    namespace export version
    ::xsdb::setcmdmeta {xsdbserver version} categories {miscellaneous}
    ::xsdb::setcmdmeta {xsdbserver version} brief {Return XSDB command server version}
    ::xsdb::setcmdmeta {xsdbserver version} description {
SYNOPSIS {
    xsdbserver version
        Return XSDB command server protocol version.
}
RETURNS {
    Server version if there is an active connection.
    Error string, if there is no active connection.
}
}

    namespace ensemble create -command ::xsdbserver
}

package provide xsdb::server $::xsdb::server::version
