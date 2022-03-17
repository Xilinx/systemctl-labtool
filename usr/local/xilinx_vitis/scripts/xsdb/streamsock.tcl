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

namespace eval ::streamsock {
    variable update_poll_active 0
    variable update_poll_delay 50000
    variable instances {}
    variable instance_id 0

    proc server_close {name} {
	variable instances

	if { [dict exists $instances $name sockid] } {
	    close [dict get $instances $name sockid]
	    dict unset instances $name sockid
	}
    }

    proc client_close {name} {
	variable instances

	if { [dict exists $instances $name connectionid] } {
	    close [dict get $instances $name connectionid]
	    dict unset instances $name connectionid
	}
    }

    proc set_error {name error} {
	variable instances

	if { [dict exists $instances $name error] } return
	dict set instances $name error $error
	client_close $name
	server_close $name
    }

    proc stream_write_done {name err} {
	variable instances

	if { ![dict exists $instances $name] } {
	    return
	}
	dict with instances $name {incr stream_write_count -1}

	set chan [dict get $instances $name chan]
	if { [string length $err] > 0 } {
	    set_error $name $err
	} elseif { [catch {::tcf::read $chan e} data] } {
	    puts "set_error $name $data"
	} else {
	    sock_reader $name
	}
    }

    proc sock_reader {name} {
	variable instances

	if { ![dict exists $instances $name] } {
	    puts "internal error: sock_reader called with unknown name: $name"
	    return
	}

	set sock [dict get $instances $name connectionid]
	if { [eof $sock] } {
	    client_close $name
	    return
	}

	set buf [read $sock 4096]
	set len [string length $buf]
	if { $len > 0 } {
	    set chan [dict get $instances $name chan]
	    set streamid [dict get $instances $name RXStreamID]
	    ::tcf::send_command $chan Streams write [list ::streamsock::stream_write_done $name]
	    ::tcf::write $chan siB [list $streamid $len $buf]
	    dict with instances $name {incr stream_write_count 1}
	}

	if { [dict get $instances $name stream_write_count] < 4 } {
	    fileevent $sock readable [list ::streamsock::sock_reader $name]
	} else {
	    fileevent $sock readable {}
	}
    }

    proc sock_writer {name} {
	variable instances

	if { ![dict exists $instances $name] } {
	    return
	}

	set sock_bufs [dict get $instances $name sock_bufs]
	if { [llength $sock_bufs] > 0 } {
	    set sock [dict get $instances $name connectionid]
	    if { [llength $sock_bufs] == 1 } {
		# nothing more to write
		fileevent $sock writable {}
	    }
	    set buf [lindex $sock_bufs 0]
	    dict set instances $name sock_bufs [lrange $sock_bufs 1 end]
	    puts -nonewline $sock $buf
	}
    }

    proc stream_read_done {name err} {
	variable instances

	if { ![dict exists $instances $name] } {
	    return
	}
	dict with instances $name {incr stream_read_count -1}

	set chan [dict get $instances $name chan]
	if { [string length $err] > 0 } {
	    set_error $name $err
	} elseif { [catch {::tcf::read $chan Beib} data] } {
	    puts "$name $data"
	} else {
	    foreach {buf err lost eos} $data break
	    if { $eos } {
		dict set instances $name eos 1
	    }
	    if { [string length $buf] > 0 } {
		if { [llength [dict get $instances $name sock_bufs]] == 0 } {
		    set sock [dict get $instances $name connectionid]
		    fileevent $sock writable [list ::streamsock::sock_writer $name]
		}
		dict with instances $name {lappend sock_bufs $buf}
	    }
	    stream_reader $name
	}
    }

    proc stream_reader {name} {
	variable instances

	if { ![dict exists $instances $name] } {
	    puts "internal error: stream_reader called with unknown name: $name"
	    return
	}

	if { [dict get $instances $name eos] } {
	    # end of stream, no need to read anymore
	    return
	}

	if { [dict get $instances $name stream_read_count] < 1 } {
	    set chan [dict get $instances $name chan]
	    set streamid [dict get $instances $name TXStreamID]
	    ::tcf::send_command $chan Streams read [list ::streamsock::stream_read_done $name]
	    ::tcf::write $chan si [list $streamid 4096]
	    dict with instances $name {incr stream_read_count 1}
	}
    }

    proc accept {name sock addr port} {
	variable instances

	if { ![dict exists $instances $name] } {
	    close $sock
	    return
	}

	if { [dict exists $instances $name connectionid] } {
	    # already have a client connection
	    close $sock
	    return
	}

	fconfigure $sock -buffering none -blocking 0 -translation binary
	dict set instances $name connectionid $sock

	# initiate the readers
	sock_reader $name
	stream_reader $name
    }

    proc set_update_poll_delay { delay } {
	 variable update_poll_delay [expr $delay]
    }

    proc get_update_poll_delay {} {
	 variable update_poll_delay
	 expr $update_poll_delay
    }


    proc update_poll {} {
	variable update_poll_active
        variable update_poll_delay
	variable instances

	catch update
	if { [dict size $instances] > 0 } {
	    ::tcf::post_event update_poll $update_poll_delay
	} else {
	    set update_poll_active 0
	}
    }

    proc update_streams { name rx tx } {
	variable instances

	dict set instances $name RXStreamID $rx
	dict set instances $name TXStreamID $tx
	sock_reader $name
	stream_reader $name
    }

    proc create {chan TXStreamID RXStreamID {port 0}} {
	variable update_poll_active
	variable instances
	variable instance_id

	set name "streamsock#$instance_id"
	set sockid [socket -server [list ::streamsock::accept $name] $port]
	set port [lindex [fconfigure $sockid -sockname] 2]
	dict set instances $name [dict create name $name chan $chan sockid $sockid TXStreamID $TXStreamID RXStreamID $RXStreamID eos 0 stream_read_count 0 stream_write_count 0 sock_bufs {}]
	incr instance_id
	if { !$update_poll_active } {
	    update_poll
	}
	return [list $name $port]
    }

    proc delete {name} {
	variable instances

	if { ![dict exists $instances $name] } {
	    error "streamsock \"$name\" is not valid"
	}

	client_close $name
	server_close $name
	dict unset instances $name
    }
}
