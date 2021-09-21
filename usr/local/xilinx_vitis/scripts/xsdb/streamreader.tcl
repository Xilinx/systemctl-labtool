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

namespace eval ::streamreader {
    variable instances {}
    variable instance_id 0

    proc get_streamreader_table {name} {
	variable instances

	if { ![dict exists $instances $name] } {
	    puts "internal error: get_streamreader_table called with unknown name: $name"
	    return
	}
	if { [dict get $instances $name unset_pending] } {
	    dict unset instances $name
	    return
	}
	set sock [dict get $instances $name connectionid]
	set sock_bufs [dict get $instances $name sock_bufs]
	dict set instances $name sock_bufs {}
	if { ![dict get $instances $name disconnecting] } {
	    # don't clear the pending flag when user calls streamreader_handler
	    dict set instances $name event_pending 0
	}
	stream_reader $name
	return [dict create sock $sock sock_bufs $sock_bufs]
    }

    proc set_disconnecting {name} {
	variable instances

	if { ![dict exists $instances $name] } {
	    puts "internal error: set_disconnecting called with unknown name: $name"
	    return
	}
	dict set instances $name eos 1
	dict set instances $name disconnecting 1
    }

    proc set_error {name error} {
	variable instances

	if { ![dict exists $instances $name] } {
	    puts "internal error: set_error called with unknown name: $name"
	    return
	}
	if { [dict exists $instances $name error] } return
	if { $error == "Command canceled" && [dict get $instances $name eos] } {
	    # Filter out errors from cancelled commands due to
	    # disconnect from stream
	    return
	}
	dict set instances $name error $error
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
	    set_error $name $data
	} elseif { ![dict exists $instances $name error] } {
	    incr num_errors
	    foreach {buf err lost eos} $data break
	    if { $eos } {
		dict set instances $name eos 1
	    }
	    if { [string length $buf] > 0 } {
		dict with instances $name {lappend sock_bufs $buf}
		if { [llength [dict get $instances $name sock_bufs]] == 1 } {
		    # send the buffer to client
		    [dict get $instances $name ns]::eval_event [list ::xsdb::streamreader_handler $name]
		    dict set instances $name event_pending 1
		}
	    }
	    if { [llength [dict get $instances $name sock_bufs]] < 16 } {
		# read from the stream if the buffered data is within limit
		stream_reader $name
	    }
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

	if { [dict exists $instances $name error] } {
	    # stop reading after error
	    return
	}

	if { [dict get $instances $name stream_read_count] < 1 } {
	    set chan [dict get $instances $name chan]
	    set streamid [dict get $instances $name TXStreamID]
	    ::tcf::send_command $chan Streams read [list ::streamreader::stream_read_done $name]
	    ::tcf::write $chan si [list $streamid 4096]
	    dict with instances $name {incr stream_read_count 1}
	}
    }

    proc create {ns chan TXStreamID sock} {
	variable update_poll_active
	variable instances
	variable instance_id

	set name "streamreader#$instance_id"
	dict set instances $name [dict create name $name ns $ns chan $chan connectionid $sock TXStreamID $TXStreamID eos 0 event_pending 0 unset_pending 0 disconnecting 0 stream_read_count 0 sock_bufs {}]
	incr instance_id
	stream_reader $name
	return $name
    }

    proc delete {name} {
	variable instances

	if { ![dict exists $instances $name] } {
	    error "streamreader \"$name\" is not valid"
	}
	set err ""
	if { [dict exists $instances $name error] } {
	    set err [dict get $instances $name error]
	}
	if { [dict get $instances $name event_pending] } {
	    # In non-interactive mode, streamreader_handler posted by stream_read_done can be
	    # in event queue, and executed after the user script is run. By that time, the
	    # streamreader::delete can be called by the user script. Do not delete the instance
	    # if the handler is pending and eos is set. delete it when the handler is called
	    dict set instances $name unset_pending 1
	} else {
	    dict unset instances $name
	}
	return $err
    }
}
