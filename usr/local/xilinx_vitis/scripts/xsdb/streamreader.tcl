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

namespace eval ::streamreader {
    variable instances {}
    variable instance_id 0

    proc get_streamreader_table {name} {
	variable instances

	if { ![dict exists $instances $name] } {
	    puts "internal error: get_streamreader_table called with unknown name: $name"
	    return
	}
	set sock [dict get $instances $name connectionid]
	set sock_bufs [dict get $instances $name sock_bufs]
	dict set instances $name sock_bufs {}
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
	dict set instances $name [dict create name $name ns $ns chan $chan connectionid $sock TXStreamID $TXStreamID eos 0 stream_read_count 0 sock_bufs {}]
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
	dict unset instances $name
	return $err
    }
}
