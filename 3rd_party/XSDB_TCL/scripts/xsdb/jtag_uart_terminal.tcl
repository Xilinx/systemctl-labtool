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

set xsdb_socket stdout

# Read Characters from STDIN and send them over Sockets.
proc stdin_reader { chan } {
    global xsdb_socket
    set str [gets $chan]
    if { $str == "terminal_exit" } {
	puts "CMD: terminal_exit. Closing JTAG-based Hyperterminal"
	exit
    }

    if { $str == "\n" } {
	return
    }

    puts $xsdb_socket $str
}

# Print DCC/MDM UART output from sockets to StdOut
proc stdout_writer { chan } {
    if { [eof $chan] } {
	exit
    }
    set str [read $chan 1000]
    if { $str == "xsdb_exit" } {
	close $chan
	exit
    }
    puts -nonewline $str
    flush stdout
}


proc bgerror { msg } {
    puts $msg
# Following needed 'cos usually bgerror removes any STDIN channel handlers
    fileevent stdin readable "stdin_reader stdin"
}

#
# Connect to Terminal Server and read/write from/to STDIN/STDOUT
#
if { $argc != 1 } {
    puts "USAGE: tclsh jtag_uart_terminal.tcl <TCP Portno>"
    exit
}

set terminal_portno [lindex $argv 0]
if { [catch {set terminal_socket [socket localhost $terminal_portno]} err ] } {
    puts "Waiting for terminal to initialize..."
    after 10000
    set terminal_socket [socket localhost $terminal_portno]
}
set xsdb_socket $terminal_socket

fconfigure $terminal_socket -buffering none -blocking 0 -translation auto
puts "JTAG-based Hyperterminal.
Connected to JTAG-based Hyperterminal over TCP port : $terminal_portno
(using socket : $terminal_socket)
Help :
Terminal requirements :
  (i) Processor's STDOUT is redirected to the ARM DCC/MDM UART
  (ii) Processor's STDIN is redirected to the ARM DCC/MDM UART.
       Then, text input from this console will be sent to DCC/MDM's UART port.
  NOTE: This is a line-buffered console and you have to press \"Enter\"
        to send a string of characters to DCC/MDM.
"
fileevent stdin readable "stdin_reader stdin"
fileevent $terminal_socket readable "stdout_writer $terminal_socket"

vwait forever
