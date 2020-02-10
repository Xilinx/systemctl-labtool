###############################################################################
##
## Copyright (c) 1995-2014 Xilinx, Inc.  All rights reserved.
##
## Xilinx, Inc.
## XILINX IS PROVIDING THIS DESIGN, CODE, OR INFORMATION "AS IS" AS A
## COURTESY TO YOU. BY PROVIDING THIS DESIGN, CODE, OR INFORMATION AS
## ONE POSSIBLE IMPLEMENTATION OF THIS FEATURE, APPLICATION OR
## STANDARD, XILINX IS MAKING NO REPRESENTATION THAT THIS IMPLEMENTATION
## IS FREE FROM ANY CLAIMS OF INFRINGEMENT, AND YOU ARE RESPONSIBLE
## FOR OBTAINING ANY RIGHTS YOU MAY REQUIRE FOR YOUR IMPLEMENTATION.
## XILINX EXPRESSLY DISCLAIMS ANY WARRANTY WHATSOEVER WITH RESPECT TO
## THE ADEQUACY OF THE IMPLEMENTATION, INCLUDING BUT NOT LIMITED TO
## ANY WARRANTIES OR REPRESENTATIONS THAT THIS IMPLEMENTATION IS FREE
## FROM CLAIMS OF INFRINGEMENT, IMPLIED WARRANTIES OF MERCHANTABILITY
## AND FITNESS FOR A PARTICULAR PURPOSE.
##
## jtag_uart_terminal.tcl
##
## JTAG-based Hyperterminal that works over XSDB and DCC/MDM UART
##
###############################################################################

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
