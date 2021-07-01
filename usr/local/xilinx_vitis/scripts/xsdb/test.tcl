#######################################################################
# Copyright (c) 2013-2018 Xilinx, Inc.  All rights reserved.
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

variable chan [tcf c tcp:]

puts "==== Diagnostics echo"
tcf send_command $chan Diagnostics echo s s hi

tcf send_command $chan Jtag getChildren s eA ""

puts "==== Breakpoints add"
tcf send_command $chan Breakpoints add {o{Enabled b}} e {ID foobar Enabled 1}

puts "==== Breakpoints getProperties with invalid context 1"
set result [tcf send_command $chan Breakpoints getProperties s o{}o{} xyz]
if [expr { ![dict exists [lindex $result 0] Format] ||
	   [dict get [lindex $result 0] Format] ne "Invalid context"}] {
    puts "command failed as expected, but with unexpected result: $result"
}

puts "==== Breakpoints getProperties with invalid context 2"
if [catch {tcf send_command $chan Breakpoints getProperties s eo{} xyz} result] {
    if [expr { ![dict exists [lindex $result end] Format] ||
	       [dict get [lindex $result end] Format] ne "Invalid context"}] {
	puts "command failed as expected, but with unexpected result: $result"
    }
} else {
    puts "command successed unexpectedly"
}

puts "==== Breakpoints getProperties with context"
set bp [tcf send_command $chan Breakpoints getProperties s eo{} foobar]
puts "tcf send returns: $bp"
puts "Breakpoint ID: [dict get [lindex $bp 1] ID]: properties: [lindex $bp 1]"

for {set i 0} {$i < 20} {incr i} {
    variable children [tcf send_command $chan Jtag getChildren s eas {}]
    if {[lindex $children 0] != {}} {
	error "getChildren return error: [lindex $children 0]"
    }

    if {[lindex $children 1] != {}} {
	break
    }

    after 100
}

puts "Children: [lindex $children 1]"

proc send_done args {
    global foo
    global count
    global afterid

    incr count -1
    if [expr { $count == 0 }] {
	puts "count reached 0"
	set foo x1
	after cancel $afterid
    }
}

proc after_done args {
    puts "entering after_done"
    global foo
    set foo x2
}

set count 10000
set afterid [after 5000 after_done]
puts "afterid: $afterid"
puts [time {tcf send_command $chan Diagnostics echo {s} {s} 1.5 send_done} $count]
vwait foo

tcf d $chan
