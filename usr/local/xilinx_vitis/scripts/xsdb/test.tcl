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
