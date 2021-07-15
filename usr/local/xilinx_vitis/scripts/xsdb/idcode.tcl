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

set nchains 0
while {$nchains == 0} {
    set scanchains [jtag_get_scan_chains $chan]
    set nchains [llength $scanchains]
}
puts "scan chains: $scanchains"

set scanchain [lindex $scanchains 0] 
puts "first scan chain id: $scanchain"

set devices [jtag_get_child_nodes $chan $scanchain]
puts "device: $devices"

set ndev [llength $devices]
for {set i 0} {$i < $ndev} {incr i} {
    set device [lindex $devices $i]
    puts "device$i id: $device"

    set context [jtag_get_node_properties $chan $device]
    puts "device$i properties: $context"
}

jtag_lock $chan $scanchain

set seq [jtag_sequence_create]
jtag_sequence_state seq RESET 5
jtag_sequence_progress seq 1
jtag_sequence_shift seq 0 1 1024 {state IDLE value 1}
jtag_sequence_progress seq 2
set result [jtag_sequence_run seq $chan $scanchain]

jtag_unlock $chan $scanchain

binary scan $result i* idcodes
puts "idcodes: $idcodes"

binary scan $result H* hexdata
puts "hexdata: $hexdata"
