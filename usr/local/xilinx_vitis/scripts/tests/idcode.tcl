#######################################################################
# Copyright (c) 2013-2021 Xilinx, Inc.  All rights reserved.
# Copyright (c) 2022-2023 Advanced Micro Devices, Inc. All rights reserved.
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
