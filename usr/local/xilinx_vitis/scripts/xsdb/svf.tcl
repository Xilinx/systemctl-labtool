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

package require xsdb::elf

namespace eval ::xsdb::svf {
    variable version 0.1
    variable daps {}
    variable commands {getapinfo abort dpread dpwrite apread apwrite memread memwrite run clear delete reset}
    variable outfile ""
    variable scan_chain
    variable device_index 0
    variable delaytcks 0
    variable linkdap
    variable dbgbase
    variable ctibase
    variable cmd_dict [dict create a64 [dict create open zynqmp_open mwr arm_mwr dow arm_dow stop a64_stop con a64_con setpc a64_setpc] \
				   a32 [dict create open zynqmp_open mwr arm_mwr dow arm_dow stop a32_stop con a32_con setpc a32_setpc] \
				   armv7 [dict create open zynq_open mwr arm_mwr dow arm_dow stop armv7_stop con armv7_con setpc armv7_setpc] \
				   mb [dict create open mb_open mwr mb_mwr dow mb_dow stop mb_stop con mb_con setpc mb_setpc]]
    variable config_dict [dict create]
    variable bscan_dict [dict create user1 0x2 user2 0x3 user3 0x22 user4 0x23 pmu 0x0e4]
    ::xsdb::setcmdmeta svf brief {SVF Operations}

    # AP indices
    variable AHB 		0
    variable APB 		1
    variable JTAG 		2

    # Base address for DBG & CTI registers
    variable A53_DBGBASE 	0x80410000
    variable A53_CTIBASE 	0x80420000
    variable A9_DBGBASE 	0x80090000
    variable R5_DBGBASE 	0x803f0000

    # Offsets of DBG registers
    variable DBGDTRRX 		0x80
    variable DBGDTRTX 		0x8c
    variable DBGITR 		0x84
    variable DBGDRCR 		0x90
    variable DBGDSCR 		0x88
    variable DBGOSLAR 		0x300
    variable DBGVCR 		0x1c

    # Offsets of CTI registers
    variable CTICTRL		0x0
    variable CTIINTACK		0x10
    variable CTIAPPSET		0x14
    variable CTIAPPCLR		0x18
    variable CTIAPPPULSE	0x1c
    variable CTIOUTEN0		0xa0
    variable CTIOUTEN1		0xa4
    variable CTIGATE		0x140
    variable CTILAR		0xfb0

    # Instructions
    variable ARMV8_MRS_DBG_DTR_X0 	0xd5330400
    variable ARMV8_MSR_X0_TO_DLR 	0xd51b4520
    variable ARMV8_INSTR_IC_IALLU	0xd508751f
    variable ARMV8_INSTR_DSB_SYS	0xd5033f9f
    variable ARMV7_MOVE_CP14_C5_TO_GPR 	0xee100e15
    variable ARMV7_MOVE_GPR_TO_PC 	0xe1a0f000
    variable ARMV7_MCR_CP15_R0_ICIALLU	0xee070f15
    variable ARMV7_DATA_SYNC_BARRIER	0xee070f9a
    variable ARMV7_INSTR_SYNC_BARRIER	0xee070f95
    variable T32_MOVE_CP14_C5_TO_GPR 	0x0e15ee10
    variable T32_MOVE_GPR_TO_PC 	0x0f35ee64
    variable T32_MCR_CP15_R0_ICIALLU	0x0f15ee07
    variable T32_MCR_CP15_R0_BPIALL	0x0fd5ee07
    variable T32_INSTR_DSB_SYS		0x8f4ff3bf

    #---------------------------------------------------------------------------------------#
    # config
    # Configures the SVF utility
    #---------------------------------------------------------------------------------------#
    proc config { args } {
	variable A53_DBGBASE
	variable A53_CTIBASE
	variable A9_DBGBASE
	variable R5_DBGBASE
	variable arch
	variable cpu_type
	variable dbgbase
	variable ctibase
	variable cmd_dict
	variable config_dict
	variable bscan_dict

	set options {
	    {help "command help"}
	    {device-index "scan chain device index" {default 0 args 1}}
	    {cpu-index "cpu index on the device" {default 0 args 1}}
	    {out "output file" {default "out.svf" args 1}}
	    {delay "delay between apwrites" {default 0 args 1}}
	    {scan-chain "scan chain info" {args 1}}
	    {bscan "bscan user register" {default user2 args 1}}
	    {mb-chunksize "download chunk size for microblaze" {default 1024 args 1}}
	    {exec-mode "execution mode of ARM v8 cores" {default a64 args 1}}
	    {linkdap "dap link"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help svf [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { ![info exists params(scan-chain)] } {
	    error "scan chain not specified"
	}
	if { ![dict exists $bscan_dict $params(bscan)] } {
	    error "unknown bscan option - $params(bscan)"
	}
	set outfile [::open $params(out) "w"]
	set config_dict [dict create scan_chain $params(scan-chain) \
				     cpu_index $params(cpu-index) \
				     device_index $params(device-index) \
				     delaytcks $params(delay) \
				     bscan [dict get $bscan_dict $params(bscan)] \
				     outfile $outfile \
				     chunk_size $params(mb-chunksize)]

	if { [info exists params(linkdap)] } {
	    dict set config_dict linkdap $params(linkdap)
	}
	# Identify device
	set id_code [lindex $params(scan-chain) [expr ($params(device-index) * 2)]]
	::xsdb::checkint $id_code
	switch -- $id_code {
	    0x5ba00477 {
		if { $params(cpu-index) <= 3 } {
		    if { $params(exec-mode) == "a64" } {
			set arch "a64"
		    } else {
			set arch "a32"
		    }
		    set cpu_type "a53"
		    set dbgbase [expr $A53_DBGBASE + $params(cpu-index) * 0x100000]
		    set ctibase [expr $A53_CTIBASE + $params(cpu-index) * 0x100000]
		} elseif { $params(cpu-index) <= 5 } {
		    set arch "armv7"
		    set cpu_type "r5"
		    set dbgbase [expr $R5_DBGBASE + ( $params(cpu-index) - 4 ) * 0x2000]
		} else {
		    error "cpu-index should be between 0 to 5"
		}
	    }
	    0x4ba00477  {
		if { $params(cpu-index) > 1 } {
		    error "cpu-index should be 0 or 1"
		}
		set arch "armv7"
		set cpu_type "a9"
		set dbgbase [expr $A9_DBGBASE+ ($params(cpu-index) * 0x2000)]
	    }
	    default {
		set arch "mb"
		set cpu_type "mb"
	    }
	}

	writesvf "// Generating SVF for $arch "
	set open_cmd [dict get $cmd_dict $arch open]
	if { $cpu_type == "r5" } {
	    set open_cmd zynqmp_open
	}
	eval {$open_cmd}
    }
    namespace export config
    ::xsdb::setcmdmeta {svf config} categories {svf}
    ::xsdb::setcmdmeta {svf config} brief {Configure options for SVF file.}
    ::xsdb::setcmdmeta {svf config} description {
SYNOPSIS {
    svf config [options]
	Configure and generate SVF file.
}
OPTIONS {
    -scan-chain <list of idcode-irlength pairs>
	List of idcode-irlength pairs.
	This can be obtained from xsdb command - jtag targets

    -device-index <index>
	This is used to select device in the jtag scan chain.

    -cpu-index <processor core>
	Specify the cpu-index to generate the SVF file.
	For A53#0 - A53#3 on ZynqMP, use cpu-index 0 -3
	For R5#0 - R5#1 on ZynqMP, use cpu-index 4 -5
	For A9#0 - A9#1 on Zynq, use cpu-index 0 -1
	If multiple MicroBlaze processors are connected to MDM,
	select the specific MicroBlaze index for execution.

    -out <filename>
	Output SVF file.

    -delay <tcks>
	Delay in ticks between AP writes.

    -linkdap
	Generate SVF for linking DAP to the jtag chain for ZynqMP
	Silicon versions 2.0 and above.

    -bscan <user port>
	This is used to specify user bscan port to which MDM is connected.

    -mb-chunksize <size in bytes>
	This option is used to specify the chunk size in bytes for each
	transaction while downloading.
	Supported only for MicroBlaze processors.

    -exec-mode
        Execution mode for Arm v8 cores. Supported modes are
        a32 (v8 core is set up in 32-bit mode)
        and a64 (v8 core is set up in 64-bit mode).
}
RETURNS {
    Nothing.
}
EXAMPLE {
    svf config -scan-chain {0x14738093 12 0x5ba00477 4} -device-index 1 \
	       -cpu-index 0 -out "test.svf"
	This creates a SVF file with name test.svf for core A53#0

    svf config -scan-chain {0x14738093 12 0x5ba00477 4} -device-index 0 \
	      -bscan pmu -cpu-index 0 -out "test.svf"
	This creates a SVF file with name test.svf for PMU MB

    svf config -scan-chain {0x23651093 6} -device-index 0 -cpu-index 0 \
	      -bscan user1 -out "test.svf"
	This creates a SVF file with name test.svf for MB connected to MDM
	on bscan USER1
}
}

    #---------------------------------------------------------------------------------------#
    # generate
    # Generates the SVF file
    #---------------------------------------------------------------------------------------#
    proc generate { args } {
	variable config_dict
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help svf [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set outfile [dict get $config_dict outfile]
	if { $outfile == "" } {
	    error "svf not configured"
	}
	close $outfile
    }
    namespace export generate
    ::xsdb::setcmdmeta {svf generate} categories {svf}
    ::xsdb::setcmdmeta {svf generate} brief {Generate recorded SVF file.}
    ::xsdb::setcmdmeta {svf generate} description {
SYNOPSIS {
    svf generate
	Generate SVF file in the path specified in the config command.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    svf generate
}
}

    #---------------------------------------------------------------------------------------#
    # mwr
    # Write to memory
    #---------------------------------------------------------------------------------------#
    proc mwr { args } {
	variable arch
	variable cmd_dict
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help svf [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] == 2 } {
	    set addr [lindex $args 0]
	    set value [lindex $args 1]
	} else {
	    error "wrong # args: should be \"svf mwr <address> <value>\""
	}
	::xsdb::checkint $addr
	::xsdb::checkint $value
	writesvf "// write to mem - value-$value @ addr-$addr"
	eval [dict get $cmd_dict $arch mwr] $addr $value
   }
    namespace export mwr
    ::xsdb::setcmdmeta {svf mwr} categories {svf}
    ::xsdb::setcmdmeta {svf mwr} brief {Record memory write to SVF file.}
    ::xsdb::setcmdmeta {svf mwr} description {
SYNOPSIS {
    svf mwr <address> <value>
	Write <value> to the memory address specified by <address>.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    svf mwr 0xffff0000 0x14000000
}
}

    #This can be removed and add the above function as part of zynq dictionary
    #---------------------------------------------------------------------------------------#
    # arm memory write
    #---------------------------------------------------------------------------------------#
    proc arm_mwr { addr value } {
	variable AHB
	variable config_dict

	set device_index [dict get $config_dict device_index]

	set dapname "dap$device_index"
	$dapname memwrite $AHB $addr $value
	$dapname run

	return
    }

    #---------------------------------------------------------------------------------------#
    # MB memory write
    #---------------------------------------------------------------------------------------#
    proc mb_mwr { addr data } {
	# sequence of instructions to write words to memory:
	# "imm HIDATA"
	# "ori r11,r0,LODATA"
	# "imm HIADDR"
	# "swi r11,r0,LOADDR"
	set bitlen 136
	set bitvals {}

	set    hexval "0x96" ; # MDM sync
	append hexval "b000" ; # imm
	append hexval [format {%04x} [expr ($data >> 16) & 0xFFFF]]
	append hexval "a160" ; # ori r11, r0
	append hexval [format {%04x} [expr $data & 0xFFFF]]
	append hexval "b000" ; # imm
	append hexval [format {%04x} [expr ($addr >> 16) & 0xFFFF]]
	append hexval "f960" ; # swi r11, r0
	append hexval [format {%04x} [expr $addr & 0xFFFF]]
	set revbits [string reverse [to_bits $hexval $bitlen]]
	set bitvals [to_hex $revbits $bitlen]

	set bitvals [string map {"0x" ""} $bitvals]

	# Instruction insert command, followed by instruction
	writesvf "SDR 8 TDI (04);"
	writesvf "SDR 136 TDI ($bitvals);"

	return
    }

    #---------------------------------------------------------------------------------------#
    # dow
    # Download elf file to memory
    #---------------------------------------------------------------------------------------#
    proc dow { args } {
	variable arch
	variable cmd_dict
	set options {
	    {data "download binary file"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help svf [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] == 0 || [llength $args] > 2 } {
	    error "wrong # args: should be \"svf dow <file> <address>\""
	}
	set file [lindex $args 0]
	set addr 0
	if { [llength $args] > 1 } {
	    if { !$params(data) } {
		error "address is only applicable with -data option"
	    }
	    set addr [lindex $args 1]
	}

	writesvf "// download file"

	set f ""
	if { !$params(data) } {
	    # Open ELF file
	    set f [::xsdb::elf::open $file]
	    set hdr [$f get_hdr]
	    set entryaddr [dict get $hdr entry]
	    dict set arg f $f
	} else {
	    # Open data file
	    set f [::open $file rb]
	    dict set arg f $f
	}

	eval [dict get $cmd_dict $arch dow] $f $params(data) $addr

	if { !$params(data) } {
	    eval [dict get $cmd_dict $arch setpc] $entryaddr
	}
	if { !$params(data) } {
	    $f close
	} else {
	    close $f
	}
    }
    namespace export dow
    ::xsdb::setcmdmeta {svf dow} categories {svf}
    ::xsdb::setcmdmeta {svf dow} brief {Record elf download to SVF file.}
    ::xsdb::setcmdmeta {svf dow} description {
SYNOPSIS {
    svf dow <elf file>
	Record downloading of elf file <elf file> to the memory.

    svf dow -data <file> <addr>
	Record downloading of binary file <file> to the memory.
}
OPTIONS {
    None.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    svf dow "fsbl.elf"
	Record downloading of elf file fsbl.elf.

    svf dow -data "data.bin" 0x1000
	Record downloading of binary file data.bin to the address 0x1000.
}
}

    #---------------------------------------------------------------------------------------#
    # arm Download
    #---------------------------------------------------------------------------------------#
    proc arm_dow { args } {
	variable config_dict
	variable daps

	set f [lindex $args 0]
	set is_data_file [lindex $args 1]
	set bin_addr [lindex $args 2]

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"
	dict with daps $dapname {
	    lappend cmds [list DOWNLOAD $f $is_data_file $bin_addr]
	}
	$dapname run
	delay 100
	return
    }

    #---------------------------------------------------------------------------------------#
    # MB Download
    #---------------------------------------------------------------------------------------#
    proc mb_dow { args } {
	variable config_dict
	set chunk_size [dict get $config_dict chunk_size]
	set f [lindex $args 0]
	set is_data_file [lindex $args 1]
	set bin_addr [lindex $args 2]
	set offset 0

	if { $is_data_file } {
	    seek $f 0 end
	    set total_bytes [tell $f]
	    seek $f 0
	    set addr $bin_addr
	    set dowdata [dict create fileptr $f is_data $is_data_file total_bytes $total_bytes addr $addr offset $offset chunk_size $chunk_size]
	    mb_dow_mem $dowdata
	} else {
	    set total_bytes 0
	    foreach ph [$f get_phlist] {
		dict with ph {
		    set total_bytes 0
		    if { $type == 1 } {
			if { $filesz > 0 } {
			    set total_bytes $filesz
			    set addr [dict get $ph paddr]
			    set offset [dict get $ph offset]
			}
		    }
		    set dowdata [dict create fileptr $f is_data $is_data_file total_bytes $total_bytes addr $addr offset $offset chunk_size $chunk_size]
		    mb_dow_mem $dowdata
		}
	    }
	}

	return
    }

    proc mb_dow_mem { dowdata } {
	set f [dict get $dowdata fileptr]
	set total_bytes [dict get $dowdata total_bytes]
	set addr [dict get $dowdata addr]
	set is_data [dict get $dowdata is_data]
	set chunk_size [dict get $dowdata chunk_size]
	set offset [dict get $dowdata offset]

	set tempaddr $addr
	if { $total_bytes > 0 } {
	    for {set i 0} {$i < $total_bytes} {incr i $chunk_size} {
		set remaining_bytes [expr $total_bytes - $i]
		if { $remaining_bytes < $chunk_size } {
		    set chunk_size $remaining_bytes
		}
		if { $is_data } {
		    set data [read $f $chunk_size]
		} else {
		    set data [$f read_at $offset $chunk_size]
		}
		binary scan $data iu* data

		set hexval "0x96" ; # MDM sync
		set bitlen 8
		set bitvals {}

		for {set bytecnt 0} {$bytecnt < [expr $chunk_size/4]} {incr bytecnt} {
		    append hexval "b000" ; # imm
		    append hexval [format {%04x} [expr ([lindex $data $bytecnt] >> 16) & 0xFFFF]]
		    append hexval "a160" ; # ori r11, r0
		    append hexval [format {%04x} [expr [lindex $data $bytecnt] & 0xFFFF]]
		    append hexval "b000" ; # imm
		    append hexval [format {%04x} [expr ($tempaddr >> 16) & 0xFFFF]]
		    append hexval "f960" ; # swi r11, r0
		    append hexval [format {%04x} [expr $tempaddr & 0xFFFF]]

		    incr bitlen 128
		    incr offset 4
		    incr tempaddr 4
		}

		# Instruction insert command, followed by instruction
		set revbits [string reverse [to_bits $hexval $bitlen]]
		set bitvals [to_hex $revbits $bitlen]

		set bitvals [string map {"0x" ""} $bitvals]

		writesvf "SDR 8 TDI (04);"
		writesvf "SDR $bitlen TDI ($bitvals);"
	    }
	}
    }

    #---------------------------------------------------------------------------------------#
    # stop
    # Stops the core
    #---------------------------------------------------------------------------------------#
    proc stop { args } {
	variable arch
	variable cmd_dict
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help svf [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	writesvf "// stop"
	eval [dict get $cmd_dict $arch stop]
    }
    namespace export stop
    ::xsdb::setcmdmeta {svf stop} categories {svf}
    ::xsdb::setcmdmeta {svf stop} brief {Record stopping of core to SVF file.}
    ::xsdb::setcmdmeta {svf stop} description {
SYNOPSIS {
    svf stop
	Record suspending execution of current target to SVF file.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing.
}
EXAMPLE {
    svf stop
}
}

    #---------------------------------------------------------------------------------------#
    # armv8 stop
    #---------------------------------------------------------------------------------------#
    proc armv8_stop {} {
	variable config_dict
	variable dbgbase
	variable ctibase
	variable DBGDRCR
	variable DBGDSCR
	variable DBGOSLAR
	variable DBGITR
	variable CTICTRL
	variable CTIINTACK
	variable CTIAPPSET
	variable CTIAPPCLR
	variable CTIAPPPULSE
	variable CTIOUTEN0
	variable CTIOUTEN1
	variable CTIGATE
	variable CTILAR
	variable APB

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"

	# Unlock CTI access by writing to OS Lock Access Register
	$dapname memwrite $APB [expr $dbgbase + $DBGOSLAR] 0

	# Set up debug state
	$dapname memwrite $APB [expr $dbgbase + $DBGDSCR] 0x2007C02

	# Unlock CTI registers
	$dapname memwrite $APB [expr $ctibase + $CTILAR] 0xc5acce55

	# Close CTI gate
	$dapname memwrite $APB [expr $ctibase + $CTIGATE] 0x00000000

	# Enable CTI
	$dapname memwrite $APB [expr $ctibase + $CTICTRL] 0x00000001

	# Disable chan#0 -> TrigOut#1
	$dapname memwrite $APB [expr $ctibase + $CTIOUTEN1] 0x00000000

	# Ack TrigOut#1
	$dapname memwrite $APB [expr $ctibase + $CTIINTACK] 0x00000002

	# Clear chan#0
	$dapname memwrite $APB [expr $ctibase + $CTIAPPCLR] 0x00000001

	# Enable chan#0 -> TrigOut#0
	$dapname memwrite $APB [expr $ctibase + $CTIOUTEN0] 0x00000001

	# Set active chan#0
	$dapname memwrite $APB [expr $ctibase + $CTIAPPSET] 0x00000001

	# Clear chan#0
	$dapname memwrite $APB [expr $ctibase + $CTIAPPCLR] 0x00000001
	$dapname run

	delay 100

	# Check if stopped, check bit 0 & overrun errors of DSCR & ACK of the transaction
	_cmd_memread $dapname $APB [expr $dbgbase + $DBGDSCR] 0x800000a 0xe800000a
	$dapname run
    }

    proc a32_stop {} {
	variable config_dict
	variable dbgbase
	variable DBGITR
	variable APB

	armv8_stop

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"

	# Invalidate cache
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] T32_MCR_CP15_R0_ICIALLU
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] T32_MCR_CP15_R0_BPIALL
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] T32_INSTR_DSB_SYS
	$dapname run

	return
    }

    #---------------------------------------------------------------------------------------#
    # a64 stop
    #---------------------------------------------------------------------------------------#
    proc a64_stop {} {
	variable config_dict
	variable dbgbase
	variable DBGITR
	variable APB

	armv8_stop

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"

	# Invalidate cache
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] ARMV8_INSTR_IC_IALLU
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] ARMV8_INSTR_DSB_SYS
	$dapname run

	return
    }

    #---------------------------------------------------------------------------------------#
    # MB stop
    #---------------------------------------------------------------------------------------#
    proc mb_stop {} {
	# Control Register - Dbg WakeUp, Normal Stop, Disable interrupts & ARM
	writesvf "SDR 8 TDI (01);"
	writesvf "SDR 10 TDI (0249);"

	# Try to reset MB_CONTROL_STOP, then set it again
	writesvf "SDR 8 TDI (01);"
	writesvf "SDR 10 TDI (0040);"

	writesvf "SDR 8 TDI (01);"
	writesvf "SDR 10 TDI (0048);"

	return
    }

    #---------------------------------------------------------------------------------------#
    # armv7 stop
    #---------------------------------------------------------------------------------------#
    proc armv7_stop {} {
	variable dbgbase
	variable DBGDRCR
	variable DBGDSCR
	variable DBGITR
	variable DBGOSLAR
	variable config_dict
	variable APB

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"

	$dapname memwrite $APB [expr $dbgbase + $DBGDRCR] 0x1

	#Enable the instruction execution bit
	$dapname memwrite $APB [expr $dbgbase + $DBGDSCR] 0x3186003

	#Invalidate cache
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] ARMV7_MCR_CP15_R0_ICIALLU
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] ARMV7_DATA_SYNC_BARRIER
	$dapname run

	delay 100
	#Check if the core has stopped
	_cmd_memread $dapname $APB [expr $dbgbase + $DBGDSCR] 0xa 0xa;
	$dapname run

	return
    }

    #---------------------------------------------------------------------------------------#
    # con
    # Executes the instructions from PC in the core
    #---------------------------------------------------------------------------------------#
    proc con { args } {
	variable arch
	variable cmd_dict
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]

	if { $params(help) } {
	    return [help svf [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	writesvf "// continue"

	eval [dict get $cmd_dict $arch con]
    }
    namespace export con
    ::xsdb::setcmdmeta {svf con} categories {svf}
    ::xsdb::setcmdmeta {svf con} brief {Record resuming of core to SVF file.}
    ::xsdb::setcmdmeta {svf con} description {
SYNOPSIS {
    svf con
	Record resuming the execution of active target to SVF file.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing.
}
EXAMPLE {
    svf con
}
}

    #---------------------------------------------------------------------------------------#
    # armv8 continue
    #---------------------------------------------------------------------------------------#
    proc armv8_con {} {
	variable config_dict
	variable dbgbase
	variable ctibase
	variable DBGDRCR
	variable DBGITR
	variable DBGDSCR
	variable CTICTRL
	variable CTIINTACK
	variable CTIAPPCLR
	variable CTIAPPPULSE
	variable CTIOUTEN0
	variable CTIOUTEN1
	variable CTIGATE
	variable CTILAR
	variable APB

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"

	# Unlock CTI registers
	$dapname memwrite $APB [expr $ctibase + $CTILAR] 0xc5acce55
	# Clear sticky bits (RCR)
	$dapname memwrite $APB [expr $dbgbase + $DBGDRCR] [expr 1 << 3]
	# Close CTI gate
	$dapname memwrite $APB [expr $ctibase + $CTIGATE] 0x00000000
	# Enable CTI
	$dapname memwrite $APB [expr $ctibase + $CTICTRL] 0x00000001
	# Disable chan#0 -> TrigOut#0
	$dapname memwrite $APB [expr $ctibase + $CTIOUTEN0] 0x00000000
	# Clear chan#0
	$dapname memwrite $APB [expr $ctibase + $CTIAPPCLR] 0x00000001
	# Ack TrigOut#0
	$dapname memwrite $APB [expr $ctibase + $CTIINTACK] 0x00000001
	# Enable chan#0 -> TrigOut#1
	$dapname memwrite $APB [expr $ctibase + $CTIOUTEN1] 0x00000001
	# Pulse chan#0
	$dapname memwrite $APB [expr $ctibase + $CTIAPPPULSE] 0x00000001

	delay 100
	# Check if running, check bit 0 & overrun errors of DSCR & ACK of the transaction
	_cmd_memread $dapname $APB [expr $dbgbase + $DBGDSCR] 0x2 0xe800000a
	$dapname run

	return
    }

    proc a32_con {} {
	variable config_dict
	variable dbgbase
	variable DBGITR
	variable DBGDSCR
	variable DBGOSLAR
	variable APB

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"
	$dapname memwrite $APB [expr $dbgbase + $DBGOSLAR] 0
	$dapname memwrite $APB [expr $dbgbase + $DBGDSCR]  0x3006327

	# Invalidate cache
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] T32_MCR_CP15_R0_ICIALLU
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] T32_MCR_CP15_R0_BPIALL
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] T32_INSTR_DSB_SYS
	$dapname run

	armv8_con
	return
    }

    proc a64_con {} {
	variable config_dict
	variable dbgbase
	variable DBGITR
	variable DBGDSCR
	variable DBGOSLAR
	variable APB

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"
	$dapname memwrite $APB [expr $dbgbase + $DBGOSLAR] 0
	$dapname memwrite $APB [expr $dbgbase + $DBGDSCR]  0x3006327

	# Invalidate cache
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] ARMV8_INSTR_IC_IALLU
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] ARMV8_INSTR_DSB_SYS
	$dapname run

	armv8_con
	return
    }

    #---------------------------------------------------------------------------------------#
    # MB continue
    #---------------------------------------------------------------------------------------#
    proc mb_con {} {
	writesvf "SDR 8 TDI (01);"
	writesvf "SDR 10 TDI (0105);"

	return
    }

    #---------------------------------------------------------------------------------------#
    # armv7 continue
    #---------------------------------------------------------------------------------------#
    proc armv7_con {} {
	variable config_dict
	variable dbgbase
	variable DBGDRCR
	variable DBGDSCR
	variable DBGITR
	variable APB

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"

	# Invalidate cache
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] ARMV7_MCR_CP15_R0_ICIALLU;
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] ARMV7_INSTR_SYNC_BARRIER; $dapname run
	delay 100

	# Check if the instruction complete bit is set
	_cmd_memread $dapname $APB [expr $dbgbase + $DBGDSCR] 0x10000000 0x10000000; $dapname run

	$dapname memwrite $APB [expr $dbgbase + $DBGDRCR] 0x8;

	$dapname memwrite $APB [expr $dbgbase + $DBGDSCR] 0x03184003;

	$dapname memwrite $APB [expr $dbgbase + $DBGDRCR] 0x2;
	$dapname run

	return
    }

    #---------------------------------------------------------------------------------------#
    # a32_setpc
    # Set the program counter to the start address of the elf
    #---------------------------------------------------------------------------------------#
    proc a32_setpc { value } {
	variable config_dict
	variable dbgbase
	variable DBGDTRRX
	variable APB
	variable DBGITR
	variable DBGDSCR
	variable T32_MOVE_CP14_C5_TO_GPR
	variable T32_MOVE_GPR_TO_PC

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"

	writesvf "// setting pc to 0x[format %lx $value]"

	$dapname memwrite $APB [expr $dbgbase + $DBGDTRRX] $value

	# Select DBGITR (offset - 0x84) and write the instruction INSTR_MOVE_CP14_C5_TO_GPR
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] $T32_MOVE_CP14_C5_TO_GPR
	# Select DBGITR (offset - 0x84) and write the instruction INSTR_MOVE_GPR_TO_PC
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] $T32_MOVE_GPR_TO_PC
	$dapname run

	return
    }

    #---------------------------------------------------------------------------------------#
    # a64_setpc
    # Set the program counter to the start address of the elf
    #---------------------------------------------------------------------------------------#
    proc a64_setpc { value } {
	variable config_dict
	variable dbgbase
	variable DBGDTRRX
	variable DBGDTRTX
	variable DBGOSLAR
	variable DBGDSCR
	variable DBGITR
	variable ARMV8_MRS_DBG_DTR_X0
	variable ARMV8_MSR_X0_TO_DLR
	variable APB

	set device_index [dict get $config_dict device_index]
	writesvf "// setting pc to 0x[format %lx $value]"

	set dapname "dap$device_index"
	$dapname memwrite $APB [expr $dbgbase + $DBGOSLAR] 0
	$dapname memwrite $APB [expr $dbgbase + $DBGDSCR] 0x3006327

	# Select DBGDTRRX (offset - 0x80) and write the value of PC
	$dapname memwrite $APB [expr $dbgbase + $DBGDTRTX] [expr [expr $value >> 32] & 0xffffffff]
	$dapname memwrite $APB [expr $dbgbase + $DBGDTRRX] [expr $value & 0xffffffff]

	# Select DBGITR (offset - 0x84) and write the instruction INSTR_MOVE_CP14_C5_TO_GPR
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] $ARMV8_MRS_DBG_DTR_X0
	# Select DBGITR (offset - 0x84) and write the instruction INSTR_MOVE_GPR_TO_PC
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] $ARMV8_MSR_X0_TO_DLR
	$dapname run

	return
    }

    #---------------------------------------------------------------------------------------#
    # armv7_setpc
    # Set the program counter to the start address of the elf
    #---------------------------------------------------------------------------------------#
    proc armv7_setpc { value } {
	variable config_dict
	variable dbgbase
	variable DBGDTRRX
	variable APB
	variable DBGITR
	variable DBGDSCR
	variable ARMV7_MOVE_CP14_C5_TO_GPR
	variable ARMV7_MOVE_GPR_TO_PC

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"

	writesvf "// setting pc to 0x[format %lx $value]"

	$dapname memwrite $APB [expr $dbgbase + $DBGDTRRX] $value

	# Select DBGITR (offset - 0x84) and write the instruction INSTR_MOVE_CP14_C5_TO_GPR
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] $ARMV7_MOVE_CP14_C5_TO_GPR
	# Select DBGITR (offset - 0x84) and write the instruction INSTR_MOVE_GPR_TO_PC
	$dapname memwrite $APB [expr $dbgbase + $DBGITR] $ARMV7_MOVE_GPR_TO_PC
	$dapname run

	return
    }

    #---------------------------------------------------------------------------------------#
    # mb_setpc
    # Set the program counter to the start address of the elf
    #---------------------------------------------------------------------------------------#
    proc mb_setpc { value } {
	writesvf "// setting pc to 0x[format %lx $value]"
	set bitlen 72
	set    hexval "0x96" ; # MDM sync
	append hexval "b000" ; # imm
	append hexval [format {%04x} [expr ($value >> 16) & 0xFFFF]]
	append hexval "b808" ; # imm
	append hexval [format {%04x} [expr $value & 0xFFFF]]
	set revbits [string reverse [to_bits $hexval $bitlen]]
	set bitvals [to_hex $revbits $bitlen]
	set bitvals [string map {"0x" ""} $bitvals]

	writesvf "SDR 8 TDI (04);"
	writesvf "SDR 72 TDI ($bitvals);"
	# Start execution
	writesvf "SDR 8 TDI (01);"
	writesvf "SDR 10 TDI (0105);"
	writesvf "SDR 8 TDI (04);"
	writesvf "SDR 72 TDI ($bitvals);"

	return
    }

    #---------------------------------------------------------------------------------------#
    # delay
    # Executes the instructions from PC in the core
    #---------------------------------------------------------------------------------------#
    proc delay { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help svf [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] == 1 } {
	    set tcks [lindex $args 0]
	} else {
	    error "wrong # args: should be \"svf delay <delay in tcks>\""
	}

	writesvf "RUNTEST $tcks TCK;"
    }
    namespace export delay
    ::xsdb::setcmdmeta {svf delay} categories {svf}
    ::xsdb::setcmdmeta {svf delay} brief {Record delay in tcks to SVF file.}
    ::xsdb::setcmdmeta {svf delay} description {
SYNOPSIS {
    svf delay <delay in tcks>
	Record delay in tcks to SVF file.
}
OPTIONS {
    None.
}
RETURNS {
    Nothing.
}
EXAMPLE {
    svf delay 1000
	Delay of 1000 tcks is added to the SVF file.
}
}

    #---------------------------------------------------------------------------------------#
    # rst
    # Reset System
    #---------------------------------------------------------------------------------------#
    proc rst { args } {
	variable config_dict
	variable arch
	variable AHB
	variable cpu_type

	set options {
	    {processor "processor reset"}
	    {system "system reset"}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]

	if { $params(help) } {
	    return [help svf [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set device_index [dict get $config_dict device_index]
	set dapname "dap$device_index"
	set cpu_index [dict get $config_dict cpu_index]

	writesvf "// reset"
	if { $params(processor) } {
	    if { $cpu_type == "a53" } {
		    set rst_ctrl [expr 1 << {$cpu_index + 10}]
		    set rst_ctrl [expr $rst_ctrl | [expr 1 << 8]]
		    set rst_ctrl [expr $rst_ctrl | [expr 1 << $cpu_index]]
		    # Bootloop
		    $dapname memwrite $AHB 0xffff0000 0x14000000
		    # Activate reset
		    $dapname memwrite $AHB 0xfd1a0104 $rst_ctrl
		    # Write the reset address to RVBARADDR
		    $dapname memwrite $AHB [expr {0xfd5c0040 + 8*$cpu_index}] 0xffff0000
		    $dapname memwrite $AHB [expr {0xfd5c0044 + 8*$cpu_index}] 0x0
		    # Clear reset
		    set rst_ctrl [expr $rst_ctrl & ~[expr {0x1 << 8}]]
		    set rst_ctrl [expr $rst_ctrl & ~[expr 1 << {$cpu_index + 10}]]
		    set rst_ctrl [expr $rst_ctrl & ~[expr 1 << $cpu_index]]
		    $dapname memwrite $AHB 0xfd1a0104 $rst_ctrl
		    # Stop the core
		    armv8_stop
	    }
	    if { $cpu_type == "r5" } {
		    set r5_cpu_index [expr $cpu_index - 0x4]
		    set rst_ctrl [expr 0x188fc0 | {0x1 << $r5_cpu_index}]
		    # Bootloop
		    $dapname memwrite $AHB 0xffff0000 0xeafffffe
		    # Activate reset
		    $dapname memwrite $AHB 0xff5e023c $rst_ctrl
		    # Start from OCM after reset
		    $dapname memwrite $AHB 0xff9a0100 0x5
		    # Clear reset
		    set rst_ctrl [expr $rst_ctrl & ~[expr {0x1 << $r5_cpu_index}]]
		    $dapname memwrite $AHB 0xff5e023c $rst_ctrl
		    # Stop the core
		    armv7_stop
	    }
	    if { $cpu_type == "a9" } {
		    set rst_ctrl [expr {0x1 << $cpu_index}]
		    # Bootloop
		    $dapname memwrite $AHB 0x0 0xeafffffe
		    # Unlock SLCR
		    $dapname memwrite $AHB 0xf8000008 0xdf0d
		    # Activate reset
		    $dapname memwrite $AHB 0xf8000244 $rst_ctrl
		    # Stop clock
		    $dapname memwrite $AHB 0xf8000244 0x11
		    # Start clock
		    $dapname memwrite $AHB 0xf8000244 0x10
		    # Clear reset
		    set rst_ctrl [expr $rst_ctrl & ~[expr {0x1 << $cpu_index}]]
		    $dapname memwrite $AHB 0xf8000244 $rst_ctrl
		    # Stop the core
		    armv7_stop
	    }
	} else {
	    $dapname reset
	}
	$dapname run
    }
    namespace export rst
    ::xsdb::setcmdmeta {svf rst} categories {svf}
    ::xsdb::setcmdmeta {svf rst} brief {Reset}
    ::xsdb::setcmdmeta {svf rst} description {
SYNOPSIS {
    svf rst
    System Reset
}
OPTIONS {
    None.
}
RETURNS {
    If successful, this command returns nothing.
    Otherwise it returns an error.
}
EXAMPLE {
    svf rst
}
}
    # writesvf
    # Write to svf file
    #---------------------------------------------------------------------------------------#
    proc writesvf { args } {
	variable config_dict
	set outfile [dict get $config_dict outfile]
	if { $outfile == "" } {
	    error "svf not configured"
	}
	foreach str $args {
	    puts $outfile $str
	}
    }

    #---------------------------------------------------------------------------------------#
    # DAP internal routines
    #---------------------------------------------------------------------------------------#
    # Get APs
    proc _cmd_getaps { name } {
	variable daps
	#dict set aps 0 {idr 1148649473 name AHB-AP mem 1 cfg 0 ctrl 0 size {8 16 32}}
	#dict set aps 1 {idr 611778562 name APB-AP mem 1 cfg 0 ctrl 0x80000000 size 32 base 2147483648}
	#dict set aps 2 {idr 343277584 name JTAG-AP mem 0}

	dict set aps 0 {idr 880214020 name AXI-AP mem 1 cfg 2 ctrl 0x30006000 size {8 16 32}}
	dict set aps 1 {idr 1148649474 name APB-AP mem 1 cfg 0 ctrl 0x80000000 size 32 base 2147483651}
	dict set aps 2 {idr 611713040 name JTAG-AP mem 0}
	dict set daps $name aps $aps
	return $aps
    }

    proc _cmd_reset {name args} {
	variable daps
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"rst\""
	}
	dict with daps $name {
	    lappend cmds [list RESET]
	}
    }

    # Abort
    proc _cmd_abort {name args} {
	variable daps
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"abort\""
	}
	dict with daps $name {
	    lappend cmds [list ABORT]
	}
	return
    }

    # DP Read
    proc _cmd_dpread {name args} {
	variable daps
	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"dpread \[addr\]\""
	}
	set addr [lindex $args 0]
	dict with daps $name {
	    lappend cmds [list DPREAD $addr]
	}
	return
    }

    # DP Write
    proc _cmd_dpwrite {name args} {
	variable daps
	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"dpwrite \[addr\] \[value\]\""
	}
	set addr [lindex $args 0]
	set value [lindex $args 1]
	dict with daps $name {
	    lappend cmds [list DPWRITE $addr $value]
	}
	return
    }

    # AP Read
    proc _cmd_apread {name args} {
	variable daps
	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"apread \[ap\] \[addr\]\""
	}
	set ap [lindex $args 0]
	set addr [lindex $args 1]
	dict with daps $name {
	    lappend cmds [list APREAD $ap $addr]
	}
	return
    }

    # AP Write
    proc _cmd_apwrite {name args} {
	variable daps
	if { [llength $args] != 3 } {
	    error "wrong # args: should be \"apwrite \[ap\] \[addr\] \[value\]\""
	}
	set ap [lindex $args 0]
	set addr [lindex $args 1]
	set value [lindex $args 2]
	dict with daps $name {
	    lappend cmds [list APWRITE $ap $addr $value]
	}
	return
    }

    proc _csw_size {size} {
	switch -- $size {
	    1 { return 0 }
	    2 { return 1 }
	    4 { return 2 }
	    8 { return 3 }
	}
	error "unsupported CSW size: $size"
    }

    # Read Memory and compare
    proc _cmd_memread {name args} {
	set options {
	    {size "access size" {args 1 default 4}}
	    {ctrl "AP ctrl value" {args 1}}
	}
	array set params [::xsdb::get_options args $options 0]

	variable daps
	set size $params(size)
	if { [llength $args] == 4 } {
	    set refval [lindex $args 2]
	    set refmask [lindex $args 3]
	} elseif {[llength $args] == 2} {
	    set refval 2
	    set refmask 0
	} else {
	    error "wrong # args: read should be \"memread \[ap\] \[addr\] \"
		   read and compare should be \"memread \[ap\] \[addr\] \[refval\] \[refmask\]\""
	}
	if { ![dict exists $daps $name aps] } {
	    _cmd_getaps $name
	}
	set ap [lindex $args 0]
	set addr [lindex $args 1]
	if { ![dict exists $daps $name aps $ap] ||
	     ![dict get $daps $name aps $ap mem] } {
	    error "invalid AP index: $ap"
	}
	if { ($addr & ($size - 1)) != 0 } {
	    error "invalid AP address alignment: $addr"
	}
	if { [info exists params(ctrl)] } {
	    set apctrl $params(ctrl)
	} else {
	    set apctrl [dict get $daps $name aps $ap ctrl]
	}
	set apctrl [expr {($apctrl & ~7) | [_csw_size $size]}]
	dict with daps $name {
	    lappend cmds [list MEMREAD $ap $apctrl $addr $refval $refmask]
	}
	return
    }

    # Write Memory
    proc _cmd_memwrite {name args} {
	set options {
	    {size "access size" {args 1 default 4}}
	    {ctrl "AP ctrl value" {args 1}}
	}
	array set params [::xsdb::get_options args $options 0]

	variable daps
	set size $params(size)
	if { [llength $args] != 3 } {
	    error "wrong # args: should be \"memwrite \[ap\] \[addr\] \[value\]\""
	}

	if { ![dict exists $daps $name aps] } {
	    _cmd_getaps $name
	}
	set ap [lindex $args 0]
	set addr [lindex $args 1]
	set value [lindex $args 2]
	if { ![dict exists $daps $name aps $ap] ||
	     ![dict get $daps $name aps $ap mem] } {
	    error "invalid AP index: $ap"
	}
	if { ($addr & ($size - 1)) != 0 } {
	    error "invalid AP address alignment: $addr"
	}
	if { [info exists params(ctrl)] } {
	    set apctrl $params(ctrl)
	} else {
	    set apctrl [dict get $daps $name aps $ap ctrl]
	}
	set apctrl [expr {($apctrl & ~7) | [_csw_size $size]}]
	dict with daps $name {
	    lappend cmds [list MEMWRITE $ap $apctrl $addr $value]
	}
	return
    }

    variable DAP_ABORT 8
    variable DAP_DPACC a
    variable DAP_APACC b

    # DAP Abort
    proc _run_set_abort {} {
	variable DAP_ABORT
	upvar 1 ir ir
	upvar 1 seq seq
	if { $ir != $DAP_ABORT } {
	    set ir $DAP_ABORT
	    writesvf "SIR 4 TDI ($ir) SMASK (f);"
	}
    }

    # DP Access
    proc _run_set_dpacc {} {
	variable DAP_DPACC
	upvar 1 ir ir
	upvar 1 seq seq
	if { $ir != $DAP_DPACC } {
	    set ir $DAP_DPACC
	    writesvf "SIR 4 TDI ($ir) SMASK (f);"
	}
    }

    # AP Access
    proc _run_set_apacc {} {
	variable DAP_APACC
	upvar 1 ir ir
	upvar 1 seq seq
	if { $ir != $DAP_APACC } {
	    set ir $DAP_APACC
	    writesvf "SIR 4 TDI ($ir) SMASK (f);"
	}
    }

    # Select DP
    proc _run_set_dpselect {addr} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_dpacc
	set diff [expr {($select ^ ($addr >> 4)) & 0xf}]
	if { $diff != 0 } {
	    lappend resptypes $resptype
	    set resptype i
	    set select [expr {$select ^ $diff}]
	    set svftdi [expr {($select << 3) | 4}]
	    set svftdi [format %lx $svftdi]
	    writesvf "SDR 35 TDI ($svftdi) TDO (2) MASK(7);"
	}
    }

    # Select AP
    proc _run_set_apselect {ap addr} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	set diff [expr {($select ^ (($ap << 24) | $addr)) & 0xff0000f0}]
	if { $diff != 0 } {
	    lappend resptypes $resptype
	    set resptype i
	    set select [expr {$select ^ $diff}]
	    _run_set_dpacc
	    set svftdi [expr {($select << 3) | 4}]
	    set svftdi [format %lx $svftdi]
	    writesvf "SDR 35 TDI ($svftdi) TDO (2) MASK(7);"
	}
	_run_set_apacc
    }

    # CLear Sticky Bits
    proc _run_clear_stickybits {} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 ctrl ctrl
	upvar 1 select select
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	upvar 1 stickybits stickybits
	if { $stickybits } {
	    # Clear CTRL/STAT.SICKYORUN
	    set addr 4
	    set data [expr {$ctrl | $stickybits}]
	    _run_set_dpselect $addr
	    lappend resptypes $resptype
	    set resptype i
	    set svftdi [expr {($data << 3) | (($addr & 0xc) >> 1)}]
	    set svftdi [format %lx $svftdi]
	    writesvf "SDR 35 TDI ($svftdi) TDO (2) MASK(7);"
	    set stickybits 0
	}
    }

    # DP Read
    proc _run_dpread {addr} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_dpselect $addr
	lappend resptypes $resptype
	set resptype r
	set svftdi [expr {(($addr & 0xc) >> 1) | 1}]
	set svftdi [format %lx $svftdi]
	writesvf "SDR 35 TDI ($svftdi) TDO (2) MASK(7);"
    }

    # DP Write
    proc _run_dpwrite {addr data} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_dpselect $addr
	lappend resptypes $resptype
	set resptype w
	set svftdi [expr {($data << 3) | (($addr & 0xc) >> 1)}]
	set svftdi [format %lx $svftdi]
	writesvf "SDR 35 TDI ($svftdi) TDO (2) MASK(7);"
    }

    # AP Read
    proc _run_apread {ap addr} {
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 apdelay apdelay
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_apselect $ap $addr
	lappend resptypes $resptype
	set resptype r
	set svftdi [expr {(($addr & 0xc) >> 1) | 1}]
	set svftdi [format %lx $svftdi]
	writesvf "SDR 35 TDI ($svftdi) TDO (2) MASK(7);"
    }

    # AP Write
    proc _run_apwrite {ap addr data {ignoretdo 0}} {
	variable config_dict
	upvar 1 ir ir
	upvar 1 seq seq
	upvar 1 select select
	upvar 1 apdelay apdelay
	upvar 1 endstate endstate
	upvar 1 resptype resptype
	upvar 1 resptypes resptypes
	_run_set_apselect $ap $addr
	lappend resptypes $resptype
	set resptype w
	set svftdi [expr {($data << 3) | (($addr & 0xc) >> 1)}]
	set svftdi [format %lx $svftdi]
	if {$ignoretdo} {
	    writesvf "SDR 35 TDI ($svftdi);"
	} else {
	    writesvf "SDR 35 TDI ($svftdi) TDO (2) MASK(7);"
	}
	set delaytcks [dict get $config_dict delaytcks]
	if { $delaytcks != 0 } {
	    writesvf "RUNTEST $delaytcks TCK;"
	}
    }

    # Command execute
    proc _cmd_run {name args} {
	variable daps
	variable config_dict
	variable cmd_dict
	variable AHB
	variable JTAG
	variable arch
	variable cpu_type
	set cpu_index [dict get $config_dict cpu_index]

	set delaytcks [dict get $config_dict delaytcks]
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"clear\""
	}
	dict with daps $name {
	    set err ""
	    if { [catch {
		set ir 15
		set rst_cmd 0
		set select 0xff0000ff
		set ncmds [llength $cmds]
		set complete 0
		$seq clear
		set resptype i
		set resptypes {}
		set stickybits 2
		_run_clear_stickybits
		set csw_shadow -1
		set csw_done 0
		set refval 2
		set refmask 0
		set result {}
		while { $complete < $ncmds } {
		    for {set i $complete} {$i < $ncmds} {incr i} {
			set cmd [lindex $cmds $i]
			switch -- [lindex $cmd 0] {
			    ABORT {
				_run_set_abort
				writesvf "SDR 35 TDI (8) TDO (2) MASK(7);"
				_run_set_dpacc
				lappend resptypes $resptype
				set resptype r
				writesvf "SDR 35 TDI (7) TDO (2) MASK(7);"
			    }

			    DPREAD {
				_run_dpread [lindex $cmd 1]
			    }

			    DPWRITE {
				_run_dpwrite [lindex $cmd 1] [lindex $cmd 2]
			    }

			    APREAD {
				_run_apread [lindex $cmd 1] [lindex $cmd 2]
			    }

			    APWRITE {
				_run_apwrite [lindex $cmd 1] [lindex $cmd 2] [lindex $cmd 3]
			    }

			    MEMREAD {
				set ap [lindex $cmd 1]
				set csw [lindex $cmd 2]
				if {!$csw_done || $csw_shadow != $csw} {
				    set csw_shadow $csw
				    set csw_done 1
				    _run_apwrite $ap 0x00 $csw
				}
				set refval [lindex $cmd 4]
				set refmask [lindex $cmd 5]
				set refval [format %lx $refval]
				set refmask [format %lx $refmask]
				set resptype i
				_run_apwrite $ap 0x04 [lindex $cmd 3]
				set resptype i
				_run_apread $ap 0x0c
			    }

			    MEMWRITE {
				set ap [lindex $cmd 1]
				set csw [lindex $cmd 2]
				if {!$csw_done || $csw_shadow != $csw} {
				    set csw_shadow $csw
				    set csw_done 1
				    _run_apwrite $ap 0x00 $csw
				}
				#writesvf "RUNTEST $delaytcks TCK;"
				set resptype i
				_run_apwrite $ap 0x04 [lindex $cmd 3]
				#writesvf "RUNTEST $delaytcks TCK;"
				set resptype i
				_run_apwrite $ap 0x0c [lindex $cmd 4]
			    }

			    DOWNLOAD {
				set f [lindex $cmd 1]
				set is_data_file [lindex $cmd 2]

				if { $is_data_file } {
				    # Data file
				    seek $f 0 end
				    set binaddr [lindex $cmd 3]
				    set total_bytes [tell $f]
				    seek $f 0
				    set addr $binaddr

				    if { $total_bytes > 0 } {
					# Set up auto-incr mode for download
					writesvf "// auto-inc"
					if { $cpu_type == "a9" } {
					     _run_apwrite $AHB 0x00 0x00000012
					} else {
					     _run_apwrite $AHB 0x00 0x30006012
					}

					writesvf "// address"
					_run_apwrite $AHB 0x04 $addr

					writesvf "// data"
					for {set i 0} {$i < [expr $total_bytes/4]} {incr i} {
					    # Set up address - TAR auto-inc can handle only address @ 1 KB boundary
					    if { [expr ($addr + 4*$i) % 0x400] == 0 } {
						_run_apwrite $AHB 0x04 [expr $addr + 4*$i]
					    }
					    set bindata [read $f 4]
					    binary scan $bindata iu bindata
					    _run_apwrite $AHB 0x0c $bindata
					}
				    }
				} else {
				    # ELF file
				    set total_bytes 0
				    foreach ph [$f get_phlist] {
					dict with ph {
					    set total_bytes 0
					    if { $type == 1 } {
						if { $filesz > 0 } {
						    set total_bytes $filesz
						    set addr [dict get $ph paddr]
						    set offset [dict get $ph offset]
						}
					    }

					    if { $total_bytes > 0 } {
						# Set up auto-incr mode for download
						writesvf "// auto-inc"
						if { $cpu_type == "a9" } {
						     _run_apwrite $AHB 0x00 0x00000012
						} else {
						     _run_apwrite $AHB 0x00 0x30006012
						}
						writesvf "// address "
						if { $cpu_type == "r5" } {
						   if { 0x0 == $addr } {
							set addr [expr $addr + 0xffe00000]
						      }
						}
						_run_apwrite $AHB 0x04 $addr


						writesvf "// data"
						for {set i 0} {$i < [expr $total_bytes/4]} {incr i} {
						    # Set up address - TAR auto-inc can handle only address @ 1 KB boundary
						    if { [expr ($addr + 4*$i) % 0x400] == 0 } {
							_run_apwrite $AHB 0x04 [expr $addr + 4*$i]
						    }
						    set bindata [$f read_at $offset 4]
						    binary scan $bindata iu bindata
						    _run_apwrite $AHB 0x0c $bindata
						    incr offset 4
						}
					    }
					}
				    }
				}
			    }
			    RESET {
				_run_apwrite $JTAG 0x00 0x0 1
				_run_apwrite $JTAG 0x04 0x1 1
				_run_apwrite $JTAG 0x00 0xd 1
				delay 500000
				set open_cmd [dict get $cmd_dict $arch open]
				if { $cpu_type == "r5" } {
				    set open_cmd zynqmp_open
				}
				if { $open_cmd == "zynqmp_open"} {
				    dict set config_dict linkdap 1
				    eval {$open_cmd}
				    dict set config_dict linkdap 0
				}
				eval {$open_cmd}
				set rst_cmd 1
			    }

			    default {
				error "invalid DAP command: $cmd"
			    }
			}
			incr complete
		    }
		}
		#Reading the CTRL/STAT and read buffer after every run
		_run_set_dpacc
		if {$rst_cmd} {
			writesvf "SDR 35 TDI (3);"
			writesvf "SDR 35 TDI (7);"
			set rst_cmd 0
		} else {
			writesvf "SDR 35 TDI (3) TDO ($refval) MASK($refmask);"
			writesvf "SDR 35 TDI (7) TDO (0) MASK(20);"
		}

	    } msg] } {
		set err $msg
	    }
	    set cmds {}

	    return
	}
    }
    # Clear command
    proc _cmd_clear {name args} {
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"clear\""
	}
	dict set daps $name cmds {}
	return
    }

    # Delete command
    proc _cmd_delete {name args} {
	variable daps
	if { [llength $args] != 0 } {
	    error "wrong # args: should be \"delete\""
	}
	[dict get $daps $name seq] delete
	dict unset daps $name
	interp alias {} ::$name {}
	return
    }

    # Dispatch command
    proc _dap_dispatch {name {cmd ""} args} {
	variable daps
	variable commands
	set matches [lsearch -all -inline -glob  $commands $cmd*]
	if { [llength $matches] == 0 } {
	    error "unknown subcommand \"$cmd\": must be [join $commands {, }]"
	}
	if { [llength $matches] == 1 } {
	    set cmd [lindex $matches 0]
	} else {
	    set match [lsearch $matches $cmd]
	    if { $match < 0 } {
		error "ambiguous subcommand \"$cmd\": must be [join $matches {, }]"
	    }
	    set cmd [lindex $matches $match]
	}
	if { ![dict exists $daps $name] } {
	    error "dap object \"$name\" is not valid"
	}
	return [_cmd_$cmd $name {*}$args]
    }

    #---------------------------------------------------------------------------------------#
    # Utility routines for MB
    #---------------------------------------------------------------------------------------#
    # Convert hexadecimal value with "0x" prefix to binary string of length len
    proc to_bits { value len } {
	set val [string range $value 2 end]
	set hexlen [string length $val]
	set binlen [expr $hexlen * 4]
	binary scan [binary format H${hexlen} $val] B${binlen} binval
	return [string range $binval [expr $binlen - $len] end]
    }

    # Convert binary string value to hexadecimal with prefix "0x"
    proc to_hex { value len } {
	set binlen [string length $value]
	set hexlen [expr ($len + 3) / 4]
	set extra  [string repeat "0" [expr $hexlen * 4 - $len]]
	binary scan [binary format B${binlen} ${extra}${value}] H${hexlen} hexval
	return "0x$hexval"
    }

    # Find mask values
    proc get_mask { offset } {
	set mask ""
	if { $offset > 64 } {
	    for {set i 0} {$i < $offset} {incr i 4} {
		if { $offset - $i > 4 } {
		    set count 4
		} else {
		    set count [expr $offset - $i]
		}
		set mask "[format %x "0b[string repeat 1 $count]"]$mask"
	    }
	} else {
	    set mask [format %lx "0b[string repeat 1 $offset]"]
	}
	return $mask
    }

    proc write_header_trailer_regs { device_index } {
	variable config_dict
	set tir_offset 0
	set tir_mask 0
	set tdr_mask 0
	set hir_offset 0
	set hir_mask 0
	set hdr_mask 0
	set scan_chain [dict get $config_dict scan_chain]
	set total_devices [expr {[llength $scan_chain] / 2}]

	for {set i 0} {$i < $device_index} {incr i} {
	    set tir_offset [expr {$tir_offset + [lindex $scan_chain [expr {($i * 2) + 1}]]}]
	}
	if { $tir_offset != 0 } {
	    set tir_mask [get_mask $tir_offset]
	}
	if { $device_index != 0 } {
	    set tdr_mask [get_mask $device_index]
	}
	for {set i [expr $device_index + 1]} {$i < $total_devices} {incr i} {
	    set hir_offset [expr {$hir_offset + [lindex $scan_chain [expr {($i * 2) + 1}]]}]
	}
	if { $hir_offset != 0 } {
	    set hir_mask [get_mask $hir_offset]
	}
	set hdr_devices [expr {$total_devices - $device_index - 1}]
	if { $hdr_devices != 0 } {
	    set hdr_mask [get_mask $hdr_devices]
	}

	writesvf "HIR $hir_offset TDI ($hir_mask) SMASK ($hir_mask);"
	writesvf "TIR $tir_offset TDI ($tir_mask) SMASK ($tir_mask);"
	writesvf "HDR $hdr_devices TDI (0) SMASK ($hdr_mask);"
	writesvf "TDR $device_index TDI (00) SMASK ($tdr_mask);"
    }

    #---------------------------------------------------------------------------------------#
    # ZynqMP Initialize
    #---------------------------------------------------------------------------------------#
    proc zynqmp_open {} {
	variable daps
	variable config_dict
	variable arch

	set scan_chain [dict get $config_dict scan_chain]
	set device_index [dict get $config_dict device_index]
	set linkdap [dict get $config_dict linkdap]
	set cpu_index [dict get $config_dict cpu_index]

	writesvf "TRST OFF;"
	writesvf "ENDIR IDLE;"
	writesvf "ENDDR IDLE;"
	writesvf "STATE RESET;"
	writesvf "STATE IDLE;"

	if { $linkdap } {
	    set device_index [expr $device_index - 1]
	    write_header_trailer_regs $device_index
	    writesvf "SIR 12 TDI (824);"
	    writesvf "SDR 32 TDI (3);"
	    writesvf "RUNTEST 10000 TCK;"
	    writesvf "STATE IDLE;"
	    writesvf "ENDIR IDLE;"
	    writesvf "ENDDR IDLE;"
	    writesvf "STATE RESET;"
	    writesvf "RUNTEST 10000 TCK;"
	    writesvf "STATE IDLE;"
	} else {
	    set name "dap$device_index"
	    set seq [jtag sequence]
	    set ctrl 0x50000001
	    dict set daps $name [dict create seq $seq cmds {} ctrl $ctrl apdelay 100 endstate DRUPDATE]
	    set dap [interp alias {} ::$name {} [namespace current]::_dap_dispatch $name]

	    writesvf "RUNTEST 100000 TCK;"
	    write_header_trailer_regs $device_index
	    writesvf "SIR 4 TDI (0e);"
	    writesvf "SDR 32 TDI (00000000) TDO (05ba00477) MASK (0fffffff);"

	    _cmd_dpwrite $name 4 $ctrl
	    _cmd_run $name
	    _cmd_apwrite $name 0 0 0x30006002
	    _cmd_run $name
	    _cmd_apwrite $name 1 0 0x80000002
	    _cmd_run $name
	}
    }

     #---------------------------------------------------------------------------------------#
    # Zynq Initialize
    #---------------------------------------------------------------------------------------#
    proc zynq_open {} {
	variable daps
	variable svfoutfile
	variable config_dict
	variable DBGVCR
	variable dbgbase
	variable APB
	set device_index [dict get $config_dict device_index]
	set scan_chain [dict get $config_dict scan_chain]
	set cpu_index [dict get $config_dict cpu_index]

	set name "dap$device_index"

	set seq [jtag sequence]
	set ctrl 0x50000001
	dict set daps $name [dict create seq $seq cmds {} ctrl $ctrl apdelay 100 endstate DRUPDATE]
	set dap [interp alias {} ::$name {} [namespace current]::_dap_dispatch $name]

	writesvf "TRST OFF;"
	writesvf "ENDIR IDLE;"
	writesvf "ENDDR IDLE;"
	writesvf "STATE RESET;"
	writesvf "STATE IDLE;"
	writesvf "RUNTEST 100000 TCK;"
	write_header_trailer_regs $device_index

	_cmd_dpwrite $name 4 $ctrl
	_cmd_run $name
	_cmd_apwrite $name 0 0 0x2
	_cmd_run $name
	_cmd_apwrite $name 1 0 0x80000002
	_cmd_run $name
	$name memwrite $APB [expr $dbgbase + $DBGVCR] 0x0; $name run
    }


    #---------------------------------------------------------------------------------------#
    # MB Initialize
    #---------------------------------------------------------------------------------------#
    proc mb_open {} {
	variable config_dict

	set scan_chain [dict get $config_dict scan_chain]
	set device_index [dict get $config_dict device_index]
	set cpu_index [dict get $config_dict cpu_index]
	set bscan [dict get $config_dict bscan]

	writesvf "TRST OFF;"
	writesvf "ENDIR IDLE;"
	writesvf "ENDDR IDLE;"
	writesvf "STATE RESET;"
	writesvf "STATE IDLE;"
	write_header_trailer_regs $device_index

	set ir_length [lindex $scan_chain [expr ($device_index * 2) + 1]]
	if { $ir_length != 0 } {
	    set bypass [get_mask $ir_length]
	}

	# For ZynqMP (PMU & MBs on PL)
	if { $ir_length == 12 && $bscan != 0x0e4 } {
	    set bscan [expr (0x24 << 6) | ($bscan)]
	}
	# Bypass
	writesvf "// bypass"
	writesvf "SIR $ir_length TDI ($bypass);"

	# Select USER bscan
	# MDM USER register
	writesvf "// select user bscan for pmu mdm"
	set bscan [format %lx "$bscan"]
	writesvf "SIR $ir_length TDI ($bscan);"

	# Select MDM port
	writesvf "// select mdm port"
	writesvf "SDR 4 TDI (01);"

	# Select which MB
	writesvf "// which mb"
	writesvf "SDR 8 TDI (0d);"
	# CPU index should always be 0 for PMU
	if { $bscan == "e4" } {
	    set cpu_index 0
	}
	set select_mb_reg_bitlen 8
	if { $cpu_index > 7 } {
	    set select_mb_reg_bitlen [expr ($cpu_index + 1)]
	}
	set cpu_index [format %lx [expr (1 << $cpu_index)]]
	writesvf "SDR $select_mb_reg_bitlen TDI ($cpu_index);"
    }

    namespace ensemble create -command ::svf
}
package provide xsdb::svf $::xsdb::svf::version
