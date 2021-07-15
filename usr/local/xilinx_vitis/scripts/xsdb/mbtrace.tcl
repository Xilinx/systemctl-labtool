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

namespace eval ::xsdb::mbtrace {
    variable version 0.1
    variable trace_dict [dict create]
    variable trace_elftext
    variable trace_read_seq ""

    package require xsdb::elf
    package require xsdb::mbtrace_dis

    variable trigger_event
    array set trigger_event {
      0 "Debug stop"
      1 "Continue execution"
      2 "Stop program trace"
      3 "Start program trace"
      4 "Stop performance monitor"
      5 "Start performance monitor"
      6 "Disable profiling"
      7 "Enable profiling"
    }
    variable exception_event
    array set exception_event {
      0 "Stream exception"
      1 "Unaligned data access exception"
      2 "Illegal op-code exception"
      3 "Instruction bus error exception"
      4 "Data bus error exception"
      5 "Divide exception"
      6 "Floating point unit exception"
      7 "Privileged instruction exception"
      8 "Unexpected exception 8"
      9 "Debug"
     10 "Interrupt"
     11 "Non-maskable break"
     12 "Break"
     13 "Unexpected exception 13"
     14 "Unexpected exception 14"
     15 "Unexpected exception 15"
     16 "Data storage exception"
     17 "Instruction storage exception"
     18 "Data TLB miss exception"
     19 "Instruction TLB miss exception"
     20 "Unexpected exception 20"
     21 "Unexpected exception 21"
     22 "Unexpected exception 22"
     23 "Unexpected exception 23"
     24 "Unexpected exception 24"
     25 "Unexpected exception 25"
     26 "Unexpected exception 26"
     27 "Unexpected exception 27"
     28 "Unexpected exception 28"
     29 "Unexpected exception 29"
     30 "Unexpected exception 30"
     31 "Unexpected exception 31"
    }

    #---------------------------------------------------------------------------------------#
    # MB Trace Initialization
    # This routine is used to initialize the MB trace
    #---------------------------------------------------------------------------------------#
    proc mbtrace_set_config { config } {
	variable trace_dict
	set trace_dict $config
        dict set trace_dict init_done 0
    }

    #---------------------------------------------------------------------------------------#
    # MB Trace Initialization
    # This routine is used to initialize the MB trace
    #---------------------------------------------------------------------------------------#
    proc mbtrace_init {} {
	variable trace_dict

	dict set trace_dict ctrl_reg     0
	dict set trace_dict cmd_reg      0
	dict set trace_dict cycles       0
	dict set trace_dict packet_count 0
	dict set trace_dict tpiu_packet  {}
	dict set trace_dict tpiu_index   0
	dict set trace_dict ftm_packet   {}
	dict set trace_dict ftm_index    0
	dict set trace_dict dis_getaddr  0x0
	dict set trace_dict dis_items    {}
	dict set trace_dict dis_index    0
	dict set trace_dict init_done    1

	set mode        [dict get $trace_dict mode]
	set halt        [dict get $trace_dict halt]
	set save        [dict get $trace_dict save]
	set low         [dict get $trace_dict low]
	set high        [dict get $trace_dict high]
	set data_format [dict get $trace_dict data_format]
	set trace_dma   [dict get $trace_dict trace_dma]

	mbtrace_set $mode $halt $save $low $high $data_format $trace_dma
    }

    #---------------------------------------------------------------------------------------#
    # MB Trace Check init
    # This routine is used to check if trace is initialized
    #---------------------------------------------------------------------------------------#
    proc mbtrace_checkinit {} {
	variable trace_dict

	if {[dict get $trace_dict init_done] == 1} {
	    return 1
	}
	error "Trace not configured, please configure using command mbtrace"
    }

    #---------------------------------------------------------------------------------------#
    # MB Trace Set
    # This routine is used to set trace parameters
    #---------------------------------------------------------------------------------------#
    proc mbtrace_set { mode halt saveld low high data_format trace_dma } {
	variable trace_dict
	variable trace_elftext
	variable trace_read_seq

	if {![mbtrace_checkinit]} { return }

	dict set trace_dict mode        $mode
	dict set trace_dict halt        $halt
	dict set trace_dict save        $saveld
	dict set trace_dict low         $low
	dict set trace_dict high        $high
	dict set trace_dict data_format $data_format
	dict set trace_dict trace_dma   $trace_dma

        # Determine MicroBlaze properties: bscan, which, mdmaddr, mdmconfig
	set chan [xsdb::getcurchan]
	set ctx [xsdb::getcurtarget]
	set props [xsdb::get_target_microblaze_props $chan $ctx]

	dict set trace_dict bscan ""
	if { [dict exists $props JtagChain] } {
	    dict set trace_dict bscan "[string range [dict get $props JtagChain] 4 end]"
	}

	if { [dict exists $props MBCore] && [dict get $props MBCore] >= 0 } {
	    dict set trace_dict which [dict get $props MBCore]
	} else {
	    error "Invalid target. Only supported for MicroBlaze targets"
	}

	dict set trace_dict mdmaddr ""
	if { [dict exists $props MDMAddr] } {
	    dict set trace_dict mdmaddr [dict get $props MDMAddr]
	}

	dict set trace_dict mdmconfig [dict get $props MDMConfig]

	# Configuration Register Read
	set config_extended_debug     [mb_get_config 161]
	set config_trace_size         [mb_get_config 269 271] ; # 1=8096, 2=16384, ...
	set config_has_external_trace [mb_get_config 275]
	set config_addr_size          [mb_get_config 276 281]
	set config_data_size_64       [mb_get_config 282]

	if {$config_extended_debug == 0} {
	  error "Trace is not enabled in the design. Enable Extended Debug in MicroBlaze configuration"
	} 
        if {$config_trace_size == 0} {
	  error "Trace is not enabled in the design. Set Trace Size in MicroBlaze configuration"
	}

        set pc_items 2
        if {$config_addr_size > 0 && $config_data_size_64 == 1} {
          set pc_items [expr $config_addr_size > 16 ? 4 : 3]
	}
        dict set trace_dict pc_items $pc_items

	set config_mdm [mb_trace_mdm_read_config]
	set config_mdm_trace_output   0
	set config_mdm_trace_protocol 0
	if {$config_mdm & 0x00400000} {
	  set config_mdm_ext            [mb_trace_mdm_read_ext_config]
	  set config_mdm_trace_output   [expr ($config_mdm_ext >> 35) & 3]
	  set config_mdm_trace_protocol [expr ($config_mdm_ext >> 37) & 1]
	}
	set config_mdm_mb_dbg_ports [expr (($config_mdm >> 8) & 0xff) > 1]
	set output $config_mdm_trace_output
	if {($output > 0) && ($config_has_external_trace == 0)} {
	  set output 0
	}
	dict set trace_dict output    $output
	dict set trace_dict protocol  $config_mdm_trace_protocol
	dict set trace_dict dbg_ports $config_mdm_mb_dbg_ports

	if {$output > 0 && $mode == 0} {
	  error "Full trace not available with external trace"
	}
	if {$output > 0 && $low > $high} {
	  error "Must give valid low and high address for external memory trace buffer"
	}

	# Initialize control register
	set saveret [expr $mode != 2]
	set savepc  [expr $mode == 2]
	set ctrl    [expr ($mode << 4) | ($halt << 3) | ($savepc << 2) | ($saveld << 1) | $saveret]
	mb_trace_write_control $ctrl

	# Stop and clear trace
	set cmd_stop  2
	set cmd_clear 8
	set cmd [expr $cmd_clear | $cmd_stop]
	mb_trace_write_command $cmd

	# Prepare read data JTAG sequence
	set bscan [dict get $trace_dict bscan]
	if {$output == 0 && $bscan != ""} {
	  set dbg_ports [dict get $trace_dict dbg_ports]

	  if {$trace_read_seq != ""} { $trace_read_seq delete }
	  set trace_read_seq [jtag sequence]
	  $trace_read_seq irshift -state IRUPDATE -register bypass
	  $trace_read_seq irshift -state IDLE -register user$bscan
	  $trace_read_seq drshift -state DRUPDATE -int 4 1
	  # Set MDM Which MB register to current target, if more than one MicroBlaze
	  if {$dbg_ports > 1} {
	    set len [expr $dbg_ports > 8 ? $dbg_ports : 8]
	    $trace_read_seq drshift -state DRUPDATE -int 8 0x0d
	    $trace_read_seq drshift -state DRUPDATE -int $len [expr 1 << $which]
	  }
	  for {set i 0} {$i < 512 - 1} {incr i} {
	    $trace_read_seq drshift -state DRUPDATE -int 8 0x66
	    $trace_read_seq drshift -state DRUPDATE -tdi 0 -capture 18
	  }
	  $trace_read_seq drshift -state DRUPDATE -int 8 0x66
	  $trace_read_seq drshift -state IDLE -tdi 0 -capture 18
	}

	# Set AXI Stream delay between words required by FTM
	if {$output == 2 && $data_format != "mdm"} {
	  mb_trace_mdm_write_control 0x10 8
	}

	# Set external trace low and high address
	set lowaddr  [expr $low  >> 16]
	set highaddr [expr $high >> 16]
	if {$output == 3} {
	  if {[catch {mb_trace_mdm_write_lowaddr $lowaddr} err]} {
	    error "Failed to write low address: \"$err\""
	  }
	  if {[catch {mb_trace_mdm_write_highaddr $highaddr} err]} {
	    error "Failed to write high address: \"$err\""
	  }
	} elseif {$output > 0} {
	  mwr [expr $trace_dma + 16] $lowaddr
	  mwr [expr $trace_dma + 20] $highaddr
	}

	# Get current ELF contents and prepare disassembly
        array set trace_elftext {}
	if {[dict exist $trace_dict elf]} {
          set elf_file [dict get $trace_dict elf]
	  set elf [xsdb::elf::open $elf_file]
	  foreach sec [xsdb::elf::get_exec_sections $elf] {
	    set secname [dict get $sec name]
	    set secdata [lindex [xsdb::elf::get_elf_section_data $elf $secname] 1]
	    set secaddr [dict get $secdata loadaddr]
	    set secbin  [dict get $secdata secbin]
	    binary scan $secbin H* sechex
	    for {set index 0} {$index < [string length $sechex]} {} {
	      set b0 [string range $sechex $index $index+1] ; incr index 2
	      set b1 [string range $sechex $index $index+1] ; incr index 2
	      set b2 [string range $sechex $index $index+1] ; incr index 2
	      set b3 [string range $sechex $index $index+1] ; incr index 2
	      set instr "0x${b3}${b2}${b1}${b0}"
	      set trace_elftext($secaddr) [list $instr [xsdb::mbtrace_dis::disassemble $secaddr $instr]]
	      incr secaddr 4
	    }
	  }
	  xsdb::elf::close $elf
	}

	return ""
    }

    #---------------------------------------------------------------------------------------#
    # MB Trace Start
    # This routine is used to turn on trace
    #---------------------------------------------------------------------------------------#
    proc mbtrace_start {} {
	variable trace_dict

	if {![mbtrace_checkinit]} { return }

	set output [dict get $trace_dict output]
	set halt   [dict get $trace_dict halt]

	# Set external trace to halt when trace buffer full
	if {$output == 3} {
	    mb_trace_mdm_write_control $halt 1
	} elseif {$output > 0} {
	    set trace_dma_addr [dict get $trace_dict trace_dma]
	    mwr [expr $trace_dma_addr + 24] $halt
	}

	set cmd_start 4
	mb_trace_write_command $cmd_start
    }

    #---------------------------------------------------------------------------------------#
    # MB Trace Stop
    # This routine is used to turn off trace
    #---------------------------------------------------------------------------------------#
    proc mbtrace_stop {} {
	variable trace_dict

	if {![mbtrace_checkinit]} { return }

	set cmd_stop 2
	mb_trace_write_command $cmd_stop
    }

    #---------------------------------------------------------------------------------------#
    # Get Instr
    # This routine gets the next instruction to output, either from target or ELF file
    #---------------------------------------------------------------------------------------#
    proc get_instr { pc {disas 0} } {
      variable trace_dict
      variable trace_elftext

      if {[array size trace_elftext] == 0} {
	# Get instruction from target
	set result [mrd -value $pc]
	if {$disas} {
	  set result [xsdb::mbtrace_dis::disassemble $pc $result]
	}
      } else {
	# Get instruction from saved ELF data
	set result [lindex [array get trace_elftext $pc] 1]
	if {$result == ""} {
	  # Program counter not within executable section
	  if {! $disas} { set result "0x00000000" }
	} else {
	  set result [lindex $result $disas]
	}
      }
      return $result
    }

    #---------------------------------------------------------------------------------------#
    # Puts Dis
    # This routine outputs disassembly to the channel
    #---------------------------------------------------------------------------------------#
    proc puts_dis { chan pc {cycles ""} {extra ""} } {
	set text [get_instr $pc 1]
	if {$text == ""} {
	    return 1
	}
	if {[string length $cycles] == 0} {
	    set cycle_text "       "
	} else {
	    set cycle_text "[format {%-6d } $cycles]"
	}
	if {[string length $extra] > 0} {
	    set text "[format {%s%-42s; %s} $cycle_text $text $extra]"
	} else {
	    set text "[format {%s%s} $cycle_text $text]"
	}
	puts $chan [string trimright $text]
	return 0
    }

    #---------------------------------------------------------------------------------------#
    # Puts Event
    # This routine outputs an event to the channel
    #---------------------------------------------------------------------------------------#
    proc puts_event { chan event {extra ""} } {
	variable trace_dict
	variable trigger_event
	variable exception_event

	set mode         [dict get $trace_dict mode]
	set total_cycles [dict get $trace_dict cycles]

	set text "Event ([format {%04x} $event]): "
	if {($event & 0xC000) == 0x8000} {
	    for {set i 0} {$i < 8} {incr i} {
	      if {($event & (1 << $i)) != 0} {
		append text "\"$trigger_event($i)\" "
	      }
	    }
	}
	if {($event & 0xC000) == 0xC000} {
	    set ec [expr $event & 0x1f]
	    append text "\"$exception_event($ec)\" "
	}
	if {($event & 0xC000) == 0x4000} {
	    set total_cycles [expr $total_cycles + ($event & 0x3FFF)]
	}

	if {$mode != 2} {
	    set cycle_text "       "
	} elseif {[string length $total_cycles] == 0} {
	    set cycle_text "       "
	} else {
	    set cycle_text "[format {%-20d } $total_cycles]"
	}
	set text "[format {%s%-50s%s} $cycle_text $text $extra]"
	puts $chan [string trimright $text]

	set total_cycles 0
	dict set trace_dict cycles $total_cycles
    }

    #---------------------------------------------------------------------------------------#
    # Get TPIU Byte
    # This routine gets a byte from a TPIU encoded packet
    #---------------------------------------------------------------------------------------#
    proc get_tpiu_byte {} {
	variable trace_dict

	set tpiu_packet [dict get $trace_dict tpiu_packet]
	set tpiu_index  [dict get $trace_dict tpiu_index]

	if {[llength $tpiu_packet] == 0} {
	  # Read TPIU frame - skip any synchronization packets 0x7FFFFFFF
	  set low         [dict get $trace_dict low]
	  set high        [dict get $trace_dict high]
	  set data_format [dict get $trace_dict data_format]
	  set dis_getaddr [dict get $trace_dict dis_getaddr]

	  set frame {}
	  set i 0
	  while {$i < 4} {
	    set item [mrd -value $dis_getaddr]
	    if {$item != 0x7FFFFFFF} {
	      lappend frame $item
	      incr i
	    }
	    incr dis_getaddr 4
	  }
	  if {$dis_getaddr >= $high + 0x10000} {
	    set dis_getaddr $low ; # Trace buffer wrap
	  }
	  dict set trace_dict dis_getaddr $dis_getaddr

	  set packet_count [dict get $trace_dict packet_count]
	  incr packet_count
	  dict set trace_dict packet_count $packet_count

	  # Split TPIU frame into bytes
	  set blist {}
	  for {set k 0} {$k < 4} {incr k} {
	    for {set i 0} {$i < 4} {incr i} {
	      set b [expr ([lindex $frame $k] >> ($i*8)) & 0xff]
	      lappend blist $b
	    }
	  }

	  if {$data_format == "tpiu"} {
	    # Decode TPIU packet
	    set final [lindex $blist 15]
	    for {set k 0} {$k < 15} {incr k} { ; # Skip final byte
	      set b [lindex $blist $k]
	      if {($k & 1) == 1} { ; # Data byte
		lappend tpiu_packet $b
	      } elseif {($b & 1) == 0} { ; # Data byte with final bit
		lappend tpiu_packet [expr ($b & 0xfe) | ($final & 1)]
		set final [expr $final >> 1]
	      } else { ; # ID byte with final bit
		set id [expr ($b & 0xfe) | ($final & 1)]
		if {$id != 0x40 && $id != 0x41} {
		  # Accept 0x40 and 0x41 - When FTM is set up with FTMATID = 0x20, it outputs 0x40 or 0x41
		  error "Packet error: expected ID 0x40 or 0x41, found [format {0x%02x} $id]"
		}
		set final [expr $final >> 1]
	      }
	    }
	  } else {
	    set tpiu_packet $blist
	  }
	  dict set trace_dict tpiu_packet $tpiu_packet
	  set tpiu_index 0
	}

	set res [lindex $tpiu_packet $tpiu_index]
	incr tpiu_index
	if {$tpiu_index == [llength $tpiu_packet]} {
	  dict set trace_dict tpiu_packet {}
	}
	dict set trace_dict tpiu_index $tpiu_index

	return $res
    }

    #---------------------------------------------------------------------------------------#
    # Get FTM Byte
    # This routine gets a byte from an FTM packet
    #---------------------------------------------------------------------------------------#
    proc get_ftm_byte {} {
	variable trace_dict

	set ftm_packet [dict get $trace_dict ftm_packet]
	set ftm_index  [dict get $trace_dict ftm_index]

	if {[llength $ftm_packet] == 0} {
	  # Decode FTM packets
	  set expecting_first 1
	  set type "unknown"
	  set index 0
	  set expected_len 0
	  set done 0
	  while {! $done} {
	    set b [get_tpiu_byte]
	    if {($b & 0x80) == 0} {
	      # First or last byte in packet
	      if {$expecting_first} {
		set index 0
		if {$b == 0x00} {
		  # Synchronization packet
		  set type "sync"
		  set expected_len 8
		} elseif {$b == 0x20} {
		  # Trigger packet
		  set type "trigger"
		  set expected_len 4
		  set data $b
		} elseif {$b == 0x28} {
		  # Manual does not include this packet type
		  set type "undocumented"
		  set expected_len 4
		  set data $b
		} elseif {$b == 0x68} {
		  set type "overflow"
		  set expected_len 4
		  set data $b
		} elseif {($b & 0x87) == 5} {
		  # Trace packet
		  set type "trace"
		  set expected_len 5
		  set data [expr ($b >> 3) & 0xf]
		} else {
		  error "Unexpected packet type: [format {%02x} $b] = [format {%02x} [expr $b & 0x87]]"
		}
	      } else {
		# Last data byte
		incr index
		set len [expr $index + 1]
		if {$len != $expected_len} {
		  error "Unexpected packet length: expected $expected_len found $len"
		}
		if {$type == "trace"} {
		  set data [expr (($b & 0x7F) << 25) | $data]
		  set done 1
		}
		if {$type == "cycles"} {
		  set data [expr (($b & 0x7F) << 25) | $data]
		  puts "FTM cycles: [format {%d} $data]"
		}
		if {$type == "trigger"} {
		  set data [expr ($b << 24) | $data]
		  puts "FTM trigger (packet = [format {0x%08x} $data])"
		}
		if {$type == "overflow"} {
		  set data [expr ($b << 24) | $data]
		  puts "FTM FIFO overflow (packet = [format {0x%08x} $data])"
		}
		if {$type == "undocumented"} {
		  set data [expr ($b << 24) | $data]
		  puts "FTM undocumented packet type (packet = [format {0x%08x} $data])"
		}
	      }
	      set expecting_first [expr ! $expecting_first]
	    } else {
	      # Intermediate bytes in packet
	      incr index
	      if {$type == "trace" || $type == "cycles"} {
		# Intermediate data or count bytes
		set data [expr (($b & 0x7f) << (($index - 1) * 7 + 4)) | $data]
	      }
	      if {$type == "trigger" || $type == "overflow" || $type == "undocumented"} {
		set data [expr (($b & 0xff) << ($index * 8)) | $data]
	      }
	      if {$type == "trace" || $type == "trigger"} {
		if {$index == $expected_len} {
		  # Cycle count packet next
		  set type "cycles_next"
		  if {$type == "trace"} {
		    puts "Trace: [format {0x%08x} $data]"
		  }
		  if {$type == "trigger"} {
		    puts "Trigger (packet = [format {0x%08x} $data])"
		  }
		}
	      }
	      if {$type == "cycles_next"} {
		# Cycle count packet
		set type "cycles"
		set expected_len 5
		set data [expr ($b >> 3) & 0xf]
	      }
	    }
	  }

	  # Split packet into bytes
	  for {set i 0} {$i < 4} {incr i} {
	    set b [expr ($data >> ($i*8)) & 0xff]
	    lappend ftm_packet $b
	  }
	  set ftm_index 0
	  dict set trace_dict ftm_packet $ftm_packet
	}

	set res [lindex $ftm_packet $ftm_index]
	incr ftm_index
	if {$ftm_index == [llength $ftm_packet]} {
	  dict set trace_dict ftm_packet {}
	}
	dict set trace_dict ftm_index $ftm_index
	return $res
    }

    #---------------------------------------------------------------------------------------#
    # Get Disassembly Item
    # This routine gets the next disassembly item, decoding any packets if necessary
    #---------------------------------------------------------------------------------------#
    proc get_dis_item {} {
	variable trace_dict
        variable trace_read_seq

	set output       [dict get $trace_dict output]
	set protocol     [dict get $trace_dict protocol]
	set dis_items    [dict get $trace_dict dis_items]
	set dis_index    [dict get $trace_dict dis_index]

	set trace_id 110 ; # Must match the MDM parameter C_TRACE_ID with default value 110

	if {$output == 3 || $output == 1 || ($output == 2 && $protocol == 1)} {
	  # Read and decode packet from external memory if needed, and then return an item
	  if {[llength $dis_items] == 0} {
	    # Read packet
	    set low         [dict get $trace_dict low]
	    set high        [dict get $trace_dict high]
	    set dis_getaddr [dict get $trace_dict dis_getaddr]

	    set packet [mrd -value $dis_getaddr 20]
	    incr dis_getaddr 80
	    if {$dis_getaddr >= $high + 0x10000} {
	      set dis_getaddr $low ; # Trace buffer wrap
	    }
	    dict set trace_dict dis_getaddr $dis_getaddr

	    # Decode packet based on protocol
	    set packet_count [dict get $trace_dict packet_count]
	    set data {}
	    set data_is_id 0
	    for {set f 0} {$f < 5} {incr f} {
	      set final [expr ([lindex $packet [expr $f * 4 + 3]] >> 24) & 0xff]
	      for {set k [expr $f * 4]} {$k < $f * 4 + 4} {incr k} {
		for {set i 0} {$i < 4} {incr i} {
		  set b [expr ([lindex $packet $k] >> ($i*8)) & 0xff]
		  if {($k == $f * 4 + 3) && ($i == 3)} { break } ; # Skip final byte
		  if {($i & 1) == 1} { ; # Data byte, or ID for protocol 1
		    if {$data_is_id} {
		      if {$b != 0x20} {
			error "Packet error ($packet_count): expected ID 0x20, found [format {0x%02x} $b] (protocol $protocol)"
		      }
		      set data_is_id 0
		    } else {
		      lappend data $b
		    }
		  } elseif {($b & 1) == 0} { ; # Data byte with final bit
		    lappend data [expr ($b & 0xfe) | ($final & 1)]
		    set final [expr $final >> 1]
		  } else { ; # ID byte with final bit
		    set id_byte [expr ($b & 0xfe) | ($final & 1)]
		    if {$protocol == 0 && $id_byte != 0x20} {
		      error "Packet error ($packet_count): expected ID 0x20, found [format {0x%02x} $id_byte] (protocol $protocol)"
		    }
		    if {$protocol == 1 && $id_byte != $trace_id * 2 && $id_byte != ($trace_id + 1) * 2} {
		      error "Packet error ($packet_count): expected ID [format {0x%02x} $trace_id] or [format {0x%02x} [expr $trace_id + 1]], found [format {0x%02x} [expr $id_byte / 2]] (protocol $protocol)"
		    }
		    set data_is_id [expr $protocol == 1 && $id_byte == $trace_id * 2]
		    set final [expr $final >> 1]
		  }
		}
	      }
	    }
	    if {[llength $data] != 72} {
	      error "Packet error ($packet_count): expected 72 bytes, found [llength $data] (protocol $protocol)"
	    }

	    # Generate trace data
	    set dis_items {}
	    for {set k 0} {$k < [llength $data]} {incr k 9} {
	      lappend dis_items [expr [lindex $data $k]          + ([lindex $data [expr $k+1]] << 8) + (([lindex $data [expr $k+8]] << 16) & 0x30000)]
	      lappend dis_items [expr [lindex $data [expr $k+2]] + ([lindex $data [expr $k+3]] << 8) + (([lindex $data [expr $k+8]] << 14) & 0x30000)]
	      lappend dis_items [expr [lindex $data [expr $k+4]] + ([lindex $data [expr $k+5]] << 8) + (([lindex $data [expr $k+8]] << 12) & 0x30000)]
	      lappend dis_items [expr [lindex $data [expr $k+6]] + ([lindex $data [expr $k+7]] << 8) + (([lindex $data [expr $k+8]] << 10) & 0x30000)]
	    }
	    dict set trace_dict dis_items $dis_items

	    set dis_index 0
	  }

	} elseif {$output == 2} {
	  # Read and decode packet from external memory if needed, and then return an item
	  if {[llength $dis_items] == 0} {

	    set data_format [dict get $trace_dict data_format]
	    if {$data_format == "mdm"} {
	      # Read packet
	      set low         [dict get $trace_dict low]
	      set high        [dict get $trace_dict high]
	      set dis_getaddr [dict get $trace_dict dis_getaddr]

	      set packet [mrd -value $dis_getaddr 18]
	      incr dis_getaddr 72
	      if {$dis_getaddr >= $high + 0x10000} {
		set dis_getaddr $low ; # Trace buffer wrap
	      }
	      dict set trace_dict dis_getaddr $dis_getaddr

	      # Split packet into bytes
	      set data {}
	      for {set k 0} {$k < 18} {incr k} {
		for {set i 0} {$i < 4} {incr i} {
		  set b [expr ([lindex $packet $k] >> ($i*8)) & 0xff]
		  lappend data $b
		}
	      }

	      # Generate trace data
	      set dis_items {}
	      for {set k 0} {$k < [llength $data]} {incr k 9} {
		lappend dis_items [expr [lindex $data $k]          + ([lindex $data [expr $k+1]] << 8) + (([lindex $data [expr $k+8]] << 16) & 0x30000)]
		lappend dis_items [expr [lindex $data [expr $k+2]] + ([lindex $data [expr $k+3]] << 8) + (([lindex $data [expr $k+8]] << 14) & 0x30000)]
		lappend dis_items [expr [lindex $data [expr $k+4]] + ([lindex $data [expr $k+5]] << 8) + (([lindex $data [expr $k+8]] << 12) & 0x30000)]
		lappend dis_items [expr [lindex $data [expr $k+6]] + ([lindex $data [expr $k+7]] << 8) + (([lindex $data [expr $k+8]] << 10) & 0x30000)]
	      }
	    }

	    if {$data_format == "tpiu" || $data_format == "ftm"} {
	      # Get FTM bytes for one MDM trace packet (18 words)
	      set data {}
	      for {set k 0} {$k < 18 * 4} {incr k} {
		lappend data [get_ftm_byte]
	      }

	      # Generate trace data
	      set dis_items {}
	      for {set k 0} {$k < [llength $data]} {incr k 9} {
		lappend dis_items [expr [lindex $data $k]          + ([lindex $data [expr $k+1]] << 8) + (([lindex $data [expr $k+8]] << 16) & 0x30000)]
		lappend dis_items [expr [lindex $data [expr $k+2]] + ([lindex $data [expr $k+3]] << 8) + (([lindex $data [expr $k+8]] << 14) & 0x30000)]
		lappend dis_items [expr [lindex $data [expr $k+4]] + ([lindex $data [expr $k+5]] << 8) + (([lindex $data [expr $k+8]] << 12) & 0x30000)]
		lappend dis_items [expr [lindex $data [expr $k+6]] + ([lindex $data [expr $k+7]] << 8) + (([lindex $data [expr $k+8]] << 10) & 0x30000)]
	      }
	    }
	    dict set trace_dict dis_items $dis_items
	    set dis_index 0
	  }

	} else {
	  # Return an item from the embedded trace buffer
	  if {[llength $dis_items] == 0} {
	    if {$trace_read_seq != ""} {
	      set tdo_value [$trace_read_seq run -current-target -single -binary]
	      for {set tdo_index 0} {$tdo_index < 1536} {incr tdo_index 3} {
		set binval [string range $tdo_value $tdo_index $tdo_index+2]
		binary scan $binval b18 b
		binary scan [binary format B20 00$b] H5 hex
		lappend dis_items "0x$hex"
	      }
	    } else {
	      set mdmaddr [dict get $trace_dict mdmaddr]
	      for {set i 0} {$i < 512} {incr i} {
		lappend dis_items [mrd -value [expr $mdmaddr + 0x5980]]
	      }
	    }
	    dict set trace_dict dis_items $dis_items
	    set dis_index 0
	  }
	}

	set res [lindex $dis_items $dis_index]
	incr dis_index
	if {$dis_index == [llength $dis_items]} {
	  dict set trace_dict dis_items {}
	}
	dict set trace_dict dis_index $dis_index
	return $res
    }

    #---------------------------------------------------------------------------------------#
    # MB Trace Disassembly
    # These routines disassemble and output the trace on the channel
    #---------------------------------------------------------------------------------------#
    proc mbtrace_dis_internal { {chan stdout} {lines -1} } {
	variable trace_dict

	set mode        [dict get $trace_dict mode]
	set control     [dict get $trace_dict ctrl_reg]
	set output      [dict get $trace_dict output]
	set protocol    [dict get $trace_dict protocol]
	set low         [dict get $trace_dict low]
	set high        [dict get $trace_dict high]
        set pc_items    [dict get $trace_dict pc_items]

	if {![mbtrace_checkinit]} { return 1 }

	if {$output == 0} {
	  set cmd_sample 1
	  mb_trace_write_command $cmd_sample
	  set count [expr [mb_trace_read_status] & 0xFFFF]
	} else {
	  if {$output == 3} {
	    set stat [mb_trace_mdm_read_status]
	    set resp [expr $stat & 3]
	    set wrap [expr ($stat >> 2) & 1]
	    set curraddr [mb_trace_mdm_read_curraddr]
	  } else {
	    set trace_dma_addr [dict get $trace_dict trace_dma]
	    set stat [mrd -value [expr $trace_dma_addr + 8]]
	    set resp [expr $stat & 3]
	    set wrap [expr ($stat >> 2) & 1]
	    set curraddr [mrd -value [expr $trace_dma_addr + 12]]
	  }
	  if {$resp != 0} {
	    error "External trace response error $resp: Check address range [format {0x%08x} $low] - [format {0x%08x} $high]"
	  }
	  # Protocol 0: 18 32-bit words per packet = 16 36-bit words = 32 items
	  # Protocol 1: 20 32-bit words per packet = 16 36-bit words = 32 items
	  set bytes_per_packet [expr 72 + 8 * $protocol]
	  if {$wrap} {
	    set count [expr ($high + 0x10000 - $low) / $bytes_per_packet * 32]
	    set dis_getaddr $curraddr
	  } else {
	    set count [expr ($curraddr - $low) / $bytes_per_packet * 32]
	    set dis_getaddr $low
	  }
	  dict set trace_dict dis_getaddr $dis_getaddr
	}
	dict set trace_dict dis_items   {}
	dict set trace_dict dis_index   0

	set err 0
	set i   0

	# Full trace
	if {$mode == 0} {
	  set line 0
	  while {$i < $count && ($line < $lines || $lines == -1)} {
	    incr i 8
	    incr line

	    set traceline {}
	    for {set n 0} {$n < 8} {incr n} {
	      set item [get_dis_item]
	      lappend traceline $item
	    }
	    set cycles  [expr (([lindex $traceline 0] >> 3) & 0x7FFF) + 1]
	    set msr     [expr (([lindex $traceline 0] << 12) & 0x7000) | \
			      (([lindex $traceline 1] >> 6)  & 0xFFF)]
	    set regaddr [expr ([lindex $traceline 1] >> 1) & 0x1F]
	    set regwr   [expr [lindex $traceline 1] & 1]
	    set exckind [expr ([lindex $traceline 2] >> 13) & 0x1F]
	    set exc     [expr ([lindex $traceline 2] >> 12) & 1]
	    set datard  [expr ([lindex $traceline 2] >> 11) & 1]
	    set datawr  [expr ([lindex $traceline 2] >> 10) & 1]
	    set be      [expr ([lindex $traceline 2] >> 6) & 0xF]
	    set data    [expr (([lindex $traceline 2] << 26) & 0xFC000000) | \
			      (([lindex $traceline 3] << 8)  & 0x3FFFF00)  | \
			      (([lindex $traceline 4] >> 10) & 0xFF)]
	    set addr    [expr (([lindex $traceline 4] << 22) & 0xFFC00000) | \
			      (([lindex $traceline 5] << 4)  & 0x3FFFF0)   | \
			      (([lindex $traceline 6] >> 14) & 0xF)]
	    set pc      [expr (([lindex $traceline 6] << 18) & 0xFFFC0000) | [lindex $traceline 7]]

	    # Skip output of last item if it is a branch with delayslot, immediate value or return,
	    # since those instructions will be re-executed when execution continues
	    if {$i >= $count} {
	      set instr [get_instr $pc]
	      set opcode [format {0x%02X} [expr $instr >> 26]]
	      switch $opcode {
		0x2E -
		0x2F {
		  # Static branch
		  set delay_slot [expr ($opcode == 0x2E && (($instr & 0x00100000) != 0)) || \
				       ($opcode == 0x2F && (($instr & 0x02000000) != 0))]
		  if {$delay_slot} { break }
		}
		0x26 -
		0x27 {
		  # Dynamic branch
		  set delay_slot [expr ($opcode == 0x26 && (($instr & 0x00100000) != 0)) || \
				       ($opcode == 0x27 && (($instr & 0x02000000) != 0))]
		  if {$delay_slot} { break }
		}
		0x2C -
		0x2D {
		  # Immediate value or return
		  break
		}
	      } 
	    }

	    # Output
	    if {$regwr} {
	      set err [puts_dis $chan $pc $cycles "[format {r%-2d=0x%08x} $regaddr $data]"]
	    } elseif {$datawr} {
	      set err [puts_dis $chan $pc $cycles "[format {*0x%08x=0x%08x} $addr $data]"]
	    } else {
	      set err [puts_dis $chan $pc $cycles]
	    }
	    if {$err} { break }
	  }
	}

	# Branch, Branch & Cycle Count
	if {$mode != 0} {
	  set pc 0
	  set line 0
	  set before_pc 1
	  set new_reg_value 0
	  set next_imm_valid 0
	  set next_imm 0x0000
	  set long_imm 0
	  set next_item ""
	  set at_dyn_ret 1
	  set at_first_pc 0
	  set branch_delay_slot 0
	  set loadget_delay_slot 0
	  set delayslot_pc 0x0
	  set item_branch_count 0
	  set saveld [expr ($control & 0x2) != 0]
	  while {$i < $count && ($line < $lines || $lines == -1)} {
	    if {$next_item == ""} {
	      set item [get_dis_item]
	    } else {
	      set item $next_item
	      set next_item ""
	    }
	    set kind [expr ($item >> 16) & 3]
	    switch $kind {
	      0 {
		set long_cycle_count 0
		if {$mode == 3} {
		  set branch_count [expr ($item & 0xC000) >> 14]
		  if {$branch_count == 3} {
		    set branch_count 1
		    set long_cycle_count 1
		  }
		  set max_branch_count 2
		} else {
		  set branch_count [expr ($item & 0xF000) >> 12]
		  set max_branch_count 12
		}
		if {$branch_count > $max_branch_count || ($output == 0 && $branch_count == 0)} {
		  error "Unexpected branch count $branch_count in read data [format {0x%05x} $item] (item $i of $count)"
		}
		set item_branch_count $branch_count
		if {$before_pc} { incr i ; continue }

		set n 0
		while {$n < $branch_count} {
		  set cycles ""
		  if {$mode == 3} {
		    if {$long_cycle_count} {
		      # Branch and long cycle count
		      set taken  [expr $item & 1]
		      set cycles [expr (($item >> 1) & 0x1FFF) + 1]
		    } else {
		      # Branch and short cycle count
		      set taken  [expr ($item >> (7 - $n * 7)) & 1]
		      set cycles [expr (($item >> (8 - $n * 8)) & 0x3F) + 1]
		    }
		  } else {
		    # Branch only
		    set taken  [expr ($item >> (11 - $n)) & 1]
		  }
		  set done 0
		  set at_dyn_ret [expr $mode == 2]
		  while {! $done} {
		    set instr [get_instr $pc]
		    set opcode [format {0x%02X} [expr $instr >> 26]]
		    switch $opcode {
		      0x2E -
		      0x2F {
			# Static branch
			set err [puts_dis $chan $pc $cycles]

			set delay_slot [expr ($opcode == 0x2E && (($instr & 0x00100000) != 0)) || \
					     ($opcode == 0x2F && (($instr & 0x02000000) != 0))]
			set absolute [expr $opcode == 0x2E && ($instr & 0x00080000) != 0]
			set imm [expr $instr & 0xFFFF]
			if {$opcode == 0x2E && ! $taken} {
			  error "Unconditional static branch in [format {0x%05x} $item] ($n) not taken, PC [format {0x%016x} $pc] (item $i of $count)"
			}

			if {$delay_slot} {
			  set delayslot_pc [expr $pc + 4]
			  set instr [get_instr $delayslot_pc]
			  set opcode [format {0x%02X} [expr $instr >> 26]]
			  set is_get  [expr $saveld && $opcode == 0x1B && ($instr & 0x8000) == 0]
			  set is_getd [expr $saveld && $opcode == 0x13 && ($instr & 0x0400) == 0]
			  set is_load [expr $saveld && ($opcode == 0x30 || $opcode == 0x31 || $opcode == 0x32 || \
							$opcode == 0x38 || $opcode == 0x39 || $opcode == 0x3A)]
			  set loadget_delay_slot [expr $is_get || $is_getd || $is_load]
			  set long_imm 0
			  if {! $loadget_delay_slot} {
			    set err [puts_dis $chan $delayslot_pc]
			  }
			  if {! $taken} { incr pc 8 }
			} elseif {! $taken} {
			  incr pc 4
			}

			if {$next_imm_valid > 0} {
			  # Immediate value - 24-bit (imml) sign extend
			  if {($imm & 0x800000) != 0} {
			    set imm [expr 0xFFFFFF0000000000 | $imm | $next_imm]
			  } else {
			    set imm [expr $imm | $next_imm]
			  }
			} elseif {($imm & 0x8000) != 0} {
			  set imm [expr 0xFFFF0000 | $imm]
			}
			if {$pc_items > 2} {
			  # Long branch
			  if {$taken &&   $absolute} { set pc [expr $imm & 0xFFFFFFFFFFFFFFFF] }
			  if {$taken && ! $absolute} { set pc [expr ($pc + $imm) & 0xFFFFFFFFFFFFFFFF] }
			} else {
			  if {$taken &&   $absolute} { set pc [expr $imm & 0xFFFFFFFF] }
			  if {$taken && ! $absolute} { set pc [expr ($pc + $imm) & 0xFFFFFFFF] }
			}

			set next_imm_valid 0
			set done 1
			incr line 2
		      }
		      0x26 -
		      0x27 {
			# Dynamic branch
			set err [puts_dis $chan $pc $cycles]
			incr pc 4
			set delay_slot [expr ($opcode == 0x26 && (($instr & 0x00100000) != 0)) || \
					     ($opcode == 0x27 && (($instr & 0x02000000) != 0))]
			if {$delay_slot} {
			  set err [puts_dis $chan $pc]
			  incr pc 4
			}
			if {$n != $branch_count - 1} {
			  error "Unexpected dynamic branch for [format {0x%05x} $item] ($n), PC [format {0x%016x} $pc] (item $i of $count)"
			}
			if {$opcode == 0x26 && ! $taken} {
			  error "Unconditional dynamic branch in [format {0x%05x} $item] ($n) not taken, PC [format {0x%016x} $pc] (item $i of $count)"
			}
			set next_imm_valid 0
			set done 1
			incr line 2
			set at_dyn_ret 1
		      }
		      0x2D {
			# Return
			set err [puts_dis $chan $pc $cycles]
			incr pc 4
			set err [puts_dis $chan $pc]
			incr pc 4
			if {$n != $branch_count - 1} {
			  error "Unexpected dynamic branch for [format {0x%05x} $item] ($n), PC [format {0x%016x} $pc] (item $i of $count)"
			}
			set next_imm_valid 0
			set done 1
			incr line 2
			set at_dyn_ret 1
		      }
		      0x2C {
			# Immediate value - 24-bit (imml) or 16-bit (imm)
			if {(($instr >> 24) & 3) == 2} {
			  set next_imm [expr ($instr & 0xFFFFFF) << 16]
			  set next_imm_valid 2
			} else {
			  set next_imm [expr ($instr & 0xFFFF) << 16]
			  set next_imm_valid 1
			}
			set err [puts_dis $chan $pc]
			incr pc 4
			incr line
		      }
		      default {
			set err [puts_dis $chan $pc]
			incr pc 4
			set next_imm_valid 0
			incr line
		      }
		    }
		    if {$err} { break }
		  }
		  if {$err} { break }
		  incr n
		}
	      }
	      1 {
		set new_pc [expr $item & 0xFFFF]
		for {set pc_item 2} {$pc_item <= $pc_items} {incr pc_item} {
		  incr i
		  set item [get_dis_item]
		  while {$i < $count && $item == 0} {
		    incr i
		    set item [get_dis_item]
		  }
		  set new_pc [expr ($new_pc << 16) | ($item & 0xFFFF)]
		  if {($item >> 16) != 1} {
		    error "Unexpected PC read data $pc_item [format {0x%05x} $item] (item $i of $count)"
		  }
		}
		if {$mode == 2} {
		  set err [puts_dis $chan $pc]
		}

		# Set current PC to first PC after continue execution
		if {$at_first_pc} {
		  set pc $new_pc
		  set at_first_pc 0
		}

		# Look ahead to check if the PC is for a debug stop event
		set next_item [get_dis_item]
		if {($next_item & 0x3C0FF) == 0x38001} {
		  set at_dyn_ret 1
		}

		# Check if PC is in already output delay slot
		if {$branch_delay_slot && $pc == $delayslot_pc} {
		  set at_dyn_ret 1
		  set branch_delay_slot 0
		}

		if {! $at_dyn_ret} {
		  # Output to return, dynamic branch, or load/get when enabled
		  set done 0
		  while {! $done && $new_pc != $pc} {
		    set instr [get_instr $pc]
		    set opcode [format {0x%02X} [expr $instr >> 26]]
		    switch $opcode {
		      0x2E -
		      0x2F {
			# Static branch
			error "Unexpected branch before PC read [format {0x%016x} $new_pc], PC [format {0x%016x} $pc] (item $i of $count)"
			set done 1
			incr line 2
		      }
		      0x26 -
		      0x27 {
			# Dynamic branch
			set err [puts_dis $chan $pc]
			incr pc 4
			incr line
			set delay_slot [expr ($opcode == 0x26 && (($instr & 0x00100000) != 0)) || \
					     ($opcode == 0x27 && (($instr & 0x02000000) != 0))]
			if {$delay_slot} {
			  set err [puts_dis $chan $pc]
			  set delayslot_pc $pc
			  set branch_delay_slot 1
			  incr pc 4
			  incr line
			}
			set done 1
		      }
		      0x2D {
			# Return
			set err [puts_dis $chan $pc]
			incr pc 4
			set err [puts_dis $chan $pc]
			set delayslot_pc $pc
			set branch_delay_slot 1
			incr pc 4
			set done 1
			incr line 2
		      }
		      0x2C {
			# Immediate value
			set long_imm [expr (($instr >> 24) & 3) == 2]
			set err [puts_dis $chan $pc]
			incr pc 4
			incr line
		      }
		      0x30 -
		      0x31 -
		      0x32 -
		      0x38 -
		      0x39 -
		      0x3A {
			# Load
			if {$saveld} { 
			  set done 1
			} else {
			  set err [puts_dis $chan $pc]
			  incr pc 4
			  incr line
			}
		      }
		      0x13 {
			# Dynamic get or put
			if {$saveld && ($instr & 0x0400) == 0} {
			  set done 1
			} else {
			  set err [puts_dis $chan $pc]
			  incr pc 4
			  incr line
			}
		      }
		      0x1B {
			# Get or put
			if {$saveld && ($instr & 0x8000) == 0} {
			  set done 1
			} else {
			  set err [puts_dis $chan $pc]
			  incr pc 4
			  incr line
			}
		      }
		      default {
			set err [puts_dis $chan $pc]
			incr pc 4
			incr line
		      }
		    }
		    if {$err} { break }
		  }
		}
		set at_dyn_ret [expr $mode == 2]
		set before_pc 0
		set pc $new_pc
	      }
	      2 {
		set new_reg_value [expr ($item & 0xFFFF) << 16]
		incr i
		set item [get_dis_item]
		while {$i < $count && $item == 0} {
		  incr i
		  set item [get_dis_item]
		}
		set new_reg_value [expr $new_reg_value | ($item & 0xFFFF)]
		if {($item >> 16) != 2} {
		  error "Unexpected load/get read data 2 [format {0x%05x} $item] (item $i of $count)"
		}

		# Output to load or get
		set done [expr $loadget_delay_slot || $mode == 2]
		while {! $done} {
		  set instr [get_instr $pc]
		  set opcode [format {0x%02X} [expr $instr >> 26]]
		  switch $opcode {
		    0x13 {
		      # Dynamic get or put
		      if {(instr & 0x0400) == 0} {
			set err [puts_dis $chan $pc "" "[format {=0x%08x} $new_reg_value]"]
			set done 1
		      } else {
			set err [puts_dis $chan $pc]
		      }
		      incr pc 4
		      incr line
		    }
		    0x1B {
		      # Get or put
		      if {(instr & 0x8000) == 0} {
			set err [puts_dis $chan $pc "" "[format {=0x%08x} $new_reg_value]"]
			set done 1
		      } else {
			set err [puts_dis $chan $pc]
		      }
		      incr pc 4
		      incr line
		    }
		    0x2E -
		    0x2F {
		      # Static branch
		      error "Unexpected static branch for load/get read [format {0x%05x} $item], PC [format {0x%016x} $pc] (item $i of $count)"
		      set done 1
		    }
		    0x26 -
		    0x27 {
		      # Dynamic branch
		      set err [puts_dis $chan $pc]
		      incr pc 4
		      incr line
		      set delay_slot [expr ($opcode == 0x26 && (($instr & 0x00100000) != 0)) || \
					   ($opcode == 0x27 && (($instr & 0x02000000) != 0))]
		      if {! $delay_slot} {
			error "Unexpected dynamic branch for load/get read [format {0x%05x} $item], PC [format {0x%016x} $pc] (item $i of $count)"
		      }
		      set next_imm_valid 0
		      set at_dyn_ret 1
		    }
		    0x2D {
		      # Return
		      set err [puts_dis $chan $pc]
		      incr pc 4
		      set next_imm_valid 0
		      incr line
		      set at_dyn_ret 1
		    }
		    0x2C {
		      # Immediate value
		      set long_imm [expr (($instr >> 24) & 3) == 2]
		      set err [puts_dis $chan "" $pc]
		      incr pc 4
		      incr line
		    }
		    0x30 -
		    0x31 -
		    0x32 -
		    0x38 -
		    0x39 -
		    0x3A {
		      # Load (lbu, lhu, lw, lbui, lhui, lwi) or load long (ll, lli)
		      set long [expr ($instr & 0x00000200) != 0 && $opcode == 0x32]
		      if {$long || $long_imm} {
			for {set load_item 3} {$load_item <= 4} {incr load_item} {
			  incr i
			  set item [get_dis_item]
			  while {$i < $count && $item == 0} {
			    incr i
			    set item [get_dis_item]
			  }
			  set new_reg_value [expr ($new_reg_value << 16) | ($item & 0xFFFF)]
			  if {($item >> 16) != 2} {
			    error "Unexpected load read data $load_item [format {0x%05x} $item] (item $i of $count)"
			  }
			}
			set err [puts_dis $chan $pc "" "[format {=0x%016x} $new_reg_value]"]
			set long_imm 0
		      } else {
			set err [puts_dis $chan $pc "" "[format {=0x%08x} $new_reg_value]"]
		      }
		      incr pc 4
		      incr line
		      set done 1
		    }
		    default {
		      set err [puts_dis $chan "" $pc]
		      incr pc 4
		      incr line
		    }
		  }
		  if {$err} { break }
		}
		if {$loadget_delay_slot} {
		  set err [puts_dis $chan $delayslot_pc "" "[format {=0x%08x} $new_reg_value]"]
		}
		set loadget_delay_slot 0
		if {$before_pc} { incr i ; continue }
	      }
	      3 {
		set event [expr $item & 0xFFFF]
		if {$event != 0x8001 && $event != 0x8002} {
		  puts_event $chan $event
		}
		if {($event & 0xC000) == 0xC000} {
		  set at_dyn_ret 1
		}
		if {($event & 0xC0FF) == 0x8002} {
		  set at_first_pc 1
		}
	      }
	      default {
		error "Unexpected read data: [format {0x%05x} $item] (item $i of $count)" 
	      }
	    }
	    if {$err} { break }
	    incr i
	  }
	}
	return $err
    }

    proc mbtrace_dis { {filename ""} {action ""} {lines -1} } {
	set chan stdout
	if {$filename != ""} {
	    if {[file exist $filename] && $action == ""} {
		error "Overwrite of existing file isn't enabled, use -force to overwrite, or -append to append data"
	    }
	    if {$action == "append"} {
		set chan [open $filename a]
	    } else {
		set chan [open $filename w]
	    }
	}
	mbtrace_dis_internal $chan $lines
	if {$filename != ""} {
	    close $chan
	}
    }

    #---------------------------------------------------------------------------------------#
    # MB Trace Continue
    # This routine continues execution until breakpoint while displaying embedded trace
    #---------------------------------------------------------------------------------------#
    proc mbtrace_continue { {filename ""} {action ""} } {
	variable trace_dict

	set output       [dict get $trace_dict output]
	set halt         [dict get $trace_dict halt]
	set control      [dict get $trace_dict ctrl_reg]
	set total_cycles [dict get $trace_dict cycles]

	if {![mbtrace_checkinit]} { return }

	if {$output > 0} {
	  error "External debug must not be enabled to use continue"
	}

	if {$halt == 0} {
	  # Set trace to halt when trace buffer full
	  set control [expr $control | 0x8]
	  mb_trace_write_control $control
	}

	set bp_addresses {}
	set chan [xsdb::getcurchan]
	set ctx  [xsdb::getcurtarget]
	dict for {key bpdata} $xsdb::bptable {
	  set enabled [xsdb::dict_get_safe $bpdata Enabled]
	  set ctxids [xsdb::dict_get_safe $bpdata ContextIds]
	  foreach ctxid $ctxids {
	    if {$ctxid == $ctx && $enabled} {
	      set id [xsdb::dict_get_safe $bpdata ID]
	      set status [xsdb::get_bp_status $id]
	      set sdata [lindex [lindex $status 1] 0]
	      set bp_address [dict get $sdata Address]
	      lappend bp_addresses $bp_address
	      break
	    }
	  }
	}
	if {[llength $bp_addresses] == 0} {
	  error "Must set at least one breakpoint or watchpoint to use continue"
	}

	set chan stdout
	if {$filename != ""} {
	  if {[file exist $filename] && $action == ""} {
	    error "Overwrite of existing file isn't enabled, use -force to overwrite, or -append to append data"
	  }
	  if {$action == "append"} {
	    set chan [open $filename a]
	  } else {
	    set chan [open $filename w]
	  }
	}

	while {1} {
	  con -block

	  set pc [lindex [rrd -format-result pc] 1]

	  # workaround to avoid issue with next con in the loop
	  stpi ; catch {stop}

	  set err [mbtrace_dis_internal $chan]
	  if {$err} { break }

	  set done [expr [lsearch -exact -integer $bp_addresses $pc] >= 0]
	  if {$done} { break }

	  mb_trace_write_control $control

	  set cmd_stop  2
	  set cmd_clear 8
	  mb_trace_write_command [expr $cmd_clear | $cmd_stop]
	  mb_trace_write_command $cmd_start

	  set total_cycles 0
	  dict set trace_dict cycles $total_cycles
	}

	if {$filename != ""} {
	  close $chan
	}
    }

    #---------------------------------------------------------------------------------------#
    # A short snippet for formatting code to binary
    #---------------------------------------------------------------------------------------#
    proc to_bin { {str} {binform} } {
	binary format $binform $str
    }

    #---------------------------------------------------------------------------------------#
    # A short snippet for formatting from binary to decimal
    #---------------------------------------------------------------------------------------#
    proc bin2dec { from } {
	set result 0
	foreach bit [split $from {}] {
	    set result [expr $result * 2 + $bit]
	}
	return $result
    }

    #---------------------------------------------------------------------------------------#
    # Code to read from MDM debug register with memory mapped access
    #---------------------------------------------------------------------------------------#
    proc mb_trace_mdm_mrd { mdmaddr ctrl } {
	configparam debug-poll-enable 0
	mwr [expr $mdmaddr + 0x10] $ctrl
	result = [mrd -value [expr $mdmaddr + 0x14]]
	configparam debug-poll-enable 1
	return $result
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to MDM debug register with memory mapped access
    #---------------------------------------------------------------------------------------#
    proc mb_trace_mdm_mwr { mdmaddr ctrl value } {
	configparam debug-poll-enable 0
	mwr [expr $mdmaddr + 0x10] $ctrl
	mwr [expr $mdmaddr + 0x14] $value
	configparam debug-poll-enable 1
    }

    #---------------------------------------------------------------------------------------#
    # Code to read from MicroBlaze configuration register
    # MDM Command - 0x07
    # Size - 288 bits to read extended and relevant 64-bit configuration
    # Dbg_ARADDR*4 - 0x41c0-0x41e0
    #---------------------------------------------------------------------------------------#
    proc mb_get_config { from {to -1} } {
	variable trace_dict

	if { $to == -1 } { set to $from }

	set mdmaddr [dict get $trace_dict mdmaddr]
        if {$mdmaddr == ""} {
	    set bscan [dict get $trace_dict bscan]
	    set which [dict get $trace_dict which]
	    set config [mb_drrd -user $bscan -which $which 0x07 288]
	    set config_hex [binary format {H72} [string range $config 2 end]]
	    binary scan $config_hex {B288} config_bin
	    return [bin2dec [string reverse [string range $config_bin $from $to]]]
	}

	set wordaddr [expr ($from / 32) * 4]
	set bitshift [expr 31 - $from % 32]
	set bitmask  [expr (1 << ($to - $from + 1)) - 1]
	set configword [mrd -value [expr $mdmaddr + 0x41c0 + $wordaddr]]
	return [expr ($configword >> $bitshift) & $bitmask]
    }

    #---------------------------------------------------------------------------------------#
    # Code to read from MicroBlaze trace status register
    # MDM Command - 0x63
    # Size - 18 bits
    # Dbg_ARADDR*4 - 0x58c0
    #---------------------------------------------------------------------------------------#
    proc mb_trace_read_status {} {
	variable trace_dict

	set mdmaddr [dict get $trace_dict mdmaddr]
        if {$mdmaddr == ""} {
	    set bscan [dict get $trace_dict bscan]
	    set which [dict get $trace_dict which]
	    return [mb_drrd -user $bscan -which $which 0x63 18]
	}
	return [mrd -value [expr $mdmaddr + 0x58c0]]
    }

    #---------------------------------------------------------------------------------------#
    # Code to get MDM configuration register
    #---------------------------------------------------------------------------------------#
    proc mb_trace_mdm_read_config {} {
	variable trace_dict
	return [expr [dict get $trace_dict mdmconfig] & 0xffffffff]
    }

    proc mb_trace_mdm_read_ext_config {} {
	variable trace_dict
	return [dict get $trace_dict mdmconfig]
    }

    #---------------------------------------------------------------------------------------#
    # Code to read from MDM external trace status register
    # MDM Command - 0x4a
    # Size - 3 bits
    # MDM DBG_CTRL - 0x69402
    #---------------------------------------------------------------------------------------#
    proc mb_trace_mdm_read_status {} {
	variable trace_dict

	set mdmaddr [dict get $trace_dict mdmaddr]
	if {$mdmaddr == ""} {
	    return [mdm_drrd 0x4a 3]
	}
	return [mb_trace_mdm_mrd $mdmaddr 0x69402]
    }

    #---------------------------------------------------------------------------------------#
    # Code to read from MDM external trace current address register
    # MDM Command - 0x4b
    # Size - 32 bits
    # MDM DBG_CTRL - 0x6961f
    #---------------------------------------------------------------------------------------#
    proc mb_trace_mdm_read_curraddr {} {
	variable trace_dict

	set mdmaddr [dict get $trace_dict mdmaddr]
	if {$mdmaddr == ""} {
	    return [mdm_drrd 0x4b 32]
	}
	return [mb_trace_mdm_mrd $mdmaddr 0x6961f]
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to trace control register
    # MDM Command - 0x61
    # Size - 22 bits
    # Dbg_AWADDR*4 - 0x5840
    #---------------------------------------------------------------------------------------#
    proc mb_trace_write_control { value } {
	variable trace_dict

	set mdmaddr [dict get $trace_dict mdmaddr]
	if {$mdmaddr == ""} {
	    set bscan [dict get $trace_dict bscan]
	    set which [dict get $trace_dict which]
	    mb_drwr -user $bscan -which $which 0x61 [format {0x%06x} $value] 22
	} else {
	    mwr [expr $mdmaddr + 0x5840] $value
	}
	dict set trace_dict ctrl_reg $value
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to trace command register
    # MDM Command - 0x62
    # Size - 4 bits
    # Dbg_AWADDR*4 - 0x5880
    #---------------------------------------------------------------------------------------#
    proc mb_trace_write_command { value } {
	variable trace_dict

	set mdmaddr [dict get $trace_dict mdmaddr]
	if {$mdmaddr == ""} {
	    set bscan [dict get $trace_dict bscan]
	    set which [dict get $trace_dict which]
	    mb_drwr -user $bscan -which $which 0x62 [format {0x%x} $value] 4
	} else {
	    mwr [expr $mdmaddr + 0x5880] $value
	}
	dict set trace_dict cmd_reg $value
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to MDM external trace low address register
    # MDM Command - 0x4c
    # Size - 16 bits
    # MDM DBG_CTRL - 0x6980f
    #---------------------------------------------------------------------------------------#
    proc mb_trace_mdm_write_low_addr { value } {
	variable trace_dict

	set mdmaddr [dict get $trace_dict mdmaddr]
	if {$mdmaddr == ""} {
	    mdm_drwr 0x4c [format {0x%04x} $value] 16
	} else {
	    mb_trace_mdm_mwr $mdmaddr 0x6980f $value
	}
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to MDM external trace high address register
    # MDM Command - 0x4d
    # Size - 16 bits
    # MDM DBG_CTRL - 0x69a0f
    #---------------------------------------------------------------------------------------#
    proc mb_trace_mdm_write_high_addr { value } {
	variable trace_dict

	set mdmaddr [dict get $trace_dict mdmaddr]
	if {$mdmaddr == ""} {
	    mdm_drwr 0x4d [format {0x%04x} $value] 16
	} else {
	    mb_trace_mdm_mwr $mdmaddr 0x69a0f $value
	}
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to MDM external trace control register
    # MDM Command - 0x4e
    # Size - 1 bit or 8 bits
    # MDM DBG_CTRL - 0x69c00 or 0x69c07
    #---------------------------------------------------------------------------------------#
    proc mb_trace_mdm_write_control { value size } {
	variable trace_dict

	set mdmaddr [dict get $trace_dict mdmaddr]
	if {$mdmaddr == ""} {
	    mdm_drwr 0x4e [format {0x%02x} $value] $size
	} else {
	    mb_trace_mdm_mwr $mdmaddr [expr ($size == 1) ? 0x69c00 : 0x69c07] $value
	}
    }
}

package provide xsdb::mbtrace $::xsdb::mbtrace::version
