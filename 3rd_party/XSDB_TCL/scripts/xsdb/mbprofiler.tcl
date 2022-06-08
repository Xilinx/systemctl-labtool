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

namespace eval ::xsdb::mbprofiler {
    variable version 0.1
    variable profile_dict [dict create]
    package require math::bignum
    #---------------------------------------------------------------------------------------#
    # MB Profiler Initialization
    # This routine is used to initialize the MB profiling
    #---------------------------------------------------------------------------------------#
    proc mbprof_set_config { config } {
	variable profile_dict
	set profile_dict $config
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Initialization
    # This routine is used to initialize the MB profiling
    #---------------------------------------------------------------------------------------#
    proc mbprof_init {} {
	variable profile_dict
	dict set profile_dict dirty_data 1

        # Determine MicroBlaze properties: bscan, which, mdmaddr, mdmconfig
	set chan [xsdb::getcurchan]
	set ctx [xsdb::getcurtarget]
	set props [xsdb::get_target_microblaze_props $chan $ctx]

	dict set profile_dict bscan ""
	if { [dict exists $props JtagChain] } {
	    dict set profile_dict bscan "[string range [dict get $props JtagChain] 4 end]"
	}

	if { [dict exists $props MBCore] && [dict get $props MBCore] >= 0 } {
	    dict set profile_dict which [dict get $props MBCore]
	} else {
	    error "Invalid target. Only supported for MicroBlaze targets"
	}

	dict set profile_dict mdmaddr ""
	if { [dict exists $props MDMAddr] } {
	    dict set profile_dict mdmaddr [dict get $props MDMAddr]
	}

	dict set profile_dict mdmconfig [dict get $props MDMConfig]

        # Determine MDM property: dbg_ports
	set config_mdm [expr [dict get $profile_dict mdmconfig] & 0xffffffff]
	set config_mdm_mb_dbg_ports [expr (($config_mdm >> 8) & 0xff) > 1]
	dict set profile_dict dbg_ports $config_mdm_mb_dbg_ports

	# Configuration Register Read
	set config_extended_debug [mb_get_config 161]
	set config_profile_size [mb_get_config 272 274] ; # 1=4096, 2=8192, ...
	set config_addr_size    [mb_get_config 276 281]
	set config_data_size_64 [mb_get_config 282]

	if {$config_extended_debug == 0} {
	    error "Profiling is not enabled in the design. Enable Extended Debug in MicroBlaze configuration"
	} elseif {$config_profile_size == 0} {
	    error "Profiling is not enabled in the design. Set Profile Size in MicroBlaze configuration"
	} else {
	    set mem_bits [expr $config_profile_size + 9]
	    set mem_words [expr 1 << $mem_bits]
	    set addr_bits [expr $config_data_size_64 ? $config_addr_size + 32 : 32]
	    dict set profile_dict mem_bits $mem_bits
	    dict set profile_dict mem_words $mem_words
	    dict set profile_dict addr_bits $addr_bits
	    dict set profile_dict init_done 1
	    dict set profile_dict ctrl_reg 0
	    dict set profile_dict clear_mem 1
	    if { [dict get $profile_dict cumulate] } {
		dict set profile_dict clear_mem 0
	    }
	    mbprof_clear_mem
	    mbprof_set
	}
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Clear
    # This routine is used to clear the Profiler memory
    #---------------------------------------------------------------------------------------#
    proc mbprof_clear_mem {} {
	variable profile_dict

	if {![mbprof_checkinit]} { return }

	if {![dict get $profile_dict clear_mem]} { return }

	# Must stop profiling in order to write data
	mbprof_write c [expr ([dict get $profile_dict ctrl_reg] & 0x3f) | 0x40]

	# Set ram to 0
	mbprof_write b 0
	set mdmaddr [dict get $profile_dict mdmaddr]
	set mem_words [dict get $profile_dict mem_words]
	if {$mdmaddr == ""} {
	    set bscan [dict get $profile_dict bscan]
	    set dbg_ports [dict get $profile_dict dbg_ports]
	    set which [dict get $profile_dict which]

	    set command 0x77
	    set len 32
	    set res [::xsdb::to_bits 0x0 $len]

	    set seqname [jtag sequence]
	    $seqname irshift -state IRUPDATE -register bypass
	    $seqname irshift -state IDLE -register user$bscan
	    $seqname drshift -state DRUPDATE -int 4 1
	    # Set MDM Which MB register to current target, if more than one MicroBlaze
	    if {$dbg_ports > 1} {
		set len [expr $dbg_ports > 8 ? $dbg_ports : 8]
		$trace_read_seq drshift -state DRUPDATE -int 8 0x0d
		$trace_read_seq drshift -state DRUPDATE -int $len [expr 1 << $which]
	    }
	    for {set i 0} {$i < $mem_words - 1} {incr i} {
		$seqname drshift -state DRUPDATE -int 8 $command
		$seqname drshift -state DRUPDATE -bits $len $res
	    }
	    $seqname drshift -state DRUPDATE -int 8 $command
	    $seqname drshift -state IDLE -bits $len $res
	    $seqname run -current-target
	    $seqname delete
	} else {
	    for {set i 0} {$i < $mem_words} {incr i} {
		mwr [expr $mdmaddr + 0x5DC0] 0
	    }
	}
	dict set profile_dict clear_mem 0
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Check init
    # This routine is used to check if profiling is initialized
    #---------------------------------------------------------------------------------------#
    proc mbprof_checkinit {} {
	variable profile_dict

	if {[dict get $profile_dict init_done] == 1} {
	    return 1
	}
	error "Profiling not configured, please configure using command mbprofile"
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Write Register
    # This routine is used to write
    #---------------------------------------------------------------------------------------#
    proc mbprof_write {reg {value 0}} {
	variable profile_dict

	if {![mbprof_checkinit]} { return }

	if {[string first "c" $reg] != -1} {
	    # control register
	    mb_prof_write_control $value
	    dict set profile_dict ctrl_reg $value
	}
	if {[string first "l" $reg] != -1} {
	    # profiling low address register
	    set addr_bits [dict get $profile_dict addr_bits]
	    mb_prof_write_low_addr [expr $value >> 2] [expr $addr_bits - 2]
	}
	if {[string first "h" $reg] != -1} {
	    # profiling high address register
	    set addr_bits [dict get $profile_dict addr_bits]
	    mb_prof_write_high_addr [expr $value >> 2] [expr $addr_bits - 2]
	}
	if {[string first "b" $reg] != -1} {
	    # profiling buffer address register
	    mb_prof_write_buf_addr $value [dict get $profile_dict mem_bits]
	}
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Set
    # This routine is used to set profiler parameters and start profiling
    #---------------------------------------------------------------------------------------#
    proc mbprof_set {} {
	variable profile_dict

	if {![mbprof_checkinit]} { return }

	set low_addr [dict get $profile_dict low_addr]
	set high_addr [dict get $profile_dict high_addr]
	set mem_words [dict get $profile_dict mem_words]
	set cnt_instr [dict get $profile_dict cnt_instr]
	set use_cycles 0

	if { $low_addr >= $high_addr } {
	    error "High address can't be same or smaller than low address"
	}

	dict set profile_dict dirty_data 1

	set offset [expr ($high_addr - $low_addr + (4 * $mem_words - 1)) / (4 * $mem_words)]
	set bin [expr int(ceil(log($offset) / log(2))) ]
	set mem_addr_bits [expr 2 + int(ceil(log($mem_words) / log(2))) ]

	# Need to calculate the actual low_addr that plist will be using for
	# getting correct offsets into the BRAM
	if { $bin > 20 } {
	    set real_low_addr 0
	    set real_low_addrmask 0xffffffff
	} else {
	    set real_low_addrmask [expr (1 << ($mem_addr_bits + $bin)) - 1]
	    set real_low_addr [expr $low_addr & ~$real_low_addrmask]
	    # Need to detect if there is a rounding error if low addr start very close to end of offset
	    while {$real_low_addr + ($mem_words * (1 << $bin) * 4) < $high_addr} {
		incr bin
		set real_low_addrmask [expr (1 << ($mem_addr_bits+$bin)) - 1]
		set real_low_addr [expr $low_addr & ~$real_low_addrmask ]
	    }
	}

	dict set profile_dict profiler_bin $bin

	mbprof_write l $low_addr
	mbprof_write h $high_addr
	if { $cnt_instr == 0 } {
	    set use_cycles 1
	}
	mbprof_write c [expr ($bin & 0x1f) + ($use_cycles << 5)]
	set real_high_addr [expr $real_low_addr+$real_low_addrmask]
	set binsize [expr 1 << $bin]
	if { $cnt_instr == 0 } {
	    set fmt "Profiler: \n\tFull range  = 0x%08x-0x%08x\n\tTrace range = 0x%08x-0x%08x\n\tBin size    = %d instructions\n\tCounting Cycles"
	} else {
	    set fmt "Profiler: \n\tFull range  = 0x%08x-0x%08x\n\tTrace range = 0x%08x-0x%08x\n\tBin size    = %d instructions\n\tCounting Instructions"
	}
	puts [format $fmt $real_low_addr $real_high_addr $low_addr $high_addr $binsize]
	dict set profile_dict real_low_addr  $real_low_addr
	dict set profile_dict real_high_addr $real_high_addr
	return ""
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Start
    # This routine is used to turn on profiling
    #---------------------------------------------------------------------------------------#
    proc mbprof_start {} {
	variable profile_dict

	if {![mbprof_checkinit]} { return }
	mbprof_clear_mem
	dict set profile_dict clear_mem 1
	if { [dict get $profile_dict cumulate] } {
	    dict set profile_dict clear_mem 0
	}
	dict set profile_dict dirty_data 1
	mbprof_write c [expr ([dict get $profile_dict ctrl_reg] & 0x3f) | 0x80]
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Stop
    # This routine is used to turn off profiling
    #---------------------------------------------------------------------------------------#
    proc mbprof_stop { } {
	variable profile_dict

	if {![mbprof_checkinit]} { return }
	mbprof_write c [expr ([dict get $profile_dict ctrl_reg] & 0x3f) | 0x40]
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Output
    # This routine is used to save profile data to a gmon.out file
    #---------------------------------------------------------------------------------------#
    proc mbprof_gmonout { outfile } {
	variable profile_dict
	set nr_runs 1
	set accuraccy 1000000

	if {![mbprof_checkinit]} { return }

	set freq [dict get $profile_dict freq]
	set mem_words [dict get $profile_dict mem_words]
	set real_low_addr [dict get $profile_dict real_low_addr]
	set real_high_addr [dict get $profile_dict real_high_addr]
	set cnt_instr [dict get $profile_dict cnt_instr]

	# Stop profiling before reading data
	mbprof_stop

	# Read out all profiling data (36 bits/item)
	read_prof_data
	set prof_list_unscaled [dict get $profile_dict raw_data]

	# If a bin value is larger than $nr_runs*65536 (4194240) we need to make it smaller.
	# To maintain ratios, we make everything equally smaller. This will of course reduce accuraccy
	# but specifying the $accuraccy argument higher reduces this at a cost of processing.
	# To simplify things with bignum, we multiply by accuraccy here, and if we do we set the flag
	# need_accuraccy to 1, so we divide by it when we read the bignum value.
	set needs_accuraccy 0
	set largest [find_largest $prof_list_unscaled]

	if {$largest > [expr $nr_runs * 65536] } {
	    puts "Info: A bin has a value higher than [expr $nr_runs * 65536]. Scaling to fit."
	    puts "Info: To improve accuraccy when using command line gprof increase number of runs."
	    set needs_accuraccy 1
	    set scaler [expr ($nr_runs * 65536 * $accuraccy)/$largest]
	    # We calculate the scale as a number between 0.0 and 1.0, and use multiplication instead of division
	    set scaled_hertz [expr $scaler * $freq / $accuraccy]
	    set bignum_scaler [::math::bignum::fromstr $scaler]
	    set prof_list {}
	    foreach {old_value} $prof_list_unscaled {
	      set new_value [::math::bignum::mul $old_value $bignum_scaler]
	      lappend prof_list $new_value
	    }
	} else {
	    # If we do not need to scale, the used variables still get updated to clarify code further down
	    set scaled_hertz $freq
	    set prof_list $prof_list_unscaled
	}

	# Open file and prepare binary
	set outfileptr [open $outfile w]
	fconfigure $outfileptr -translation binary
	set b16 s1
	set b32 i1

	# Create Header
	# Format
	puts -nonewline $outfileptr "gmon"
	# Version
	puts -nonewline $outfileptr [to_bin 1 $b32]

	# Trailing space
	for {set i 0} {$i < 12} {incr i} {
	  puts -nonewline $outfileptr [binary format c 0]
	}
	# Create histogram
	# 00 is Histogram
	puts -nonewline $outfileptr [binary format c 0]

	# Output address range
	puts -nonewline $outfileptr [to_bin $real_low_addr $b32]
	puts -nonewline $outfileptr [to_bin [expr $real_high_addr + 1 ] $b32]
	puts -nonewline $outfileptr [to_bin $mem_words $b32]

	# Output frequency in Hz
	puts -nonewline $outfileptr [to_bin $scaled_hertz $b32]

	# Output dimension which uses 15 chars
	puts -nonewline $outfileptr "seconds"
	for {set i 0} {$i < 8} {incr i} {
	    puts -nonewline $outfileptr [binary format c 0]
	}

	# Output abbreviation for dimension
	puts -nonewline $outfileptr "s"

	# Used to handle overcarry values
	set cindex {}
	set cvalue {}

	# Write our values
	for {set i 0} {$i < $mem_words} {incr i} {
	    set bignum_value [lindex $prof_list $i]
	    set value [::math::bignum::tostr $bignum_value 10]
	    if {$needs_accuraccy == 1} {
		set value [expr $value/$accuraccy]
	  }
	  # If a value is larger than 16 bits, we fill it with 0xffff and reduce it by 0xffff
	  if { $value > 65535 } {
		lappend cindex $i
		lappend cvalue [expr $value - 65535]
		puts -nonewline $outfileptr [to_bin 65535 $b16]
	  } else {
		puts -nonewline $outfileptr [to_bin $value $b16]
	  }
	}
	set run_count 0
	set run_append_hist 1
	# If a single value > 0xFFFF we need to append several new histograms with the difference
	# We only do this a maximum of $run_count times to stop infinite loops and keep filesize reasonable
	while { $run_append_hist != 0 && $run_count < $nr_runs && [llength $cindex] > 0 && [llength $cvalue] > 0 } {
	    incr run_count
	    # Unless there is further carry over this is the last time we run the while loop
	    set run_append_hist 0

	    # Create histogram
	    # 00 is Histogram
	    puts -nonewline $outfileptr [binary format c 0]

	    # Output address range
	    puts -nonewline $outfileptr [to_bin $real_low_addr $b32]
	    puts -nonewline $outfileptr [to_bin [expr $real_high_addr + 1 ] $b32]
	    puts -nonewline $outfileptr [to_bin $mem_words $b32]

	    # Output frequency in Hz
	    puts -nonewline $outfileptr [to_bin $scaled_hertz $b32]

	    # Output dimension which uses 15 chars
	    puts -nonewline $outfileptr "seconds"
	    for {set i 0} {$i < 8} {incr i} {
		puts -nonewline $outfileptr [binary format c 0]
	    }

	    # Output abbreviation for dimension
	    puts -nonewline $outfileptr "s"

	    # Output the profiling data
	    for {set i 0} {$i < $mem_words} {incr i} {
		set idx [lsearch $cindex $i]
		# If the adress hasn't carried over, we keep it at 0x0000
		if { $idx == -1} {
		    puts -nonewline $outfileptr [to_bin 0000 $b16]
		} else {
		    if { [lindex $cvalue $idx] > 65535 } {
			# There is further carry over, and we need to append another histogram
			puts -nonewline $outfileptr [to_bin 65535 $b16]
			set run_append_hist 1
			set cvalue [lreplace $cvalue $idx $idx [expr [lindex $cvalue $idx ] - 65535 ] ]
		    } else {
			# This is last time address carried over, therefore we delete the elements from our lists
			puts -nonewline $outfileptr [to_bin [lindex $cvalue $idx ] $b16]
			set cvalue [lreplace $cvalue $idx $idx ]
			set cindex [lreplace $cindex $idx $idx ]
		    }
		}
	    }
	}
	close $outfileptr
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Disassemble & Annotate
    # This routine is used to disassemble and annotate
    #---------------------------------------------------------------------------------------#
    proc mbprof_disassembly_annotate { filename {usetime 1} {source 1} } {
	variable profile_dict

	if {![mbprof_checkinit]} { return }

	set elf_file [dict get $profile_dict elf]
	set mem_words [dict get $profile_dict mem_words]
	set low_addr [dict get $profile_dict low_addr]
	set high_addr [dict get $profile_dict high_addr]
	set real_low_addr [dict get $profile_dict real_low_addr]
	set real_high_addr [dict get $profile_dict real_high_addr]
	set profiler_bin [dict get $profile_dict profiler_bin]
	set cnt_instr [dict get $profile_dict cnt_instr]
	set freq [dict get $profile_dict freq]

	set res ""

	if {$source == 1} {
	    set dump_opt "-S"
	} else {
	    set dump_opt "-d"
	}

	if {$elf_file != ""} {
	    set pgm mb-objdump
	    set cmd [list exec $pgm $dump_opt $elf_file]
	    if {[catch $cmd res] != 0} {
		error "mb-objdump returned with error code"
	    }

	    set dis_line [split $res "\n"]
	    set outfileptr [open $filename w]

	    # Must stop profiling in order to read data
	    mbprof_stop
	    read_prof_data
	    set raw_data [dict get $profile_dict raw_data]
	    set data {}
	    set bignum_total [::math::bignum::zero]

	    for {set i 0} {$i < $mem_words} {incr i} {
		set bignum_value [lindex $raw_data $i]
		set bignum_total [::math::bignum::add $bignum_total $bignum_value]
		set firstaddr   [expr $real_low_addr + $i * (1 << $profiler_bin) * 4]
		set secondaddr  [expr $real_low_addr + ($i + 1) * (1 << $profiler_bin) * 4 - 4]
		set dataitem " $firstaddr $secondaddr [list $bignum_value]"
		lappend data $dataitem
	    }

	    set fmt      "%12s"
	    set fmt_nsec "%07.3f ns  "
	    set fmt_usec "%07.3f us  "
	    set fmt_msec "%07.3f ms  "
	    set fmt_sec  "%07.3f s   "

	    set nsec_per_clock [expr 1.0E9 / $freq]

	    foreach line $dis_line {
		if {[regexp -nocase {^ *([0-9a-f]+):} $line match pcaddr]} {
		    if {("0x$pcaddr" >= $low_addr) && ("0x$pcaddr" <= $high_addr) } {
			set found [lsearch -exact -integer -index 0 $data 0x$pcaddr]
			if {($found != -1)} {
			    set bignum_value [lindex $data $found 2]
			    set bignum_str   [::math::bignum::tostr $bignum_value 10]
			    if {$usetime == 0} {
				puts -nonewline $outfileptr [format $fmt $bignum_str]
			    } else {
				set exectime [expr $nsec_per_clock * $bignum_str]
				if {$exectime < 1000.0} {
				    puts -nonewline $outfileptr [format $fmt_nsec $exectime]
				} elseif  {$exectime < 1000000.0} {
				    puts -nonewline $outfileptr [format $fmt_usec [expr $exectime / 1000.0]]
				} elseif  {$exectime < 1000000000.0} {
				    puts -nonewline $outfileptr [format $fmt_msec [expr $exectime / 1000000.0]]
				} else {
				    puts -nonewline $outfileptr [format $fmt_sec [expr $exectime / 1000000000.0]]
				}
			    }
			    puts $outfileptr $line
			} else {
			    set pcrangeaddr [expr 0x$pcaddr & ~( ((1 << $profiler_bin) * 4) - 1)]
			    set found_range [lsearch -exact -integer -index 0 $data $pcrangeaddr]
			    if {($found_range != -1)} {
				puts -nonewline $outfileptr "||          "
			    } else {
				puts -nonewline $outfileptr "            "
			    }
			    puts $outfileptr $line
			}
		    } else {
			# outside profiling address range, just copy the line
			puts $outfileptr $line
		    }
		} else {
		    # no pc address so just copy the line
		    puts $outfileptr $line
		}
	    }
	    if { $cnt_instr == 0 } {
		set fmt "Total cycles %s - Total execution time %s msec at %d MHz"
		set millisec_scale [::math::bignum::fromstr [expr ($freq / 1000)]]
		set freq_mhz [expr ($freq / 1000000)]
		set exec_time [::math::bignum::tostr [::math::bignum::div $bignum_total $millisec_scale]]
		puts [format $fmt [::math::bignum::tostr $bignum_total 10] $exec_time $freq_mhz]
	    } else {
		puts "Total instructions: [::math::bignum::tostr $bignum_total 10]"
	    }
	    close $outfileptr
	} else {
	    error "elf-file is undefined"
	}
    }

    #---------------------------------------------------------------------------------------#
    # MB Profiler Read Profiled Data
    # This routine is used to read all data from profiling buffer
    #---------------------------------------------------------------------------------------#
    proc read_prof_data {} {
	variable profile_dict

	set dirty_data [dict get $profile_dict dirty_data]

	if {$dirty_data == 0} {
	    return [dict get $profile_dict raw_data]
	} else {
	    mbprof_write b 0
	    set prof_list_raw {}
	    set mdmaddr [dict get $profile_dict mdmaddr]
	    set mem_words [dict get $profile_dict mem_words]
	    if {$mdmaddr == ""} {
		set bscan [dict get $profile_dict bscan]
		set dbg_ports [dict get $profile_dict dbg_ports]
		set which [dict get $profile_dict which]

		set command 0x76
		set len 36

		set seqname [jtag sequence]
		$seqname irshift -state IRUPDATE -register bypass
		$seqname irshift -state IDLE -register user$bscan
		$seqname drshift -state DRUPDATE -int 4 1
		# Set MDM Which MB register to current target, if more than one MicroBlaze
		if {$dbg_ports > 1} {
		set len [expr $dbg_ports > 8 ? $dbg_ports : 8]
		    $trace_read_seq drshift -state DRUPDATE -int 8 0x0d
		    $trace_read_seq drshift -state DRUPDATE -int $len [expr 1 << $which]
		}
		for {set i 0} {$i < $mem_words - 1} {incr i} {
		    $seqname drshift -state DRUPDATE -int 8 $command
		    $seqname drshift -state DRUPDATE -tdi 0 -capture $len
		}
		$seqname drshift -state DRUPDATE -int 8 $command
		$seqname drshift -state IDLE -tdi 0 -capture $len
		set res [$seqname run -current-target -bits]
		$seqname delete

		foreach r $res {
		    set r [xsdb::to_hex $r $len]
		    lappend prof_list_raw $r
		}
	    } else {
		for {set i 0} {$i < $mem_words} {incr i} {
		    set datalow  [mrd -value [expr $mdmaddr + 0x5D80]]
		    set datahigh [mrd -value [expr $mdmaddr + 0x5D84]]
		    lappend prof_list_raw [format {0x%09x} [expr ($datahigh << 32) | $datalow]]
		}
	    }

	    set prof_list {}
	    foreach item $prof_list_raw {
		set msw "[string range $item 0 2]"
		set lsw "0x[string range $item 3 10]"
		set bignum_lsw    [::math::bignum::fromstr $lsw]
		set bignum_msw    [::math::bignum::fromstr $msw]
		set bignum_msw_sh [::math::bignum::lshift $bignum_msw 32]
		set bignum_value  [::math::bignum::add $bignum_msw_sh $bignum_lsw]
		lappend prof_list $bignum_value
	    }
	    dict set profile_dict raw_data $prof_list
	    dict set profile_dict dirty_data 0
	}
    }

    #---------------------------------------------------------------------------------------#
    # Find and return largest bignum in a bignum list
    #---------------------------------------------------------------------------------------#
    proc find_largest {{ls}} {
	set largest 0
	set largest_bn 0
	for {set i 0} {$i < [llength $ls]} {incr i} {
	    set bignum_value [lindex $ls $i]
	    set value [::math::bignum::tostr $bignum_value 10]
	    if {$value > $largest} {
		set largest_bn $bignum_value
		set largest $value
	    }
	}
	return $largest
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
    # Code to read from MDM configuration register
    #---------------------------------------------------------------------------------------#
    proc mb_get_config {from {to -1}} {
	variable profile_dict

	if { $to == -1 } { set to $from	}

	set mdmaddr [dict get $profile_dict mdmaddr]
	if {$mdmaddr == ""} {
	    set bscan [dict get $profile_dict bscan]
	    set which [dict get $profile_dict which]
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
    # Code to write to profiling control register
    # MDM Command - 0x71
    # Size - 8 bits
    # Dbg_AWADDR*4 - 0x5C40
    #---------------------------------------------------------------------------------------#
    proc mb_prof_write_control { value } {
	variable profile_dict

	set mdmaddr [dict get $profile_dict mdmaddr]
	if {$mdmaddr == ""} {
	    set bscan [dict get $profile_dict bscan]
	    set which [dict get $profile_dict which]
	    mb_drwr -user $bscan -which $which 0x71 [format {0x%08x} $value] 8
	} else {
	    mwr [expr $mdmaddr + 0x5C40] $value
	}
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to profiler low address register
    # MDM Command - 0x72
    # Size - C_ADDR_SIZE-2 bits
    # Dbg_AWADDR*4 - 0x5C80, and 0x5C84 if len > 30
    #---------------------------------------------------------------------------------------#
    proc mb_prof_write_low_addr { value {len 30}} {
	variable profile_dict

	set mdmaddr [dict get $profile_dict mdmaddr]
	if {$mdmaddr == ""} {
	    set bscan [dict get $profile_dict bscan]
	    set which [dict get $profile_dict which]
	    set nibbles [expr ($len + 3) / 4]
	    mb_drwr -user $bscan -which $which 0x72 [format "0x%0${nibbles}x" $value] $len
	} else {
	    mwr [expr $mdmaddr + 0x5C80] [expr $value & 0x7FFFFFFF]
	    if {$len > 30} {
		mwr [expr $mdmaddr + 0x5C84] [expr $value >> 30]
	    }
	}
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to profiler high address register
    # MDM Command - 0x73
    # Size - C_ADDR_SIZE-2 bits
    # Dbg_AWADDR*4 - 0x5CC0, and 0x5CC4 if len > 30
    #---------------------------------------------------------------------------------------#
    proc mb_prof_write_high_addr {value {len 30}} {
	variable profile_dict

	set mdmaddr [dict get $profile_dict mdmaddr]
	if {$mdmaddr == ""} {
	    set bscan [dict get $profile_dict bscan]
	    set which [dict get $profile_dict which]
	    set nibbles [expr ($len + 3) / 4]
	    mb_drwr -user $bscan -which $which 0x73 [format "0x%0${nibbles}x" $value] $len
	} else {
	    mwr [expr $mdmaddr + 0x5CC0] [expr $value & 0x7FFFFFFF]
	    if {$len > 30} {
		mwr [expr $mdmaddr + 0x5CC4] [expr $value >> 30]
	    }
	}
    }

    #---------------------------------------------------------------------------------------#
    # Code to write to profiler buffer address register
    # MDM Command - 0x74
    # Size - Depends on bin size
    # Dbg_AWADDR*4 - 0x5D00
    #---------------------------------------------------------------------------------------#
    proc mb_prof_write_buf_addr { value {len 10} } {
	variable profile_dict

	set mdmaddr [dict get $profile_dict mdmaddr]
	if {$mdmaddr == ""} {
	    set bscan [dict get $profile_dict bscan]
	    set which [dict get $profile_dict which]
	    mb_drwr -user $bscan -which $which 0x74 [format {0x%08x} $value] $len
	} else {
	    mwr [expr $mdmaddr + 0x5D00] $value
	}
    }
}

package provide xsdb::mbprofiler $::xsdb::mbprofiler::version
