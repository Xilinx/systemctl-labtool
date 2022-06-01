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

namespace eval ::xsdb::gprof {
    variable version 0.1
    variable gmonparamstruct ""
    variable profile_enabled 0
    variable prof_addr ""
    variable elf ""
    variable bigendian 0
    variable ngmonsections ""
    variable histbinsize 4
    variable gmonparamoffsets [dict create GPARAM_START 0\
			       GPARAM_HIST_O 4\
			       GPARAM_HISTSIZE_O 8\
			       GPARAM_CG_FROM_O 12\
			       GPARAM_CG_FROMSIZE_O 16\
			       GPARAM_CG_TO_O 20\
			       GPARAM_CG_TOSIZE_O 24\
			       GPARAM_LOWPC_O 28\
			       GPARAM_HIGHPC_O 32\
			       GPARAM_TEXTSIZE_O 36\
			       GPARAM_SIZE 40]

    #---------------------------------------------------------------------------------------#
    # Get the offsets of gmonparam
    #---------------------------------------------------------------------------------------#
    proc get_gmonparam_offsets {} {
	variable gmonparamoffsets
	return $gmonparamoffsets
    }

    #---------------------------------------------------------------------------------------#
    # Get the gmon param structure
    #---------------------------------------------------------------------------------------#
    proc get_gmonparamstruct {} {
	variable gmonparamstruct
	return $gmonparamstruct
    }

    #---------------------------------------------------------------------------------------#
    # Enable/disable the profiling
    #---------------------------------------------------------------------------------------#
    proc enable_profiling { } {
	variable profile_enabled
	set profile_enabled 1
    }

    #---------------------------------------------------------------------------------------#
    # Check if profile is initialized and enabled
    #---------------------------------------------------------------------------------------#
    proc is_profiling_enabled { } {
	variable profile_enabled
	return $profile_enabled
    }

    #---------------------------------------------------------------------------------------#
    # Set the elf associated with current profiling
    #---------------------------------------------------------------------------------------#
    proc set_profile_elf { file } {
	variable elf
	variable ngmonsections
	variable gmonparamstruct
	variable prof_addr
	variable bigendian

	set elf [::xsdb::elf::open $file]
	set ngmonsections 0
	set gmonparamstruct ""
	set prof_addr ""
	set bigendian ""
    }

    #---------------------------------------------------------------------------------------#
    # Set the endianness
    #---------------------------------------------------------------------------------------#
    proc set_prof_endianness { val } {
	variable bigendian
	set bigendian $val
    }

    #---------------------------------------------------------------------------------------#
    # Get the endianness
    #---------------------------------------------------------------------------------------#
    proc get_prof_endianness { } {
	variable bigendian
	return $bigendian
    }

    #---------------------------------------------------------------------------------------#
    # Check if profile is enable in elf
    #---------------------------------------------------------------------------------------#
    proc is_elf_prof_enabled {elf} {
	if { [$elf get_sym_addr "_mcount"] != "" || [$elf get_sym_addr "__gnu_mcount_nc"] != "" } {
	    return 1
	}
        return 0
    }

    #---------------------------------------------------------------------------------------#
    # Check profile version
    #---------------------------------------------------------------------------------------#
    proc get_prof_version { } {
	variable elf
	variable bigendian
	# Check profile version
	if { [$elf get_sym_val "profile_version" $bigendian] == 0 } {
	    return 0
	}
	return 1
    }

    #---------------------------------------------------------------------------------------#
    # Extract and store addresses of profile related variables
    # Extract address of all the profile related symbols from elf
    #---------------------------------------------------------------------------------------#
    proc get_prof_addresses { } {
	variable elf
	variable prof_addr
	dict set prof_addr sampfreq [$elf get_sym_addr "sample_freq_hz"]
	dict set prof_addr binsize [$elf get_sym_addr "binsize"]
	dict set prof_addr timerticks [$elf get_sym_addr "timer_clk_ticks"]
	dict set prof_addr cpufreq [$elf get_sym_addr "cpu_clk_freq"]
	dict set prof_addr ngmonsecs [$elf get_sym_addr "n_gmon_sections"]
	dict set prof_addr gmonparam [$elf get_sym_addr "_gmonparam"]
	return $prof_addr
    }

    #---------------------------------------------------------------------------------------#
    # Get CPU frequeny
    #---------------------------------------------------------------------------------------#
    proc get_prof_cpufreq { } {
	variable elf
	variable bigendian
	return [$elf get_sym_val "cpu_clk_freq" $bigendian]
    }

    #---------------------------------------------------------------------------------------#
    # Get no. of gmon sections
    #---------------------------------------------------------------------------------------#
    proc get_no_of_gmon_sections { } {
	variable ngmonsections
	return $ngmonsections
    }

    #---------------------------------------------------------------------------------------#
    # Extract executable sections & sort them
    #---------------------------------------------------------------------------------------#
    proc get_sorted_exec_sections { } {
	variable elf
	variable ngmonsections
	variable gmonparamstruct

	# Extract only executable sections
	set exec_sections [$elf get_exec_sections]

	# Sort profile sections
	foreach exsec $exec_sections {
	    set retsec [sort_sections $exsec]
	    if { $retsec != "-1"} {
		lappend sortedsec $retsec
	    }
	}

	if {$ngmonsections == 0} {
	    error "program cannot be profiled - no code sections found"
	}

	set code_highpc [dict get [lindex $sortedsec [expr $ngmonsections - 1]] highpc]
	set code_lowpc [dict get [lindex $sortedsec 0] lowpc]
	set code_size [expr $code_highpc - $code_lowpc]

	if {$code_size > 0x100000} {
	    error "code sections are located far apart, which results in huge profile output file"
	}
	return $sortedsec
    }

    #---------------------------------------------------------------------------------------#
    # Sort the sections as per address and discard some sections
    #---------------------------------------------------------------------------------------#
    proc sort_sections {sec} {
	variable ngmonsections
	variable histbinsize
	set discard_sections [list ".vectors.reset" ".vectors.sw_exception" ".vectors.interrupt" ".vectors.hw_exception" ".init" ".fini"]

	set lowpc [dict get $sec addr]
	set size [dict get $sec size]
	set secname [dict get $sec name]
	set highpc [expr $lowpc + $size]

	# If section is very small (< 10 instructions), do not profile
	# Min profile section size = 10 inst * 4 bytes per inst
	if { $size < [expr 10 * 4] } {
	    return "-1"
	}

	# Exclude all discarded sections
	foreach dissec $discard_sections {
	    if { $secname == $dissec } {
		return "-1"
	    }
	}

	# Rounding off
	set t_down [rounddown $highpc [expr $histbinsize * 4]]
	set t_up [roundup $highpc [expr $histbinsize * 4]]

	set t_highpc $t_down
	set t_down [rounddown $lowpc [expr $histbinsize * 4]]
	set t_lowpc $t_down

	set retdict [dict create secname $secname lowpc $t_lowpc highpc $t_highpc]
	incr ngmonsections

	return $retdict
    }

    #---------------------------------------------------------------------------------------#
    # Get the call graph sizes & Histogram size and count
    #---------------------------------------------------------------------------------------#
    proc get_prof_cg_hist_details {secdict cputype} {
	variable elf
	variable bigendian
	variable histbinsize
	variable gmonparamstruct
	set retdict {}

	set sname [dict get $secdict secname]

	# Calculate call graph size
	set cgsize [calc_cg_size $sname $cputype]

	# Calculate histogram size
	set highpc [dict get $secdict highpc]
	set lowpc [dict get $secdict lowpc]
	set histcount [expr [expr $highpc - $lowpc] / [expr 4 * $histbinsize]]
	set histsize [roundup [expr $histcount * 2] 4]

	## Update the sizes in the dict
	set retdict [dict create secname $sname lowpc $lowpc highpc $highpc cgsize $cgsize histcount $histcount histsize $histsize]
	lappend gmonparamstruct $retdict

	return $retdict
    }

    #---------------------------------------------------------------------------------------#
    # Calculate call graph size
    #---------------------------------------------------------------------------------------#
    proc calc_cg_size {sname cputype} {
	variable elf
	variable bigendian
	set calltype 0
	set fmt "iu*"
	set fc_cnt 0
	set fpc_cnt 0
	set secdata [$elf get_elf_section_data $sname]

	if { $bigendian == 1 } {
	    set fmt [string toupper $fmt 0 0]
	}

	set bindata [dict get $secdata $sname secbin]
	binary scan $bindata $fmt inst

	for {set i 0} {$i < [llength $inst]} {incr i} {
	    switch -- $cputype {
		"Cortex-A9" {
		    set calltype [is_func_call_instr_a9 [lindex $inst $i]]
		}
		"MicroBlaze" {
		    set calltype [is_func_call_instr_mb [lindex $inst $i]]
		}
	    }
	    if { $calltype == 1 } {
		incr fc_cnt
	    } elseif { $calltype == 2 } {
		incr fpc_cnt
	    }
	}
	# Call Graph Mem Size 	= Size for FromStruct + Size for TosStruct
	# 			= (Total Num of Calls * FromStructSize) +  ((Num of Func Calls + (Num of FuncPtr Calls * ~Num of Target Calls)) * TosStructSize)
	# cgmemsize = [ {(fc + fpc)*12} + {(fc + fpc*5)*12} ]
	set cgmemsize [expr [expr [expr $fc_cnt + $fpc_cnt] * 12] + [expr [expr $fc_cnt + [expr $fpc_cnt * 5]] * 12]]
	return [roundup $cgmemsize 4]
    }

    #---------------------------------------------------------------------------------------#
    # Check func call instructions for A9 & R5
    #---------------------------------------------------------------------------------------#
    proc is_func_call_instr_a9 {instr} {
	set calltype [expr {$instr & 0x0F000000}]
	if { $calltype == 0x01000000 } {
	    if { [expr {$instr & 0x002FFF30}] == 0x002FFF30 } {
		# Func pointer call type
		return 2
	    }
	} elseif { $calltype == 0x0B000000 } {
	    # Func call type
	    return 1
	}
	return 0
    }

    #---------------------------------------------------------------------------------------#
    # Check func call instructions for MB
    #---------------------------------------------------------------------------------------#
    proc is_func_call_instr_mb {instr} {
	set calltype [format 0x%x [expr {$instr >> 26}]]
	set brtype [format 0x%x [expr [expr {$instr >> 16}] & 0x1F]]
	if { $calltype == 0x26 } {
	    # brld & brald (func pointer call)
	    if { ($brtype == 0x14) || ($brtype == 0x1c) } {
	        return 2
	    }
        } elseif { $calltype == 0x2e } {
	    # brlid & bralid (func call)
	    if { ($brtype == 0x14) || ($brtype == 0x1c) } {
	        return 1
	    }
	}
	return 0
    }

    #---------------------------------------------------------------------------------------#
    # Round Down
    #---------------------------------------------------------------------------------------#
    proc rounddown { x y } {
	return [expr ($x / $y) * $y]
    }

    #---------------------------------------------------------------------------------------#
    # Round Up
    #---------------------------------------------------------------------------------------#
    proc roundup { x y } {
	return [expr (($x + $y - 1) / $y) * $y]
    }
}

package provide xsdb::gprof $::xsdb::gprof::version
