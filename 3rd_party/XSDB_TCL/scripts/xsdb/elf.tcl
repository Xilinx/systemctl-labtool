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

namespace eval ::xsdb::elf {
    variable version 0.1
    variable openfiles {}
    variable openid 0
    variable instance_commands {close read_at get_hdr get_shlist get_phlist get_sym_table get_sym_addr get_sym_val get_exec_sections get_elf_section_data}

    proc _hdr2dict {bin fmt fields} {
	set ff {}
	foreach f $fields {
	    lappend ff $f $f
	}

	set d {}
	dict update d {*}$ff {
	    binary scan $bin $fmt {*}$fields
	}
	return $d
    }

    proc open {path} {
	variable openfiles
	variable openid

	set f [::open $path rb]
	if { [catch {
	    set hdrbin [::read $f 1024]
	    binary scan $hdrbin a4cccccx7 magic class data version osabi abiversion
	    if { $magic != "\177ELF" } {
		error "invalid ELF file"
	    }

	    if { $data == 1 } {
		set bigEndian 0
	    } elseif { $data == 2 } {
		set bigEndian 1
	    } else {
		error "unsupported ELF data: $data"
	    }

	    if { $class == 1 } {
		set elfSize 32
		if { $data == 1 } {
		    set hdrfmt susuiuiuiuiuiususususususu
		    set shdrfmt iuiuiuiuiuiuiuiuiuiu
		    set phdrfmt iuiuiuiuiuiuiuiu
		    set symtabfmt iuiuiucucusu
		} else {
		    set hdrfmt SuSuIuIuIuIuIuSuSuSuSuSuSu
		    set shdrfmt IuIuIuIuIuIuIuIuIuIu
		    set phdrfmt IuIuIuIuIuIuIuIu
		    set symtabfmt IuIuIuCuCuSu
		}
		set phdrfields [list type offset vaddr paddr filesz memsz flags align]
		set symtabfields [list name value size info other shndx]
	    } elseif { $class == 2 } {
		set elfSize 64
		if { $data == 1 } {
		    set hdrfmt susuiuwuwuwuiususususususu
		    set shdrfmt iuiuwuwuwuwuiuiuwuwu
		    set phdrfmt iuiuwuwuwuwuwuwu
		    set symtabfmt iucucusuwuwu
		} else {
		    set hdrfmt SuSuIuWuWuWuIuSuSuSuSuSuSu
		    set shdrfmt IuIuWuWuWuWuIuIuWuWu
		    set phdrfmt IuIuWuWuWuWuWuWu
		    set symtabfmt IuCuCuSuWuWu
		}
		set phdrfields [list type flags offset vaddr paddr filesz memsz align]
		set symtabfields [list name info other shndx value size]
	    } else {
		error "unsupported ELF class: $class"
	    }
	    set hdrfields [list type machine version entry phoff shoff flags ehsize phentsize phnum shentsize shnum shstrndx]
	    set shdrfields [list name_offset type flags addr offset size link info align entsize]

	    set hdr [_hdr2dict $hdrbin "x16$hdrfmt" $hdrfields]
	} msg opt] } {
	    ::close $f
	    return -options $opt $msg
	}
	set name "elf#$openid"
	incr openid

	dict set openfiles $name [dict create f $f elfSize $elfSize bigEndian $bigEndian hdrfmt $hdrfmt shdrfmt $shdrfmt phdrfmt $phdrfmt symtabfmt $symtabfmt hdr $hdr hdrfields $hdrfields shdrfields $shdrfields phdrfields $phdrfields symtabfields $symtabfields]

	interp alias {} ::$name {} ::xsdb::elf::_elf_dispatch $name
	return $name
    }

    proc _elf_dispatch {name {cmd ""} args} {
	variable instance_commands
	set cmds [lsearch -all -inline $instance_commands $cmd*]
	if { [llength $cmds] != 1 } {
	    if { [llength [info level 0]] < 3 } {
		error "wrong # args: should be \"$name option ?arg arg ...?\""
	    }
	    error "bad option \"$cmd\": must be [join $cmds ", "]"
	}
	::xsdb::elf::[set cmds] $name {*}$args
    }

    proc _lookup_name {name} {
	variable openfiles
	if { ![dict exist $openfiles $name] } {
	    error "$name is not and open ELF file"
	}
	return [dict get $openfiles $name]
    }

    proc close {name} {
	variable openfiles
	set e [_lookup_name $name]
	::close [dict get $e f]
	dict unset openfiles $name
	interp alias {} ::$name {}
    }

    proc _read_at {f pos len} {
	seek $f $pos
	set ret [::read $f $len]
	if { [string len $ret] != $len } {
	    error "incomplete ELF file read"
	}
	return $ret
    }

    proc read_at {name pos len} {
	variable openfiles
	set e [_lookup_name $name]
	dict with openfiles $name {
	    return [_read_at $f $pos $len]
	}
    }

    proc get_hdr {name} {
	variable openfiles
	set e [_lookup_name $name]
	dict with openfiles $name {
	    return $hdr
	}
    }

    proc get_sym_table {name} {
	variable openfiles
	set e [_lookup_name $name]
	dict with openfiles $name {
	    if { ![dict exist $e symlist] } {
		set symlist {}
		set shdrlist [$name get_shlist]
		set shnum [dict get $hdr shnum]
		set shdr {}
		for {set i 0} {$i < $shnum} {incr i} {
		    set shdr [lindex $shdrlist $i]
		    if { [dict get $shdr type] == "2" } {
			break
		    }
		}

		set symtaboff [dict get $shdr offset]
		set size [dict get $shdr size]
		set entsize [dict get $shdr entsize]
		set count [expr $size / $entsize]
		set symtabbin [_read_at $f $symtaboff $size]
		set link [dict get $shdr link]
		for {set i 0} {$i < $count} {incr i} {
		    lappend symlist [_hdr2dict $symtabbin "x[expr $i * $entsize]$symtabfmt" $symtabfields]
		}
		dict set e symlist $symlist

		if { $link > 0 && $link < $shnum } {
		    set symstr [lindex $shdrlist $link]
		    set strpool1 [_read_at $f [dict get $symstr offset] [dict get $symstr size]]
		    for {set i 0} {$i < $count} {incr i} {
			set s [lindex $symlist $i]
			set symnameoff [dict get $s name]
			if { $symnameoff > 0 && $symnameoff < [string length $strpool1] } {
			    set end [string first "\0" $strpool1 $symnameoff]
			    if { $end >= 0 } {
				dict set s symname [string range $strpool1 $symnameoff [expr $end - 1]]
				lset symlist $i $s
			    }
			}
		    }
		}
		dict set openfiles $name symlist $symlist
	    }
	    return $symlist
	}
    }

    proc get_sym_addr {name symbol} {
	variable openfiles
	set e [_lookup_name $name]
	if { [dict get $e elfSize] == 32 } {
	    set fmt "0x%08x"
	} else {
	    set fmt "0x%16x"
	}
	set symtable [$name get_sym_table]
	dict with openfiles $name {
	    foreach sym $symtable {
		if { [dict exist $sym symname] } {
		    set sym_name [dict get $sym symname]
		    if { $sym_name == $symbol } {
			return [format $fmt [dict get $sym value]]
		    }
		}
	    }
	}
	return ""
    }

    proc get_sym_val {name symbol bigendian} {
	variable openfiles
	set fmt iu

	if {$bigendian == 1} {
	    set fmt Iu
	}

	set e [_lookup_name $name]
	set symtable [$name get_sym_table]
	set sym_name ""
	set symbol_value ""
	set sym_addr ""
	dict with openfiles $name {
	    foreach sym $symtable {
		if { [dict exist $sym symname] } {
		    set sym_name [dict get $sym symname]
		    if { $sym_name == $symbol } {
			set sym_addr [dict get $sym value]
			set sym_size [dict get $sym size]
			break
		    }
		}
	    }
	    set shdrlist [$name get_shlist]
	    foreach shdr $shdrlist {
		set loadadd [dict get $shdr addr]
		set datasize [dict get $shdr size]
		set secoff [dict get $shdr offset]
		set symoff [expr $secoff + $sym_addr - $loadadd]
		set highadd [expr $loadadd + $datasize]
		if { ($sym_addr >= $loadadd) && ($sym_addr < $highadd) } {
		    set symtabbin [_read_at $f $symoff $sym_size]
		    binary scan $symtabbin $fmt symbol_value
		    return $symbol_value
		}
	    }
	}
	return ""
    }

    proc get_elf_section_data {elf sectionname} {
	variable openfiles
	set secdata {}
	set e [_lookup_name $elf]
	dict with openfiles $elf {
	    set shdrlist [$elf get_shlist]
	    foreach shdr $shdrlist {
		if { [dict exist $shdr name] } {
		    if { [dict get $shdr name] == $sectionname } {
			set loadaddr [dict get $shdr addr]
			set datasize [dict get $shdr size]
			set secoff [dict get $shdr offset]
			set secbin [_read_at $f $secoff $datasize]
			dict set secdata $sectionname [dict create loadaddr $loadaddr datasize $datasize secbin $secbin]
			break
		    }
		}
	    }
	}
	return $secdata
    }

    proc get_exec_sections {name} {
	variable openfiles
	set execseclist {}
	set e [_lookup_name $name]
	dict with openfiles $name {
	    foreach shdr [$name get_shlist] {
		set flag [dict get $shdr flags]
		if { ($flag & 0x4) && ($flag & 0x2) } {
		    if { [dict get $shdr size] == 0 } {
			continue
		    }
		    lappend execseclist $shdr
		}
	    }
	}
	return $execseclist
    }

    proc get_shlist {name} {
	variable openfiles
	set e [_lookup_name $name]
	dict with openfiles $name {
	    if { ![dict exist $e shlist] } {
		set shlist {}
		set shnum [dict get $hdr shnum]
		set shentsize [dict get $hdr shentsize]
		set shstrndx [dict get $hdr shstrndx]
		set shdrbin [_read_at $f [dict get $hdr shoff] [expr $shnum * $shentsize]]
		for {set i 0} {$i < $shnum} {incr i} {
		    lappend shlist [_hdr2dict $shdrbin "x[expr $i * $shentsize]$shdrfmt" $shdrfields]
		}
		dict set e shlist $shlist

		if { $shstrndx > 0 && $shstrndx < $shnum } {
		    set shstr [lindex $shlist $shstrndx]
		    set strpool [_read_at $f [dict get $shstr offset] [dict get $shstr size]]
		    for {set i 0} {$i < $shnum} {incr i} {
			set s [lindex $shlist $i]
			set name_offset [dict get $s name_offset]
			if { $name_offset > 0 && $name_offset < [string length $strpool] } {
			    set end [string first "\0" $strpool $name_offset]
			    if { $end >= 0 } {
				dict set s name [string range $strpool $name_offset [expr $end - 1]]
				lset shlist $i $s
			    }
			}
		    }
		}
		dict set openfiles $name shlist $shlist
	    }
	    return $shlist
	}
    }

    proc get_phlist {name} {
	variable openfiles
	set e [_lookup_name $name]
	dict with openfiles $name {
	    if { ![dict exist $e phlist] } {
		set phlist {}
		set phnum [dict get $hdr phnum]
		set phentsize [dict get $hdr phentsize]
		set phdrbin [_read_at $f [dict get $hdr phoff] [expr $phnum * $phentsize]]
		for {set i 0} {$i < $phnum} {incr i} {
		    lappend phlist [_hdr2dict $phdrbin "x[expr $i * $phentsize]$phdrfmt" $phdrfields]
		}
		dict set e phlist $phlist
		dict set openfiles $name phlist $phlist
	    }
	    return $phlist
	}
    }
}

package provide xsdb::elf $::xsdb::elf::version
