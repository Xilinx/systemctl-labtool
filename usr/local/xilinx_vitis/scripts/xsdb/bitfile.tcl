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

package require control

namespace eval ::xsdb::bitfile {
    variable version 0.1
    variable openfiles {}
    variable openid 0
    variable instance_commands {close read}
    variable opnames {0 NOP 1 Read 2 Write 3 Reserved}
    variable addrnames {0 CRC 1 FAR 2 FDRI 3 FDRO 4 CMD 5 CTL0 6 MASK
	7 STAT 8 LOUT 9 COR0 10 MFWR 11 CBC 12 IDCODE 13 AXSS
	14 COR1 15 PCAP 16 WBSTAR 17 TIMER 18 RBCRC_HW 19 RBCRC_SW
	20 RBCRC_LIVE 21 EFAR 22 BOOTSTS 23 TESTMODE 24 CTL1
	25 MEMRD_PARAM 26 DWC 27 TRIM 29 FUSE_STAT 30 BOUT 31 BSPI}

    proc logf {fmt args} {
	#uncomment the following line to enable logging
	#puts -nonewline [uplevel 1 [list format $fmt {*}$args]]
    }

    proc _check_name {name} {
	variable openfiles
	if { ![dict exist $openfiles $name] } {
	    error "$name is not an open bit file"
	}
    }

    proc _fill_buffer {name size} {
	variable openfiles
	dict with openfiles $name {
	    control::assert { $readpos <= $pos }
	    control::assert { $datapos <= $readpos }
	    control::assert { $datapos + $datalen >= $readpos }

	    if { $size < $minsize } {
		set size $minsize
	    }
	    if { $datapos + $datalen < $readpos + $size } {
		if { $datapos + $datalen == $readpos } {
		    incr datapos $datalen
		    set datalen 0
		    set data ""
		}

		append data [::read $f [expr $readpos + $size - $datapos - $datalen]]
		set datalen [string length $data]
	    }
	}
    }

    proc _read_ahead {name} {
	variable openfiles
	variable opnames
	variable addrnames
	dict with openfiles $name {
	    if { $pos >= [dict get $state endpos] } {
		# End of current segment is found, wait for reader to
		# catchup before processing more
		return
	    }

	    if { $datapos + $datalen < $pos + $minsize } {
		# Insufficient data to do anything useful
		return
	    }

#puts "-3- readpos $readpos type [dict get $state type] pos $pos $datapos $datalen endpos [dict get $state endpos]"
	    if { [dict get $state type] == "unknown" } {
		if { $pos == 0 } {
		    set offset 0
		    set isbitfile 1
		    binary scan $data "Su" magicsz
		    incr offset 2
		    if { $magicsz < 100 } {
			if { $datalen < $offset + $magicsz } {
			    set minsize [expr $offset + $magicsz]
			    return
			}
			binary scan $data "@[expr $offset]cu[expr $magicsz]" magic
#puts "-5- magic $magic"
			incr offset $magicsz
			while { [llength $magic] > 2 &&
				[lindex $magic 0] == 0x0f &&
				[lindex $magic 1] == 0xf0 } {
			    set magic [lrange $magic 2 end]
			}
#puts "-5- magic $magic"
			if { [llength $magic] > 1 && [lindex $magic 0] == 0x0f } {
			    set magic [lrange $magic 1 end]
			}
#puts "-5- magic $magic"
			if { [llength $magic] != 1 || [lindex $magic 0] != 0 } {
			    set isbitfile 0
			}
		    }
		    if { $isbitfile} {
			if { $datalen < $offset + 2 } {
			    set minsize [expr $offset + 2]
			    return
			}
			# special marker length
			binary scan $data "@[expr $offset]Su" markersz
#puts "-5- markersz $markersz"
			incr offset 2
			if { $markersz != 1 } {
			    set isbitfile 0
			}
		    }
		    if { $isbitfile} {
			while 1 {
			    if { $datalen < $offset + 1 } {
				set minsize [expr $offset + 1]
				return
			    }
			    binary scan $data "@[expr $offset]a1" m
#puts "-5- marker $m"
			    incr offset 1
			    switch -- $m {
				a -
				b -
				c -
				d {
				    if { $datalen < $offset + 2 } {
					set minsize [expr $offset + 2]
					return
				    }
				    binary scan $data "@[expr $offset]Su" l
				    if { $datalen < $offset + 2 + $l } {
					set minsize [expr $offset + 2 + $l]
					return
				    }
				    incr offset 2
				    binary scan $data "@[expr $offset]a[expr $l - 1]c" v n
				    if { $n != 0 } {
					set isbitfile 0
					break
				    }
				    set k [dict get {a "design" b "part" c "date" d "time"} $m]
				    dict set state $k $v
				    dict set openfiles $name state $state
				    incr offset $l
				}
				e {
				    if { $datalen < $offset + 4 } {
					set minsize [expr $offset + 4]
					return
				    }
				    binary scan $data "@[expr $offset]Iu" l
				    incr offset 4
				    set pos $offset
				    if { $pos > [dict get $state endpos] } {
					set pos [dict get $state endpos]
				    }
				    incr offset $l
				    if { $offset > [dict get $state endpos] } {
					set offset [dict get $state endpos]
				    }
				    dict set state type bitfile
				    lappend statestack $state
				    lappend statestack [dict create type bitstream endpos $offset slr 0]
				    dict set state endpos $pos
				    dict set openfiles $name state $state
				    logf "%*soffset %8x %s\n" [expr [llength $statestack] * 2] "" 0 $state
				    set minsize 4
				    return
				}
				default {
				    set isbitfile 0
				    break
				}
			    }
			}
		    }
		}
#puts "-5- offset $offset"
		dict set openfiles $name pos [dict get $state endpos]
		#puts "-1- readpos $readpos type [dict get $state type] pos $pos"
		return
	    }

	    control::assert { [dict get $state type] == "bitstream" || [dict get $state type] == "bout" }
	    while { $datapos + $datalen - $pos >= 4 } {
		binary scan $data "@[expr $pos - $datapos]Iu" ph
		if { $ph == 0x3003c000 && [dict get $state type] == "bitstream" } {
		    lappend statestack $state
		    lappend statestack [dict create type bout endpos [dict get $state endpos]]
		    dict set state endpos $pos
		    dict set openfiles $name state $state
		    set minsize 8
		    break
		}
		set ht [expr {($ph >> 29) & 7}]
		logf "%*soffset %8x type %d" [expr [llength $statestack] * 2] "" $pos $ht

		if { $ht == 0 } {
		    if { $ph == 0x000000BB } {
			logf " Bus Width Sync Word"
		    } else {
			logf " %08x" $ph
		    }
		    set size 0
		    incr pos 4
		} elseif { $ht == 1 } {
		    set opcode [expr {($ph >> 27) & 3}]
		    set addr [expr {($ph >> 13) & 0x3fff}]
		    set size [expr {($ph & 0x7ff)*4}]
		    if { [catch { set addrname "[dict get $addrnames $addr]($addr)" }] } {
			set addrname $addr
		    }
		    if { $opcode == 0 && $addr == 0 } {
			logf " %s" [dict get $opnames $opcode]
		    } else {
			logf " %s %s" [dict get $opnames $opcode] $addrname
		    }
		    incr pos 4
		} elseif { $ht == 2 } {
		    set opcode [expr {($ph >> 27) & 3}]
		    set size [expr {($ph & 0x07ffffff)*4}]
		    logf " %s size 0x%x" [dict get $opnames $opcode] $size
		    if { [dict get $state type] == "bout" } {
			set s [lindex $statestack end]
			dict incr s slr
			dict set s endpos [expr $pos + 4 + $size]
			lappend statestack $s
			dict set state endpos [expr $pos + 4]
			dict set openfiles $name state $state
			set size 0
		    }
		    incr pos 4
		} elseif { $ph == 0xaa995566 } {
		    logf " SyncWord"
		    set size 0
		    incr pos 4
		} elseif { $ph == 0xffffffff } {
		    logf " DummyWord"
		    set size 0
		    incr pos 4
		} else {
		    logf " %08x" $ph
		    set size 0
		    incr pos 4
		}
		if { $size >= 4 } {
		    logf " %x" $size
		    set words [binary scan $data "@[expr $pos - $datapos]IuIu" w1 w2]
		    if { $size >= 4 } {
			logf "("
			if { $words >= 1 } {
			    logf "%x" $w1
			} else {
			    logf "..."
			}
			if { $size >= 8 } {
			    if { $words >= 2 } {
				logf " %x" $w2
			    }
			    if { $size >= 12 || $words < 2 } {
				logf " ..."
			    }
			}
			logf ")"
		    }
		    incr pos $size
		}
		logf "\n"
		if { $pos >= [dict get $state endpos] } {
		    #puts "-2- readpos $readpos type [dict get $state type] pos $pos"
		    if { $pos > [dict get $state endpos] } {
			logf "--- entry ends 0x%x past section end 0x%08x\n" $pos [dict get $state endpos]
			set pos [dict get $state endpos]
		    }
		    control::assert { $readpos <= $pos }
		    break
		}
	    }
	    dict set openfiles $name pos $pos
	    #puts "-6- readpos $readpos type [dict get $state type] pos $pos"
	}
    }

    proc open {path} {
	variable openfiles
	variable openid

	set f [::open $path rb]
	fconfigure $f -buffering none

	set b {}
	dict set b maxbuffer 0x10000
	dict set b f $f
	dict set b state type "unknown"
	dict set b state endpos [file size $path]
	dict set b minsize 4
	dict set b pos 0
	dict set b statestack {}
	dict set b readpos 0
	dict set b datapos 0
	dict set b datalen 0
	dict set b data ""

	set name "bitfile#$openid"
	incr openid
	dict set openfiles $name $b
	return $name
    }

    proc _read {name size} {
	variable openfiles
	dict with openfiles $name {
	    #puts "-4- readpos $readpos pos $pos state $state"
	    if { $readpos < $pos } {
		control::assert { [dict get $state endpos] > 0 }
		control::assert { $pos <= [dict get $state endpos] }
		if { $datapos + $datalen < $pos } {
		    set avail [expr $datapos + $datalen - $readpos]
		} else {
		    set avail [expr $pos - $readpos]
		}
		if { $avail == 0 } {
		    return ""
		}
	    } else {
		#puts "-7- readpos $readpos state $state pos $pos $datapos $datalen"
		if { [llength $statestack] == 0 && $pos == [dict get $state endpos] } {
		    return [list eof {} ""]
		}
		if { $readpos >= [dict get $state endpos] } {
#puts "-8- reducing endpos $readpos $state"
		    set ret [list [dict get $state type] $state ""]
		    set state [lindex $statestack end]
		    dict set openfiles $name state $state
		    set statestack [lrange $statestack 0 end-1]
		    dict set openfiles $name statestack $statestack
		    return $ret
		}
		if { $pos < [dict get $state endpos] } {
		    return ""
		}
		return ""
	    }
	    if { $size > $avail } {
		set size $avail
	    }
	    set offset [expr $readpos - $datapos]
	    incr readpos $size
	    return [list [dict get $state type] $state [string range $data $offset [expr $offset + $size - 1]]]
	}
    }

    proc read {name size} {
	variable openfiles
	_check_name $name
	while 1 {
	    set ret [_read $name $size]
	    if { [llength $ret] > 0 } {
		return $ret
	    }
	    _fill_buffer $name $size
	    _read_ahead $name
	}
    }

    proc close {name} {
	variable openfiles
	_check_name $name
	::close [dict get $openfiles $name f]
	dict unset openfiles $name
    }
}

package provide xsdb::bitfile $::xsdb::bitfile::version
