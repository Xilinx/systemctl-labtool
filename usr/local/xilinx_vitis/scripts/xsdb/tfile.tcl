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

package require tcf

namespace eval ::xsdb::tfile {
    variable version 0.1
    variable files {}
    variable file_attr {Size i UID i GID i Permissions i ATime i MTime i Win32Attrs i}

    ::xsdb::setcmdmeta tfile brief {Target File System}

    proc open { args } {
	variable files
	variable file_attr
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] < 1 || [llength $args] > 3 } {
	    error "wrong # args: should be \"path \[mode\] \[attributes\]\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	if { [llength $args] > 1 } {
	    set flags [lindex $args 1]
	} else {
	    set flags 0
	}
	if { [llength $args] > 2 } {
	    set attrs [lindex $args 2]
	} else {
	    set attrs {}
	}
	set fhandle [lindex [::tcf::send_command $chan FileSystem open "sio[list $file_attr]" es [list $path $flags $attrs]] 1]
	dict set files $chan $fhandle offset 0
	return $fhandle
    }
    namespace export open
    ::xsdb::setcmdmeta {tfile open} categories {tfile}
    ::xsdb::setcmdmeta {tfile open} brief {Open file.}
    ::xsdb::setcmdmeta {tfile open} description {
SYNOPSIS {
    tfile open <path>
        Open specified file.
}
RETURNS {
    File handle.
}
}

    proc close { args } {
	variable files
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"\[handle\]\""
	}

	set chan [::xsdb::getcurchan]
	set fhandle [lindex $args 0]
	if { ![dict exists $files $chan $fhandle offset] } {
	    error "invalid file handle: $fhandle"
	}
	::tcf::send_command $chan FileSystem close s e [list $fhandle]
	dict unset files $chan $fhandle
	return ""
    }
    namespace export close
    ::xsdb::setcmdmeta {tfile close} categories {tfile}
    ::xsdb::setcmdmeta {tfile close} brief {Close file handle.}
    ::xsdb::setcmdmeta {tfile close} description {
SYNOPSIS {
    tfile close <handle>
        Close specified file handle.
}
RETURNS {
	Nothing.
}
}

    proc read { args } {
	variable files
	set options {
	    {offset "seek offset" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"handle length\""
	}

	set chan [::xsdb::getcurchan]
	set fhandle [lindex $args 0]
	set len [lindex $args 1]
	if { ![dict exists $files $chan $fhandle offset] } {
	    error "invalid file handle: $fhandle"
	}
	if { [info exists params(offset)] } {
	    set offset $params(offset)
	} else {
	    set offset [dict get $files $chan $fhandle offset]
	}
	set data [lindex [::tcf::send_command $chan FileSystem read sii Beb [list $fhandle $offset $len]] 0]
	incr offset [string length $data]
	dict set files $chan $fhandle offset $offset
	return $data
    }
    namespace export read
    ::xsdb::setcmdmeta {tfile read} categories {tfile}
    ::xsdb::setcmdmeta {tfile read} brief {Read file handle.}
    ::xsdb::setcmdmeta {tfile read} description {
SYNOPSIS {
    tfile read <handle>
        Read from specified file handle.
}
OPTIONS {
    -offset <seek>
        File offset to read from.
}
RETURNS {
    Read data.
}
}

    proc write { args } {
	variable files
	set options {
	    {offset "seek offset" {args 1}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"handle \[data\]\""
	}

	set chan [::xsdb::getcurchan]
	set fhandle [lindex $args 0]
	set data [lindex $args 1]
	if { ![dict exists $files $chan $fhandle offset] } {
	    error "invalid file handle: $fhandle"
	}
	if { [info exists params(offset)] } {
	    set offset $params(offset)
	} else {
	    set offset [dict get $files $chan $fhandle offset]
	}
	::tcf::send_command $chan FileSystem write siB e [list $fhandle $offset $data]
	incr offset [string length $data]
	dict set files $chan $fhandle offset $offset
	return ""
    }
    namespace export write
    ::xsdb::setcmdmeta {tfile write} categories {tfile}
    ::xsdb::setcmdmeta {tfile write} brief {Write file handle.}
    ::xsdb::setcmdmeta {tfile write} description {
SYNOPSIS {
    tfile write <handle>
        Write to specified file handle.
}
OPTIONS {
    -offset <seek>
        File offset to write to.
}
RETURNS {
    Nothing.
}
}

    proc stat { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"handle\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	lindex [::tcf::send_command $chan FileSystem stat s eo{} [list $path]] 1
    }
    namespace export stat
    ::xsdb::setcmdmeta {tfile stat} categories {tfile}
    ::xsdb::setcmdmeta {tfile stat} brief {Get file attributes from path.}
    ::xsdb::setcmdmeta {tfile stat} description {
SYNOPSIS {
    tfile stat <handle>
        Get file attributes for <path>.
}
RETURNS {
    File attributes.
}
}

    proc lstat { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"handle\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	lindex [::tcf::send_command $chan FileSystem lstat s eo{} [list $path]] 1
    }
    namespace export lstat
    ::xsdb::setcmdmeta {tfile lstat} categories {tfile}
    ::xsdb::setcmdmeta {tfile lstat} brief {Get link file attributes from path.}
    ::xsdb::setcmdmeta {tfile lstat} description {
SYNOPSIS {
    tfile lstat <path>
        Get link file attributes for <path>.
}
RETURNS {
    Link file attributes.
}
}

    proc fstat { args } {
	variable files
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"handle\""
	}

	set chan [::xsdb::getcurchan]
	set fhandle [lindex $args 0]
	if { ![dict exists $files $chan $fhandle offset] } {
	    error "invalid file handle: $fhandle"
	}
	lindex [::tcf::send_command $chan FileSystem fstat s eo{} [list $fhandle]] 1
    }
    namespace export fstat
    ::xsdb::setcmdmeta {tfile fstat} categories {tfile}
    ::xsdb::setcmdmeta {tfile fstat} brief {Get file attributes from handle.}
    ::xsdb::setcmdmeta {tfile fstat} description {
SYNOPSIS {
    tfile fstat <handle>
        Get file attributes for <handle>.
}
RETURNS {
    File attributes.
}
}

    proc setstat { args } {
	variable file_attr
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"handle attributes\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	set attrs [lindex $args 1]
	::tcf::send_command $chan FileSystem setstat "so[list $file_attr]" e [list $path $attrs]
	return ""
    }
    namespace export setstat
    ::xsdb::setcmdmeta {tfile setstat} categories {tfile}
    ::xsdb::setcmdmeta {tfile setstat} brief {Set file attributes for path.}
    ::xsdb::setcmdmeta {tfile setstat} description {
SYNOPSIS {
    tfile setstat <path> <attributes>
        Set file attributes for <path>.
}
RETURNS {
    File attributes.
}
}

    proc fsetstat { args } {
	variable file_attr
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"handle attributes\""
	}

	set chan [::xsdb::getcurchan]
	set fhandle [lindex $args 0]
	set attrs [lindex $args 1]
	::tcf::send_command $chan FileSystem fsetstat "so[list $file_attr]" e [list $fhandle $attrs]
	return ""
    }
    namespace export fsetstat
    ::xsdb::setcmdmeta {tfile fsetstat} categories {tfile}
    ::xsdb::setcmdmeta {tfile fsetstat} brief {Set file attributes for handle.}
    ::xsdb::setcmdmeta {tfile fsetstat} description {
SYNOPSIS {
    tfile fsetstat <handle> <attributes>
        Set file attributes for <handle>.
}
RETURNS {
    File attributes.
}
}

    proc remove { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"path\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	::tcf::send_command $chan FileSystem remove s e [list $path]
	return ""
    }
    namespace export remove
    ::xsdb::setcmdmeta {tfile remove} categories {tfile}
    ::xsdb::setcmdmeta {tfile remove} brief {Remove path.}
    ::xsdb::setcmdmeta {tfile remove} description {
SYNOPSIS {
    tfile remove <path>
        Remove <path>.
}
RETURNS {
    Nothing.
}
}

    proc rmdir { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"path\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	::tcf::send_command $chan FileSystem rmdir s e [list $path]
	return ""
    }
    namespace export rmdir
    ::xsdb::setcmdmeta {tfile rmdir} categories {tfile}
    ::xsdb::setcmdmeta {tfile rmdir} brief {Remove directory.}
    ::xsdb::setcmdmeta {tfile rmdir} description {
SYNOPSIS {
    tfile rmdir <path>
        Remove directory <path>.
}
RETURNS {
    Nothing.
}
}

    proc mkdir { args } {
	variable file_attr
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] < 1 || [llength $args] > 2 } {
	    error "wrong # args: should be \"path \[attributes\]\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	if { [llength $args] > 1 } {
	    set attrs [lindex $args 1]
	} else {
	    set attrs {}
	}
	::tcf::send_command $chan FileSystem mkdir "so[list $file_attr]" e [list $path $attrs]
	return ""
    }
    namespace export mkdir
    ::xsdb::setcmdmeta {tfile mkdir} categories {tfile}
    ::xsdb::setcmdmeta {tfile mkdir} brief {Create directory.}
    ::xsdb::setcmdmeta {tfile mkdir} description {
SYNOPSIS {
    tfile mkdir <path>
        Make directory <path>.
}
RETURNS {
    Nothing.
}
}

    proc realpath { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"path\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	lindex [::tcf::send_command $chan FileSystem realpath s es [list $path]] 1
    }
    namespace export realpath
    ::xsdb::setcmdmeta {tfile realpath} categories {tfile}
    ::xsdb::setcmdmeta {tfile realpath} brief {Get real path.}
    ::xsdb::setcmdmeta {tfile realpath} description {
SYNOPSIS {
    tfile realpath <path>
        Get real path of <path>.
}
RETURNS {
    Real path.
}
}

    proc rename { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"old new\""
	}

	set chan [::xsdb::getcurchan]
	set oldpath [lindex $args 0]
	set newpath [lindex $args 1]
	::tcf::send_command $chan FileSystem rename ss e [list $oldpath $newpath]
	return ""
    }
    namespace export rename
    ::xsdb::setcmdmeta {tfile rename} categories {tfile}
    ::xsdb::setcmdmeta {tfile rename} brief {Rename path.}
    ::xsdb::setcmdmeta {tfile rename} description {
SYNOPSIS {
    tfile rename <old path> <new path>
        Rename file or directory.
}
RETURNS {
    Nothing.
}
}

    proc readlink { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"path\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	lindex [::tcf::send_command $chan FileSystem readlink s es [list $path]] 1
    }
    namespace export readlink
    ::xsdb::setcmdmeta {tfile readlink} categories {tfile}
    ::xsdb::setcmdmeta {tfile readlink} brief {Read symbolic link.}
    ::xsdb::setcmdmeta {tfile readlink} description {
SYNOPSIS {
    tfile readlink <path>
        Read link file.
}
RETURNS {
    Target path.
}
}

    proc symlink { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"link target\""
	}

	set chan [::xsdb::getcurchan]
	set lpath [lindex $args 0]
	set tpath [lindex $args 1]
	::tcf::send_command $chan FileSystem symlink ss e [list $lpath $tpath]
	return ""
    }
    namespace export symlink
    ::xsdb::setcmdmeta {tfile symlink} categories {tfile}
    ::xsdb::setcmdmeta {tfile symlink} brief {Create symbolic link.}
    ::xsdb::setcmdmeta {tfile symlink} description {
SYNOPSIS {
    tfile symlink <old path> <new path>
        Symlink file or directory.
}
RETURNS {
    Nothing.
}
}
    proc opendir { args } {
	variable files
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"\[path\]\""
	}

	set chan [::xsdb::getcurchan]
	set path [lindex $args 0]
	set fhandle [lindex [::tcf::send_command $chan FileSystem opendir s es [list $path]] 1]
	dict set files $chan $fhandle offset 0
	return $fhandle
    }
    namespace export opendir
    ::xsdb::setcmdmeta {tfile opendir} categories {tfile}
    ::xsdb::setcmdmeta {tfile opendir} brief {Open directory.}
    ::xsdb::setcmdmeta {tfile opendir} description {
SYNOPSIS {
    tfile opendir <path>
        Open directory <path>.
}
RETURNS {
    File handle.
}
}

    proc readdir { args } {
	variable files
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 1 } {
	    error "wrong # args: should be \"\[handle\]\""
	}

	set chan [::xsdb::getcurchan]
	set fhandle [lindex $args 0]
	if { ![dict exists $files $chan $fhandle offset] } {
	    error "invalid file handle: $fhandle"
	}
	lindex [::tcf::send_command $chan FileSystem readdir s a{}eb [list $fhandle]] 0
    }
    namespace export readdir
    ::xsdb::setcmdmeta {tfile readdir} categories {tfile}
    ::xsdb::setcmdmeta {tfile readdir} brief {Read directory.}
    ::xsdb::setcmdmeta {tfile readdir} description {
SYNOPSIS {
    tfile readdir <file handle>
        Read directory.
}
RETURNS {
    File handle.
}
}

    proc copy { args } {
	set options {
	    {from-host "copy from host to target"}
	    {to-host "copy from target to host"}
	    {owner "copy owner information"}
	    {permissions "copy permissions"}
	    {chunk-size "size of read/write requests" {args 1 default 16384}}
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] != 2 } {
	    error "wrong # args: should be \"\[src\] \[dest\]\""
	}

	set src [lindex $args 0]
	set dest [lindex $args 1]
	if { $params(from-host) || $params(to-host) } {
	    if { $params(from-host) } {
		set fsrc [::open $src rb]
	    } else {
		set fsrc [open $src 1]
	    }
	    if { $params(to-host) } {
		set fdest [::open $dest wb]
	    } else {
		set fdest [open $dest 0x1a]
	    }
	    set err [catch {
		while 1 {
		    if { $params(from-host) } {
			set data [::read $fsrc $params(chunk-size)]
		    } else {
			set data [read $fsrc $params(chunk-size)]
		    }
		    if { [string length $data] == 0 } break
		    if { $params(to-host) } {
			::puts -nonewline $fdest $data
		    } else {
			write $fdest $data
		    }
		}
	    } msg opt]
	    if { $params(from-host) } {
		::close $fsrc
	    } else {
		close $fsrc
	    }
	    if { $params(to-host) } {
		::close $fdest
	    } else {
		close $fdest
	    }
	    if { $err } {
		return -options $opt $msg
	    }
	} else {
	    set chan [::xsdb::getcurchan]
	    ::tcf::send_command $chan FileSystem copy ssbb e [list $src $dest $params(owner) $params(permissions)]
	}
	return ""
    }
    namespace export copy
    ::xsdb::setcmdmeta {tfile copy} categories {tfile}
    ::xsdb::setcmdmeta {tfile copy} brief {Copy target file.}
    ::xsdb::setcmdmeta {tfile copy} description {
SYNOPSIS {
    tfile copy <src> <dest>
        Copy file <src> to <dest>.
}
RETURNS {
    Copy file locally on target.
}
}

    proc user { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set chan [::xsdb::getcurchan]
	::tcf::send_command $chan FileSystem user {} iiiis [list]
    }
    namespace export user
    ::xsdb::setcmdmeta {tfile user} categories {tfile}
    ::xsdb::setcmdmeta {tfile user} brief {Get user attributes.}
    ::xsdb::setcmdmeta {tfile user} description {
SYNOPSIS {
    tfile user
        Get user attributes.
}
RETURNS {
    User information.
}
}

    proc roots { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	set chan [::xsdb::getcurchan]
	lindex [::tcf::send_command $chan FileSystem roots {} a{}e [list]] 0
    }
    namespace export roots
    ::xsdb::setcmdmeta {tfile roots} categories {tfile}
    ::xsdb::setcmdmeta {tfile roots} brief {Get file system roots.}
    ::xsdb::setcmdmeta {tfile roots} description {
SYNOPSIS {
    tfile roots
        Get file system roots.
}
RETURNS {
    List of file system roots.
}
}

    proc ls { args } {
	set options {
	    {help "command help"}
	}
	array set params [::xsdb::get_options args $options 0]
	if { $params(help) } {
	    return [help tfile [lindex [split [lindex [info level 0] 0] ::] end]]
	}

	if { [llength $args] > 1 } {
	    error "wrong # args: should be \"\[path\]\""
	}

	set chan [::xsdb::getcurchan]
	if { [llength $args] == 0 } {
	    set roots [lindex [::tcf::send_command $chan FileSystem roots {} a{}e [list]] 0]
	    if { [llength $roots] == 1 } {
		set path [dict get [lindex $roots 0] FileName]
	    } else {
		set l $roots
	    }
	} else {
	    set path [lindex $args 0]
	}
	if { [info exists path] } {
	    set fhandle [opendir $path]
	    set l {}
	    if { [catch {
		while 1 {
		    set v [readdir $fhandle]
		    if { $v == {} } break
		    lappend l {*}$v
		}
	    } msg opt] } {
		close $fhandle
		return -options $opt $msg
	    }
	    close $fhandle
	}
	set result {}
	foreach e $l {
	    set size ""
	    if { [dict exists $e Attrs Size] } {
		set size [format "%u" [dict get $e Attrs Size]]
	    }
	    lappend result [format " %10s %s" $size [dict get $e FileName]]
	}
	return [join $result \n]
    }
    namespace export ls
    ::xsdb::setcmdmeta {tfile ls} categories {tfile}
    ::xsdb::setcmdmeta {tfile ls} brief {List directory contents.}
    ::xsdb::setcmdmeta {tfile ls} description {
SYNOPSIS {
    tfile ls <path>
        List directory contents.
}
RETURNS {
    Directory contents.
}
}

    namespace ensemble create -command ::tfile
}

package provide xsdb::tfile $::xsdb::tfile::version
