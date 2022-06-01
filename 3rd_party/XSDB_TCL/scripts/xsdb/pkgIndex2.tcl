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

if {![package vsatisfies [package provide Tcl] 8.5]} {
    # PRAGMA: returnok
    return
}

package ifneeded xsdb::tcfinterp 0.1 [list source [file join [file dirname [info script]] xsdb_tcfinterp.tcl]]
package ifneeded xsdb::elf 0.1 [list source [file join [file dirname [info script]] elf.tcl]]
package ifneeded xsdb::gprof 0.1 [list source [file join [file dirname [info script]] gprof.tcl]]
package ifneeded xsdb::mbprofiler 0.1 [list source [file join [file dirname [info script]] mbprofiler.tcl]]
package ifneeded xsdb::bitfile 0.1 [list source [file join [file dirname [info script]] bitfile.tcl]]
package ifneeded xsdb::jtag::sequence 0.1 [list source [file join [file dirname [info script]] jtag_sequence.tcl]]
package ifneeded xsdb::mbtrace 0.1 [list source [file join [file dirname [info script]] mbtrace.tcl]]
package ifneeded xsdb::mbtrace_dis 0.1 [list source [file join [file dirname [info script]] mbtrace_dis.tcl]]
