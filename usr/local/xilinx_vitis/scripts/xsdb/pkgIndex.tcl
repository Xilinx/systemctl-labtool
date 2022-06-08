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

package ifneeded xsdb 0.1 [list source [file join [file dirname [info script]] xsdb.tcl]]
package ifneeded xsdb::jtag 0.1 [list source [file join [file dirname [info script]] jtag.tcl]]
package ifneeded xsdb::tfile 0.1 [list source [file join [file dirname [info script]] tfile.tcl]]
package ifneeded xsdb::server 0.1 [list source [file join [file dirname [info script]] xsdbserver.tcl]]
package ifneeded xsdb::gdbremote 0.1 [list source [file join [file dirname [info script]] gdbremote.tcl]]
package ifneeded sdk 0.1 [list source [file join [file dirname [info script]] sdk.tcl]]
package ifneeded xsdb::svf 0.1 [list source [file join [file dirname [info script]] svf.tcl]]
package ifneeded xsdb::stapl 0.1 [list source [file join [file dirname [info script]] stapl.tcl]]
package ifneeded xsdb::device 0.1 [list source [file join [file dirname [info script]] device.tcl]]
source [file join [file dirname [info script]] pkgIndex2.tcl]
