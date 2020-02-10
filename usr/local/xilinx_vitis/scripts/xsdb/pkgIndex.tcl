#######################################################################
# Copyright (c) 2014-2018 Xilinx, Inc.  All rights reserved.
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
package ifneeded xsdb::device 0.1 [list source [file join [file dirname [info script]] device.tcl]]
source [file join [file dirname [info script]] pkgIndex2.tcl]
