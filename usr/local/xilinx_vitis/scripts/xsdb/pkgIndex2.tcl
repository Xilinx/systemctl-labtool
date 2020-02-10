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

package ifneeded xsdb::tcfinterp 0.1 [list source [file join [file dirname [info script]] xsdb_tcfinterp.tcl]]
package ifneeded xsdb::elf 0.1 [list source [file join [file dirname [info script]] elf.tcl]]
package ifneeded xsdb::gprof 0.1 [list source [file join [file dirname [info script]] gprof.tcl]]
package ifneeded xsdb::mbprofiler 0.1 [list source [file join [file dirname [info script]] mbprofiler.tcl]]
package ifneeded xsdb::bitfile 0.1 [list source [file join [file dirname [info script]] bitfile.tcl]]
package ifneeded xsdb::jtag::sequence 0.1 [list source [file join [file dirname [info script]] jtag_sequence.tcl]]
package ifneeded xsdb::mbtrace 0.1 [list source [file join [file dirname [info script]] mbtrace.tcl]]
package ifneeded xsdb::mbtrace_dis 0.1 [list source [file join [file dirname [info script]] mbtrace_dis.tcl]]
