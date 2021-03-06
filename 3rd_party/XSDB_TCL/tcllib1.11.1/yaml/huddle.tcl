# huddle.tcl (working title)
#
# huddle.tcl 0.1.3 2008-06-05 17:51:30 KATO Kanryu(kanryu6@users.sourceforge.net)
#
#   It is published with the terms of tcllib's BSD-style license.
#   See the file named license.terms.
#
# This library provide functions to differentinate string/list/dict in multi-ranks.
#

if {$::tcl_version < 8.5} {
    package require dict
}

package provide huddle 0.1.3

namespace eval ::huddle {
    namespace export huddle
    # common subcommands:
    #   get gets strip jsondump set remove
    # type specified subcommands:
    #   create list llength keys
    
    variable methods
    variable types
}

proc huddle {command args} {
    variable huddle::methods
    if {[info exists huddle::methods($command)]} {
        return [eval $huddle::methods($command) $command $args]
    }
    switch -- $command {
        set {
            return [eval ::huddle::_set $args]
        }
        append {
            return [eval ::huddle::_append $args]
        }
        call {
            variable huddle::types
            foreach {tag cmd args} $args break
            return [eval $huddle::types(callback:$tag) $cmd $args]
        }
        default {
            return [eval ::huddle::$command $args]
        }
    }
}


proc ::huddle::addType {procedure} {
    variable methods
    variable types
    
    set setting [$procedure setting]
    dict with setting {
        foreach {m} $method {
            set methods($m) $procedure
        }
        foreach {t node} $tag {
            set types(type:$t) $type
            set types(node:$t) $node
            set types(callback:$t) $procedure
            set types(constructor:$t) $constructor
            set types(str:$t) $str
        }
    }
}

proc ::huddle::isHuddle {arg} {
    if {[lindex $arg 0] eq "HUDDLE" && [llength $arg] == 2} {
        return 1
    } else {
        return 0
    }
}

proc ::huddle::strip {node} {
    variable types
    foreach {head value} $node break
    if {[info exists types(type:$head)]} {
        if {$types(node:$head) eq "parent"} {
            return [$types(callback:$head) strip $value]
        } else {
            return $value
        }
    }
    switch -- $head {
        HUDDLE {
            return [strip $value]
        }
        default {
            error "\{$src\} is not a huddle."
        }
    }
    return $value
}

proc ::huddle::combine {args} {
    variable types

    foreach {obj} $args {
        checkHuddle $obj
    }
    set tag ""
    foreach {obj} $args {
        foreach {nop node} $obj break
        foreach {t src} $node break
        if {$tag eq ""} {
            set tag $t
        } else {
            if {$tag ne $t} {error "unmatched huddles are given."}
        }
        eval lappend result $src
    }
    set src [$types(callback:$tag) append "" {} $result]
    return [wrap $tag $src]
    
}

proc ::huddle::checkHuddle {src} {
    if {![isHuddle $src]} {
        error "\{$src\} is not a huddle."
    }
}

proc ::huddle::to_node {src {tag ""}} {
    if {$tag eq ""} {set tag s}
    if {[isHuddle $src]} {
        return [lindex $src 1]
    } else {
        return [list $tag $src]
    }
}

proc ::huddle::wrap {head src} {
    if {$head ne ""} {
        return [list HUDDLE [list $head $src]]
    } else {
        return [list HUDDLE $src]
    }
}

proc ::huddle::get {src args} {
    checkHuddle $src
    return [_key_reflexive _get [lindex $src 1] [llength $args] $args 0]
}

proc ::huddle::gets {src args} {
    checkHuddle $src
    return [_key_reflexive _get [lindex $src 1] [llength $args] $args 1]
}

proc ::huddle::type {src args} {
    checkHuddle $src
    lappend args "nop"
    return [_key_reflexive _type [lindex $src 1] [llength $args] $args]
}

proc ::huddle::_set {objvar args} {
    upvar 2 $objvar obj
    checkHuddle $obj
    set path [lrange $args 0 end-1]
    set value [lindex $args end]
    set value [to_node $value]
    foreach {nop node} $obj break
    set node [_set_subs set $node [llength $path] $path $value]
    set obj [wrap "" $node]
}

proc ::huddle::remove {src args} {
    checkHuddle $src
    foreach {nop src} $src break
    set src [_set_subs remove $src [llength $args] $args ""]
    set obj [wrap "" $src]
}

proc ::huddle::equal {obj1 obj2} {
    checkHuddle $obj1
    checkHuddle $obj2
    return [_equal_subs [lindex $obj1 1] [lindex $obj2 1]]
}
proc ::huddle::_equal_subs {obj1 obj2} {
    variable types

    foreach {tag1 src1} $obj1 break
    foreach {tag2 src2} $obj2 break
    if {$tag1 ne $tag2} {return 0}
    return [$types(callback:$tag1) equal $src1 $src2]
}

proc ::huddle::_append {objvar args} {
    variable types

    upvar 2 $objvar obj
    checkHuddle $obj
    foreach {tag src} [lindex $obj 1] break
    set src [$types(callback:$tag) append $types(str:$tag) $src $args]
    set obj [wrap $tag $src]
    return $obj
}

proc ::huddle::_set_subs {command node len path value} {
    variable types
    foreach {tag src} $node break
    if {$len > 1} {
        set key [lindex $path 0]
        set subpath [lrange $path 1 end]
        incr len -1
        if {![info exists types(type:$tag)]} {error "\{$src\} don't have any child node."}
        set subs [$types(callback:$tag) get_sub $src $key]
        set subs [_set_subs $command $subs $len $subpath $value]
        set src [$types(callback:$tag) set $src $key $subs]
        return [list $tag $src]
    }
    if {![info exists types(type:$tag)]} {error "\{$src\} is not a huddle node."}
    set src [$types(callback:$tag) $command $src $path $value]
    return [list $tag $src]
}

proc ::huddle::_key_reflexive {command node len path {option ""}} {
    variable types
    foreach {tag src} $node break
    if {$len > 1} {
        set key [lindex $path 0]
        set subpath [lrange $path 1 end]
        incr len -1
        if {![info exists types(type:$tag)]} {error "\{$src\} don't have any child node."}
        set subs [$types(callback:$tag) get_sub $src $key]
        return [_key_reflexive $command $subs $len $subpath $option] 
    } elseif {$len == 1} {
        set path [lindex $path 0]        
    }
    if {![info exists types(type:$tag)]} {error "\{$src\} is not a huddle node."}
    return [$command $node $path $option]
}

proc ::huddle::_get {node path strip} {
    variable types
    foreach {tag src} $node break
    set subs [$types(callback:$tag) get_sub $src $path]
    return [_strip_wrap "" $subs $strip]
}

proc ::huddle::_type {node nop nop} {
    variable types
    foreach {tag src} $node break
    return $types(type:$tag)
}

proc ::huddle::_strip_wrap {head src {striped 0}} {
    if {$striped} {
        return [strip $src]
    } else {
        return [wrap $head $src]
    }
}

proc ::huddle::_dict_setting {command args} {
# __TRANSCRIBE_BEGIN__
    switch -- $command {
        setting { ; # type definition
            return {
                type dict
                method {create keys}
                tag {d child D parent}
                constructor create
                str s
            }
            # type:   the type-name
            # method: add methods to huddle's subcommand.
            #          "get_sub/strip/set/remove/equal/append" called by huddle module.
            #          "strip" must be defined at all types.
            #          "get_sub" must be defined at container types.
            #          "set/remove/equal/append" shuould be defined, if you call them.
            # tag:    tag definition("child/parent" word is maybe obsoleted)
        }
        get_sub { ; # get a sub-node specified by "key" from the tagged-content
            foreach {src key} $args break
            return [dict get $src $key]
        }
        strip { ; # strip from the tagged-content
            foreach {src nop} $args break
            foreach {key val} $src {
                lappend result $key [huddle strip $val]
            }
            return $result
        }
        set { ; # set a sub-node from the tagged-content
            foreach {src key value} $args break
            dict set src $key $value
            return $src
        }
        remove { ; # remove a sub-node from the tagged-content
            foreach {src key value} $args break
            return [dict remove $src $key]
        }
        equal { ; # check equal for each node
            foreach {src1 src2} $args break
            if {[llength $src1] != [llength $src2]} {return 0}
            foreach {key1 val1} $src1 {
                if {![dict exists $src2 $key1]} {return 0}
                if {![huddle _equal_subs $val1 [dict get $src2 $key1]]} {return 0}
            }
            return 1
        }
        append { ; # append nodes
            foreach {str src list} $args break
            if {[llength $list] % 2} {error {wrong # args: should be "huddle append objvar ?key value ...?"}}
            set resultL $src
            foreach {key value} $list {
                if {$str ne ""} {
                    lappend resultL $key [huddle to_node $value $str]
                } else {
                    lappend resultL $key $value
                }
            }
            return [eval dict create $resultL]
        }
        create { ; # $args: all arguments after "huddle create"
            if {[llength $args] % 2} {error {wrong # args: should be "huddle create ?key value ...?"}}
            set resultL {}
            foreach {key value} $args {
                lappend resultL $key [huddle to_node $value]
            }
            return [huddle wrap D $resultL]
        }
        keys {
            foreach {src nop} $args break
            return [dict keys [lindex [lindex $src 1] 1]]
        }
        default {
            error "$command is not callback for dict"
        }
    }
# __TRANSCRIBE_END__
}

proc ::huddle::_list_setting {command args} {
    switch -- $command {
        setting {
            return {
                type list
                method {list llength}
                tag {l child L parent}
                constructor list
                str s
            }
        }
        get_sub {
            foreach {src index} $args break
            return [lindex $src $index]
        }
        strip {
            foreach {src nop} $args break
            foreach {val} $src {
                lappend result [strip $val]
            }
            return $result
        }
        set {
            foreach {src index value} $args break
            lset src $index $value
            return $src
        }
        remove {
            foreach {src index value} $args break
            return [lreplace $src $index $index]
        }
        equal {
            foreach {src1 src2} $args break
            if {[llength $src1] != [llength $src2]} {return 0}
            set i 0
            foreach {val1} $src1 {
                if {![huddle _equal_subs $val1 [lindex $src2 $i]]} {return 0}
                incr i
            }
            return 1
        }
        append { ; # append nodes
            foreach {str src list} $args break
            set resultL $src
            foreach {value} $list {
                if {$str ne ""} {
                    lappend resultL [huddle to_node $value $str]
                } else {
                    lappend resultL $value
                }
            }
            return $resultL
        }
        list {
            set resultL {}
            foreach {value} $args {
                lappend resultL [huddle to_node $value]
            }
            return [huddle wrap L $resultL]
        }
        llength {
            foreach {src nop} $args break
            return [llength [lindex [lindex $src 1] 1]]
        }
        default {
            error "$command is not callback for list"
        }
    }
}

proc ::huddle::_string_setting {command args} {
    switch -- $command {
        setting {
            return {
                type string
                method {}
                tag {s child}
                constructor ""
                str s
            }
        }
        equal {
            foreach {src1 src2} $args break
            return [expr {$src1 eq $src2}]
        }
        default {
            error "$command is not callback for string"
        }
    }
}


proc ::huddle::jsondump {data {offset ""}} {
    set nextoff "$offset  "
    switch -- [huddle type $data] {
        "string" {
            set data [huddle strip $data]
            if {[regexp {^true$|^false$} $data]} {return $data}
            return "\"$data\""
        }
        "list" {
            set inner {}
            set len [huddle llength $data]
            for {set i 0} {$i < $len} {incr i} {
                set sub [huddle get $data $i]
                lappend inner [jsondump $sub $nextoff]
            }
            return [join [list "\[\n" $nextoff [join $inner ",\n$nextoff"] "\n" $offset "\]"] ""]
        }
        "dict" {
            set inner {}
            foreach {key} [huddle keys $data] {
                set val [jsondump [huddle get $data $key] $nextoff]
                lappend inner "\"$key\": $val"
            }
            if {[llength $inner] == 1 && ![regexp {[\n\{\[]]} $array]} {
                return "{ $array }"
            }
            return [join [list "\{\n" $nextoff [join $inner ",\n$nextoff"] "\n" $offset "\}"] ""]
        }
        default {
            return $data
        }
    }
}

namespace eval ::huddle {
    array set methods {}
    array set types {}
    array set callbacks {}
    ::huddle::addType ::huddle::_dict_setting
    ::huddle::addType ::huddle::_list_setting
    ::huddle::addType ::huddle::_string_setting
}



