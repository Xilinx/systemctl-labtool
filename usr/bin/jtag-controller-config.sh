#!/bin/bash
# Generate AVC JTAG config file based on detected UIO devices

CONFIG_FILE="/dev/xavc_jc_proxy0"

# Find UIO device number by name
find_uio() {
    local name="$1"
    for uio in /sys/class/uio/uio*; do
        if [ -L "$uio" ] && readlink "$uio" | grep -q "$name"; then
            basename "$uio"
            return 0
        fi
    done
    return 1
}

JTAG_CLK_UIO=$(find_uio "jtag_clock")
JTAG_CTL_UIO=$(find_uio "jtag_ctl")

if [ -z "$JTAG_CLK_UIO" ] || [ -z "$JTAG_CTL_UIO" ]; then
    echo "Error: Could not find required UIO devices"
    echo "  jtag_clock: $JTAG_CLK_UIO"
    echo "  jtag_ctl: $JTAG_CTL_UIO"
    exit 1
fi

cat > "$CONFIG_FILE" << EOF
{
    "JTAGFilepath": "/dev/$JTAG_CTL_UIO",
    "MMCMFilepath": "/dev/$JTAG_CLK_UIO",
    "TDOFilepath": "/dev/mem",
    "TDOAXIAddress": "0x7fe00000",
    "TDOSize": "0x100000",
    "TDOOffset": "0x7fe00000",
    "InputClk": 100000000,
    "PortCount": 1,
    "UseIOC": false
}
EOF

echo "Generated $CONFIG_FILE"
cat "$CONFIG_FILE"
