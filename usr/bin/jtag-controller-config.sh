#!/bin/bash
# Generate AVC JTAG config file based on detected UIO devices and DMA heap

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

# Find the jtag_tdo_buffer DMA heap and extract address from name
find_tdo_heap() {
    for heap in /dev/dma_heap/jtag_tdo_buffer@*; do
        if [ -e "$heap" ]; then
            echo "$heap"
            return 0
        fi
    done
    return 1
}

# Extract address from heap name (e.g., jtag_tdo_buffer@7f800000 -> 0x7f800000)
extract_address() {
    local heap_path="$1"
    local heap_name
    heap_name=$(basename "$heap_path")
    # Extract the hex address after @
    echo "0x${heap_name##*@}"
}

JTAG_CLK_UIO=$(find_uio "jtag_clock")
JTAG_CTL_UIO=$(find_uio "jtag_ctl")
TDO_HEAP=$(find_tdo_heap)

if [ -z "$JTAG_CLK_UIO" ] || [ -z "$JTAG_CTL_UIO" ]; then
    echo "Error: Could not find required UIO devices"
    echo "  jtag_clock: $JTAG_CLK_UIO"
    echo "  jtag_ctl: $JTAG_CTL_UIO"
    exit 1
fi

if [ -z "$TDO_HEAP" ]; then
    echo "Error: Could not find jtag_tdo_buffer DMA heap in /dev/dma_heap/"
    ls -la /dev/dma_heap/ 2>/dev/null
    exit 1
fi

TDO_ADDRESS=$(extract_address "$TDO_HEAP")

cat > "$CONFIG_FILE" << EOF
{
    "JTAGFilepath": "/dev/$JTAG_CTL_UIO",
    "MMCMFilepath": "/dev/$JTAG_CLK_UIO",
    "TDOFilepath": "$TDO_HEAP",
    "TDOAXIAddress": "$TDO_ADDRESS",
    "TDOSize": "0x100000",
    "InputClk": 100000000,
    "PortCount": 1,
    "UseIOC": false
}
EOF

echo "Generated $CONFIG_FILE"
cat "$CONFIG_FILE"
