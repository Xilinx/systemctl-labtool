# systemctl-labtool-yocto

This repo contains all the labtool binaries and scripts which will be used by:

https://gitenterprise.xilinx.com/Yocto/meta-petalinux/tree/2020/recipes-utils/labtool-jtag-support

The Labtool binaries (hw_Server, xsdb) is covered under "Permissive Binary Only" license: LICENSE_PBO

The xsdb TCL scripts are covered under "Xilinx MIT" License: LICENSE_TCL

To use XSDB with system controller (This might not be required if the script was sourced on boot):

```
source /etc/init.d/xsdb
```
Then run xsdb from the following path:
``` 
rlwrap /usr/local/xilinx_vitis/xsdb
```
In xsdb, connect to the hw_server and xvc_server setup with the following command:
```
connect -xvc-url TCP:127.0.0.1:2542
```
Make sure you have configured system-controller JTAG lines to route to the target device
