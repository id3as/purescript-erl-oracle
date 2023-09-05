
The nix oci-cli is slightly out of date (3.14.0), but our usage is pretty basic at present

https://docs.oracle.com/en-us/iaas/tools/oci-cli/3.29.1/oci_cli_docs/cmdref/compute.html

Create ~/.oci
Add Keys & make the config file:

https://docs.oracle.com/en-us/iaas/Content/API/Concepts/apisigningkey.htm
grep


Get the tenancy (= compartment id) from the config file
grep tenancy ~/.oci/config

Create an output config file ~/.oci/oci_cli_rc

[DEFAULT]
compartment-id = ocid1.tenancy.oc1..whatever
output = json
