# Installation Guide

One can install the MaZX package using the `paclet` system that has recently beeen introduced by Wolfram Research.

## Requirements

* [Mathematica v12.2 or later](https://www.wolfram.com/mathematica)
* [Q3 Application](https://github.com/quantum-mob/Q3)


## Installation

Copy the following code, and just evaluate it in your Mathematica(R) Notebook:

```Mathematica
Module[
  { ps },
  ps = PacletSiteRegister[
    "https://github.com/quantum-mob/PacletServer/raw/main",
    "Quantum Mob Paclet Server"
   ];
  PacletSiteUpdate[ps];
  PacletInstall["Q3"];
  PacletInstall["MaZX"]
 ]
```

Note that along with MaZX, it also installs the main application Q3 for your convenience.


## Update

Once installed, MaZX will automatically check for updates and install the newest update (if any).

## Uninstall

If you do not want to use MaZX any longer, just evaluate the following code:

```Mathematica
PacletUninstall["MaZX"]
```
