# HsWebots

[![Hackage version](https://img.shields.io/hackage/v/HsWebots.svg?style=flat)](https://hackage.haskell.org/package/HsWebots)
![linux](https://github.com/cyberbotics/HsWebots/workflows/stack-linux/badge.svg)
![macos](https://github.com/cyberbotics/HsWebots/workflows/stack-macos/badge.svg)



Webots bindings for Haskell

# Usage

```
#setup WEBOTS_HOME and LD_LIBRARY_PATH
source setenv 

#Build
stack build

#When you generate haskell-code from c-header, type following command.
stack build --flag HsWebots:codegen
stack run "c-header"
```

# Supported Platform

* Webots: R2020a revision 1
* OS: ubuntu/bionic and macos(Mojave)
* Build Tools: stack
