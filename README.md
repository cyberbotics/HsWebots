# HsWebots

Webots bindings for Haskell

# Usage

```
#setup WEBOTS_HOME and LD_LIBRARY_PATH
source setenv 

#Build
stack build

#When you generate haskell-code from c-header, type following command.
stack run "c-header"
```

# Supported Platform

* Webots: R2020a revision 1
* OS: ubuntu/bionic and macos(Mojave)
* Build Tools: stack
