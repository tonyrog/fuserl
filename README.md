Erlang bindings for FUSE
========================

Currently the low-level FUSE interface is supported.

This version of fuserl is rebarified. Also, the driver and the erlang
part is kept in the same repo, to have a simplified build process.

# Linux 

You need to install libfuse-dev

    sudo apt-get install libfuse-dev

To use fuse a bit more relaxed ( while lower the sceurity ? )
then add your self to the fuse group and add user_allow_other in
the fuse.conf.

    sudo usermod -a -G fuse $USER


# Mac os x

You need to install osxfuse. You can find the OSX package
at http://osxfuse.github.io.

# Build

    rebar compile
