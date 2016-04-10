#!/bin/sh

oldecho=`echo -n test | grep '\-n'`
if [ "x$oldecho" = "x" ]; then
    #This echo supports '-n' (no newline)
    echon="echo -n"
else
    echon=echo
fi

if test -z "$DISPLAY" ; then
    echo "No X server."
else
    if test "x$1" != "x--run-myself" ; then
        exec xterm -e $0 --run-myself
    fi
fi

echo "GCC4TI Installation Wizard"

I_AM_ROOT="`id | grep root`"

# Get GCC4TI prefix directory
if test -z "$I_AM_ROOT" ; then
    PREFIX_GCC4TI=$HOME/gcc4ti
else
    PREFIX_GCC4TI="/usr/local/share/gcc4ti"
fi
${echon} "Destination directory ($PREFIX_GCC4TI) - will become \$TIGCC: "
read PREFIX_GCC4TInew
PREFIX_GCC4TInew=`eval echo "$PREFIX_GCC4TInew"`
if [ ! -z "$PREFIX_GCC4TInew" ]; then
    PREFIX_GCC4TI="$PREFIX_GCC4TInew"
fi
export PREFIX_GCC4TI

# copy the file except setup.sh
mkdir -p "$PREFIX_GCC4TI"
cp -r . "$PREFIX_GCC4TI"
rm -f "$PREFIX_GCC4TI"/setup.sh

# Now offer to install environment variables
${echon} "Add environment settings (\$TIGCC, \$PATH) to bashrc [Y/n]? "
read AddEnvSettings
echo
if [ -z "$AddEnvSettings" -o "$AddEnvSettings" = y -o "$AddEnvSettings" = Y ]
then
    TIGCC="$PREFIX_GCC4TI"
    export TIGCC
    "$PREFIX_GCC4TI/bin/envreg"
    echo "Done. You must restart bash for the new environment settings to take effect."
    exit 0;
fi
echo "Done. Make sure you set \$TIGCC to \"$PREFIX_GCC4TI\" and add \$TIGCC/bin to your \$PATH before using GCC4TI."
exit 0
