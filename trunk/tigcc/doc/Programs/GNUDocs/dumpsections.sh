#!/bin/bash

# TIGCC Documentation Tools
#
# Copyright (C) 2002-2004 Sebastian Reichelt
# Copyright (C) 2008 Kevin Kofler
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.

rm -f DumpSections.out
for f in "$1"/*.hss ; do
  title=`grep '^Title=' "$f" | sed -e 's/^Title=//g'`
  if [ -n "$title" ] ; then
    case "$title" in
      .*)
        echo '  else if Pos('\'"$title"\'', Ref) > 0 then' >>DumpSections.out
        ;;
      *)
        case "$1" in
          *cpp*)
            echo '  else if Ref = '\'"$title"\'' then' >>DumpSections.out
            ;;
          *)
            echo '  else if Ref = '\'"$title,,$title"\'' then' >>DumpSections.out
            ;;
        esac
        ;;
    esac
    fb=`basename "$f"`
    echo '    Result := '\'`basename $1`/"${fb%.hss}"\' >>DumpSections.out
  fi
done
