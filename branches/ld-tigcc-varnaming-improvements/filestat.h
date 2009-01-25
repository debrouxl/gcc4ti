/* filestat.h: Definitions for file statistics

   Copyright (C) 2002-2003 Sebastian Reichelt

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#ifndef FILESTAT_H
#define FILESTAT_H

#include "generic.h"

#include <sys/stat.h>

#ifdef WIN32
#undef stat
#define stat _stat
#endif

typedef struct stat FILE_STATS;

#define GetFileStats(FileName,Stats) (stat ((FileName), &(Stats)))

#define StatGetMode(Stat) ((Stat).st_mode)
#define StatGetUID(Stat) ((Stat).st_uid)
#define StatGetGID(Stat) ((Stat).st_gid)
#define StatGetModificationTime(Stat) ((Stat).st_mtime)

#define StatFileIsNewer(Stat1,Stat2) ((Stat1).st_mtime > (Stat2).st_mtime)
#define StatCopyAttributes(Stat1,Stat2) ((Stat1).st_mode = (Stat2).st_mode, (Stat1).st_uid = (Stat2).st_uid, (Stat1).st_gid = (Stat2).st_gid, (Stat1).st_atime = (Stat2).st_atime, (Stat1).st_mtime = (Stat2).st_mtime, (Stat1).st_ctime = (Stat2).st_ctime)

#endif
