/* tree.h: Definitions for trees

   Copyright (C) 2008 Lionel Debroux

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

#ifndef TREE_H
#define TREE_H

#include "lists.h"

#define TREE_HEADER LIST_HEADER
#define TREE_ITEM_HEADER LIST_ITEM_HEADER

#define TreePrev GetPrev
#define TreeNext GetNext

#define TreeFirst GetFirst
#define TreeLast GetLast

#define TreeIsEmpty IsEmpty

#define TreeUnlink Unlink

#define TreeAppend Append

#define TreePush Push

#define TreeInsertAfter InsertAfter

#define TreeInsertBefore InsertBefore

#define tree_for_each for_each

#define TreeCountItems CountItems

#endif
