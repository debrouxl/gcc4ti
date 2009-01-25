/* lists.h: Definitions for linked lists

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

#ifndef LISTS_H
#define LISTS_H

#define LIST_HEADER(Type) struct { Type *First, *Last; } Header
#define LIST_ITEM_HEADER(Type) struct { Type *Prev, *Next; } Header

#define GetPrev(Item) ((Item)->Header.Prev)
#define GetNext(Item) ((Item)->Header.Next)

#define GetFirst(List) ((List).Header.First)
#define GetLast(List)  ((List).Header.Last)

#define IsEmpty(List) (!(GetFirst (List)))

#define Unlink(List,Item) ({ \
	if ((Item) == GetFirst (List)) \
		(List).Header.First = GetNext (Item); \
	if ((Item) == GetLast (List)) \
		(List).Header.Last = GetPrev (Item); \
	if (GetPrev (Item)) \
		GetPrev(Item)->Header.Next = GetNext (Item); \
	if (GetNext (Item)) \
		GetNext(Item)->Header.Prev = GetPrev (Item); \
	(Item)->Header.Prev = NULL; \
	(Item)->Header.Next = NULL; \
})

#define Append(List,Item) ({ \
	(Item)->Header.Prev = GetLast (List); \
	(Item)->Header.Next = NULL; \
	if (!(GetFirst (List))) \
		(List).Header.First = (Item); \
	if (GetLast (List)) \
		GetLast(List)->Header.Next = (Item); \
	(List).Header.Last = (Item); \
})

#define Push(List,Item) ({ \
	(Item)->Header.Prev = NULL; \
	(Item)->Header.Next = GetFirst (List); \
	if (GetFirst (List)) \
		GetFirst(List)->Header.Prev = (Item); \
	(List).Header.First = (Item); \
	if (!(GetLast (List))) \
		(List).Header.Last = (Item); \
})

#define InsertAfter(List,Item,After) ({ \
	if (!(After)) \
		Push (List, Item); \
	else \
	{ \
		(Item)->Header.Next = GetNext (After); \
		(Item)->Header.Prev = (After); \
		(After)->Header.Next = (Item); \
		if (GetNext (Item)) \
			GetNext(Item)->Header.Prev = (Item); \
		else \
			(List).Header.Last = (Item); \
	} \
})

#define InsertBefore(List,Item,Before) ({ \
	if (!(Before)) \
		Append (List, Item); \
	else \
		InsertAfter (List, Item, GetPrev (Before)); \
})

#define for_each(Item,List) for ((Item) = GetFirst (List); (Item); (Item) = GetNext (Item))

#define CountItems(List,Type) ({ \
	unsigned long Result = 0; \
	Type *Item; \
	for_each (Item, List) \
		Result++; \
	Result; \
})

#endif
