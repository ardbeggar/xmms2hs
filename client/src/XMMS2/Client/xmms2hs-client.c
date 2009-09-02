/* XMMS2 client library.

   Author:  Oleg Belozeorov
   Created: 2 Sep. 2009

   Copyright (C) 2009 Oleg Belozeorov

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. */

#include <HsFFI.h>
#include <xmms2hs-client.h>


static void
xmms2hs_finalize_result_notifier (void *notifier)
{
  hs_free_fun_ptr ((HsFunPtr) notifier);
}

void
xmms2hs_result_notifier_set (xmmsc_result_t *res,
							 xmmsc_result_notifier_t func)
{
  xmmsc_result_notifier_set_full (res, func, (void *) func, xmms2hs_finalize_result_notifier);
}
