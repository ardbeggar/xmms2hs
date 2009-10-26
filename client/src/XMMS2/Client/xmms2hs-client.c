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
#include <stdlib.h>
#include <xmmsc/xmmsc_util.h>
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


extern int
xmms2hs_get_list_iter (xmmsv_t *value, xmmsv_list_iter_t **iter)
{
  int res = xmmsv_get_list_iter (value, iter);
  
  if (res)
	xmmsv_ref (value);
  
  return res;
}

extern void
xmms2hs_finalize_list_iter (xmmsv_list_iter_t *iter)
{
  xmmsv_t *parent = xmmsv_list_iter_get_parent (iter);

  xmmsv_list_iter_explicit_destroy (iter);
  xmmsv_unref (parent);
}

extern int
xmms2hs_get_dict_iter (xmmsv_t *value, xmms2hs_dict_iter_t **iter)
{
  xmms2hs_dict_iter_t *result;
  xmmsv_dict_iter_t *xi;
  
  if (xmmsv_get_dict_iter (value, &xi)) {
	*iter = x_malloc (sizeof (xmms2hs_dict_iter_t));
	if (!*iter)
	  x_oom ();

	(*iter)->parent = value;
	(*iter)->iter = xi;
	xmmsv_ref (value);

	return 1;
  }
  
  return 0;
}

extern void
xmms2hs_finalize_dict_iter (xmms2hs_dict_iter_t *iter)
{
  xmmsv_dict_iter_explicit_destroy (iter->iter);
  xmmsv_unref (iter->parent);
  free (iter);
}
