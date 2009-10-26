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

#ifndef XMMS2HS_CLIENT_H
#define XMMS2HS_CLIENT_H

#include <xmmsclient/xmmsclient.h>


extern void xmms2hs_result_notifier_set (xmmsc_result_t *, xmmsc_result_notifier_t);

extern int xmms2hs_get_list_iter (xmmsv_t *, xmmsv_list_iter_t **);
extern void xmms2hs_finalize_list_iter (xmmsv_list_iter_t *);


typedef struct {
  xmmsv_t *parent;
  xmmsv_dict_iter_t *iter;
} xmms2hs_dict_iter_t;

extern int xmms2hs_get_dict_iter (xmmsv_t *, xmms2hs_dict_iter_t **);
extern void xmms2hs_finalize_dict_iter (xmms2hs_dict_iter_t *);


#endif /* XMMS2HS_CLIENT_H */
