/*===========================================================================*
 *                                                                           *
 *  ggcode.h    Studio Wizard Generator                                      *
 *                                                                           *
 *  Written:    98/07/03    iMatix <tools@imatix.com>                        *
 *  Revised:    99/06/02                                                     *
 *                                                                           *
 *   Copyright (c) 1996-98 iMatix                                            *
 *                                                                           *
 *   This software is copyright (c) 1996-98 iMatix.  All rights reserved.    *
 *   Use and distribution of this software, documentation, executables,      *
 *   source files, and object files is only permitted in the context of a    *
 *   current License Agreement between the user and iMatix.                  *
 *                                                                           *
 *===========================================================================*/


#ifndef GGCOMM_INCLUDED                /*  Allow multiple inclusions        */
#define GGCOMM_INCLUDED

/*---------------------------------------------------------------------------*/

/*- Function prototype ------------------------------------------------------*/

int  gg_code          (XML_ITEM *xml_tree);
void gg_send_stdout   (CONSOLE_FCT *console_fct, Bool echo);


/*---------------------------------------------------------------------------*/

#endif
