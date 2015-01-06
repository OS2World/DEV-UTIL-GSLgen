/*===========================================================================*
 *                                                                           
 *  ggeval.h    GSLgen Schema Parser Header                
 *                                                                           
 *  Written:    99/02/28    iMatix <tools@imatix.com>                        
 *  Revised:    99/07/28
 *                                                                           
 *  Copyright (c) 1996-99 iMatix Corporation                                  
 *                                                                            
 *  This program is free software; you can redistribute it and/or modify      
 *  it under the terms of the GNU General Public License as published by      
 *  the Free Software Foundation; either version 2 of the License, or         
 *  (at your option) any later version.                                       
 *                                                                            
 *  This program is distributed in the hope that it will be useful,           
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of            
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             
 *  GNU General Public License for more details.                              
 *                                                                            
 *  You should have received a copy of the GNU General Public License         
 *  along with this program; if not, write to the Free Software               
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                 
 *                                                                           
 *===========================================================================*/

#ifndef GGEVAL_INCLUDED                 /*  Allow multiple inclusions        */
#define GGEVAL_INCLUDED

/*---------------------------------------------------------------------------*/

#include "sfl.h"                        /*  Universal include file           */
#include "ggcomm.h"                     /*  Common declarations              */
#include "ggeval.h"                     /*  Evaluation functions             */
#include "ggpars.h"                     /*  Schema parser header file        */

/*- Functions ---------------------------------------------------------------*/

int evaluate_schema      (SCHEMA_NODE *node);
int evaluate_schema_node (SCHEMA_NODE *node);


/*---------------------------------------------------------------------------*/

#endif



