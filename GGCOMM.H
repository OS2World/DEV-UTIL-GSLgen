/*===========================================================================*
 *                                                                           
 *  ggcomm.h    GSLgen Common Functions                    
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

#ifndef GGCOMM_INCLUDED                 /*  Allow multiple inclusions        */
#define GGCOMM_INCLUDED

/*---------------------------------------------------------------------------*/

#include "sfl.h"                        /*  Universal include file           */

/*- Macros ------------------------------------------------------------------*/

#define LONG_WIDTH 10
#define PATH       "PATH"


/*- Type definitions --------------------------------------------------------*/

typedef struct _SCHEMA_LINE SCHEMA_LINE;
typedef struct _SCHEMA_FILE SCHEMA_FILE;

struct _SCHEMA_FILE {
    SCHEMA_FILE  *next,
                 *prev;
    char         *name;
    FILE         *file;
    char         *text,
                 *ptr;
    LIST          line_head;
    SCHEMA_LINE  *cur_line;
    int           next_line;
    Bool          eof;
};

struct _SCHEMA_LINE {
    SCHEMA_LINE  *next,
                 *prev;
    SCHEMA_FILE *parent;
    int           line;
    SCHEMA_NODE  *node;
};

typedef struct _SCOPE_BLOCK {
    struct _SCOPE_BLOCK
         *next,
         *prev;
    char *name;                         /*  Scope name                       */
    XML_ITEM
         *xml_item;                     /*  Attributes are data in scope     */
} SCOPE_BLOCK;

typedef struct {
    void
        *next,
        *prev;
    XML_ITEM
        *xml_item;                      /*  Data for this item               */
    char
        *sort_key;                      /*  Calculated from sort expression  */
    long
        index;                          /*  Object index before filter/sort  */
} FOR_ITEM;

typedef struct _FOR_BLOCK {
    struct _FOR_BLOCK
        *next,
        *prev;
    LIST
        item_list;                      /*  List of items to iterate through */
    FOR_ITEM
        *for_item;                      /*  Current item in for list         */
    SCOPE_BLOCK
        *scope_block;                   /*  Corresponding scope for loop     */
} FOR_BLOCK;

typedef enum {
    CTL_UNDEFINED,
    CTL_IF,
    CTL_ELSE,
    CTL_FOR,
    CTL_WHILE,
    CTL_MACRO,
    CTL_NEW
} CONTROL_TYPE;

typedef struct {
    CONTROL_TYPE  type;
    SCHEMA_NODE  *condition;            /*  For .if/.while control blocks    */
    SCHEMA_LINE  *position;
} CONTROL_BLOCK;

typedef struct {
    SCHEMA_LINE *position;
    LIST         control_stack;
} SCHEMA_BLOCK;
    
typedef struct {                        /*  Store reference for macros       */
    char        *name;
    SCHEMA_NODE *args;
    SCHEMA_LINE *position;
} MACRO;


/*- Global variables --------------------------------------------------------*/

extern int
    shuffle,
    feedback,
    line_length;
extern SCHEMA_FILE
    *cur_schema_file;
extern SCHEMA_BLOCK
    cur_schema_block;
extern char
    *me;
extern LIST
    scope_stack,
    for_stack,
    schema_stack,
    schema_list;
extern Bool
    ignorecase;


/*- Functions ---------------------------------------------------------------*/

Bool         open_schema_file           (char *name);
void         open_schema_text           (char *name, char *text);
void         close_schema_file          (void);
Bool         parse_next_schema_line     (void);
SCHEMA_LINE *schema_position            (void);
void         restore_position           (SCHEMA_LINE *position);
void         destroy_schema_data        (void);

void         reset_scope_stack          (void);
SCOPE_BLOCK *create_scope_block         (const char  *name);
void         destroy_scope_block        (void);
XML_ITEM    *lookup_scope_xml           (char *name);
XML_ITEM    *first_scope_xml            (void);
XML_ITEM    *last_scope_xml             (void);
char        *valueof                    (char *scope, char *name);
void         put_numeric_attr           (XML_ITEM *item,
                                         const char *name, const long value);

void         reset_for_stack            (void);
FOR_BLOCK   *create_for_block           (const char *alias);
void         destroy_for_block          (void);
FOR_BLOCK   *lookup_for_block           (char *name);
FOR_BLOCK   *last_for_block             (void);

FOR_ITEM    *create_for_item            (FOR_BLOCK *for_block,
                                         XML_ITEM *xml_item,
                                         long index);
void         destroy_for_item           (FOR_ITEM *for_item);

Bool         first_for_item             (FOR_BLOCK *for_block);
Bool         next_for_item              (FOR_BLOCK *for_block);

void         reset_schema_stack       (void);
void         push_schema_block        (void);
void         pop_schema_block         (void);

void         push_control             (CONTROL_TYPE type,
                                       SCHEMA_NODE *condition);
void         pop_control              (CONTROL_BLOCK *control);

void         report_error             (char type, char *format, ...);

/*---------------------------------------------------------------------------*/

#endif
