/*===========================================================================*
 *                                                                           
 *  ggpars.h    GSLgen Schema Parser Header                                  
 *                                                                           
 *  Written:    98/04/07    iMatix <tools@imatix.com>                        
 *  Revised:    99/06/02                                                     
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
 *===========================================================================*/

#ifndef GGPARS_INCLUDED                 /*  Allow multiple inclusions        */
#define GGPARS_INCLUDED

/*---------------------------------------------------------------------------*/

#include "sfl.h"                        /*  Universal include file           */


/*- Type definitions --------------------------------------------------------*/

typedef struct _SCHEMA_NODE        SCHEMA_NODE;
typedef struct _SUBSTITUTE_OBJECT  SUBSTITUTE_OBJECT;

typedef enum { GG_COMMENT,
               GG_TEXT,
               GG_SUBSTITUTE,
               GG_LITERAL,
               GG_SYMBOL,
               GG_MEMBER,
               GG_FUNCTION,
               GG_OPERAND,
               GG_OPERATOR,
               GG_CLOSE,
               GG_ELSE,
               GG_END_IF,
               GG_END_FOR,
               GG_END_MACRO,
               GG_END_NEW,
               GG_END_WHILE,
               GG_DUMP,
               GG_OUTPUT,
               GG_APPEND,
               GG_INCLUDE,
               GG_INTERPRET,
               GG_DELETE,
               GG_COPY,
               GG_RENAME,
               GG_ECHO,
               GG_DEFINE,
               GG_MACRO,
               GG_INVOKE,
               GG_XML,
               GG_NEW,
               GG_IF,
               GG_ELSIF,
               GG_FOR,
               GG_WHILE,
               GG_ABORT,
               GG_UNDEFINED }
    SCHEMA_NODE_TYPE;


typedef enum { OP_UNDEFINED,
               TIMES,
               DIVIDE,
               PLUS,
               MINUS,
               EQUALS,
               NOT_EQUALS,
               GREATER_THAN,
               LESS_THAN,
               GREATER_EQUAL,
               LESS_EQUAL,
               SAFE_EQUALS,
               SAFE_NOT_EQUALS,
               SAFE_GREATER_THAN,
               SAFE_LESS_THAN,
               SAFE_GREATER_EQUAL,
               SAFE_LESS_EQUAL,
               OR,
               AND,
               NOT,
               NEXT_ARG }
    OPERATOR;

struct _SCHEMA_NODE {
    SCHEMA_NODE_TYPE
        type;
    SCHEMA_NODE
        *parent;
    int
        brackets;
    int  
        length,                         /*  Length of schema construct      */
        spaces;                         /*  Leading spaces in schema        */
    int
        indent;                         /*  Leading spaces after shuffling  */
    int
        shuffle_cnt;                    /*  Shuffle count after this node   */
    char
        *result;                        /*  Result when evaluated           */
    char 
        *text;                          /*  Text-only node                  */
    SCHEMA_NODE
        *op1,                           /*  Also name, literal value        */
        *op2,                           /*  Also scope of identifier        */
        *op3,                           /*  Also 'as' expr, default value   */
        *pretty,                        /*  Also 'where' expression         */
        *format,                        /*  also 'by' expression            */
        *culprit;                       /*  for undefined expression        */
    OPERATOR
        operator;
    Bool
        extend,
        line_break;
};

/*  Function type for function to read next line of schema                   */

typedef Bool (SCHEMA_READ) (char *text);


/*- Macros ------------------------------------------------------------------*/
            
#define init_schema_node(n)                                                   \
    (n). type       = GG_UNDEFINED;                                           \
    (n). parent     = NULL;                                                   \
    (n). brackets   = 0;                                                      \
    (n). length     = 0;                                                      \
    (n). spaces     = 0;                                                      \
    (n). result     = NULL;                                                   \
    (n). text       = NULL;                                                   \
    (n). op1        = NULL;                                                   \
    (n). op2        = NULL;                                                   \
    (n). op3        = NULL;                                                   \
    (n). pretty     = NULL;                                                   \
    (n). format     = NULL;                                                   \
    (n). culprit    = NULL;                                                   \
    (n). operator   = OP_UNDEFINED;                                           \
    (n). extend     = FALSE;                                                  \
    (n). line_break = FALSE;

/*  Prototypes  */
void         gg_free             (SCHEMA_NODE *node);
void         gg_clean            (SCHEMA_NODE *node);
void         print_node_type     (FILE *stream, 
                                  int level, 
                                  SCHEMA_NODE_TYPE type);
void         gg_print            (FILE *stream, SCHEMA_NODE *node);
char        *gg_error            (void);
SCHEMA_NODE *gg_parse_schema     (SCHEMA_READ *read);
SCHEMA_NODE *gg_parse_expression (char *line);

/*---------------------------------------------------------------------------*/

#endif
