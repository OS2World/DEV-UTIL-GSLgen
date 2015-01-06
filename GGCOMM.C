/*===========================================================================
 *                                                                           
 *  ggcomm.c    GSLgen common functions                     
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
 *===========================================================================*/

#include "sfl.h"                        /*  Universal include file           */
#include "ggpars.h"                     /*  Schema parser header file        */
#include "ggcomm.h"                     /*  Header file                      */

/*- Global variables --------------------------------------------------------*/

int
    shuffle,
    feedback,
    line_length;
SCHEMA_FILE
    *cur_schema_file = NULL;
SCHEMA_BLOCK
    cur_schema_block = {NULL, {& cur_schema_block. control_stack, 
                               & cur_schema_block. control_stack} };
char
    *me = "";
LIST
    scope_stack,
    for_stack,
    schema_stack,
    schema_list = {& schema_list, & schema_list};  /*  Empty list  */
Bool
    ignorecase = TRUE;


/*- Prototypes --------------------------------------------------------------*/

static Bool schema_read (char *text);
static Bool next_line   (char **line, char *text);


/*- Schema Functions --------------------------------------------------------*/

Bool
open_schema_file (char *name)
{
    SCHEMA_FILE
        *schema_file;

    cur_schema_file = NULL;
    FORLIST (schema_file, schema_list)
        if (streq (schema_file-> name, name))
          {
            cur_schema_file = schema_file;
            break;
          }

    if (! cur_schema_file)
      {
        /*  Open unopened schema file  */
        list_create (schema_file, sizeof (SCHEMA_FILE));
        ASSERT (schema_file);
        schema_file-> file = file_locate (PATH, name, NULL);
        if (!schema_file-> file)
          {
            mem_free (schema_file);
            return FALSE;
          }
        
        schema_file-> name      = mem_strdup (name);
        schema_file-> text      = NULL;
        schema_file-> ptr       = NULL;
        schema_file-> next_line = 1;
        schema_file-> eof       = FALSE;
        list_reset (& schema_file-> line_head);

        list_relink_after (schema_file, & schema_list);
        cur_schema_file = schema_file;
      }

    cur_schema_file-> cur_line = (SCHEMA_LINE *) & cur_schema_file-> line_head;
    return TRUE;
}


void
open_schema_text (char *name, char *text)
{
    SCHEMA_FILE
        *schema_file;

    list_create (schema_file, sizeof (SCHEMA_FILE));
    ASSERT (schema_file);

    schema_file-> file      = NULL;
    schema_file-> name      = mem_strdup (name);
    schema_file-> text      = text;
    schema_file-> ptr       = text;
    schema_file-> next_line = 1;
    schema_file-> eof       = FALSE;
    list_reset (& schema_file-> line_head);

    list_relink_after (schema_file, & schema_list);

    cur_schema_file = schema_file;
    cur_schema_file-> cur_line = (SCHEMA_LINE *) & cur_schema_file-> line_head;
}


void
close_schema_file (void)
{
    mem_strfree (& cur_schema_file-> text);
    cur_schema_file = NULL;
}


Bool
parse_next_schema_line (void)
{
    SCHEMA_LINE
        *schema_line;

    /*  Clean up result of evaluation last parse tree  */
    if ((void *) cur_schema_file-> cur_line != & cur_schema_file-> line_head)
        gg_clean (cur_schema_file-> cur_line-> node);

    cur_schema_file-> cur_line = cur_schema_file-> cur_line-> next;
    /*  Skip lines with no parse trees.  These are those with parse errors  */
    while (((void *) cur_schema_file-> cur_line != & cur_schema_file-> line_head)
       && (!cur_schema_file-> cur_line-> node))
        cur_schema_file-> cur_line = cur_schema_file-> cur_line-> next;

    if ((void *) cur_schema_file-> cur_line == & cur_schema_file-> line_head)
      {
        list_create (schema_line, sizeof (SCHEMA_LINE));
        ASSERT (schema_line);
        
        schema_line-> parent = cur_schema_file;
        schema_line-> line   = cur_schema_file-> next_line;
        schema_line-> node   = gg_parse_schema (schema_read);
        if (cur_schema_file-> next_line == schema_line-> line)
          {
            mem_free (schema_line);
            return FALSE;
          }
        list_relink_before (schema_line, & cur_schema_file-> line_head);
        cur_schema_file-> cur_line = schema_line;
      }
    return TRUE;
}


static Bool
schema_read (char *text)
{
    char
        line [LINE_MAX + 1];

    if (cur_schema_file-> file)
      {
        if (!file_read (cur_schema_file-> file, line))
          {
            cur_schema_file-> eof = TRUE;
            return FALSE;
          }
        strncpy (text, line, LINE_MAX);
      }
    else
      {
        text [0] = '\0';
        if (!next_line (&cur_schema_file-> ptr, text))
          {
            cur_schema_file-> eof = TRUE;
            return FALSE;
          }
      }
    cur_schema_file-> next_line++;
    text [LINE_MAX] = '\0';             /*  Just in case  */
    return TRUE;
}


static Bool
next_line (char **line, char *text)
{
    char
        *pointer;
    int
        len,
        newlen;

    /* Handle blank line separately because strtok doesn't like empty tokens */
    if ((*line) [0] == '\0')
        return FALSE;
    else
      {
        pointer = strchr (*line, '\n');
        if (pointer)
            len = pointer - *line;
        else
            len = strlen (*line);
        
        newlen = strlen (text) + len;

        if (newlen > LINE_MAX)
          {
            report_error ('E', "Extended input line too long.");
            return FALSE;
          }
        
        strncat (text, *line, len);
        text [newlen] = '\0';

        if (pointer)
            (*line) = pointer + 1;
        else
            (*line) += len;
      }
    return TRUE;
}


SCHEMA_LINE *
schema_position (void)
{
    if (cur_schema_file)
        return cur_schema_file-> cur_line;
    else
        return NULL;
}


void
restore_position (SCHEMA_LINE *position)
{
    cur_schema_file            = position-> parent;
    cur_schema_file-> cur_line = position;
}


void
destroy_schema_data (void)
{
    SCHEMA_FILE
        *schema_file;
    SCHEMA_LINE
        *schema_line;

    while (! list_empty (& schema_list))
      {
        schema_file = (SCHEMA_FILE *) schema_list. prev;
        if (schema_file-> file)
            file_close (schema_file-> file);
        mem_free (schema_file-> name);
        mem_free (schema_file-> text);
        while (! list_empty (& schema_file-> line_head))
          {
            schema_line = (SCHEMA_LINE *) schema_file-> line_head. prev;
            gg_free (schema_line-> node);
            list_unlink (schema_line);
            mem_free (schema_line);
          }
        list_unlink (schema_file);
        mem_free (schema_file);
      }
}          


/*- Scope Functions ---------------------------------------------------------*/

void
reset_scope_stack (void)
{
    list_reset (& scope_stack);
}


SCOPE_BLOCK *
create_scope_block (const char *name)
{
    SCOPE_BLOCK
        *scope_block;

    list_create (scope_block, sizeof (SCOPE_BLOCK));
    ASSERT (scope_block);
    list_relink_after (scope_block, &scope_stack);

    scope_block-> name     = mem_strdup (name);
    scope_block-> xml_item = NULL;

    return scope_block;
}


void 
destroy_scope_block (void)
{
    SCOPE_BLOCK 
        *scope_block;

    scope_block = scope_stack.next;

    list_unlink (scope_block);

    mem_free (scope_block-> name);
    mem_free (scope_block);
}
        

XML_ITEM *
lookup_scope_xml (char *name)
{
    SCOPE_BLOCK
        *scope_block = NULL;

    if (name)
        if (ignorecase)
          {
            FORLIST (scope_block, scope_stack)
                if (lexcmp (scope_block-> name, name) == 0)
                    return scope_block-> xml_item;
          }
        else
          {
            FORLIST (scope_block, scope_stack)
                if (streq (scope_block-> name, name))
                    return scope_block-> xml_item;
          }
    return NULL;
}


XML_ITEM *
first_scope_xml (void)
{
    if (!list_empty (&scope_stack))
        return ((SCOPE_BLOCK *) scope_stack.prev) -> xml_item;
    else
        return NULL;
}


XML_ITEM *
last_scope_xml (void)
{
    if (!list_empty (&scope_stack))
        return ((SCOPE_BLOCK *) scope_stack.next) -> xml_item;
    else
        return NULL;
}


/*  Valueof handles extracting values from the XML tree.  Four cases are     */
/*  covered: attribute value with or without scope, and object value with    */
/*  or without scope.                                                        */
char *
valueof (char *scope, char *name)
{
    SCOPE_BLOCK
        *scope_block;
    char
        *value = NULL,
        *result;

    if (scope)
      {
        if (ignorecase)
          {
            FORLIST (scope_block, scope_stack)
                if (lexcmp (scope, scope_block-> name) == 0)
                    if (name)
                      {
                        char *name_lwc = strlwc (mem_strdup (name));
                        result = xml_get_attr (scope_block-> xml_item,
                                               name_lwc,
                                               NULL);
                        mem_free (name_lwc);
                        return result;
                      }
                    else
                        return xml_item_value (scope_block-> xml_item);
          }
        else
          {
            FORLIST (scope_block, scope_stack)
                if (streq (scope, scope_block-> name))
                    if (name)
                        return xml_get_attr (scope_block-> xml_item,
                                             name,
                                             NULL);
                    else
                        return xml_item_value (scope_block-> xml_item);
          }

      }
    else
    if (name)
      {
        if (ignorecase)
          {
            char *name_lwc = strlwc (mem_strdup (name));
            FORLIST (scope_block, scope_stack)
                if (scope_block-> xml_item)
                  {
                    value = xml_get_attr (scope_block-> xml_item,
                                          name_lwc,
                                          NULL);
                    if (value != NULL)
                      {
                        mem_free (name_lwc);
                        return value;
                      }
                  }
            mem_free (name_lwc);
          }
        else
          {
            FORLIST (scope_block, scope_stack)
                if (scope_block-> xml_item)
                    if ((value = xml_get_attr (scope_block-> xml_item,
                                               name,
                                               NULL)) != NULL)
                        return value;
          }
      }
    else
        FORLIST (scope_block, scope_stack)
            if (scope_block-> xml_item)
                return xml_item_value (scope_block-> xml_item);

    return NULL;
}


void
put_numeric_attr (XML_ITEM *item, const char *name, const long value)
{
    char
        buffer [LONG_WIDTH + 1];

    sprintf (buffer, "%ld", value);
    xml_put_attr (item, name, buffer);
}

/*- For Stack Functions -----------------------------------------------------*/

void
reset_for_stack (void)
{
    list_reset (& for_stack);
}


FOR_BLOCK *
create_for_block (const char *alias)
{
    FOR_BLOCK
        *for_block;

    list_create (for_block, sizeof (FOR_BLOCK));
    ASSERT (for_block);
    list_relink_after (for_block, &for_stack);

    list_reset (& for_block-> item_list);
    for_block-> for_item = NULL;
    for_block-> scope_block = create_scope_block (alias);

    return for_block;
}


void 
destroy_for_block (void)
{
    FOR_BLOCK
        *for_block = last_for_block ();

    list_unlink (for_block);

    while (! list_empty (& for_block-> item_list))
        destroy_for_item ((FOR_ITEM *) (for_block-> item_list. prev));

    destroy_scope_block ();
    mem_free (for_block);
}
        

FOR_BLOCK *
lookup_for_block (char *name)
{
    FOR_BLOCK
        *for_block = NULL;

    if (name)
        if (ignorecase)
          {
            FORLIST (for_block, for_stack)
              { 
                if (lexcmp (for_block-> scope_block-> name, name) == 0)
                    return for_block;
              }
          }
        else
          {
            FORLIST (for_block, for_stack)
              { 
                if (streq (for_block-> scope_block-> name, name))
                    return for_block;
              }
          }
    return NULL;
}


FOR_BLOCK *
last_for_block (void)
{
    if (!list_empty (&for_stack))
        return (FOR_BLOCK *) for_stack.next;
    else
        return NULL;
}


FOR_ITEM *
create_for_item (FOR_BLOCK *for_block, XML_ITEM *xml_item, long index)
{
    FOR_ITEM
        *for_item;

    list_create (for_item, sizeof (FOR_ITEM));
    ASSERT (for_item);
    list_relink_before (for_item, &for_block-> item_list);

    for_item-> xml_item = xml_item;
    for_item-> sort_key = NULL;
    for_item-> index    = index;

    return for_item;
}


void
destroy_for_item (FOR_ITEM *for_item)
{
    list_unlink (for_item);
    mem_free (for_item-> sort_key);
    mem_free (for_item);
}


Bool
first_for_item (FOR_BLOCK *for_block)
{
    if (! list_empty (& for_block-> item_list))
      {
        for_block-> for_item = (FOR_ITEM *) for_block-> item_list. next;
        for_block-> scope_block-> xml_item = for_block-> for_item-> xml_item;
        return TRUE;
      }
    else
      {
        for_block-> for_item = NULL;
        return FALSE;
      }
}


Bool
next_for_item (FOR_BLOCK *for_block)
{
    if (for_block-> for_item-> next != & for_block-> item_list)
      {
        for_block-> for_item = for_block-> for_item-> next;
        for_block-> scope_block-> xml_item = for_block-> for_item-> xml_item;
        return TRUE;
      }
    else
      {
        for_block-> for_item = NULL;
        return FALSE;
      }
}


/*- Schema Block Functions --------------------------------------------------*/

void
reset_schema_stack (void)
{
    list_reset (& schema_stack);
    cur_schema_block. position = NULL;
    list_reset (& cur_schema_block. control_stack);
}


void
push_schema_block (void)
{
    list_push (& schema_stack, cur_schema_block);
    cur_schema_block. position = NULL;
    list_reset (& cur_schema_block. control_stack);
}


void 
pop_schema_block (void)
{
    list_destroy (& cur_schema_block. control_stack);
    if (! list_empty (& schema_stack))
        list_pop (& schema_stack, cur_schema_block);
    else
      {
        cur_schema_block. position = NULL;
        list_reset (& cur_schema_block. control_stack);
      }
}


void
push_control (CONTROL_TYPE type, SCHEMA_NODE *condition)
{
    CONTROL_BLOCK
        control;

    control.type      = type;
    control.condition = condition;
    control.position  = schema_position ();
    list_push (& cur_schema_block. control_stack, control);
}


void
pop_control (CONTROL_BLOCK *control)
{
    list_pop (& cur_schema_block. control_stack, *control);
}


/*- Error Functions ---------------------------------------------------------*/

void
report_error (char type, char *format, ...)
{
    va_list
        argptr;
    char
        buffer [LINE_MAX];
    char
        *ptr;
    int
        length;

    va_start (argptr, format);          /*  Start variable arguments list    */
#if (defined (DOES_SNPRINTF))
    length = vsnprintf (buffer, LINE_MAX, format, argptr);
    ASSERT (length != -1);
#else
    length = vsprintf (buffer, format, argptr);
    ASSERT (length <= LINE_MAX);
#endif
    va_end (argptr);                    /*  End variable arguments list      */

    ptr = strtok (buffer, "\n");
    while (ptr)
      {
        fprintf (stderr, "%s %c: ", me, type);
        if (cur_schema_file)
            fprintf (stderr, "(%s %u) ", 
                     cur_schema_file-> name ? cur_schema_file-> name : "",
                     ( (void *) cur_schema_file-> cur_line
                        ==  & cur_schema_file-> line_head)
                        ? 1 : cur_schema_file-> cur_line-> line);

        fprintf (stderr, "%s\n", ptr);
        ptr = strtok (NULL, "\n");
      }

    feedback = -1;                      /*  So ggcode returns an error       */
}
