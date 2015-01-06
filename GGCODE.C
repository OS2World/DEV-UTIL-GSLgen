/*===========================================================================
 *                                                                           
 *  ggcode.c    GSL code generator                                      
 *                                                                           
 *  Written:    98/03/30    iMatix <tools@imatix.com>                        
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
#include "ggcomm.h"                     /*  Common declarations              */
#include "ggeval.h"                     /*  Evaluation functions             */
#include "ggcode.h"                     /*  Include header file              */
#include "ggcode.d"                     /*  Include dialog data              */

/*- Type definitions --------------------------------------------------------*/


/*- Function prototypes -----------------------------------------------------*/

static void        construct_compound_item_values
                                              (XML_ITEM *item);
static void        skip_control_block         (CONTROL_TYPE open_type);
static void        unmatched_control_error    (CONTROL_BLOCK control);
static void        open_output_file           (char *mode);
static XML_ITEM   *lookup_from_xml            (SCHEMA_NODE *node);
static Bool        sort_compare               (LIST *t1, LIST *t2);
static int         substitute_parameters_into_xml (XML_ITEM *xml_item,
                                               SCHEMA_NODE *params,
                                               SCHEMA_NODE *args);
static Bool        free_data                  (SYMBOL *symbol, ...);
static void        gg_handle_signal           (int the_signal);

/*- Macros ------------------------------------------------------------------*/


/*- Global variables used in this source file only --------------------------*/

static int
    output_line,                        /*  Line number in output file       */
    last_line;                          /*  Most recently written line       */

static char
    *initial_schema,
    *output_name,
    buffer [LINE_MAX + 1];              /*  Buffer for line i/o              */

static XML_ITEM
    *xml_root;

static FILE
    *output;

static SCHEMA_NODE
    *schema_root;

static SYMTAB
    *macros;

static Bool
    stdout_echo = TRUE;                 /*  Copy to stdout                   */
static CONSOLE_FCT
    *stdout_fct = NULL;                 /*  Redirector function              */


/********************************   M A I N   ********************************/

int
gg_code (XML_ITEM *xml_tree)
{
    signal (SIGSEGV, gg_handle_signal);
    signal (SIGABRT, gg_handle_signal);
    xml_root = xml_tree;

    feedback = 0;                       /*  No errors so far                 */
#   include "ggcode.i"                  /*  Include dialog interpreter       */
}



/*************************   INITIALISE THE PROGRAM   ************************/

MODULE initialise_the_program (void)
{
    if (!xml_root)
      {
        report_error ('E', "No XML data.");
        raise_exception (anomaly_event);
      }
    else
      {
        the_next_event = ok_event;
      }
}


/************************   INITIALISE PROGRAM DATA   ************************/

MODULE initialise_program_data (void)
{
    /*  Initialise a whole bunch of stuff.  We have to do this explicitly    */
    /*  rather than with initial values so that the code can be called more  */
    /*  than once.                                                           */


    reset_scope_stack ();
    reset_for_stack ();
    reset_schema_stack ();

    initial_schema  = NULL;
    output_name     = NULL;
    output          = NULL;
    schema_root     = NULL;

    construct_compound_item_values (xml_root);
}

 
/*  Construct an item value for each XML item.  The value is made concaten-  */
/*  ing the individual value definitions.                                    */

static void
construct_compound_item_values (XML_ITEM *item)
{
    XML_ITEM
        *child;
    char
        *name,
        *value,
        *compound = NULL;
    size_t
        length;

    child = xml_first_child (item);
    while (child)
      {
        name = xml_item_name (child);
        if (name)
            construct_compound_item_values (child);
        else
          {
            value = xml_item_value (child);
            if (value)
              {
                while ((*value) && iscntrl (*value))
                    value++;

                if (compound)
                  {
                    length = strlen (compound);
                    compound = mem_realloc (compound,
                                            length + strlen (value) + 1);
                  }
                else
                  {
                    length = 0;
                    compound = mem_alloc (strlen (value) + 1);
                  }
                ASSERT (compound);
                strcpy (compound + length, value);
              }
          }
        child = xml_next_sibling (child);
      }
    if (compound)
      {
        xml_modify_value (item, compound);
        mem_free (compound);
      }
}


/************************   PREPARE TO GENERATE CODE   ***********************/

MODULE prepare_to_generate_code (void)
{
    FOR_BLOCK
        *root_block;

    /*  Create initial blocks                                                */
    root_block = create_for_block (xml_item_name (xml_root));
    create_for_item  (root_block, xml_root, 0);
    first_for_item   (root_block);

    /*  Create macro symbol table                                            */
    macros = sym_create_table ();

    /*  Find initial schema name                                             */
    initial_schema = mem_strdup (valueof (NULL, "schema"));

    /*  Find our own name                                                    */
    me = mem_strdup (valueof (NULL, "me"));

    if (!initial_schema)
      {
        report_error ('E', "No schema specified.");
        raise_exception (anomaly_event);
        return;
      }
}


/**************************   START INITIAL SCHEMA   *************************/

MODULE start_initial_schema (void)
{
    if (!open_schema_file (initial_schema))
      {
        report_error ('E', "Can't open schema file %s", initial_schema);
        raise_exception (anomaly_event);
        return;
      }

    xml_put_attr (first_scope_xml (),
                  "schema",
                  cur_schema_file-> name);
}


/*************************   GET NEXT SCHEMA LINE   **************************/

MODULE get_next_schema_line (void)
{
    char
        *value;

    if (parse_next_schema_line ())
      {
        schema_root = cur_schema_file-> cur_line-> node;
        if (!schema_root)
          {
            report_error ('E', "%s", gg_error ());
            
            /*  Treat bad schema line like a comment  */
            the_next_event = comment_event;
            return;
          }
      }
    else
      {
        the_next_event = end_of_schema_event;
        return;
      }

    /*  Lookup ignore case flag                                              */
    value = valueof (NULL, "ignorecase") ;
    if (value)
        ignorecase = atoi (value);
    else
        ignorecase = 0;

    /*  Lookup shuffle flag                                                  */
    value = valueof (NULL, "shuffle") ;
    if (value)
        shuffle = atoi (value);
    else
        shuffle = 0;

    switch (schema_root-> type)
      {
        case GG_COMMENT     :
            the_next_event = comment_event;
            break;
        case GG_TEXT        : 
        case GG_SUBSTITUTE  :
        case GG_OPERATOR    : 
            if (evaluate_schema (schema_root) == 0)
                the_next_event = text_event;
            else
                the_next_event = anomaly_event;
            break;
        case GG_CLOSE     : 
            the_next_event = close_event;
            break;
        case GG_ELSE      : 
            the_next_event = else_event;    
            break;
        case GG_END_IF    :
            the_next_event = end_if_event;  
            break;
        case GG_END_FOR   :
            the_next_event = end_for_event;  
            break;
        case GG_END_MACRO :
            the_next_event = end_macro_event;  
            break;
        case GG_END_NEW   :
            the_next_event = end_new_event;  
            break;
        case GG_END_WHILE :
            the_next_event = end_while_event;  
            break;
        case GG_DUMP      :
            the_next_event = dump_event;    
            break;
        case GG_OUTPUT    :
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = output_event;
            else
                the_next_event = anomaly_event;
            break;
        case GG_APPEND    :
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = append_event;
            else
                the_next_event = anomaly_event;
            break;
        case GG_INCLUDE   :
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = include_event;  
            else
                the_next_event = anomaly_event;
            break;
        case GG_INTERPRET :
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = interpret_event;  
            else
                the_next_event = anomaly_event;
            break;
        case GG_DELETE    : 
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = delete_event;  
            else
                the_next_event = anomaly_event;
            break;
        case GG_COPY      :
            if (evaluate_schema (schema_root-> op1) == 0)
                if (evaluate_schema (schema_root-> op2) == 0)
                  {
                    the_next_event = copy_event;
                    break;
                  }
            the_next_event = anomaly_event;
            break;
        case GG_RENAME    :
            if (evaluate_schema (schema_root-> op1) == 0)
                if (evaluate_schema (schema_root-> op2) == 0)
                  {
                    the_next_event = rename_event;
                    break;
                  }
            the_next_event = anomaly_event;
            break;
        case GG_ECHO      : 
             if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = echo_event;  
            else
                the_next_event = anomaly_event;
            break;
        case GG_DEFINE    :
            if (evaluate_schema (schema_root-> op1) == 0)
                if (evaluate_schema (schema_root-> op2) == 0)
                    if (evaluate_schema (schema_root-> op3) == 0)
                      {
                        the_next_event = define_event;  
                        break;
                      }
            the_next_event = anomaly_event;
            break;
        case GG_MACRO     :
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = macro_event;
            else
                the_next_event = anomaly_event;
            break;
        case GG_INVOKE    :
            if (evaluate_schema (schema_root-> op1) == 0)
              {
                if (evaluate_schema_node (schema_root-> op2) == 0)
                  {
                    the_next_event = invoke_event;
                    break;
                  }
              }
                the_next_event = anomaly_event;
                break;
        case GG_IF        : 
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = if_event;  
            else
                the_next_event = anomaly_event;
            break;
        case GG_ELSIF     : 
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = elsif_event;  
            else
                the_next_event = anomaly_event;
            break;
        case GG_FOR       : 
            if (evaluate_schema (schema_root-> op1) == 0)
                if (evaluate_schema (schema_root-> op2) == 0)
                    if (evaluate_schema (schema_root-> op3) == 0)
                      {
                        the_next_event = for_event;  
                        break;
                      }
            the_next_event = anomaly_event;
            break;
        case GG_WHILE     : 
            the_next_event = while_event;  
            break;
        case GG_XML       :
            if (evaluate_schema (schema_root-> op1) == 0)
                if (evaluate_schema (schema_root-> op2) == 0)
                  {
                    the_next_event = xml_event;
                    break;
                  }
            the_next_event = anomaly_event;
            break;
        case GG_NEW       : 
            if (evaluate_schema (schema_root-> op1) == 0)
                if (evaluate_schema (schema_root-> op2) == 0)
                    if (evaluate_schema (schema_root-> op3) == 0)
                      {
                        the_next_event = new_event;
                        break;
                      }
            the_next_event = anomaly_event;
            break;
        case GG_ABORT     : 
            if (evaluate_schema (schema_root-> op1) == 0)
                the_next_event = abort_event;
            else
                the_next_event = anomaly_event;
            break;
        default           : report_error ('E', "Unknown schema construct");
                            raise_exception (anomaly_event);
      }
}


/************************   STORE SYMBOL DEFINITION   ************************/

MODULE store_symbol_definition (void)
{
    XML_ITEM
        *xml_item;
    char
        *scope = NULL,
        *name  = NULL;
    SCHEMA_NODE
        *expr_root = NULL;

    if (schema_root-> op1)
        name = schema_root-> op1-> result;

    if (schema_root-> op2)
        scope = schema_root-> op2-> result;

    if (name && !scope)
      {
        /*  If we find a name with no scope, re-parse the name in case       */
        /*  it is a substitute which will evaluate into a scope & name.      */
        expr_root = gg_parse_expression (name);
        if (expr_root-> type != GG_SYMBOL)
          {
            report_error ('E', "Illegal identifier: %s\n", name);
            gg_free (expr_root);
            raise_exception (anomaly_event);
          }
        if (evaluate_schema (expr_root-> op1) != 0)
          {
            raise_exception (anomaly_event);
            gg_free (expr_root);
            return;
          }
        if (evaluate_schema (expr_root-> op2) != 0)
          {
            raise_exception (anomaly_event);
            gg_free (expr_root);
            return;
          }
        if (expr_root-> op1)
            name = expr_root-> op1-> result;
        else
            name = NULL;

        if (expr_root-> op2)
            scope = expr_root-> op2-> result;
      }

    if (scope)
      {
        xml_item = lookup_scope_xml (scope);
        if (xml_item == NULL)
          {
            report_error ('E', "Unknown scope for .define: %s",
                          scope);
            raise_exception (anomaly_event);
            gg_free (expr_root);
            return;
          }
      }
    else
        xml_item = first_scope_xml ();

    if (name)
      {
        if (ignorecase)
            name = strlwc (mem_strdup (name));

        xml_put_attr (xml_item,
                      name,
                      (schema_root-> op3) ? schema_root-> op3-> result : NULL);

        if (ignorecase)
            mem_free (name);
      }
    else
        xml_modify_value (xml_item,
                          (schema_root-> op3) ? schema_root-> op3-> result : NULL);

    gg_free (expr_root);
}


/*************************   STORE MACRO DEFINITION   ************************/

MODULE store_macro_definition (void)
{
    SYMBOL
        *symbol;

    symbol = sym_assume_symbol (macros, schema_root-> op1-> result, NULL);
    ASSERT (symbol);

    if (!symbol-> data)       /*  Macro not already defined  */
        symbol-> data = mem_alloc (sizeof (MACRO));

    ASSERT (symbol-> data);

    ((MACRO *) (symbol-> data))-> position = schema_position ();
    ((MACRO *) (symbol-> data))-> args     = schema_root-> op2;
}


/****************************   SKIP MACRO BODY   ****************************/

MODULE skip_macro_body (void)
{
    skip_control_block (CTL_MACRO);
}


static void
skip_control_block (CONTROL_TYPE open_type)
{
    CONTROL_BLOCK
        control;

    push_schema_block ();               /*  Treat block as dummy include     */
    push_control (open_type, NULL);
    while (!list_empty (& cur_schema_block. control_stack))
      {
        if (parse_next_schema_line ())
          {
            schema_root = cur_schema_file-> cur_line-> node;
            if (!schema_root)
              {
                report_error ('E', "%s", gg_error ());
                raise_exception (anomaly_event);
                pop_schema_block ();
                return;
              }
          }
        else
          {
            pop_control (& control);
            unmatched_control_error (control);
            pop_schema_block ();
            raise_exception (anomaly_event);
            return;
          }

        if (schema_root-> type == GG_IF)
            push_control (CTL_IF, NULL);
        else
        if (schema_root-> type == GG_END_IF)
          {
            pop_control (& control);
            if ((control.type != CTL_IF)
            &&  (control.type != CTL_ELSE))
              {
                unmatched_control_error (control);
                pop_schema_block ();
                raise_exception (anomaly_event);
                return;
              }
          }
        else
        if (schema_root-> type == GG_ELSE)
          {
            pop_control (& control);
            if (control.type != CTL_IF)
              {
                unmatched_control_error (control);
                pop_schema_block ();
                raise_exception (anomaly_event);
                return;
              }
            if (!list_empty (& cur_schema_block. control_stack))
                push_control (CTL_ELSE, NULL);
          }
        else
        if (schema_root-> type == GG_ELSIF)
          {
            pop_control (& control);
            if (control.type != CTL_IF)
              {
                unmatched_control_error (control);
                pop_schema_block ();
                raise_exception (anomaly_event);
                return;
              }
            if (!list_empty (& cur_schema_block. control_stack))
                push_control (CTL_IF, NULL);
          }
        else
        if (schema_root-> type == GG_FOR)
            push_control (CTL_FOR, NULL);
        else
        if (schema_root-> type == GG_END_FOR)
          {
            pop_control (& control);
            if (control.type != CTL_FOR)
              {
                unmatched_control_error (control);
                pop_schema_block ();
                raise_exception (anomaly_event);
                return;
              }
          }
        else
        if (schema_root-> type == GG_WHILE)
            push_control (CTL_WHILE, NULL);
        else
        if (schema_root-> type == GG_END_WHILE)
          {
            pop_control (& control);
            if (control.type != CTL_WHILE)
              {
                unmatched_control_error (control);
                pop_schema_block ();
                raise_exception (anomaly_event);
                return;
              }
          }
        else
        if (schema_root-> type == GG_MACRO)
            push_control (CTL_MACRO, NULL);
        else
        if (schema_root-> type == GG_END_MACRO)
          {
            pop_control (& control);
            if (control.type != CTL_MACRO)
              {
                unmatched_control_error (control);
                pop_schema_block ();
                raise_exception (anomaly_event);
                return;
              }
          }
        else
        if (schema_root-> type == GG_NEW)
            push_control (CTL_NEW, NULL);
        else
        if (schema_root-> type == GG_END_NEW)
          {
            pop_control (& control);
            if (control.type != CTL_NEW)
              {
                unmatched_control_error (control);
                pop_schema_block ();
                raise_exception (anomaly_event);
                return;
              }
          }
      }
    pop_schema_block ();
}


static void
unmatched_control_error (CONTROL_BLOCK control)
{
    if (control.position)
        report_error ('E', "Unmatched control at (%s %u).",
                            control.position-> parent-> name,
                            control.position-> line);
    else
        report_error ('E', "Unmatched control.");
}


/****************************   NEW OUTPUT FILE   ****************************/

MODULE new_output_file (void)
{
    open_output_file (FOPEN_WRITE_TEXT);
    output_line = 1;
    last_line = 0;
    line_length = 0;

    put_numeric_attr (first_scope_xml (),
		      "line",
		      output_line);
}


static void
open_output_file (char *mode)
{
    if (output)
        close_output_file ();

    output_name = mem_strdup (schema_root-> op1-> result);

    if (output_name && *output_name)
        output = fopen (output_name, mode);

    if (output)
        xml_put_attr (first_scope_xml (),
                      "outfile",
                      output_name);
    else
      {
        report_error ('E', "Can't open output file %s", output_name);
        raise_exception (anomaly_event);
      }
}


/***************************   EXTEND OUTPUT FILE   **************************/

MODULE extend_output_file (void)
{
    FILE
        *current;

    /*  Count line number for extended file  */
    output_line = 0;
    last_line = 0;
    line_length = 0;
    current = fopen (schema_root-> op1-> result, FOPEN_READ_TEXT);
    if (current)
      {
        while (file_read (current, buffer))
            output_line++;
        file_close (current);
      }

    open_output_file (FOPEN_APPEND_TEXT);

    put_numeric_attr (first_scope_xml (),
		      "line",
		      output_line);
}


/**************************   PUSH SCHEMA POSITION   *************************/

MODULE push_schema_position (void)
{
    cur_schema_block. position = schema_position ();
    push_schema_block ();
}


/*************************   START INCLUDED SCHEMA   *************************/

MODULE start_included_schema (void)
{
    if (!open_schema_file (schema_root-> op1-> result))
      {
        report_error ('E', "Can't open schema file %s", 
                           schema_root-> op1-> result);
        raise_exception (anomaly_event);
      }
    else
        xml_put_attr (first_scope_xml (),
                      "schema",
                      cur_schema_file-> name);
}


/************************   START INTERPRETED SCHEMA   ***********************/

MODULE start_interpreted_schema (void)
{
    char
        buffer [32];
    SCHEMA_LINE
        *position;

    position = schema_position ();
    snprintf (buffer, 32, "(%s %u)",
                          position-> parent-> name,
                          position-> line);
    open_schema_text (buffer, schema_root-> op1-> result);

    schema_root-> op1-> result = NULL;    /*  To avoid deallocation  */
}


/***************************   CLOSE OUTPUT FILE   ***************************/

MODULE close_output_file (void)
{
    if (output)
        file_close (output);

    output = NULL;
    mem_strfree (&output_name);
    xml_put_attr (first_scope_xml (),
                  "outfile",
                  NULL);

    xml_put_attr (first_scope_xml (),
                  "line",
                  NULL);
}


/**************************   INITIALISE FOR BLOCK   *************************/

MODULE initialise_for_block (void)
{
    FOR_BLOCK
        *for_block;
    long
        index;
    Bool
        where_condition;
    FOR_ITEM
        *for_item;
    XML_ITEM
        *from_xml,
        *xml_item;
    char
        *xml_name,
        *for_name;

    /*  .for <op2>.<op1> as <op3> where <pretty> by <format> */

    from_xml = lookup_from_xml (schema_root-> op2);
    if (! from_xml)
        return;

    for_block = create_for_block (schema_root-> op3
                                      ? schema_root-> op3-> result
                                      : schema_root-> op1
                                          ? schema_root-> op1-> result
                                          : "");
    for_name = schema_root-> op1
	           ? schema_root-> op1-> result
		   : NULL;

    xml_item = xml_first_child (from_xml);
    index = 0;
    while (xml_item)
      {
        while (xml_item)
          {
            xml_name = xml_item_name (xml_item);
            /*  Consider only named children - others are part of value  */
            if (xml_name)
                if (for_name)
                  {
                    if (ignorecase)
                      {
                        if (lexcmp (xml_name, for_name) == 0)
                            break;
                      }
                    else
                      {
                        if (streq (xml_name, for_name))
                            break;
                      }
                  }
                else
                    /*  If no child name specified, take all named children  */
                    break;

            xml_item = xml_next_sibling (xml_item);
          }

        if (xml_item)
          {
            index++;
            for_item = create_for_item (for_block, xml_item, index);
            for_block-> for_item               = for_item;
            for_block-> scope_block-> xml_item = xml_item;
            
            if (schema_root-> pretty)  /*  Where clause  */
              {
                if (evaluate_schema (schema_root-> pretty) != 0)
                  {
                    raise_exception (anomaly_event);
                    return;
                  }
                where_condition = (Bool) atoi (schema_root-> pretty-> result);
                gg_clean (schema_root-> pretty);
                if (!where_condition)
                  {
                    destroy_for_item (for_item);
                    xml_item = xml_next_sibling (xml_item);
                    continue;
                  }
              }
          }
        
        if (xml_item)
          {
            if (schema_root-> format)      /*  By clause  */
              {
                if (evaluate_schema (schema_root-> format) != 0)
                  {
                    raise_exception (anomaly_event);
                    return;
                  }
                for_item-> sort_key = schema_root-> format-> result;
                schema_root-> format-> result = NULL;
                gg_clean (schema_root-> format);
              }
            else
                for_item-> sort_key = NULL;

            xml_item = xml_next_sibling (xml_item);
          }
      }         

    /*  Sort if necessary  */
    if (schema_root-> format)
        list_sort (& for_block-> item_list, sort_compare);

    if (first_for_item (for_block))
        push_control (CTL_FOR, NULL);
    else
      {
        skip_control_block (CTL_FOR);
        destroy_for_block ();
      }
}


static XML_ITEM *
lookup_from_xml (SCHEMA_NODE *node)
{
    FOR_BLOCK
        *from_block;

    if (node)
      {
        from_block = lookup_for_block (node-> result);
        if (! from_block)
          {
            report_error ('E', "Unknown scope: %s", node-> result);
            raise_exception (anomaly_event);
            return NULL;
          }
      }
    else
        /*  Use innermost .for block  */
        from_block = last_for_block ();

    return from_block-> scope_block-> xml_item;
}


static Bool
sort_compare (LIST *t1, LIST *t2)
{
    long
        n1,
        n2;
    char
        *endptr1,
        *endptr2;

    n1 = strtol (((FOR_ITEM *) t1)-> sort_key, &endptr1, 10);
    n2 = strtol (((FOR_ITEM *) t2)-> sort_key, &endptr2, 10);

    /*  If both operands are numeric  */
    if ((*endptr1 == 0) && (*endptr2 == 0))
        return (n1 > n2);
    else
        return (strcmp (((FOR_ITEM *) t1)-> sort_key,
                        ((FOR_ITEM *) t2)-> sort_key) > 0);
}


/***************************   ITERATE FOR BLOCK   ***************************/

MODULE iterate_for_block (void)
{
    CONTROL_BLOCK
        control = {CTL_UNDEFINED, NULL, NULL};

    if (!list_empty (& cur_schema_block. control_stack))
        pop_control (& control);

    if (control.type != CTL_FOR)
      {
        unmatched_control_error (control);
        raise_exception (anomaly_event);
        return;
      }

    if (next_for_item (last_for_block ()))
      {
        /*  Push same control back  */
        list_push (& cur_schema_block. control_stack, control);
        restore_position (control.position);
      }
    else
        destroy_for_block ();
}


/*************************   INITIALISE WHILE BLOCK   ************************/

MODULE initialise_while_block (void)
{
    Bool
        where_condition;

    if (evaluate_schema (schema_root-> op1) != 0)
      {
        raise_exception (anomaly_event);
        return;
      }

    where_condition = (Bool) atoi (schema_root-> op1-> result);
    gg_clean (schema_root-> op1);
    if (where_condition)
        push_control (CTL_WHILE, schema_root-> op1);
    else
        skip_control_block (CTL_WHILE);
}


/**************************   ITERATE WHILE BLOCK   **************************/

MODULE iterate_while_block (void)
{
    CONTROL_BLOCK
        control = {CTL_UNDEFINED, NULL, NULL};
    Bool
        where_condition;

    if (!list_empty (& cur_schema_block. control_stack))
        pop_control (& control);

    if (control.type != CTL_WHILE)
      {
        unmatched_control_error (control);
        raise_exception (anomaly_event);
        return;
      }

    if (evaluate_schema (control.condition) != 0)
      {
        raise_exception (anomaly_event);
        return;
      }

    where_condition = (Bool) atoi (control.condition-> result);
    gg_clean (control.condition);
    if (where_condition)
      {
        /*  Push same control back */
        list_push (& cur_schema_block. control_stack, control);
        restore_position (control.position);
      }
}


/*************************   SKIP IF BLOCK IF FALSE   ************************/

MODULE skip_if_block_if_false (void)
{
    if (atoi (schema_root-> op1-> result))
        push_control (CTL_IF, NULL);
    else
      {
        skip_control_block (CTL_IF);

        /*  Now skip .elsif until one condition is TRUE  */
        while (schema_root ? (schema_root-> type == GG_ELSIF) : FALSE)
          {
            if (evaluate_schema (schema_root-> op1) != 0)
              {
                raise_exception (anomaly_event);
                return;
              }
            if (atoi (schema_root-> op1-> result))
              {
                push_control (CTL_IF, NULL);
                return;
              }
            else
                skip_control_block (CTL_IF);
          }

        /*  If we hit a .else then we continue processing  */
        if (schema_root && schema_root-> type == GG_ELSE)
            push_control (CTL_ELSE, NULL);
      }
}


/*************************   SKIP ELSE BLOCK ALWAYS   ************************/

MODULE skip_else_block_always (void)
{
    CONTROL_BLOCK
        control = {CTL_UNDEFINED, NULL, NULL};

    if (!list_empty (& cur_schema_block. control_stack))
        pop_control (& control);

    if (control.type != CTL_IF)
      {
        unmatched_control_error (control);
        raise_exception (anomaly_event);
      }
    else
        skip_control_block (CTL_ELSE);
}


/************************   SKIP ELSIF BLOCK ALWAYS   ************************/

MODULE skip_elsif_block_always (void)
{
    CONTROL_BLOCK
        control = {CTL_UNDEFINED, NULL, NULL};

    if (!list_empty (& cur_schema_block. control_stack))
        pop_control (& control);

    if (control.type != CTL_IF)
      {
        unmatched_control_error (control);
        raise_exception (anomaly_event);
      }
    else
        while ((schema_root)
           && ((schema_root-> type == GG_ELSIF)
           ||  (schema_root-> type == GG_ELSE)))
            skip_control_block (CTL_IF);
}


/*****************************   CLOSE IF BLOCK   ****************************/

MODULE close_if_block (void)
{
    CONTROL_BLOCK
        control = {CTL_UNDEFINED, NULL, NULL};

    if (!list_empty (& cur_schema_block. control_stack))
        pop_control (& control);

    if ((control.type != CTL_IF)
    &&  (control.type != CTL_ELSE))
      {
        unmatched_control_error (control);
        raise_exception (anomaly_event);
      }
}


/*************************   COPY FILE AS SPECIFIED   ************************/

MODULE copy_file_as_specified (void)
{
}


/************************   RENAME FILE AS SPECIFIED   ***********************/

MODULE rename_file_as_specified (void)
{
}


/************************   DELETE FILE AS SPECIFIED   ***********************/

MODULE delete_file_as_specified (void)
{
}


/**************************   ECHO TEXT TO CONSOLE   *************************/

MODULE echo_text_to_console (void)
{
    if (schema_root-> op1)
        printf ("%s M: %s\n", me, schema_root-> op1-> result);
    else
        printf ("%s M:\n", me);
}


/***************************   DUMP SYMBOL TABLE   ***************************/

MODULE dump_symbol_table (void)
{
}


/**************************   COPY LINE TO OUTPUT   **************************/

MODULE copy_line_to_output (void)
{
    Bool
        cobol;                          /*  Are we producing COBOL code?     */
    int
        indent;                         /*  Desired line indentation         */
    char
        *text;                          /*  Schema text line                 */

    indent = schema_root-> indent;
    text   = schema_root-> result;
    cobol  = (valueof (NULL, "cobol") != NULL);

    if (indent > shuffle)
        if (line_length <= schema_root-> indent - shuffle)
            indent -= line_length;
        else
            indent = shuffle;

    if (cobol && (output_line > last_line))
      {
/* PH 1999/07/13    These lines caused COBOL code to be misformatted
                    and are not necessary - we just prefix with six digits.
        if (indent >= 6)
            indent -= 6;
        else
          {
            if (strlen (text) > 6)
                text += (6 - indent);
            else
                text = "";
            indent = 0;
          }
*/
        line_length += sprintf (buffer, "%04d00%*s%s",
                                output_line, indent, "", text);
        last_line = output_line;
      }
    else
        line_length += sprintf (buffer, "%*s%s", indent, "", text);

    if (!schema_root-> extend)
      {
        strcat (buffer, "\n");
        output_line++;
        line_length = 0;
        put_numeric_attr (first_scope_xml (), "line", output_line);
      }

    /*  Now print to output if open, else send to stdout as specified        */
    if (output)
        fprintf (output, "%s", buffer);
    else
      {
        if (stdout_fct)
            (stdout_fct) (buffer);
        if (stdout_echo)
            fprintf (stdout, "%s", buffer);
      }
}


/*************************   INITIALISE MACRO BLOCK   ************************/

MODULE initialise_macro_block (void)
{
    SYMBOL
        *symbol;
    SCOPE_BLOCK
        *scope_block;
    XML_ITEM
        *xml_parent;

    symbol = sym_lookup_symbol (macros, schema_root-> op1-> result);
    if (!symbol)
      {
        pop_schema_position ();  /*  Because we already pushed it   */
        report_error ('E', "Undefined macro: %s", schema_root-> op1-> result);
        raise_exception (anomaly_event);
        return;
      }

    /*  Make macro xml parameter block a child of the last scope so it       */
    /*  doesn't get orphaned if the macro block is never closed.             */
    xml_parent = last_scope_xml ();
    scope_block = create_scope_block (schema_root-> op1-> result);
    scope_block-> xml_item = xml_new (xml_parent, "", "");
    if (substitute_parameters_into_xml (scope_block-> xml_item,
                                        schema_root-> op2,
                                        ((MACRO *) (symbol-> data))-> args))
      {
        raise_exception (anomaly_event);
        return;
      }
    gg_clean (schema_root);

    restore_position (((MACRO *) (symbol-> data))-> position);

    /*  And push a MACRO control onto control stack                          */
    push_control (CTL_MACRO, NULL);
}


static int
substitute_parameters_into_xml (XML_ITEM *xml_item,
                                SCHEMA_NODE *params,
                                SCHEMA_NODE *args)
{
    int
        result;
    char
        *name;

    if ((!args) && (!params))
        return 0;

    ASSERT (args);

    if ((args-> type     == GG_OPERATOR)
    &&  (args-> operator == NEXT_ARG))
      {
        if (params
           ? (params-> type != GG_OPERATOR) || (params-> operator != NEXT_ARG)
           : TRUE)
          {
            pop_schema_position ();
            report_error ('E', "Mismatched parameters.");
            return -1;
          }

        result = substitute_parameters_into_xml (xml_item, 
                                                 params-> op1,
                                                 args-> op1);
        if (result)
            return result;

        result = substitute_parameters_into_xml (xml_item, 
                                                 params-> op2,
                                                 args-> op2);
        return result;
      }
    else
      {
        name = args-> text;             /*  Don't evaluate simple text       */
        ASSERT (name);
        if (ignorecase)
            name = strlwc (mem_strdup (name));

        xml_put_attr (xml_item,
                      name,
                      params ? params-> result : NULL);

        if (ignorecase)
            mem_free (name);
        return 0;
      }
}


/***************************   CLOSE MACRO BLOCK   ***************************/

MODULE close_macro_block (void)
{
    CONTROL_BLOCK
        control = {CTL_UNDEFINED, NULL, NULL};

    if (!list_empty (& cur_schema_block. control_stack))
        pop_control (& control);

    if (control.type != CTL_MACRO)
      {
        unmatched_control_error (control);
        raise_exception (anomaly_event);
      }

    /*  Free xml parameter block  */
    xml_free (last_scope_xml ());
    destroy_scope_block ();
}


/**************************   INITIALISE NEW BLOCK   *************************/

MODULE initialise_new_block (void)
{
    XML_ITEM
        *from_xml;
    SCOPE_BLOCK
        *scope_block;

    /*  .new <op2>.<op1> as <op3> */

    from_xml = lookup_from_xml (schema_root-> op2);
    if (! from_xml)
        return;

    scope_block = create_scope_block (schema_root-> op3
                                          ? schema_root-> op3-> result
                                          : schema_root-> op1
                                              ? schema_root-> op1-> result
                                              : "");
    scope_block-> xml_item = xml_new (from_xml,
                                      schema_root-> op1
                                          ? schema_root-> op1-> result
                                          : NULL,
                                      NULL);
    ASSERT (scope_block-> xml_item);

    /*  And push a NEW control onto control stack                            */
    push_control (CTL_NEW, NULL);
}


/****************************   CLOSE NEW BLOCK   ****************************/

MODULE close_new_block (void)
{
    CONTROL_BLOCK
        control = {CTL_UNDEFINED, NULL, NULL};

    if (!list_empty (& cur_schema_block. control_stack))
        pop_control (& control);

    if (control.type != CTL_NEW)
      {
        unmatched_control_error (control);
        raise_exception (anomaly_event);
      }

    destroy_scope_block ();
}


/*****************************   READ XML FILE   *****************************/

MODULE read_xml_file (void)
{
    XML_ITEM
        *from_xml,
        *new_root,
        *new_item;

    /*  .xml <op1> from <op2> */

    from_xml = lookup_from_xml (schema_root-> op2);
    if (! from_xml)
        return;

    new_root = xml_load (PATH, schema_root-> op1-> result);
    if (!new_root)
      {
        report_error ('E', "Error reading XML file %s...\n%s",
                      schema_root-> op1-> result,
                      xml_error ());
        raise_exception (anomaly_event);
        return;
      }

    new_item = xml_first_child (new_root);    /*  Get actual data, not root  */
    xml_detach (new_item);
    xml_attach (from_xml, new_item);
    xml_free   (new_root);

    construct_compound_item_values (new_item);
}


/******************************   CLOSE SCHEMA   *****************************/

MODULE close_schema (void)
{
    if (cur_schema_file)
        close_schema_file ();
}


/**************************   POP SCHEMA POSITION   **************************/

MODULE pop_schema_position (void)
{
    if (! list_empty (& schema_stack))
      {
        pop_schema_block ();
        restore_position (cur_schema_block. position);
      }
    else
        raise_exception (finished_event);
}


/************************   DESTROY DATA STRUCTURES   ************************/

MODULE destroy_data_structures (void)
{
    while (! list_empty (& schema_stack))
        pop_schema_block ();

    destroy_schema_data ();

    while (!list_empty (&for_stack))
        destroy_for_block ();

    while (!list_empty (&scope_stack))
        destroy_scope_block ();

    while (!list_empty (&schema_stack))
        pop_schema_block ();
    pop_schema_block ();                /*  To clear control stack           */

    sym_exec_all (macros, free_data);
    sym_delete_table (macros);
    mem_strfree (&initial_schema);
    mem_strfree (&me);
}


static Bool
free_data (SYMBOL *symbol, ...)
{
/*      gg_free  (((MACRO *) (symbol-> data))-> args); */
    mem_free (symbol-> data);
    return TRUE;
}


/***********************   VERIFY ALL CONTROLS CLOSED   **********************/

MODULE verify_all_controls_closed (void)
{
    CONTROL_BLOCK
        control;

    if (!list_empty (& cur_schema_block. control_stack))
      {
        pop_control (& control);
        unmatched_control_error (control);
        raise_exception (anomaly_event);
      }
}


/***************************   GET EXTERNAL EVENT   **************************/

MODULE get_external_event (void)
{
}


/*************************   TERMINATE THE PROGRAM    ************************/

MODULE terminate_the_program (void)
{
    the_next_event = terminate_event;
}

/*  -------------------------------------------------------------------------
 *  gg_handle_signal -- internal
 *
 */

static void
gg_handle_signal (int the_signal)
{
    signal (SIGSEGV, SIG_DFL);
    signal (SIGABRT, SIG_DFL);
    if (the_signal == SIGSEGV)
        report_error ('E', "Segmentation violation");
    else
    if (the_signal == SIGABRT)
        report_error ('E', "Abort");
    else
        report_error ('E', "Unknown signal");

    abort ();
}


/*  ---------------------------------------------------------------------[<]-
    Function: gg_send_stdout

    Synopsis: Redirects stdout output to a specified CONSOLE_FCT function.
    If the specified address is NULL, redirects back to the stdout stream.
    If the echo argument is TRUE, stdout output is also sent to stdout as
    normal.
    ---------------------------------------------------------------------[>]-*/

void
gg_send_stdout (CONSOLE_FCT *new_stdout_fct, Bool echo)
{
    stdout_fct  = new_stdout_fct;
    stdout_echo = echo;                 /*  Copy to stdout                   */
}
