/*===========================================================================
 *                                                                           
 *  ggeval.c    Studio Wizard Generator                                      
 *                                                                           
 *  Written:    99/02/28    iMatix <tools@imatix.com>                        
 *  Revised:    99/07/25
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
#include "ggeval.h"                     /*  Include header file              */


/*- Function prototypes -----------------------------------------------------*/

static int  evaluate_substitute_node (SCHEMA_NODE *node, char **result);
static void pretty_print             (char *text,
				      SCHEMA_NODE *pretty,
				      char *example);
static void strneat                  (char *text);
static void undefined_expression_error
                                     (SCHEMA_NODE *node);
static int  evaluate_literal_node    (SCHEMA_NODE *node, char **result);
static int  evaluate_operand_node    (SCHEMA_NODE *node, char **result);
static int  evaluate_symbol_node     (SCHEMA_NODE *node, char **result);
static int  evaluate_member_node     (SCHEMA_NODE *node, char **result);
static int  evaluate_function_node   (SCHEMA_NODE *node, char **result);
static Bool verify_function_parameters
                                     (SCHEMA_NODE *fn_node, int min, int max);
static Bool validate_identifier_as_parameter
                                     (SCHEMA_NODE *node,
                                      char **scope, char **name);
static SCHEMA_NODE *function_parameter 
                                     (SCHEMA_NODE *node, int n);
static int  node_must_be_countable   (SCHEMA_NODE *node, int n);
static int  evaluate_operator        (SCHEMA_NODE *node);
static int  evaluate_text_node       (SCHEMA_NODE *node);

/*- Functions ---------------------------------------------------------------*/

int evaluate_schema (SCHEMA_NODE *node)
{
    int
        result;

    if (node)
      {
        result = evaluate_schema_node (node);
        if ((result == 0) && (!(node-> result)))
          {
            undefined_expression_error (node);
            node-> result = mem_strdup ("");
          }
        return result;
      }
    else
        return 0;
}


int
evaluate_schema_node (SCHEMA_NODE *node)
{
    char
        *result = NULL;

    if (node == NULL)
        return 0;

    if (node-> type == GG_SUBSTITUTE)
       if (evaluate_substitute_node (node, &result) != 0)
           return -1;

    if (node-> type == GG_LITERAL)
        if (evaluate_literal_node (node, &result) != 0)
           return -1;

    if (node-> type == GG_OPERAND)
        if (evaluate_operand_node (node, &result) != 0)
           return -1;

    if (node-> type  == GG_SYMBOL)
        if (evaluate_symbol_node (node, &result) != 0)
           return -1;

    if (node-> type  == GG_MEMBER)
        if (evaluate_member_node (node, &result) != 0)
           return -1;

    if (node-> type == GG_FUNCTION)
        if (evaluate_function_node (node, &result) != 0)
             return -1;

    if ((node-> type == GG_SUBSTITUTE)
    ||  (node-> type == GG_LITERAL)
    ||  (node-> type == GG_SYMBOL)
    ||  (node-> type == GG_MEMBER)
    ||  (node-> type == GG_FUNCTION)
    ||  (node-> type == GG_OPERAND))
        node-> result = result;

    if (node-> type == GG_OPERATOR)
        if (evaluate_operator (node) != 0)
             return -1;

    if (node-> type == GG_TEXT)
        if (evaluate_text_node (node) != 0)
             return -1;

    node-> indent = node-> spaces;
    if (node-> line_break)
        node-> shuffle_cnt = 0;
    else
        node-> shuffle_cnt = ((node-> result) ? strlen (node-> result) : 0)
                           -  node-> length;
    return 0;
}


static int
evaluate_substitute_node (SCHEMA_NODE *node, char **result)
{
    char
        *format,
        buffer [LINE_MAX + 1];
    int
        length;

    if (evaluate_schema_node (node-> op1) != 0)
        return -1;
    if (evaluate_schema_node (node-> op3) != 0)
        return -1;
    if (evaluate_schema_node (node-> pretty) != 0)
        return -1;
    if (evaluate_schema_node (node-> format) != 0)
        return -1;

    if ((node-> op1) ? node-> op1-> result : FALSE)
      {
        *result = mem_strdup (node-> op1-> result);
        
        /*  Only match the case if ignorecase is TRUE and the expression */
        /*  consists of a single identifier.                             */
        if ((ignorecase)
        &&  (node-> op1-> type == GG_SYMBOL)
        &&  (node-> op1-> op1))
            pretty_print (*result, node-> pretty, node-> op1-> op1-> result);
        else
            pretty_print (*result, node-> pretty, NULL);
      }
    else
    if (node-> op3)
      {
        if (node-> op3-> result)
            *result = mem_strdup (node-> op3-> result);
        else
            node-> culprit = node-> op3-> culprit;
      }
    else
      {
        if (node-> op1)
            node-> culprit = node-> op1-> culprit;

        undefined_expression_error (node);
        *result = mem_strdup ("");
      }

    if (node-> format)
      {
        if (strchr (node-> format-> result, '%') == NULL)
          {
            format = mem_alloc (strlen (node-> format-> result) + 2);
            strcpy (format, "%");
            strcat (format, node-> format-> result);
          }
        else
            format = mem_strdup (node-> format-> result);

#if (defined (DOES_SNPRINTF))
        length = snprintf (buffer, LINE_MAX, format, *result);
        mem_free (format);
        if (length == -1)
            report_error ('W', "Output line too long.");
#else
        length = sprintf (buffer, format, *result);
        mem_free (format);
        if (length > LINE_MAX)
          {
            report_error ('W', "Output line too long.");
            mem_free (*result);
            return -1;
          }
#endif
            
        mem_free (*result);
        *result = mem_alloc (length + 1);
        ASSERT (*result);
        strcpy (*result, buffer);
      }
    return 0;
}


/*  pretty_print: Reformats a string according to a format string consisting */
/*  of a comma-separated series of specifiers.  Currently accepted formats:  */
/*         lower - lower case                                                */
/*         UPPER - UPPER CASE                                                */
/*         Neat  - Neat Case                                                 */
/*         c     - Valid_c_identifier                                        */
/*         COBOL - VALID-COBOL-IDENTIFIER                                    */
/*                                                                           */
/*  If no case modifier is provided and pretty is not empty and the expr-    */
/*  is a single identifier, its case is used as an example to match for      */
/*  lower, upper or neat case.                                               */

static void
pretty_print (char *text, SCHEMA_NODE *pretty, char *example)
{
    char
        *tokens,
        *token,
        *c;
    Bool
        use_example;

    ASSERT (text);

    if (*text == 0)                     /* Do nothing to null string         */
        return;

    use_example = (example != NULL);

    if (pretty)
      {
        if (*pretty-> result == 0)
            use_example = FALSE;

        tokens = mem_strdup (pretty-> result);
        token = strtok (tokens, ", ");
        while (token)
          {
            strlwc (token);
            if (streq (token, "lower"))
              {
                use_example = FALSE;
                strlwc (text);
              }
            else
            if (streq (token, "upper"))
              {
                use_example = FALSE;
                strupc (text);
              }
            else
            if (streq (token, "neat"))
              {
                use_example = FALSE;
                strneat (text);
              }
            else
            if (streq (token, "c"))
              {
                c = text;
                if (!isalpha (*c))
                    *c = '_';
                
                while (*c)
                  {
                    if (!(isalpha (*c) || isdigit (*c)))
                        *c = '_';

                    c++;
                  }
              }
            else
            if (streq (token, "cobol"))
              {
                c = text;
                if (!isalpha (*c))
                    *c = '-';
                
                while (*c)
                  {
                    if (!(isalpha (*c) || isdigit (*c)))
                        *c = '-';

                    c++;
                  }
              }

            token = strtok (NULL, ", ");
          }
        mem_free (tokens);
      }

    if ((use_example)
    &&  (strlen (example) > 1))
      {
        c = example;

        if (isupper (*c))
            while ((isupper (*c) || !isalpha (*c)) && (*c))
                c++;

        if (*c == 0)
            strupc (text);
        else
        if (c == example + 1)
          {
            if (islower (*c))
              {
                while ((islower (*c) || !isalpha (*c)) && (*c))
                    c++;
                if (!isupper (*c))
                    strneat (text);
              }
          }
        else
            if (c == example)
              {
                if (islower (*c))
                    while ((islower (*c) || !isalpha (*c)) && (*c))
                        c++;

                if (*c == 0)
                    strlwc (text);
              }
      }
}


static void
strneat (char *text)
{
    char
        *c;

    c = text;
    while (*c)
      {
        while ((!isalpha (*c)) && (*c))
            c++;
        *c = toupper (*c);
        c++;
        while (isalpha (*c) && (*c))
          {
            *c = tolower (*c);
            c = c++;
          }
      }
}


static void
undefined_expression_error (SCHEMA_NODE *node)
{
    char 
        *scope = "",
        *point = ".";
    SCHEMA_NODE
        *culprit;

    culprit = node-> culprit;
    if ((culprit) ? culprit-> type == GG_SYMBOL : FALSE)
      {
        if (culprit-> op2)
            scope = culprit-> op2-> result;

        /*  Print point except if there is identifier and no scope  */
        if (culprit-> op1 && ! culprit-> op2)
            point = "";
        report_error ('W', "Undefined identifier: %s%s%s", 
                      scope, point,
                      (culprit-> op1) ? culprit-> op1-> result : "");
      }
    else
        report_error ('W', "Undefined expression.");
}


static int
evaluate_literal_node (SCHEMA_NODE *node, char **result)
{
    if (node-> op1)
      {
        if (evaluate_schema_node (node-> op1) != 0)
            return -1;

        *result = mem_alloc (node-> op1-> indent +
                             strlen (node-> op1-> result) +
                             1);
        ASSERT (*result);
        memset (*result, ' ', node-> op1-> indent);
        strcpy (*result + node-> op1-> indent,
                node-> op1-> result);
      }
    else
        *result = mem_strdup ("");

    return 0;
}


static int
evaluate_operand_node (SCHEMA_NODE *node, char **result)
{
    SCHEMA_NODE
        *expr_root = NULL;
    int
        rc;

    if (node-> op1)
      {
        if (evaluate_schema_node (node-> op1) != 0)
            return -1;
       
        expr_root = gg_parse_expression (node-> op1-> result);
        if (expr_root)
          {
            rc = evaluate_schema_node (expr_root);
            if (rc == 0)
                if (expr_root-> result)
                    *result = mem_strdup (expr_root-> result);
                else
                  {
                    undefined_expression_error (expr_root);
                    *result = mem_strdup ("");
                  }

            gg_free (expr_root);
            return rc;
          }
        else
          {
            gg_free (expr_root);
            report_error ('E', "%s", gg_error ());
            return -1;
          }
      }
    else
        *result = mem_strdup ("");

    return 0;
}


static int
evaluate_symbol_node (SCHEMA_NODE *node, char **result)
{
    char
        *scope = NULL,
        *name  = NULL;

    if (node-> op1)
      {
        if (evaluate_schema_node (node-> op1) != 0)
            return -1;

        name = node-> op1-> result;
      }
    if (node-> op2)
      {
        if (evaluate_schema_node (node-> op2) != 0)
            return -1;

        scope = node-> op2-> result;
      }

    *result = valueof (scope, name);
    if (*result)
        *result = mem_strdup (*result);
    else
        node-> culprit = node;

    return 0;
}


static int
evaluate_member_node (SCHEMA_NODE *node, char **result)
{
    char
        *item  = NULL;
    XML_ITEM
        *xml_item,
        *xml_child;

    ASSERT (node-> op1);
    if (evaluate_schema_node (node-> op1) != 0)
        return -1;

    item = node-> op1-> result;

    if (node-> op2)
      {
        if (evaluate_schema_node (node-> op2) != 0)
            return -1;

	xml_item = lookup_scope_xml (node-> op2-> result);
      }
    else
        xml_item = last_scope_xml ();

    if (xml_item)
        FORCHILDREN (xml_child, xml_item)
            if (ignorecase)
              {
                if (lexcmp (item, xml_item_name (xml_child)) == 0)
                  {
                    *result = mem_strdup (xml_item_value (xml_child));
                    return 0;
                  }
              }
            else
              {
                if (streq (item, xml_item_name (xml_child)))
                  {
                    *result = mem_strdup (xml_item_value (xml_child));
                    return 0;
                  }
              }
    return 0;
}


static int
evaluate_function_node (SCHEMA_NODE *node, char **result)
{
    char
        buffer [LINE_MAX + 1];
    SCHEMA_NODE
        *identifier,
        *condition,
        *string,
        *start,
        *end,
        *len;
    long
        n,
        index,
        start_n,
        end_n,
        len_n;
    time_t
        timer;
    char
        *scope,
        *name,
        *xml_name;
    Bool
        where_condition,
        error = FALSE;
    FOR_BLOCK
        *from_block,
        *for_block;
    FOR_ITEM
        *for_item;
    XML_ITEM
        *xml_item;

    if (evaluate_schema_node (node-> op1) != 0)
        return -1;

    strlwc (node-> op1-> result);
    if (streq (node-> op1-> result, "count"))
      {
        if (!verify_function_parameters (node-> op2, 1, 2))
            return -1;

        identifier = function_parameter (node-> op2, 1);
        condition  = function_parameter (node-> op2, 2);
        if (!validate_identifier_as_parameter (identifier, &scope, &name))
            return -1;

        if (scope)
          {
            from_block = lookup_for_block (scope);
	    if (! from_block)
              {
                report_error ('E', "Unknown scope: %s", scope);
                return -1;
              }
          }
        else
            from_block = last_for_block ();

	for_block = create_for_block ("count");

        n = 0;
        index = 0;
        xml_item = xml_first_child (from_block-> scope_block-> xml_item);
        while (xml_item)
          {
            while (xml_item)
              {
                xml_name = xml_item_name (xml_item);
                if (xml_name && name)
                  {
                    if (ignorecase)
                      {
                        if (lexcmp (xml_name, name) == 0)
                            break;
                      }
                    else
                      {
                        if (streq (xml_name, name))
                            break;
                      }
                  }
                else
                    /*  Take all named children; others are part of value  */
                    if (xml_name && (! name))
                        break;

                xml_item = xml_next_sibling (xml_item);
              }

            if (xml_item)
              {
                index++;
                if (condition)
                  {
                    for_item = create_for_item (for_block, xml_item, index);
                    for_block-> for_item               = for_item;
                    for_block-> scope_block-> xml_item = xml_item;
                    if (evaluate_schema (condition) != 0)
                      {
                        error = TRUE;
                        break;
                      }
                    where_condition = (Bool) atoi (condition-> result);
                    destroy_for_item (for_item);
                    gg_clean (condition);
                    if (where_condition)
                        n++;
                  }
                else
                    n++;

                xml_item = xml_next_sibling (xml_item);
	      }
          }

	destroy_for_block ();

        if (error)
            return -1;

        sprintf (buffer, "%ld", n);
        *result = mem_strdup (buffer);
        ASSERT (*result);
      }
    else
    if (streq (node-> op1-> result, "env"))
      {
        if (!verify_function_parameters (node-> op2, 1, 1))
            return -1;

        if (evaluate_schema_node (node-> op2) != 0)
            return -1;
            
        if (node-> op2-> result)
            *result = mem_strdup (getenv (node-> op2-> result));
        else
          {
            *result = NULL;
            node-> culprit = node-> op2;
          }
      }
    else
    if (streq (node-> op1-> result, "index"))
      {
        if (!verify_function_parameters (node-> op2, 0, 1))
            return -1;

        if (node-> op2)
            if ((node-> op2-> type == GG_OPERAND)
            ||  (node-> op2-> type == GG_SYMBOL))
              {
                if (evaluate_schema_node (node-> op2-> op1) != 0)
                    return -1;

                if (node-> op2-> op2)
                  {
                    report_error ('E', "Argument may not contain a scope.");
                    return -1;
                  }

                for_block = lookup_for_block (node-> op2-> op1-> result);
                if (for_block == NULL)
                  {
		    report_error ('E', "Unknown .for scope: %s",
                                  node-> op2-> op1-> result);
                    return -1;
                  }
              }
            else
              {
                report_error ('E', "Argument must be a legal identifier.");
                return -1;
              }
        else
            for_block = last_for_block ();

        n = for_block-> for_item-> index;
        
        sprintf (buffer, "%ld", n);
        *result = mem_strdup (buffer);
        ASSERT (*result);
      }
    else
    if (streq (node-> op1-> result, "name"))
      {
        if (!verify_function_parameters (node-> op2, 0, 1))
            return -1;

        if (node-> op2)
            if ((node-> op2-> type == GG_OPERAND)
            ||  (node-> op2-> type == GG_SYMBOL))
              {
                if (evaluate_schema_node (node-> op2-> op1) != 0)
                    return -1;

                if (node-> op2-> op2)
                  {
                    report_error ('E', "Argument may not contain a scope.");
                    return -1;
                  }

                for_block = lookup_for_block (node-> op2-> op1-> result);
                if (for_block == NULL)
                  {
		    report_error ('E', "Unknown .for scope: %s",
                                  node-> op2-> op1-> result);
                    return -1;
                  }
              }
            else
              {
                report_error ('E', "Argument must be a legal identifier.");
                return -1;
              }
        else
            for_block = last_for_block ();

        *result = xml_item_name (for_block-> for_item-> xml_item);
        if (*result)
            *result = mem_strdup (*result);
        else
            *result = mem_strdup ("");

        ASSERT (*result);
      }
    else
    if (streq (node-> op1-> result, "exists"))
      {
        if (!verify_function_parameters (node-> op2, 1, 1))
            return -1;
        if (evaluate_schema_node (node-> op2) != 0)
            return -1;
        
        if (file_exists (node-> op2-> result))
            n = 1;
        else
            n = 0;

        sprintf (buffer, "%ld", n);
        *result = mem_strdup (buffer);
        ASSERT (*result);
      }
    else
    if (streq (node-> op1-> result, "timestamp"))
      {
        if (!verify_function_parameters (node-> op2, 1, 1))
            return -1;
        if (evaluate_schema_node (node-> op2) != 0)
            return -1;
        
        timer = get_file_time (node-> op2-> result);
        if (timer)
          {
            sprintf (buffer, "%ld%ld", timer_to_date (timer),
                                       timer_to_time (timer));
            *result = mem_strdup (buffer);
            ASSERT (*result);
          }
      }
    else
    if (streq (node-> op1-> result, "length"))
      {
        if (!verify_function_parameters (node-> op2, 1, 1))
            return -1;
        if (evaluate_schema_node (node-> op2) != 0)
            return -1;

        *result = NULL;
        if (node-> op2)
            if (node-> op2-> result)
              {          
                n = strlen (node-> op2-> result);
                sprintf (buffer, "%ld", n);
                *result = mem_strdup (buffer);
                ASSERT (*result);
              }
      }
    else
    if (streq (node-> op1-> result, "defined"))
      {
        if (!verify_function_parameters (node-> op2, 1, 1))
            return -1;
        if (!validate_identifier_as_parameter (node-> op2, &scope, &name))
            return -1;

        if (valueof ((scope)
                     ?  scope
                     :  NULL,
                     name) != NULL)
            n = 1;
        else
            n = 0;

        sprintf (buffer, "%ld", n);
        *result = mem_strdup (buffer);
        ASSERT (*result);
      }
    else
    if (streq (node-> op1-> result, "substr"))
      {
        if (!verify_function_parameters (node-> op2, 2, 4))
            return -1;

        string = function_parameter (node-> op2, 1);
        start  = function_parameter (node-> op2, 2);
        end    = function_parameter (node-> op2, 3);
        len    = function_parameter (node-> op2, 4);
        if (evaluate_schema_node (string) != 0)
            return -1;

        if (start && end && len)
          {
            report_error ('E', "Too many parameters for function 'substr'.");
            return -1;
          }
        if (!(start || end || len))
          {
            report_error ('E', "Not enough parameters for function 'substr'.");
            return -1;
          }

        start_n = node_must_be_countable (start, 2);
        if (start_n < 0)
            return -1;

        end_n = node_must_be_countable (end, 3);
        if (end_n < 0)
            return -1;

        len_n = node_must_be_countable (len, 4);
        if (len_n < 0)
            return -1;

        if (start
        &&  end
        && (end_n < start_n))
          {
            report_error ('E', "'End' must be at least 'Start' in 'substr'");
            return -1;
          }
        if (len && !start)
          {
            if (!end)
                end_n = strlen (string-> result) - 1;

            start_n = end_n - len_n + 1;
          }
        else
          {
            if (!start)
                start_n = 0;

            if (!len)
                if (end)
                    len_n = end_n - start_n + 1;
                else
                    len_n = strlen (string-> result);
          }
        if (start_n >= (long) strlen (string-> result))
            *result = mem_strdup ("");
        else
          {
            *result = mem_alloc (len_n + 1);
            if (start_n >= 0)
                strncpy (*result, & string-> result [start_n], len_n);
            else
                strncpy (*result, string-> result, len_n);

            (*result) [len_n] = '\0';
          }
      }
    else
    if (streq (node-> op1-> result, "trim"))
      {
        if (!verify_function_parameters (node-> op2, 1, 1))
            return -1;

        if (evaluate_schema_node (node-> op2) != 0)
            return -1;

        if (node-> op2-> result)
            *result = mem_strdup (trim (node-> op2-> result));
        else
            *result = mem_strdup ("");
        ASSERT (*result);
      }
    else
    if (streq (node-> op1-> result, "justify"))
      {
        SCHEMA_NODE
            *string,
            *width,
            *prefix;
        int
            width_n;

        if (!verify_function_parameters (node-> op2, 3, 3))
            return -1;

        string = function_parameter (node-> op2, 1);
        width  = function_parameter (node-> op2, 2);
        prefix = function_parameter (node-> op2, 3);
        if (evaluate_schema_node (string) != 0)
            return -1;
        if (evaluate_schema_node (prefix) != 0)
            return -1;
        width_n = node_must_be_countable (width, 2);
        if (width_n < 0)
            return -1;

        *result = strreformat (string-> result? string-> result: "", 
                               width_n, prefix-> result);
        ASSERT (*result);
      }
    else
      {
        report_error ('E', "Unknown function: %s", node-> op1-> result);
        return -1;
      }
    return 0;
}                    


static Bool
verify_function_parameters (SCHEMA_NODE *fn_node, int min, int max)
{
    int
        count;
    SCHEMA_NODE
        *node;

    count = 0;
    node = fn_node;
    if (node)
        count++;
    while (node != NULL)
      {
        if ((node-> type     == GG_OPERATOR)
        &&  (node-> operator == NEXT_ARG))
            count++;
        node = node-> op2;
      }
    
    if (count < min)
      {
        report_error ('E', "Missing function parameter.");
        return FALSE;
      }
    if (count > max)
      {
        report_error ('E', "Too many function parameters.");
        return FALSE;
      }
    return TRUE;
}


static Bool
validate_identifier_as_parameter (SCHEMA_NODE *node,
                                       char **scope,
                                       char **name)
{
  if (node-> type == GG_OPERAND)
    {
      if (evaluate_schema_node (node-> op1) != 0)
          return FALSE;

      *scope = strtok (node-> op1-> result, ".");
      *name  = strtok (NULL, " .");
      if (!*name)
        {
          *name  = *scope;
          *scope = NULL;
        }
    }
  else
  if (node-> type == GG_SYMBOL)
    {
      if (evaluate_schema_node (node-> op1) != 0)
          return FALSE;
      if (evaluate_schema_node (node-> op2) != 0)
          return FALSE;

      *scope = (node-> op2) ? node-> op2-> result : NULL;
      *name  = node-> op1-> result;
    }
  else
    {
      report_error ('E', "Function argument must be an identifier.");
      return FALSE;
    }
  return TRUE;
}


/*  Return n-th function parameter  */
static SCHEMA_NODE *
function_parameter (SCHEMA_NODE *node, int n)
{
    while ((n > 1) && node)
        if ((node-> type     == GG_OPERATOR)
        &&  (node-> operator == NEXT_ARG))
          {
            node = node-> op2;
            n--;
          }
        else
            return NULL;

    if (node)
        if ((node-> type     == GG_OPERATOR)
        &&  (node-> operator == NEXT_ARG))
            return node-> op1;
        else
            return node;
    else
        return NULL;
}


static int
node_must_be_countable (SCHEMA_NODE *node, int n)
{
    int
        result;
    char
        *endptr = NULL;

    if (node)
        if (evaluate_schema_node (node) != 0)
            return -1;
        else
          {
            if (!node-> result)
              {
                report_error ('E', "Parameter %i is undefined.", n);
                return -1;
              }
            result = strtol (node-> result, &endptr, 10);
            if (*endptr != 0)
              {
                report_error ('E', "Parameter %i is not numeric.", n);
                return -1;
              }
            if (result < 0)
              {
                report_error ('E', "Parameter %i is negative.", n);
                return -1;
              }
            return result;
          }
    else
        return 0;
}


static int
evaluate_operator (SCHEMA_NODE *node)
{
    OPERATOR
        the_operator = node-> operator;
    long
        n1,
        n2,
        n = 0;
    char
        *endptr;
    Bool
        numeric1,
        numeric2,
        safe;
    int
        offset,
        length,
        cmp;
    char
        buffer [LINE_MAX + 1];

    node-> line_break = node-> op2 ? node-> op2-> line_break : FALSE;
    node-> extend     = node-> op2 ? node-> op2-> extend     : FALSE;
    node-> result     = NULL;
    node-> culprit    = NULL;
    node-> indent     = node-> spaces;

    safe = ((the_operator == SAFE_EQUALS)
         || (the_operator == SAFE_NOT_EQUALS)
         || (the_operator == SAFE_GREATER_THAN)
         || (the_operator == SAFE_LESS_THAN)
         || (the_operator == SAFE_GREATER_EQUAL)
         || (the_operator == SAFE_LESS_EQUAL));

    /*  Evaluate first operand  */
    if (evaluate_schema_node (node-> op1) != 0)
        return -1;

    if (node-> op1)
        if (node-> op1-> result)
          {
            n1 = strtol (node-> op1-> result, &endptr, 10);
            numeric1 = (*endptr == 0);
            if (((n1 == LONG_MIN) || (n1 == LONG_MAX))
                &&  (errno == ERANGE))   /* Treat over/under-flow as non-num */
                numeric1 = FALSE;

            /*  Now try some optimisation.  This also allows for             */
            /*  constructs such as '.if defined (N) & N = 1'                 */
            if (numeric1)
                if ((the_operator == OR) && (n1))
                  {
                    n = 1;
                    sprintf (buffer, "%ld", n);
                    node-> result = mem_strdup (buffer);
                    ASSERT (node-> result);
                    return 0;
                  }
                else
                if ((the_operator == AND) && (!n1))
                  {
                    n = 0;
                    sprintf (buffer, "%ld", n);
                    node-> result = mem_strdup (buffer);
                    ASSERT (node-> result);
                    return 0;
                  }
          }
        else
            if (! safe)
              {
                node-> culprit = node-> op1-> culprit;
                return 0;
              }
            else
              {
                n = 0;    /*  Return FALSE if operator is undefined  */
                sprintf (buffer, "%ld", n);
                node-> result = mem_strdup (buffer);
                ASSERT (node-> result);
                return 0;
              }
    else            /* This is a unary operator */
      {
        n1 = 0;
        numeric1 = TRUE;
      }

    if (evaluate_schema_node (node-> op2) != 0)
        return -1;
    if (node-> op2)
        if (node-> op2-> result)
          {
            n2 = strtol (node-> op2-> result, &endptr, 10);
            numeric2 = (*endptr == 0);
            if (((n2 == LONG_MIN) || (n2 == LONG_MAX))
                &&  (errno == ERANGE))   /* Treat over/under-flow as non-num */
                numeric2 = FALSE;
          }
        else
            if (! safe)
              {
                node-> culprit = node-> op2-> culprit;
                return 0;
              }
            else
              {
                n = 0;    /*  Return FALSE if operator is undefined  */
                sprintf (buffer, "%ld", n);
                node-> result = mem_strdup (buffer);
                ASSERT (node-> result);
                return 0;
              }
    else
        return 0;      /*  This should never happen, but might save us  */

    if (the_operator == SAFE_EQUALS)
        the_operator = EQUALS;
    else if (the_operator == SAFE_NOT_EQUALS)
        the_operator = NOT_EQUALS;
    else if (the_operator == SAFE_GREATER_THAN)
        the_operator = GREATER_THAN;
    else if (the_operator == SAFE_LESS_THAN)
        the_operator = LESS_THAN;
    else if (the_operator == SAFE_GREATER_EQUAL)
        the_operator = GREATER_EQUAL;
    else if (the_operator == SAFE_LESS_EQUAL)
        the_operator = LESS_EQUAL;

    if (the_operator == NEXT_ARG)
        return 0;

    /*  If the operator is UNDEFINED then we are concatenating strings,      */
    /*  including the spaces, and shuffling if enabled.  If the operator is  */
    /*  PLUS and at least one of the operands is non-numeric then we also    */
    /*  concatenate but do not include the spaces before the 2nd string.     */
    if (( the_operator == OP_UNDEFINED)
    ||  ((the_operator == PLUS)
    &&   ((! numeric1) || (! numeric2)) ) )
      {
        /*  We must be careful that there is a first operator, since PLUS    */
        /*  is a valid unary operator.                                       */
        if (node-> op1)
          {
            offset = strlen (node-> op1-> result);
            if (node-> op1-> line_break)
                offset++;
            node-> indent = node-> op1-> indent;
          }
        else
          {
            offset = 0;
            node-> indent = 0;
          }

        if (the_operator == OP_UNDEFINED)  /*  Must be 2 ops in this case   */
            if (! node-> op1-> line_break)
                if (node-> op2-> indent > shuffle)
                    if (node-> op1-> shuffle_cnt <= node-> op2-> indent
                                                  - shuffle)
                        node-> op2-> indent -= node-> op1-> shuffle_cnt;
                    else
                        node-> op2-> indent = shuffle;

        length = offset + node-> op2-> indent + strlen (node-> op2-> result);

        if (node-> op1 ? node-> op1-> line_break : FALSE)
            node-> shuffle_cnt = node-> op2-> shuffle_cnt;
        else
            node-> shuffle_cnt = length - node-> length;

        node-> result = mem_alloc (length + 1);
        ASSERT (node-> result);
        if (node-> op1)
          {
            strcpy (node-> result, node-> op1-> result);
            if (node-> op1-> line_break)
                strcat (node-> result, "\n");
          }
        
        if (the_operator == OP_UNDEFINED)
          {
            memset (node-> result + offset, ' ', node-> op2-> indent);
            offset += node-> op2-> indent;
          }
        strcpy (node-> result + offset, node-> op2-> result);
        return 0;
      }

    /*  "string" * 3 = "stringstringstring"  */
    if ((the_operator == TIMES)
    &&  (!numeric1)
    &&  ( numeric2))
      {
        length = strlen (node-> op1-> result);
        node-> result = mem_alloc (length * n2 + 1);
        ASSERT (node-> result);
        offset = 0;
        while (n2-- > 0)
          {
            memcpy (node-> result + offset, node-> op1-> result, length);
            offset += length;
          }
        node-> result [offset] = '\0';
        return 0;
      }

    /*  If at least one operand is non-numeric, treat as a string operator   */
    if ((!numeric1) || (!numeric2))
      {
        char
            *str1, *str2;
        str1 = node-> op1? node-> op1-> result: "";
        str2 = node-> op2? node-> op2-> result: "";
        cmp = strcmp (str1, str2);
        switch (the_operator)
          {
            case EQUALS        : n = (cmp == 0);  break;
            case NOT_EQUALS    : n = (cmp != 0);  break;
            case GREATER_THAN  : n = (cmp >  0);  break;
            case LESS_THAN     : n = (cmp <  0);  break;
            case GREATER_EQUAL : n = (cmp >= 0);  break;
            case LESS_EQUAL    : n = (cmp <= 0);  break;
            default            : report_error ('E', "Invalid operator.");
                                 return -1;
          }
        sprintf (buffer, "%ld", n);
        node-> result = mem_strdup (buffer);
        ASSERT (node-> result);
        return 0;
      }

    /*  If we got here then we have two numeric operands.  The operator may  */
    /*  be unary, in which case n1 is zero.                                  */
    switch (the_operator)
      {
        case NOT           : n = !n2;                break;
        case PLUS          : n = n1 + n2;            break;
        case MINUS         : n = n1 - n2;            break;
        case TIMES         : n = n1 * n2;            break;
        case DIVIDE        : if (n2 != 0)
                               {
                                 n = n1 / n2;            
                                 break;
                               }
                             else
                               {
                                 report_error ('E', "Division by zero.");
                                 return -1;
                               }
        case EQUALS        : n = (long) (n1 == n2);  break;
        case NOT_EQUALS    : n = (long) (n1 != n2);  break;
        case GREATER_THAN  : n = (long) (n1 > n2);   break;
        case LESS_THAN     : n = (long) (n1 < n2);   break;
        case GREATER_EQUAL : n = (long) (n1 >= n2);  break;
        case LESS_EQUAL    : n = (long) (n1 <= n2);  break;
        case OR            : n = n1 | n2;            break;
        case AND           : n = n1 & n2;            break;
        default            : report_error ('E', "Invalid operator.");
                             return -1;
      }
    sprintf (buffer, "%ld", n);
    node-> result = mem_strdup (buffer);
    ASSERT (node-> result);
    return 0;
}


static int
evaluate_text_node (SCHEMA_NODE *node)
{
    char
        *backslash, 
        *last;

    /*  Replace \x with x */
    node-> result = mem_strdup (node-> text);
    last          = strchr (node-> result, 0);
    backslash     = strchr (node-> result, '\\');
    while (backslash && (backslash < last - 1))
      {
        memmove (backslash, backslash + 1, last - backslash);
        backslash = strchr (backslash + 1, '\\');
      }

    return 0;
}
