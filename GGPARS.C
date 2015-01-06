/*===========================================================================
 *                                                                           
 *  ggpars.c    Schema parsing functions 'gg_parse_schema' &                 
 *              'gg_parse_expression'                                        
 *                                                                           
 *  Written:    98/04/15  iMatix <tools@imatix.com>                          
 *  Revised:    99/07/29
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

#include "sfl.h"                        /*  Public definitions               */
#include "ggpars.h"                     /*  Include public type declarations */
#include "ggpars.d"                     /*  Include dialog data              */

/*  Types */

typedef enum { TEXT,
               SUBSTITUTE,
               SCOPE, 
               OPERAND,
               LITERAL_OPERAND,
               IDENTIFIER,
               IDENTIFIER_CONTINUE,
               SCOPE_IDENTIFIER,
               WHITE_SPACE,
               QUOTED,
               FUNCTION,
               ARGUMENTS,
               ATTRIBUTE,
               ONE_MORE,
               DEFINE,
               MACRO,
               MACRO_ARGS,
               INVOKE,
               INVOKE_ARGS,
               XML,
               NEW,
               FOR }
    STATE;



/*  Pointers into parse tree */
static SCHEMA_NODE **the_node_ptr,      /*  Ptr to ptr to current node       */
                   *the_parent,         /*  Ptr to parent of current node    */
                   *the_root,           /*  Ptr to root of parse tree        */
                   *feedback;           /*  Value returned to caller         */

/*  Variables concerning current token  */
static int         schema_ptr;          /*  Offset to next char to parse     */
static int         token_posn;          /*  Offset of last token parsed      */
static int         token_length;        /*  Length of last token parsed      */
static char        the_token;           /*  Current expression token         */
static long        the_number;          /*  Current number value             */
static OPERATOR    the_operator;        /*  Expression operator              */
static int         brackets;            /*  Number of brackets to open       */
static int         spaces;              /*  Number of spaces before token    */

/*  Other variables  */
static event_t     start_event;         /*  What to start parsing            */
static SCHEMA_READ *schema_read;        /*  The schema reading function      */
static char        *schema_line;
static char        buffer [LINE_MAX + 1];
static MEMTRN      *transaction;        /*  Transaction for memory rollback  */
static char        error_message [LINE_MAX + 1];

static LIST        state_stack,         /*  Dialog state stack               */
                   quote_stack;         /*  Quote character                  */


/*  Internal function prototypes  */

static void print_node                            (FILE *stream,
                                                   int   level,
                                                   SCHEMA_NODE *node);
static void print_string                          (FILE *stream,
                                                   int level,
                                                   char *text);
static void print_indented                        (FILE *stream, 
                                                   int level, 
                                                   char *format, ...);
static void print_operator                        (FILE *stream,
                                                   OPERATOR op);
static SCHEMA_NODE *
            gg_start_parsing                      (void);
static void collect_spaces                        (void);
static void generate_character_token_event        (const char *char_tokens,
                                                   event_t char_event);
static void generate_literal_event                (const char *terminators);
static void generate_substitute_event             (void);
static void generate_extend_event                 (void);
static void generate_end_of_line_event            (void);
static void generate_other_event                  (void);
static void recognise_schema_command_token        (void);
static void generate_quote_event                  (void);
static void generate_unary_operator_event         (void);
static void generate_operator_event               (void);
static void generate_next_arg_event               (void);
static void generate_number_event                 (void);
static void generate_member_event                 (void);
static void recognise_schema_token                (void);
static void generate_spaces_event                 (void);

static void insert_simple_node                    (SCHEMA_NODE_TYPE type);
static void insert_the_node                       (SCHEMA_NODE *app_node,
                                                   OPERATOR operator);
static int  priority                              (OPERATOR operator);
static void find_operator_insertion_point         (OPERATOR operator);
static Bool grouping                              (OPERATOR operator);
static void return_one_level                      (void);

static void error_exception                       (char *format, ...);


/*  ---------------------------------------------------------------------[<]-
    Function: gg_free

    Synopsis: Frees all memory allocated to a parse tree.  Assumes that all
    pointer fields are either NULL or allocated with separate calls to
    sflmem memory allocation functions.
    ---------------------------------------------------------------------[>]-*/

void gg_free (SCHEMA_NODE *node)
{
    if (node)
      {
        mem_free (node-> result);
        mem_free (node-> text);
        gg_free (node-> op1);
        gg_free (node-> op2);
        gg_free (node-> op3);
        gg_free (node-> pretty);
        gg_free (node-> format);
        mem_free (node);
      }
}


/*  ---------------------------------------------------------------------[<]-
    Function: gg_clean

    Synopsis: Cleans up a parse tree, that is, frees additional memory
    allocated since parsing.  This allows an expression to be re-evaluated
    without first being re-parsed.
    ---------------------------------------------------------------------[>]-*/

void gg_clean (SCHEMA_NODE *node)
{
    if (node)
      {
        mem_strfree (&node-> result);

        gg_clean (node-> op1);
        gg_clean (node-> op2);
        gg_clean (node-> op3);
        gg_clean (node-> pretty);
        gg_clean (node-> format);
      }
}


/*  ---------------------------------------------------------------------[<]-
    Function: gg_print

    Synopsis: Prints a textual representation of a parse tree to the
    specified stream.
    ---------------------------------------------------------------------[>]-*/

void gg_print (FILE *stream, SCHEMA_NODE *node)
{
    print_node (stream, 0, node);
}


void print_node (FILE *stream, int level, SCHEMA_NODE *node)
{
    if (!node)
        return;

    print_node_type (stream, level, node-> type);
    print_indented (stream, level, "  length: %u\n", node-> length);
    if (node-> brackets)
        print_indented (stream, level, "  brackets: %u\n", node-> brackets);
    if (node-> spaces)
        print_indented (stream, level, "  spaces: %u\n", node-> spaces);
    if (node-> extend)
        print_indented (stream, level, "  extend: TRUE\n");
    if (node-> line_break)
        print_indented (stream, level, "  line break: TRUE\n");
    if (node-> operator)
      {
        print_indented (stream, level, "  operator: ", level*4, "");
        print_operator (stream, node-> operator);
        fprintf (stream, "\n");
      }
    if (node-> text)
        print_indented (stream, level, "  text: %s\n", node-> text);
    if (node-> op1)
      {
        print_string (stream, level, "  op1:");
        print_node (stream, level + 1, node-> op1);
      }
    if (node-> op2)
      {
        print_string (stream, level, "  op2:");
        print_node (stream, level + 1, node-> op2);
      }
    if (node-> op3)
      {
        print_string (stream, level, "  op3:");
        print_node (stream, level + 1, node-> op3);
      }
    if (node-> pretty)
      {
        print_string (stream, level, "  pretty:");
        print_node (stream, level + 1, node-> pretty);
      }
    if (node-> format)
      {
        print_string (stream, level, "  format:");
        print_node (stream, level + 1, node-> format);
      }
}


void print_node_type (FILE *stream, int level, SCHEMA_NODE_TYPE type)
{
    switch (type)
      {
        case GG_COMMENT    :  print_string (stream, level, "COMMENT");   break;
        case GG_TEXT       :  print_string (stream, level, "TEXT");      break;
        case GG_SUBSTITUTE :  print_string (stream, level, "SUBSTITUTE");break;
        case GG_LITERAL    :  print_string (stream, level, "LITERAL");   break;
        case GG_SYMBOL     :  print_string (stream, level, "SYMBOL");    break;
        case GG_MEMBER     :  print_string (stream, level, "MEMBER");    break;
        case GG_FUNCTION   :  print_string (stream, level, "FUNCTION");  break;
        case GG_OPERAND    :  print_string (stream, level, "OPERAND");   break;
        case GG_OPERATOR   :  print_string (stream, level, "OPERATOR");  break;
        case GG_CLOSE      :  print_string (stream, level, "CLOSE");     break;
        case GG_ELSE       :  print_string (stream, level, "ELSE");      break;
        case GG_END_IF     :  print_string (stream, level, "END_IF");    break;
        case GG_END_FOR    :  print_string (stream, level, "END_FOR");   break;
        case GG_END_MACRO  :  print_string (stream, level, "END_MACRO"); break;
        case GG_END_NEW    :  print_string (stream, level, "END_NEW");   break;
        case GG_END_WHILE  :  print_string (stream, level, "END_WHILE"); break;
        case GG_DUMP       :  print_string (stream, level, "DUMP");      break;
        case GG_OUTPUT     :  print_string (stream, level, "OUTPUT");    break;
        case GG_APPEND     :  print_string (stream, level, "APPEND");    break;
        case GG_INCLUDE    :  print_string (stream, level, "INCLUDE");   break;
        case GG_INTERPRET  :  print_string (stream, level, "INTERPRET"); break;
        case GG_DELETE     :  print_string (stream, level, "DELETE");    break;
        case GG_COPY       :  print_string (stream, level, "COPY");      break;
        case GG_RENAME     :  print_string (stream, level, "RENAME");    break;
        case GG_ECHO       :  print_string (stream, level, "ECHO");      break;
        case GG_DEFINE     :  print_string (stream, level, "DEFINE");    break;
        case GG_MACRO      :  print_string (stream, level, "MACRO");     break;
        case GG_INVOKE     :  print_string (stream, level, "INVOKE");    break;
        case GG_XML        :  print_string (stream, level, "XML");       break;
        case GG_NEW        :  print_string (stream, level, "NEW");       break;
        case GG_IF         :  print_string (stream, level, "IF");        break;
        case GG_ELSIF      :  print_string (stream, level, "ELSIF");     break;
        case GG_FOR        :  print_string (stream, level, "FOR");       break;
        case GG_WHILE      :  print_string (stream, level, "WHILE");     break;
        case GG_ABORT      :  print_string (stream, level, "ABORT");     break;
        case GG_UNDEFINED  :  print_string (stream, level, "UNDEFINED"); break;
      }
}


void print_string (FILE *stream, int level, char *text)
{
    print_indented (stream, level, "%s\n", text);
}


void print_indented (FILE *stream, int level, char *format, ...)
{
    va_list argptr;                     /*  Argument list pointer            */

    fprintf (stream, "%*s", level * 4, "");
    va_start (argptr, format);          /*  Start variable arguments list    */
    vfprintf (stream, format, argptr);
    va_end   (argptr);                  /*  End variable arguments list      */
}


void print_operator (FILE *stream, OPERATOR op)
{
    switch (op)
      {
        case OP_UNDEFINED       :                             break;
        case TIMES              : fprintf (stream, "*");      break;
        case DIVIDE             : fprintf (stream, "/");      break;
        case PLUS               : fprintf (stream, "+");      break;
        case MINUS              : fprintf (stream, "-");      break;
        case EQUALS             : fprintf (stream, "=");      break;
        case NOT_EQUALS         : fprintf (stream, "<>");     break;
        case GREATER_THAN       : fprintf (stream, ">");      break;
        case LESS_THAN          : fprintf (stream, "<");      break;
        case GREATER_EQUAL      : fprintf (stream, ">=");     break;
        case LESS_EQUAL         : fprintf (stream, "<=");     break;
        case OR                 : fprintf (stream, "|");      break;
        case AND                : fprintf (stream, "&");      break;
        case NOT                : fprintf (stream, "!");      break;
        case SAFE_EQUALS        : fprintf (stream, "?=");     break;
        case SAFE_NOT_EQUALS    : fprintf (stream, "?<>");    break;
        case SAFE_GREATER_THAN  : fprintf (stream, "?>");     break;
        case SAFE_LESS_THAN     : fprintf (stream, "?<");     break;
        case SAFE_GREATER_EQUAL : fprintf (stream, "?>=");    break;
        case SAFE_LESS_EQUAL    : fprintf (stream, "?<=");    break;
        case NEXT_ARG           : fprintf (stream, ",");      break;
      }
}


/*  ---------------------------------------------------------------------[<]-
    Function: gg_error

    Synopsis: Returns a pointer to the last error message generated during 
    a call to gg_parse_schema or gg_parse_expression.
    ---------------------------------------------------------------------[>]-*/

char *gg_error (void)
{
    return error_message;
}


/*  ---------------------------------------------------------------------[<]-
    Function: gg_parse_schema

    Synopsis: Parses the supplied schema line into a schema parse tree.  If
    an error occurs, returns NULL and generates an error message which may
    be read with gg_error.
    ---------------------------------------------------------------------[>]-*/

SCHEMA_NODE *
gg_parse_schema (SCHEMA_READ *read)
{
    start_event = schema_event;
    schema_read = read;
    schema_line = buffer;
    return gg_start_parsing ();
}


SCHEMA_NODE *
gg_parse_expression (char *line)
{
    start_event = expression_event;
    schema_read = NULL;
    schema_line = line;
    schema_ptr  = 0;                    /*  Start of line                    */
    return gg_start_parsing ();
}


SCHEMA_NODE *
gg_start_parsing (void)
{
    feedback = NULL;

#   include "ggpars.i"                  /*  Do dialog manager                */
}


/*************************   INITIALISE THE PROGRAM   ************************/

MODULE initialise_the_program (void)
{
    spaces      = 0;
    brackets    = 0;
    transaction = mem_new_trans ();

    the_root     = NULL;
    the_parent   = NULL;
    the_node_ptr = & the_root;

    list_reset (& state_stack);
    list_reset (& quote_stack);

    the_next_event = start_event;
}


/****************************   READ SCHEMA LINE   ***************************/

MODULE read_schema_line (void)
{
    schema_ptr = 0;                     /*  Start of line                    */
    if (schema_read)
        if ((schema_read) (schema_line))
            return;

    token_posn      = 0;
    token_length    = 0;
    spaces          = 0;
    schema_line [0] = '\0';
    raise_exception (end_of_file_event);
}


/****************************   GET FIRST TOKEN   ****************************/

MODULE get_first_token (void)
{
    collect_spaces ();

    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    generate_character_token_event (".", schema_event);
    generate_literal_event (" ");
    generate_substitute_event ();
    generate_extend_event ();
    generate_end_of_line_event ();
    generate_other_event ();
}


void collect_spaces (void)
{
    spaces = 0;
    while (schema_line [schema_ptr] == ' ')
      {
        schema_ptr++;
        spaces++;
      }
}


void generate_character_token_event (const char *char_tokens,
                                     event_t char_event)
{
    if ((the_next_event == _LR_NULL_EVENT)
    &&  (the_token      != '\0')
    &&   strchr (char_tokens, the_token))
      {
        the_next_event = char_event;
        token_posn     = schema_ptr++;
        token_length   = 1;
      }
}


void generate_literal_event (const char *terminators)
{
    if (the_next_event == _LR_NULL_EVENT)
      {
        token_posn   = schema_ptr;
        token_length = 0;

        while ((the_token = schema_line [schema_ptr++]) != '\0')
            if (the_token == '\\')      /*  Ignore next char unless EOL      */
                if (schema_line [schema_ptr] == '\0')
                    break;
                else
                  {
                    schema_ptr++;
                    token_length += 2;
                  }
            else
                if ((strchr (terminators, the_token))
                ||  ((the_token == '$')
                &&   (schema_line [schema_ptr] == '(')))
                    break;
                else
                    token_length++;
        
        schema_ptr--;                   /*  Back to point at terminator      */
        if (token_length > 0)
            the_next_event = literal_event;
      }
}


void generate_substitute_event (void)
{
    if ((the_next_event == _LR_NULL_EVENT)
    &&  (the_token == '$')  
    &&  (schema_line [schema_ptr + 1] == '('))
      {
        the_next_event = substitute_event;
        token_posn     = schema_ptr;
        token_length   = 2;
        schema_ptr    += 2;
      }
}


void generate_extend_event (void)
{
    if ((the_next_event == _LR_NULL_EVENT)
    &&  (the_token      == '\\')
    &&  (schema_line [schema_ptr + 1] == '\0'))
      {
        the_next_event = extend_event;
        token_posn     = schema_ptr++;
        token_length   = 1;
      }
}


void generate_end_of_line_event (void)
{
    if ((the_next_event == _LR_NULL_EVENT)
    &&  (the_token      == '\0'))
        the_next_event = end_of_line_event;
}


void generate_other_event (void)
{
    if (the_next_event == _LR_NULL_EVENT)
      {
        the_next_event = other_event;
        token_posn   = schema_ptr;
        token_length = 0;
      }
}


/************************   GET SCHEMA COMMAND TOKEN   ***********************/

MODULE get_schema_command_token (void)
{
    collect_spaces ();

    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    generate_character_token_event ("-", comment_event);
    generate_literal_event (" .=<>+-/*,()?:%'\"");
    recognise_schema_command_token ();

    generate_other_event ();
}


void recognise_schema_command_token (void)
{
    char
        save;

    if (the_next_event == literal_event)
      {
        save = schema_line [token_posn + token_length];
        schema_line [token_posn + token_length] = 0;

        if (lexcmp (& schema_line [token_posn], "close") == 0)
            the_next_event = close_event;
        else
        if (lexcmp (& schema_line [token_posn], "else") == 0)
            the_next_event = else_event;
        else
        if (lexcmp (& schema_line [token_posn], "endif") == 0)
            the_next_event = end_if_event;
        else
        if (lexcmp (& schema_line [token_posn], "endfor") == 0)
            the_next_event = end_for_event;
        else
        if (lexcmp (& schema_line [token_posn], "endmacro") == 0)
            the_next_event = end_macro_event;
        else
        if (lexcmp (& schema_line [token_posn], "endnew") == 0)
            the_next_event = end_new_event;
        else
        if (lexcmp (& schema_line [token_posn], "endwhile") == 0)
            the_next_event = end_while_event;
        else
        if (lexcmp (& schema_line [token_posn], "dump") == 0)
            the_next_event = dump_event;
        else
        if (lexcmp (& schema_line [token_posn], "output") == 0)
            the_next_event = output_event;
        else
        if (lexcmp (& schema_line [token_posn], "append") == 0)
            the_next_event = append_event;
        else
        if (lexcmp (& schema_line [token_posn], "include") == 0)
            the_next_event = include_event;
        else
        if (lexcmp (& schema_line [token_posn], "interpret") == 0)
            the_next_event = interpret_event;
        else
        if (lexcmp (& schema_line [token_posn], "delete") == 0)
            the_next_event = delete_event;
        else
        if (lexcmp (& schema_line [token_posn], "copy") == 0)
            the_next_event = copy_event;
        else
        if (lexcmp (& schema_line [token_posn], "rename") == 0)
            the_next_event = rename_event;
        else
        if (lexcmp (& schema_line [token_posn], "echo") == 0)
            the_next_event = echo_event;
        else
        if (lexcmp (& schema_line [token_posn], "define") == 0)
            the_next_event = define_event;
        else
        if (lexcmp (& schema_line [token_posn], "macro") == 0)
            the_next_event = macro_event;
        else
        if (lexcmp (& schema_line [token_posn], "invoke") == 0)
            the_next_event = invoke_event;
        else
        if (lexcmp (& schema_line [token_posn], "xml") == 0)
            the_next_event = xml_event;
        else
        if (lexcmp (& schema_line [token_posn], "new") == 0)
            the_next_event = new_event;
        else
        if (lexcmp (& schema_line [token_posn], "if") == 0)
            the_next_event = if_event;
        else
        if (lexcmp (& schema_line [token_posn], "elsif") == 0)
            the_next_event = elsif_event;
        else
        if (lexcmp (& schema_line [token_posn], "for") == 0)
            the_next_event = for_event;
        else
        if (lexcmp (& schema_line [token_posn], "while") == 0)
            the_next_event = while_event;
        else
        if (lexcmp (& schema_line [token_posn], "abort") == 0)
            the_next_event = abort_event;

        schema_line [token_posn + token_length] = save;
      }
}


/**************************   GET EXPRESSION TOKEN   *************************/

MODULE get_expression_token (void)
{
    collect_spaces ();

    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    generate_quote_event ();
    generate_substitute_event ();
    generate_member_event ();
    generate_unary_operator_event ();
    generate_operator_event ();
    generate_next_arg_event ();
    generate_number_event ();
    generate_literal_event (" .=<>+-/*,()?:%'\"");
    recognise_schema_token ();
    generate_character_token_event (".", point_event);
    generate_character_token_event ("(", open_event);
    generate_character_token_event (")", close_event);
    generate_extend_event ();
    generate_end_of_line_event ();
    generate_other_event ();
}


void generate_quote_event (void)
{
    if ((the_next_event == _LR_NULL_EVENT)
    &&  ((the_token == '\'')
    ||   (the_token == '"')))
      {
        the_next_event = quote_event;
        token_posn     = schema_ptr++;
        token_length   = 1;
      }
}


void generate_unary_operator_event (void)
{
    if (the_next_event == _LR_NULL_EVENT)
      {
        the_operator = OP_UNDEFINED;
        if (the_token == '+')
            the_operator = PLUS;
        else
        if (the_token == '-')
            the_operator = MINUS;
        else
        if (the_token == '!')
            the_operator = NOT;

        if (the_operator != OP_UNDEFINED)
          {
            token_posn     = schema_ptr++;
            token_length   = 1;
            the_next_event = unary_operator_event;
          }
      }
}


void generate_operator_event (void)
{
    Bool
        safe;
    char
        the_char;

    if (the_next_event == _LR_NULL_EVENT)
      {
        token_posn   = schema_ptr++;
        token_length = 1;
        the_operator = OP_UNDEFINED;
        safe         = (the_token == '?');

        if (safe)
          {
            the_char = schema_line [schema_ptr++];
            token_length++;
          }
        else
            the_char = the_token;

        if (the_char == '*')
            the_operator = TIMES;
        else if (the_char == '/')
            the_operator = DIVIDE;
        else if (the_char == '+')
            the_operator = PLUS;
        else if (the_char == '-')
            the_operator = MINUS;
        else if (the_char == '=')
            the_operator = EQUALS;
        else if (the_char == '<')
            the_operator = LESS_THAN;
        else if (the_char == '>')
            the_operator = GREATER_THAN;
        else if (the_char == '|')
            the_operator = OR;
        else if (the_char == '&')
            the_operator = AND;
        else if (the_char == '!')
            the_operator = NOT;
        else
            schema_ptr = token_posn;

        if (the_operator == LESS_THAN)
            if (schema_line [schema_ptr] == '>')
              {
                the_operator = NOT_EQUALS;
                schema_ptr++;
                token_length++;
              }
            else if (schema_line [schema_ptr] == '=')
              {
                the_operator = LESS_EQUAL;
                schema_ptr++;
                token_length++;
              }
        if ((the_operator == GREATER_THAN)
        &&  (schema_line [schema_ptr] == '='))
          {
            the_operator = GREATER_EQUAL;
            schema_ptr++;
            token_length++;
          }
        if (safe)
            if (the_operator == EQUALS)
                the_operator = SAFE_EQUALS;
            else if (the_operator == NOT_EQUALS)
                the_operator = SAFE_NOT_EQUALS;
            else if (the_operator == GREATER_THAN)
                the_operator = SAFE_GREATER_THAN;
            else if (the_operator == LESS_THAN)
                the_operator = SAFE_LESS_THAN;
            else if (the_operator == GREATER_EQUAL)
                the_operator = SAFE_GREATER_EQUAL;
            else if (the_operator == LESS_THAN)
                the_operator = SAFE_LESS_EQUAL;
            else
              {
                the_operator = OP_UNDEFINED;
                schema_ptr = token_posn;
              }

        if (the_operator != OP_UNDEFINED)
            the_next_event = operator_event;
      }
}


void generate_next_arg_event (void)
{
    if ((the_next_event == _LR_NULL_EVENT)
    &&  (the_token == ','))
      {
        the_next_event = next_arg_event;
        token_posn     = schema_ptr++;
        token_length   = 1;
        the_operator   = NEXT_ARG;
      }
}
    

void generate_number_event (void)
{
    char
        *ptr;

    if (the_next_event == _LR_NULL_EVENT)
      {
        the_number   = strtol (& schema_line [schema_ptr], & ptr, 10);
        token_length = ptr - & schema_line [schema_ptr];
        if (token_length > 0)
          {
            token_posn     = schema_ptr;
            schema_ptr    += token_length;
            the_next_event = number_event;
          }
      }
}


void generate_member_event (void)
{
    if ((the_next_event == _LR_NULL_EVENT)
    &&  (the_token == '-')
    &&  (schema_line [schema_ptr + 1] == '>'))
      {
        the_next_event = member_event;
        token_posn     = schema_ptr+= 2;
        token_length   = 2;
      }
}


void recognise_schema_token (void)
{
    char
        save;

    if (the_next_event == literal_event)
      {
        save = schema_line [token_posn + token_length];
        schema_line [token_posn + token_length] = 0;

        if (lexcmp (& schema_line [token_posn], "as") == 0)
            the_next_event = as_event;
        else
        if (lexcmp (& schema_line [token_posn], "where") == 0)
            the_next_event = where_event;
        else
        if (lexcmp (& schema_line [token_posn], "by") == 0)
            the_next_event = by_event;
        else
        if (lexcmp (& schema_line [token_posn], "from") == 0)
            the_next_event = from_event;

        schema_line [token_posn + token_length] = save;
      }
}


/****************************   GET SCHEMA TOKEN   ***************************/

MODULE get_schema_token (void)
{
    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    generate_spaces_event ();
    generate_substitute_event ();
    generate_character_token_event ("=", equals_event);
    generate_literal_event (" .=<>+-/*,()?:%'\"");
    recognise_schema_token ();
    generate_extend_event ();
    generate_end_of_line_event ();

    generate_other_event ();
}


void generate_spaces_event (void)
{
    if ((the_next_event == _LR_NULL_EVENT)
    &&  (the_token == ' '))
      {
        collect_spaces ();
        token_posn     = schema_ptr;
        token_length   = 0;
        the_next_event = spaces_event;
      }
}


/***********************   GET FIRST IDENTIFIER TOKEN   **********************/

MODULE get_first_identifier_token (void)
{
    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    collect_spaces ();
    generate_substitute_event ();
    generate_literal_event (" .=<>+-/*,()?:%'\"");
    recognise_schema_token ();

    generate_other_event ();
}


/**************************   GET IDENTIFIER TOKEN   *************************/

MODULE get_identifier_token (void)
{
    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    generate_spaces_event ();
    generate_substitute_event ();
    generate_literal_event (" .=<>+-/*,()?:%'\"");
    recognise_schema_token ();
    generate_character_token_event (".", point_event);

    generate_other_event ();
}


/****************************   GET OUTPUT TOKEN   ***************************/

MODULE get_output_token (void)
{
    collect_spaces ();

    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    generate_literal_event (" ");
    generate_substitute_event ();
    generate_extend_event ();
    generate_end_of_line_event ();
    generate_other_event ();
}


/****************************   GET QUOTED TOKEN   ***************************/

MODULE get_quoted_token (void)
{
    char
        characters [3] = "  ",
        the_quote;

    collect_spaces ();

    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    list_pop  (&quote_stack, the_quote);
    characters [0] = the_quote;
    characters [2] = '\0';
    generate_literal_event (characters);

    characters [1] = '\0';
    generate_character_token_event (characters, quote_event);
    if (the_next_event != quote_event)
        list_push (&quote_stack, the_quote);

    generate_substitute_event ();
    generate_extend_event ();
    generate_end_of_line_event ();
    generate_other_event ();
}


/**************************   GET SUBSTITUTE TOKEN   *************************/

MODULE get_substitute_token (void)
{
    collect_spaces ();

    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    generate_character_token_event ("?:%", attribute_event);
    generate_literal_event (" ?:%)");
    generate_substitute_event ();
    generate_character_token_event (")", close_event);
    generate_extend_event ();
    generate_other_event ();
}


/*************************   GET WHITE SPACE TOKEN   *************************/

MODULE get_white_space_token (void)
{
    the_token      = schema_line [schema_ptr];
    the_next_event = _LR_NULL_EVENT;

    generate_spaces_event ();
    generate_extend_event ();
    generate_end_of_line_event ();
    generate_other_event ();
}


/***************************   EXPECT END OF LINE  ***************************/

MODULE expect_end_of_line (void)
{
    collect_spaces ();

    the_token = schema_line [schema_ptr];
    if (the_token != '\0')
        raise_exception (other_event);
}


/****************************   PUSH TEXT STATE   ****************************/

MODULE push_text_state (void)
{
    STATE
        state = TEXT;

    list_push  (&state_stack, state);
}


/*************************   PUSH SUBSTITUTE STATE   *************************/

MODULE push_substitute_state (void)
{
    STATE
        state = SUBSTITUTE;

    list_push  (&state_stack, state);
}


/***************************   PUSH OPERAND STATE   **************************/

MODULE push_operand_state (void)
{
    STATE
        state = OPERAND;

    list_push  (&state_stack, state);
}


/***********************   PUSH LITERAL OPERAND STATE   **********************/

MODULE push_literal_operand_state (void)
{
    STATE
        state = LITERAL_OPERAND;

    list_push  (&state_stack, state);
}


/****************************   PUSH SCOPE STATE   ***************************/

MODULE push_scope_state (void)
{
    STATE
        state = SCOPE;

    list_push  (&state_stack, state);
}


/*************************   PUSH IDENTIFIER STATE   *************************/

MODULE push_identifier_state (void)
{
    STATE
        state = IDENTIFIER;

    list_push  (&state_stack, state);
}


/*********************   PUSH IDENTIFIER CONTINUE STATE   ********************/

MODULE push_identifier_continue_state (void)
{
    STATE
        state = IDENTIFIER_CONTINUE;

    list_push  (&state_stack, state);
}


/**********************   PUSH SCOPE IDENTIFIER STATE   **********************/

MODULE push_scope_identifier_state (void)
{
    STATE
        state = SCOPE_IDENTIFIER;

    list_push  (&state_stack, state);
}


/*************************   PUSH WHITE SPACE STATE   ************************/

MODULE push_white_space_state (void)
{
    STATE
        state = WHITE_SPACE;

    list_push  (&state_stack, state);
}


/***************************   PUSH QUOTED STATE   ***************************/

MODULE push_quoted_state (void)
{
    STATE
        state = QUOTED;

    list_push  (&state_stack, state);
}


/**************************   PUSH FUNCTION STATE   **************************/

MODULE push_function_state (void)
{
    STATE
        state = FUNCTION;

    list_push  (&state_stack, state);
}


/**************************   PUSH ARGUMENTS STATE   *************************/

MODULE push_arguments_state (void)
{
    STATE
        state = ARGUMENTS;

    list_push  (&state_stack, state);
}


/**************************   PUSH ATTRIBUTE STATE   *************************/

MODULE push_attribute_state (void)
{
    STATE
        state = ATTRIBUTE;

    list_push  (&state_stack, state);
}


/**************************   PUSH ONE MORE STATE   **************************/

MODULE push_one_more_state (void)
{
    STATE
        state = ONE_MORE;

    list_push  (&state_stack, state);
}


/***************************   PUSH DEFINE STATE   ***************************/

MODULE push_define_state (void)
{
    STATE
        state = DEFINE;

    list_push  (&state_stack, state);
}


/***************************   PUSH INVOKE STATE   ***************************/

MODULE push_invoke_state (void)
{
    STATE
        state = INVOKE;

    list_push  (&state_stack, state);
}


/*****************************   PUSH XML STATE   ****************************/

MODULE push_xml_state (void)
{
    STATE
        state = XML;

    list_push  (&state_stack, state);
}


/*****************************   PUSH NEW STATE   ****************************/

MODULE push_new_state (void)
{
    STATE
        state = NEW;

    list_push  (&state_stack, state);
}


/*****************************   PUSH FOR STATE   ****************************/

MODULE push_for_state (void)
{
    STATE
        state = FOR;

    list_push  (&state_stack, state);
}


/****************************   PUSH MACRO STATE   ***************************/

MODULE push_macro_state (void)
{
    STATE
        state = MACRO;

    list_push  (&state_stack, state);
}


/*************************   PUSH MACRO ARGS STATE   *************************/

MODULE push_macro_args_state (void)
{
    STATE
        state = MACRO_ARGS;

    list_push  (&state_stack, state);
}


/*************************   PUSH INVOKE ARGS STATE   ************************/

MODULE push_invoke_args_state (void)
{
    STATE
        state = INVOKE_ARGS;

    list_push  (&state_stack, state);
}


/***************************   POP PREVIOUS STATE   **************************/

MODULE pop_previous_state (void)
{
    STATE
        state;

    if (list_empty (& state_stack))
        the_next_event = empty_stack_event;
    else
      {
        list_pop (&state_stack, state);
        switch (state)
          {
            case TEXT        : the_next_event = text_event;        break;
            case SUBSTITUTE  : the_next_event = substitute_event;  break;
            case SCOPE       : the_next_event = scope_event;       break;
            case OPERAND     : the_next_event = operand_event;     break;
            case LITERAL_OPERAND :
                               the_next_event = literal_operand_event;
                                                                   break;
            case IDENTIFIER  : the_next_event = identifier_event;  break;
            case IDENTIFIER_CONTINUE  :
                               the_next_event = identifier_continue_event;
                                                                   break;
            case SCOPE_IDENTIFIER  :
                               the_next_event = scope_identifier_event;
                                                                   break;
            case WHITE_SPACE : the_next_event = white_space_event; break;
            case QUOTED      : the_next_event = quoted_event;      break;
            case FUNCTION    : the_next_event = function_event;    break;
            case ARGUMENTS   : the_next_event = arguments_event;   break;
            case ATTRIBUTE   : the_next_event = attribute_event;   break;
            case ONE_MORE    : the_next_event = one_more_event;    break;
            case DEFINE      : the_next_event = define_event;      break;
            case MACRO       : the_next_event = macro_event;       break;
            case MACRO_ARGS  : the_next_event = macro_args_event;  break;
            case INVOKE      : the_next_event = invoke_event;      break;
            case INVOKE_ARGS : the_next_event = invoke_args_event; break;
            case XML         : the_next_event = xml_event;         break;
            case NEW         : the_next_event = new_event;         break;
            case FOR         : the_next_event = for_event;         break;
          }
      }
}


/**************************   PUSH QUOTE CHARACTER   *************************/

MODULE push_quote_character (void)
{
        list_push (&quote_stack, the_token);
}


/****************************   INSERT TEXT NODE   ***************************/

MODULE insert_text_node (void)
{
    char
        save;

    insert_simple_node (GG_TEXT);
    /*  After calling insert_simple_node, the_parent points to the new node  */

    /*  Convoluted way of extracting string straight from source line  */
    save = schema_line [token_posn + token_length];
    schema_line [token_posn + token_length] = 0;
    if (*the_node_ptr)
        (*the_node_ptr)-> text  = memt_strdup (transaction, & schema_line [token_posn]);
    else
        the_parent-> text  = memt_strdup (transaction, & schema_line [token_posn]);
    schema_line [token_posn + token_length] = save;
}


void insert_simple_node (SCHEMA_NODE_TYPE type)
{
    SCHEMA_NODE
        *new_leaf;

    new_leaf = memt_alloc (transaction, sizeof (*new_leaf));
    ASSERT (new_leaf);
    init_schema_node (*new_leaf);

    new_leaf-> type     = type;
    new_leaf-> spaces   = spaces;
    new_leaf-> brackets = brackets;

    spaces   = 0;
    brackets = 0;

    insert_the_node (new_leaf, OP_UNDEFINED);
}


/*  insert_the_node - does the tree-building work.  the_node_ptr is a        */
/*  pointer to a pointer (usually in another node, but may be the global     */
/*  variable the_root.  The value of *the_node_ptr may either be NULL, in    */
/*  which case the node *app_node is inserted, or non-NULL, in which case    */
/*  an operator node is inserted between **the_node_ptr and *app_node,       */
/*  which may by NULL.  If the_node_ptr is NULL then there is not current    */
/*  insertion point indicating an internal error.                            */

void insert_the_node (SCHEMA_NODE *app_node, OPERATOR operator)
{
    SCHEMA_NODE
        *new_node;

    ASSERT (the_node_ptr);
    if ((operator      != OP_UNDEFINED)
    ||  (*the_node_ptr != NULL))
      {
        if (*the_node_ptr)
            find_operator_insertion_point (operator);
        
        new_node = memt_alloc (transaction, sizeof (*new_node));
        ASSERT (new_node);
        init_schema_node (*new_node);
        
        new_node-> type     = GG_OPERATOR;
        new_node-> operator = operator;
        new_node-> op1      = *the_node_ptr;
        new_node-> op2      = app_node;
        new_node-> parent   = the_parent;
        new_node-> parent   = the_parent;
        
        /*  Migrate brackets, spaces and length to highest level  */
        if (*the_node_ptr)
          {
            new_node-> spaces   = (*the_node_ptr)-> spaces;
            new_node-> length   = (*the_node_ptr)-> length;
            new_node-> brackets = (*the_node_ptr)-> brackets;
            (*the_node_ptr)-> brackets = 0;
            (*the_node_ptr)-> parent   = new_node;
          }

        *the_node_ptr = new_node;

        if (app_node)
          {
            app_node -> parent = new_node;

            the_node_ptr = &new_node-> op2;
            the_parent   = new_node;
          }
      }
    else
      {
        ASSERT (app_node);
        app_node-> parent = the_parent;
        *the_node_ptr     = app_node;
      }
}


int priority (OPERATOR operator)
{
    switch (operator)
      {
        case OP_UNDEFINED       : return 7;
        case TIMES              :
        case DIVIDE             : return 6;
        case PLUS               :
        case MINUS              : return 5;
        case EQUALS             :
        case NOT_EQUALS         :
        case GREATER_THAN       :
        case LESS_THAN          :
        case GREATER_EQUAL      :
        case LESS_EQUAL         :
        case SAFE_EQUALS        :
        case SAFE_NOT_EQUALS    :
        case SAFE_GREATER_THAN  :
        case SAFE_LESS_THAN     :
        case SAFE_GREATER_EQUAL :
        case SAFE_LESS_EQUAL    : return 4;
        case NOT                : return 3;
        case OR                 :
        case AND                : return 2;
        case NEXT_ARG           : return 1;
      }
    return 0;
}


/*  This function finds the tree node to insert a new operator.  It uses    */
/*  the function 'priority' to determine precendence and the function       */
/*  'grouping' to determing if the grouping of the same operator should     */
/*  take place from the left (ie A+B+C is equivalent to (A+B)+C) or from    */
/*  the right (ie A+B+C is equivalent to A+(B+C)).                          */
void find_operator_insertion_point (OPERATOR operator)
{
    if (grouping (operator))
        while ( (the_parent)
                 ? (   ((*the_node_ptr)-> brackets == 0)
                    && (the_parent-> type == GG_OPERATOR)
                    && ( (priority (the_parent-> operator) >= priority (operator))
                      || (!the_parent-> op1) ) )
                 : FALSE )
            return_one_level ();
    else
        while ( (the_parent)
                 ? (   ((*the_node_ptr)-> brackets == 0)
                    && (the_parent-> type == GG_OPERATOR)
                    && ( (priority (the_parent-> operator) >  priority (operator))
                      || (!the_parent-> op1) ) )
                 : FALSE )
            return_one_level ();
}


Bool grouping (OPERATOR operator)
{
    switch (operator)
      {
        case NEXT_ARG      : return FALSE;
        default            : return TRUE;
      }
}


void return_one_level (void)
{
    ASSERT (the_parent);

    /*  Length and spaces of child is added to length of parent  */
    if (the_node_ptr)
        if (*the_node_ptr)
            the_parent-> length += (*the_node_ptr)-> spaces
                                 + (*the_node_ptr)-> length;

    /*  Find out which child the current parent is */
    if (the_parent-> parent == NULL)
        the_node_ptr = & the_root;
    else
    if (the_parent == the_parent-> parent-> op1)
        the_node_ptr = & the_parent-> parent-> op1;
    else
    if (the_parent == the_parent-> parent-> op2)
        the_node_ptr = & the_parent-> parent-> op2;
    else
    if (the_parent == the_parent-> parent-> op3)
        the_node_ptr = & the_parent-> parent-> op3;
    else
    if (the_parent == the_parent-> parent-> pretty)
        the_node_ptr = & the_parent-> parent-> pretty;
    else
    if (the_parent == the_parent-> parent-> format)
        the_node_ptr = & the_parent-> parent-> format;
    else
      {
        raise_exception (anomaly_event);
        return;
      }
    the_parent = the_parent-> parent;
/*     print_node_type (stdout, 0, (*the_node_ptr)-> type); */
}


/*************************   INSERT SUBSTITUTE NODE   ************************/

MODULE insert_substitute_node (void)
{
    insert_simple_node (GG_SUBSTITUTE);
}


/**************************   READY FOR OP1 FIELD   **************************/

MODULE ready_for_op1_field (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    the_parent   =    *the_node_ptr;
    the_node_ptr = & (*the_node_ptr)-> op1;
}


/**************************   READY FOR OP2 FIELD   **************************/

MODULE ready_for_op2_field (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    the_parent   =    *the_node_ptr;
    the_node_ptr = & (*the_node_ptr)-> op2;
}


/**************************   READY FOR OP3 FIELD   **************************/

MODULE ready_for_op3_field (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    the_parent   =    *the_node_ptr;
    the_node_ptr = & (*the_node_ptr)-> op3;
}


/*************************   READY FOR PRETTY FIELD   ************************/

MODULE ready_for_pretty_field (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    the_parent   =    *the_node_ptr;
    the_node_ptr = & (*the_node_ptr)-> pretty;
}


/*************************   READY FOR FORMAT FIELD   ************************/

MODULE ready_for_format_field (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    the_parent   =    *the_node_ptr;
    the_node_ptr = & (*the_node_ptr)-> format;
}


/****************************   MOVE OP1 TO OP2   ****************************/

MODULE move_op1_to_op2 (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    (*the_node_ptr)-> op2 = (*the_node_ptr)-> op1;
    (*the_node_ptr)-> op1 = NULL;
}


/***********************   READY FOR ATTRIBUTE FIELD   ***********************/

MODULE ready_for_attribute_field (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    the_parent = *the_node_ptr;

    if (the_token == '?')
        the_node_ptr = & (*the_node_ptr)-> op3;
    else
    if (the_token == ':')
        the_node_ptr = & (*the_node_ptr)-> pretty;
    else
    if (the_token == '%')
        the_node_ptr = & (*the_node_ptr)-> format;
    else
      {
        raise_exception (anomaly_event);
        return;
      }

    if (*the_node_ptr != NULL)
        error_exception ("Duplicate attribute");
}


/**************************   READY FOR NULL FIELD   *************************/

MODULE ready_for_null_field (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    the_parent   = *the_node_ptr;
    the_node_ptr = NULL;
}


/****************************   SET EXTEND FLAG   ****************************/

MODULE set_extend_flag (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    (*the_node_ptr)-> extend = TRUE;
}


/**************************   SET LINE BREAK FLAG   **************************/

MODULE set_line_break_flag (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    (*the_node_ptr)-> line_break = TRUE;
}


/************************   INSERT SPACES ONLY NODE   ************************/

MODULE insert_spaces_only_node (void)
{
    int
        length;

    if ((spaces > 0)
    ||  (*the_node_ptr == NULL))
      {
        length = token_length;
        token_length = 0;               /*  This node is just for the spaces */
        insert_text_node ();
        token_length = length;
      }
}


/**************************   RETURN TO ROOT NODE   **************************/

MODULE return_to_root_node (void)
{
    while (the_parent)
        return_one_level ();

}


/***********************   COMMIT MEMORY ALLOCATIONS   ***********************/

MODULE commit_memory_allocations (void)
{
    mem_commit (transaction);
    feedback = the_root;
}


/**************************   INSERT LITERAL NODE   **************************/

MODULE insert_literal_node (void)
{
    insert_simple_node (GG_LITERAL);
}


/***************************   COUNT TOKEN LENGTH   **************************/

MODULE count_token_length (void)
{
    ASSERT (the_node_ptr);

    if (*the_node_ptr)
      {
        (*the_node_ptr)-> length += token_length;
        (*the_node_ptr)-> spaces += spaces;
      }
    else
      {
        ASSERT (the_parent);
        (the_parent)-> length += token_length;
        (the_parent)-> spaces += spaces;
      }
    spaces = 0;
}


/*********************   COUNT SPACES AND TOKEN LENGTH   *********************/

MODULE count_spaces_and_token_length (void)
{
    ASSERT (the_node_ptr);

    if (*the_node_ptr)
        (*the_node_ptr)-> length += spaces + token_length;
    else
      {
        ASSERT (the_parent);
        (the_parent)-> length += spaces + token_length;
      }

    spaces = 0;
}


/****************************   RETURN TO PARENT   ***************************/

MODULE return_to_parent (void)
{
    while (the_parent)
        if ((the_parent-> type     == GG_OPERATOR)
        &&  (the_parent-> operator == OP_UNDEFINED))
            return_one_level ();
        else
            break;

    if (the_parent)
        return_one_level ();
}


/***************************   INSERT SYMBOL NODE   **************************/

MODULE insert_symbol_node (void)
{
    insert_simple_node (GG_SYMBOL);
}


/***************************   INSERT MEMBER NODE   **************************/

MODULE insert_member_node (void)
{
    insert_simple_node (GG_MEMBER);
}


/**************************   INSERT OPERAND NODE   **************************/

MODULE insert_operand_node (void)
{
    insert_simple_node (GG_OPERAND);
}


/****************************   ADD ONE BRACKET   ****************************/

MODULE add_one_bracket (void)
{
    brackets++;
}


/**************************   REWIND TO SAME TOKEN   *************************/

MODULE rewind_to_same_token (void)
{
    schema_ptr = token_posn - spaces;
}


/*********************   CHANGE OPERAND TO SYMBOL NODE   *********************/

MODULE change_operand_to_symbol_node (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    if ((*the_node_ptr)-> type == GG_OPERAND)
        (*the_node_ptr)-> type = GG_SYMBOL;
}


/**********************   CHANGE SYMBOL TO MEMBER NODE   *********************/

MODULE change_symbol_to_member_node (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    if ((*the_node_ptr)-> type == GG_SYMBOL)
        (*the_node_ptr)-> type = GG_MEMBER;
}


/************************   CHANGE TO FUNCTION NODE   ************************/

MODULE change_to_function_node (void)
{
    ASSERT (the_node_ptr);
    ASSERT (*the_node_ptr);

    if ((((*the_node_ptr)-> type == GG_SYMBOL)
    &&   ((*the_node_ptr)-> op2  == NULL))
    ||   ((*the_node_ptr)-> type == GG_OPERAND))
        (*the_node_ptr)-> type = GG_FUNCTION;
    else
        error_exception ("Unexpected token.");
}


/**************************   INSERT OPERATOR NODE   *************************/

MODULE insert_operator_node (void)
{
    insert_the_node (NULL, the_operator);

    (*the_node_ptr)-> brackets += brackets;
    brackets = 0;
}


/*********************   INSERT UNDEFINED OPERATOR NODE   ********************/

MODULE insert_undefined_operator_node (void)
{
    insert_the_node (NULL, OP_UNDEFINED);

    (*the_node_ptr)-> brackets += brackets;
    brackets = 0;
}


/***************************   CLOSE ONE BRACKET   ***************************/

MODULE close_one_bracket (void)
{
    ASSERT (the_node_ptr);

    if (*the_node_ptr == NULL)
        raise_exception (no_bracket_event);
    else
      {
        while ( (the_parent)
                 ? (((*the_node_ptr)-> brackets == 0)
                   && (the_parent-> type     == GG_OPERATOR)
                   && (the_parent-> operator != NEXT_ARG))
                 : FALSE )
            return_one_level ();

        if ((*the_node_ptr)-> brackets > 0)
            (*the_node_ptr)-> brackets--;
        else
            raise_exception (no_bracket_event);
      }
}


/***********************   RETURN TO ARGUMENTS PARENT   **********************/

MODULE return_to_arguments_parent (void)
{
    while (the_parent)
        if (((*the_node_ptr)-> type     == GG_OPERATOR)
        &&  ((*the_node_ptr)-> operator == NEXT_ARG))
            return_one_level ();
        else
            break;
}


/**********************   RETURN TO EXPRESSION PARENT   **********************/

MODULE return_to_expression_parent (void)
{
    if (the_node_ptr)
        if (*the_node_ptr)
            if ((*the_node_ptr)-> brackets > 0)
                error_exception ("Mismatched brackets.");

    while (the_parent)
        if ((the_parent-> type     == GG_OPERATOR)
        &&  (the_parent-> operator != NEXT_ARG)
        &&  (the_parent-> brackets == 0))
            return_one_level ();
        else
            break;

    if (the_parent)
        return_one_level ();
}


/************************   READY FOR NEXT ARGUMENT   ************************/

MODULE ready_for_next_argument (void)
{
    /*  This is a bit of a fudge, but cleaner than any other solution I     */
    /*  could find.  We have to go back to the op2 field which holds the    */
    /*  argument list, so we undo the length calculation which was done     */
    /*  previously.  This calc'n will be redone later.                      */
    if ((*the_node_ptr)-> op2)
        (*the_node_ptr)-> length -= (*the_node_ptr)-> op2-> spaces
                                  + (*the_node_ptr)-> op2-> length;

    ready_for_op2_field ();
    insert_operator_node ();
    ready_for_op2_field ();
}


/**********************   INSERT EMPTY NODE IF NEEDED   **********************/

MODULE insert_empty_node_if_needed (void)
{
    if (the_node_ptr)
        if (!*the_node_ptr)
          {
            insert_simple_node (GG_TEXT);
            (*the_node_ptr)-> text = memt_strdup (transaction, "");
          }
}


/**********************   CONFIRM ATTRIBUTE SPECIFIED   **********************/

MODULE confirm_attribute_specified (void)
{
    if (!the_node_ptr)
        error_exception ("Unexpected token.");
}


/**************************   INSERT COMMENT NODE   **************************/

MODULE insert_comment_node (void)
{
    insert_simple_node (GG_COMMENT);
}


/***************************   INSERT CLOSE NODE   ***************************/

MODULE insert_close_node (void)
{
    insert_simple_node (GG_CLOSE);
}


/****************************   INSERT ELSE NODE   ***************************/

MODULE insert_else_node (void)
{
    insert_simple_node (GG_ELSE);
}


/***************************   INSERT END IF NODE   **************************/

MODULE insert_end_if_node (void)
{
    insert_simple_node (GG_END_IF);
}



/**************************   INSERT END FOR NODE   **************************/

MODULE insert_end_for_node (void)
{
    insert_simple_node (GG_END_FOR);
}


/*************************   INSERT END MACRO NODE   *************************/

MODULE insert_end_macro_node (void)
{
    insert_simple_node (GG_END_MACRO);
}


/**************************   INSERT END NEW NODE   **************************/

MODULE insert_end_new_node (void)
{
    insert_simple_node (GG_END_NEW);
}


/*************************   INSERT END WHILE NODE   *************************/

MODULE insert_end_while_node (void)
{
    insert_simple_node (GG_END_WHILE);
}


/****************************   INSERT DUMP NODE   ***************************/

MODULE insert_dump_node (void)
{
    insert_simple_node (GG_DUMP);
}


/***************************   INSERT OUTPUT NODE   **************************/

MODULE insert_output_node (void)
{
    insert_simple_node (GG_OUTPUT);
}


/***************************   INSERT APPEND NODE   **************************/

MODULE insert_append_node (void)
{
    insert_simple_node (GG_APPEND);
}


/**************************   INSERT INCLUDE NODE   **************************/

MODULE insert_include_node (void)
{
    insert_simple_node (GG_INCLUDE);
}


/*************************   INSERT INTERPRET NODE   *************************/

MODULE insert_interpret_node (void)
{
    insert_simple_node (GG_INTERPRET);
}


/***************************   INSERT DELETE NODE   **************************/

MODULE insert_delete_node (void)
{
    insert_simple_node (GG_DELETE);
}


/****************************   INSERT COPY NODE   ***************************/

MODULE insert_copy_node (void)
{
    insert_simple_node (GG_COPY);
}


/***************************   INSERT RENAME NODE   **************************/

MODULE insert_rename_node (void)
{
    insert_simple_node (GG_RENAME);
}


/****************************   INSERT ECHO NODE   ***************************/

MODULE insert_echo_node (void)
{
    insert_simple_node (GG_ECHO);
}


/***************************   INSERT ABORT NODE   ***************************/

MODULE insert_abort_node (void)
{
    insert_simple_node (GG_ABORT);
}


/***************************   INSERT DEFINE NODE   **************************/

MODULE insert_define_node (void)
{
    insert_simple_node (GG_DEFINE);
}


/***************************   INSERT MACRO NODE   ***************************/

MODULE insert_macro_node (void)
{
    insert_simple_node (GG_MACRO);
}


/***************************   INSERT INVOKE NODE   **************************/

MODULE insert_invoke_node (void)
{
    insert_simple_node (GG_INVOKE);
}


/****************************   INSERT XML NODE   ****************************/

MODULE insert_xml_node (void)
{
    insert_simple_node (GG_XML);
}


/****************************   INSERT NEW NODE   ****************************/

MODULE insert_new_node (void)
{
    insert_simple_node (GG_NEW);
}


/*****************************   INSERT IF NODE   ****************************/

MODULE insert_if_node (void)
{
    insert_simple_node (GG_IF);
}


/***************************   INSERT ELSIF NODE   ***************************/

MODULE insert_elsif_node (void)
{
    insert_simple_node (GG_ELSIF);
}


/****************************   INSERT FOR NODE   ****************************/

MODULE insert_for_node (void)
{
    insert_simple_node (GG_FOR);
}


/***************************   INSERT WHILE NODE   ***************************/

MODULE insert_while_node (void)
{
    insert_simple_node (GG_WHILE);
}


/*************************   SIGNAL INTERNAL ERROR   *************************/

MODULE signal_internal_error (void)
{
    sprintf (error_message, "Internal parser error.\n");
}


/**********************   ROLLBACK MEMORY ALLOCATIONS   **********************/

MODULE rollback_memory_allocations (void)
{
    STATE
        state;
    char
        ch;

    mem_rollback (transaction);
    while (!list_empty (& state_stack))
        list_pop (&state_stack, state);

    while (!list_empty (& quote_stack))
        list_pop (&quote_stack, ch);

    feedback = NULL;
}


/*************************   SIGNAL INVALID TOKEN   **************************/

MODULE signal_invalid_token (void)
{
    error_exception ("Unexpected token.");
}


/*********************   SIGNAL UNEXPECTED END OF FILE   *********************/

MODULE signal_unexpected_end_of_file (void)
{
    error_exception ("Unexpected end of file.");
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

/*****************************************************************************/

void
error_exception (char *format, ...)
{
    char
        *mess_ptr;
    int
        offset;
    va_list
        argptr;

    mess_ptr = error_message; 
    if (*schema_line)
      {
        offset = sprintf (mess_ptr, "%s\n",  schema_line);
        mess_ptr += offset;
        offset = sprintf (mess_ptr, "%*s\n", token_posn, "^");
        mess_ptr += offset;
      }
    va_start (argptr, format);          /*  Start variable arguments list    */
    offset = vsprintf (mess_ptr, format, argptr);
    va_end   (argptr);                  /*  End variable arguments list      */
    mess_ptr += offset;

    sprintf (mess_ptr, "\n");
    raise_exception (error_event);
}
