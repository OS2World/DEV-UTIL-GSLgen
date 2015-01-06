/*=========================================================================== 
 *                                                                            
 *   gslgen.c    GSL code generator                                           
 *                                                                            
 *   Written:    98/07/04    iMatix <tools@imatix.com>                        
 *   Revised:    99/06/01
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
#include "ggcode.h"                     /*  Include header file              */
#include "gslgen.d"                     /*  Include dialog data              */


/*- Macros ------------------------------------------------------------------*/

#define PATH       "PATH"


/*- Function prototypes -----------------------------------------------------*/


/*- Global variables used in this source file only --------------------------*/

static int
    feedback,                           /*  Feedback for calling program     */
    next_arg,
    argc;

static char
    **argv,
    *arg,
    *xml_name,
    *me      = "gslgen",
    *version = "1.2";

static XML_ITEM
    *xml_switches = NULL,
    *xml_source   = NULL;

static Bool
    quiet = FALSE;


/********************************   M A I N   ********************************/

int
main (int _argc, char *_argv [])
{
    argc = _argc;
    argv = _argv;

    feedback = 0;                       /*  No errors so far                 */
#   include "gslgen.i"                  /*  Include dialog interpreter       */
}


/*************************   INITIALISE THE PROGRAM   ************************/

MODULE initialise_the_program (void)
{
    if (argc > 1)
      {
        the_next_event = ok_event;
        next_arg = 1;
      }
    else
        the_next_event = anomaly_event;
}


/************************   INITIALISE PROGRAM DATA   ************************/

MODULE initialise_program_data (void)
{
    xml_switches = xml_new (NULL, NULL, NULL);
}


/************************   GET NEXT ARGUMENT VALUE   ************************/

MODULE get_next_argument_value (void)
{
    if (next_arg < argc)
      {
        arg = argv [next_arg++];
        if (arg[0] == '-')
            the_next_event = switch_event;
        else
          {
            xml_name = arg;
            the_next_event = ok_event;
          }
      }
    else
        the_next_event = finished_event;
}


/**********************   DISPLAY WELCOME IF NOT QUIET   *********************/

MODULE display_welcome_if_not_quiet (void)
{
    if (!quiet)
      {
        fprintf (stderr,
                 "%s - Generalised Schema Language Generator V%s\n",
                 me, version);
        fprintf (stderr,
                 "Copyright (c) 1996-99 iMatix - http://www.imatix.com\n\n");
      }
}


/**********************   DISPLAY COMMAND LINE SYNTAX   **********************/

MODULE display_command_line_syntax (void)
{
    fprintf (stderr, 
             "syntax: %s -<option> ... -<attr>[:<value>] ... <filename> ...\n",
             me);
    fprintf (stderr, "    Options:\n");
    fprintf (stderr, "        -q   quiet: suppress routine messages\n");
}


/***************************   PROCESS THE SWITCH   **************************/

MODULE process_the_switch (void)
{
    char
        *name,
        *value;

    name  = strtok (arg, ":") + 1;
    value = strtok (NULL, "");
    if (((lexcmp (name, "q")     == 0)
    ||   (lexcmp (name, "quiet") == 0))
    &&  (!value))
        quiet = TRUE;
    else
      {
        if (value)
            xml_put_attr (xml_switches, name, value);
        else
            xml_put_attr (xml_switches, name, "");
      }
}


/*****************************   READ XML FILE   *****************************/

MODULE read_xml_file (void)
{
    if (!quiet)
        fprintf (stderr, "%s I: processing %s...\n", me, xml_name);

    xml_source = xml_load (PATH, xml_name);
    if (!xml_source)
      {
        fprintf (stderr, "%s E: Error processing %s...\n", me, xml_name);
        fprintf (stderr, xml_error ());
        raise_exception (anomaly_event); 
      }
}


/***********************   OVERLAY STANDARD ONTO XML   ***********************/

MODULE overlay_standard_onto_xml (void)
{
    xml_put_attr (xml_first_child (xml_source), "date",
                  conv_date_pict (date_now (), "yyyy/mm/dd"));
    xml_put_attr (xml_first_child (xml_source), "time",
                  conv_time_pict (time_now (), "h:mm:ss"));
    xml_put_attr (xml_first_child (xml_source), "version",    version);
    xml_put_attr (xml_first_child (xml_source), "me",         me);
    xml_put_attr (xml_first_child (xml_source), "shuffle",    "1");
    xml_put_attr (xml_first_child (xml_source), "ignorecase", "1");
    xml_put_attr (xml_first_child (xml_source), "filename",   xml_name);
}


/***********************   OVERLAY SWITCHES ONTO XML   ***********************/

MODULE overlay_switches_onto_xml (void)
{
    XML_ATTR
        *xml_switch;

    FORATTRIBUTES (xml_switch, xml_switches)
        xml_put_attr (xml_first_child (xml_source),
                      xml_attr_name (xml_switch),
                      xml_attr_value (xml_switch));
}


/**************************   CALL CODE GENERATOR   **************************/

MODULE call_code_generator (void)
{
    feedback = gg_code (xml_first_child (xml_source));
}


/***************************   FREE XML SRUCTURES   **************************/

MODULE free_xml_sructures (void)
{
    if (xml_source)
        xml_free (xml_source);
}


/************************   FREE SWITCHES STRUCTURE   ************************/

MODULE free_switches_structure (void)
{
    if (xml_switches)
        xml_free (xml_switches);
}


/***************************   GET EXTERNAL EVENT   **************************/

MODULE get_external_event (void)
{
}


/*************************   TERMINATE THE PROGRAM   *************************/

MODULE terminate_the_program (void)
{
    mem_assert ();
    the_next_event = terminate_event;
}
