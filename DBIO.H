/*  ----------------------------------------------------------------<Prolog>-
    Name:       dbio.h
    Title:      Database interface - header file
    Package:    Studio

    Written:    97/01/25  iMatix <tools@imatix.com>
    Revised:    98/03/08

    Synopsis:   Defines structures and constants for the db interface.

    This software is copyright (c) 1996-98 iMatix.  All rights reserved.
    Use and distribution of this software, documentation, executables,
    source files, and object files is only permitted in the context of a
    current License Agreement between the user and iMatix.
 ------------------------------------------------------------------</Prolog>-*/

#ifndef _DBIO_INCLUDED
#define _DBIO_INCLUDED

#include "sfl.h"

/*- These definitions depend on compile-time options ------------------------*/

#if defined (_WINDOWS) || defined (WIN32)
#    define __DBIO_ODBC
#elif defined (DB_ORACLE)
#    define __DBIO_ORACLE
#endif

#define GOTO_NEXT_RECORD       99999
#define ERR_MSG_SIZE           300
#define ERR_CODE_SIZE          20

#define CACHE_SIZE             21
#define ORA_CACHE_SIZE         21

/* Error structure                                                           */

typedef struct
{
    int  code;                          /* Error code ( 0 = NO ERROR)        */
    char code_msg [ERR_CODE_SIZE];      /* Message code                      */
    char message  [ERR_MSG_SIZE];       /* Error message                     */
    int  parse_offset;                  /* Parse error offset                */
    int  nb_rows;                       /* number of rows processed          */
} DBIO_ERR;

/* All feedback type                                                         */

enum {
    OK                    = 0,
    HARD_ERROR            = 1,
    ILLEGAL_OPERATION     = 2,
    ILLEGAL_MODE          = 3,
    RECORD_NOT_PRESENT    = 4,
    DUPLICATE_RECORD      = 5,
    ILLEGAL_FILE_USAGE    = 6,
    RECORD_LOCKED         = 7,
    RECORD_CHANGED        = 8,
    RECORD_INTEGRITY      = 9,
    INVALID_SEARCH_KEY    = 10,
    NOT_CONNECTED         = 11
};

/* Position mode                                                             */

enum {
    THIS_RECORD           = 0,
    EQUAL_TO              = 1,
    GREATER_OR_EQUAL      = 2,
    GREATER_THAN          = 3,
    LESS_THAN             = 4,
    LESS_THAN_OR_EQUAL    = 5,
    NEXT_RECORD           = 6,
    PRIOR_RECORD          = 7,
    LAST_RECORD           = 8
};

/* KEY selector                                                              */

enum {
    PRIMARY               = 0,
    ALTERNATE_A           = 1,
    ALTERNATE_B           = 2,
    ALTERNATE_C           = 3,
    ALTERNATE_D           = 4,
    ALTERNATE_E           = 5,
    ALTERNATE_F           = 6,
    ALTERNATE_G           = 7,
    ALTERNATE_H           = 8,
    ALTERNATE_I           = 9,
    ALTERNATE_J           = 10,
    ALTERNATE_K           = 11,
    ALTERNATE_L           = 12,
    ALTERNATE_M           = 13,
    ALTERNATE_N           = 14,
    ALTERNATE_O           = 15,
    ALTERNATE_P           = 16,
    ALTERNATE_Q           = 17,
    ALTERNATE_R           = 18,
    ALTERNATE_S           = 19,
    ALTERNATE_T           = 20,
    ALTERNATE_U           = 21,
    ALTERNATE_V           = 22,
    ALTERNATE_W           = 23,
    ALTERNATE_X           = 24,
    ALTERNATE_Y           = 25,
    ALTERNATE_Z           = 26,
    ALTERNATE_ERROR       = 27
};

#ifdef __cplusplus
extern "C" {
#endif

void     *dbio_connect                      (char *default_db_name);
void     *dbio_get_connect_handle           (char *default_db_name);
void      dbio_disconnect                   (void);
int       dbio_terminate_transaction        (void);
int       dbio_restore_previous_transaction (void);
DBIO_ERR *dbio_get_error                    (void);
int       dbio_error_code                   (void);
char     *dbio_error_message                (void);
int       dbio_nb_rows                      (void);

#ifdef __cplusplus
}
#endif

#endif
