.----------------------------------------------------------------------------
.-
.-  predbml.sch - Prelude schema for DBML files.
.-
.-  This validates the data read from a DBM (database markup language) file.
.-
.-  Written:    98/09/11    Jonathan Schultz
.-  Revised:    98/10/02    Jonathan Schultz
.-
.-  Copyright:  Copyright (c) 1996-99 iMatix Corporation
.-
.-  License:    This is Open Source software; you can redistribute it and/or
.-              modify it under the terms of the iMatix Corporation Open
.-              Source License Agreement as provided in the file LICENSE.TXT.
.-              This software is distributed in the hope that it will be
.-              useful, but without any warranty.
.----------------------------------------------------------------------------
.-
.define TABLE.SIZE       = 0
.define TABLE.MAXNAMELEN = 0
.-
.if !defined (TABLE.NAME)
.    abort "ERROR - Missing table name"
.endif
.if !defined (TABLE.FILEIO)
.    abort "ERROR - Missing table fileio"
.endif
.-
.for FIELD
.-   Ensure compulsory attributess are present
.    if !defined (FIELD.NAME)
.        abort "ERROR - Missing field name in field: $(NAME)"
.    endif
.-   Ensure field name is unique
.    for TABLE.FIELD as CHECKFIELD where FIELD.NAME = CHECKFIELD.NAME & index (FIELD) < index (CHECKFIELD)
.        abort "ERROR - Duplicate field name: $(NAME)"
.    endfor
.    if !defined (FIELD.TYPE)
.        abort "ERROR - Missing field type in field: $(NAME)"
.    endif
.-   Validate field type
.    if TYPE = "TEXTUAL"
.        if !defined (FIELD.SIZE)
.            define FIELD.SIZE = 10
.        endif
.    elsif TYPE = "NUMERIC"
.        if !defined (FIELD.SIZE)
.            define FIELD.SIZE = 8
.        endif
.        if !defined (FIELD.DECIMAL)
.            define FIELD.DECIMAL = 0
.        endif
.    elsif TYPE = "DATE"
.        define FIELD.SIZE = 8
.    elsif TYPE = "BOOLEAN"
.        define FIELD.SIZE = 1
.    else
.        abort "ERROR - Field $(NAME) has unknown type: $(TYPE)"
.    endif
.-   Deal with field size and field name length
.    define TABLE.SIZE = TABLE.SIZE + FIELD.SIZE
.    if length (NAME) > MAXNAMELEN
.        define MAXNAMELEN = length (NAME)
.    endif
.endfor
.-
.for KEY
.    if !defined (DUPLICATES)
.        define DUPLICATES = "FALSE"
.    endif
.-   Validate fields in each key
.    for FIELD
.-       Find definition of field
.        define FIELD.NUM
.        for TABLE.FIELD as CHECKFIELD where NAME = FIELD.NAME
.            define FIELD.NUM  = index (CHECKFIELD)
.            define FIELD.TYPE        $(CHECKFIELD.TYPE)
.            if defined (CHECKFIELD.SIZE)
.                define FIELD.SIZE    $(CHECKFIELD.SIZE)
.            endif
.            if defined (CHECKFIELD.DECIMAL)
.                define FIELD.DECIMAL $(CHECKFIELD.DECIMAL)
.            endif
.            if defined (CHECKFIELD.COMMENT)
.                define FIELD.COMMENT $(CHECKFIELD.COMMENT:)
.            endif
.        endfor
.        if !defined (NUM)
.            abort "ERROR - Undeclared field: $(NAME) in key: $(KEY.NAME)"
.        endif
.    endfor
.endfor

