!----------------------------------------------------------------------------
!   Name:       gslgen.l
!   Title:      Dialog for gslgen
!   Package:    iMatix Studio 1.0
!
!   Written:    98/07/03  iMatix <tools@imatix.com>
!   Revised:    98/12/04
!
!   Copyright (c) 1996-99 iMatix Corporation                                  
!                                                                             
!   This program is free software; you can redistribute it and/or modify      
!   it under the terms of the GNU General Public License as published by      
!   the Free Software Foundation; either version 2 of the License, or         
!   (at your option) any later version.                                       
!                                                                             
!   This program is distributed in the hope that it will be useful,           
!   but WITHOUT ANY WARRANTY; without even the implied warranty of            
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             
!   GNU General Public License for more details.                              
!                                                                             
!   You should have received a copy of the GNU General Public License         
!   along with this program; if not, write to the Free Software               
!   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                 
!----------------------------------------------------------------------------
-schema=lrschema.c

After-Init:
    (--) Ok                                 -> Have-First-Argument
          + Initialise-Program-Data
          + Get-Next-Argument-Value
    (--) Anomaly                            ->
          + Display-Welcome-If-Not-Quiet
          + Display-Command-Line-Syntax
          + Terminate-The-Program

Have-First-Argument:
    (--) Ok                                 -> Have-Next-Argument
          + Display-Welcome-If-Not-Quiet
          + Read-XML-File
          + Overlay-Standard-Onto-XML
          + Overlay-Switches-Onto-XML
          + Call-Code-Generator
          + Free-XML-Sructures
          + Get-Next-Argument-Value
    (--) Switch                             -> Have-First-Argument
          + Process-The-Switch
          + Get-Next-Argument-Value
    (--) Finished                           ->
          + Display-Welcome-If-Not-Quiet
          + Free-Switches-Structure
          + Terminate-The-Program

Have-Next-Argument:
    (--) Ok                                 -> Have-Next-Argument
          + Read-XML-File
          + Overlay-Standard-Onto-XML
          + Overlay-Switches-Onto-XML
          + Call-Code-Generator
          + Free-XML-Sructures
          + Get-Next-Argument-Value
    (--) Switch                             -> Have-Next-Argument
          + Process-The-Switch
          + Get-Next-Argument-Value
    (--) Finished                           ->
          + Free-Switches-Structure
          + Terminate-The-Program

Defaults:
    (--) Anomaly                            ->
          + Free-XML-Sructures
          + Free-Switches-Structure
          + Terminate-The-Program


