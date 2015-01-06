!----------------------------------------------------------------------------
!   Name:       ggcode.l
!   Title:      Dialog for gslgen
!   Package:    iMatix Studio 1.0
!
!   Written:    97/12/01  iMatix <tools@imatix.com>
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
    (--) Ok                                 -> Have-Schema-Line
          + Initialise-Program-Data
          + Prepare-To-Generate-Code
          + Start-Initial-Schema
          + Get-Next-Schema-Line
    (--) Finished                           ->
          + Terminate-The-Program

Have-Schema-Line:
    (--) Comment                            -> Have-Schema-Line
          + Get-Next-Schema-Line
    (--) Define                             -> Have-Schema-Line
          + Store-Symbol-Definition
          + Get-Next-Schema-Line
    (--) Macro                              -> Have-Schema-Line
          + Store-Macro-Definition
          + Skip-Macro-Body
          + Get-Next-Schema-Line
    (--) Output                             -> Have-Schema-Line
          + Close-Output-File
          + New-Output-File
          + Get-Next-Schema-Line
    (--) Append                             -> Have-Schema-Line
          + Close-Output-File
          + Extend-Output-File
          + Get-Next-Schema-Line
    (--) Include                            -> Have-Schema-Line
          + Push-Schema-Position
          + Start-Included-Schema
          + Get-Next-Schema-Line
    (--) Interpret                          -> Have-Schema-Line
          + Push-Schema-Position
          + Start-Interpreted-Schema
          + Get-Next-Schema-Line
    (--) Close                              -> Have-Schema-Line
          + Close-Output-File
          + Get-Next-Schema-Line
    (--) For                                -> Have-Schema-Line
          + Initialise-For-Block
          + Get-Next-Schema-Line
    (--) End-For                            -> Have-Schema-Line
          + Iterate-For-Block
          + Get-Next-Schema-Line
    (--) While                              -> Have-Schema-Line
          + Initialise-While-Block
          + Get-Next-Schema-Line
    (--) End-While                          -> Have-Schema-Line
          + Iterate-While-Block
          + Get-Next-Schema-Line
    (--) If                                 -> Have-Schema-Line
          + Skip-If-Block-If-False
          + Get-Next-Schema-Line
    (--) Else                               -> Have-Schema-Line
          + Skip-Else-Block-Always
          + Get-Next-Schema-Line
    (--) Elsif                              -> Have-Schema-Line
          + Skip-Elsif-Block-Always
          + Get-Next-Schema-Line
    (--) End-If                             -> Have-Schema-Line
          + Close-If-Block
          + Get-Next-Schema-Line
    (--) Copy                               -> Have-Schema-Line
          + Copy-File-As-Specified
          + Get-Next-Schema-Line
    (--) Rename                             -> Have-Schema-Line
          + Rename-File-As-Specified
          + Get-Next-Schema-Line
    (--) Delete                             -> Have-Schema-Line
          + Delete-File-As-Specified
          + Get-Next-Schema-Line
    (--) Echo                               -> Have-Schema-Line
          + Echo-Text-To-Console
          + Get-Next-Schema-Line
    (--) Abort                              ->
          + Echo-Text-To-Console
          + Close-Schema
          + Close-Output-File
          + Destroy-Data-Structures
          + Terminate-The-Program
    (--) Dump                               -> Have-Schema-Line
          + Dump-Symbol-Table
          + Get-Next-Schema-Line
    (--) Text                               -> Have-Schema-Line
          + Copy-Line-To-Output
          + Get-Next-Schema-Line
    (--) Invoke                             -> Have-Schema-Line
          + Push-Schema-Position
          + Initialise-Macro-Block
          + Get-Next-Schema-Line
    (--) End-Macro                          -> Have-Schema-Line
          + Close-Macro-Block
          + Close-Schema
          + Pop-Schema-Position
          + Get-Next-Schema-Line
    (--) Xml                                -> Have-Schema-Line
          + Read-Xml-File
          + Get-Next-Schema-Line
    (--) New                                -> Have-Schema-Line
          + Initialise-New-Block
          + Get-Next-Schema-Line
    (--) End-New                            -> Have-Schema-Line
          + Close-New-Block
          + Get-Next-Schema-Line
    (--) End-Of-Schema                      -> Have-Schema-Line
          + Verify-All-Controls-Closed
          + Close-Schema
          + Pop-Schema-Position
          + Get-Next-Schema-Line
    (--) Finished                           ->
          + Close-Output-File
          + Destroy-Data-Structures
          + Terminate-The-Program

Defaults:
    (--) Anomaly                            ->
          + Close-Schema
          + Close-Output-File
          + Destroy-Data-Structures
          + Terminate-The-Program
