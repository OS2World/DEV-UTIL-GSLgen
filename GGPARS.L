!----------------------------------------------------------------------------
!   Name:       ggpars.l
!   Title:      Dialog for gslgen schema parser
!   Package:    iMatix Studio 1.0
!
!   Written:    98/04/16  iMatix <tools@imatix.com>
!   Revised:    99/07/28
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

!- ggpars can be called in two ways: gg_parse_schema and gg_parse_expression

After-Init:
    (--) Schema                             -> Expecting-Text
          + Read-Schema-Line
          + Get-First-Token
    (--) Expression                         -> Expecting-First-Operand
          + Get-Expression-Token
    (--) End-Of-File                        ->
          + Signal-Unexpected-End-Of-File
          + Rollback-Memory-Allocations
          + Terminate-The-Program

!- Dialog state while parsing text for direct output

Expecting-Text:
    (--) Schema                             -> Expecting-Schema
          + Get-Schema-Command-Token
    (--) Literal                            ->
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Output-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Push-Text-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Extend                             ->
          + Insert-Spaces-Only-Node
          + Set-Extend-Flag
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) End-Of-Line                        ->
          + Insert-Spaces-Only-Node
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program

!- Expression dialog states.  Pops state off stack when done.

Expecting-First-Operand:
    (--) Quote                              -> Expecting-Quoted
          + Push-Quote-Character
          + Push-Operand-State
          + Insert-Literal-Node
          + Ready-For-Op1-Field
          + Count-Spaces-And-Token-Length
          + Get-Quoted-Token
    (--) Unary-Operator                     ->
          + Insert-Operator-Node
          + Count-Token-Length
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) Number                             -> Expecting-Operator
          + Insert-Literal-Node
          + Ready-For-Op1-Field
          + Insert-Text-Node
          + Count-Token-Length
          + Return-To-Parent
          + Get-Expression-Token
    (--) Literal                            -> Expecting-Identifier-Continue
          + Push-Literal-Operand-State
          + Push-Scope-State
          + Insert-Symbol-Node
          + Ready-For-Op1-Field
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Identifier-Token
    (--) Point                              -> Expecting-Operator
          + Insert-Symbol-Node
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) Member                             -> Expecting-Identifier
          + Push-Operand-State
          + Insert-Member-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-First-Identifier-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Push-Literal-Operand-State
          + Push-Scope-State
          + Insert-Operand-Node
          + Count-Token-Length
          + Ready-For-Op1-Field
          + Push-Identifier-Continue-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Open                               ->
          + Add-One-Bracket
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token
    (--) $other                             -> Returning
          + Rewind-To-Same-Token
          + Pop-Previous-State

!- Quoted string parsing state.  Pops state off stack when done.

Expecting-Quoted:
    (--) Literal                            ->
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Quoted-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Push-Quoted-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Quote                              -> Returning
          + Insert-Spaces-Only-Node
          + Count-Spaces-And-Token-Length
          + Pop-Previous-State
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Quoted-Token
    (--) End-Of-Line                        ->
          + Insert-Spaces-Only-Node
          + Set-Line-Break-Flag
          + Read-Schema-Line
          + Get-Quoted-Token

!- Identifier parsing states.  Pops state off stack when done.

Expecting-Identifier:
    (--) Literal                            -> Expecting-Identifier-Continue
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Identifier-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Push-Identifier-Continue-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token

Expecting-Identifier-Continue:
    (--) Literal                            ->
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Identifier-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Push-Identifier-Continue-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) $other                             -> Returning
          + Rewind-To-Same-Token
          + Pop-Previous-State

!- Identifier handling when scope may be specified.

After-Scope:
    (--) Point                              -> Expecting-Identifier-Continue
          + Change-Operand-To-Symbol-Node
          + Count-Spaces-And-Token-Length
          + Move-Op1-To-Op2
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Member                             -> Expecting-Identifier
          + Change-Symbol-To-Member-Node
          + Count-Spaces-And-Token-Length
          + Move-Op1-To-Op2
          + Ready-For-Op1-Field
          + Get-First-Identifier-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token
    (--) $other                             -> Returning
          + Ready-For-Null-Field
          + Rewind-To-Same-Token
          + Pop-Previous-State

!- Full identifier (., scope., scope.name, name) handling

Expecting-Scope-Identifier:
    (--) Literal                            -> Expecting-Identifier-Continue
          + Push-Scope-State
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Identifier-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Push-Scope-State
          + Push-Identifier-Continue-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Point                              -> Returning
          + Count-Spaces-And-Token-Length
          + Pop-Previous-State
    (--) Member                             -> Expecting-Identifier
          + Insert-Member-Node
          + Count-Spaces-And-Token-Length
          + Get-First-Identifier-Token
    (--) $other                             -> Returning
          + Rewind-To-Same-Token
          + Pop-Previous-State

!- Pick up function calls by '(' following operand

After-Literal-Operand:
    (--) Quote                              -> Expecting-Quoted
          + Push-Quote-Character
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Push-Operand-State
          + Insert-Literal-Node
          + Ready-For-Op1-Field
          + Count-Spaces-And-Token-Length
          + Get-Quoted-Token
    (--) Open                               -> Expecting-First-Operand
          + Change-To-Function-Node
          + Push-Function-State
          + Push-Arguments-State
          + Count-Spaces-And-Token-Length
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) Unary-Operator                     -> Expecting-Operand
          + Insert-Operator-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) Operator                           -> Expecting-Operand
          + Insert-Operator-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) Number                             -> Expecting-Operator
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Insert-Literal-Node
          + Ready-For-Op1-Field
          + Insert-Text-Node
          + Count-Token-Length
          + Return-To-Parent
          + Get-Expression-Token
    (--) Literal                            -> Expecting-Identifier-Continue
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Push-Literal-Operand-State
          + Push-Scope-State
          + Insert-Symbol-Node
          + Ready-For-Op1-Field
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Identifier-Token
    (--) Point                              -> Expecting-Operator
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Insert-Symbol-Node
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) Member                             -> Expecting-Identifier
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Push-Operand-State
          + Insert-Member-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-First-Identifier-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Push-Literal-Operand-State
          + Push-Scope-State
          + Insert-Operand-Node
          + Count-Token-Length
          + Ready-For-Op1-Field
          + Push-Identifier-Continue-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Close                              -> Expecting-Operator
          + Close-One-Bracket
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) No-Bracket                         -> Returning
          + Rewind-To-Same-Token
          + Pop-Previous-State
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token
    (--) $other                             -> Returning
          + Rewind-To-Same-Token
          + Pop-Previous-State

Expecting-Operand:
    (--) Quote                              -> Expecting-Quoted
          + Push-Quote-Character
          + Push-Operand-State
          + Insert-Literal-Node
          + Ready-For-Op1-Field
          + Count-Spaces-And-Token-Length
          + Get-Quoted-Token
    (--) Unary-Operator                     ->
          + Insert-Operator-Node
          + Count-Token-Length
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) Number                             -> Expecting-Operator
          + Insert-Literal-Node
          + Ready-For-Op1-Field
          + Insert-Text-Node
          + Count-Token-Length
          + Return-To-Parent
          + Get-Expression-Token
    (--) Literal                            -> Expecting-Identifier-Continue
          + Push-Literal-Operand-State
          + Push-Scope-State
          + Insert-Symbol-Node
          + Ready-For-Op1-Field
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Identifier-Token
    (--) Point                              -> Expecting-Operator
          + Insert-Symbol-Node
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) Member                             -> Expecting-Identifier
          + Push-Operand-State
          + Insert-Member-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-First-Identifier-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Push-Literal-Operand-State
          + Push-Scope-State
          + Insert-Operand-Node
          + Count-Token-Length
          + Ready-For-Op1-Field
          + Push-Identifier-Continue-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Open                               ->
          + Add-One-Bracket
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token

!- State to pick up arguments for functions and macro invocations

Expecting-Arguments:
    (--) Next-Arg                           -> Expecting-First-Operand
          + Ready-For-Next-Argument
          + Push-Arguments-State
          + Get-Expression-Token
    (--) Close                              -> Returning
          + Count-Spaces-And-Token-Length
          + Pop-Previous-State
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token

Expecting-Operator:
    (--) Quote                              -> Expecting-Quoted
          + Push-Quote-Character
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Push-Operand-State
          + Insert-Literal-Node
          + Ready-For-Op1-Field
          + Count-Spaces-And-Token-Length
          + Get-Quoted-Token
    (--) Unary-Operator                     -> Expecting-Operand
          + Insert-Operator-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) Operator                           -> Expecting-Operand
          + Insert-Operator-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) Number                             -> Expecting-Operator
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Insert-Literal-Node
          + Ready-For-Op1-Field
          + Insert-Text-Node
          + Count-Token-Length
          + Return-To-Parent
          + Get-Expression-Token
    (--) Literal                            -> Expecting-Identifier-Continue
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Push-Literal-Operand-State
          + Push-Scope-State
          + Insert-Symbol-Node
          + Ready-For-Op1-Field
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Identifier-Token
    (--) Point                              -> Expecting-Operator
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Insert-Symbol-Node
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) Member                             -> Expecting-Identifier
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Push-Operand-State
          + Insert-Member-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-First-Identifier-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Push-Literal-Operand-State
          + Push-Scope-State
          + Insert-Operand-Node
          + Count-Token-Length
          + Ready-For-Op1-Field
          + Push-Identifier-Continue-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Open                               -> Expecting-Operand
          + Insert-Undefined-Operator-Node
          + Ready-For-Op2-Field
          + Add-One-Bracket
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) Close                              -> Expecting-Operator
          + Close-One-Bracket
          + Count-Spaces-And-Token-Length
          + Get-Expression-Token
    (--) No-Bracket                         -> Returning
          + Rewind-To-Same-Token
          + Pop-Previous-State
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token
    (--) $other                             -> Returning
          + Rewind-To-Same-Token
          + Pop-Previous-State

!- Dialog to handle attributes and end of substitution token

Expecting-Substitute:
    (--) Attribute                          -> Expecting-Attribute
          + Count-Spaces-And-Token-Length
          + Ready-For-Attribute-Field
          + Get-Substitute-Token
    (--) Close                              -> Returning
          + Count-Spaces-And-Token-Length
          + Pop-Previous-State
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Substitute-Token

Expecting-Attribute:
    (--) Literal                            ->
          + Insert-Text-Node
          + Count-Token-Length
          + Get-Substitute-Token
    (--) Substitute                         -> Expecting-First-Operand
          + Confirm-Attribute-Specified
          + Push-Attribute-State
          + Push-Substitute-State
          + Insert-Substitute-Node
          + Count-Spaces-And-Token-Length
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Substitute-Token
    (--) $other                             -> Expecting-Substitute
          + Insert-Empty-Node-If-Needed
          + Return-To-Parent
          + Rewind-To-Same-Token
          + Get-Substitute-Token

!- Handle schema statements

Expecting-Schema:
    (--) Comment                            -> 
          + Insert-Comment-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Close                              ->
          + Insert-Close-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Else                               ->
          + Insert-Else-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) End-If                             ->
          + Insert-End-If-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) End-For                            ->
          + Insert-End-For-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) End-Macro                          ->
          + Insert-End-Macro-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) End-New                            ->
          + Insert-End-New-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) End-While                          ->
          + Insert-End-While-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Dump                               ->
          + Insert-Dump-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Output                             -> Expecting-First-Operand
          + Insert-Output-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Append                             -> Expecting-First-Operand
          + Insert-Append-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Include                            -> Expecting-First-Operand
          + Insert-Include-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Interpret                          -> Expecting-First-Operand
          + Insert-Interpret-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Delete                             -> Expecting-First-Operand
          + Insert-Delete-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Copy                               -> Expecting-First-Operand
          + Push-One-More-State
          + Insert-Copy-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Rename                             -> Expecting-First-Operand
          + Push-One-More-State
          + Insert-Rename-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Echo                               -> Expecting-First-Operand
          + Insert-Echo-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Abort                              -> Expecting-First-Operand
          + Insert-Abort-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Define                             -> Expecting-White-Space
          + Push-Define-State
          + Push-White-Space-State
          + Push-Scope-Identifier-State
          + Insert-Define-Node
          + Ready-For-Op1-Field
          + Get-White-Space-Token
    (--) Macro                              -> Expecting-White-Space
          + Push-Macro-State
          + Push-Identifier-State
          + Insert-Macro-Node
          + Ready-For-Op1-Field
          + Get-White-Space-Token
    (--) Invoke                             -> Expecting-White-Space
          + Push-Invoke-State
          + Push-Identifier-State
          + Insert-Invoke-Node
          + Ready-For-Op1-Field
          + Get-White-Space-Token
    (--) Xml                                -> Expecting-First-Operand
          + Push-Xml-State
          + Insert-Xml-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) If                                 -> Expecting-First-Operand
          + Insert-If-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) Elsif                              -> Expecting-First-Operand
          + Insert-Elsif-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token
    (--) New                                -> Expecting-White-Space
          + Push-New-State
          + Push-Scope-State
          + Push-Identifier-State
          + Insert-New-Node
          + Ready-For-Op1-Field
          + Get-White-Space-Token
    (--) For                                -> Expecting-White-Space
          + Push-For-State
          + Push-Scope-Identifier-State
          + Insert-For-Node
          + Ready-For-Op1-Field
          + Get-White-Space-Token
    (--) While                              -> Expecting-First-Operand
          + Insert-While-Node
          + Ready-For-Op1-Field
          + Get-Expression-Token

!- Collecting white spaces

Expecting-White-Space:
    (--) Spaces                             -> Continuing-White-Space
          + Get-White-Space-Token
    (--) Extend                             -> Continuing-White-Space
          + Read-Schema-Line
          + Get-White-Space-Token
    (--) End-Of-Line                        -> Returning
          + Pop-Previous-State

Continuing-White-Space:
    (--) Spaces                             ->
          + Get-White-Space-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-White-Space-Token
    (--) $other                             -> Returning
          + Pop-Previous-State

!- After identifier in .define statement

Expecting-Define-Value:
    (--) Equals                             -> Expecting-First-Operand
          + Ready-For-Op3-Field
          + Get-Expression-Token
    (--) End-Of-Line                        ->
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Spaces                             ->
          + Get-Schema-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Schema-Token
    (--) $other                             -> Expecting-Text
          + Ready-For-Op3-Field
          + Rewind-To-Same-Token
          + Get-Output-Token

After-Macro:
    (--) Open                               -> Expecting-Identifier
          + Push-Macro-Args-State
          + Ready-For-Op2-Field
          + Get-First-Identifier-Token
    (--) End-Of-Line                        ->
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token

Expecting-Macro-Args:
    (--) Next-Arg                           -> Expecting-Identifier
          + Push-Macro-Args-State
          + Insert-Operator-Node
          + Count-Token-Length
          + Ready-For-Op2-Field
          + Get-First-Identifier-Token
    (--) Close                              ->
          + Expect-End-Of-Line
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token

!- After identifier in .invoke statement

Expecting-Invoke-Args:
    (--) Open                               -> Expecting-First-Operand
          + Push-Invoke-Args-State
          + Push-Arguments-State
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) End-Of-Line                        ->
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Expression-Token

!- After file name in .xml statement

Expecting-Xml:
    (--) From                               -> Expecting-White-Space
          + Push-Identifier-State
          + Ready-For-Op2-Field
          + Get-White-Space-Token
    (--) End-Of-Line                        ->
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Spaces                             ->
          + Get-Schema-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Schema-Token

!- After identifier and scope in .new statement

Expecting-New:
    (--) As                                 -> Expecting-White-Space
          + Push-Identifier-State
          + Ready-For-Op3-Field
          + Get-White-Space-Token
    (--) End-Of-Line                        ->
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Spaces                             ->
          + Get-Schema-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Schema-Token

!- After identifier and scope in .for statement

Expecting-For:
    (--) As                                 -> Expecting-White-Space
          + Push-For-State
          + Push-Identifier-State
          + Ready-For-Op3-Field
          + Get-White-Space-Token
    (--) Where                              -> Expecting-First-Operand
          + Push-For-State
          + Ready-For-Pretty-Field
          + Get-Expression-Token
    (--) By                                 -> Expecting-First-Operand
          + Push-For-State
          + Ready-For-Format-Field
          + Get-Expression-Token
    (--) End-Of-Line                        ->
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Spaces                             ->
          + Get-Schema-Token
    (--) Extend                             ->
          + Read-Schema-Line
          + Get-Schema-Token

!- Dialog state to handling flow after popping state from stack.

Returning:
    (--) Text                               -> Expecting-Text
          + Get-Output-Token
    (--) Substitute                         -> Expecting-Substitute
          + Return-To-Expression-Parent
          + Get-Substitute-Token
    (--) Operand                            -> Expecting-Operator
          + Return-To-Parent
          + Get-Expression-Token
    (--) Literal-Operand                    -> After-Literal-Operand
          + Return-To-Parent
          + Get-Expression-Token
    (--) Scope                              -> After-Scope
          + Return-To-Parent
          + Get-Expression-Token
    (--) Identifier                         -> Expecting-Identifier
          + Get-First-Identifier-Token
    (--) Scope-Identifier                   -> Expecting-Scope-Identifier
          + Get-Identifier-Token
    (--) Identifier-Continue                -> Expecting-Identifier-Continue
          + Get-Identifier-Token
    (--) White-Space                        -> Expecting-White-Space
          + Get-White-Space-Token
    (--) Quoted                             -> Expecting-Quoted
          + Get-Quoted-Token
    (--) Function                           -> Expecting-Operator
          + Return-To-Arguments-Parent
          + Get-Expression-Token
    (--) Arguments                          -> Expecting-Arguments
          + Return-To-Expression-Parent
          + Get-Expression-Token
    (--) Attribute                          -> Expecting-Attribute
          + Get-Substitute-Token
    (--) One-More                           -> Expecting-First-Operand
          + Return-To-Expression-Parent
          + Ready-For-Op2-Field
          + Get-Expression-Token
    (--) Define                             -> Expecting-Define-Value
          + Return-To-Parent
          + Get-Schema-Token
    (--) Macro                              -> After-Macro
          + Return-To-Parent
          + Get-Expression-Token
    (--) Macro-Args                         -> Expecting-Macro-Args
          + Get-Expression-Token
    (--) Invoke                             -> Expecting-Invoke-Args
          + Return-To-Parent
          + Get-Expression-Token
    (--) Invoke-Args                        ->
          + Expect-End-Of-Line
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program
    (--) Xml                                -> Expecting-Xml
          + Return-To-Expression-Parent
          + Get-Schema-Token
    (--) New                                -> Expecting-New
          + Return-To-Parent
          + Get-Schema-Token
    (--) For                                -> Expecting-For
          + Return-To-Expression-Parent
          + Get-Schema-Token
    (--) Empty-Stack                        ->
          + Expect-End-Of-Line
          + Return-To-Root-Node
          + Commit-Memory-Allocations
          + Terminate-The-Program

Defaults:
    (--) Anomaly                            ->
          + Signal-Internal-Error
          + Rollback-Memory-Allocations
          + Terminate-The-Program
    (--) Error
          + Rollback-Memory-Allocations
          + Terminate-The-Program
    (--) End-Of-File                        ->
          + Signal-Unexpected-End-Of-File
          + Rollback-Memory-Allocations
          + Terminate-The-Program
    (--) Other                              ->
          + Signal-Invalid-Token
          + Rollback-Memory-Allocations
          + Terminate-The-Program
    (--) $other                             ->
          + Signal-Invalid-Token
          + Rollback-Memory-Allocations
          + Terminate-The-Program
