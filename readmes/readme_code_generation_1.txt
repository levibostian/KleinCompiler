*-------------------------------------------------------------*
*                  Klein - Code Generation 1                  *
*                                                             *
*                   cs4550: Fall 2013                         *
*                 Language: Scheme/Racket                     *    
*                     Team: RackAttack                        *
*                           Taylor Brown                      *
*                           Levi Bostian                      *
*                           Kyle Mueller                      *
* ------------------------------------------------------------*
   
/// Design of Solution:
1. generate function receives annotated abstract syntax tree
   from symantic analysis phase along with symbol table. 
2. For each type found walking through nodes of AST, the appropriate
   TM code is generated in list format. 
3. Reason TM code is generated into a list instead of a string is
   to help in the process of finding the return addresses for 
   function calls. Symbol table is modified when return addresses
   are created before function call. After all TM code is generated,
   list of TM code is walked down and filled in with appropriate 
   integer return address values from the embedded functions in the list.

/// Parser Execution:
***Must have racket language installed to execute parser.
   And path-ro-racket/bin/racket must also be located in $PATH variable.
$ kleinc <path-to-klein-source>

User may also load run-time-generator.rkt source code file into DrRacket IDE
,click "Run", then at prompt of DrRacket, enter:
> (generate (semantic-actions (parser <path-to-klein-program>)))

<path-to-racket> is path on your machine where racket language is
   installed. 
<path-to-klein-source> example: "/klein-programs/euclid.kln"

/// Files description:
klnexamples/ 
   contains example programs created by team in Klein
klein-programs/
   klein example programs pulled from Dr Wallingford
non-code-docs/
   directory containing non-code related documents for compiler.

   deterministic_grammer.txt
      deterministic grammer of Klein grammer
   first-sets.txt
      First sets of Klein grammer
   first-sets_full.txt
      extended version of first sets of Klein grammer
   follow-sets.txt
      Follow sets of Klein grammer
   follow-sets_full.txt
      extended version of follow sets of Klein grammer
   parsing_table.ods
      spreadsheet form of parsing table of Klein
   code-generation-pictures/
      contains pictures of diagrams created demonstrating implementation 
      of code generation.

readmes/
   all readme documents for submissions.
submissions/
   directory containing *.tar.gz compressed files submitted throughout term.
data-type.rkt
   containing lists of defined data types and helper functions
parser
   Bash shell script to execute parser
parser.rkt
   Klein parser racket source file
parser-extra.rkt
   Helper functions for working with stack, errors, and tokens for parser. 
parser-output-klein.rkt
   expected output of example program files from Dr Wallingford
   to run against unit tests for parser
parser-unit-testing.rkt
   RackUnit unit tests created for parser
parse-table.rkt
   racket source file of hash parser table
parse-to-ast
   Bash shell script to execute parser to produce AST
print-parser.rkt
   Racket source code used to print output AST struct from parser. 
pull.sh
   **(used internally) Bash shell script for pulling git version control 
                       easier
readme_code_generation_1.txt
   this file
readme_parser.txt
   readme file specifically for parser (created after part 1 phase)
readme_parser_part2.txt
   readme file specifically for parser submission 2. 
readme_scanner.txt
   readme file specifically for scanner and code at point of scanner 
   completion
run-racket.rkt
    Bash file not currently being used. Keeping for future inspiration. 
scanner
   Bash shell script to execute scanner
scanner.rkt
   Klein scanner racket source file
scanner-helper-functions.rkt
   racket helper functions for scanner
scanner-output-klein.rkt
   expected output of example program files from Dr Wallingford
   to run against unit tests for scanner
scanner-unit-testing.rkt
   RackUnit unit tests created for scanner
semantic-actions.rkt
   Contains Racket structs used to represent semantic actions. 
symbol-table.rkt
   Racket source file responsible for creating hash symbol. 
symbol-table-helpers.rkt
   Helper functions dealing with symbol table implementation. 
type-checker.rkt
   Racket source file responsible for type checking. 