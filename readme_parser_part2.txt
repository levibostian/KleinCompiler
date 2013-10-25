*-------------------------------------------------------------*
*                  Klein - Parser -> AST                      *
*                                                             *
*                   cs4550: Fall 2013                         *
*                 Language: Scheme/Racket                     *    
*                     Team: RackAttack                        *
*                           Taylor Brown                      *
*                           Levi Bostian                      *
*                           Kyle Mueller                      *
* ------------------------------------------------------------*

/// Design of Solution:
1. 

/// Parser Execution:
***Must have racket language installed to execute parser.
   And path-ro-racket/bin/racket must also be located in $PATH variable.
$ parse-to-ast <path-to-klein-source>

If path-to-racket/bin/racket is not in your machine's $PATH 
environment variable, run the parser with:
$ <path-to-racket>/bin/racket parser.rkt <path-to-klein-source>

User may also load parser.rkt source code file into DrRacket IDE
,click "Run", then at prompt of DrRacket, enter:
> (parser <path-to-klein>)

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
readme_parser.txt
   readme file specifically for parser (created after part 1 phase)
readme_parser_part2.txt
   This file.
readme_scanner.txt
   readme file specifically for scanner and code at point of scanner 
   completion
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