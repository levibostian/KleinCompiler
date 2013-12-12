*-------------------------------------------------------------*
*                       Klein - Parser                        *
*                                                             *
*                   cs4550: Fall 2013                         *
*                 Language: Scheme/Racket                     *    
*                     Team: RackAttack                        *
*                           Taylor Brown                      *
*                           Levi Bostian                      *
*                           Kyle Mueller                      *
* ------------------------------------------------------------*

/// Design of Solution:
1. Function 'scanner' takes path to source language file in 
   form of string.
2. Parser calls the previously written scanner program to receive
   the list of tokens to process from user's source language file.
3. (token-reader-helper) is initialized with:
   parser-accum: an accumulator used for the parser output functionality
   stack: parser stack (initialized with 'program and '$ starting 
          symbols)
   token-list: list of tokens produced from scanner.
4. (token-reader-helper) on each recursive call will check if
   top of stack is a terminal or not. If it is a terminal, 
   check if the current token in token-list is equal to the 
   terminal on stack. 
   If they are equal-- recurse with top of stack
                       popped and first token in token list consumed. 
   If they are not equal-- Fail. Print error to user with 
                           symbol that error was found on as well as 
                           column and row number found. 
   If the top of stack is a non-terminal:
   Do a look up in our parser hash table looking for a rule. 
   If rule exists-- recurse with top of stack popped and then push
                    the looked-up grammer rule onto stack. 
   If rule does not exist--- Fail. Print error to user with
                             symbol that error was found on as well as
                             column and row number found.
5. (token-reader-helper) will continue with recursive calls until all 
   tokens are consumed and stack is empty. Print the parser-accum parser
   output to user at this point with the idea that the parser succeeded. 
***Note on a design choice:
   Our team made an edit to the scanner token list output for the parser's 
   need. A 'dummy' token has been added to the end of the token list to
   indicate the end of the file has been reached.
   Example token: '(<end-of-file> '$ "1" "1")
   This was implemented into scanner to implementing language Scheme/Racket
   error in parser. When the token-list is empty but there are still items
   on the stack to pop off, the parser needs to be able to run more
   to pop off remaining items of stack. The parser would attempt to
   retrieve the next item of the token-list, (car token-list), on an
   empty token-list. After brainstorming, this implementation was decided.

/// Parser Execution:
***Must have racket language installed to execute parser.
   And path-ro-racket/bin/racket must also be located in $PATH variable.
$ parser <path-to-klein-source>

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
parser-output-klein.rkt
   expected output of example program files from Dr Wallingford
   to run against unit tests for parser
parser-unit-testing.rkt
   RackUnit unit tests created for parser
parse-table.rkt
   racket source file of hash parser table
pull.sh
   **(used internally) Bash shell script for pulling git version control 
                       easier
readme_parser.txt
   This file
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