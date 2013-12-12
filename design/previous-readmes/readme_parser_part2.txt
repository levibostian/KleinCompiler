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
1. The token-reader-helper (which is the parser, essentially) has been
   updated to include the Semantic Action portion of the algorithm.
2. How we decided to handle semantic actions:
     -We decided to use structs to represent a program
     -Structs were made for ever part of the language
   How it works: 
     The Parser runs through the grammar like it normally did. Except now
     the grammar includes semantic-actions, which are functions. If/when a
     semantic action is found, we apply that function to the semantic-stack.
     There are functions for each part of the language and they do the 
     pushing/popping that are required. 
     An example here is: 
       If we have a Def, we know it's made up of 4 things. So, the struct
       for Def has four parameters. Ident/Formals/Type/Body. When a 
       Def semantic-action is executed (the parser doesn't know, or care 
       what action is found, it just applies it to the semantic-stack)
       it pops 4 things off the top of the semantic stack. It then takes 
       these 4 things and puts them all inside a Def struct. 
     This happens for everything in the language. Our program is
     represented as structs within structs. 
3. How an arbitrary amount of Defs/Formals/Actuals/etc... are handled:
     We realized early on that this might be a problem. But, our solution
     was pretty simple. Basically, for something like Formals, we 
     have a nonemptyformals struct that contains a Formal followed 
     by a nonemptyformals. When it comes to things in the language having
     an arbitrary length we just nest the structs in the expression.
     Example:
       Definitions is Def/Definitions'
       Struct looks kind of like:
         Definitions(Def, Definitions-Prime(Def, Definitions-Prime(...)))
4. This implementation has left the actual parser algorithm fairly clean.
   There's a lot of parameters being thrown around, and that will be fixed,
   but not for this turn-in. 
5. Printing the output:
     We made print/--- functions for each part of the language. The file
     print-parser.rkt is going to be difficult to read, probably. 
     Essentially what it's doing is breaking each struct down in to it's
     parts, and calling print/--- on that part. We initially followed the
     grammar for this, and realized it doesn't matter. We just need it to 
     print, we don't care if an expr can print a division or an if statement.
     So, we made a catch-all that does just that. It catches everything any kind
     of expression can be, and prints it. This simplified a lot of the code. 
     Then we had to add spaces. We decided to make this an argument, and pass it 
     along to each print/--- call. We incremented by 4 where we needed to. 
     In the end, this works. It could be cleaned up, and made a lot simpler
     but for now it works. 
 

/// Parser Execution:

***NOTE: Use print-parser.rkt file for displaying the AST.
parser.rkt produces the AST struct for you with:
(parser <path-to-klein-program>)

While print-parser.rkt displays the AST struct to you with:
(printf (print/program (parser <path-to-klein-program>)))

***Must have racket language installed to execute parser.
   And path-ro-racket/bin/racket must also be located in $PATH variable.
$ parse-to-ast <path-to-klein-source>

User may also load print-parser.rkt source code file into DrRacket IDE
,click "Run", then at prompt of DrRacket, enter:
> (printf (print/program (parser <path-to-klein-program>)))

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
