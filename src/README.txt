*-------------------------------------------------------------*
*                  The Klein programming language             *
*                                                             *
*                   cs4550: Fall 2013                         *
*                 Language: Scheme/Racket                     *    
*                     Team: RackAttack                        *
*                           Taylor Brown                      *
*                           Levi Bostian                      *
*                           Kyle Mueller                      *
* ------------------------------------------------------------*

///Klein language features not implemented in compiler:
   none.
   
///List of known bugs:
   1. If Klein source file contains multiple main() functions, 
      the compiler compiles code successfully but only using 
      the last occurrence of main() found.

///Description of optimizations implemented:
   Tail Recursion is implemented!

///Notes:
   -There is a lot of TM generated that may be useless. 
   -We were able to run a lot of programs, including Circular-prime.kln
   -Didn't have to increase TM memory to run any of the kln programs
   -We decided to use Register 5 to keep track of the top of DM
   -Register 3 keeps track of bottom of current call frame, seemed easy
   -We didn't use 3-address code. Just generated it from AST
   -Templated out each expression, thus extra TM code
   Otherwise, not much changed from our design documented in the last 
   submission. We used these registers to get around with offsets. 
   The generated TM code is long but it does the job and was easy
   to understand. 

///Compiler Execution:
***Must have racket language installed to execute parser.
   And path-to-racket/bin/racket must also be located in $PATH variable.
$ ./kleinc <path-to-klein-source> <path-to-tm-output>
If no output is shown, compilation completed successfully. 
**If input or output file is not found, bash error will be produced, not Klein compiler
error.**

To run generated tm code:
$ ./tm <path-to-tm-source> <optional-main-arguments>

<path-to-racket/bin/racket> is path on your machine where racket language is
   installed. 
<path-to-klein-source> example: "euclid.kln"
<path-to-tm-output> example: "euclid.tm"
<path-to-tm-source> example: "euclid.tm"
<optional-main-arguments> command-line arguments sent to main() if required.

///Files description:
src/
   all compiler source files implemented in Racket.
   all bash scripts used to execute stages of compiler.
design/
   readme*.txt files from previous stages of compiler.
   pictures, spreadsheet, and txt files used during design
      process of compiler construction.
previous-submissions/
   *.tar.gz files turned in previous stages of compiler.
test-programs/
   class-downloaded/
      files provided by Dr Wallingford.
   class-written/
      files written by all teams in class.
   team-written/ 
      files written and used internally by team.
