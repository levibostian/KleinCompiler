*-------------------------------------------------------------*
*                       Klein - Scanner                       *
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
2. 'file-reader' and 'file-reader-helper' opens language file, 
   reads sends each line of the file to line-reader where each
   line is read character by character.
3. When 'line-reader' encounters one of the follow characters:
   " " ) ( : , + - / * < =
   Then line-reader is designed to stop reading characters and 
   create tokens for the string accumulated before the stopping
   character was encountered, along with creating a token for 
   the stopping character itself. (comments are ignored and
   tokens are not created for them.) The tokens are created by
   checking string equality with specific keywords hard-coded into
   the scanner. If accumulated string or stopping character belongs
   to a certain family of other keywords, then a token is labeled 
   as that family encountered.
4. Each token that was generated in step 3 is added the accumulated 
   tokens list on each recursive call to line-reader.
5. Once the entire source file has been traversed, return 
   the list to the user. 

/// Scanner Execution:
***Must have racket language installed to execute scanner.
   And path-ro-racket/bin/racket must also be located in $PATH variable.
$ scanner <path-to-klein-source>

If path-to-racket/bin/racket is not in your machine's $PATH 
environment variable, run the scanner with:
$ <path-to-racket>/bin/racket scanner.rkt <path-to-klein-source>

User may also load scanner.rkt source code file into DrRacket IDE
,click "Run", then at prompt of DrRacket, enter:
> (scanner <path-to-klein>)

<path-to-racket> is path on your machine where racket language is
   installed. 
<path-to-klein-source> example: "/klein-programs/euclid.kln"

/// Files description:
klnexamples/ 
   contains example programs created by team in Klein
klein-programs:
   klein example programs pulled from Dr Wallingford
klein-unit-testing.rkt
   RackUnit unit tests created for scanner
scanner
   Bash shell script to execute scanner
scanner.rkt
   Klein scanner racket source file
readme_scanner.txt
   This file
scanner-output-klein.rkt
   expected output of example program files from Dr Wallingford
   to run against unit tests for scanner