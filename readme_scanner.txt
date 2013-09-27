*-------------------------------------------------------------*
*                         Klein - Scanner                     *
*                                                             *
*                      cs4550: Fall 2013                      *
*                    Language: Scheme/Racket                  *    
*                        Team: RackAttack                     *
*                              Taylor Brown                   *
*                              Levi Bostian                   *
*                              Kyle Mueller                   *
* ------------------------------------------------------------*

/// Design of Solution:
1. Function 'scanner' takes path to source language file.
2. Opens language file, reads line by line/character by character.
3. When scanner encounters one of the follow characters:
   " ", ")", "(", ":", ","
   Then scanner is designed to stop reading characters and 
   create tokens for the string accumulated before the stopping
   character was encountered, along with creating a token for 
   the stopping character itself. 
4. Each token is added the list of other tokens generated.
5. Once the entire source file has been traversed, return 
   the list to the user. 

