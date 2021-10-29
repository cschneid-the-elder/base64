       ID Division.
      * 
      * Copyright (C) 2021 Craig Schneiderwent.  All rights reserved.
      * 
      * I accept no liability for damages of any kind resulting 
      * from the use of this software.  Use at your own risk.
      *
      * This software may be modified and distributed under the terms
      * of the MIT license. See the LICENSE file for details.
      *
      * Base64 encoding in the COBOL language
      * 
      * The nested program does the encoding, this parent program
      * provides the data to be encoded.  The separation of duties
      * allows the nested program to be coded in a manner which is
      * likely to be more portable between compilers.
      *
      * This parent program accepts one of two command-line
      * parameters:
      * test - initiates tests using strings listed on the Base64  
      *        encoding page
      *        https://en.wikipedia.org/wiki/Base64#Output_padding
      * file - initiates encoding of a file named favicon.ico
      *        located in the current directory, which is presumed
      *        to contain the result of retrieving data from
      *        http://rosettacode.org/favicon.ico
      *
      * Be advised that output from this parent program includes a
      * trailing x'0a' line feed.
      *
       Program-ID. b64demo1.
       Environment Division.
       Input-Output Section.
       File-Control.
           Select INPUT01 Assign To './favicon.ico'
             Organization Record Binary Sequential.
       Data Division.
       File Section.
       FD  INPUT01.
       01  INPUT01-REC PIC X(3638).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'b64demo1'.

       77  PROCESS-TYPE           PIC X(004)         VALUE SPACES.
       77  PROCESS-SW             PIC X(004)         VALUE SPACES.
           88  PROCESS-FILE                          VALUE 'FILE'.
           88  PROCESS-TEST                          VALUE 'TEST'.
       77  IN-BUFFER-LEN          PIC 9(008)   COMP  VALUE 0.
       77  IN-BUFFER              PIC X(32768)       VALUE LOW-VALUES.
       77  OUT-BUFFER-LEN         PIC 9(008)   COMP  VALUE 0.
       77  OUT-BUFFER             PIC X(65536)       VALUE LOW-VALUES.

       Procedure Division.

           ACCEPT PROCESS-TYPE FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(PROCESS-TYPE)
             TO PROCESS-SW

           EVALUATE TRUE
              WHEN PROCESS-FILE PERFORM 1000-PROCESS-FAVICON
              WHEN PROCESS-TEST PERFORM 2000-PROCESS-WIKIPEDIA-TESTS
              WHEN OTHER
                   DISPLAY MYNAME
                           ' requires a command line argument'
                           ' of FILE or TEST'
           END-EVALUATE

           GOBACK
           .

       1000-PROCESS-FAVICON.
           OPEN INPUT INPUT01

           READ INPUT01 INTO IN-BUFFER

           CLOSE INPUT01

           MOVE LENGTH OF INPUT01-REC TO IN-BUFFER-LEN

           CALL 'B64ENCOD' USING
               IN-BUFFER-LEN
               IN-BUFFER
               OUT-BUFFER-LEN
               OUT-BUFFER
           END-CALL

           DISPLAY OUT-BUFFER(1:OUT-BUFFER-LEN)
           .

       2000-PROCESS-WIKIPEDIA-TESTS.
           MOVE 1 TO IN-BUFFER-LEN
           STRING 'light work.' INTO IN-BUFFER POINTER IN-BUFFER-LEN
           SUBTRACT 1 FROM IN-BUFFER-LEN

           CALL 'B64ENCOD' USING
               IN-BUFFER-LEN
               IN-BUFFER
               OUT-BUFFER-LEN
               OUT-BUFFER
           END-CALL

           DISPLAY MYNAME ' IN-BUFFER-LEN = ' IN-BUFFER-LEN
           DISPLAY MYNAME ' IN-BUFFER = ' IN-BUFFER(1:IN-BUFFER-LEN)
           DISPLAY MYNAME ' OUT-BUFFER-LEN = ' OUT-BUFFER-LEN
           DISPLAY MYNAME ' OUT-BUFFER = ' OUT-BUFFER(1:OUT-BUFFER-LEN)

           MOVE 1 TO IN-BUFFER-LEN
           STRING 'light work' INTO IN-BUFFER POINTER IN-BUFFER-LEN
           SUBTRACT 1 FROM IN-BUFFER-LEN

           CALL 'B64ENCOD' USING
               IN-BUFFER-LEN
               IN-BUFFER
               OUT-BUFFER-LEN
               OUT-BUFFER
           END-CALL

           DISPLAY MYNAME ' IN-BUFFER-LEN = ' IN-BUFFER-LEN
           DISPLAY MYNAME ' IN-BUFFER = ' IN-BUFFER(1:IN-BUFFER-LEN)
           DISPLAY MYNAME ' OUT-BUFFER-LEN = ' OUT-BUFFER-LEN
           DISPLAY MYNAME ' OUT-BUFFER = ' OUT-BUFFER(1:OUT-BUFFER-LEN)

           MOVE 1 TO IN-BUFFER-LEN
           STRING 'light wor' INTO IN-BUFFER POINTER IN-BUFFER-LEN
           SUBTRACT 1 FROM IN-BUFFER-LEN

           CALL 'B64ENCOD' USING
               IN-BUFFER-LEN
               IN-BUFFER
               OUT-BUFFER-LEN
               OUT-BUFFER
           END-CALL

           DISPLAY MYNAME ' IN-BUFFER-LEN = ' IN-BUFFER-LEN
           DISPLAY MYNAME ' IN-BUFFER = ' IN-BUFFER(1:IN-BUFFER-LEN)
           DISPLAY MYNAME ' OUT-BUFFER-LEN = ' OUT-BUFFER-LEN
           DISPLAY MYNAME ' OUT-BUFFER = ' OUT-BUFFER(1:OUT-BUFFER-LEN)

           MOVE 1 TO IN-BUFFER-LEN
           STRING 'light wo' INTO IN-BUFFER POINTER IN-BUFFER-LEN
           SUBTRACT 1 FROM IN-BUFFER-LEN

           CALL 'B64ENCOD' USING
               IN-BUFFER-LEN
               IN-BUFFER
               OUT-BUFFER-LEN
               OUT-BUFFER
           END-CALL

           DISPLAY MYNAME ' IN-BUFFER-LEN = ' IN-BUFFER-LEN
           DISPLAY MYNAME ' IN-BUFFER = ' IN-BUFFER(1:IN-BUFFER-LEN)
           DISPLAY MYNAME ' OUT-BUFFER-LEN = ' OUT-BUFFER-LEN
           DISPLAY MYNAME ' OUT-BUFFER = ' OUT-BUFFER(1:OUT-BUFFER-LEN)

           MOVE 1 TO IN-BUFFER-LEN
           STRING 'light w' INTO IN-BUFFER POINTER IN-BUFFER-LEN
           SUBTRACT 1 FROM IN-BUFFER-LEN

           CALL 'B64ENCOD' USING
               IN-BUFFER-LEN
               IN-BUFFER
               OUT-BUFFER-LEN
               OUT-BUFFER
           END-CALL

           DISPLAY MYNAME ' IN-BUFFER-LEN = ' IN-BUFFER-LEN
           DISPLAY MYNAME ' IN-BUFFER = ' IN-BUFFER(1:IN-BUFFER-LEN)
           DISPLAY MYNAME ' OUT-BUFFER-LEN = ' OUT-BUFFER-LEN
           DISPLAY MYNAME ' OUT-BUFFER = ' OUT-BUFFER(1:OUT-BUFFER-LEN)
           .

       ID Division.
      * 
      * Copyright (C) 2021 Craig Schneiderwent.  All rights reserved.
      * 
      * I accept no liability for damages of any kind resulting 
      * from the use of this software.  Use at your own risk.
      *
      * This software may be modified and distributed under the terms
      * of the MIT license. See the LICENSE file for details.
      *
      * Base64 encode data passed via CALL parameters in the 
      * Linkage Section.
      *
      * This program presumes big-endian encoding for COMP
      * data items, that PIC 9(008) COMP is four bytes long,
      * and that a byte is eight bits, but otherwise should
      * be compatible with any current COBOL compiler.
      *
      * The input buffer to be encoded is limited to 32K, but
      * this is arbitrary and can be increased to the limits
      * of your compiler.
      *
      * The output buffer containing the encoded data is limited
      * to 64K, but this is arbitrary and can be increased 
      * subject only to the limits of your compiler.
      *
       Program-ID. B64ENCOD Is Initial.
       Environment Division.
       Input-Output Section.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'B64ENCOD'.
           05  SIX-BIT-CHARS.
               10  PIC X(032) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef'.
               10  PIC X(032) VALUE 'ghijklmnopqrstuvwxyz0123456789+/'.

       01  WORK-AREAS.
           05  IN-BUFFER-PTR      PIC 9(008)  COMP VALUE 1.
           05  OUT-BUFFER-PTR     PIC 9(008)  COMP VALUE 1.
           05  FOUR-BYTE-INT      PIC 9(008)  COMP VALUE ZERO.
           05  FOUR-BYTE-INT-BYTES1
               REDEFINES FOUR-BYTE-INT.
               10                     PIC X(003).
               10  FOUR-BYTE-INT-LOW1 PIC X(001).
           05  FOUR-BYTE-INT-BYTES2
               REDEFINES FOUR-BYTE-INT.
               10                     PIC X(001).
               10  FOUR-BYTE-INT-LOW3 PIC X(003).
           05  SIX-BIT-SUB            PIC 9(008) COMP VALUE ZERO.
           05  SIX-BIT-SUB-X
               REDEFINES SIX-BIT-SUB.
               10                     PIC X(003).
               10  SIX-BIT-SUB-LOW1     PIC X(001).
           05  OUT-BLOCK-SUB          PIC 9(004) COMP VALUE 5.
           05  OUT-BLOCK              PIC X(004)      VALUE SPACES.
           05  NB-GROUPS-OF-THREE     PIC 9(004) COMP.
           05  NB-GROUPS-OF-THREE-R   PIC 9(004) COMP.
           05  NB-BYTES               PIC 9(004) COMP.
           05  OUT-BLOCK-START        PIC 9(004) COMP.

       Linkage Section.
       77  IN-BUFFER-LEN       PIC 9(008)  COMP.
       77  IN-BUFFER           PIC X(32768).
       77  OUT-BUFFER-LEN      PIC 9(008)  COMP.
       77  OUT-BUFFER          PIC X(65536).

       Procedure Division Using
           IN-BUFFER-LEN
           IN-BUFFER
           OUT-BUFFER-LEN
           OUT-BUFFER
           .

           INITIALIZE OUT-BUFFER-LEN
           PERFORM 1000-CONVERT

           GOBACK.

       1000-CONVERT.
           DIVIDE IN-BUFFER-LEN BY 3
             GIVING NB-GROUPS-OF-THREE
             REMAINDER NB-GROUPS-OF-THREE-R

           PERFORM 1100-CONVERT-GROUPS-OF-THREE
             NB-GROUPS-OF-THREE TIMES

           EVALUATE NB-GROUPS-OF-THREE-R
             WHEN 2
                  PERFORM 1200-CONVERT-JUST-TWO
             WHEN 1
                  PERFORM 1300-CONVERT-JUST-ONE
           END-EVALUATE
           .

       1100-CONVERT-GROUPS-OF-THREE.
           INITIALIZE FOUR-BYTE-INT
           MOVE IN-BUFFER(IN-BUFFER-PTR:3) TO FOUR-BYTE-INT-LOW3

      *    Expect 4 output bytes for 3 input bytes
           MOVE 4 TO NB-BYTES
           PERFORM 2000-CONVERT-ONE-BLOCK
           ADD 3 TO IN-BUFFER-PTR
           .

       1200-CONVERT-JUST-TWO.
           INITIALIZE FOUR-BYTE-INT
           MOVE IN-BUFFER(IN-BUFFER-PTR:2)
             TO FOUR-BYTE-INT-LOW3(2:2)
      *    Shift Left Logical 2 bits because we only have
      *    two bytes to convert, two bytes = 16 bits, but
      *    we need 18 bits to comprise our 3 output bytes
           MULTIPLY 4 BY FOUR-BYTE-INT

      *    Expect 3 output bytes for 2 input bytes
           MOVE 3 TO NB-BYTES
           PERFORM 2000-CONVERT-ONE-BLOCK

      *    Pad with '=' because some implementations require it
           STRING '='
             INTO OUT-BUFFER
             POINTER OUT-BUFFER-PTR
           END-STRING

      *    Account for the padding
           ADD 1 TO OUT-BUFFER-LEN
           .

       1300-CONVERT-JUST-ONE.
           INITIALIZE FOUR-BYTE-INT
           MOVE IN-BUFFER(IN-BUFFER-PTR:1)
             TO FOUR-BYTE-INT-LOW3(3:1)
      *    Shift Left Logical 4 bits because we only have
      *    one byte to convert, one byte = 8 bits, but
      *    we need 12 bits to comprise our 2 output bytes
           MULTIPLY 16 BY FOUR-BYTE-INT

      *    Expect 2 output bytes for 1 input byte
           MOVE 2 TO NB-BYTES
           PERFORM 2000-CONVERT-ONE-BLOCK

      *    Pad with '==' because some implementations require it
           STRING '=='
             INTO OUT-BUFFER
             POINTER OUT-BUFFER-PTR
           END-STRING

      *    Account for the padding
           ADD 2 TO OUT-BUFFER-LEN
           .

       2000-CONVERT-ONE-BLOCK.
           MOVE 5 TO OUT-BLOCK-SUB
           MOVE SPACES TO OUT-BLOCK
           PERFORM 2010-CONVERT-ONE-BYTE NB-BYTES TIMES
           COMPUTE OUT-BLOCK-START = 4 - NB-BYTES + 1
           STRING OUT-BLOCK(OUT-BLOCK-START:NB-BYTES)
             INTO OUT-BUFFER
             POINTER OUT-BUFFER-PTR
           END-STRING
           ADD NB-BYTES TO OUT-BUFFER-LEN
           .

       2010-CONVERT-ONE-BYTE.
      *    Shift Left Logical 2 bits
           MULTIPLY 4 BY FOUR-BYTE-INT

      *    Initialize target storage subscript
           INITIALIZE SIX-BIT-SUB

      *    Copy bit-shifted byte to target storage
           MOVE FOUR-BYTE-INT-LOW1 TO SIX-BIT-SUB-LOW1

      *    Shift Right Logical 2 bits
           COMPUTE SIX-BIT-SUB = SIX-BIT-SUB / 4

      *    COBOL is 1-based not 0-based
           ADD 1 TO SIX-BIT-SUB

      *    Filling OUT-BLOCK from right to left
           SUBTRACT 1 FROM OUT-BLOCK-SUB

      *    Copy converted byte to OUT-BLOCK
           MOVE SIX-BIT-CHARS(SIX-BIT-SUB:1)
             TO OUT-BLOCK(OUT-BLOCK-SUB:1)

      *    Initialize storage formerly occupied by converted byte
           MOVE LOW-VALUES TO FOUR-BYTE-INT-LOW1

      *    Shift Right Logical 8 bits, setup for next iteration
           COMPUTE FOUR-BYTE-INT = FOUR-BYTE-INT / 256
           .

       END PROGRAM B64ENCOD.
       END PROGRAM b64demo1.

