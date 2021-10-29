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
      * Base64 decoding in the COBOL language
      * 
      * The nested program does the decoding, this parent program
      * provides the data to be decoded.  The separation of duties
      * allows the nested program to be coded in a manner which is
      * likely to be more portable between compilers.
      *
      * Be advised that output from this parent program includes a
      * trailing x'0a' line feed.
      *
       Program-ID. b64demo2.
       Environment Division.
       Input-Output Section.
       File-Control.
           Select INPUT01 Assign To Keyboard.
       Data Division.
       File Section.
       FD  INPUT01.
       01  INPUT01-REC PIC X(49152).

       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'b64demo2'.

       77  WS-INPUT01-REC         PIC X(49152)       VALUE SPACES.
       77  INPUT01-EOF-SW         PIC X(004)         VALUE 'N'.
           88  INPUT01-EOF                           VALUE 'Y'.
       77  IN-BUFFER-LEN          PIC 9(008)   COMP  VALUE 1.
       77  IN-BUFFER              PIC X(49152)       VALUE LOW-VALUES.
       77  OUT-BUFFER-LEN         PIC 9(008)   COMP  VALUE 0.
       77  OUT-BUFFER             PIC X(32768)       VALUE LOW-VALUES.

       Procedure Division.

           PERFORM 1000-PROCESS-STDIN

           GOBACK
           .

       1000-PROCESS-STDIN.
           PERFORM 1010-FILL-IN-BUFFER

           CALL 'B64DECOD' USING
               IN-BUFFER-LEN
               IN-BUFFER
               OUT-BUFFER-LEN
               OUT-BUFFER
           END-CALL

           IF RETURN-CODE = 0
               DISPLAY OUT-BUFFER(1:OUT-BUFFER-LEN)
           ELSE
               DISPLAY 'Error in conversion'
           END-IF
           .

       1010-FILL-IN-BUFFER.
           OPEN INPUT INPUT01

           PERFORM 8010-READ-STDIN

           STRING WS-INPUT01-REC DELIMITED SPACE
             INTO IN-BUFFER
             POINTER IN-BUFFER-LEN
             OVERFLOW PERFORM 9010-ABORT
           END-STRING

      *    IN-BUFFER-LEN points to the next byte to be
      *    used in the STRING statement.  Subracting 1
      *    makes it contain the correct length.
           SUBTRACT 1 FROM IN-BUFFER-LEN

           CLOSE INPUT01
           .

       8010-READ-STDIN.
           READ INPUT01 INTO WS-INPUT01-REC
             AT END SET INPUT01-EOF TO TRUE
           END-READ
           .

       9010-ABORT.
           MOVE 12 TO RETURN-CODE
           GOBACK
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
      * data items, that PIC 9(015) COMP is eight bytes long,
      * and that a byte is eight bits, but otherwise should
      * be compatible with any current COBOL compiler.
      *
      * The input buffer to be encoded is limited to 48K, but
      * this is arbitrary and can be increased to the limits
      * of your compiler.
      *
      * The output buffer containing the encoded data is limited
      * to 32K, but this is arbitrary and can be increased to
      * accomodate an increased input buffer size subject only
      * to the limits of your compiler.
      *
      * While the input must be valid base64 encoded data, it need
      * not be padded with '=' characters so its length is a
      * multiple of 4.
      *
       Program-ID. B64DECOD Is Initial.
       Environment Division.
       Input-Output Section.
       Data Division.
       Working-Storage Section.
       01  CONSTANTS.
           05  MYNAME             PIC X(008) VALUE 'B64DECOD'.
           05  SIX-BIT-CHARS-VAL.
               10  PIC X(032) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef'.
               10  PIC X(032) VALUE 'ghijklmnopqrstuvwxyz0123456789+/'.
           05  SIX-BIT-CHARS-TBL
               REDEFINES SIX-BIT-CHARS-VAL.
               10  SIX-BIT-CHAR
                   OCCURS 64
                   PIC X(001).

       01  WORK-AREAS.
           05  IN-BUFFER-PTR      PIC 9(008)  COMP VALUE 1.
           05  OUT-BUFFER-PTR     PIC 9(008)  COMP VALUE 1.
           05  IN-BLOCK-SIZE      PIC 9(008)  COMP VALUE 0.
           05  GROUP-OF-FOUR-SUB  PIC 9(008)  COMP VALUE 0.
           05  GROUP-OF-FOUR-TBL.
               10  GROUP-OF-FOUR-CHAR
                   OCCURS 4
                   PIC X(001).
           05  EIGHT-BYTE-INT      PIC 9(015)  COMP VALUE ZERO.
           05  EIGHT-BYTE-INT-BYTES
               REDEFINES EIGHT-BYTE-INT.
               10  EIGHT-BYTE-INT-CHAR
                   OCCURS 8
                   PIC X(001).
           05  SIX-BIT-SUB            PIC 9(008) COMP VALUE ZERO.
           05  SIX-BIT-SUB-X
               REDEFINES SIX-BIT-SUB.
               10                     PIC X(003).
               10  SIX-BIT-SUB-LOW1   PIC X(001).
           05  OUT-BLOCK-SIZE         PIC 9(004) COMP VALUE 0.
           05  OUT-BLOCK              PIC X(004)      VALUE SPACES.
           05  NB-GROUPS-OF-FOUR      PIC 9(004) COMP.
           05  NB-GROUPS-OF-FOUR-R    PIC 9(004) COMP.

       01  SWITCHES.
           05  SIX-BIT-CHAR-SW        PIC X(001)      VALUE 'N'.
               88  SIX-BIT-CHAR-FOUND                 VALUE 'Y'.

       Linkage Section.
      *
      * The value of IN-BUFFER-LEN is presumed to be the length
      * of the base64 encoded data in IN-BUFFER.
      *
       77  IN-BUFFER-LEN       PIC 9(008)  COMP.
      *
      * The contents of IN-BUFFER are presumed to be valid
      * base64 encoded data.
      *
       77  IN-BUFFER           PIC X(49152).
       77  OUT-BUFFER-LEN      PIC 9(008)  COMP.
       77  OUT-BUFFER          PIC X(32768).

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
           DIVIDE IN-BUFFER-LEN BY 4
             GIVING NB-GROUPS-OF-FOUR
             REMAINDER NB-GROUPS-OF-FOUR-R

           MOVE 4 TO IN-BLOCK-SIZE

           PERFORM 1100-CONVERT-ONE-BLOCK
             NB-GROUPS-OF-FOUR TIMES

           EVALUATE NB-GROUPS-OF-FOUR-R
             WHEN 0 
                    MOVE 0 TO RETURN-CODE
             WHEN 1
      *             Invalid base64 encoded data
                    MOVE 12 TO RETURN-CODE
             WHEN OTHER
                    MOVE NB-GROUPS-OF-FOUR-R TO IN-BLOCK-SIZE
                    PERFORM 1100-CONVERT-ONE-BLOCK
                    MOVE 0 TO RETURN-CODE
           END-EVALUATE

           GOBACK
           .

      * 
      * COBOL has no bit shift operator, and the algorithm
      * requires that high order bits be squished out of bytes.
      * This is accomplished via multiplication.
      * 
      * Presume we have, in ASCII, the characters 'Man' which have
      * been base64 encoded as 'TWFu' (borrowing from Wikipedia).
      * 
      * Character T        W        F        u
      * Sextet    19       22       5        46
      * Bits      00010011 00010110 00000101 00101110
      * 
      * The sextet value is the 0 based index of the character in the
      * SIX-BIT-CHAR table.
      * 
      * This paragraph will put b'00101110' in the low order byte
      * of EIGHT-BYTE-INT (a 64 bit big endian integer).
      * 
      * EIGHT-BYTE-INT now looks like this...
      *0000000000000000000000000000000000000000000000000000000000101110
      *1       2       3       4       5       6       7       8       
      *
      * Multiply by 4 and the high order two bits are squished out.
      *
      * EIGHT-BYTE-INT now looks like this...
      *0000000000000000000000000000000000000000000000000000000010111000
      *1       2       3       4       5       6       7       8       
      *
      * Now put b'00000101' in the byte to the left of the low
      * order byte.  
      * 
      * EIGHT-BYTE-INT now looks like this...
      *0000000000000000000000000000000000000000000000000000010110111000
      *1       2       3       4       5       6       7       8       
      *
      * Multiply by 4, and
      * 
      * EIGHT-BYTE-INT now looks like this...
      *0000000000000000000000000000000000000000000000000001011011100000
      *1       2       3       4       5       6       7       8       
      *
      * Now put b'00010110' into the third byte from the right.
      * 
      * EIGHT-BYTE-INT now looks like this...
      *0000000000000000000000000000000000000000000101100001011011100000
      *1       2       3       4       5       6       7       8       
      *
      * Multiply by 4, and
      * 
      * EIGHT-BYTE-INT now looks like this...
      *0000000000000000000000000000000000000000010110000101101110000000
      *1       2       3       4       5       6       7       8       
      * 
      * Now put b'00010011' into the fourth byte from the right.
      * 
      * EIGHT-BYTE-INT now looks like this...
      *0000000000000000000000000000000000010011010110000101101110000000
      *1       2       3       4       5       6       7       8       
      * 
      * Multiply by 4, and
      * 
      * EIGHT-BYTE-INT now looks like this...
      *0000000000000000000000000000000001001101011000010110111000000000
      *1       2       3       4       5       6       7       8       
      * 
      * 
      * Bytes 5, 6, and 7 have now each had their high order two bits
      * squished out.  Also important is that the 4 groups of 6 bits
      * are all adjacent, leaving b'010011010110000101101110', or...
      * 
      * 01001101 01100001 01101110
      * 
      * Bits      01001101 01100001 01101110
      * Decimal   77       97       110
      * Character M        a        n
      * 
      * 
       1100-CONVERT-ONE-BLOCK.
           INITIALIZE
             EIGHT-BYTE-INT
             OUT-BLOCK-SIZE

      *    GNU COBOL (OpenCOBOL) 1.1.0 gives a truncation
      *    warning for this statement.  The reference modification
      *    makes it okay.
           MOVE IN-BUFFER(IN-BUFFER-PTR:IN-BLOCK-SIZE)
             TO GROUP-OF-FOUR-TBL

           PERFORM VARYING GROUP-OF-FOUR-SUB FROM IN-BLOCK-SIZE BY -1
           UNTIL GROUP-OF-FOUR-SUB = 0
           OR IN-BUFFER-PTR > IN-BUFFER-LEN
             IF GROUP-OF-FOUR-CHAR(GROUP-OF-FOUR-SUB) = '='
                 CONTINUE
             ELSE
                 PERFORM 1110-FIND-SIX-BIT-CHAR
                 IF SIX-BIT-CHAR-FOUND
      *              COBOL is 1-based, subtract 1 to get 0-based
                     SUBTRACT 1 FROM SIX-BIT-SUB
                     MOVE SIX-BIT-SUB-LOW1
                       TO EIGHT-BYTE-INT-CHAR(GROUP-OF-FOUR-SUB + 4)
                     ADD 1 TO OUT-BLOCK-SIZE
      *              Shift Left Logical 2 bits
                     MULTIPLY 4 BY EIGHT-BYTE-INT
                 END-IF
                 ADD 1 TO IN-BUFFER-PTR
             END-IF
           END-PERFORM

      *    OUT-BLOCK-SIZE was incremented once for each input
      *    byte, but 4 input bytes processed = 3 output bytes
      *    3 input bytes processed = 2 output bytes, and 2
      *    input bytes processed = 1 output byte.
           SUBTRACT 1 FROM OUT-BLOCK-SIZE
           STRING EIGHT-BYTE-INT-BYTES(5:OUT-BLOCK-SIZE)
             INTO OUT-BUFFER
             POINTER OUT-BUFFER-PTR
           END-STRING

           ADD OUT-BLOCK-SIZE TO OUT-BUFFER-LEN
           .

       1110-FIND-SIX-BIT-CHAR.
           INITIALIZE SIX-BIT-CHAR-SW
           INITIALIZE SIX-BIT-SUB

           PERFORM
           UNTIL SIX-BIT-SUB > LENGTH OF SIX-BIT-CHARS-TBL
           OR SIX-BIT-CHAR-FOUND
             ADD 1 TO SIX-BIT-SUB
             IF SIX-BIT-CHAR(SIX-BIT-SUB) 
             = GROUP-OF-FOUR-CHAR(GROUP-OF-FOUR-SUB)
                 SET SIX-BIT-CHAR-FOUND TO TRUE
             END-IF
           END-PERFORM
           .

       END PROGRAM B64DECOD.
       END PROGRAM b64demo2.

