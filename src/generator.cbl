      *IDENTIFICATION DIVISION.
      *PROGRAM-ID. Generator.

      *DATA DIVISION.
      *LINKAGE SECTION.
      *01  ReturnString     PIC X(50).

      *PROCEDURE DIVISION USING ReturnString.
      *    MOVE "hello" TO ReturnString
      *    GOBACK.

      *END PROGRAM Generator.

      *IDENTIFICATION DIVISION.
      *PROGRAM-ID. Generator.

      *DATA DIVISION.
      *LINKAGE SECTION.
      *01  ReturnString     PIC X(50).

      *PROCEDURE DIVISION USING ReturnString.
      *    MOVE "hello" TO ReturnString
      *    GOBACK.

      *END PROGRAM Generator.
  
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Generator.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  I                   PIC 9(02) VALUE 1.
       77  RAND-VAL            PIC 9(02) VALUE 0.
       77  RAND-INDEX          PIC 99.
       77  RANDOM-CHAR         PIC X.
       77  SEED                PIC 9(9) VALUE 123456789.
       77  A                   PIC 9(9) VALUE 1664525.
       77  C                   PIC 9(9) VALUE 10139.
       77  M                   PIC 9(9) VALUE 4294967296.
       77  ASCII-VALUE         PIC 9(03).
       77  TOTAL-ASCII         PIC 9(09) VALUE 0.
       01  LOCAL-STRING        PIC X(50) VALUE SPACES.
       01  CHAR-TABLE.
           05 CHARS            PIC X(52) VALUE 
           "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".

       LINKAGE SECTION.
       01  ReturnString        PIC X(50).
       01  Identifier          PIC X(50).

       PROCEDURE DIVISION USING Identifier ReturnString.
           MOVE 0 TO TOTAL-ASCII

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 
                   FUNCTION LENGTH(Identifier)
               COMPUTE ASCII-VALUE = FUNCTION ORD(Identifier (I:1))
               ADD ASCII-VALUE TO TOTAL-ASCII
           END-PERFORM

           MOVE TOTAL-ASCII TO SEED

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
               COMPUTE SEED = (A * SEED + C)
               COMPUTE RAND-VAL = SEED / (M / 52) + 1
               MOVE CHARS(RAND-VAL:1) TO RANDOM-CHAR
               MOVE RANDOM-CHAR TO LOCAL-STRING (I:1)
           END-PERFORM

           MOVE LOCAL-STRING TO ReturnString

           GOBACK.

       END PROGRAM Generator.
