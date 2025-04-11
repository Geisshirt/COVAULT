       IDENTIFICATION DIVISION.
       PROGRAM-ID. Encrypt.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       01 I                 PIC 9(02) VALUE 1.
       01 J                 PIC 9(02) VALUE 1.
       01 CharPassed        PIC X.
       01 CharHash          PIC X.
       01 CharEncrypted     PIC X.
       01 CodePassed        PIC 9(03).
       01 CodeHash          PIC 9(03).
       01 CodeEncrypted     PIC 9(03).
       
       LINKAGE SECTION.
       01 Passed       PIC X(50).
       01 Hash         PIC X(50).
       01 ReturnString PIC X(50).
       
       PROCEDURE DIVISION USING Passed Hash ReturnString.
       Encrypt.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > FUNCTION 
                   LENGTH(Passed)
               COMPUTE J = FUNCTION MOD(I - 1, FUNCTION 
                   LENGTH(Hash)) + 1
       
               MOVE Passed(I:1) TO CharPassed
               MOVE FUNCTION ORD(CharPassed) TO CodePassed
       
               MOVE Hash(J:1) TO CharHash
               MOVE FUNCTION ORD(CharHash) TO CodeHash
       
               COMPUTE CodeEncrypted = FUNCTION MOD(CodePassed + 
                   CodeHash - 32, 94) + 32
               *> Replace commas since we save in .csv
               IF CodeEncrypted = 45
                   MOVE 34 TO CodeEncrypted 
               END-IF

               MOVE FUNCTION CHAR(CodeEncrypted) TO CharEncrypted
               MOVE CharEncrypted TO ReturnString(I:1)
           END-PERFORM
           EXIT PROGRAM.
       
       END PROGRAM Encrypt.
