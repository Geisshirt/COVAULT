       IDENTIFICATION DIVISION.
       PROGRAM-ID. Decrypt.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       01 I                 PIC 9(02) VALUE 1.
       01 J                 PIC 9(02) VALUE 1.
       01 CharEncrypted     PIC X.
       01 CharHash          PIC X.
       01 CharDecrypted     PIC X.
       01 CodeEncrypted     PIC 9(03).
       01 CodeHash          PIC 9(03).
       01 CodeDecrypted     PIC 9(03).
       
       LINKAGE SECTION.
       01 Encrypted     PIC X(50).
       01 Hash          PIC X(50).
       01 ReturnString  PIC X(50).
       
       PROCEDURE DIVISION USING Encrypted Hash ReturnString.
       Decrypt.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > FUNCTION 
                   LENGTH(Encrypted)
               COMPUTE J = FUNCTION MOD(I - 1, FUNCTION 
                   LENGTH(Hash)) + 1
       
               MOVE Encrypted(I:1) TO CharEncrypted
               MOVE FUNCTION ORD(CharEncrypted) TO CodeEncrypted
       
               MOVE Hash(J:1) TO CharHash
               MOVE FUNCTION ORD(CharHash) TO CodeHash
             
               *> .csv can't handle commas (ascii 44) so we add 1 in encrypt. 
               IF CodeEncrypted > 44
                   SUBTRACT 1 FROM CodeEncrypted
               END-IF
               COMPUTE CodeDecrypted = FUNCTION MOD(CodeEncrypted - 
                   CodeHash - 32 + 94, 94) + 32        
       
               MOVE FUNCTION CHAR(CodeDecrypted) TO CharDecrypted
               MOVE CharDecrypted TO ReturnString(I:1)
           END-PERFORM
           EXIT PROGRAM.
       
       END PROGRAM Decrypt.
       