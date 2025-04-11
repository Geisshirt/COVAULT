       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PswFile ASSIGN TO 'passwords.csv'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  PswFile.
       01  PasswordRecord    PIC X(100).  

       WORKING-STORAGE SECTION.
       01  FileStatus             PIC XX.
       01  CorrectPassword        PIC X(50).
       01  MainPassword           PIC X(50).
       01  Choice                 PIC X(1).
       01  Identifier             PIC X(50).
       01  NewPassword            PIC X(50).
       01  TempRecord             PIC X(100).
       01  CLEAR-COMMAND          PIC X(100) VALUE "clear".
       01  EOF                    PIC X(01) VALUE 'N'.
       01  PswRecord              PIC X(100).
       01  ReadIdentifier         PIC X(50).
       01  ReadPassword           PIC X(50).
       01  Temp                   PIC X(1).
       01  EncryptedMainPassword  PIC X(100).

       PROCEDURE DIVISION.
           OPEN INPUT PswFile
           IF FileStatus = '00' THEN
               READ PswFile INTO PasswordRecord
               IF FileStatus = '10' THEN
                   DISPLAY 'Error: No data in password file.'
                   CLOSE PswFile
                   STOP RUN
               ELSE
                   UNSTRING PasswordRecord DELIMITED BY ',' 
                       INTO Identifier CorrectPassword
       
                   CALL "SYSTEM" USING CLEAR-COMMAND
                   DISPLAY 'Enter main password: '
                   ACCEPT MainPassword
       
                   CALL 'Encrypt' USING 
                       FUNCTION TRIM(MainPassword), 
                       FUNCTION TRIM(MainPassword), 
                       EncryptedMainPassword
       
                   IF FUNCTION TRIM(EncryptedMainPassword) = 
                           FUNCTION TRIM(CorrectPassword) THEN
                       CLOSE PswFile
                       PERFORM Choices
                   ELSE
                       DISPLAY 'Incorrect password.'
                       CLOSE PswFile
                       STOP RUN
                   END-IF
               END-IF
       
           ELSE
               CALL "SYSTEM" USING CLEAR-COMMAND
               DISPLAY 'Type in main password to create user:'
               ACCEPT MainPassword
       
               CALL 'Encrypt' USING 
                   FUNCTION TRIM(MainPassword), 
                   FUNCTION TRIM(MainPassword), 
                   EncryptedMainPassword
       
               STRING 
                   'USER'
                   ','
                   FUNCTION TRIM(EncryptedMainPassword)
                   DELIMITED BY SIZE 
                   INTO TempRecord
       
               OPEN OUTPUT PswFile
               IF FileStatus = '00' THEN
                   MOVE TempRecord TO PasswordRecord
                   WRITE PasswordRecord
                   CLOSE PswFile
                   DISPLAY 'Main password saved.'
               END-IF
               PERFORM Choices
           END-IF
           CLOSE PswFile.
           STOP RUN.

       Choices.
           PERFORM UNTIL Choice = '3'
               CALL "SYSTEM" USING CLEAR-COMMAND
               DISPLAY '1) Add password'
               DISPLAY '2) View passwords'
               DISPLAY '3) Exit'
               ACCEPT Choice
               EVALUATE Choice
                   WHEN '1'
                       PERFORM AddPassword
                   WHEN '2'
                       PERFORM ViewPassword
                   WHEN OTHER
                       PERFORM Choices
               END-EVALUATE
           END-PERFORM.

       AddPassword.
           OPEN EXTEND PswFile
           IF FileStatus NOT = '00' THEN
               CLOSE PswFile
              STOP RUN
           END-IF
    
           DISPLAY 'Password identifier (e.g., username or URL):'
           ACCEPT Identifier
           DISPLAY 'Type password or hit enter for generated password.'
           ACCEPT NewPassword
    
           IF FUNCTION TRIM(NewPassword) = '' THEN
               CALL 'Generator' USING Identifier NewPassword
           
           END-IF

           CALL 'Encrypt'
               USING
                   Identifier
                   MainPassword
                   Identifier

           CALL 'Encrypt' 
               USING 
                   NewPassword 
                   MainPassword 
                   NewPassword

           STRING 
               FUNCTION TRIM(Identifier) 
               ',' 
               FUNCTION TRIM(NewPassword)
               DELIMITED BY SIZE 
               INTO TempRecord
    
           MOVE TempRecord TO PasswordRecord
           WRITE PasswordRecord
           CLOSE PswFile.

       ViewPassword.
           MOVE 'N' TO EOF  *> Reset EOF flag
           OPEN INPUT PswFile
           IF FileStatus NOT = '00' THEN
               CLOSE PswFile
               STOP RUN
           END-IF
    
           DISPLAY 
               'Type identifier or press enter to show all passwords.'
           ACCEPT Identifier
    
           PERFORM UNTIL EOF = 'Y'
               READ PswFile INTO PswRecord
                   AT END 
                       MOVE 'Y' TO EOF
                   NOT AT END 
                       UNSTRING PswRecord DELIMITED BY ','
                           INTO ReadIdentifier, ReadPassword
                       END-UNSTRING

                       IF FUNCTION TRIM(ReadIdentifier) = 'USER'
                           CONTINUE
                       ELSE
                           CALL 'Decrypt'
                               USING
                                   ReadIdentifier
                                   MainPassword
                                   ReadIdentifier

                           CALL 'Decrypt'
                               USING ReadPassword 
                                     MainPassword 
                                     ReadPassword

                           IF FUNCTION TRIM(Identifier) = ''
                               DISPLAY 'ID: ' 
                               FUNCTION TRIM(ReadIdentifier) 
                               ', Password: ' 
                               FUNCTION TRIM(ReadPassword)
                           ELSE
                               IF FUNCTION TRIM(ReadIdentifier) = 
                                       FUNCTION TRIM(Identifier) 
                                   DISPLAY 'Password for ' 
                                   FUNCTION TRIM(Identifier) 
                                   ': ' 
                                   FUNCTION TRIM(ReadPassword)
                               END-IF
                          END-IF
                       END-IF
               END-READ
           END-PERFORM.

           DISPLAY ""
           DISPLAY "Press any key to continue."
           ACCEPT Temp
           CLOSE PswFile.
