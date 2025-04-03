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
       01  UserPassword        PIC X(50).
       01  FileStatus          PIC XX.
       01  CorrectPassword     PIC X(50).
       01  MainPassword        PIC X(50).
       01  Choice              PIC X(1).
       01  Identifier          PIC X(50).
       01  NewPassword         PIC X(50).
       01  TempRecord          PIC X(100).
       01  CLEAR-COMMAND       PIC X(100) VALUE "clear".
       01  EOF                 PIC X(01) VALUE 'N'.
       01  PswRecord           PIC X(100).
       01  ReadIdentifier      PIC X(50).
       01  ReadPassword        PIC X(50).
       01  Temp                PIC X(1).
       
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
               ACCEPT UserPassword
                              
               IF FUNCTION TRIM(UserPassword) = 
                       FUNCTION TRIM(CorrectPassword) THEN
                   CLOSE PswFile
                   PERFORM Choices
               ELSE
                   DISPLAY 'Incorrect password.'
                   CLOSE PswFile
                   STOP RUN
               END-IF

           ELSE
              DISPLAY 'Type in main password to create user:'
              ACCEPT MainPassword
              STRING 
                  'MAIN_USER'
                  ',' 
                  FUNCTION TRIM(MainPassword)
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
               DISPLAY 'Error opening file for appending.'
               CLOSE PswFile
              STOP RUN
           END-IF
    
           DISPLAY 'Password identifier (e.g., username or URL):'
           ACCEPT Identifier
           DISPLAY 'Type password or hit enter for generated password.'
           ACCEPT NewPassword
    
           IF FUNCTION TRIM(NewPassword) = '' THEN
               MOVE 'Auto generated password.' TO NewPassword
           END-IF
    
           STRING 
               FUNCTION TRIM(Identifier) 
               ',' 
               FUNCTION TRIM(NewPassword) 
               DELIMITED BY SIZE 
               INTO TempRecord
    
           MOVE TempRecord TO PasswordRecord
           WRITE PasswordRecord
    
           IF FileStatus NOT = '00' THEN
               DISPLAY 'Error writing to file. Status: ' FileStatus
           ELSE
               DISPLAY 'Password saved successfully!'
           END-IF
    
           CLOSE PswFile.

       ViewPassword.
           MOVE 'N' TO EOF  *> Reset EOF flag
           OPEN INPUT PswFile
           IF FileStatus NOT = '00' THEN
               DISPLAY 'Error opening file for reading.'
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
                       
                       IF FUNCTION TRIM(Identifier) = '' THEN
                           DISPLAY 'ID: ' FUNCTION TRIM(ReadIdentifier) 
                              ', Password: ' FUNCTION TRIM(ReadPassword)
       
                       ELSE
                           IF FUNCTION TRIM (ReadIdentifier) 
                               = FUNCTION TRIM(Identifier) 
                               DISPLAY 'Password for ' FUNCTION 
                                   TRIM(Identifier) ': ' ReadPassword
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           DISPLAY "Press any key to continue."
           ACCEPT Temp
           CLOSE PswFile.
