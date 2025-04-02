       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MainPswFile ASSIGN TO 'mainPassword.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FileStatus.

           SELECT PswFile ASSIGN TO 'passwords.csv'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  MainPswFile.
       01  MainPswRecord    PIC X(20).
       FD  PswFile.
       01  PasswordRecord.
           05  Identifier    PIC X(50).
           05  Filler        PIC X(1) VALUE ','.
           05  NewPassword   PIC X(50).

       WORKING-STORAGE SECTION.
       01  UserPassword        PIC X(20).
       01  FileStatus          PIC XX.
       01  FileExists          PIC X VALUE 'N'.
       01  CorrectPassword     PIC X(20).
       01  MainPassword        PIC X(20).
       01  Choice              PIC x(1).
       01  IdentifierIn        PIC x(20).
       01  NewPasswordIN       PIC x(20).

       PROCEDURE DIVISION.
       OPEN INPUT MainPswFile
           IF FileStatus = '00'
               MOVE 'Y' TO FileExists
               READ MainPswFile INTO CorrectPassword
               IF FileStatus = '00'
                   DISPLAY 'Enter main password: '
                   ACCEPT UserPassword
                   IF UserPassword = CorrectPassword
                       DISPLAY 'Correct password.'
                       CLOSE MainPswFile
                       PERFORM Choices
                   ELSE
                       DISPLAY 'Incorrect password.'
                   END-IF
               END-IF
           ELSE
               DISPLAY 'Type in main password to create user.'
               ACCEPT MainPassword
               MOVE MainPassword TO MainPswRecord
               OPEN OUTPUT PswFile
               IF FileStatus = '00'
                   WRITE MainPswRecord
                   CLOSE PswFile
                   DISPLAY 'Main password saved.'
               END-IF
               PERFORM Choices 
           END-IF
       CLOSE PswFile.
       STOP RUN.

       Choices.
           DISPLAY '1) Add password'
           DISPLAY '2) View passwords'
           DISPLAY '3) Exit'
           ACCEPT choice
           IF choice = 1
               PERFORM AddPassword
           ELSE IF choice = 2
           ELSE IF choice = 3
           ELSE
               PERFORM Choices
           END-IF
           STOP RUN.

       AddPassword.
           OPEN EXTEND PswFile
           DISPLAY 'Password identifier (e.g. username or URL):'
           ACCEPT IdentifierIn
           DISPLAY 'Type password or hit enter for generated password.'
           ACCEPT NewPasswordIn
           IF NewPasswordIn = ''
               MOVE 'hejsa' TO NewPasswordIn
           END-IF

           STRING 
               FUNCTION TRIM(IdentifierIn) 
               ',' 
               FUNCTION TRIM(NewPasswordIn) 
               DELIMITED BY SIZE 
               INTO PasswordRecord.

           WRITE PasswordRecord
           CLOSE PswFile
           DISPLAY 'Password saved to identifier ' IdentifierIn
           STOP RUN.
