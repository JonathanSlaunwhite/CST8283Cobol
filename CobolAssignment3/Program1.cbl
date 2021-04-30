
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CST-8283-PROJECT-3.
       AUTHOR. JONATHON SLAUNWHITE, LIAM HENLEY-VACHON.
       INSTALLATION. ALGONQUIN.
       DATE-WRITTEN. 04-01-2021.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *Link Student file and set as line sequential
       SELECT STUFILE-FILE
        ASSIGN TO
       "C:\Users\Jonathan\Documents\School\Business Programming\Assignme
      -"nt 2\STUFILE3.txt"
           ORGANIZATION  IS  LINE  SEQUENTIAL.

      *Link file for student report output and set as line sequential
       SELECT STUDENT-REPORT-RECORD
       ASSIGN TO
       "C:\Users\Jonathan\Documents\School\Business Programming\Assignme
      -"nt 2\STUDENT-REPORT-RECORD.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

      *Link program file and set as line sequential
       SELECT PROGRAM-FILE
       ASSIGN TO
       "C:\Users\Jonathan\Documents\School\Business Programming\Assignme
      -"nt 2\PROGRAM.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

      *Link a student file and set it as indexed line sequential
      *with access mode as random and the student number as the record
      *key
       SELECT STUFILE-FILE-INDEXED
       ASSIGN TO
       "C:\Users\Jonathan\Documents\School\Business Programming\Assignme
      -"nt 2\STUFILE-INDEXED.txt"
          ORGANIZATION  IS INDEXED
          ACCESS MODE IS RANDOM
          RECORD KEY IS STUDENT-NUMBER-INDEXED
          FILE STATUS IS FILE-STATUS-THING.

       DATA DIVISION.
       FILE SECTION.

      *Variables for new indexed student file
        FD STUFILE-FILE-INDEXED.
       01 STUFILE-FILE-IN-INDEXED.
         05 STUDENT-NUMBER-INDEXED PIC 9(6).
         05 TUITION-OWED-INDEXED PIC 9999V99.
         05 STUDENT-NAME-INDEXED PIC X(40).
         05 PROGRAM-OF-STUDY-INDEXED PIC X(5).
         05 COURSE-CODE-1-INDEXED PIC X(7).
         05 COURSE-AVERAGE-1-INDEXED PIC 9(3).
         05 COURSE-CODE-2-INDEXED PIC X(7).
         05 COURSE-AVERAGE-2-INDEXED PIC 9(3).
         05 COURSE-CODE-3-INDEXED PIC X(7).
         05 COURSE-AVERAGE-3-INDEXED PIC 9(3).
         05 COURSE-CODE-4-INDEXED PIC X(7).
         05 COURSE-AVERAGE-4-INDEXED PIC 9(3).
         05 COURSE-CODE-5-INDEXED PIC X(7).
         05 COURSE-AVERAGE-5-INDEXED PIC 9(3).

      *Variables for original student file
       FD STUFILE-FILE.
       01 STUFILE-FILE-IN.
         05 STUDENT-NUMBER PIC 9(6).
         05 TUITION-OWED PIC 9(6).
         05 STUDENT-NAME PIC X(40).
         05 PROGRAM-OF-STUDY PIC X(5).
         05 COURSE-CODE-1 PIC X(7).
         05 COURSE-AVERAGE-1 PIC 9(3).
         05 COURSE-CODE-2 PIC X(7).
         05 COURSE-AVERAGE-2 PIC 9(3).
         05 COURSE-CODE-3 PIC X(7).
         05 COURSE-AVERAGE-3 PIC 9(3).
         05 COURSE-CODE-4 PIC X(7).
         05 COURSE-AVERAGE-4 PIC 9(3).
         05 COURSE-CODE-5 PIC X(7).
         05 COURSE-AVERAGE-5 PIC 9(3).

      *Student report out variables
       FD STUDENT-REPORT-RECORD.
       01 STUDENT-REPORT-OUT.
         05 STUDENT-INFO-OUT.
           10 STUDENT-NAME-RECORD PIC X(40).
           10 FILLER PIC X(2).
           10 STUDENT-AVERAGE-RECORD PIC 9(3).
           10 FILLER PIC X(4).
           10 PROGRAM-NAME-RECORD PIC X(20).
           10 FILLER PIC X(4).
           10 TUITION-OWED-RECORD PIC ZZZZ.99.

      *Program file table as copy member
       FD PROGRAM-FILE.
       COPY "D:\Users\Liam\project3_table_data.txt".

      *Column header declarations for student report out file
       01 COLUMN-HEADER.
         05 NAME-HEADER PIC X(4).
         05 FILLER PIC X(2).
         05 AVERAGE PIC X(7).
         05 FILLER PIC X(4).
         05 PROGRAM-HEADER PIC X(7).
         05 FILLER PIC X(4).
         05 TUITION-OWED-HEADER PIC X(12).

      *Number of student records read and
       01 STUDENT-RECORDS-READ-SENTENCE.
         05 NUMBER-RECORDED PIC Z9.
         05 FILLER PIC X(14).
         05 SENTENCE-WRITE-TWO PIC X(30).
         05 NUMBER-RECORDED-TWO PIC Z9.

       WORKING-STORAGE SECTION.

      *Program file container for course code and name
       01 PROGRAM-FILE-CONTAINER.
         05 PROGRAM-TABLE-CONTAINER OCCURS 20 TIMES.
           10 PROGRAM-CODE-CONTAINER PIC X(5).
           10 PROGRAM-NAME-CONTAINER PIC X(20).

      *Table end-of-file flag, table counter, and table found flag
       01 TABLE-VARIBLES.
         05 LOAD-TABLE-EOF PIC X(3).
         05 LOAD-TABLE-COUNTER PIC 9(2).
         05 TABLE-COUNTER PIC 9(2).
         05 TABLE-FOUND PIC X(3).

      *End-of-file flag
       01 WS-EOF PIC X(4).
      *Temporarily store student average calculations
       01 STUDENT-AVERAGE PIC 9(3).
      *Tuition payment from transactions
       01 PAYMENT-WS PIC 9(5)V99.

      *File status field, currently unused but left it just in case
       01 FILE-STATUS-THING PIC X(2).
      *Var to check if student number from transactions exists in file
       01 WS-STU-NUM PIC 9(6).

      *Keep track of number of records read and written
       01 COUNTERS.
         05 Student-Records-read PIC 9(2).
         05 Student-Report-records-written PIC 9(2).
         05 READ-COUNTER pic 9(3).

      *Screen Section to handle user transactions
       SCREEN SECTION.
       01 STUDENT-DATA-ENTRY-SCREEN.
         05 VALUE "DATA DISPLAY" BLANK SCREEN LINE 1 COL 35.
         05 VALUE "STUDENT NUMBER" LINE 3 COL 10.
         05 STUDENT-NUMBER-IN LINE 3 COL 25 PIC 9(6) TO
                              STUDENT-NUMBER-INDEXED.
         05 VALUE "PAYMENT" LINE 5 COL 10.
         05 PAYMENT-IN LINE 5 COL 25 PIC 9(5)V99 TO PAYMENT-WS.

       PROCEDURE DIVISION.

       100-PRODUCE-STUDENT-REPORT.
      *Initialize the column headers with their values
           PERFORM 200-INITIALIZE-COLUMN-HEADER.
      *Open files for read and write
           PERFORM 201-OPEN-EXTERNAL-FILES.
      *Load program dta into table
           PERFORM 202-READ-TABLE.

      *Write student info to output file
           WRITE STUDENT-REPORT-OUT.
      *Reset column headers to be empty strings since it caused printing
      *issues
           PERFORM 203-RESET-COLUMN-HEADER.

      *While its not the end of the file, perform these operations
           PERFORM UNTIL WS-EOF = 'YES'
      *Read in data from the student file
               PERFORM 204-READ-STUDENT-FILE
      *Perform transcations on student tuition
               PERFORM 205-TRANSACTIONS
      *Calculate student average
               PERFORM 206-ARITHMETIC-OPERATIONS
      *Search table for correct course until it has been found
               PERFORM 207-TABLE-SEARCH
                 VARYING TABLE-COUNTER FROM 1 BY 1
                 UNTIL TABLE-FOUND = "YES"
                 OR TABLE-COUNTER = 20
      *If course wasn't found
               MOVE "NO" TO TABLE-FOUND

      *Write out student records and track number of records read
               IF WS-EOF IS NOT EQUAL "YES"
                   PERFORM 208-WRITE-STUDENT-RECORDS
                   ADD 1 TO Student-Records-read
               END-IF

           END-PERFORM.

      *Initialize number of records read pic
           MOVE "Number of records read" TO
             STUDENT-RECORDS-READ-SENTENCE.

      *Initialize number of records read variables
           MOVE STUDENT-RECORDS-READ-SENTENCE TO STUDENT-REPORT-OUT.
           MOVE Student-Records-read TO NUMBER-RECORDED.
           MOVE "   Number of records written" TO SENTENCE-WRITE-TWO.
           MOVE Student-Records-read TO NUMBER-RECORDED-TWO.

           WRITE STUDENT-REPORT-OUT.

      *Close all external files
           PERFORM 209-CLOSE-EXTERNAL-FILES.

      *Accept user data from keyboard for transactions
       205-TRANSACTIONS.
           DISPLAY STUDENT-DATA-ENTRY-SCREEN.
           ACCEPT STUDENT-DATA-ENTRY-SCREEN.
      *Check to see if entered student number is valid
           IF WS-STU-NUM = STUDENT-NUMBER-INDEXED
      *Update tuition owed
               PERFORM 210-UPDATE-TUITION
      *Re-write indexed student file
               REWRITE STUFILE-FILE-IN-INDEXED
               END-REWRITE
           END-IF.

      *Calculate new tuition cost
       210-UPDATE-TUITION.
           SUBTRACT PAYMENT-WS FROM TUITION-OWED-INDEXED
             GIVING TUITION-OWED-INDEXED.

      *Initialize column headers
       200-INITIALIZE-COLUMN-HEADER.
           MOVE "NAME" TO NAME-HEADER.
           MOVE "AVERAGE" TO AVERAGE.
           MOVE "PROGRAM" TO PROGRAM-HEADER.
           MOVE "TUITION OWED" TO TUITION-OWED-HEADER.

      *Reset column headers to avoid printing issues
       203-RESET-COLUMN-HEADER.
           MOVE " " TO NAME-HEADER.
           MOVE " " TO AVERAGE.
           MOVE " " TO PROGRAM-HEADER.
           MOVE " " TO TUITION-OWED-HEADER.

      *Open all external files for read and write
       201-OPEN-EXTERNAL-FILES.
           OPEN INPUT PROGRAM-FILE.
           OPEN I-O STUFILE-FILE.
           OPEN OUTPUT STUDENT-REPORT-RECORD.
           OPEN OUTPUT STUFILE-FILE-INDEXED.

       202-READ-TABLE.

      *    GO THROUGH 20 INSTANCES OF THE PROGRAM FILE
           PERFORM LOAD-TABLE
             VARYING LOAD-TABLE-COUNTER FROM 1 BY 1
             UNTIL LOAD-TABLE-COUNTER IS EQUAL TO 20
             OR LOAD-TABLE-EOF IS EQUAL TO "YES".

       LOAD-TABLE.

      *    READ THE PROGRAM FILE AND ITS CONTENTS TO LOAD TABLE COUNTER
           READ PROGRAM-FILE
               AT END
                   MOVE "YES" TO LOAD-TABLE-EOF
               NOT AT END
                   MOVE PROGRAM-FILE-IN TO PROGRAM-TABLE-CONTAINER(
                       LOAD-TABLE-COUNTER).

       208-WRITE-STUDENT-RECORDS.

      *    MOVE THE STUDENT VARIABLES TO BE PRINTED TO THE NEW
      *    STUDENT REPORT OUT  FILE
           MOVE STUDENT-NAME TO STUDENT-NAME-RECORD.
           MOVE STUDENT-AVERAGE TO STUDENT-AVERAGE-RECORD.
           MOVE TUITION-OWED TO TUITION-OWED-RECORD.
           WRITE STUDENT-REPORT-OUT.

       207-TABLE-SEARCH.

      *    SERCHING FOR NEW VALUE SET FOUND VARIABLE TO FALSE
           MOVE "NO" TO TABLE-FOUND.

      *    IF PROGRAM OF STUDY IS FOUND MOVE FOUND VARIABLE TO YES
      *    THEN MOVE PROGRAM OF STUDY TO APPROPIRATE VARIABLE
           IF PROGRAM-OF-STUDY = PROGRAM-CODE-CONTAINER(TABLE-COUNTER)
               MOVE PROGRAM-NAME-CONTAINER(TABLE-COUNTER) TO
                 PROGRAM-NAME-RECORD
               MOVE "YES" TO TABLE-FOUND.

       204-READ-STUDENT-FILE.

      *    READ THE STUDENT FILE S
           READ STUFILE-FILE INTO STUFILE-FILE-IN
               AT END
                   MOVE 'YES' TO WS-EOF
           END-READ.

      *    MOVE THE STUDENT DATA INTO A INDEXED SEQUENTIAL FILE
           MOVE STUFILE-FILE-IN TO STUFILE-FILE-IN-INDEXED.

           ADD 1 TO READ-COUNTER.

           DISPLAY READ-COUNTER.

      *ENSURE THAT ONLY THE CORRECT NUMBER OF RECORDS IS READ FROM FILE

           IF READ-COUNTER <= 10 THEN

      *        WRITE THE DATA TO THE INDEXED SEQENTIAL FILE
               WRITE STUFILE-FILE-IN-INDEXED
                   INVALID KEY
                       DISPLAY "INVALID KEY"
                       DISPLAY "NOT INVALID KEY"
           END-IF.

       206-ARITHMETIC-OPERATIONS.

      * THis is how we would call external program if we could get it
      * working,

      *        CALL 'CALCULATE' USING COURSE-AVERAGE-1,
      *          * COURSE-AVERAGE-2,
      *          * COURSE-AVERAGE-3,
      *          * COURSE-AVERAGE-4,
      *          * COURSE-AVERAGE-5,
      *          * STUDENT-AVERAGE.

      *FORMULA TO CALCULATE THE STUDENT AVERAGE
           ADD COURSE-AVERAGE-1 COURSE-AVERAGE-2 COURSE-AVERAGE-3
             COURSE-AVERAGE-4 TO COURSE-AVERAGE-5.

           DIVIDE 5 INTO COURSE-AVERAGE-5 ROUNDED.

           MOVE COURSE-AVERAGE-5 TO STUDENT-AVERAGE.

       209-CLOSE-EXTERNAL-FILES.

      *    CLOSE ALL FILES OPENED IN PROGRAM
           CLOSE STUFILE-FILE-INDEXED.
           CLOSE PROGRAM-FILE.
           CLOSE STUFILE-FILE.
           CLOSE STUDENT-REPORT-RECORD.

      *    END OF PROGRAM
           STOP RUN.
           GOBACK.

       END PROGRAM CST-8283-PROJECT-3.
