       identification division.

       Program-id. CST-8283-PROJECT-2.
       AUTHOR. JONATHAN SLAUNWHITE LIAM HENLEY.
       INSTALLATION. ALGONQUIN.
       DATE-WRITTEN. 2021-03-02.
       DATE-COMPILED. 2021-03-02.

       environment division.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT PROGRAM-FILE
       ASSIGN TO
       "C:\Users\Jonathan\Documents\School\Business Programming\Assignme
      -"nt 2\PROGRAM.txt"
               ORGANIZATION  IS  LINE  SEQUENTIAL.

       SELECT STUFILE-FILE
       ASSIGN TO
       "C:\Users\Jonathan\Documents\School\Business Programming\Assignme
      -"nt 2\STUFILE.txt"
          ORGANIZATION  IS  LINE  SEQUENTIAL.

       SELECT STUDENT-REPORT-RECORD
       ASSIGN TO
       "C:\Users\Jonathan\Documents\School\Business Programming\Assignme
      -"nt 2\STUDENT-REPORT-RECORD.txt"
       ORGANIZATION  IS  LINE  SEQUENTIAL.

       configuration section.
       data division.

       FILE SECTION.

       FD PROGRAM-FILE.
       01 PROGRAM-FILE-IN.
         10 PROGRAM-CODE PIC X(5).
         10 PROGRAM-NAME PIC X(20).

       FD STUFILE-FILE.
       01 STUFILE-FILE-IN.
         05 STUDENT-NUMBER PIC 9(6).
         05 TUITION-OWED PIC 9999V99.
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

       01 COLUMN-HEADER.
         05 NAME-HEADER PIC X(38).
         05 FILLER PIC X(2).
         05 AVERAGE PIC X(10).
         05 FILLER PIC X(4).
         05 PROGRAM-HEADER PIC X(12).
         05 FILLER PIC X(4).
         05 TUITION-OWED-HEADER PIC X(12).

       01 STUDENT-RECORDS-READ-SENTENCE.
         05 SENTENCE-WRITE PIC X(23).
         05 NUMBER-RECORDED PIC Z9.
         05 FILLER PIC X(14).
         05 SENTENCE-WRITE-TWO PIC X(30).
         05 NUMBER-RECORDED-TWO PIC Z9.
       working-storage section.

       01 PROGRAM-FILE-CONTAINER.
         05 PROGRAM-TABLE-CONTAINER OCCURS 20 TIMES.
           10 PROGRAM-CODE-CONTAINER PIC X(5).
           10 PROGRAM-NAME-CONTAINER PIC X(20).

       01 WS-EOF PIC X(4).
       01 STUDENT-AVERAGE PIC 9(3).

       01 COUNTERS.
         05 Student-Records-read PIC 9(2) VALUE 0.
         05 Student-Report-records-written PIC 9(2).

       01 TABLE-VARIBLES.
         05 LOAD-TABLE-EOF PIC X(3).
         05 LOAD-TABLE-COUNTER PIC 9(2).
         05 TABLE-COUNTER PIC 9(2).
         05 TABLE-FOUND PIC X(3).

       procedure division.

       100-PRODUCE-STUDENT-REPORT.

           PERFORM 200-INITIALIZE-COLUMN-HEADER.
           PERFORM 201-OPEN-EXTERNAL-FILES.
           PERFORM 202-READ-TABLE.

           WRITE STUDENT-REPORT-OUT.
           PERFORM 203-RESET-COLUMN-HEADER.

           PERFORM UNTIL WS-EOF = 'YES'
               PERFORM 204-READ-STUDENT-FILE
               PERFORM 205-ARITHMETIC-OPERATIONS

               PERFORM 206-TABLE-SEARCH
                 VARYING TABLE-COUNTER FROM 1 BY 1
                 UNTIL TABLE-FOUND = "YES"
                 OR TABLE-COUNTER = 20

               MOVE "NO" TO TABLE-FOUND

               IF WS-EOF IS NOT EQUAL "YES"
                   PERFORM 207-WRITE-STUDENT-RECORDS
                   ADD 1 TO Student-Records-read
               END-IF

           END-PERFORM

           MOVE "Number of records read" TO
             STUDENT-RECORDS-READ-SENTENCE.

           MOVE STUDENT-RECORDS-READ-SENTENCE TO STUDENT-REPORT-OUT.
           MOVE Student-Records-read TO NUMBER-RECORDED.
           MOVE "   Number of records written" TO SENTENCE-WRITE-TWO.
           MOVE Student-Records-read TO NUMBER-RECORDED-TWO.

           WRITE STUDENT-REPORT-OUT.

           PERFORM 208-CLOSE-EXTERNAL-FILES.

       200-INITIALIZE-COLUMN-HEADER.

           MOVE "NAME" TO NAME-HEADER.
           MOVE "AVERAGE" TO AVERAGE.
           MOVE "PROGRAM" TO PROGRAM-HEADER.
           MOVE "TUITION OWED" TO TUITION-OWED-HEADER.

       203-RESET-COLUMN-HEADER.

           MOVE " " TO NAME-HEADER.
           MOVE " " TO AVERAGE.
           MOVE " " TO PROGRAM-HEADER.
           MOVE " " TO TUITION-OWED-HEADER.

       201-OPEN-EXTERNAL-FILES.

           OPEN INPUT PROGRAM-FILE.
           OPEN INPUT STUFILE-FILE.
           OPEN OUTPUT STUDENT-REPORT-RECORD.

       202-READ-TABLE.

           PERFORM LOAD-TABLE
             VARYING LOAD-TABLE-COUNTER FROM 1 BY 1
             UNTIL LOAD-TABLE-COUNTER IS EQUAL TO 20
             OR LOAD-TABLE-EOF IS EQUAL TO "YES".

       LOAD-TABLE.

           READ PROGRAM-FILE
               AT END
                   MOVE "YES" TO LOAD-TABLE-EOF
               NOT AT END
                   MOVE PROGRAM-FILE-IN TO PROGRAM-TABLE-CONTAINER(
                       LOAD-TABLE-COUNTER).

       207-WRITE-STUDENT-RECORDS.

           MOVE STUDENT-NAME TO STUDENT-NAME-RECORD.
           MOVE STUDENT-AVERAGE TO STUDENT-AVERAGE-RECORD.

           MOVE TUITION-OWED TO TUITION-OWED-RECORD.

           WRITE STUDENT-REPORT-OUT.

       206-TABLE-SEARCH.

           MOVE "NO" TO TABLE-FOUND.

           IF PROGRAM-OF-STUDY = PROGRAM-CODE-CONTAINER(TABLE-COUNTER)
               MOVE PROGRAM-NAME-CONTAINER(TABLE-COUNTER) TO
                 PROGRAM-NAME-RECORD
               MOVE "YES" TO TABLE-FOUND.

       204-READ-STUDENT-FILE.
           READ STUFILE-FILE INTO STUFILE-FILE-IN
               AT END
                   MOVE 'YES' TO WS-EOF
           END-READ.

       205-ARITHMETIC-OPERATIONS.

           ADD COURSE-AVERAGE-1 COURSE-AVERAGE-2 COURSE-AVERAGE-3
             COURSE-AVERAGE-4 TO COURSE-AVERAGE-5.

           DIVIDE 5 INTO COURSE-AVERAGE-5 ROUNDED.

           MOVE COURSE-AVERAGE-5 TO STUDENT-AVERAGE.

       208-CLOSE-EXTERNAL-FILES.

           CLOSE PROGRAM-FILE.
           CLOSE STUFILE-FILE.
           CLOSE STUDENT-REPORT-RECORD.

           STOP run.
           goback.

       end program CST-8283-PROJECT-2.