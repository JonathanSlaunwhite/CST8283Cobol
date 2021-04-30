       identification division.
       program-id. Program1.

       environment division.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT STUDENT-FILE-IN
           ASSIGN TO
           "C:\Users\Jonathan\Documents\School\HELLO.txt"
               organization IS LINE SEQUENTIAL.

       configuration section.

       data division.
       FILE SECTION.
       FD STUDENT-FILE-IN.
       01 STUDENT-RECORD-IN.
         05 STUDENT-NUMBER-IN PIC 9(6).
         05 TUITION-OWED-IN PIC 9(6).
         05 STUDENT-NAME-IN PIC A(40).

       working-storage section.
       01 RECORD-BEING-ENTERED.
         05 REPEAT-DATA-ENTRY PIC A(3).

       procedure division.

       100-STUDENT-RECORDS.

           PERFORM 201-INITIALIZATION.
           PERFORM 202-File-Processing.
           PERFORM 203-PROGRAM-TERMINATION.

       201-INITIALIZATION.

           PERFORM 301-OPEN-FILE.
           PERFORM 302-ENTER-DATA-TO-RECORD.

       301-OPEN-FILE.

           OPEN OUTPUT STUDENT-FILE-IN.

       302-ENTER-DATA-TO-RECORD.
           DISPLAY 'Is a Record being entered YES OR NO'.
           ACCEPT REPEAT-DATA-ENTRY.
           PERFORM 202-File-Processing.

       305-FILE-HANDLING.

           PERFORM 401-GET-RECORD-DATA-IN.
           PERFORM 402-WRITE-DATA-TO-EXTERNAL-FILE.
           PERFORM 302-ENTER-DATA-TO-RECORD.

       401-GET-RECORD-DATA-IN.

           DISPLAY 'PLEASE ENTER STUDENT NUMBER'.
           ACCEPT STUDENT-NUMBER-IN.

           DISPLAY 'PLEASE ENTER TUITION'.
           ACCEPT TUITION-OWED-IN

           DISPLAY 'PLEASE ENTER STUDENT NAME'.
           ACCEPT STUDENT-NAME-IN.

       402-WRITE-DATA-TO-EXTERNAL-FILE.

           WRITE STUDENT-RECORD-IN.

       202-File-Processing.

           IF REPEAT-DATA-ENTRY IS EQUAL TO "NO" OR "no"
               PERFORM 203-PROGRAM-TERMINATION

           END-IF.

           PERFORM 305-FILE-HANDLING.

       203-PROGRAM-TERMINATION.

           PERFORM 306-CLOSE-FILE.
           PERFORM 307-PROGRAM-END.

       306-CLOSE-FILE.

           CLOSE STUDENT-FILE-IN.

       307-PROGRAM-END.
           goback.

       end program Program1.