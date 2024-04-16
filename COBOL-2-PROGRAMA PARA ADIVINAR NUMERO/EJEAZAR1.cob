      ******************************************************************
      * Author:GONZALEZ NAZARENA ARACELI
      * Date:02/2024
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEAZAR1.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  flag                  PIC 9     VALUE 1.
       77  aux                   PIC 9.
       77  ROUTINE-NAME          PIC X(8) VALUE "EJEAZAR0".
       77  aux2                  PIC 9999 COMP.


       01  NUMBERSS OCCURS 4 TIMES.
           05 NumberEntered      PIC 9 COMP.

       01  NUM-INCOGNITOO.
           05 FILLER             PIC X(20) VALUE "EL NRO INCOGNITO ES:".
       01  P-CORRECT.
           05 FILLER             PIC X(18) VALUE "PUNTOS CORRECTOS:".
           05 CORRECT            PIC 9 .

       01  P-REGULAR.
           05 FILLER             PIC X(18) VALUE "PUNTOS REGULARES:".
           05 REGULAR            PIC 9 .

       01  P-ERROR.
           05 FILLER             PIC X(18) VALUE "PUNTOS ERRONEOS:".
           05 ERRORR             PIC 9.


       01  SHOW-NUMBER-OF-ATTEMPTS.
           05 Titles             PIC X(18) VALUE "NRO INTENTOS: ".
           05  GameAttempts      PIC 9     VALUE 0.


       01  TITLE-NUM-INCOG.
           05 FILLER             PIC X(17) VALUE "NRO SECRETO:".
           05 NUMB-INCOG         PIC 9999.


       01  NUMBER-INCOGNITO OCCURS 4 TIMES.
           05 Number-Incog         PIC 9 COMP.


       01 AREA-COMUNICACION.
           05 CAMPO-INCOGNITO      PIC X(4).

       PROCEDURE DIVISION.
      */////////////////////////////////////////////////////////////////
       MAIN-PROCEDURE.
           PERFORM 1000-CALL-ROUTINE
           PERFORM 2000-ENTER-VALIDATED-NUMBER
           PERFORM 3000-CORRECT-REGULAR-OR-ERROR
           PERFORM 4000-NUMBER-OF-ATTEMPTS
           STOP RUN.
      */////////////////////////////////////////////////////////////////

       1000-CALL-ROUTINE.
           DISPLAY "***************************************************"
           DISPLAY "*            INICIO DEL JUEGO DE AZAR             *"
           DISPLAY "***************************************************"
           INITIALIZE AREA-COMUNICACION
           CALL ROUTINE-NAME USING AREA-COMUNICACION
           PERFORM 1100-DECOMPOSE-NUMBER
           MOVE CAMPO-INCOGNITO TO NUMB-INCOG
           DISPLAY TITLE-NUM-INCOG.


      *-----------------------------------------------------------------

       1100-DECOMPOSE-NUMBER.
           MOVE CAMPO-INCOGNITO to aux2
           COMPUTE Number-Incog(1) = aux2 / 1000
           COMPUTE Number-Incog(2)= (aux2 - Number-Incog(1)*1000)/100
           COMPUTE Number-Incog(3) = (aux2 - Number-Incog(2)*100
           - Number-Incog(1)*1000) / 10
           COMPUTE Number-Incog(4) = aux2 - (Number-Incog(1) * 1000)
           - (Number-Incog(2) * 100) - (Number-Incog(3) * 10).

      *-----------------------------------------------------------------

       2000-ENTER-VALIDATED-NUMBER.
           MOVE 1 TO flag
           PERFORM UNTIL flag > 4
               MOVE 5 TO aux
               PERFORM UNTIL  aux NOT EQUAL 5
                   DISPLAY "INGRESE CUATRO NUMEROS DISTINTOS"
                   ACCEPT NUMBERSS(flag)
                   PERFORM 2100-NOT-REPEAT-ENTERED-NUMBER
               END-PERFORM
               ADD 1 TO flag
           END-PERFORM.

      *-----------------------------------------------------------------

       2100-NOT-REPEAT-ENTERED-NUMBER.
           MOVE 1 TO AUX
           PERFORM UNTIL flag = aux
               IF NUMBERSS(aux) EQUAL NUMBERSS(flag)
                   MOVE 5 TO aux
                   DISPLAY "error.NO se pueden repetir numeros"
                   DISPLAY "Ingrese otra vez el numero"
                   ACCEPT NUMBERSS(flag)
               ELSE
                   ADD 1 TO aux
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------

       3000-CORRECT-REGULAR-OR-ERROR.
           PERFORM UNTIL CORRECT EQUAL 4
               PERFORM 3100-INICIALICE-GAME-POINTS
               PERFORM 3200-CHECK-IF-THE-NUMBER-IS
               DISPLAY P-CORRECT
               DISPLAY P-REGULAR
               DISPLAY P-ERROR

               IF CORRECT NOT EQUAL 4
     ¨             DISPLAY "SI QUIERE SEGUIR JUGANDO ESCRIBA 1 SINO 0"
                   ACCEPT flag
                   IF flag equal 1
                       PERFORM 2000-ENTER-VALIDATED-NUMBER
                       PERFORM 3100-INICIALICE-GAME-POINTS
                    ELSE
                        MOVE 4 TO CORRECT
                    END-IF
               END-IF
               ADD 1 TO GameAttempts
           END-PERFORM.

      *-----------------------------------------------------------------

       3100-INICIALICE-GAME-POINTS.
           MOVE 0 TO CORRECT
           MOVE 0 TO REGULAR
           MOVE 0 TO ERRORR.

      *-----------------------------------------------------------------

       3200-CHECK-IF-THE-NUMBER-IS.
           MOVE 1 TO aux
           PERFORM UNTIL aux > 4
               MOVE 1 TO aux2
               PERFORM UNTIL aux2 > 4
                   IF NUMBERSS(aux) EQUAL Number-Incog(aux2)
                   AND  aux = aux2
                       ADD 1 TO CORRECT
                       MOVE 7 TO AUX2
                   ELSE
                       IF  NUMBERSS(aux) EQUAL Number-Incog(aux2)
                       AND aux NOT EQUAL aux2
                           ADD 1 TO REGULAR
                           MOVE 7 TO AUX2
                       END-IF
                   END-IF
                   ADD 1 TO aux2
               END-PERFORM
               IF aux2 equal 5
                   ADD 1 TO ERRORR
               END-IF
               ADD 1 TO aux
           END-PERFORM.

      *-----------------------------------------------------------------

       4000-NUMBER-OF-ATTEMPTS.
           PERFORM 3100-INICIALICE-GAME-POINTS
           PERFORM 3200-CHECK-IF-THE-NUMBER-IS
           DISPLAY "***************************************************"
           DISPLAY "*                  FIN DEL JUEGO                  *"
           DISPLAY "***************************************************"
           DISPLAY P-CORRECT
           DISPLAY P-REGULAR
           DISPLAY P-ERROR
           DISPLAY SHOW-NUMBER-OF-ATTEMPTS
           DISPLAY TITLE-NUM-INCOG.

      *-----------------------------------------------------------------
       END PROGRAM EJEAZAR1.
