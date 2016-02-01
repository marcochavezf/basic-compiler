10  REM PRINTS THE FACTORIALS OF NUMBERS FROM 0 TO 20.
20  REM FACTORIALS ARE COMPUTED USING A RECURSIVE SUBROUTINE (OMG!).
30  GOSUB 300
40  STOP
100 REM -------------------------------------------------------------
110 REM RECURSIVE FACTORIAL SUBROUTINE.
120 REM INPUT: X
130 REM OUTPUT: R
140 REM -------------------------------------------------------------
150 IF X > 0 THEN 180 
160 LET R = 1
170 RETURN
180 LET X = X - 1
190 GOSUB 100
200 LET X = X + 1
210 LET R = R * X
220 RETURN
300 REM -------------------------------------------------------------
310 REM MAIN SUBROUTINE.
320 REM -------------------------------------------------------------
330 PRINT "COMPUTING FACTORIALS RECURSIVELY:"
340 FOR I = 0 TO 20
350 LET X = I
360 GOSUB 100
370 PRINT I "", R
380 NEXT I
390 RETURN
400 END
