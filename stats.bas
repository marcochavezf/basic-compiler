10  REM COMPUTES THE AVERAGE AND STANDARD DEVIATION OF 200 RANDOM NUMBERS.
20  LET N = 200
30  DIM A(200)
40  GOSUB 100
50  GOSUB 200
60  GOSUB 400
70  PRINT "AVERAGE = " V
80  PRINT "STANDARD DEVIATION = " D
90  STOP
100 REM -------------------------------------------------------------
110 REM FILL LIST A.
120 REM INPUT: N
130 REM OUTPUT: A
140 REM -------------------------------------------------------------
150 FOR J = 1 TO N
160 LET A(J) = INT(RND(0) * 100)
170 NEXT J
180 RETURN
200 REM -------------------------------------------------------------
210 REM COMPUTE AVERAGE OF A.
220 REM INPUT: A, N
230 REM OUTPUT: V
240 REM -------------------------------------------------------------
250 LET S = 0
260 FOR K = 1 TO N
270 LET S = S + A(K)
280 NEXT K
290 LET V = S / N
300 RETURN
400 REM -------------------------------------------------------------
410 REM COMPUTE STANDARD DEVIATION OF A.
420 REM INPUT: A, N, V
430 REM OUTPUT: D
440 REM -------------------------------------------------------------
450 LET S = 0
460 FOR L = 1 TO N
470 LET S = S + (A(L) - V) + 2
480 NEXT L
490 LET D = SQR(S / N)
500 RETURN
999 END
