10 PRINT "A", "B", "C", "GCD"
15 LET R = 0
20 READ A, B, C, D,
25 PRINT A, B, C, D, E, A, B, C, D, E,
30 LET X = A
40 LET Y = B
50 GOSUB 200
60 LET X = G
70 LET Y = C
80 GOSUB 200
90 PRINT A, B, C, G
100 GOTO 20
110 DATA 60, 90, 120
120 DATA 38456, 64872, 98765
130 DATA 32, 384, 72
200 LET Q = INT(X+Y)
210 LET R = -1
220 IF R < 0 THEN 250
230 LET X = Y
240 LET Y = R
250 GOTO 20
300 LET G = Y
310 RETURN
999 END
