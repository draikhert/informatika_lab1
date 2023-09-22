IDENTIFICATION DIVISION.
PROGRAM-ID. FibonacciToDecimal.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 Fibonacci-Input PIC 9(10).
01 Decimal-Output PIC 9(10).
01 Fibonacci-Sequence.
   05 Fibonacci-Array OCCURS 20 TIMES.
      10 Fibonacci-Value PIC 9(10).
01 Decimal-Result PIC 9(10).
01 Remainder PIC 9(10).
01 Fib-Index PIC 9(3) VALUE 2.

PROCEDURE DIVISION.
    DISPLAY "Enter a number in the Fibonacci number system: ".
    ACCEPT Fibonacci-Input.

    CALL 'FibonacciToDecimal' USING Fibonacci-Input
                                Fibonacci-Sequence
                                Decimal-Result.

    DISPLAY "In the decimal number system, it is: " Decimal-Result.

    STOP RUN.

    ENTRY 'FibonacciToDecimal' USING Fibonacci-Input
                                   Fibonacci-Sequence
                                   Decimal-Result.

    MOVE 0 TO Fibonacci-Sequence (1)
                   Fibonacci-Sequence (2).
    MOVE 0 TO Decimal-Result.
    MOVE 2 TO Fib-Index.

    PERFORM VARYING Fib-Index FROM 3 BY 1 UNTIL Decimal-Result >= Fibonacci-Input
        COMPUTE Fibonacci-Sequence (Fib-Index)
            = Fibonacci-Sequence (Fib-Index - 1)
            + Fibonacci-Sequence (Fib-Index - 2)
        ADD 1 TO Fib-Index
        MOVE Fibonacci-Sequence (Fib-Index) TO Decimal-Result
    END-PERFORM.

    MOVE SPACES TO Decimal-Result.

    PERFORM VARYING Fib-Index FROM Fib-Index BY -1 UNTIL Fib-Index < 2
        IF Decimal-Result >= Fibonacci-Sequence (Fib-Index)
            MOVE 1 TO Decimal-Result (Fib-Index)
            SUBTRACT Fibonacci-Sequence (Fib-Index) FROM Decimal-Result
        ELSE
            MOVE 0 TO Decimal-Result (Fib-Index)
        END-IF
    END-PERFORM.

    IF Decimal-Result >= 1
        MOVE 1 TO Decimal-Result (1)
    END-IF.

    COMPUTE Decimal-Output = FUNCTION NUMVAL (Decimal-Result)
    MOVE Decimal-Output TO Decimal-Result.

    EXIT PROGRAM.
