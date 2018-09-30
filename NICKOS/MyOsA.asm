;
; Boot - Hello
;
.386P
CSEG SEGMENT WORD 'Code' USE16
     ASSUME CS:CSEG, DS:CSEG, ES:NOTHING
ORG 0H

    mov   ah,0Eh
    mov   al,'H'
    int   010h
    mov   al,'e'
    int   010h
    mov   al,'l'
    int   010h
    mov   al,'l'
    int   010h
    mov   al,'o'
    int   010h

    jmp   $

    db    486 DUP(0)

    dw    0aa55h

CSEG      ENDS    
          END