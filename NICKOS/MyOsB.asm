.386P

CSEG    SEGMENT WORD 'Code' USE16
        ASSUME CS:CSEG, DS:CSEG, ES:Nothing

ORG 0h

    JMP   SHORT Bootup
    NOP

; The following are pointers to my IDT and GDT after my OS loads
; and are not part of the above boot sector structure.

IDTptr    DW 7FFh                       ;LIMIT 256 IDT Slots
          DD 0000h                      ;BASE (Linear)

GDTptr    DW 17FFh                      ;LIMIT 768 slots
          DD 0800h                      ;BASE (Linear)

; This is where we jump from those first 3 bytes

BootUp:

; Boot block's first instruction starts here after initial jump from beginning
    CLI                                 ;Clear interrupts

; Stick the stack at 98000h (an arbitrary location)
    MOV   AX,9000h
    MOV   SS,AX
    MOV   SP,8000h

; Move this boot sector UP to 90000h Linear.
    MOV  AX,09000h
    MOV  ES,AX
    XOR  DI,DI
    MOV  AX,7C0h
    MOV  DS,AX
    XOR  SI,SI
    MOV  CX,512
    REP  MOVSB

  ; Now we jump UP to where we moved it.
    MOV   AX, 09000h                    ;Segment
    PUSH  AX
    MOV   AX,34h                        ; Offset to new location <-------------
    PUSH  AX                            ;                                     |
    RETF                                ;                                     |
                                        ;                                     |
; Now set DS equal to ES which is 9000h ;                                     |
    PUSH  ES                            ; The address of this instruction and offset above must match
    POP   DS

    MOV   SI, OFFSET MsgLoad
    CALL  PutChars
    JMP   $

PutChars:
    LODSB
    OR    AL,AL
    JZ    SHORT Done
    MOV   AH,0Eh
    MOV   BX,0007
    INT   10h
    JMP   SHORT PutChars
Done:
  RETN

MsgLoad   DB 0Dh, 0Ah, 'Loading MMURTE', 00h

          DB 01A0h DUP(0)

BootSig   DW 0AA55h

CSEG    ENDS
        END