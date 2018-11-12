;**********************************************************
; Stage3.asm
;   A basic 32 bit binary kernel running
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
; nasm -f bin Stage3.asm -o Stage3.bin -l Stage3.lst
;**********************************************************

[bits  32]                              ; 32 bit code
    ORG   100000h                       ; Kernel starts at 1 MB
    JMP   Stage3                        ; jump to entry point

;--------------------------------------------------------------------------------------------------
; Video Routines
;--------------------------------------------------------------------------------------------------

;------------------------------------------
; Routine to calculate video memory address
;   represented by the given Row,Col
;------------------------------------------
CalcVideoAddr:
    XOR   EAX,EAX                       ; Row calculation
    MOV   AL,[Row]                      ;  row
    DEC   EAX                           ;  minus 1
    MOV   EDX,160                       ;  times
    MUL   EDX                           ;  160
    PUSH  EAX                           ;  save it
    XOR   EAX,EAX                       ; Col calculation
    MOV   AL,[Col]                      ;  col
    MOV   EDX,2                         ;  times
    MUL   EDX                           ;  2
    SUB   EAX,EDX                       ;  minus 2
    POP   EDX                           ; Add col calculation
    ADD   EAX,EDX                       ;  to row calculation
    ADD   EAX,VidMem                    ;  plus VidMem
    MOV   EDI,EAX                       ; save in EDI
    RET

;------------------------------
; Put a character on the screen
; EDI = address in video memory
;------------------------------
PutChar:
    ;-------------------
    ; Watch for new line
    ;-------------------
    CMP   BL,0Ah                        ; is it a newline character?
    JE    PutChar1                      ; yep--go to next row

    ;------------------
    ; Print a character
    ;------------------
    MOV   DL,BL                         ; Get character
    MOV   DH,ChAttrib                   ; the character attribute
    MOV   WORD [EDI],DX                 ; write to video display

    ;---------------------
    ; Update next position
    ;---------------------
    INC   BYTE [Col]                    ; go to next character
    JMP   PutChar2                      ; we're done

    ;---------------
    ; Go to next row
    ;---------------
  PutChar1:
    MOV BYTE [Col],1                    ; go back to col 1
    INC BYTE [Row]                      ; go to next row

    ;---------------------------
    ; Restore registers & return
    ;---------------------------
 PutChar2:
    RET

;---------------------------------
; Print a null terminated string
; EBX = address of string to print
;---------------------------------
PutStr:
    ; Save registers
    PUSHA                               ; save registers
    XOR   ECX,ECX                       ; clear ECX
    PUSH  EBX                           ; copy the string address in EBX
    POP   ESI                           ;  to ESI
    MOV   CX,[ESI]                      ; grab string length using ESI, stuff it into CX
    SUB   CX,2                          ; subtract out 2 bytes for the length field
    ADD   ESI,2                         ; bump past the length field to the beginning of string
PutStr1:
    MOV   BL,BYTE [ESI]                 ; get next character
    CALL  CalcVideoAddr                 ; calculate video address
    CALL  PutChar                       ; print it out
    INC   ESI                           ; go to next character
    LOOP  PutStr1
    ; Update hardware cursor
    ; Its more efficiant to update the cursor after displaying
    ; the complete string because direct VGA is slow
    MOV   BH,BYTE [Row]                 ; BH = row
    MOV   BL,BYTE [Col]                 ; BL = col
    DEC   BH                            ; BH-- (this works, but why??, MoveCursor might need work)
    CALL  MovCursor                     ; update cursor
    POPA                                ; restore registers, and return
    RET

;-----------------------
; Update hardware cursor
; bh = Y pos
; bl = x pos
;-----------------------
MovCursor:
    PUSHA                               ; save registers (aren't you getting tired of this comment?)

    ; Get current position
    ; Here, Col and Row are relitave to the current position on screen, not in memory.
    ; That is, we don't need to worry about the byte alignment we do when displaying characters,
    ; so just follow the forumla: location = Col + Row * Cols
    XOR   EAX,EAX
    MOV   ECX,Cols
    MOV   AL,BH                         ; get y pos
    MUL   ECX                           ; multiply y*Cols
    ADD   AL,BL                         ; Now add x
    MOV   EBX,EAX

    ; Set low byte index to VGA register
    MOV   AL,0Fh
    MOV   DX,03D4h
    OUT   DX,AL

    MOV   AL,BL
    MOV   DX,03D5h
    OUT   DX,AL                         ; low byte

    ; Set high byte index to VGA register
    XOR   EAX,EAX

    MOV   AL,0Eh
    MOV   DX,03D4h
    OUT   DX,AL

    MOV   AL,BH
    MOV   DX,03D5h
    OUT   DX,AL                         ; high byte

    POPA
    RET

;-------------
; Clear Screen
;-------------
ClrSrc:
    PUSHA
    CLD
    MOV   EDI,VidMem
    MOV   CX,2000
    MOV   AH,ChAttrib
    MOV   AL,' '
    REP   STOSW
    MOV   BYTE [Col],1
    MOV   BYTE [Row],1
    POPA
    RET

;--------------------------------------------------------------------------------------------------
; Stage3 - Our Kernel!
;--------------------------------------------------------------------------------------------------
Stage3:
    ;--------------
    ; Set registers
    ;--------------
    MOV   AX,10h                        ; set data segments to data selector (10h)
    MOV   DS,AX
    MOV   SS,AX
    MOV   ES,AX
    MOV   ESP,90000h                    ; stack begins from 90000h

    ;-------------------------------
    ; Clear screen and print success
    ;-------------------------------
    CALL  ClrSrc

    MOV   BYTE [Col],1
    MOV   BYTE [Row],10
    MOV   EBX,Msg1
    CALL  PutStr

    MOV   EBX,NewLine
    CALL  PutStr

    MOV   EBX,Msg2
    CALL  PutStr

    ;---------------
    ; Stop execution
    ;---------------
    CLI
    HLT

;--------------------------------------------------------------------------------------------------
; Working Storage
;--------------------------------------------------------------------------------------------------
%macro String 2
%1          DW  %%EndStr-%1
            DB  %2
%%EndStr:
%endmacro
String  Msg1,"------   MyOs v0.1.1   -----"
String  Msg2,"------  32 Bit Kernel  -----"
String  NewLine,0x0A

ChAttrib    EQU 63                      ; character attribute (White text on light blue background)
Cols        EQU 80                      ; width and height of screen
Row         DB  0                       ; Row (1-25)
Col         DB  0                       ; Col (1-80)
VidMem      EQU 0B8000h                 ; video memory