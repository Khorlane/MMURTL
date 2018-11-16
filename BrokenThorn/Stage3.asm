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

;---------------
;- Color Codes -
;---------------
;  0 0 Black
;  1 1 Blue
;  2 2 Green
;  3 3 Cyan
;  4 4 Red
;  5 5 Purple
;  6 6 Brown
;  7 7 Gray
;  8 8 Dark Gray
;  9 9 Light Blue
; 10 A Light Green
; 11 B Light Cyan
; 12 C Light Red
; 13 D Light Purple
; 14 E Yellow
; 15 F White
; Example 3F 
;         ^^
;         ||
;         ||- Foreground F = White
;         |-- Background 3 = Cyan

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
    PUSHA                               ; Save registers
    MOV   DL,[Char]                     ; DL = character
    CMP   DL,0Ah                        ; is it a newline character?
    JE    PutChar1                      ; yep--go to next row
    MOV   DH,[ColorAttr]                ; DH = attribute
    MOV   WORD [EDI],DX                 ; Move attribute and character to video display
    INC   BYTE [Col]                    ; Bump the column by 1
    JMP   PutChar2                      ; We're done
PutChar1:
    MOV BYTE [Col],1                    ; Newline, so go back to col 1
    INC BYTE [Row]                      ;  and bump row by 1
PutChar2:
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;---------------------------------
; Print a null terminated string
; EBX = address of string to print
;---------------------------------
PutStr:
    PUSHA                               ; Save registers
    XOR   ECX,ECX                       ; Clear ECX
    PUSH  EBX                           ; Copy the string address in EBX
    POP   ESI                           ;  to ESI
    MOV   CX,[ESI]                      ; Grab string length using ESI, stuff it into CX
    SUB   CX,2                          ; Subtract out 2 bytes for the length field
    ADD   ESI,2                         ; Bump past the length field to the beginning of string
PutStr1:
    MOV   BL,BYTE [ESI]                 ; Get next character
    MOV   [Char],BL                     ;  and save it
    CALL  CalcVideoAddr                 ; Calculate video address
    CALL  PutChar                       ; Print it out
    INC   ESI                           ; Bump ESI to next character
    LOOP  PutStr1                       ; Loop on CX
    CALL  MoveCursor                    ; Update cursor (do this once after displaying the string, more efficient)
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;-----------------------
; Update hardware cursor
;-----------------------
MoveCursor:
    PUSHA                               ; Save registers
    MOV   BH,BYTE [Row]                 ; BH = row
    MOV   BL,BYTE [Col]                 ; BL = col
    DEC   BH                            ; BH-- (Make row zero based)

    XOR   EAX,EAX                       ; Clear EAX
    MOV   ECX,TotCol                    ; ECX = TotCol
    MOV   AL,BH                         ; Row
    MUL   ECX                           ;  * TotCol
    ADD   AL,BL                         ;  + Col
    MOV   EBX,EAX                       ; Save result in EBX (BL,BH in particular)

    XOR   EAX,EAX                       ; Clear EAX
    MOV   DX,03D4h                      ; Set VGA port to  03D4h (Video controller register select)
    MOV   AL,0Fh                        ; Set VGA port-index 0Fh (cursor location low byte)
    OUT   DX,AL                         ; Write to the VGA port
    MOV   DX,03D5h                      ; Set VGA port to  03D5h (Video controller data)
    MOV   AL,BL                         ; Set low byte of calculated cursor position from above
    OUT   DX,AL                         ; Write to the VGA port

    XOR   EAX,EAX                       ; Clear EAX
    MOV   DX,03D4h                      ; Set VGA port to  03D4h (Video controller register select)
    MOV   AL,0Eh                        ; Set VGA port-index 0Fh (cursor location high byte)
    OUT   DX,AL                         ; Write to the VGA port
    MOV   DX,03D5h                      ; Set VGA port to  03D5h (Video controller data)
    MOV   AL,BH                         ; Set high byte of calculated cursor position from above
    OUT   DX,AL                         ; Write to the VGA port

    POPA                                ; Restore registers
    RET                                 ; Return to caller

;-------------
; Clear Screen
;-------------
ClrScr:
    PUSHA                               ; Save registers
    CLD                                 ; Clear DF Flag, REP STOSW increments EDI
    MOV   EDI,VidMem                    ; Set EDI to beginning of Video Memory
    MOV   CX,2000                       ; 2,000 'words' on the screen
    MOV   AH,[ColorAttr]                ; Set color attribute
    MOV   AL,' '                        ; We're going to 'blank' out the screen
    REP   STOSW                         ; Do it!
    MOV   BYTE [Col],1                  ; Set Col to 1
    MOV   BYTE [Row],1                  ; Set Row to 1
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;-------------------
;Set Color Attribute
;-------------------
SetColorAttr:
    PUSHA                               ; Save registers
    MOV   AL,[ColorBack]                ; Background color (e.g. 3) 
    SHL   AL,4                          ;  goes in highest 4 bits of AL
    MOV   BL,[ColorFore]                ; Foreground color in lowest 4 bits of BL (e.g. F)
    OR    EAX,EBX                       ; AL now has the combination of background and foreground (e.g. 3F)
    MOV   [ColorAttr],AL                ; Save result in ColorAttr
    POPA                                ; Restore registers
    RET                                 ; Return to caller

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
    MOV   BYTE [ColorBack],Black        ; Background color
    MOV   BYTE [ColorFore],Purple       ; Foreground colr
    CALL  SetColorAttr                  ; Set color
    CALL  ClrScr                        ; Clear screen

    MOV   BYTE [Row],10                 ; Row 10 
    MOV   BYTE [Col],1                  ; Col 1
    MOV   EBX,Msg1                      ; Put
    CALL  PutStr                        ;  Msg1
    MOV   EBX,NewLine                   ; Put
    CALL  PutStr                        ;  a New Line
    MOV   EBX,Msg2                      ; Put
    CALL  PutStr                        ;  Msg2

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
String  NewLine,0Ah

ColorBack   DB  0                       ; Background color (00h - 0Fh)
ColorFore   DB  0                       ; Foreground color (00h - 0Fh)
ColorAttr   DB  0                       ; Combination of background and foreground color (e.g. 3Fh 3=cyan background,F=white text)
Char        DB  0                       ; ASCII character
TotCol      EQU 80                      ; width and height of screen
Row         DB  0                       ; Row (1-25)
Col         DB  0                       ; Col (1-80)
VidMem      EQU 0B8000h                 ; video memory
Black       EQU 00h                     ; Black
Cyan        EQU 03h                     ; Cyan
Purple      EQU 05h                     ; Purple
White       EQU 0Fh                     ; White