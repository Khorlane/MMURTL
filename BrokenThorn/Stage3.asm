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
; 32 bit Video Routines
;--------------------------------------------------------------------------------------------------
[bits 32]
VIDMEM      EQU 0B8000h                 ; video memory
COLS        EQU 80                      ; width and height of screen
LINES       EQU 25
CHAR_ATTRIB EQU 63                      ; character attribute (White text on light blue background)

_CurX       DB  0                       ; current x/y location
_CurY       DB  0

;----------------------------
; Print a character to screen
; BL = Character to print
;----------------------------
[bits 32]
PutCh:
    PUSHA                               ; save registers
    MOV   EDI,VIDMEM                    ; get pointer to video memory

    ;---------------------
    ; Get current position
    ;---------------------
    XOR   EAX,EAX                       ; clear eax

    ;----------------------------------------------------
    ; Remember: currentPos = x + y * COLS!
    ; x and y are in _CurX and _CurY.
    ; Because there are two bytes per character,
    ; COLS=number of characters in a line.
    ; We have to multiply this by 2 to get number
    ; of bytes per line. This is the screen width,
    ; so multiply screen with * _CurY to get current line
    ;----------------------------------------------------
    MOV   ECX,COLS*2                    ; Mode 7 has 2 bytes per char, so its COLS*2 bytes per line
    MOV   AL,BYTE [_CurY]               ; get y pos
    MUL   ECX                           ; multiply y*COLS
    PUSH  EAX                           ; save eax--the multiplication

    ;-------------------------------------
    ; Now y * screen width is in eax.
    ; Now, just add _CurX. But, again
    ; remember that _CurX is relative
    ; to the current character count,
    ; not byte count. Because there are
    ; two bytes per character, we
    ; have to multiply _CurX by 2 first,
    ; then add it to our screen width * y.
    ;-------------------------------------
    MOV   AL,BYTE [_CurX]               ; multiply _CurX by 2 because it is 2 bytes per char
    MOV   CL,2
    MUL   CL
    POP   ECX                           ; pop y*COLS result
    ADD   EAX,ECX

    ;------------------------------------
    ; Now eax contains the offset address
    ; to draw the character at, so just
    ; add it to the base address
    ; of video memory (Stored in edi)
    ;------------------------------------
    XOR ECX,ECX
    ADD EDI,EAX                         ; add it to the base address

    ;-------------------
    ; Watch for new line
    ;-------------------
    CMP   BL,0Ah                        ; is it a newline character?
    JE    PutCh1                          ; yep--go to next row

    ;------------------
    ; Print a character
    ;------------------
    MOV   DL,BL                         ; Get character
    MOV   DH,CHAR_ATTRIB                ; the character attribute
    MOV   WORD [EDI],DX                 ; write to video display

    ;---------------------
    ; Update next position
    ;---------------------
    INC   BYTE [_CurX]                  ; go to next character
  ; CMP   BYTE [_CurX],COLS             ; are we at the end of the line?
  ; JE    PutCh1                        ; yep-go to next row
    JMP   PutCh2                        ; nope, bail out

    ;---------------
    ; Go to next row
    ;---------------
  PutCh1:
    MOV BYTE [_CurX],0                  ; go back to col 0
    INC BYTE [_CurY]                    ; go to next row

    ;---------------------------
    ; Restore registers & return
    ;---------------------------
 PutCh2:
    POPA                                ; restore registers and return
    RET

;---------------------------------
; Print a null terminated string
; EBX = address of string to print
;---------------------------------
[bits 32]
PutStr:
    ; Save registers
    PUSHA                               ; save registers
    PUSH  EBX                           ; copy the string address
    POP   EDI

PutStr1:
    ; Get character
    MOV   BL,BYTE [EDI]                 ; get next character
    CMP   BL,0                          ; is it 0 (Null terminator)?
    JE    PutStr2                       ; yep-bail out

    ; Print the character
    CALL  PutCh                       ; Nope-print it out

    ; Go to next character
    INC   EDI                           ; go to next character
    JMP   PutStr1

PutStr2:
    ; Update hardware cursor
    ; Its more efficiant to update the cursor after displaying
    ; the complete string because direct VGA is slow

    MOV   BH,BYTE [_CurY]               ; get current position
    MOV   BL,BYTE [_CurX]
    CALL  MovCursor                     ; update cursor

    POPA                                ; restore registers, and return
    RET

;-----------------------
; Update hardware cursor
; bh = Y pos
; bl = x pos
;-----------------------
[bits 32]
MovCursor:
    PUSHA                               ; save registers (aren't you getting tired of this comment?)

    ; Get current position
    ; Here, _CurX and _CurY are relitave to the current position on screen, not in memory.
    ; That is, we don't need to worry about the byte alignment we do when displaying characters,
    ; so just follow the forumla: location = _CurX + _CurY * COLS
    XOR   EAX,EAX
    MOV   ECX,COLS
    MOV   AL,BH                         ; get y pos
    MUL   ECX                           ; multiply y*COLS
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
[bits 32]
ClrScr32:
    PUSHA
    CLD
    MOV   EDI,VIDMEM
    MOV   CX,2000
    MOV   AH,CHAR_ATTRIB
    MOV   AL,' '
    REP   STOSW
    MOV   BYTE [_CurX],0
    MOV   BYTE [_CurY],0
    POPA
    RET

;--------------------------------------------------------------------------------------------------
; Stage3 - Our Kernel!
;--------------------------------------------------------------------------------------------------
Msg DB  0x0A, 0x0A, "                     ------    My Os v2     -----"
    DB  0x0A, 0x0A, "                     ------  32 Bit Kernel  -----", 0x0A, 0

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
    CALL  ClrScr32               
    MOV   EBX,Msg
    CALL  PutStr  

    ;---------------
    ; Stop execution
    ;---------------
    CLI
    HLT