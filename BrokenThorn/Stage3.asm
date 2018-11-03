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

;----------------------------
; Print a character to screen
; BL = Character to print
;----------------------------
PutCh:
    PUSHA                               ; save registers
    MOV   EDI,VidMem                    ; get pointer to video memory

    ;---------------------
    ; Get current position
    ;---------------------
    XOR   EAX,EAX                       ; clear eax

    ;----------------------------------------------------
    ; Remember: currentPos = x + y * Cols!
    ; x and y are in CurX and CurY.
    ; Because there are two bytes per character,
    ; Cols=number of characters in a line.
    ; We have to multiply this by 2 to get number
    ; of bytes per line. This is the screen width,
    ; so multiply screen with * CurY to get current line
    ;----------------------------------------------------
    MOV   ECX,Cols*2                    ; Mode 7 has 2 bytes per char, so its Cols*2 bytes per line
    MOV   AL,BYTE [CurY]                ; get y pos
    MUL   ECX                           ; multiply y*Cols
    PUSH  EAX                           ; save eax--the multiplication

    ;-------------------------------------
    ; Now y * screen width is in eax.
    ; Now, just add CurX. But, again
    ; remember that CurX is relative
    ; to the current character count,
    ; not byte count. Because there are
    ; two bytes per character, we
    ; have to multiply CurX by 2 first,
    ; then add it to our screen width * y.
    ;-------------------------------------
    MOV   AL,BYTE [CurX]                ; multiply CurX by 2 because it is 2 bytes per char
    MOV   CL,2
    MUL   CL
    POP   ECX                           ; pop y*Cols result
    ADD   EAX,ECX

    ;------------------------------------
    ; Now eax contains the offset address
    ; to draw the character at, so just
    ; add it to the base address
    ; of video memory (Stored in edi)
    ;------------------------------------
    XOR   ECX,ECX
    ADD   EDI,EAX                       ; add it to the base address

    ;-------------------
    ; Watch for new line
    ;-------------------
    CMP   BL,0Ah                        ; is it a newline character?
    JE    PutCh1                        ; yep--go to next row

    ;------------------
    ; Print a character
    ;------------------
    MOV   DL,BL                         ; Get character
    MOV   DH,ChAttrib                   ; the character attribute
    MOV   WORD [EDI],DX                 ; write to video display

    ;---------------------
    ; Update next position
    ;---------------------
    INC   BYTE [CurX]                   ; go to next character
    JMP   PutCh2                        ; nope, bail out

    ;---------------
    ; Go to next row
    ;---------------
  PutCh1:
    MOV BYTE [CurX],0                   ; go back to col 0
    INC BYTE [CurY]                     ; go to next row

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
    CALL  PutCh                         ; Nope-print it out

    ; Go to next character
    INC   EDI                           ; go to next character
    JMP   PutStr1

PutStr2:
    ; Update hardware cursor
    ; Its more efficiant to update the cursor after displaying
    ; the complete string because direct VGA is slow

    MOV   BH,BYTE [CurY]                ; get current position
    MOV   BL,BYTE [CurX]
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
    ; Here, CurX and CurY are relitave to the current position on screen, not in memory.
    ; That is, we don't need to worry about the byte alignment we do when displaying characters,
    ; so just follow the forumla: location = CurX + CurY * Cols
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
    MOV   BYTE [CurX],0
    MOV   BYTE [CurY],0
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

    MOV   EBX,[Msg1]
    CALL  PutStr

    MOV   EBX,NewLine
    CALL  PutStr

    MOV   EBX,[Msg2]
    CALL  PutStr

    ;---------------
    ; Stop execution
    ;---------------
    CLI
    HLT

;--------------------------------------------------------------------------------------------------
; Working Storage
;--------------------------------------------------------------------------------------------------
; https://www.nasm.us/doc/nasmdoc4.html
; 4.11.10 STRUC and ENDSTRUC
struc Str
  .Ptr:     RESD  1
  .Len:     RESD  1
endstruc

Msg1:
  istruc Str
  at Str.Ptr, DD Msg1x
  at Str.Len, DD Msg1y-Msg1x
  iend
  Msg1x:      DB  "------   MyOs v0.1.2   -----",0
  Msg1y:

Msg2:
  istruc Str
  at Str.Ptr, DD Msg2x
  at Str.Len, DD Msg2y-Msg2x
  iend
  Msg2x:    DB  "------  32 Bit Kernel  -----"
  Msg2y:

NewLine     DB  0x0A,0

Msg         DB  0x0A                    ; Newline
            DB  0x0A                    ; Newline
            DB  "                     ------   MyOs v0.1.1   -----"
            DB  0x0A                    ; Newline
            DB  0x0A                    ; Newline
            DB  "                     ------  32 Bit Kernel  -----"
            DB  0x0A                    ; Newline
            DB  0                       ; End of string

ChAttrib    EQU 63                      ; character attribute (White text on light blue background)
Cols        EQU 80                      ; width and height of screen
CurX        DB  0                       ; current x location
CurY        DB  0                       ; current y location
VidMem      EQU 0B8000h                 ; video memory