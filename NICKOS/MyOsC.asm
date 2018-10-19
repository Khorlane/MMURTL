;------------------------------------------------------
; - Assemble using nasm myosc.asm -f bin -o myosc.bin -
;------------------------------------------------------
[org 0x7c00]

    MOV   BX,MSG_REAL
    CALL  PrintString

    CALL  SwitchToPM                  ; We never come back from this call

;--------------------------------
; % include "PrintString.asm " -
; -------------------------------
PrintString:
    MOV   ah,0x0e

Loop:
    MOV   AL,[BX]
    CMP   AL,0
    JE    Out
    INT   0x10
    ADD   BX,0x01
    JMP   Loop

Out:
    MOV   AL,' '
    INT   0x10
    RET
; ----------------
; end of include -
; ----------------

; ---------------------------------
;% include "PrintStringPM.asm " -
; ---------------------------------
[bits 32]

VIDEO_MEMORY    EQU 0xb8000
WHITE_ON_BLACK  EQU 0x0f

; data in EBX register
PrintStringPM:
    pusha
    MOV   edx,VIDEO_MEMORY

PrintStringPM_Loop:
    MOV   AL,[EBX]
    MOV   ah,WHITE_ON_BLACK

    CMP   AL,0
    JE    Done

    MOV   [edx],AX

    ADD   edx,2
    ADD   EBX,1
    JMP   PrintStringPM_Loop

Done:
    popa
    RET
; ----------------
; end of include -
; ----------------

;----------------------
; % include "GDT.asm" -
; ---------------------
; GDT table
GDT_Start:

;NULL segment
GDT_Null:
    DD    0x0
    DD    0x0

GDT_Code:
    ; base =0x0 , limit =0 xfffff ,
    ; 1st flags : ( present )1 ( privilege )00 ( descriptor type )1 -> 1001 b
    ; type flags : ( code )1 ( conforming )0 ( readable )1 ( accessed )0 -> 1010 b
    ; 2nd flags : ( granularity )1 (32 - bit default )1 (64 - bit seg )0 ( AVL )0 -> 1100 b
    DW    0xffff          ; Limit ( bits 0 -15)
    DW    0x0             ; Base ( bits 0 -15)
    DB    0x0             ; Base ( bits 16 -23)
    DB    10011010b       ; 1st flags , type flags
    DB    11001111b       ; 2nd flags , Limit ( bits 16 -19)
    DB    0x0             ; Base ( bits 24 -31)

GDT_Data:
    ; Same as code segment except for the type flags :
    ; type flags : ( code )0 ( expand down )0 ( writable )1 ( accessed )0 -> 0010 b
    DW    0xffff          ; Limit ( bits 0 -15)
    DW    0x0             ; Base ( bits 0 -15)
    DB    0x0             ; Base ( bits 16 -23)
    DB    10010010b       ; 1st flags , type flags
    DB    11001111b       ; 2nd flags , Limit ( bits 16 -19)
    DB    0x0             ; Base ( bits 24 -31)

GDT_End:  ; The reason for putting a label at the end of the
    ; GDT is so we can have the assembler calculate
    ; the size of the GDT for the GDT decriptor ( below )

GDT_Descriptor:
    DW    GDT_End - GDT_Start - 1       ; Size of our GDT , always less one
                                        ; of the true size
    DD    GDT_Start                     ; Start address of our GDT

; Define some handy constants for the GDT segment descriptor offsets , which
; are what segment registers must contain when in protected mode. For example ,
; when we set DS = 0 x10 in PM , the CPU knows that we mean it to use the
; segment described at offset 0 x10 ( i.e. 16 bytes ) in our GDT , which in our
; case is the DATA segment (0 x0 -> NULL ; 0x08 -> CODE ; 0 x10 -> DATA )

CODE_SEG EQU GDT_Code - GDT_Start
DATA_SEG EQU GDT_Data - GDT_Start

; ----------------
; end of include -
; ----------------

; ------------------------------
; % include "SwitchToPM.asm" -
;-------------------------------
[bits 16]
SwitchToPM:
    CLI                                 ; Turn off interrupts until the interrupt vector table is set up

    LGDT  [GDT_Descriptor]

    MOV   EAX,CR0                       ; see we are using 32 register (EAX) here in 16 bit, we can do that
    OR    EAX,0x1
    MOV   CR0,EAX

    JMP CODE_SEG:InitPM

[bits 32]
InitPM:
    MOV   AX,DATA_SEG                   ; Now in PM , our old segments are meaningless ,
    MOV   DS,AX                         ; so we point our segment registers to the
    MOV   SS,AX                         ; data selector we defined in our GDT
    MOV   ES,AX
    MOV   FS,AX
    MOV   GS,AX

    MOV   EBP,0x90000
    MOV   ESP,EBP

    CALL  BeginPM                       ; We never come back from this call
; ----------------
; end of include -
; ----------------

[bits 32]
BeginPM:
    MOV   EBX,MSG_PROT
    CALL  PrintStringPM

    JMP   $

MSG_REAL  DB  " Started in 16-bit RealMode v0.08", 0
MSG_PROT  DB  " Successfully landed in 32-bit Protected Mode! ", 0

Filler    Times 510-($-$$) DB 0
BootSig   DW 0xAA55