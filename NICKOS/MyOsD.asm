;------------------------------------------------------
; - Assemble using nasm myosd.asm -f bin -o myosd.bin -
;------------------------------------------------------
[org 0x7C00]
[bits 16]

    MOV   SI,MSG_REAL                   ; SI = address of 1st character of a string
    CALL  PrintStr                      ; Print it
    CALL  ProtMode                      ; We never come back from this call

; -------------------------------------------------------------------------------------------------------------------------------
;- Switch to 32-bit Protectd Mode                                                                                                      -
;--------------------------------------------------------------------------------------------------------------------------------
[bits 16]

ProtMode:
    CLI                                 ; Turn off interrupts until the interrupt vector table is set up
    LGDT  [GDT_Descriptor]              ; Loads the address of the GDT into the global descriptor table register (GDTR)
    MOV   EAX,CR0                       ; Setting the first bit of special register CR0
    OR    EAX,0x1                       ;   puts the CPU into
    MOV   CR0,EAX                       ;   32-bit protected mode
    JMP   CODE_SEG:ProtMode1            ; Far jump to force cpu to clear the pipeline so next instructions WILL be 32-bit mode

[bits 32]
ProtMode1:
    MOV   AX,DATA_SEG                   ; Now in PM, our old segments are meaningless,
    MOV   DS,AX                         ;  so we point our
    MOV   SS,AX                         ;   segment registers
    MOV   ES,AX                         ;    to the data
    MOV   FS,AX                         ;     selector we
    MOV   GS,AX                         ;      defined in our GDT

    MOV   EBP,0x90000                   ; Point our stack
    MOV   ESP,EBP                       ;  to somewhere safe

    CALL  ProtMode2                     ; We never come back from this call

;--------------------------------------------------------------------------------------------------------------------------------
;- Now in 32-bit Protected Mode                                                                                                 -
;--------------------------------------------------------------------------------------------------------------------------------
[bits 32]

ProtMode2:
    MOV   EBX,MSG_PROT                  ; EBX = address of 1st character of a string
    CALL  PrintStrPM                    ; Print it
    JMP   $                             ; Loop forever

;--------------------------------------------------------------------------------------------------------------------------------
;- Print String (16-bit using BIOS INT 10h)                                                                                     -
;--------------------------------------------------------------------------------------------------------------------------------
[bits 16]

PrintStr:
    LODSB                               ; Load byte at address DS:(E)SI into AL.
    OR    AL,AL                         ; Is AL = 0?
    JZ    PrintStr1                     ;   Yes, then we're done
    MOV   AH,0Eh                        ; Set Teletype output AL = Character, BH = Page Number, BL = Color (only in graphic mode)
    INT   10h                           ; BIOS Video Service
    JMP   PrintStr                      ; Loop
PrintStr1:
    RETN                                ; Return to caller

;--------------------------------------------------------------------------------------------------------------------------------
; Print String (32-bit Protected Mode writing directly to video memory 25 rows by 80 columns                                                          -
;--------------------------------------------------------------------------------------------------------------------------------
[bits 32]

PrintStrPM:
    PUSHA                               ; Save all registers
    MOV   EDX,VIDEO_MEMORY+ROW17_COL01  ; EDX = target address in video memory for character
    MOV   AH,WHITE_ON_PURPLE            ; AH = Attribute(foreground/background color)

PrintStrPM1:
    MOV   AL,[EBX]                      ; AL = character pointed to by EBX
    CMP   AL,0                          ; if AL(our character) = 0
    JE    PrintStrPM2                   ;  then we're done
    MOV   [EDX],AX                      ; Move our character and attribute to video memory
    ADD   EDX,2                         ; Bump EDX by 2 (1 for our character and 1 for our attribute)
    ADD   EBX,1                         ; Bump EBX by 1 (next character in our string)
    JMP   PrintStrPM1                   ; Loop

PrintStrPM2:
    POPA                                ; Restore all registers
    RET                                 ; Return to caller

;--------------------------------------------------------------------------------------------------------------------------------
;- Data Area                                                                                                                    -
;--------------------------------------------------------------------------------------------------------------------------------
[bits 32]

MSG_REAL  DB  "Started in 16-bit Real Mode v0.27", 0
MSG_PROT  DB  "Successfully landed in 32-bit Protected Mode v0.27! ", 0

;--------------------------------------------------------------------------------------------------------------------------------
;- Video Information                                                                                                            -
;--------------------------------------------------------------------------------------------------------------------------------
[bits 32]

VIDEO_MEMORY    EQU 0xB8000             ; Video memory starts at B8000
ROW17_COL01     EQU 2560                ; ((Row-1)*160)+((Col*2)-2) = ((17-1)*160)+((1*2)-2)

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

WHITE_ON_BLACK  EQU 0x0F                ; Background = 0(black)  Foreground = F(white)    white  letters on a black  background
BLACK_ON_YELLOW EQU 0xE0                ; Background = E(yellow) Foreground = 0(black)    black  letters on a yellow background
YELLOW_ON_BLACK EQU 0x0E                ; Background = 0(black)  Foreground = E(yellow)   yellow letters on a black  background
WHITE_ON_PURPLE EQU 0x5F                ; Background = 5(purple) Foreground = F(white)    white  letters on a purple background
PURPLE_ON_WHITE EQU 0xF5                ; Background = F(white)  Foreground = 5(purple)   purple letters on a white  background

;--------------------------------------------------------------------------------------------------------------------------------
;- Global Descriptor Table                                                                                                      -
;--------------------------------------------------------------------------------------------------------------------------------
[bits 32]

GDT_Start:

GDT_Null:
    ;NULL segment
    DD    0x0
    DD    0x0

GDT_Code:
    ; base =0x0 , limit =0 xfffff ,
    ; 1st  flags : ( present )1 ( privilege )00 ( descriptor type )1 -> 1001 b
    ; type flags : ( code )1 ( conforming )0 ( readable )1 ( accessed )0 -> 1010 b
    ; 2nd  flags : ( granularity )1 (32 - bit default )1 (64 - bit seg )0 ( AVL )0 -> 1100 b
    DW    0xffff          ; Limit ( bits  0 -15)
    DW    0x0             ; Base  ( bits  0 -15)
    DB    0x0             ; Base  ( bits 16 -23)
    DB    10011010b       ; 1st flags , type flags
    DB    11001111b       ; 2nd flags , Limit ( bits 16 -19)
    DB    0x0             ; Base ( bits 24 -31)

GDT_Data:
    ; Same as code segment except for the type flags :
    ; type flags : ( code )0 ( expand down )0 ( writable )1 ( accessed )0 -> 0010 b
    DW    0xffff          ; Limit ( bits  0 -15)
    DW    0x0             ; Base  ( bits  0 -15)
    DB    0x0             ; Base  ( bits 16 -23)
    DB    10010010b       ; 1st flags , type flags
    DB    11001111b       ; 2nd flags , Limit ( bits 16 -19)
    DB    0x0             ; Base ( bits 24 -31)

GDT_End:
    ; The reason for putting a label at the end of the
    ; GDT is so we can have the assembler calculate
    ; the size of the GDT for the GDT decriptor ( below )

GDT_Descriptor:
    DW    GDT_End - GDT_Start - 1       ; Size of our GDT , always less one of the true size
    DD    GDT_Start                     ; Start address of our GDT

; Define some handy constants for the GDT segment descriptor offsets , which
; are what segment registers must contain when in protected mode. For example ,
; when we set DS = 0 x10 in PM , the CPU knows that we mean it to use the
; segment described at offset 0 x10 ( i.e. 16 bytes ) in our GDT , which in our
; case is the DATA segment (0 x0 -> NULL ; 0x08 -> CODE ; 0 x10 -> DATA )

CODE_SEG EQU GDT_Code - GDT_Start
DATA_SEG EQU GDT_Data - GDT_Start

;--------------------------------------------------------------------------------------------------------------------------------
;- End of Boot Sector                                                                                                           -
;--------------------------------------------------------------------------------------------------------------------------------
[bits 32]

Filler    Times 510-($-$$) DB 0
BootSig   DW 0xAA55