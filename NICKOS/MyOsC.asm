; A boot sector that enters 32- bit protected mode.
[ org 0 x7c00 ]

    mov   bp,0x9000                     ; Set the stack.
    mov   sp,bp
    mov   bx,MSG_REAL_MODE
    call  print_string
    call  switch_to_pm                  ; Note that we never return from here.
    jmp   $

;--------------------------------------------
; % include "../ print / print_string.asm " -
; -------------------------------------------
print_string:
    mov   ah,0x0e

loop:
    mov   al,[bx]
    cmp   al,0
    je    out
    int   0x10
    add   bx,0x01
    jmp   loop

out:
    mov   al,' '
    int   0x10
    ret

;------------------------
; % include " gdt.asm " -
; -----------------------
; GDT tabel
gdt_start:

;NULL segment
gdt_null:
    dd    0x0
    dd    0x0

gdt_code:
  ; base =0x0 , limit =0 xfffff ,
  ; 1st  flags : ( present )     1 ( privilege )      00 ( descriptor type ) 1                -> 1001 b
  ; type flags : ( code )        1 ( conforming )      0 ( readable )        1 ( accessed ) 0 -> 1010 b
  ; 2nd  flags : ( granularity ) 1 (32 - bit default ) 1 (64 - bit seg )     0 ( AVL )      0 -> 1100 b
    dw    0xffff                        ; Limit ( bits  0 -15)
    dw    0x0                           ; Base  ( bits  0 -15)
    db    0x0                           ; Base  ( bits 16 -23)
    db    10011010b                     ; 1st flags , type flags
    db    11001111b                     ; 2nd flags , Limit ( bits 16 -19)
    db    0x0                           ; Base  ( bits 24 -31)

gdt_data:
    ; Same as code segment except for the type flags :
    ; type flags : ( code ) 0 ( expand down ) 0 ( writable ) 1 ( accessed ) 0                 -> 0010 b
    dw    0xffff                        ; Limit ( bits  0 -15)
    dw    0x0                           ; Base  ( bits  0 -15)
    db    0x0                           ; Base  ( bits 16 -23)
    db    10010010b                     ; 1st flags , type flags
    db    11001111b                     ; 2nd flags , Limit ( bits 16 -19)
    db    0x0                           ; Base  ( bits 24 -31)

gdt_end:                                ; The reason for putting a label at the end of the
                                        ;   GDT is so we can have the assembler calculate
                                        ;   the size of the GDT for the GDT decriptor ( below )

gdt_descriptor:
    dw    gdt_end - gdt_start - 1       ; Size of our GDT , always less one
                                        ;   of the true size
    dd    gdt_start                     ; Start address of our GDT

; Define some handy constants for the GDT segment descriptor offsets , which
; are what segment registers must contain when in protected mode. For example ,
; when we set DS = 0 x10 in PM , the CPU knows that we mean it to use the
; segment described at offset 0 x10 ( i.e. 16 bytes ) in our GDT , which in our
; case is the DATA segment (0 x0 -> NULL ; 0x08 -> CODE ; 0 x10 -> DATA )
CODE_SEG  equ gdt_code - gdt_start
DATA_SEG  equ gdt_data - gdt_start

; ---------------------------------
% include " print_string_pm.asm " -
; ---------------------------------
[bits 32]

VIDEO_MEMORY   equ 0xb8000
WHITE_ON_BLACK equ 0x0f

; data in ebx register
print_string_pm:
    pusha
    mov   edx,VIDEO_MEMORY

print_string_pm_loop:
    mov   al,[ebx]
    mov   ah,WHITE_ON_BLACK

    cmp   al,0
    je    done

    mov   [edx],ax

    add   edx,2
    add   ebx,1
    jmp   print_string_pm_loop

done:
    popa
    ret

; --------------------------------
; % include " switch_to_pm.asm " -
;---------------------------------
[bits 16]

switch_to_pm:
    cli                                 ; We must switch of interrupts until we have
                                        ;   set-up the protected mode interrupt vector
                                        ;   otherwise interrupts will run riot.
    lgdt  [gdt_descriptor]

    mov   eax,cr0                       ; See we are using 32 register (eax) here in 16 bit, we can do that
    or    eax,0x1
    mov   cr0,eax

    jmp   CODE_SEG:init_pm

[bits 32]
init_pm:
    mov   ax,DATA_SEG                   ; Now in PM , our old segments are meaningless ,
    mov   ds,ax                         ; so we point our segment registers to the
    mov   ss,ax                         ; data selector we defined in our GDT
    mov   es,ax
    mov   fs,ax
    mov   gs,ax

    mov   ebp,0x90000
    mov   esp,ebp

    call  BEGIN_PM

; -----------------------
; 32-bit Protected Mode -
; -----------------------
[ bits 32]
; This is where we arrive after switching to and initialising protected mode.
BEGIN_PM :
    mov   ebx,MSG_PROT_MODE
    call  print_string_pm               ; Use our 32- bit print routine.
    jmp   $                             ; Hang.

; ------------------
; Global variables -
; ------------------
MSG_REAL_MODE db " Started in 16- bit Real Mode ", 0
MSG_PROT_MODE db " Successfully landed in 32- bit Protected Mode ", 0

; --------------------
; Bootsector padding -
; --------------------
times 510 -($-$$) db 0
dw 0 xaa55