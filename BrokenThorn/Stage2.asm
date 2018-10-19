;**********************************************************
; Stage2.asm
;   Stage2 Bootloader
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
; nasm -f bin Stage2.asm -o KRNLDR.SYS
;**********************************************************

[bits 16]

; Remember the memory map-- 0x500 through 0x7bff is unused above the BIOS data area.
; We are loaded at 0x500 (0x50:0)

org 0x500

    jmp   main                          ; go to start

;*******************************************************
; Preprocessor directives
;*******************************************************

;------------------------------------------------------------------------------------------------------------------------------------------
;%include "stdio.inc"                    ; basic i/o routines
;------------------------------------------------------------------------------------------------------------------------------------------

;**********************************************************
; Stdio.inc
;   Input/Output routines
;
;   Included in Stage2.asm Stage3.asm
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
;**********************************************************

%ifndef __STDIO_INC_67343546FDCC56AAB872_INCLUDED__
%define __STDIO_INC_67343546FDCC56AAB872_INCLUDED__


;==========================================================
;
;  16 Bit Real Mode Routines
;==========================================================


;************************************************;
; Puts16 ()
;   -Prints a null terminated string
; DS=>SI: 0 terminated string
;************************************************;

[bits 16]

Puts16:
    pusha                               ; save registers
  .Loop1:
    lodsb                               ; load next byte from string from SI to AL
    or    al,al                         ; Does AL=0?
    jz    Puts16Done                    ; Yep, null terminator found-bail out
    mov   ah,0eh                        ; Nope-Print the character
    int   10h                           ; invoke BIOS
    jmp   .Loop1                        ; Repeat until null terminator found
Puts16Done:
    popa                                ; restore registers
    ret                                 ; we are done, so return


;==========================================================
;
;  32 Bit Protected Mode Routines
;==========================================================

[bits 32]

%define   VIDMEM      0xB8000           ; video memory
%define   COLS        80                ; width and height of screen
%define   LINES       25
%define   CHAR_ATTRIB 63                ; character attribute (White text on light blue background)

_CurX db 0                              ; current x/y location
_CurY db 0

;**************************************************;
; Putch32 ()
;   - Prints a character to screen
; BL => Character to print
;**************************************************;

Putch32:
    pusha                               ; save registers
    mov   edi,VIDMEM                    ; get pointer to video memory

    ;-------------------------------;
    ;   Get current position  ;
    ;-------------------------------;

    xor   eax,eax                       ; clear eax

    ;--------------------------------
    ; Remember: currentPos = x + y * COLS! x and y are in _CurX and _CurY.
    ; Because there are two bytes per character, COLS=number of characters in a line.
    ; We have to multiply this by 2 to get number of bytes per line. This is the screen width,
    ; so multiply screen with * _CurY to get current line
    ;--------------------------------

    mov   ecx,COLS*2                    ; Mode 7 has 2 bytes per char, so its COLS*2 bytes per line
    mov   al,byte [_CurY]               ; get y pos
    mul   ecx                           ; multiply y*COLS
    push  eax                           ; save eax--the multiplication

    ;--------------------------------
    ; Now y * screen width is in eax. Now, just add _CurX. But, again remember that _CurX is relative
    ; to the current character count, not byte count. Because there are two bytes per character, we
    ; have to multiply _CurX by 2 first, then add it to our screen width * y.
    ;--------------------------------

    mov   al,byte [_CurX]               ; multiply _CurX by 2 because it is 2 bytes per char
    mov   cl,2
    mul   cl
    pop   ecx                           ; pop y*COLS result
    add   eax,ecx

    ;-------------------------------
    ; Now eax contains the offset address to draw the character at, so just add it to the base address
    ; of video memory (Stored in edi)
    ;-------------------------------

    xor ecx,ecx
    add edi,eax                         ; add it to the base address

    ;-------------------------------;
    ;   Watch for new line          ;
    ;-------------------------------;

    cmp   bl,0x0A                       ; is it a newline character?
    je    .Row                          ; yep--go to next row

    ;-------------------------------;
    ;   Print a character           ;
    ;-------------------------------;

    mov   dl,bl                         ; Get character
    mov   dh,CHAR_ATTRIB                ; the character attribute
    mov   word [edi],dx                 ; write to video display

    ;-------------------------------;
    ;   Update next position        ;
    ;-------------------------------;

    inc   byte [_CurX]                    ; go to next character
  ; cmp   byte [_CurX], COLS              ; are we at the end of the line?
  ; je    .Row                            ; yep-go to next row
    jmp   .done                           ; nope, bail out

    ;-------------------------------;
    ;   Go to next row              ;
    ;-------------------------------;

  .Row:
    mov byte [_CurX],0                  ; go back to col 0
    inc byte [_CurY]                    ; go to next row

    ;-------------------------------;
    ;   Restore registers & return  ;
    ;-------------------------------;

  .done:
    popa                                ; restore registers and return
    ret

;**************************************************;
; Puts32 ()
;   - Prints a null terminated string
; parm\ EBX = address of string to print
;**************************************************;

Puts32:
    ;-------------------------------;
    ;   Store registers             ;
    ;-------------------------------;

    pusha                               ; save registers
    push  ebx                           ; copy the string address
    pop   edi

  .loop:

    ;-------------------------------;
    ;   Get character               ;
    ;-------------------------------;

    mov   bl,byte [edi]                 ; get next character
    cmp   bl,0                          ; is it 0 (Null terminator)?
    je    .done                         ; yep-bail out

    ;-------------------------------;
    ;   Print the character         ;
    ;-------------------------------;

    call  Putch32                       ; (Routine is local) Nope-print it out

    ;-------------------------------;
    ;   Go to next character        ;
    ;-------------------------------;

    inc   edi                           ; go to next character
    jmp   .loop

  .done:

    ;-------------------------------;
    ;   Update hardware cursor      ;
    ;-------------------------------;

    ; Its more efficiant to update the cursor after displaying
    ; the complete string because direct VGA is slow

    mov   bh,byte [_CurY]               ; get current position
    mov   bl,byte [_CurX]
    call  MovCur                        ; (Routine is local) update cursor

    popa                                ; restore registers, and return
    ret

;**************************************************;
; MoveCur ()
;   - Update hardware cursor
; parm/ bh = Y pos
; parm/ bl = x pos
;**************************************************;

[bits 32]

MovCur:
    pusha                               ; save registers (aren't you getting tired of this comment?)

    ;-------------------------------;
    ;   Get current position        ;
    ;-------------------------------;

    ; Here, _CurX and _CurY are relitave to the current position on screen, not in memory.
    ; That is, we don't need to worry about the byte alignment we do when displaying characters,
    ; so just follow the forumla: location = _CurX + _CurY * COLS

    xor   eax,eax
    mov   ecx,COLS
    mov   al,bh                         ; get y pos
    mul   ecx                           ; multiply y*COLS
    add   al,bl                         ; Now add x
    mov   ebx,eax

    ;--------------------------------------;
    ;   Set low byte index to VGA register ;
    ;--------------------------------------;

    mov   al,0x0f
    mov   dx,0x03D4
    out   dx,al

    mov   al,bl
    mov   dx,0x03D5
    out   dx,al                         ; low byte

    ;---------------------------------------;
    ;   Set high byte index to VGA register ;
    ;---------------------------------------;

    xor   eax,eax

    mov   al,0x0e
    mov   dx,0x03D4
    out   dx,al

    mov   al,bh
    mov   dx,0x03D5
    out   dx,al                         ; high byte

    popa
    ret

;**************************************************;
; ClrScr32 ()
;   - Clears screen
;**************************************************;

[bits 32]

ClrScr32:
    pusha
    cld
    mov   edi,VIDMEM
    mov   cx,2000
    mov   ah,CHAR_ATTRIB
    mov   al,' '
    rep   stosw
    mov   byte [_CurX],0
    mov   byte [_CurY],0
    popa
    ret

;**************************************************;
; GotoXY ()
;   - Set current X/Y location
; parm\ AL=X position
; parm\ AH=Y position
;**************************************************;

[bits 32]

GotoXY:
    pusha
    mov [_CurX],al                      ; just set the current position
    mov [_CurY],ah
    popa
    ret

%endif ;__STDIO_INC_67343546FDCC56AAB872_INCLUDED__

;------------------------------------------------------------------------------------------------------------------------------------------
; end of %include "stdio.inc"                    ; basic i/o routines
;------------------------------------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------------------------------------
;%include "Gdt.inc"                      ; Gdt routines
;------------------------------------------------------------------------------------------------------------------------------------------

;**********************************************************
;	Gdt.inc
;		GDT Routines
;
;   Included in Stage2.asm
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
;**********************************************************

%ifndef __GDT_INC_67343546FDCC56AAB872_INCLUDED__
%define __GDT_INC_67343546FDCC56AAB872_INCLUDED__

[bits 16]

;*******************************************
; Install our GDT
;*******************************************

InstallGDT:                             ; Stage2.asm (65)
    cli                                 ; clear interrupts
    pusha                               ; save registers
    lgdt  [toc]                         ; load GDT into GDTR
    sti                                 ; enable interrupts
    popa                                ; restore registers
    ret                                 ; All done!

;*******************************************
; Global Descriptor Table (GDT)
;*******************************************

gdt_data:                               ; Only referenced in this module
    dd    0                             ; null descriptor
    dd    0

; gdt code:                             ; code descriptor
    dw  0FFFFh                          ; limit low
    dw  0                               ; base low
    db  0                               ; base middle
    db  10011010b                       ; access
    db  11001111b                       ; granularity
    db  0                               ; base high

; gdt data:                             ; data descriptor
    dw  0FFFFh                          ; limit low (Same as code)10:56 AM 7/8/2007
    dw  0                               ; base low
    db  0                               ; base middle
    db  10010010b                       ; access
    db  11001111b                       ; granularity
    db  0                               ; base high

end_of_gdt:                             ; Only referenced in this module
toc:
    dw end_of_gdt - gdt_data - 1        ; limit (Size of GDT)
    dd gdt_data                         ; base of GDT

; give the descriptor offsets names

%define NULL_DESC 0                     ; Not used anywhere!!!
%define CODE_DESC 0x8                   ; Stage2.asm (115,158)
%define DATA_DESC 0x10                  ; Stage2.asm (132)

%endif ;__GDT_INC_67343546FDCC56AAB872_INCLUDED__

;------------------------------------------------------------------------------------------------------------------------------------------
; end of %include "Gdt.inc"                      ; Gdt routines
;------------------------------------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------------------------------------
;%include "A20.inc"                      ; A20 enabling
;------------------------------------------------------------------------------------------------------------------------------------------

;**********************************************************
;	A20.inc
;		Enable A20 address line
;
;   Included in Stage2.asm
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
;**********************************************************

%ifndef __A20_INC_67343546FDCC56AAB872_INCLUDED__
%define __A20_INC_67343546FDCC56AAB872_INCLUDED__

[bits 16]

;----------------------------------------------
; Enables a20 line through keyboard controller
;----------------------------------------------
EnableA20_KKbrd:                        ; NEVER CALLED!!!
    cli
    push  ax
    mov   al,0xdd                       ; send enable a20 address line command to controller
    out   0x64,al
    pop   ax
    ret

;--------------------------------------------
; Enables a20 line through output port
;--------------------------------------------
EnableA20_KKbrd_Out:                    ; Stage2.asm"(71)
    cli
    pusha
  
    call  wait_input                    ; (Routine is local)
    mov   al,0xAD
    out   0x64,al                       ; disable keyboard
    call  wait_input                    ; (Routine is local)

    mov   al,0xD0
    out   0x64,al                       ; tell controller to read output port
    call  wait_output                   ; (Routine is local)

    in    al,0x60
    push  eax                           ; get output port data and store it
    call  wait_input                    ; (Routine is local)

    mov   al,0xD1
    out   0x64,al                       ; tell controller to write output port
    call  wait_input                    ; (Routine is local)

    pop   eax
    or    al,2                          ; set bit 1 (enable a20)
    out   0x60,al                       ; write out data back to the output port

    call  wait_input                    ; (Routine is local)
    mov   al,0xAE                       ; enable keyboard
    out   0x64,al

    call  wait_input                    ; (Routine is local)
    popa
    sti
    ret

; wait for input buffer to be clear

wait_input:                             ; called only in this module
    in    al,0x64
    test  al,2
    jnz   wait_input
    ret

; wait for output buffer to be clear

wait_output:                            ; called only in this module
    in    al,0x64
    test  al,1
    jz    wait_output
    ret

;--------------------------------------
; Enables a20 line through bios
;--------------------------------------
EnableA20_Bios:                         ; NEVER CALLED!!!
    pusha
    mov   ax,0x2401
    int   0x15
    popa
    ret

;-------------------------------------------------
; Enables a20 line through system control port A
;-------------------------------------------------
EnableA20_SysControlA:                  ; NEVER CALLED!!!
    push  ax
    mov   al,2
    out   0x92,al
    pop   ax
    ret

%endif

;------------------------------------------------------------------------------------------------------------------------------------------
; end of %include "A20.inc"                      ; A20 enabling
;------------------------------------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------------------------------------
;%include "Fat12.inc"                    ; FAT12 driver. Kinda :)
;------------------------------------------------------------------------------------------------------------------------------------------
;**********************************************************
;	Fat12.inc
;		FAT12 filesystem for 3-1/2 floppies
;
;   Included in Stage2.asm
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
;**********************************************************

%ifndef __FAT12_INC_67343546FDCC56AAB872_INCLUDED__
%define __FAT12_INC_67343546FDCC56AAB872_INCLUDED__

[bits 16]

;------------------------------------------------------------------------------------------------------------------------------------------
;%include "Floppy16.inc"                 ; the erm.. floppy driver
;------------------------------------------------------------------------------------------------------------------------------------------

;**********************************************************
;	Floppy16.inc
;		Floppy drive interface routines
;
;   Included in FAT12.inc
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
;**********************************************************

%ifndef __FLOPPY16_INC_67343546FDCC56AAB872_INCLUDED__
%define __FLOPPY16_INC_67343546FDCC56AAB872_INCLUDED__

[bits 16]

bpbOEM                db "My OS   "
bpbBytesPerSector:    DW 512
bpbSectorsPerCluster: DB 1
bpbReservedSectors:   DW 1
bpbNumberOfFATs:      DB 2
bpbRootEntries:       DW 224
bpbTotalSectors:      DW 2880
bpbMedia:             DB 0xf0           ;; 0xF1
bpbSectorsPerFAT:     DW 9
bpbSectorsPerTrack:   DW 18
bpbHeadsPerCylinder:  DW 2
bpbHiddenSectors:     DD 0
bpbTotalSectorsBig:   DD 0
bsDriveNumber:        DB 0
bsUnused:             DB 0
bsExtBootSignature:   DB 0x29
bsSerialNumber:       DD 0xa0a1a2a3
bsVolumeLabel:        DB "MOS FLOPPY "
bsFileSystem:         DB "FAT12   "

datasector            dw 0x0000
cluster               dw 0x0000

absoluteSector        db 0x00
absoluteHead          db 0x00
absoluteTrack         db 0x00

;************************************************;
; Convert CHS to LBA
; LBA = (cluster - 2) * sectors per cluster
;************************************************;
ClusterLBA:                             ; Fat12.inc (177)
    sub   ax,0x0002                     ; zero base cluster number
    xor   cx,cx
    mov   cl,BYTE [bpbSectorsPerCluster] ; convert byte to word
    mul   cx
    add   ax,WORD [datasector]          ; base data sector
    ret

;************************************************;
; Convert LBA to CHS
; AX=>LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;
;************************************************;
LBACHS:                                 ; Local
    xor   dx,dx                         ; prepare dx:ax for operation
    div   WORD [bpbSectorsPerTrack]     ; calculate
    inc   dl                            ; adjust for sector 0
    mov   BYTE [absoluteSector],dl
    xor   dx,dx                         ; prepare dx:ax for operation
    div   WORD [bpbHeadsPerCylinder]    ; calculate
    mov   BYTE [absoluteHead],dl
    mov   BYTE [absoluteTrack],al
    ret

;************************************************;
; Reads a series of sectors
; CX=>Number of sectors to read
; AX=>Starting sector
; ES:EBX=>Buffer to read to
;************************************************;
ReadSectors:                            ; Fat12.inc (53,83,180)
  .MAIN:
    mov   di,0x0005                     ; five retries for error
  .SECTORLOOP:
    push  ax
    push  bx
    push  cx
    call  LBACHS                        ; (Routine is local) convert starting sector to CHS
    mov   ah,0x02                       ; BIOS read sector
    mov   al,0x01                       ; read one sector
    mov   ch,BYTE [absoluteTrack]       ; track
    mov   cl,BYTE [absoluteSector]      ; sector
    mov   dh,BYTE [absoluteHead]        ; head
    mov   dl,BYTE [bsDriveNumber]       ; drive
    int   0x13                          ; invoke BIOS
    jnc   .SUCCESS                      ; test for read error
    xor   ax,ax                         ; BIOS reset disk
    int   0x13                          ; invoke BIOS
    dec   di                            ; decrement error counter
    pop   cx
    pop   bx
    pop   ax
    jnz   .SECTORLOOP                   ; attempt to read again
    int   0x18
  .SUCCESS:
    pop   cx
    pop   bx
    pop   ax
    add   bx,WORD [bpbBytesPerSector]   ; queue next buffer
    inc   ax                            ; queue next sector
    loop  .MAIN                         ; read next sector
    ret

%endif    ;__FLOPPY16_INC_67343546FDCC56AAB872_INCLUDED__

;------------------------------------------------------------------------------------------------------------------------------------------
; end Floppy16.inc
;------------------------------------------------------------------------------------------------------------------------------------------

%define ROOT_OFFSET 0x2e00              ; Local
%define FAT_SEG     0x2c0               ; Local
%define ROOT_SEG    0x2e0               ; Local

;*******************************************
; Load Root Directory Table to 0x7e00
;*******************************************
LoadRoot:                               ; Stage2.asm (84)
    pusha                               ; store registers
    push  es

    ; compute size of root directory and store in "cx"

    xor   cx,cx                         ; clear registers
    xor   dx,dx
    mov   ax,32                         ; 32 byte directory entry
    mul   WORD [bpbRootEntries]         ; total size of directory
    div   WORD [bpbBytesPerSector]      ; sectors used by directory
    xchg  ax,cx                         ; move into AX

    ; compute location of root directory and store in "ax"

    mov   al,BYTE [bpbNumberOfFATs]     ; number of FATs
    mul   WORD [bpbSectorsPerFAT]       ; sectors used by FATs
    add   ax,WORD [bpbReservedSectors]
    mov   WORD [datasector],ax          ; base of root directory
    add   WORD [datasector],cx

    ; read root directory into 0x7e00

    push  word ROOT_SEG
    pop   es
    mov   bx, 0                         ; copy root dir
    call  ReadSectors                   ; (Routine is in Floppy16.inc) read in directory table
    pop   es
    popa                                ; restore registers and return
    ret

;*******************************************
; Loads FAT table to 0x7c00
;
; Parm/ ES:DI => Root Directory Table
;*******************************************
LoadFAT:                                ; (Routine is local)
    pusha                               ; store registers
    push  es

    ; compute size of FAT and store in "cx"

    xor   ax,ax
    mov   al,BYTE [bpbNumberOfFATs]     ; number of FATs
    mul   WORD [bpbSectorsPerFAT]       ; sectors used by FATs
    mov   cx,ax

    ; compute location of FAT and store in "ax"

    mov   ax,WORD [bpbReservedSectors]

    ; read FAT into memory (Overwrite our bootloader at 0x7c00)

    push  word FAT_SEG
    pop   es
    xor   bx, bx
    call  ReadSectors                   ; (Routine is in Flopppy16.inc)
    pop   es
    popa                                ; restore registers and return
    ret

;*******************************************
; Search for filename in root table
;
; parm/ DS:SI => File name
; ret/ AX => File index number in directory table. -1 if error
;*******************************************
FindFile:                               ; (Routine is local)
    push  cx                            ; store registers
    push  dx
    push  bx
    mov   bx,si                         ; copy filename for later

    ; browse root directory for binary image

    mov   cx,WORD [bpbRootEntries]      ; load loop counter
    mov   di,ROOT_OFFSET                ; locate first root entry at 1 MB mark
    cld                                 ; clear direction flag

  .LOOP:
    push  cx
    mov   cx,11                         ; eleven character name. Image name is in SI
    mov   si,bx                         ; image name is in BX
    push  di
    rep   cmpsb                         ; test for entry match
    pop   di
    je    .Found
    pop   cx
    add   di,32                         ; queue next directory entry
    loop  .LOOP

  .NotFound:
    pop   bx                            ; restore registers and return
    pop   dx
    pop   cx
    mov   ax,-1                         ; set error code
    ret

  .Found:
    pop   ax                            ; return value into AX contains entry of file
    pop   bx                            ; restore registers and return
    pop   dx
    pop   cx
    ret

;*******************************************
; Load file
; parm/ ES:SI => File to load
; parm/ EBX:BP => Buffer to load file to
; ret/ AX => -1 on error, 0 on success
; ret/ CX => number of sectors read
;*******************************************
LoadFile:                               ; Stage2.asm (93)
    xor ecx,ecx                         ; size of file in sectors
    push  ecx

  .FIND_FILE:
    push  bx                            ; BX=>BP points to buffer to write to; store it for later
    push  bp
    call  FindFile                      ; (Routine is local) find our file. ES:SI contains our filename
    cmp   ax,-1
    jne   .LOAD_IMAGE_PRE
    pop   bp
    pop   bx
    pop   ecx
    mov   ax,-1
    ret

  .LOAD_IMAGE_PRE:
    sub edi,ROOT_OFFSET
    sub eax,ROOT_OFFSET

    ; get starting cluster

    push  word ROOT_SEG                 ;root segment loc
    pop   es
    mov   dx,WORD [es:di + 0x001A]      ; DI points to file entry in root directory table. Refrence the table...
    mov   WORD [cluster],dx             ; file's first cluster
    pop   bx                            ; get location to write to so we dont screw up the stack
    pop   es
    push  bx                            ; store location for later again
    push  es
    call  LoadFAT                       ; (Routine is local)

  .LOAD_IMAGE:
    ; load the cluster

    mov   ax,WORD [cluster]             ; cluster to read
    pop   es                            ; bx:bp=es:bx
    pop   bx
    call  ClusterLBA                    ; (Routine is in Floppy16.inc)
    xor   cx,cx
    mov   cl,BYTE [bpbSectorsPerCluster]
    call  ReadSectors                   ; (Routine is in Floppy16.inc)
    pop   ecx
    inc   ecx                           ; add one more sector to counter
    push  ecx
    push  bx
    push  es
    mov   ax,FAT_SEG                    ;start reading from fat
    mov   es,ax
    xor   bx,bx

  ; get next cluster

    mov   ax,WORD [cluster]             ; identify current cluster
    mov   cx,ax                         ; copy current cluster
    mov   dx,ax
    shr   dx,0x0001                     ; divide by two
    add   cx,dx                         ; sum for (3/2)

    mov   bx,0                          ;location of fat in memory
    add   bx,cx
    mov   dx,WORD [es:bx]
    test  ax,0x0001                     ; test for odd or even cluster
    jnz   .ODD_CLUSTER

  .EVEN_CLUSTER:
    and   dx,0000111111111111b          ; take low 12 bits
    jmp   .DONE

  .ODD_CLUSTER:
    shr dx,0x0004                       ; take high 12 bits

  .DONE:
    mov   WORD [cluster],dx
    cmp   dx,0x0ff0                     ; test for end of file marker
    jb    .LOAD_IMAGE

  .SUCCESS:
    pop   es
    pop   bx
    pop   ecx
    xor   ax,ax
    ret

%endif    ;__FAT12_INC_67343546FDCC56AAB872_INCLUDED__

;------------------------------------------------------------------------------------------------------------------------------------------
; end of %include "Fat12.inc"                    ; FAT12 driver. Kinda :)
;------------------------------------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------------------------------------
;%include "Common.inc"                   ; Some constants
;------------------------------------------------------------------------------------------------------------------------------------------
;**********************************************************
;	Common.inc                            
;		Common Code
;
;   Included in Stage2.asm
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
;**********************************************************

%ifndef _COMMON_INC_INCLUDED
%define _COMMON_INC_INCLUDED

; where the kernel is to be loaded to in protected mode
%define IMAGE_PMODE_BASE 0x100000       ; Stage2.asm(150,158)

; where the kernel is to be loaded to in real mode
%define IMAGE_RMODE_BASE 0x3000         ; Stage2.asm(91,149)

; kernel name (Must be 11 bytes)
ImageName     db "KRNL    SYS"          ; Stage2.asm(92)

; size of kernel image in bytes
ImageSize     db 0                      ; Stage2.asm(94,143)

%endif

;------------------------------------------------------------------------------------------------------------------------------------------
; end of %include "Common.inc"                   ; Some constants
;------------------------------------------------------------------------------------------------------------------------------------------

;*******************************************************
; Data Section
;*******************************************************

LoadingMsg db 0x0D, 0x0A, "Searching for Operating System v4...", 0x00
msgFailure db 0x0D, 0x0A, "*** FATAL: MISSING OR CURRUPT KRNL.SYS. Press Any Key to Reboot", 0x0D, 0x0A, 0x0A, 0x00

;*******************************************************
; STAGE 2 ENTRY POINT
;
;   -Store BIOS information
;   -Load Kernel
;   -Install GDT; go into protected mode (pmode)
;   -Jump to Stage 3
;*******************************************************

main:
    ;-------------------------------;
    ;   Setup segments and stack  ;
    ;-------------------------------;

    cli                                 ; clear interrupts
    xor ax,ax                           ; null segments
    mov ds,ax
    mov es,ax
    mov ax,0x0                          ; stack begins at 0x9000-0xffff
    mov ss,ax
    mov sp,0xFFFF
    sti                                 ; enable interrupts

    ;-------------------------------;
    ;   Install our GDT   ;
    ;-------------------------------;

    call  InstallGDT                    ; (Routine is in Gdt.inc) install our GDT

    ;-------------------------------;
    ;   Enable A20      ;
    ;-------------------------------;

    call  EnableA20_KKbrd_Out           ; (Routine is in A20.inc)

    ;-------------------------------;
    ;   Print loading message ;
    ;-------------------------------;

    mov si, LoadingMsg
    call  Puts16                        ; (Routine is in Stdio.inc)
    mov   ah,0x00
    int   0x16                          ; await keypress

    ;-------------------------------;
    ; Initialize filesystem   ;
    ;-------------------------------;

    call  LoadRoot                      ; (Routine is in Fat12.inc) Load root directory table

    ;-------------------------------;
    ; Load Kernel     ;
    ;-------------------------------;

    mov   ebx,0                         ; BX:BP points to buffer to load to
    mov   bp,IMAGE_RMODE_BASE
    mov   si,ImageName                  ; our file to load
    call  LoadFile                      ; (Routine is in Fat12.inc) load our file
    mov   dword [ImageSize],ecx         ; save size of kernel
    cmp   ax,0                          ; Test for success
    je    EnterStage3                   ; yep--onto Stage 3!
    mov   si,msgFailure                 ; Nope--print error
    call  Puts16                        ; (Routine is in Stdio.inc)
    mov   ah,0
    int   0x16                          ; await keypress
    int   0x19                          ; warm boot computer
    cli                                 ; If we get here, something really went wong
    hlt

  ;-------------------------------;
  ;   Go into pmode   ;
  ;-------------------------------;

EnterStage3:
    cli                                 ; clear interrupts
    mov eax,cr0                         ; set bit 0 in cr0--enter pmode
    or  eax,1
    mov cr0,eax

    jmp CODE_DESC:Stage3                ; far jump to fix CS. Remember that the code selector is 0x8!

  ; Note: Do NOT re-enable interrupts! Doing so will triple fault!
  ; We will fix this in Stage 3.

;******************************************************
; ENTRY POINT FOR STAGE 3
;******************************************************

[bits 32]

Stage3:

    ;-------------------------------;
    ;   Set registers   ;
    ;-------------------------------;

    mov ax,DATA_DESC                    ; set data segments to data selector (0x10)
    mov ds,ax
    mov ss,ax
    mov es,ax
    mov esp,90000h                      ; stack begins from 90000h

    ;-------------------------------;
    ; Copy kernel to 1MB    ;
    ;-------------------------------;

CopyImage:
    mov   eax,dword [ImageSize]
    movzx ebx,word [bpbBytesPerSector]
    mul   ebx
    mov   ebx,4
    div   ebx
    cld
    mov   esi,IMAGE_RMODE_BASE
    mov   edi,IMAGE_PMODE_BASE
    mov   ecx,eax
    rep   movsd                         ; copy image to its protected mode address

    ;---------------------------------------;
    ;   Execute Kernel      ;
    ;---------------------------------------;

    jmp   CODE_DESC:IMAGE_PMODE_BASE    ; jump to our kernel! Note: This assumes Kernel's entry point is at 1 MB

    ;---------------------------------------;
    ;   Stop execution      ;
    ;---------------------------------------;

    cli
    hlt