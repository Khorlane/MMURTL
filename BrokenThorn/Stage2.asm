;**********************************************************
; Stage2.asm
;   Stage2 Bootloader
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
; nasm -f bin Stage2.asm -o Stage2.bin -l Stage2.lst
;**********************************************************

; Remember the memory map-- 500h through 7BFFh is unused above the BIOS data area.
; We are loaded at 500h (50h:0h)
[bits 16]
    org   0500h
    jmp   Main                          ; go to start

;--------------------------------------------------------------------------------------------------
; Prints a null terminated string
; DS => SI: 0 terminated string
;--------------------------------------------------------------------------------------------------
[bits 16]
Puts16:
    pusha                               ; save registers
  .Loop1:
    lodsb                               ; load next byte from string from SI to AL
    or    al,al                         ; Does AL=0?
    jz    Puts16Done                    ; Yep, null terminator found-bail out
    mov   ah,0Eh                        ; Nope-Print the character
    int   10h                           ; invoke BIOS
    jmp   .Loop1                        ; Repeat until null terminator found
Puts16Done:
    popa                                ; restore registers
    ret                                 ; we are done, so return

;--------------------------------------------------------------------------------------------------
; Install our GDT
; Tutorial 8: Protected Mode
;--------------------------------------------------------------------------------------------------
[bits 16]
InstallGDT:
    cli                                 ; disable interrupts
    pusha                               ; save registers
    lgdt  [GDT2]                        ; load GDT into GDTR
    sti                                 ; enable interrupts
    popa                                ; restore registers
    ret                                 ; All done!

;--------------------------------------------------------------------------------------------------
; Enable A20 line through output port
;--------------------------------------------------------------------------------------------------
[bits 16]
EnableA20:
    cli                                 ; disable interrupts
    pusha

    call  WaitInput                     ; wait for keypress
    mov   al,0ADh
    out   64h,al                        ; disable keyboard
    call  WaitInput                    

    mov   al,0D0h
    out   64h,al                        ; tell controller to read output port
    call  WaitOutput                   

    in    al,60h
    push  eax                           ; get output port data and store it
    call  WaitInput                    

    mov   al,0D1h
    out   64h,al                        ; tell controller to write output port
    call  WaitInput                    

    pop   eax
    or    al,2                          ; set bit 1 (enable a20)
    out   60h,al                        ; write out data back to the output port

    call  WaitInput                    
    mov   al,0AEh                       ; enable keyboard
    out   64h,al

    call  WaitInput                     ; wait for keypress
    popa
    sti                                 ; enable interrupts
    ret

;------------------------------
; Helper routines for EnableA20
;------------------------------
WaitInput:
    in    al,64h                        ; wait for
    test  al,2                          ;  input buffer
    jnz   WaitInput                     ;   to clear
    ret

WaitOutput:
    in    al,64h                        ; wait for
    test  al,1                          ;  output buffer
    jz    WaitOutput                   ;   to clear
    ret

;--------------------------------------------------------------------------------------------------
; Floppy Driver Routines
;--------------------------------------------------------------------------------------------------
;------------------------------------------
; Convert CHS to LBA
; LBA = (cluster - 2) * sectors per cluster
;------------------------------------------
[bits 16]
ClusterLBA:
    sub   ax,0002h                      ; zero base cluster number
    xor   cx,cx
    mov   cl,BYTE [SectorsPerCluster]   ; convert byte to word
    mul   cx
    add   ax,WORD [DataSector]          ; base data sector
    ret

;---------------------------------------------------------------------------
; Convert LBA to CHS
; AX = LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;---------------------------------------------------------------------------
[bits 16]
LBACHS:                                 ;
    xor   dx,dx                         ; prepare dx:ax for operation
    div   WORD [SectorsPerTrack]        ; calculate
    inc   dl                            ; adjust for sector 0
    mov   BYTE [AbsoluteSector],dl
    xor   dx,dx                         ; prepare dx:ax for operation
    div   WORD [HeadsPerCylinder]       ; calculate
    mov   BYTE [AbsoluteHead],dl
    mov   BYTE [AbsoluteTrack],al
    ret

;-----------------------------------
; Read a series of sectors
; CX     = Number of sectors to read
; AX     = Starting sector
; ES:EBX = Buffer
;-----------------------------------
[bits 16]
ReadSector:
    mov   di,0005h                      ; five retries for error
ReadSector1:
    push  ax
    push  bx
    push  cx
    call  LBACHS                        ; convert starting sector to CHS
    mov   ah,02h                        ; BIOS read sector
    mov   al,01h                        ; read one sector
    mov   ch,BYTE [AbsoluteTrack]       ; track
    mov   cl,BYTE [AbsoluteSector]      ; sector
    mov   dh,BYTE [AbsoluteHead]        ; head
    mov   dl,BYTE [DriveNumber]         ; drive
    int   13h                           ; invoke BIOS
    jnc   ReadSector2                   ; test for read error
    xor   ax,ax                         ; BIOS reset disk
    int   13h                           ; invoke BIOS
    dec   di                            ; decrement error counter
    pop   cx
    pop   bx
    pop   ax
    jnz   ReadSector1                ; attempt to read again
    int   18h
ReadSector2:
    pop   cx
    pop   bx
    pop   ax
    add   bx,WORD [BytesPerSector]      ; queue next buffer
    inc   ax                            ; queue next sector
    loop  ReadSector                    ; read next sector
    ret

;------------------------------------
; Load Root Directory Table to 07E00h
;------------------------------------
[bits 16]
LoadRootDir:
    pusha                               ; store registers
    push  es
    ; compute size of root directory and store in "cx"
    xor   cx,cx                         ; clear registers
    xor   dx,dx
    mov   ax,32                         ; 32 byte directory entry
    mul   WORD [RootEntries]            ; total size of directory
    div   WORD [BytesPerSector]         ; sectors used by directory
    xchg  ax,cx                         ; move into AX
    ; compute location of root directory and store in "ax"
    mov   al,BYTE [NumberOfFATs]        ; number of FATs
    mul   WORD [SectorsPerFAT]          ; sectors used by FATs
    add   ax,WORD [ReservedSectors]
    mov   WORD [DataSector],ax          ; base of root directory
    add   WORD [DataSector],cx
    ; read root directory into 07E00h
    push  word ROOT_SEG
    pop   es
    mov   bx,0                          ; copy root dir
    call  ReadSector                    ; read in directory table
    pop   es
    popa                                ; restore registers and return
    ret

;-----------------------------
; Loads FAT table to 07C00h
; ES:DI = Root Directory Table
;-----------------------------
[bits 16]
LoadFAT:
    pusha                               ; store registers
    push  es
    ; compute size of FAT and store in "cx"
    xor   ax,ax
    mov   al,BYTE [NumberOfFATs]        ; number of FATs
    mul   WORD [SectorsPerFAT]          ; sectors used by FATs
    mov   cx,ax
    ; compute location of FAT and store in "ax"
    mov   ax,WORD [ReservedSectors]
    ; read FAT into memory (Overwrite our bootloader at 07C00h)
    push  word FAT_SEG
    pop   es
    xor   bx,bx
    call  ReadSector 
    pop   es
    popa                                ; restore registers and return
    ret

;----------------------------------------------------------------
; Search for filename in root table
; parm/ DS:SI = File name
; ret/  AX    = File index number in directory table. -1 if error
;----------------------------------------------------------------
[bits 16]
FindFile:
    push  cx                            ; store registers
    push  dx
    push  bx
    mov   bx,si                         ; copy filename for later
    ; browse root directory for binary image
    mov   cx,WORD [RootEntries]         ; load loop counter
    mov   di,ROOT_OFFSET                ; locate first root entry at 1 MB mark
    cld                                 ; clear direction flag
FindFile1:
    push  cx
    mov   cx,11                         ; eleven character name. Image name is in SI
    mov   si,bx                         ; image name is in BX
    push  di
    rep   cmpsb                         ; test for entry match
    pop   di
    je    FindFile2
    pop   cx
    add   di,32                         ; queue next directory entry
    loop  FindFile1
    ; Not Found
    pop   bx                            ; restore registers and return
    pop   dx
    pop   cx
    mov   ax,-1                         ; set error code
    ret
FindFile2:
    pop   ax                            ; return value into AX contains entry of file
    pop   bx                            ; restore registers and return
    pop   dx
    pop   cx
    ret

;-----------------------------------------
; Load file
; parm/ ES:SI  = File to load
; parm/ EBX:BP = Buffer to load file to
; ret/  AX     = -1 on error, 0 on success
; ret/  CX     = number of sectors read
;-----------------------------------------
[bits 16]
LoadFile:
    xor   ecx,ecx                       ; size of file in sectors
    push  ecx
    push  bx                            ; BX => BP points to buffer to write to; store it for later
    push  bp
    call  FindFile                      ; find our file. ES:SI contains our filename
    cmp   ax,-1
    jne   LoadFile1
    ; failed to find file
    pop   bp
    pop   bx
    pop   ecx
    mov   ax,-1
    ret
LoadFile1:
    sub   edi,ROOT_OFFSET
    sub   eax,ROOT_OFFSET
    ; get starting cluster
    push  word ROOT_SEG                 ;root segment loc
    pop   es
    mov   dx,WORD [es:di + 0001Ah]      ; DI points to file entry in root directory table. Refrence the table...
    mov   WORD [Cluster],dx             ; file's first cluster
    pop   bx                            ; get location to write to so we dont screw up the stack
    pop   es
    push  bx                            ; store location for later again
    push  es
    call  LoadFAT
LoadFile2:
    ; load the cluster
    mov   ax,WORD [Cluster]             ; cluster to read
    pop   es                            ; bx:bp=es:bx
    pop   bx
    call  ClusterLBA
    xor   cx,cx
    mov   cl,BYTE [SectorsPerCluster]
    call  ReadSector 
    pop   ecx
    inc   ecx                           ; add one more sector to counter
    push  ecx
    push  bx
    push  es
    mov   ax,FAT_SEG                    ;start reading from fat
    mov   es,ax
    xor   bx,bx
    ; get next cluster
    mov   ax,WORD [Cluster]             ; identify current cluster
    mov   cx,ax                         ; copy current cluster
    mov   dx,ax
    shr   dx,0001h                      ; divide by two
    add   cx,dx                         ; sum for (3/2)
    mov   bx,0                          ;location of fat in memory
    add   bx,cx
    mov   dx,WORD [es:bx]
    test  ax,0001h                      ; test for odd or even cluster
    jnz   LoadFile3
    and   dx,0000111111111111b          ; Even cluster - take low 12 bits
    jmp   LoadFile4
LoadFile3:
    shr   dx,0004h                      ; Odd cluster  - take high 12 bits
LoadFile4:
    mov   WORD [Cluster],dx
    cmp   dx,0FF0h                      ; test for end of file marker
    jb    LoadFile2
    ; We're done
    pop   es
    pop   bx
    pop   ecx
    xor   ax,ax
    ret

;--------------------------------------------------------------------------------------------------
; Stage 2 Entry Point
; - Set Data segment registers and stack
; - Install GDT
; - Enable A20
; - Read Stage3 into memory
; - Protected mode (pmode)
;--------------------------------------------------------------------------------------------------
[bits 16]
Main:
    ;----------------------------
    ; Set Data Segement registers
    ;----------------------------
    cli                                 ; disable interrupts
    xor   ax,ax                         ; null segments
    mov   ds,ax
    mov   es,ax

    ;-----------------
    ; Set up our Stack
    ;-----------------
    mov   ax,00h                        ; stack begins at 09000h-0FFFFh
    mov   ss,ax
    mov   sp,0FFFFh
    sti                                 ; enable interrupts

    ;----------------
    ; Install our GDT
    ;----------------
    call  InstallGDT

    ;-----------
    ; Enable A20
    ;-----------
    call  EnableA20

    ;----------------------
    ; Print loading message
    ;----------------------
    mov   si,LoadingMsg
    call  Puts16
    mov   ah,00h                        ; wait
    int   16h                           ;  for keypress

    ;----------------------
    ; Initialize filesystem
    ;----------------------
    call  LoadRootDir                   ; Load root directory table

    ;----------------------
    ; Read Kernel from disk
    ;----------------------
    mov   ebx,0                         ; BX:BP points to buffer to load to
    mov   bp,IMAGE_RMODE_BASE
    mov   si,ImageName                  ; our file to load
    call  LoadFile
    mov   dword [ImageSize],ecx         ; save size of kernel
    cmp   ax,0                          ; Test for success
    je    GoProtected                   ; yep--onto Stage 3!

    ;------------------
    ; This is very bad!
    ;------------------
    mov   si,FailureMsg                 ; Nope--print error
    call  Puts16                        ;
    mov   ah,0                          ; wait
    int   16h                           ;  for keypress
    int   19h                           ; warm boot computer
    cli                                 ; If we get here, something really went wrong
    hlt

GoProtected:
    ;--------------
    ; Go into pmode
    ;--------------
    cli                                 ; clear interrupts
    mov   eax,cr0                       ; set bit 0 in cr0--enter pmode
    or    eax,1
    mov   cr0,eax
    jmp   CODE_DESC:GoStage3            ; far jump to fix CS. Remember that the code selector is 08h!

  ; Note: Do NOT re-enable interrupts! Doing so will triple fault!
  ; We will fix this in Stage 3.

;--------------------------------------------------------------------------------------------------
; Get to Stage3 - Our Kernel!
; - Set Data Segment Register
; - Set up our Stack
; - Copy Kernel to address 1 MB
; - Jump to our Kernel!!
;--------------------------------------------------------------------------------------------------
[bits 32]
GoStage3:
    ;----------------------------
    ; Set Data Segement registers
    ;----------------------------
    mov   ax,DATA_DESC                  ; set data segments to data selector (10h)
    mov   ds,ax
    mov   ss,ax
    mov   es,ax

    ;-----------------
    ; Set up our Stack
    ;-----------------
    mov   esp,90000h                    ; stack begins from 90000h

    ;-------------------
    ; Copy kernel to 1MB
    ;-------------------
    mov   eax,dword [ImageSize]
    movzx ebx,word [BytesPerSector]
    mul   ebx
    mov   ebx,4
    div   ebx
    cld
    mov   esi,IMAGE_RMODE_BASE
    mov   edi,IMAGE_PMODE_BASE
    mov   ecx,eax
    rep   movsd                         ; copy image to its protected mode address

    ;--------------------
    ; Jump to our Kernel!
    ;--------------------
    jmp   CODE_DESC:IMAGE_PMODE_BASE    ; jump to our kernel! Note: This assumes Kernel's entry point is at 1 MB

    ;-----------------
    ;   Stop execution
    ;-----------------
    cli
    hlt

;--------------------------------------------------------------------------------------------------
; Global Descriptor Table (GDT)
; Tutorial 8: Protected Mode
;--------------------------------------------------------------------------------------------------
GDT1:
;----------------
; null descriptor
;----------------
                  DD  0
                  DD  0
NULL_DESC         EQU 0
;----------------
; code descriptor
;----------------
                  DW  0FFFFh            ; limit low
                  DW  0                 ; base low
                  DB  0                 ; base middle
                  DB  10011010b         ; access
                  DB  11001111b         ; granularity
                  DB  0                 ; base high
CODE_DESC         EQU 8h
;----------------
; data descriptor
;----------------
                  DW  0FFFFh            ; limit low
                  DW  0                 ; base low
                  DB  0                 ; base middle
                  DB  10010010b         ; access
                  DB  11001111b         ; granularity
                  DB  0                 ; base high
DATA_DESC         EQU 10h
;-------------------
; pointer to our GDT
;-------------------
GDT2:
                  DW  GDT2-GDT1-1       ; limit (Size of GDT)
                  DD  GDT1              ; base of GDT

;--------------------------------------------------------------------------------------------------
; Working Storage
;--------------------------------------------------------------------------------------------------
FAT_SEG           EQU 2C0h
IMAGE_PMODE_BASE  EQU 100000h           ; where the kernel is to be loaded to in protected mode
IMAGE_RMODE_BASE  EQU 3000h             ; where the kernel is to be loaded to in real mode
ROOT_OFFSET       EQU 2E00h
ROOT_SEG          EQU 2E0h

LoadingMsg        DB  0Dh
                  DB  0Ah
                  DB  "Searching for Operating System v2..."
                  DB  00h

FailureMsg        DB  0Dh
                  DB  0Ah
                  DB  "*** FATAL: MISSING OR CURRUPT STAGE3.BIN. Press Any Key to Reboot"
                  DB  0Dh
                  DB  0Ah
                  DB  0Ah
                  DB  00h


AbsoluteHead      DB  00h
AbsoluteSector    DB  00h
AbsoluteTrack     DB  00h
BytesPerSector    DW  512
Cluster           DW  0000h
DataSector        DW  0000h
DriveNumber       DB  0
HeadsPerCylinder  DW  2
ImageName         DB  "STAGE3  BIN"      ; kernel name (Must be 11 bytes)
ImageSize         DB  0                  ; size of kernel image in bytes
NumberOfFATs      DB  2
ReservedSectors   DW  1
RootEntries       DW  224
SectorsPerCluster DB  1
SectorsPerFAT     DW  9
SectorsPerTrack   DW  18