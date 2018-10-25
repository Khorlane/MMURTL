;**********************************************************
; Boot1.asm
;   A Simple Bootloader
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
; nasm -f bin Boot1.asm -o Boot1.bin
;**********************************************************

[bits 16]                               ; we are in 16 bit real mode

org   0                                 ; we will set regisers later

start:
    jmp   Booter                        ; jump to start of bootloader

;--------------------------------------------------------------------------------------------------
; BIOS Parameter Block
;--------------------------------------------------------------------------------------------------

; BPB Begins 3 bytes from start. We do a far jump, which is 3 bytes in size.
; If you use a short jump, add a "nop" after it to offset the 3rd byte.
; See Wikipedia "Design of the FAT file system" for more info on the BIOS Parameter Block

                                        ; Hex Offset from beginning of Boot Sector
bpbOEM                db "My OS   "     ; 0x003  8 bytes padded with spaces
bpbBytesPerSector:    DW 512            ; 0x00B  2 bytes 
bpbSectorsPerCluster: DB 1              ; 0x00D  1 byte  
bpbReservedSectors:   DW 1              ; 0x00E  2 bytes
bpbNumberOfFATs:      DB 2              ; 0x010  1 bytes
bpbRootEntries:       DW 224            ; 0x011  2 bytes
bpbTotalSectors:      DW 2880           ; 0x013  2 bytes
bpbMedia:             DB 0xf0           ; 0x015  1 byte
bpbSectorsPerFAT:     DW 9              ; 0x016  2 bytes
bpbSectorsPerTrack:   DW 18             ; 0x018  2 bytes DOS 3.31 BPB
bpbHeadsPerCylinder:  DW 2              ; 0x01A  2 bytes DOS 3.31 BPB
bpbHiddenSectors:     DD 0              ; 0x01C  4 bytes DOS 3.31 BPB
bpbTotalSectorsBig:   DD 0              ; 0x020  4 bytes DOS 3.31 BPB
bsDriveNumber:        DB 0              ; 0x024  1 byte  Extended BIOS Parameter Block
bsUnused:             DB 0              ; 0x025  1 byte  Extended BIOS Parameter Block
bsExtBootSignature:   DB 0x29           ; 0x026  1 byte  Extended BIOS Parameter Block
bsSerialNumber:       DD 0xa0a1a2a3     ; 0x027  4 bytes Extended BIOS Parameter Block
bsVolumeLabel:        DB "MOS FLOPPY "  ; 0x028 11 bytes Extended BIOS Parameter Block
bsFileSystem:         DB "FAT12   "     ; 0x036  8 bytes Extended BIOS Parameter Block padded with spaces

;--------------------------------------------------------------------------------------------------
; Prints a string
; DS=>SI: 0 terminated string
;--------------------------------------------------------------------------------------------------
Print:
    lodsb                               ; Load byte at address DS:(E)SI into AL
    or    al,al                         ; Does AL=0?
    jz    PrintDone                     ; Yep, null terminator found-bail out
    mov   ah,0eh                        ; Nope-Print the character
    int   10h
    jmp   Print                         ; Repeat until null terminator found
  PrintDone:
    ret                                 ; we are done, so return

absoluteSector db 0x00
absoluteHead   db 0x00
absoluteTrack  db 0x00

;--------------------------------------------------------------------------------------------------
; Convert CHS to LBA
; LBA = (cluster - 2) * sectors per cluster
;--------------------------------------------------------------------------------------------------
ClusterLBA:
    sub   ax,0x0002                     ; zero base cluster number
    xor   cx,cx
    mov   cl,BYTE [bpbSectorsPerCluster] ; convert byte to word
    mul   cx
    add   ax,WORD [datasector]          ; base data sector
    ret

;--------------------------------------------------------------------------------------------------
; Convert LBA to CHS
; AX=>LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;--------------------------------------------------------------------------------------------------
LBACHS:
    xor   dx,dx                         ; prepare dx:ax for operation
    div   WORD [bpbSectorsPerTrack]     ; calculate
    inc   dl                            ; adjust for sector 0
    mov   BYTE [absoluteSector],dl
    xor   dx,dx                         ; prepare dx:ax for operation
    div   WORD [bpbHeadsPerCylinder]    ; calculate
    mov   BYTE [absoluteHead],dl
    mov   BYTE [absoluteTrack],al
    ret

;--------------------------------------------------------------------------------------------------
; Reads a series of sectors
; CX=>Number of sectors to read
; AX=>Starting sector
; ES:BX=>Buffer to read to
;--------------------------------------------------------------------------------------------------
ReadSectors:
    mov   di, 0x0005                          ; five retries for error
ReadSectorLoop:
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
    jnc   ReadSectorOk                  ; test for read error
    xor   ax,ax                         ; BIOS reset disk
    int   0x13                          ; invoke BIOS
    dec   di                            ; decrement error counter
    pop   cx
    pop   bx
    pop   ax
    jnz   ReadSectorLoop                ; attempt to read again
    int   0x18
ReadSectorOk:
    mov   si,msgProgress
    call  Print                         ; (Routine is local)
    pop   cx
    pop   bx
    pop   ax
    add   bx,WORD [bpbBytesPerSector]   ; queue next buffer
    inc   ax                            ; queue next sector
    loop  ReadSectors                   ; read next sector
    ret

;--------------------------------------------------------------------------------------------------
; Bootloader Entry Point
;--------------------------------------------------------------------------------------------------
Booter:
    ;-------------------------------------------------------
    ;- code located at 0000:7C00, adjust segment registers -
    ;-------------------------------------------------------
    cli                                 ; disable interrupts
    mov   ax,0x07C0                     ; setup
    mov   ds,ax                         ;  registers
    mov   es,ax                         ;   to point
    mov   fs,ax                         ;    to our
    mov   gs,ax                         ;     segment

    ;----------------
    ;- create stack -
    ;----------------
    mov   ax,0x0000                     ; set the
    mov   ss,ax                         ;  stack to
    mov   sp,0xFFFF                     ;   somewhere safe
    sti                                 ; restore interrupts

    ;---------------------------
    ;- Display loading message -
    ;---------------------------
    mov   si,msgLoading                 ; si points to first byte in msgLoading
    call  Print                         ; Print message
    mov   ah,0x00                       ; wait
    int   0x16                          ;  for keypress

    ;-----------------------------
    ;- Load root directory table -
    ;-----------------------------
    ; compute size of root directory and store in "cx"
    xor   cx,cx                         ; zero out cx
    xor   dx,dx                         ; zero out dx
    mov   ax,0x0020                     ; 32 byte directory entry
    mul   WORD [bpbRootEntries]         ; total size of directory
    div   WORD [bpbBytesPerSector]      ; sectors used by directory
    xchg  ax,cx                         ; swap ax cx

    ; compute location of root directory and store in "ax"
    mov   al,BYTE [bpbNumberOfFATs]     ; number of FATs
    mul   WORD [bpbSectorsPerFAT]       ; sectors used by FATs
    add   ax,WORD [bpbReservedSectors]  ; adjust for bootsector
    mov   WORD [datasector],ax          ; base of root directory
    add   WORD [datasector],cx

    ; read root directory into memory (7C00:0200)
    mov   bx, 0x0200                    ; copy root dir above bootcode
    call  ReadSectors                   ; (Routine is local)

    ;----------------------------------------------------
    ; Find stage 2
    ;----------------------------------------------------
    ; browse root directory for binary image
    mov   cx, WORD [bpbRootEntries]     ; load loop counter
    mov   di, 0x0200                    ; locate first root entry
FindFat:
    push  cx
    mov   cx,0x000B                     ; eleven character name
    mov   si,ImageName                  ; image name to find
    push  di
    rep   cmpsb                         ; test for entry match
    pop   di
    je    LOAD_FAT
    pop   cx
    add   di,0x0020                     ; queue next directory entry
    loop  FindFat
    jmp   FindFatFailed

;----------------------------------------------------
; Load FAT
;----------------------------------------------------
LOAD_FAT:
    ; save starting cluster of boot image
    mov   dx,WORD [di + 0x001A]
    mov   WORD [cluster],dx             ; file's first cluster

    ; compute size of FAT and store in "cx"
    xor   ax,ax
    mov   al,BYTE [bpbNumberOfFATs]     ; number of FATs
    mul   WORD [bpbSectorsPerFAT]       ; sectors used by FATs
    mov   cx,ax

    ; compute location of FAT and store in "ax"
    mov   ax,WORD [bpbReservedSectors]  ; adjust for bootsector

    ; read FAT into memory (7C00:0200)
    mov   bx,0x0200                     ; copy FAT above bootcode
    call  ReadSectors                   ; (Routine is local)

    ; read image file into memory (0050:0000)
    mov   ax,0x0050
    mov   es,ax                         ; destination for image
    mov   bx,0x0000                     ; destination for image
    push  bx

;--------------------------------------------------------------------------------------------------
; Load Stage 2
;--------------------------------------------------------------------------------------------------
LoadImage:
    mov   ax,WORD [cluster]             ; cluster to read
    pop   bx                            ; buffer to read into
    call  ClusterLBA                    ; (Routine is local) convert cluster to LBA
    xor   cx,cx
    mov   cl,BYTE [bpbSectorsPerCluster] ; sectors to read
    call  ReadSectors                   ; (Routine is local)
    push  bx

    ; compute next cluster
    mov   ax,WORD [cluster]             ; identify current cluster
    mov   cx,ax                         ; copy current cluster
    mov   dx,ax                         ; copy current cluster
    shr   dx,0x0001                     ; divide by two
    add   cx,dx                         ; sum for (3/2)
    mov   bx,0x0200                     ; location of FAT in memory
    add   bx,cx                         ; index into FAT
    mov   dx,WORD [bx]                  ; read two bytes from FAT
    test  ax,0x0001
    jnz   LoadImageOddCluster

LoadImageEvenCluster:
    and   dx,0000111111111111b          ; take low twelve bits
    jmp   LoadImageDone

LoadImageOddCluster:
    shr   dx,0x0004                     ; take high twelve bits

LoadImageDone:
    mov   WORD [cluster],dx             ; store new cluster
    cmp   dx,0x0FF0                     ; test for end of file
    jb    LoadImage

    mov   si,msgCRLF
    call  Print                         ; (Routine is local)
    push  WORD 0x0050
    push  WORD 0x0000
    retf

;--------------------------------------------------------------------------------------------------
; Failed to find FAT (File Allocation Table)
;--------------------------------------------------------------------------------------------------
FindFatFailed:
    mov   si,msgFailure
    call  Print                         ; (Routine is local)
    mov   ah,0x00
    int   0x16                          ; await keypress
    int   0x19                          ; warm boot computer

;--------------------------------------------------------------------------------------------------
; Working Storage
;--------------------------------------------------------------------------------------------------
    datasector  dw 0x0000
    cluster     dw 0x0000
    ImageName   db "KRNLDR  SYS"
    msgLoading  db 0x0D, 0x0A, "Loading Boot Image v4 ", 0x00
    msgCRLF     db 0x0D, 0x0A, 0x00
    msgProgress db ".", 0x00
    msgFailure  db 0x0D, 0x0A, "MISSING OR CURRUPT KRNLDR. Press Any Key to Reboot", 0x0D, 0x0A, 0x00

;--------------------------------------------------------------------------------------------------
; Make it a Boot Sector! (must be exactly 512 bytes)
;--------------------------------------------------------------------------------------------------
    TIMES 510-($-$$) DB 0
    DW 0xAA55                           ; Magic Word that makes this a boot sector