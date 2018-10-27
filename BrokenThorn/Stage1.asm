;**********************************************************
; Stage1.asm
;   A Simple Boot Sector that:
;   1. is exactly 512 bytes long
;   2. has the Magic Word at the end (0xAA55)
;   3. allows us to have a useable floppy
;      where we can put our Stage2/Stage3 code
;      by coding a proper BIOS Parameter Block
;   4. has code to load our Stage2 code
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
; nasm -f bin Stage1.asm -o Stage1.bin
;**********************************************************

[bits 16]                               ; we are in 16 bit real mode
    ORG   0                             ; we will set regisers later
    JMP   Booter                        ; jump to start of bootloader

;--------------------------------------------------------------------------------------------------
; BIOS Parameter Block
;   and yes, this block must start at offset 0x003
;   and yes, these are the required fields
;   and yes, they must be in this order
;   you can change the names (obviously)
;--------------------------------------------------------------------------------------------------

; BPB Begins 3 bytes from start. We do a far jump, which is 3 bytes in size.
; If you use a short jump, add a "nop" after it to offset the 3rd byte.
; See Wikipedia "Design of the FAT file system" for more info on the BIOS Parameter Block

                                        ; Hex Offset from beginning of Boot Sector
OEM                   DB "My OS   "     ; 0x003  8 bytes padded with spaces
BytesPerSector        DW 512            ; 0x00B  2 bytes
SectorsPerCluster     DB 1              ; 0x00D  1 byte
ReservedSectors       DW 1              ; 0x00E  2 bytes
NumberOfFATs          DB 2              ; 0x010  1 bytes
RootEntries           DW 224            ; 0x011  2 bytes
TotalSectors          DW 2880           ; 0x013  2 bytes
Media                 DB 0xf0           ; 0x015  1 byte
SectorsPerFAT         DW 9              ; 0x016  2 bytes
SectorsPerTrack       DW 18             ; 0x018  2 bytes DOS 3.31 BPB
HeadsPerCylinder      DW 2              ; 0x01A  2 bytes DOS 3.31 BPB
HiddenSectors         DD 0              ; 0x01C  4 bytes DOS 3.31 BPB
TotalSectorsBig       DD 0              ; 0x020  4 bytes DOS 3.31 BPB
DriveNumber           DB 0              ; 0x024  1 byte  Extended BIOS Parameter Block
Unused                DB 0              ; 0x025  1 byte  Extended BIOS Parameter Block
ExtBootSignature      DB 0x29           ; 0x026  1 byte  Extended BIOS Parameter Block
SerialNumber          DD 0xa0a1a2a3     ; 0x027  4 bytes Extended BIOS Parameter Block
VolumeLabel           DB "MYOS FLOPPY"  ; 0x028 11 bytes Extended BIOS Parameter Block
FileSystem            DB "FAT12   "     ; 0x036  8 bytes Extended BIOS Parameter Block padded with spaces

;--------------------------------------------------------------------------------------------------
; Prints a string
; DS => SI: 0 terminated string
;--------------------------------------------------------------------------------------------------
Print:
    LODSB                               ; Load byte at address DS:(E)SI into AL
    OR    AL,AL                         ; Does AL=0?
    JZ    PrintDone                     ; Yep, null terminator found-bail out
    MOV   AH,0Eh                        ; Nope-Print the character
    INT   10h
    JMP   Print                         ; Repeat until null terminator found
  PrintDone:
    RET                                 ; we are done, so return

;--------------------------------------------------------------------------------------------------
; Convert CHS to LBA
; Given: AX = Cluster to be read
; LBA = (Cluster - 2) * sectors per cluster
;--------------------------------------------------------------------------------------------------
ClusterLBA:
    SUB   AX,0x0002                     ; zero base cluster number
    XOR   CX,CX
    MOV   CL,BYTE [SectorsPerCluster]   ; convert byte to word
    MUL   CX
    ADD   AX,WORD [DataSector]          ; base data sector
    RET

;--------------------------------------------------------------------------------------------------
; Convert LBA to CHS
; AX => LBA Address to convert
;
; absolute sector = (logical sector /  sectors per track) + 1
; absolute head   = (logical sector /  sectors per track) MOD number of heads
; absolute track  =  logical sector / (sectors per track * number of heads)
;--------------------------------------------------------------------------------------------------
LBACHS:
    XOR   DX,DX                         ; prepare dx:ax for operation
    DIV   WORD [SectorsPerTrack]        ; calculate
    INC   DL                            ; adjust for sector 0
    MOV   BYTE [AbsoluteSector],DL
    XOR   DX,DX                         ; prepare dx:ax for operation
    DIV   WORD [HeadsPerCylinder]       ; calculate
    MOV   BYTE [AbsoluteHead],DL
    MOV   BYTE [AbsoluteTrack],AL
    RET

;--------------------------------------------------------------------------------------------------
; Reads a series of sectors
; CX    => Number of sectors to read
; AX    => STARTing sector
; ES:BX => Buffer to read to
;--------------------------------------------------------------------------------------------------
ReadSector:
    MOV   DI,0x0005                     ; five retries for error
ReadSectorLoop:
    PUSH  AX
    PUSH  BX
    PUSH  CX
    CALL  LBACHS                        ; convert starting sector to CHS
    MOV   AH,0x02                       ; BIOS read sector
    MOV   AL,0x01                       ; read one sector
    MOV   CH,BYTE [AbsoluteTrack]       ; track
    MOV   CL,BYTE [AbsoluteSector]      ; sector
    MOV   DH,BYTE [AbsoluteHead]        ; head
    MOV   DL,BYTE [DriveNumber]         ; drive
    INT   0x13                          ; invoke BIOS
    JNC   ReadSectorOk                  ; test for read error
    XOR   AX,AX                         ; BIOS reset disk
    INT   0x13                          ; invoke BIOS
    DEC   DI                            ; decrement error counter
    POP   CX
    POP   BX
    POP   AX
    JNZ   ReadSectorLoop                ; attempt to read again
    INT   0x18
ReadSectorOk:
    MOV   SI,ProgressMsg
    CALL  Print                         ;
    POP   CX
    POP   BX
    POP   AX
    ADD   BX,WORD [BytesPerSector]      ; queue next buffer
    INC   AX                            ; queue next sector
    LOOP  ReadSector                    ; read next sector
    RET

;--------------------------------------------------------------------------------------------------
; Boot Loader Entry Point
;--------------------------------------------------------------------------------------------------
Booter:
    ;-------------------------------------------------------
    ;- code located at 0000:7C00, adjust segment registers -
    ;-------------------------------------------------------
    CLI                                 ; disable interrupts
    MOV   AX,0x07C0                     ; setup
    MOV   DS,AX                         ;  registers
    MOV   ES,AX                         ;   to point
    MOV   FS,AX                         ;    to our
    MOV   GS,AX                         ;     segment

    ;----------------
    ;- create stack -
    ;----------------
    MOV   AX,0x0000                     ; set the
    MOV   SS,AX                         ;  stack to
    MOV   SP,0xFFFF                     ;   somewhere safe
    STI                                 ; restore interrupts

    ;---------------------------
    ;- Display loading message -
    ;---------------------------
    MOV   SI,LoadingMsg                 ; si points to first byte in msgLoading
    CALL  Print                         ; Print message
    MOV   AH,0X00                       ; wait
    INT   0x16                          ;  for keypress

    ;-----------------------------
    ;- Load root directory table -
    ;-----------------------------
    ; compute size of root directory and store in "cx"
    XOR   CX,CX                         ; zero out cx
    XOR   DX,DX                         ; zero out dx
    MOV   AX,0x0020                     ; 32 byte directory entry
    MUL   WORD [RootEntries]            ; total size of directory
    DIV   WORD [BytesPerSector]         ; sectors used by directory
    XCHG  AX,CX                         ; swap ax cx

    ; compute location of root directory and store in "ax"
    MOV   AL,BYTE [NumberOfFATs]        ; number of FATs
    MUL   WORD [SectorsPerFAT]          ; sectors used by FATs
    ADD   ax,WORD [ReservedSectors]     ; adjust for bootsector
    MOV   WORD [DataSector],AX          ; base of root directory
    ADD   WORD [DataSector],CX

    ; read root directory into memory (7C00:0200)
    MOV   BX,0x0200                     ; copy root dir above bootcode
    CALL  ReadSector                    ;

    ;----------------------------------------------------
    ; Find stage 2
    ;----------------------------------------------------
    ; browse root directory for binary image
    MOV   CX,WORD [RootEntries]         ; load loop counter
    MOV   DI,0x0200                     ; locate first root entry
FindFat:
    PUSH  CX
    MOV   CX,0x000B                     ; eleven character name
    MOV   SI,Stage2Name                 ; Stage2 file name to find
    PUSH  DI
    REP   CMPSB                         ; test for entry match
    POP   DI
    JE    LoadFat
    POP   CX
    ADD   DI,0x0020                     ; queue next directory entry
    LOOP  FindFat
    JMP   FindFatFailed

;----------------------------------------------------
; Load FAT
;----------------------------------------------------
LoadFat:
    ; save starting cluster of boot image
    MOV   DX,WORD [DI + 0x001A]
    MOV   WORD [Cluster],DX             ; file's first cluster

    ; compute size of FAT and store in "cx"
    XOR   AX,AX
    MOV   AL,BYTE [NumberOfFATs]        ; number of FATs
    MUL   WORD [SectorsPerFAT]          ; sectors used by FATs
    MOV   CX,AX

    ; compute location of FAT and store in "ax"
    MOV   AX,WORD [ReservedSectors]     ; adjust for bootsector

    ; read FAT into memory (7C00:0200)
    MOV   BX,0x0200                     ; copy FAT above bootcode
    CALL  ReadSector                    ;

    ; read Stage2 file into memory (0050:0000)
    MOV   AX,0x0050                     ; set segment register
    MOV   ES,AX                         ;  to 50h
    MOV   BX,0x0000                     ; push our starting address (0h)
    PUSH  BX                            ;  onto the stack

;--------------------------------------------------------------------------------------------------
; Load Stage 2
;--------------------------------------------------------------------------------------------------
LoadStage2:
    MOV   AX,WORD [Cluster]             ; cluster to read
    POP   BX                            ; buffer to read into
    CALL  ClusterLBA                    ; convert cluster to LBA
    XOR   CX,CX
    MOV   CL,BYTE [SectorsPerCluster]   ; sectors to read
    CALL  ReadSector                    ;
    PUSH  BX

    ; compute next cluster
    MOV   AX,WORD [Cluster]             ; identify current cluster
    MOV   CX,AX                         ; copy current cluster
    MOV   DX,AX                         ; copy current cluster
    SHR   DX,0x0001                     ; divide by two
    ADD   CX,DX                         ; sum for (3/2)
    MOV   BX,0x0200                     ; location of FAT in memory
    ADD   BX,CX                         ; index into FAT
    MOV   DX,WORD [BX]                  ; read two bytes from FAT
    TEST  AX,0x0001
    JNZ   LoadStage2OddCluster

LoadStage2EvenCluster:
    AND   DX,0000111111111111b          ; take low twelve bits
    JMP   LoadStage2Done

LoadStage2OddCluster:
    SHR   DX,0x0004                     ; take high twelve bits

LoadStage2Done:
    MOV   WORD [Cluster],DX             ; store new cluster
    CMP   DX,0x0FF0                     ; If DX is less than EOF (0x0FF0)
    JB    LoadStage2                    ;   then keep going (JB = Jump Below)

    MOV   SI,NewLineMsg
    CALL  Print                         ;
    PUSH  WORD 0x0050                   ; Jump to our Stage2 code that we put at 0050:0000
    PUSH  WORD 0x0000                   ;   by using a Far Return which pops IP(0h) then CS(50h)
    RETF                                ;   and poof, we're executing our Stage2 code!

;--------------------------------------------------------------------------------------------------
; Failed to find FAT (File Allocation Table)
;--------------------------------------------------------------------------------------------------
FindFatFailed:
    MOV   SI,FailureMsg
    CALL  Print                         ;
    MOV   AH,0x00
    INT   0x16                          ; await keypress
    INT   0x19                          ; warm boot computer

;--------------------------------------------------------------------------------------------------
; Working Storage
;--------------------------------------------------------------------------------------------------
    AbsoluteHead    DB 0x00
    AbsoluteSector  DB 0x00
    AbsoluteTrack   DB 0x00
    Cluster         DW 0x0000
    DataSector      DW 0x0000
    FailureMsg      DB 0x0D, 0x0A, "MISSING OR CURRUPT STAGE2. Press Any Key to Reboot", 0x0D, 0x0A, 0x00
    LoadingMsg      DB 0x0D, 0x0A, "Loading Boot Image v5 ", 0x00
    NewLineMsg      DB 0x0D, 0x0A, 0x00
    ProgressMsg     DB ".", 0x00
    Stage2Name      DB "STAGE2  BIN"

;--------------------------------------------------------------------------------------------------
; Make it a Boot Sector! (must be exactly 512 bytes)
;--------------------------------------------------------------------------------------------------
    TIMES 510-($-$$) DB 0
    DW 0xAA55                           ; Magic Word that makes this a boot sector