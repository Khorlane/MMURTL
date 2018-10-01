.386P

CSEG      SEGMENT WORD 'Code' USE16
          ASSUME CS:CSEG, DS:CSEG, ES:Nothing

ORG 0h

BootStart:
    JMP   SHORT Bootup
    NOP

; The following are pointers to my IDT and GDT after my OS loads
; and are not part of the above boot sector structure.

IDTptr    DW 07FFh                      ; LIMIT 256 IDT Slots
          DD 0000h                      ; BASE (Linear)

GDTptr    DW 17FFh                      ; LIMIT 768 slots
          DD 0800h                      ; BASE (Linear)

; This is where we jump from those first 3 bytes

BootUp:

; Boot block's first instruction starts here after initial jump from beginning
    CLI                                 ; Clear interrupts
    CLD                                 ; Clear Direction Flag, set auto-increment of SI/DI
; Stick the stack at 98000h (an arbitrary location)
    MOV   AX,9000h                      ; Stack
    MOV   SS,AX                         ;   Set Stack Segment SS to 9000h using AX
    MOV   SP,8000h                      ;   Set Stack Pointer SP to 8000h

; Move this boot sector UP to 90000h Linear. Bios loaded us at 0000:7C00
    MOV   AX,09000h                     ; Destination
    MOV   ES,AX                         ;   Set ES to 9000h using AX
    XOR   DI,DI                         ;   Set DI to 0h
    MOV   AX,7C0h                       ; Source
    MOV   DS,AX                         ;   Set DS to 7C0h using AX
    XOR   SI,SI                         ;   Set SI to 0h
    MOV   CX,512                        ; Set Count CX to 512
    REP   MOVSB                         ; Move DS:SI to ES:DI
                                        ;  repeat based on CX (512 times)
                                        ;  bumping SI and DI by 1 each time

  ; Now we jump UP to where we moved it.
    MOV   AX, 09000h                    ; Code Segment
    PUSH  AX                            ; Put Code Segment(CS) on stack
    LEA   AX,Jumper                     ; Offset to new location <--------------------
    PUSH  AX                            ; Put offset(IP) on stack                    |
    RETF                                ; Use RETF to jump to new code location      |
                                        ;     RETF Pops IP then Pops CS              |
; Now set DS equal to ES which is 9000h ;                                            |
Jumper:                                 ;                                            |
    PUSH  ES                            ; 'The offset of this instruction' and 'the offset placed into AX', MUST MATCH
    POP   DS                            ; Push ES and Pop DS (ES has the address we need for DS)

    MOV   SI, OFFSET MsgLoad            ; Set SI to address of first byte of MsgLoad
    CALL  PutChars                      ; Write message to screen
    JMP   $                             ; Loop forever

PutChars:
    LODSB                               ; Load byte at address DS:SI into AL, then bump SI
    OR    AL,AL                         ; if AL = 0
    JZ    SHORT Done                    ;   then we're done
    MOV   AH,0Eh                        ; Set 'Teletype output' AL = Character
    MOV   BX,0007                       ;   BH = Page Number BL = Color (Light Gray)
    INT   10h                           ; Call Bios Video Service
    JMP   SHORT PutChars                ; Loop
Done:
    RETN                                ; Return to caller

MsgLoad   DB 0Dh                        ; Carriage Return
          DB 0Ah                        ; New Line
          DB 'Loading MMURTL v0.06'     ; The Message
          DB 00h                        ; End of Message

          DB (510-($-BootStart)) DUP(0) ; Filler to make BootSig starts at 01FE
BootSig   DW 0AA55h                     ; Magic Word

CSEG      ENDS                          ; Must be at 200h
          END