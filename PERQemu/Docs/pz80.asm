; 
; Disassembly of PERQ1 Z80 ROM code
; J. Dersch 7/31/2008
;
; Interesting things of note:
; - this code does not use the index or shadow registers, and it makes almost no use of the extended Z80 instructions (bit, etc.)
; - after initializaton, the code runs a single main loop and resets the stack pointer every time through rather than properly maintaining it
; - 
; 
; Interesting memory locations:
;
; 2e26h - Input ready flag for devices?
;
; 2e27h - Z80 status byte. Relevant bits are:
;	- 7 : on = send Z80 status
;
; 2e28h - Device status.  Relevant bits are:
;	- 6 : GPIB status
;	- 5 : Floppy status
;	- 4 : Clock status
;	- 3 : Voltage status
;	- 2 : Keyboard status
;	- 1 : Tablet status
;	- 0 : RS232 status
;
; 2f33h - output message buffer : contains the data to be written to the PERQ for a message
; 2fe0h - top of Z80 stack
;
; Interesting IO ports:
;
; Output:
; 90h -
; 91h -
; 92h - 
; 93h - 
; 98h -
; b0h -
; b1h -
; b3h -
; b8h -
; beh -
; bfh - 
; c0h - 
; c8h - 
; d0h - Z80->PERQ output fifo
; d8h -

Input:
; 80h - Keyboard data
; 88h - Z80->PERQ output fifo status (?)
; a0h - 
; a8h - 
; a9h -
; b0h -
; b1h -
; b2h -
; b3h - 
; b8h -
; b9h -
; bah -
; bbh -
; bch -
; beh -
; bfh -
; 



; Entry point.  Disable interrupts and jump to start of init code.
0000 f3        di      
0001 c30001    jp      0100h	

; the following are the interrupt vectors for IM mode 2 (I is set to 00h on reset)
0004 00        nop     
0005 00        nop     
0006 00        nop     
0007 00        nop

; RST 08     
0008 00        nop     
0009 00        nop     
000a 00        nop     
000b 00        nop     
000c 00        nop     
000d 00        nop     
000e 00        nop     
000f 00        nop     

; RST 10
0010 00        nop     
0011 00        nop     
0012 00        nop     
0013 00        nop     
0014 00        nop     
0015 00        nop     
0016 00        nop     
0017 00        nop     

; RST 18
0018 00        nop     
0019 00        nop     
001a 00        nop     
001b 00        nop     
001c 00        nop     
001d 00        nop     
001e 00        nop     
001f 00        nop
				

; ISR addresses
0020 DW	7710		; 1077
0022 DW 1d10		; 101d
0024 DW 7e06		; 06e7
0026 DW	ee09		; 09ee
0028 DW 3b0a		; 0a3b
002a DW 7710		; 1077
002c DW ee09		; 09ee
002e DW aa12		; 12aa


; RST 30
0030 00        nop     
0031 00        nop     
0032 00        nop     
0033 00        nop     
0034 00        nop     
0035 00        nop     
0036 00        nop     
0037 00        nop     

; RST 38
0038 00        nop     
0039 00        nop     
003a 00        nop     
003b 00        nop     
003c 00        nop     
003d 00        nop     
003e 00        nop     
003f 00        nop

; ISR addresses
0040 DW 870d		; 0d87
0042 DW e90c		; 0ce9
0044 DW 0c0d		; 0d0c
0046 DW 6d0d		; 0d6d
0048 DW 6a0c		; 0c6a
004a DW ad12		; 12ad
004c DW ed0b		; 0bed
004e DW d40b		; 0bd4
0050 DW fc09		; 09fc
0052 DW fc09		; 09fc
0054 DW 990a		; 0a99
0056 DW f609		; 09f6
0058 DW ab0e		; 0eab
  
005a 00		   nop
005b 00        nop     
005c 00        nop     
005d 00        nop     
005e 00        nop     
005f 00        nop     
0060 00        nop     
0061 00        nop     
0062 00        nop     
0063 00        nop     
0064 00        nop     
0065 00        nop     
0066 00        nop     
0067 00        nop     
0068 00        nop     
0069 00        nop     
006a 00        nop     
006b 00        nop     
006c 00        nop     
006d 00        nop     
006e 00        nop     
006f 00        nop     
0070 00        nop     
0071 00        nop     
0072 00        nop     
0073 00        nop     
0074 00        nop     
0075 00        nop     
0076 00        nop     
0077 00        nop     
0078 00        nop     
0079 00        nop     
007a 00        nop     
007b 00        nop     
007c 00        nop     
007d 00        nop     
007e 00        nop     
007f 00        nop     
0080 00        nop     
0081 00        nop     
0082 00        nop     
0083 00        nop     
0084 00        nop     
0085 00        nop     
0086 00        nop     
0087 00        nop     
0088 00        nop     
0089 00        nop     
008a 00        nop     
008b 00        nop     
008c 00        nop     
008d 00        nop     
008e 00        nop     
008f 00        nop     
0090 00        nop     
0091 00        nop     
0092 00        nop     
0093 00        nop     
0094 00        nop     
0095 00        nop     
0096 00        nop     
0097 00        nop     
0098 00        nop     
0099 00        nop     
009a 00        nop     
009b 00        nop     
009c 00        nop     
009d 00        nop     
009e 00        nop     
009f 00        nop     
00a0 00        nop     
00a1 00        nop     
00a2 00        nop     
00a3 00        nop     
00a4 00        nop     
00a5 00        nop     
00a6 00        nop     
00a7 00        nop     
00a8 00        nop     
00a9 00        nop     
00aa 00        nop     
00ab 00        nop     
00ac 00        nop     
00ad 00        nop     
00ae 00        nop     
00af 00        nop     
00b0 00        nop     
00b1 00        nop     
00b2 00        nop     
00b3 00        nop     
00b4 00        nop     
00b5 00        nop     
00b6 00        nop     
00b7 00        nop     
00b8 00        nop     
00b9 00        nop     
00ba 00        nop     
00bb 00        nop     
00bc 00        nop     
00bd 00        nop     
00be 00        nop     
00bf 00        nop     
00c0 00        nop     
00c1 00        nop     
00c2 00        nop     
00c3 00        nop     
00c4 00        nop     
00c5 00        nop     
00c6 00        nop     
00c7 00        nop     
00c8 00        nop     
00c9 00        nop     
00ca 00        nop     
00cb 00        nop     
00cc 00        nop     
00cd 00        nop     
00ce 00        nop     
00cf 00        nop     
00d0 00        nop     
00d1 00        nop     
00d2 00        nop     
00d3 00        nop     
00d4 00        nop     
00d5 00        nop     
00d6 00        nop     
00d7 00        nop     
00d8 00        nop     
00d9 00        nop     
00da 00        nop     
00db 00        nop     
00dc 00        nop     
00dd 00        nop     
00de 00        nop     
00df 00        nop     
00e0 00        nop     
00e1 00        nop     
00e2 00        nop     
00e3 00        nop     
00e4 00        nop     
00e5 00        nop     
00e6 00        nop     
00e7 00        nop     
00e8 00        nop     
00e9 00        nop     
00ea 00        nop     
00eb 00        nop     
00ec 00        nop     
00ed 00        nop     
00ee 00        nop     
00ef 00        nop     
00f0 00        nop     
00f1 00        nop     
00f2 00        nop     
00f3 00        nop     
00f4 00        nop     
00f5 00        nop     
00f6 00        nop     
00f7 00        nop     
00f8 00        nop     
00f9 00        nop     
00fa 00        nop     
00fb 00        nop     
00fc 00        nop     
00fd 00        nop     
00fe 00        nop     
00ff 00        nop

; Init code.  Run on reset.
0100 31e02f    ld      sp,2fe0h		; top of stack is 2fe0
0103 ed5e      im      2
0105 97        sub     a			; Reset a to 0
0106 ed47      ld      i,a			; Interrupt page set to 0
0108 97        sub     a			; again ?
0109 32262e    ld      (2e26h),a
010c 32282e    ld      (2e28h),a
010f 3e9f      ld      a,9fh
0111 32272e    ld      (2e27h),a	; set device status mask to 10011111
0114 3e05      ld      a,05h
0116 d3c8      out     (0c8h),a
0118 32292e    ld      (2e29h),a
011b cd8509    call    0985h
011e cdff09    call    09ffh
0121 cd5c0a    call    0a5ch
0124 cd1c0b    call    0b1ch
0127 cd9d0c    call    0c9dh
012a cd730f    call    0f73h
012d cd910e    call    0e91h
0130 cd960f    call    0f96h
0133 cd7010    call    1070h
0136 cd8302    call    0283h
0139 fb        ei					
013a c34001    jp      0140h		; done with init, start main loop.

013d 0d        dec     c			; garbage / data ?
013e 0a        ld      a,(bc)
013f 00        nop
    
; Main Z80 loop
0140 31e02f    ld      sp,2fe0h		; reset the stack pointer every time around
0143 cda409    call    09a4h		; Do Tablet stuff
0146 cd6307    call    0763h		; Do Floppy stuff
0149 cd6e0a    call    0a6eh		; Do Shugart stuff
014c cd5e01    call    015eh		; send Z80 status info to PERQ
014f cda40f    call    0fa4h		; Do GPIB stuff
0152 cd860b    call    0b86h		; Do RS232 stuff
0155 cd0a0a    call    0a0ah		; Do keyboard stuff
0158 cd7b01    call    017bh		; send device status info to PERQ
015b c34001    jp      0140h		; go to start of main

015e 21272e    ld      hl,2e27h
0161 f3        di					; do not interrupt while we modify the status flags
0162 7e        ld      a,(hl)
0163 17        rla     
0164 da6901    jp      c,0169h		; check high bit of status byte (2e27h), if set we send Z80 status.
0167 fb        ei      
0168 c9        ret     

; Send Z80 status message. 'a' contains the status byte rotated left 1 bit on entry.
0169 1f        rra					; rotate status byte back
016a 47        ld      b,a
016b e67f      and     7fh			; clear high bit of status
016d 77        ld      (hl),a		; store back in 2e27h
016e fb        ei					; ok to interrupt this.
016f 3e0b      ld      a,0bh		
0171 cd7d12    call    127dh		; send Z80 status message
0174 78        ld      a,b
0175 cd8412    call    1284h		; send status mask to PERQ, this completes the Z80 status message.
0178 c34001    jp      0140h		; return to start of main Z80 loop.

; Dispatch status messages for Z80 devices.  Usually doesn't return, except when it does.  When it doesn't it just jumps to the top of
; the main loop which resets the stack pointer anyway.  Bizarre.
017b 3a282e    ld      a,(2e28h)
017e a7        and     a
017f c8        ret     z			; no flags set (no device status to report), return.
0180 21332f    ld      hl,2f33h		; set hl to point to the output buffer
0183 0f        rrca					; now we dispatch based on bits set in the status byte
0184 daa201    jp      c,01a2h		; if bit 0 set	- RS232 status
0187 0f        rrca    
0188 dac101    jp      c,01c1h		; bit 1 set		- Tablet status
018b 0f        rrca    
018c dade01    jp      c,01deh		; bit 2 set		- Keyboard status
018f 0f        rrca    
0190 daf301    jp      c,01f3h		; bit 3 set		- Voltage status
0193 0f        rrca    
0194 da1202    jp      c,0212h		; bit 4 set		- Clock status
0197 0f        rrca    
0198 da2702    jp      c,0227h		; bit 5 set		- Floppy status
019b 0f        rrca    
019c da4402    jp      c,0244h		; bit 6 set		- GPIB status
019f c35d02    jp      025dh		; must be bit 7, then -- need to send Z80 status next time around... 

; Send RS232 Status
01a2 3603      ld      (hl),03h		; Message length : 3 bytes
01a4 23        inc     hl
01a5 3a262e    ld      a,(2e26h)
01a8 e602      and     02h
01aa caaf01    jp      z,01afh
01ad 3e01      ld      a,01h		; data ready ?
01af 77        ld      (hl),a		; write byte 1	- data ready
01b0 23        inc     hl
01b1 3aca2d    ld      a,(2dcah)
01b4 77        ld      (hl),a		; write byte 2
01b5 23        inc     hl
01b6 3acb2d    ld      a,(2dcbh)
01b9 77        ld      (hl),a		; write byte 3
01ba 3efe      ld      a,0feh
01bc 0607      ld      b,07h
01be c36e02    jp      026eh		; send message for RS232 Status (7)

; Send Tablet Status
01c1 3602      ld      (hl),02h		; message length : 2 bytes
01c3 23        inc     hl
01c4 3a262e    ld      a,(2e26h)
01c7 e601      and     01h
01c9 cace01    jp      z,01ceh
01cc 3e02      ld      a,02h
01ce 77        ld      (hl),a		; write byte 1 - data ready ?
01cf 23        inc     hl
01d0 11e62d    ld      de,2de6h
01d3 1a        ld      a,(de)
01d4 77        ld      (hl),a		; write byte 2
01d5 97        sub     a
01d6 12        ld      (de),a
01d7 3efd      ld      a,0fdh
01d9 0608      ld      b,08h
01db c36e02    jp      026eh		; send message for Tablet status (8)

; Send Keyboard Status
01de 3601      ld      (hl),01h		; message length : 1 byte
01e0 23        inc     hl
01e1 3a262e    ld      a,(2e26h)
01e4 e604      and     04h
01e6 caeb01    jp      z,01ebh
01e9 3e01      ld      a,01h
01eb 77        ld      (hl),a		; write byte 1
01ec 3efb      ld      a,0fbh
01ee 0609      ld      b,09h
01f0 c36e02    jp      026eh		; send message for Keyboard status (9)

; Send voltage status
01f3 360c      ld      (hl),0ch
01f5 23        inc     hl
01f6 11e02f    ld      de,2fe0h
01f9 eb        ex      de,hl
01fa 010c00    ld      bc,000ch
01fd f3        di      
01fe edb0      ldir    
0200 97        sub     a
0201 32e32f    ld      (2fe3h),a
0204 32e82f    ld      (2fe8h),a
0207 32e92f    ld      (2fe9h),a
020a fb        ei      
020b 3ef7      ld      a,0f7h
020d 060d      ld      b,0dh
020f c36e02    jp      026eh				; start message for VoltageStatus

; Send Clock status
0212 3601      ld      (hl),01h
0214 23        inc     hl
0215 3a262e    ld      a,(2e26h)
0218 e608      and     08h
021a ca1f02    jp      z,021fh
021d 3e01      ld      a,01h
021f 77        ld      (hl),a
0220 3eef      ld      a,0efh
0222 060e      ld      b,0eh
0224 c36e02    jp      026eh				; start message for ClockStatus

; Send floppy status
0227 3a6a2f    ld      a,(2f6ah)			; message length : variable
022a 77        ld      (hl),a
022b 23        inc     hl
022c 11622f    ld      de,2f62h
022f 3a6a2f    ld      a,(2f6ah)
0232 47        ld      b,a
0233 f3        di      
0234 1a        ld      a,(de)
0235 13        inc     de
0236 77        ld      (hl),a
0237 23        inc     hl
0238 05        dec     b
0239 c23402    jp      nz,0234h
023c fb        ei      
023d 3edf      ld      a,0dfh
023f 0610      ld      b,10h
0241 c36e02    jp      026eh				; start message for FloppyStatus

; Send GPIB status
0244 3606      ld      (hl),06h				; message length : 6 bytes
0246 23        inc     hl
0247 112b2e    ld      de,2e2bh
024a 0606      ld      b,06h
024c f3        di      
024d 1a        ld      a,(de)
024e 13        inc     de
024f 77        ld      (hl),a
0250 23        inc     hl
0251 05        dec     b
0252 c24d02    jp      nz,024dh
0255 fb        ei      
0256 3ebf      ld      a,0bfh
0258 060f      ld      b,0fh
025a c36e02    jp      026eh				; start message for GPIBStatus

; Called when bit 7 of the device status byte is set.
025d f3        di							; don't mess with me, I'm updating state.
025e 21272e    ld      hl,2e27h				; load Z80 status byte
0261 7e        ld      a,(hl)
0262 f680      or      80h					; set bit 7 of Z80 status byte
0264 77        ld      (hl),a
0265 21282e    ld      hl,2e28h				; load device status byte
0268 7e        ld      a,(hl)
0269 e67f      and     7fh					; reset bit 7 of device status byte
026b 77        ld      (hl),a
026c fb        ei      
026d c9        ret     

; Sends a complete message to the PERQ
; b contains the message type
; a contains a mask for the device we're checking
; 2f33h contains the complete message to send
026e 21282e    ld      hl,2e28h		; load the device status byte
0271 f3        di					; don't interrupt me while I'm modifying flags
0272 a6        and     (hl)			; mask out the flag 
0273 77        ld      (hl),a		; and store it back
0274 fb        ei					; ok to interrupt now.
0275 78        ld      a,b			
0276 cd7d12    call    127dh		; send SOM for message type b
0279 3a332f    ld      a,(2f33h)	; get the first byte (message length)
027c 3c        inc     a			; add 1
027d cd9012    call    1290h		; send the rest of the message.
0280 c34001    jp      0140h		; back to the main Z80 loop!

; Called from Init
0283 e5        push    hl
0284 21692f    ld      hl,2f69h
0287 3605      ld      (hl),05h
0289 215b2f    ld      hl,2f5bh
028c 3607      ld      (hl),07h
028e 23        inc     hl
028f 361b      ld      (hl),1bh
0291 23        inc     hl
0292 3600      ld      (hl),00h
0294 23        inc     hl
0295 3600      ld      (hl),00h
0297 23        inc     hl
0298 361a      ld      (hl),1ah
029a 23        inc     hl
029b 3680      ld      (hl),80h
029d 23        inc     hl
029e 3601      ld      (hl),01h
02a0 21d402    ld      hl,02d4h
02a3 3e03      ld      a,03h
02a5 cde002    call    02e0h
02a8 21d802    ld      hl,02d8h
02ab 3e02      ld      a,02h
02ad fb        ei      
02ae cde002    call    02e0h
02b1 21692f    ld      hl,2f69h
02b4 7e        ld      a,(hl)
02b5 e67f      and     7fh
02b7 fe06      cp      06h
02b9 c2b102    jp      nz,02b1h
02bc f3        di      
02bd 21de02    ld      hl,02deh
02c0 3e01      ld      a,01h
02c2 cde002    call    02e0h
02c5 3e02      ld      a,02h
02c7 cd2403    call    0324h
02ca 3a692f    ld      a,(2f69h)
02cd f680      or      80h
02cf 32692f    ld      (2f69h),a
02d2 e1        pop     hl
02d3 c9        ret     

02d4 03        inc     bc			; garbage / data?
02d5 da24ff    jp      c,0ff24h
02d8 07        rlca    
02d9 00        nop     
02da ff        rst     38h
02db 04        inc     b
02dc 00        nop     
02dd ff        rst     38h
02de 08        ex      af,af'
02df ff        rst     38h

02e0 c5        push    bc
02e1 47        ld      b,a
02e2 dba8      in      a,(0a8h)
02e4 e6c0      and     0c0h
02e6 fe80      cp      80h
02e8 ca1a03    jp      z,031ah
02eb fec0      cp      0c0h
02ed c2e202    jp      nz,02e2h
02f0 3e07      ld      a,07h
02f2 cd2403    call    0324h
02f5 3e50      ld      a,50h
02f7 32622f    ld      (2f62h),a
02fa 3e01      ld      a,01h
02fc 326a2f    ld      (2f6ah),a
02ff 3e81      ld      a,81h
0301 32692f    ld      (2f69h),a
0304 21e52f    ld      hl,2fe5h
0307 7e        ld      a,(hl)
0308 fe02      cp      02h
030a c21803    jp      nz,0318h
030d 3600      ld      (hl),00h
030f 21292e    ld      hl,2e29h
0312 7e        ld      a,(hl)
0313 e6df      and     0dfh
0315 77        ld      (hl),a
0316 d3c8      out     (0c8h),a
0318 c1        pop     bc
0319 c9        ret     

031a 7e        ld      a,(hl)		; garbage / data ?
031b d3a9      out     (0a9h),a
031d 23        inc     hl
031e 05        dec     b
031f c2e202    jp      nz,02e2h
0322 c1        pop     bc
0323 c9        ret     

0324 e5        push    hl
0325 c5        push    bc
0326 47        ld      b,a
0327 21622f    ld      hl,2f62h
032a 0ead      ld      c,0adh
032c dba8      in      a,(0a8h)
032e e6c0      and     0c0h
0330 ca2c03    jp      z,032ch
0333 fec0      cp      0c0h
0335 ca3f03    jp      z,033fh
0338 0d        dec     c
0339 ca4903    jp      z,0349h
033c c32c03    jp      032ch
033f dba9      in      a,(0a9h)
0341 77        ld      (hl),a
0342 0ead      ld      c,0adh
0344 23        inc     hl
0345 05        dec     b
0346 c22c03    jp      nz,032ch
0349 c1        pop     bc
034a e1        pop     hl
034b c9        ret     

034c 3e01      ld      a,01h
034e 32692f    ld      (2f69h),a
0351 f5        push    af
0352 e5        push    hl
0353 21722f    ld      hl,2f72h
0356 71        ld      (hl),c
0357 2b        dec     hl
0358 70        ld      (hl),b
0359 2b        dec     hl
035a 360f      ld      (hl),0fh
035c 3e03      ld      a,03h
035e f3        di      
035f cde002    call    02e0h
0362 e1        pop     hl
0363 f1        pop     af
0364 c9        ret     

0365 f5        push    af
0366 c5        push    bc
0367 e5        push    hl
0368 3e02      ld      a,02h
036a 32692f    ld      (2f69h),a
036d cd4e0f    call    0f4eh
0370 3ae52f    ld      a,(2fe5h)
0373 fe02      cp      02h
0375 c2ce03    jp      nz,03ceh
0378 21d703    ld      hl,03d7h
037b 3e1c      ld      a,1ch
037d cd6406    call    0664h
0380 3a5e2f    ld      a,(2f5eh)
0383 0f        rrca    
0384 c67f      add     a,7fh
0386 32872f    ld      (2f87h),a
0389 328f2f    ld      (2f8fh),a
038c 21292e    ld      hl,2e29h
038f 7e        ld      a,(hl)
0390 f620      or      20h
0392 77        ld      (hl),a
0393 d3c8      out     (0c8h),a
0395 3a9f2f    ld      a,(2f9fh)
0398 cd5306    call    0653h
039b 21702f    ld      hl,2f70h
039e 3a5d2f    ld      a,(2f5dh)
03a1 f626      or      26h
03a3 77        ld      (hl),a
03a4 23        inc     hl
03a5 70        ld      (hl),b
03a6 23        inc     hl
03a7 71        ld      (hl),c
03a8 23        inc     hl
03a9 78        ld      a,b
03aa 0f        rrca    
03ab 0f        rrca    
03ac e601      and     01h
03ae 77        ld      (hl),a
03af 23        inc     hl
03b0 72        ld      (hl),d
03b1 23        inc     hl
03b2 3a5e2f    ld      a,(2f5eh)
03b5 77        ld      (hl),a
03b6 23        inc     hl
03b7 3a5f2f    ld      a,(2f5fh)
03ba 77        ld      (hl),a
03bb 23        inc     hl
03bc 3a5b2f    ld      a,(2f5bh)
03bf 77        ld      (hl),a
03c0 23        inc     hl
03c1 3a602f    ld      a,(2f60h)
03c4 77        ld      (hl),a
03c5 21702f    ld      hl,2f70h
03c8 3e09      ld      a,09h
03ca f3        di      
03cb cde002    call    02e0h
03ce e1        pop     hl
03cf c1        pop     bc
03d0 f1        pop     af
03d1 c9        ret     

03d2 00        nop					; garbage / data ?
03d3 00        nop     
03d4 00        nop     
03d5 00        nop     
03d6 00        nop     

03d7 01c3c3    ld      bc,0c3c3h	; data table ?
03da c3c3c3    jp      0c3c3h
03dd c3c7cb    jp      0cbc7h
03e0 6d        ld      l,l
03e1 a9        xor     c
03e2 7f        ld      a,a
03e3 00        nop     
03e4 6c        ld      l,h
03e5 00        nop     
03e6 9d        sbc     a,l
03e7 00        nop     
03e8 2c        inc     l
03e9 1e7f      ld      e,7fh
03eb 58        ld      e,b
03ec 50        ld      d,b
03ed 00        nop     
03ee 8a        adc     a,d
03ef cf        rst     08h
03f0 8b        adc     a,e
03f1 ab        xor     e
03f2 87        add     a,a

03f3 01c7cb    ld      bc,0cbc7h
03f6 6d        ld      l,l
03f7 a9        xor     c
03f8 7f        ld      a,a
03f9 00        nop     
03fa 6c        ld      l,h
03fb 00        nop     
03fc 50        ld      d,b
03fd 00        nop     
03fe 9d        sbc     a,l
03ff 00        nop     
0400 2c        inc     l
0401 1e7f      ld      e,7fh
0403 58        ld      e,b
0404 8a        adc     a,d
0405 cf        rst     08h
0406 ab        xor     e
0407 87        add     a,a

0408 f5        push    af
0409 c5        push    bc
040a e5        push    hl
040b 3e03      ld      a,03h
040d 32692f    ld      (2f69h),a
0410 cd4e0f    call    0f4eh
0413 3ae52f    ld      a,(2fe5h)
0416 fe02      cp      02h
0418 c27104    jp      nz,0471h
041b 217a04    ld      hl,047ah
041e 3e1d      ld      a,1dh
0420 cd6406    call    0664h
0423 3a5e2f    ld      a,(2f5eh)
0426 0f        rrca    
0427 c67f      add     a,7fh
0429 32862f    ld      (2f86h),a
042c 32902f    ld      (2f90h),a
042f 21292e    ld      hl,2e29h
0432 7e        ld      a,(hl)
0433 f620      or      20h
0435 77        ld      (hl),a
0436 d3c8      out     (0c8h),a
0438 3a9f2f    ld      a,(2f9fh)
043b cd5306    call    0653h
043e 21702f    ld      hl,2f70h
0441 3a5d2f    ld      a,(2f5dh)
0444 f605      or      05h
0446 77        ld      (hl),a
0447 23        inc     hl
0448 70        ld      (hl),b
0449 23        inc     hl
044a 71        ld      (hl),c
044b 23        inc     hl
044c 78        ld      a,b
044d 0f        rrca    
044e 0f        rrca    
044f e601      and     01h
0451 77        ld      (hl),a
0452 23        inc     hl
0453 72        ld      (hl),d
0454 23        inc     hl
0455 3a5e2f    ld      a,(2f5eh)
0458 77        ld      (hl),a
0459 23        inc     hl
045a 3a5f2f    ld      a,(2f5fh)
045d 77        ld      (hl),a
045e 23        inc     hl
045f 3a5b2f    ld      a,(2f5bh)
0462 77        ld      (hl),a
0463 23        inc     hl
0464 3a602f    ld      a,(2f60h)
0467 77        ld      (hl),a
0468 21702f    ld      hl,2f70h
046b 3e09      ld      a,09h
046d f3        di      
046e cde002    call    02e0h
0471 e1        pop     hl
0472 c1        pop     bc
0473 f1        pop     af
0474 c9        ret     

0475 00        nop						; garbage/data?
0476 00        nop     
0477 00        nop     
0478 00        nop     
0479 00        nop
     
047a c3c3c3    jp      0c3c3h			; data table?
047d c3c3c3    jp      0c3c3h
0480 c7        rst     00h
0481 cb6d      bit     5,l
0483 a9        xor     c
0484 7f        ld      a,a
0485 00        nop     
0486 6c        ld      l,h
0487 00        nop     
0488 cf        rst     08h
0489 019d00    ld      bc,009dh
048c 2c        inc     l
048d 1e7f      ld      e,7fh
048f 58        ld      e,b
0490 50        ld      d,b
0491 00        nop     
0492 8a        adc     a,d
0493 cf        rst     08h
0494 8b        adc     a,e
0495 ab        xor     e
0496 87        add     a,a
0497 c7        rst     00h

0498 cb6d      bit     5,l
049a a9        xor     c
049b 7f        ld      a,a
049c 00        nop     
049d 6c        ld      l,h
049e 00        nop     
049f cf        rst     08h
04a0 019d00    ld      bc,009dh
04a3 2c        inc     l
04a4 1e7f      ld      e,7fh
04a6 58        ld      e,b
04a7 50        ld      d,b
04a8 00        nop     
04a9 8a        adc     a,d
04aa cf        rst     08h
04ab ab        xor     e
04ac 87        add     a,a

04ad f5        push    af
04ae c5        push    bc
04af e5        push    hl
04b0 3e04      ld      a,04h
04b2 32692f    ld      (2f69h),a
04b5 cd4e0f    call    0f4eh
04b8 3ae52f    ld      a,(2fe5h)
04bb fe02      cp      02h
04bd c22905    jp      nz,0529h
04c0 213205    ld      hl,0532h
04c3 3e1c      ld      a,1ch
04c5 cd6406    call    0664h
04c8 3a5f2f    ld      a,(2f5fh)
04cb 07        rlca    
04cc 07        rlca    
04cd 3d        dec     a
04ce 32862f    ld      (2f86h),a
04d1 21292e    ld      hl,2e29h
04d4 7e        ld      a,(hl)
04d5 f620      or      20h
04d7 77        ld      (hl),a
04d8 d3c8      out     (0c8h),a
04da 3a9f2f    ld      a,(2f9fh)
04dd 217c2f    ld      hl,2f7ch
04e0 cd5306    call    0653h
04e3 1e01      ld      e,01h
04e5 21002c    ld      hl,2c00h
04e8 71        ld      (hl),c
04e9 23        inc     hl
04ea 78        ld      a,b
04eb 0f        rrca    
04ec 0f        rrca    
04ed e601      and     01h
04ef 77        ld      (hl),a
04f0 23        inc     hl
04f1 73        ld      (hl),e
04f2 23        inc     hl
04f3 3a5e2f    ld      a,(2f5eh)
04f6 77        ld      (hl),a
04f7 23        inc     hl
04f8 3a5f2f    ld      a,(2f5fh)
04fb bb        cp      e
04fc ca0305    jp      z,0503h
04ff 1c        inc     e
0500 c3e804    jp      04e8h
0503 21702f    ld      hl,2f70h
0506 3a5d2f    ld      a,(2f5dh)
0509 f60d      or      0dh
050b 77        ld      (hl),a
050c 23        inc     hl
050d 70        ld      (hl),b
050e 23        inc     hl
050f 3a5e2f    ld      a,(2f5eh)
0512 77        ld      (hl),a
0513 23        inc     hl
0514 3a5f2f    ld      a,(2f5fh)
0517 77        ld      (hl),a
0518 23        inc     hl
0519 3a5c2f    ld      a,(2f5ch)
051c 77        ld      (hl),a
051d 23        inc     hl
051e 3600      ld      (hl),00h
0520 21702f    ld      hl,2f70h
0523 3e06      ld      a,06h
0525 f3        di      
0526 cde002    call    02e0h
0529 e1        pop     hl
052a c1        pop     bc
052b f1        pop     af
052c c9        ret     

052d 00        nop					; garbage/data ?
052e 00        nop     
052f 00        nop     
0530 00        nop     
0531 00        nop     

0532 c3c3c3    jp      0c3c3h		; data table ?
0535 c3c3c3    jp      0c3c3h
0538 c7        rst     00h
0539 cb6d      bit     5,l
053b a9        xor     c
053c 67        ld      h,a
053d 00        nop     
053e 6c        ld      l,h
053f 00        nop     
0540 cf        rst     08h
0541 019d00    ld      bc,009dh
0544 2c        inc     l
0545 12        ld      (de),a
0546 58        ld      e,b
0547 50        ld      d,b
0548 00        nop     
0549 8a        adc     a,d
054a cf        rst     08h
054b 8b        adc     a,e
054c ab        xor     e
054d 87        add     a,a
054e c7        rst     00h
054f cb6d      bit     5,l
0551 a9        xor     c
0552 67        ld      h,a
0553 00        nop     
0554 6c        ld      l,h
0555 00        nop     
0556 cf        rst     08h
0557 019d00    ld      bc,009dh
055a 2c        inc     l
055b 12        ld      (de),a
055c 58        ld      e,b
055d 50        ld      d,b
055e 00        nop     
055f 8a        adc     a,d
0560 cf        rst     08h
0561 ab        xor     e
0562 87        add     a,a
0563 3e88      ld      a,88h
0565 32692f    ld      (2f69h),a
0568 3a792f    ld      a,(2f79h)
056b f604      or      04h
056d 32792f    ld      (2f79h),a
0570 c9        ret     

; Floppy Boot
0571 e5        push    hl
0572 f5        push    af
0573 d5        push    de
0574 3a792f    ld      a,(2f79h)
0577 e602      and     02h
0579 caf405    jp      z,05f4h
057c 0600      ld      b,00h
057e 0e01      ld      c,01h
0580 cdf705    call    05f7h
0583 0600      ld      b,00h
0585 0e01      ld      c,01h
0587 1601      ld      d,01h
0589 cd1006    call    0610h
058c 21002c    ld      hl,2c00h
058f 7e        ld      a,(hl)
0590 fe55      cp      55h			; check that first two bytes of boot sector are 55aa
0592 c2f405    jp      nz,05f4h
0595 23        inc     hl
0596 7e        ld      a,(hl)
0597 feaa      cp      0aah
0599 c2f405    jp      nz,05f4h		
059c 3e01      ld      a,01h
059e 326c2f    ld      (2f6ch),a
05a1 3e02      ld      a,02h
05a3 326d2f    ld      (2f6dh),a
05a6 3a6d2f    ld      a,(2f6dh)
05a9 fe1b      cp      1bh
05ab c2c005    jp      nz,05c0h
05ae 3a6c2f    ld      a,(2f6ch)
05b1 3c        inc     a
05b2 326c2f    ld      (2f6ch),a
05b5 4f        ld      c,a
05b6 0600      ld      b,00h
05b8 cdf705    call    05f7h
05bb 3e01      ld      a,01h
05bd 326d2f    ld      (2f6dh),a
05c0 3a6d2f    ld      a,(2f6dh)
05c3 57        ld      d,a
05c4 3a6c2f    ld      a,(2f6ch)
05c7 4f        ld      c,a
05c8 0600      ld      b,00h
05ca cd1006    call    0610h
05cd 3e55      ld      a,55h		; send message back to PERQ:
05cf cd8412    call    1284h		; SOM (0x55 in this case)
05d2 3e13      ld      a,13h		; 13h - good boot data
05d4 cd8412    call    1284h		; 80h - length of data (128 bytes)
05d7 3e80      ld      a,80h
05d9 cd8412    call    1284h
05dc 0680      ld      b,80h		; and send out those 128 bytes now.
05de 21002c    ld      hl,2c00h
05e1 7e        ld      a,(hl)
05e2 cd8412    call    1284h
05e5 23        inc     hl
05e6 05        dec     b
05e7 c2e105    jp      nz,05e1h
05ea 3a6d2f    ld      a,(2f6dh)
05ed 3c        inc     a
05ee 326d2f    ld      (2f6dh),a
05f1 c3a605    jp      05a6h
05f4 cd4606    call    0646h
05f7 d5        push    de
05f8 1605      ld      d,05h
05fa c5        push    bc
05fb cd4c03    call    034ch
05fe cd2c06    call    062ch
0601 b7        or      a
0602 ca0d06    jp      z,060dh
0605 c1        pop     bc
0606 15        dec     d
0607 c2fa05    jp      nz,05fah
060a cd4606    call    0646h
060d c1        pop     bc
060e d1        pop     de
060f c9        ret     

0610 e5        push    hl
0611 2e05      ld      l,05h
0613 c5        push    bc
0614 d5        push    de
0615 cd6503    call    0365h
0618 cd2c06    call    062ch
061b b7        or      a
061c ca2806    jp      z,0628h
061f d1        pop     de
0620 c1        pop     bc
0621 2d        dec     l
0622 c21306    jp      nz,0613h
0625 cd4606    call    0646h
0628 d1        pop     de
0629 c1        pop     bc
062a e1        pop     hl
062b c9        ret     

062c fb        ei      
062d 3a692f    ld      a,(2f69h)
0630 e680      and     80h
0632 ca2d06    jp      z,062dh
0635 3a622f    ld      a,(2f62h)
0638 07        rlca    
0639 da4306    jp      c,0643h
063c 07        rlca    
063d da4306    jp      c,0643h
0640 3e00      ld      a,00h
0642 c9        ret     

0643 3e01      ld      a,01h
0645 c9        ret     

0646 3e55      ld      a,55h
0648 cd8412    call    1284h
064b 3e12      ld      a,12h
064d cd8412    call    1284h
0650 c34001    jp      0140h			; jump to the start of the main loop.

0653 c5        push    bc
0654 e5        push    hl
0655 217c2f    ld      hl,2f7ch
0658 47        ld      b,a
0659 7e        ld      a,(hl)
065a d398      out     (98h),a
065c 23        inc     hl
065d 05        dec     b
065e c25906    jp      nz,0659h
0661 e1        pop     hl
0662 c1        pop     bc
0663 c9        ret     

0664 c5        push    bc
0665 d5        push    de
0666 e5        push    hl
0667 f5        push    af
0668 47        ld      b,a
0669 329f2f    ld      (2f9fh),a
066c 117c2f    ld      de,2f7ch
066f 7e        ld      a,(hl)
0670 23        inc     hl
0671 eb        ex      de,hl
0672 77        ld      (hl),a
0673 23        inc     hl
0674 eb        ex      de,hl
0675 05        dec     b
0676 c26f06    jp      nz,066fh
0679 f1        pop     af
067a e1        pop     hl
067b d1        pop     de
067c c1        pop     bc
067d c9        ret     

067e f5        push    af
067f c5        push    bc
0680 e5        push    hl
0681 21692f    ld      hl,2f69h
0684 7e        ld      a,(hl)
0685 e67f      and     7fh
0687 0601      ld      b,01h
0689 b8        cp      b
068a c29d06    jp      nz,069dh
068d 21de02    ld      hl,02deh
0690 3e01      ld      a,01h
0692 cde002    call    02e0h
0695 3e02      ld      a,02h
0697 326a2f    ld      (2f6ah),a
069a c31007    jp      0710h
069d 0602      ld      b,02h
069f b8        cp      b
06a0 c2ab06    jp      nz,06abh
06a3 3e07      ld      a,07h
06a5 326a2f    ld      (2f6ah),a
06a8 c31007    jp      0710h
06ab 0603      ld      b,03h
06ad b8        cp      b
06ae c2b906    jp      nz,06b9h
06b1 3e07      ld      a,07h
06b3 326a2f    ld      (2f6ah),a
06b6 c31007    jp      0710h
06b9 0604      ld      b,04h
06bb b8        cp      b
06bc c2c706    jp      nz,06c7h
06bf 3e07      ld      a,07h
06c1 326a2f    ld      (2f6ah),a
06c4 c31007    jp      0710h
06c7 0605      ld      b,05h
06c9 b8        cp      b
06ca c2da06    jp      nz,06dah
06cd 3e07      ld      a,07h
06cf 32692f    ld      (2f69h),a
06d2 3e00      ld      a,00h
06d4 326a2f    ld      (2f6ah),a
06d7 c31007    jp      0710h
06da 0607      ld      b,07h
06dc b8        cp      b
06dd c2f506    jp      nz,06f5h
06e0 21de02    ld      hl,02deh
06e3 3e01      ld      a,01h
06e5 cde002    call    02e0h
06e8 3e06      ld      a,06h
06ea 32692f    ld      (2f69h),a
06ed 3e02      ld      a,02h
06ef 326a2f    ld      (2f6ah),a
06f2 c31007    jp      0710h
06f5 0609      ld      b,09h
06f7 b8        cp      b
06f8 c2fe06    jp      nz,06feh
06fb c33807    jp      0738h
06fe 21de02    ld      hl,02deh
0701 3e01      ld      a,01h
0703 cde002    call    02e0h
0706 3e02      ld      a,02h
0708 326a2f    ld      (2f6ah),a
070b 3e46      ld      a,46h
070d 32692f    ld      (2f69h),a
0710 216a2f    ld      hl,2f6ah
0713 7e        ld      a,(hl)
0714 cd2403    call    0324h
0717 3a692f    ld      a,(2f69h)
071a e67f      and     7fh
071c e6bf      and     0bfh
071e fe06      cp      06h
0720 c23807    jp      nz,0738h
0723 3a692f    ld      a,(2f69h)
0726 e640      and     40h
0728 ca3807    jp      z,0738h
072b 3a622f    ld      a,(2f62h)
072e e608      and     08h
0730 c23807    jp      nz,0738h
0733 3e08      ld      a,08h
0735 32692f    ld      (2f69h),a
0738 21692f    ld      hl,2f69h
073b 7e        ld      a,(hl)
073c f680      or      80h
073e 77        ld      (hl),a
073f 21e52f    ld      hl,2fe5h
0742 7e        ld      a,(hl)
0743 fe02      cp      02h
0745 c25d07    jp      nz,075dh
0748 3a692f    ld      a,(2f69h)
074b e67f      and     7fh
074d fe02      cp      02h
074f ca5407    jp      z,0754h
0752 3600      ld      (hl),00h
0754 21292e    ld      hl,2e29h
0757 7e        ld      a,(hl)
0758 e607      and     07h
075a 77        ld      (hl),a
075b d3c8      out     (0c8h),a
075d e1        pop     hl
075e c1        pop     bc
075f f1        pop     af
0760 fb        ei      
0761 ed4d      reti    

; Called from main loop
; Floppy related
0763 f5        push    af
0764 e5        push    hl
0765 3a692f    ld      a,(2f69h)
0768 07        rlca    
0769 da6f07    jp      c,076fh
076c e1        pop     hl
076d f1        pop     af
076e c9        ret     

076f 3a692f    ld      a,(2f69h)
0772 e67f      and     7fh
0774 fe09      cp      09h
0776 c27f07    jp      nz,077fh
0779 cd7105    call    0571h
077c c33208    jp      0832h
077f 21622f    ld      hl,2f62h
0782 7e        ld      a,(hl)
0783 07        rlca    
0784 da9307    jp      c,0793h
0787 07        rlca    
0788 da9307    jp      c,0793h
078b 3a792f    ld      a,(2f79h)
078e e6fe      and     0feh
0790 c39807    jp      0798h
0793 3a792f    ld      a,(2f79h)
0796 f601      or      01h
0798 32792f    ld      (2f79h),a
079b 7e        ld      a,(hl)
079c e608      and     08h
079e c2a907    jp      nz,07a9h
07a1 3a792f    ld      a,(2f79h)
07a4 f602      or      02h
07a6 c3ae07    jp      07aeh
07a9 3a792f    ld      a,(2f79h)
07ac e6fd      and     0fdh
07ae 32792f    ld      (2f79h),a
07b1 3a692f    ld      a,(2f69h)
07b4 e67f      and     7fh
07b6 fe02      cp      02h
07b8 caef07    jp      z,07efh
07bb fe06      cp      06h
07bd cad707    jp      z,07d7h
07c0 fe08      cp      08h
07c2 c2ea07    jp      nz,07eah
07c5 f3        di      
07c6 3e05      ld      a,05h
07c8 32692f    ld      (2f69h),a
07cb 21d802    ld      hl,02d8h
07ce 3e02      ld      a,02h
07d0 cde002    call    02e0h
07d3 fb        ei      
07d4 c34001    jp      0140h
07d7 3a792f    ld      a,(2f79h)
07da e604      and     04h
07dc ca3208    jp      z,0832h
07df 3a792f    ld      a,(2f79h)
07e2 e6fb      and     0fbh
07e4 32792f    ld      (2f79h),a
07e7 c3ea07    jp      07eah

07ea 3e11      ld      a,11h
07ec c3f107    jp      07f1h

07ef 3e05      ld      a,05h
07f1 cd7d12    call    127dh			; send FloppyData message
07f4 3a792f    ld      a,(2f79h)
07f7 e601      and     01h
07f9 ca0108    jp      z,0801h
07fc 3e01      ld      a,01h
07fe c30308    jp      0803h
0801 3e00      ld      a,00h
0803 cd8412    call    1284h
0806 3a692f    ld      a,(2f69h)
0809 e67f      and     7fh
080b fe02      cp      02h
080d c23208    jp      nz,0832h
0810 3a602f    ld      a,(2f60h)
0813 feff      cp      0ffh
0815 c21908    jp      nz,0819h
0818 97        sub     a
0819 cd8412    call    1284h
081c 3a602f    ld      a,(2f60h)
081f feff      cp      0ffh
0821 c22508    jp      nz,0825h
0824 97        sub     a
0825 47        ld      b,a
0826 21002c    ld      hl,2c00h
0829 7e        ld      a,(hl)
082a cd8412    call    1284h
082d 23        inc     hl
082e 05        dec     b
082f c22908    jp      nz,0829h
0832 f3        di      
0833 21272e    ld      hl,2e27h
0836 3a792f    ld      a,(2f79h)
0839 e602      and     02h
083b ca4408    jp      z,0844h
083e 7e        ld      a,(hl)
083f f604      or      04h
0841 c34708    jp      0847h
0844 7e        ld      a,(hl)
0845 e6fb      and     0fbh
0847 f680      or      80h
0849 77        ld      (hl),a
084a 3e06      ld      a,06h
084c 32692f    ld      (2f69h),a
084f fb        ei      
0850 21e52f    ld      hl,2fe5h
0853 7e        ld      a,(hl)
0854 fe02      cp      02h
0856 c25b08    jp      nz,085bh
0859 3600      ld      (hl),00h
085b c34001    jp      0140h
085e 326b2f    ld      (2f6bh),a
0861 216708    ld      hl,0867h
0864 c38e10    jp      108eh
0867 326c2f    ld      (2f6ch),a
086a 217008    ld      hl,0870h
086d c38e10    jp      108eh
0870 326d2f    ld      (2f6dh),a
0873 217908    ld      hl,0879h
0876 c38e10    jp      108eh
0879 326e2f    ld      (2f6eh),a
087c 218208    ld      hl,0882h
087f c38e10    jp      108eh
0882 326f2f    ld      (2f6fh),a
0885 f3        di      
0886 3a6f2f    ld      a,(2f6fh)
0889 cd5c12    call    125ch
088c c5        push    bc
088d d5        push    de
088e 3a6e2f    ld      a,(2f6eh)
0891 3d        dec     a
0892 fe06      cp      06h
0894 d2fb08    jp      nc,08fbh
0897 07        rlca    
0898 5f        ld      e,a
0899 1600      ld      d,00h
089b 21fe08    ld      hl,08feh
089e 19        add     hl,de
089f 5e        ld      e,(hl)
08a0 23        inc     hl
08a1 56        ld      d,(hl)
08a2 eb        ex      de,hl
08a3 e9        jp      (hl)

08a4 216b2f    ld      hl,2f6bh
08a7 46        ld      b,(hl)
08a8 23        inc     hl
08a9 4e        ld      c,(hl)
08aa 23        inc     hl
08ab 56        ld      d,(hl)
08ac cd6503    call    0365h
08af c3fb08    jp      08fbh

08b2 3ae52f    ld      a,(2fe5h)
08b5 fe01      cp      01h
08b7 cacf08    jp      z,08cfh
08ba 21332e    ld      hl,2e33h
08bd 11002c    ld      de,2c00h
08c0 3a602f    ld      a,(2f60h)
08c3 47        ld      b,a
08c4 04        inc     b
08c5 7e        ld      a,(hl)
08c6 23        inc     hl
08c7 eb        ex      de,hl
08c8 77        ld      (hl),a
08c9 23        inc     hl
08ca eb        ex      de,hl
08cb 05        dec     b
08cc c2c508    jp      nz,08c5h
08cf 216b2f    ld      hl,2f6bh
08d2 46        ld      b,(hl)
08d3 23        inc     hl
08d4 4e        ld      c,(hl)
08d5 23        inc     hl
08d6 56        ld      d,(hl)
08d7 cd0804    call    0408h
08da c3fb08    jp      08fbh

08dd 216b2f    ld      hl,2f6bh
08e0 46        ld      b,(hl)
08e1 23        inc     hl
08e2 4e        ld      c,(hl)
08e3 cdad04    call    04adh
08e6 c3fb08    jp      08fbh
08e9 216b2f    ld      hl,2f6bh
08ec 46        ld      b,(hl)
08ed 23        inc     hl
08ee 4e        ld      c,(hl)
08ef cd4c03    call    034ch
08f2 c3fb08    jp      08fbh

08f5 cd6305    call    0563h
08f8 c3fb08    jp      08fbh
08fb d1        pop     de
08fc c1        pop     bc
08fd c9        ret     

08fe a4        and     h			; table of addresses for functions? (see above)
08ff 08        ex      af,af'
0900 b2        or      d
0901 08        ex      af,af'
0902 dd08      ex      af,af'
0904 e9        jp      (hl)
0905 08        ex      af,af'
0906 f5        push    af
0907 08        ex      af,af'

0908 e5        push    hl			; looks like possibly real code from here on in, but who calls it?
0909 f3        di      
090a cd5c12    call    125ch
090d 21332e    ld      hl,2e33h
0910 7e        ld      a,(hl)
0911 325d2f    ld      (2f5dh),a
0914 23        inc     hl
0915 7e        ld      a,(hl)
0916 32612f    ld      (2f61h),a
0919 23        inc     hl
091a 7e        ld      a,(hl)
091b e601      and     01h
091d c22809    jp      nz,0928h
0920 3a292e    ld      a,(2e29h)
0923 e6fe      and     0feh
0925 c32d09    jp      092dh
0928 3a292e    ld      a,(2e29h)
092b f601      or      01h
092d 32292e    ld      (2e29h),a
0930 d3c8      out     (0c8h),a
0932 3a5d2f    ld      a,(2f5dh)
0935 fe40      cp      40h
0937 ca5609    jp      z,0956h
093a 3e07      ld      a,07h
093c 325b2f    ld      (2f5bh),a
093f 3e1b      ld      a,1bh
0941 325c2f    ld      (2f5ch),a
0944 3e00      ld      a,00h
0946 325e2f    ld      (2f5eh),a
0949 3e1a      ld      a,1ah
094b 325f2f    ld      (2f5fh),a
094e 3e80      ld      a,80h
0950 32602f    ld      (2f60h),a
0953 c36f09    jp      096fh
0956 3e0e      ld      a,0eh
0958 325b2f    ld      (2f5bh),a
095b 3e36      ld      a,36h
095d 325c2f    ld      (2f5ch),a
0960 3e01      ld      a,01h
0962 325e2f    ld      (2f5eh),a
0965 3e1a      ld      a,1ah
0967 325f2f    ld      (2f5fh),a
096a 3eff      ld      a,0ffh
096c 32602f    ld      (2f60h),a
096f 21692f    ld      hl,2f69h
0972 7e        ld      a,(hl)
0973 f680      or      80h
0975 77        ld      (hl),a
0976 3e00      ld      a,00h
0978 326a2f    ld      (2f6ah),a
097b e1        pop     hl
097c c9        ret     

097d 3e09      ld      a,09h
097f f680      or      80h
0981 32692f    ld      (2f69h),a
0984 c9        ret     

; Called from Init
0985 f5        push    af
0986 c5        push    bc
0987 d5        push    de
0988 e5        push    hl
0989 97        sub     a
098a d3c0      out     (0c0h),a
098c 32d52d    ld      (2dd5h),a
098f 21d52d    ld      hl,2dd5h
0992 11d62d    ld      de,2dd6h
0995 011100    ld      bc,0011h
0998 edb0      ldir    
099a 3e02      ld      a,02h
099c 32e52d    ld      (2de5h),a
099f e1        pop     hl
09a0 d1        pop     de
09a1 c1        pop     bc
09a2 f1        pop     af
09a3 c9        ret     

; Called from main loop
; tablet related
09a4 3a262e    ld      a,(2e26h)
09a7 e601      and     01h
09a9 c8        ret     z
09aa 21d52d    ld      hl,2dd5h
09ad 7e        ld      a,(hl)
09ae fe01      cp      01h
09b0 c0        ret     nz
09b1 f3        di      
09b2 3600      ld      (hl),00h
09b4 23        inc     hl
09b5 7e        ld      a,(hl)
09b6 32db2d    ld      (2ddbh),a
09b9 23        inc     hl
09ba 7e        ld      a,(hl)
09bb 32da2d    ld      (2ddah),a
09be 23        inc     hl
09bf 7e        ld      a,(hl)
09c0 32dd2d    ld      (2dddh),a
09c3 e6e0      and     0e0h
09c5 c2d009    jp      nz,09d0h
09c8 3adb2d    ld      a,(2ddbh)
09cb f680      or      80h
09cd 32db2d    ld      (2ddbh),a
09d0 23        inc     hl
09d1 7e        ld      a,(hl)
09d2 32dc2d    ld      (2ddch),a
09d5 fb        ei      
09d6 3e03      ld      a,03h
09d8 cd7d12    call    127dh			; send TabletData message
09db 21da2d    ld      hl,2ddah
09de 11332f    ld      de,2f33h
09e1 010400    ld      bc,0004h
09e4 edb0      ldir    
09e6 3e05      ld      a,05h
09e8 cd9012    call    1290h
09eb c34001    jp      0140h

; ISR for ?
09ee f5        push    af
09ef 97        sub     a
09f0 d3c0      out     (0c0h),a
09f2 f1        pop     af
09f3 fb        ei      
09f4 ed4d      reti    

; ISR for ?
09f6 f5        push    af
09f7 3e03      ld      a,03h
09f9 d393      out     (93h),a
09fb f1        pop     af

; ISR for ? - does nothing
09fc fb        ei      
09fd ed4d      reti    

; Called from Init - initialize keyboard buffer
09ff db80      in      a,(80h)			; ok, I give, why are we reading port 80 here?
0a01 21d32d    ld      hl,2dd3h
0a04 36c0      ld      (hl),0c0h
0a06 23        inc     hl
0a07 36c0      ld      (hl),0c0h		; init buffer at 2dd3h with two c0 entries
0a09 c9        ret     

; Called from main loop
; Keyboard related
0a0a 3a262e    ld      a,(2e26h)
0a0d e604      and     04h
0a0f c8        ret     z

0a10 cd210a    call    0a21h
0a13 d8        ret     c

0a14 f5        push    af
0a15 3e01      ld      a,01h
0a17 cd7d12    call    127dh			; send KeyboardData message
0a1a f1        pop     af
0a1b cd8412    call    1284h
0a1e c34001    jp      0140h			; and return to the top of the main loop.

0a21 e5        push    hl
0a22 21d32d    ld      hl,2dd3h
0a25 7e        ld      a,(hl)
0a26 23        inc     hl
0a27 be        cp      (hl)
0a28 ca380a    jp      z,0a38h
0a2b 7e        ld      a,(hl)
0a2c 3c        inc     a
0a2d e607      and     07h
0a2f f6c0      or      0c0h
0a31 f3        di      
0a32 77        ld      (hl),a
0a33 6f        ld      l,a
0a34 7e        ld      a,(hl)
0a35 fb        ei      
0a36 e1        pop     hl
0a37 c9        ret     

0a38 37        scf     
0a39 e1        pop     hl
0a3a c9        ret     

; ISR for Keyboard
0a3b f5        push    af
0a3c e5        push    hl
0a3d db80      in      a,(80h)
0a3f f5        push    af
0a40 21d32d    ld      hl,2dd3h
0a43 7e        ld      a,(hl)
0a44 3c        inc     a
0a45 e607      and     07h
0a47 f6c0      or      0c0h
0a49 23        inc     hl
0a4a be        cp      (hl)
0a4b ca580a    jp      z,0a58h
0a4e 2b        dec     hl
0a4f 77        ld      (hl),a
0a50 6f        ld      l,a
0a51 f1        pop     af
0a52 77        ld      (hl),a
0a53 e1        pop     hl
0a54 f1        pop     af
0a55 fb        ei      
0a56 ed4d      reti    
0a58 e1        pop     hl
0a59 c3530a    jp      0a53h

; Called from Init
0a5c 211f2e    ld      hl,2e1fh
0a5f 97        sub     a
0a60 77        ld      (hl),a
0a61 d3d8      out     (0d8h),a
0a63 3e50      ld      a,50h
0a65 d390      out     (90h),a
0a67 3e03      ld      a,03h
0a69 d393      out     (93h),a
0a6b d392      out     (92h),a
0a6d c9        ret     

; Called from main loop
; Hard-disk related
0a6e 211f2e    ld      hl,2e1fh
0a71 7e        ld      a,(hl)
0a72 e602      and     02h
0a74 c8        ret     z

0a75 7e        ld      a,(hl)
0a76 e6fd      and     0fdh
0a78 77        ld      (hl),a
0a79 3e0a      ld      a,0ah
0a7b cd7d12    call    127dh				; send SeekComplete message
0a7e c34001    jp      0140h				; and jump back to the top of the main loop.

0a81 e5        push    hl
0a82 f5        push    af
0a83 211f2e    ld      hl,2e1fh
0a86 7e        ld      a,(hl)
0a87 f601      or      01h
0a89 77        ld      (hl),a
0a8a 23        inc     hl
0a8b 3ed7      ld      a,0d7h
0a8d d392      out     (92h),a
0a8f f1        pop     af
0a90 77        ld      (hl),a
0a91 d392      out     (92h),a
0a93 3e08      ld      a,08h
0a95 d3d8      out     (0d8h),a
0a97 e1        pop     hl
0a98 c9        ret     

; ISR for Hard disk ?
0a99 f5        push    af
0a9a d5        push    de
0a9b e5        push    hl
0a9c 211f2e    ld      hl,2e1fh
0a9f 7e        ld      a,(hl)
0aa0 e601      and     01h
0aa2 cae20a    jp      z,0ae2h
0aa5 7e        ld      a,(hl)
0aa6 e6fe      and     0feh
0aa8 77        ld      (hl),a
0aa9 97        sub     a
0aaa d3d8      out     (0d8h),a
0aac 23        inc     hl
0aad 7e        ld      a,(hl)
0aae fe20      cp      20h
0ab0 d2be0a    jp      nc,0abeh
0ab3 21fc0a    ld      hl,0afch
0ab6 1600      ld      d,00h
0ab8 5f        ld      e,a
0ab9 19        add     hl,de
0aba 6e        ld      l,(hl)
0abb c3c10a    jp      0ac1h
0abe c634      add     a,34h
0ac0 6f        ld      l,a
0ac1 2600      ld      h,00h
0ac3 e5        push    hl
0ac4 29        add     hl,hl
0ac5 29        add     hl,hl
0ac6 d1        pop     de
0ac7 19        add     hl,de
0ac8 3ea7      ld      a,0a7h
0aca d392      out     (92h),a
0acc 7d        ld      a,l
0acd a7        and     a
0ace cad20a    jp      z,0ad2h
0ad1 24        inc     h
0ad2 d392      out     (92h),a
0ad4 7c        ld      a,h
0ad5 32202e    ld      (2e20h),a
0ad8 3ea5      ld      a,0a5h
0ada d392      out     (92h),a
0adc 97        sub     a
0add d392      out     (92h),a
0adf c3f70a    jp      0af7h
0ae2 23        inc     hl
0ae3 35        dec     (hl)
0ae4 c2f70a    jp      nz,0af7h
0ae7 2b        dec     hl
0ae8 7e        ld      a,(hl)
0ae9 f602      or      02h
0aeb 77        ld      (hl),a
0aec 3e03      ld      a,03h
0aee d392      out     (92h),a
0af0 21272e    ld      hl,2e27h
0af3 7e        ld      a,(hl)
0af4 f690      or      90h
0af6 77        ld      (hl),a
0af7 e1        pop     hl
0af8 d1        pop     de
0af9 f1        pop     af
0afa fb        ei      
0afb ed4d      reti    

0afd 2a2c2e    ld      hl,(2e2ch)		; garbage / data?
0b00 3032      jr      nc,0b34h
0b02 34        inc     (hl)
0b03 3638      ld      (hl),38h
0b05 3a3a3c    ld      a,(3c3ah)
0b08 3e3e      ld      a,3eh
0b0a 40        ld      b,b
0b0b 42        ld      b,d
0b0c 42        ld      b,d
0b0d 44        ld      b,h
0b0e 46        ld      b,(hl)
0b0f 46        ld      b,(hl)
0b10 48        ld      c,b
0b11 48        ld      c,b
0b12 4a        ld      c,d
0b13 4c        ld      c,h
0b14 4c        ld      c,h
0b15 4e        ld      c,(hl)
0b16 4e        ld      c,(hl)
0b17 50        ld      d,b
0b18 50        ld      d,b
0b19 52        ld      d,d
0b1a 52        ld      d,d
0b1b 52        ld      d,d

; Called from Init
0b1c 21c82d    ld      hl,2dc8h
0b1f 3641      ld      (hl),41h
0b21 23        inc     hl
0b22 3641      ld      (hl),41h
0b24 23        inc     hl
0b25 3602      ld      (hl),02h
0b27 23        inc     hl
0b28 36f4      ld      (hl),0f4h
0b2a 23        inc     hl
0b2b 3600      ld      (hl),00h
0b2d 21cd2d    ld      hl,2dcdh
0b30 3600      ld      (hl),00h
0b32 23        inc     hl
0b33 3660      ld      (hl),60h
0b35 23        inc     hl
0b36 3660      ld      (hl),60h
0b38 e5        push    hl
0b39 21ca2d    ld      hl,2dcah
0b3c 3e17      ld      a,17h
0b3e d390      out     (90h),a
0b40 7e        ld      a,(hl)
0b41 d390      out     (90h),a
0b43 23        inc     hl
0b44 3e18      ld      a,18h
0b46 d3b1      out     (0b1h),a
0b48 3e10      ld      a,10h
0b4a d3b1      out     (0b1h),a
0b4c 3e04      ld      a,04h
0b4e d3b1      out     (0b1h),a
0b50 7e        ld      a,(hl)
0b51 e60f      and     0fh
0b53 f640      or      40h
0b55 d3b1      out     (0b1h),a
0b57 3e03      ld      a,03h
0b59 d3b1      out     (0b1h),a
0b5b 7e        ld      a,(hl)
0b5c e6c0      and     0c0h
0b5e f621      or      21h
0b60 d3b1      out     (0b1h),a
0b62 3e05      ld      a,05h
0b64 d3b1      out     (0b1h),a
0b66 7e        ld      a,(hl)
0b67 e630      and     30h
0b69 07        rlca    
0b6a f68a      or      8ah
0b6c d3b1      out     (0b1h),a
0b6e 3e10      ld      a,10h
0b70 d3b1      out     (0b1h),a
0b72 3e01      ld      a,01h
0b74 d3b1      out     (0b1h),a
0b76 3a262e    ld      a,(2e26h)
0b79 e602      and     02h
0b7b 3e1b      ld      a,1bh
0b7d c2820b    jp      nz,0b82h
0b80 3e02      ld      a,02h
0b82 d3b1      out     (0b1h),a
0b84 e1        pop     hl
0b85 c9        ret     

; Called from main loop
; RS232 related
0b86 cdb70b    call    0bb7h
0b89 d8        ret     c
0b8a 21342f    ld      hl,2f34h
0b8d 1601      ld      d,01h
0b8f c39f0b    jp      0b9fh
0b92 7a        ld      a,d
0b93 fe10      cp      10h
0b95 caa60b    jp      z,0ba6h
0b98 cdb70b    call    0bb7h
0b9b daa60b    jp      c,0ba6h
0b9e 14        inc     d
0b9f 71        ld      (hl),c
0ba0 23        inc     hl
0ba1 70        ld      (hl),b
0ba2 23        inc     hl
0ba3 c3920b    jp      0b92h
0ba6 3e02      ld      a,02h
0ba8 cd7d12    call    127dh					; send RS232Data message
0bab 7a        ld      a,d
0bac 07        rlca    
0bad 32332f    ld      (2f33h),a
0bb0 3c        inc     a
0bb1 cd9012    call    1290h
0bb4 c34001    jp      0140h	; return to top of main loop

0bb7 e5        push    hl
0bb8 21c82d    ld      hl,2dc8h
0bbb 7e        ld      a,(hl)
0bbc 23        inc     hl
0bbd be        cp      (hl)
0bbe cad10b    jp      z,0bd1h
0bc1 7e        ld      a,(hl)
0bc2 c602      add     a,02h
0bc4 e61f      and     1fh
0bc6 f640      or      40h
0bc8 f3        di      
0bc9 77        ld      (hl),a
0bca 6f        ld      l,a
0bcb 4e        ld      c,(hl)
0bcc 2b        dec     hl
0bcd 46        ld      b,(hl)
0bce fb        ei      
0bcf e1        pop     hl
0bd0 c9        ret     

0bd1 37        scf     
0bd2 e1        pop     hl
0bd3 c9        ret     

; ISR for ?
0bd4 f5        push    af
0bd5 c5        push    bc
0bd6 3e01      ld      a,01h
0bd8 d3b1      out     (0b1h),a
0bda dbb1      in      a,(0b1h)
0bdc 0f        rrca    
0bdd 0f        rrca    
0bde 0f        rrca    
0bdf 0f        rrca    
0be0 e607      and     07h
0be2 4f        ld      c,a
0be3 dbb0      in      a,(0b0h)
0be5 47        ld      b,a
0be6 3e30      ld      a,30h
0be8 d3b1      out     (0b1h),a
0bea c3f40b    jp      0bf4h

; ISR for ?
0bed f5        push    af
0bee c5        push    bc
0bef 0e00      ld      c,00h
0bf1 dbb0      in      a,(0b0h)
0bf3 47        ld      b,a
0bf4 e5        push    hl
0bf5 21c82d    ld      hl,2dc8h
0bf8 7e        ld      a,(hl)
0bf9 c602      add     a,02h
0bfb e61f      and     1fh
0bfd f640      or      40h
0bff 23        inc     hl
0c00 be        cp      (hl)
0c01 ca180c    jp      z,0c18h
0c04 2b        dec     hl
0c05 77        ld      (hl),a
0c06 6f        ld      l,a
0c07 3acc2d    ld      a,(2dcch)
0c0a b1        or      c
0c0b 77        ld      (hl),a
0c0c 97        sub     a
0c0d 32cc2d    ld      (2dcch),a
0c10 2b        dec     hl
0c11 70        ld      (hl),b
0c12 e1        pop     hl
0c13 c1        pop     bc
0c14 f1        pop     af
0c15 fb        ei      
0c16 ed4d      reti    

0c18 3e02      ld      a,02h
0c1a 32cc2d    ld      (2dcch),a
0c1d c3120c    jp      0c12h
0c20 e5        push    hl
0c21 d5        push    de
0c22 c5        push    bc
0c23 47        ld      b,a
0c24 11332e    ld      de,2e33h
0c27 21cd2d    ld      hl,2dcdh
0c2a 7e        ld      a,(hl)
0c2b 80        add     a,b
0c2c 77        ld      (hl),a
0c2d 23        inc     hl
0c2e 6e        ld      l,(hl)
0c2f 7d        ld      a,l
0c30 3c        inc     a
0c31 e61f      and     1fh
0c33 f660      or      60h
0c35 6f        ld      l,a
0c36 1a        ld      a,(de)
0c37 77        ld      (hl),a
0c38 13        inc     de
0c39 05        dec     b
0c3a c22f0c    jp      nz,0c2fh
0c3d 7d        ld      a,l
0c3e 2ece      ld      l,0ceh
0c40 77        ld      (hl),a
0c41 dbb1      in      a,(0b1h)
0c43 e604      and     04h
0c45 ca570c    jp      z,0c57h
0c48 23        inc     hl
0c49 7e        ld      a,(hl)
0c4a 3c        inc     a
0c4b e61f      and     1fh
0c4d f660      or      60h
0c4f 77        ld      (hl),a
0c50 6f        ld      l,a
0c51 7e        ld      a,(hl)
0c52 d3b0      out     (0b0h),a
0c54 2ecd      ld      l,0cdh
0c56 35        dec     (hl)
0c57 2ecd      ld      l,0cdh
0c59 7e        ld      a,(hl)
0c5a fe11      cp      11h
0c5c d2660c    jp      nc,0c66h
0c5f 21272e    ld      hl,2e27h
0c62 7e        ld      a,(hl)
0c63 f681      or      81h
0c65 77        ld      (hl),a
0c66 c1        pop     bc
0c67 d1        pop     de
0c68 e1        pop     hl
0c69 c9        ret     

; ISR for ?
0c6a f5        push    af
0c6b e5        push    hl
0c6c 21cd2d    ld      hl,2dcdh
0c6f 7e        ld      a,(hl)
0c70 a7        and     a
0c71 ca960c    jp      z,0c96h
0c74 2ecf      ld      l,0cfh
0c76 7e        ld      a,(hl)
0c77 3c        inc     a
0c78 e61f      and     1fh
0c7a f660      or      60h
0c7c 77        ld      (hl),a
0c7d 6f        ld      l,a
0c7e 7e        ld      a,(hl)
0c7f d3b0      out     (0b0h),a
0c81 2ecd      ld      l,0cdh
0c83 35        dec     (hl)
0c84 7e        ld      a,(hl)
0c85 fe10      cp      10h
0c87 c2910c    jp      nz,0c91h
0c8a 21272e    ld      hl,2e27h
0c8d 7e        ld      a,(hl)
0c8e f681      or      81h
0c90 77        ld      (hl),a
0c91 e1        pop     hl
0c92 f1        pop     af
0c93 fb        ei      
0c94 ed4d      reti    

0c96 3e28      ld      a,28h
0c98 d3b1      out     (0b1h),a
0c9a c3910c    jp      0c91h

; Called from Init
0c9d 216b0e    ld      hl,0e6bh
0ca0 11002d    ld      de,2d00h
0ca3 011c00    ld      bc,001ch
0ca6 edb0      ldir    
0ca8 3e17      ld      a,17h
0caa d391      out     (91h),a
0cac 3e05      ld      a,05h
0cae d391      out     (91h),a
0cb0 32e12f    ld      (2fe1h),a
0cb3 32e22f    ld      (2fe2h),a
0cb6 97        sub     a
0cb7 32e02f    ld      (2fe0h),a
0cba 32e32f    ld      (2fe3h),a
0cbd 32e82f    ld      (2fe8h),a
0cc0 32e92f    ld      (2fe9h),a
0cc3 32ea2f    ld      (2feah),a
0cc6 32eb2f    ld      (2febh),a
0cc9 3e02      ld      a,02h
0ccb 32e72f    ld      (2fe7h),a
0cce 21d80c    ld      hl,0cd8h
0cd1 0eb3      ld      c,0b3h
0cd3 0611      ld      b,11h
0cd5 edb3      otir    
0cd7 c9        ret     

; this looks to be misaligned by one byte
0cd8 1802      jr      0cdch
0cda 40        ld      b,b
0cdb 1004      djnz    0ce1h
0cdd 00        nop     
0cde 0655      ld      b,55h
0ce0 07        rlca    
0ce1 81        add     a,c
0ce2 03        inc     bc
0ce3 c0        ret     nz
0ce4 05        dec     b
0ce5 e21001    jp      po,0110h


0ce8 ddf5      push    af
0cea c5        push    bc
0ceb e5        push    hl
0cec dbb3      in      a,(0b3h)
0cee 4f        ld      c,a
0cef e640      and     40h
0cf1 caf80c    jp      z,0cf8h
0cf4 3ec0      ld      a,0c0h
0cf6 d3b3      out     (0b3h),a
0cf8 79        ld      a,c
0cf9 e608      and     08h
0cfb ca020d    jp      z,0d02h
0cfe 21252e    ld      hl,2e25h
0d01 34        inc     (hl)
0d02 3e10      ld      a,10h
0d04 d3b3      out     (0b3h),a
0d06 e1        pop     hl
0d07 c1        pop     bc
0d08 f1        pop     af
0d09 fb        ei      
0d0a ed4d      reti    

; ISR for ?
0d0c f5        push    af
0d0d c5        push    bc
0d0e d5        push    de
0d0f e5        push    hl
0d10 dbb2      in      a,(0b2h)
0d12 2f        cpl     
0d13 47        ld      b,a
0d14 21de2d    ld      hl,2ddeh
0d17 34        inc     (hl)
0d18 7e        ld      a,(hl)
0d19 21de2d    ld      hl,2ddeh
0d1c 85        add     a,l
0d1d 6f        ld      l,a
0d1e 70        ld      (hl),b
0d1f 21de2d    ld      hl,2ddeh
0d22 7e        ld      a,(hl)
0d23 fe06      cp      06h
0d25 c2660d    jp      nz,0d66h
0d28 3e03      ld      a,03h
0d2a d3b3      out     (0b3h),a
0d2c 3ec0      ld      a,0c0h
0d2e d3b3      out     (0b3h),a
0d30 dbb3      in      a,(0b3h)
0d32 cb47      bit     0,a
0d34 ca3c0d    jp      z,0d3ch
0d37 dbb2      in      a,(0b2h)
0d39 c3300d    jp      0d30h
0d3c 3e03      ld      a,03h
0d3e d3b3      out     (0b3h),a
0d40 3ed1      ld      a,0d1h
0d42 d3b3      out     (0b3h),a
0d44 3e10      ld      a,10h
0d46 d3b3      out     (0b3h),a
0d48 3600      ld      (hl),00h
0d4a 21e52d    ld      hl,2de5h
0d4d 7e        ld      a,(hl)
0d4e fe00      cp      00h
0d50 ca570d    jp      z,0d57h
0d53 35        dec     (hl)
0d54 c3660d    jp      0d66h
0d57 11d52d    ld      de,2dd5h
0d5a 3e01      ld      a,01h
0d5c 12        ld      (de),a
0d5d 13        inc     de
0d5e 21df2d    ld      hl,2ddfh
0d61 010400    ld      bc,0004h
0d64 edb0      ldir    
0d66 e1        pop     hl
0d67 d1        pop     de
0d68 c1        pop     bc
0d69 f1        pop     af
0d6a fb        ei      
0d6b ed4d      reti    

; ISR for ?
0d6d f5        push    af
0d6e e5        push    hl
0d6f 3e01      ld      a,01h
0d71 d3b3      out     (0b3h),a
0d73 dbb3      in      a,(0b3h)
0d75 21e52d    ld      hl,2de5h
0d78 3602      ld      (hl),02h
0d7a 23        inc     hl
0d7b 34        inc     (hl)
0d7c dbb2      in      a,(0b2h)
0d7e 3e30      ld      a,30h
0d80 d3b3      out     (0b3h),a
0d82 e1        pop     hl
0d83 f1        pop     af
0d84 fb        ei      
0d85 ed4d      reti    

; ISR for ? - does nothing
0d87 fb        ei      
0d88 ed4d      reti    

0d8a f5        push    af
0d8b e5        push    hl
0d8c c5        push    bc
0d8d 21272e    ld      hl,2e27h
0d90 7e        ld      a,(hl)
0d91 e6fd      and     0fdh
0d93 77        ld      (hl),a
0d94 dba0      in      a,(0a0h)
0d96 47        ld      b,a
0d97 fe20      cp      20h
0d99 caa60d    jp      z,0da6h
0d9c 21e32f    ld      hl,2fe3h
0d9f 7e        ld      a,(hl)
0da0 f601      or      01h
0da2 77        ld      (hl),a
0da3 c3440e    jp      0e44h
0da6 21e52f    ld      hl,2fe5h
0da9 7e        ld      a,(hl)
0daa fe02      cp      02h
0dac c2b90d    jp      nz,0db9h
0daf 21e32f    ld      hl,2fe3h
0db2 7e        ld      a,(hl)
0db3 f602      or      02h
0db5 77        ld      (hl),a
0db6 c3440e    jp      0e44h
0db9 3601      ld      (hl),01h
0dbb 2ae82f    ld      hl,(2fe8h)
0dbe 23        inc     hl
0dbf 22e82f    ld      (2fe8h),hl
0dc2 262c      ld      h,2ch
0dc4 3aea2f    ld      a,(2feah)
0dc7 6f        ld      l,a
0dc8 0ea0      ld      c,0a0h
0dca edb2      inir    
0dcc 7d        ld      a,l
0dcd 32ea2f    ld      (2feah),a
0dd0 e67f      and     7fh
0dd2 c2410e    jp      nz,0e41h
0dd5 3ae42f    ld      a,(2fe4h)
0dd8 fe00      cp      00h
0dda c2280e    jp      nz,0e28h
0ddd 21292e    ld      hl,2e29h
0de0 7e        ld      a,(hl)
0de1 f6a0      or      0a0h
0de3 77        ld      (hl),a
0de4 d3c8      out     (0c8h),a
0de6 3aeb2f    ld      a,(2febh)
0de9 32112d    ld      (2d11h),a
0dec 21002d    ld      hl,2d00h
0def 0e98      ld      c,98h
0df1 061c      ld      b,1ch
0df3 edb3      otir    
0df5 21e02f    ld      hl,2fe0h
0df8 7e        ld      a,(hl)
0df9 fe00      cp      00h
0dfb ca0d0e    jp      z,0e0dh
0dfe 3e17      ld      a,17h
0e00 d391      out     (91h),a
0e02 3ae12f    ld      a,(2fe1h)
0e05 d391      out     (91h),a
0e07 32e22f    ld      (2fe2h),a
0e0a 3e00      ld      a,00h
0e0c 77        ld      (hl),a
0e0d 3e05      ld      a,05h
0e0f d3b3      out     (0b3h),a
0e11 3eea      ld      a,0eah
0e13 d3b3      out     (0b3h),a
0e15 3e10      ld      a,10h
0e17 d3b3      out     (0b3h),a
0e19 3e01      ld      a,01h
0e1b 32e42f    ld      (2fe4h),a
0e1e 21eb2f    ld      hl,2febh
0e21 7e        ld      a,(hl)
0e22 c680      add     a,80h
0e24 77        ld      (hl),a
0e25 c32e0e    jp      0e2eh
0e28 3aeb2f    ld      a,(2febh)
0e2b 32112d    ld      (2d11h),a
0e2e 21e72f    ld      hl,2fe7h
0e31 7e        ld      a,(hl)
0e32 3d        dec     a
0e33 77        ld      (hl),a
0e34 ca4d0e    jp      z,0e4dh
0e37 21272e    ld      hl,2e27h
0e3a 7e        ld      a,(hl)
0e3b f682      or      82h
0e3d 77        ld      (hl),a
0e3e c34d0e    jp      0e4dh
0e41 c3370e    jp      0e37h
0e44 dba0      in      a,(0a0h)
0e46 05        dec     b
0e47 c2440e    jp      nz,0e44h
0e4a c3370e    jp      0e37h
0e4d c1        pop     bc
0e4e e1        pop     hl
0e4f f1        pop     af
0e50 c9        ret     

0e51 00        nop					; garbage/data?
0e52 00        nop     
0e53 00        nop     
0e54 00        nop     
0e55 00        nop     
0e56 00        nop     
0e57 00        nop     
0e58 00        nop     
0e59 00        nop     
0e5a 00        nop     
0e5b 85        add     a,l
0e5c 00        nop     
0e5d cf        rst     08h
0e5e 8b        adc     a,e
0e5f ab        xor     e
0e60 87        add     a,a
0e61 00        nop     
0e62 00        nop     
0e63 00        nop     
0e64 00        nop     
0e65 00        nop     
0e66 00        nop     
0e67 00        nop     
0e68 00        nop     
0e69 00        nop     
0e6a 00        nop     

0e6b c3c3c3    jp      0c3c3h		; data table of some sort?
0e6e c3c3c3    jp      0c3c3h
0e71 c7        rst     00h
0e72 cb6d      bit     5,l
0e74 b2        or      d
0e75 7f        ld      a,a
0e76 00        nop     
0e77 6c        ld      l,h
0e78 00        nop     
0e79 cf        rst     08h
0e7a 019d00    ld      bc,009dh
0e7d 2c        inc     l
0e7e 12        ld      (de),a
0e7f 58        ld      e,b
0e80 50        ld      d,b
0e81 00        nop     
0e82 8a        adc     a,d
0e83 cf        rst     08h
0e84 8b        adc     a,e
0e85 ab        xor     e
0e86 87        add     a,a
0e87 00        nop     
0e88 00        nop     
0e89 00        nop     
0e8a 00        nop     
0e8b 00        nop     
0e8c 00        nop     
0e8d 00        nop     
0e8e 00        nop     
0e8f 00        nop     
0e90 00        nop

; Called from Init     
0e91 97        sub     a
0e92 32e52f    ld      (2fe5h),a
0e95 32e42f    ld      (2fe4h),a
0e98 32e62f    ld      (2fe6h),a
0e9b 21a50e    ld      hl,0ea5h
0e9e 0e98      ld      c,98h
0ea0 0606      ld      b,06h
0ea2 edb3      otir    
0ea4 c9        ret     

0ea5 c3c7cb    jp      0cbc7h
0ea8 91        sub     c
0ea9 1058      djnz    0f03h

; ISR for ?
0eab f5        push    af
0eac e5        push    hl
0ead c5        push    bc
0eae 21410f    ld      hl,0f41h
0eb1 0e98      ld      c,98h
0eb3 0603      ld      b,03h
0eb5 edb3      otir    
0eb7 21e52f    ld      hl,2fe5h
0eba 7e        ld      a,(hl)
0ebb 32e62f    ld      (2fe6h),a
0ebe fe01      cp      01h
0ec0 cad10e    jp      z,0ed1h
0ec3 3600      ld      (hl),00h
0ec5 21292e    ld      hl,2e29h
0ec8 7e        ld      a,(hl)
0ec9 e607      and     07h
0ecb d3c8      out     (0c8h),a
0ecd 77        ld      (hl),a
0ece c3310f    jp      0f31h
0ed1 21e72f    ld      hl,2fe7h
0ed4 7e        ld      a,(hl)
0ed5 3c        inc     a
0ed6 77        ld      (hl),a
0ed7 fe02      cp      02h
0ed9 c2030f    jp      nz,0f03h
0edc 21292e    ld      hl,2e29h
0edf 7e        ld      a,(hl)
0ee0 e607      and     07h
0ee2 77        ld      (hl),a
0ee3 d3c8      out     (0c8h),a
0ee5 3e05      ld      a,05h
0ee7 d3b3      out     (0b3h),a
0ee9 3ee2      ld      a,0e2h
0eeb d3b3      out     (0b3h),a
0eed 3e10      ld      a,10h
0eef d3b3      out     (0b3h),a
0ef1 97        sub     a
0ef2 32e42f    ld      (2fe4h),a
0ef5 3aea2f    ld      a,(2feah)
0ef8 e67f      and     7fh
0efa c22a0f    jp      nz,0f2ah
0efd 32e52f    ld      (2fe5h),a
0f00 c32a0f    jp      0f2ah
0f03 21e02f    ld      hl,2fe0h
0f06 7e        ld      a,(hl)
0f07 fe00      cp      00h
0f09 ca1a0f    jp      z,0f1ah
0f0c 3e17      ld      a,17h
0f0e d391      out     (91h),a
0f10 3ae12f    ld      a,(2fe1h)
0f13 d391      out     (91h),a
0f15 32e22f    ld      (2fe2h),a
0f18 3600      ld      (hl),00h
0f1a 21002d    ld      hl,2d00h
0f1d 0e98      ld      c,98h
0f1f 061c      ld      b,1ch
0f21 edb3      otir    
0f23 21eb2f    ld      hl,2febh
0f26 7e        ld      a,(hl)
0f27 c680      add     a,80h
0f29 77        ld      (hl),a
0f2a 21272e    ld      hl,2e27h
0f2d 7e        ld      a,(hl)
0f2e f682      or      82h
0f30 77        ld      (hl),a
0f31 c1        pop     bc
0f32 e1        pop     hl
0f33 f1        pop     af
0f34 fb        ei      
0f35 ed4d      reti    

0f37 00        nop     
0f38 00        nop     
0f39 00        nop     
0f3a 00        nop     
0f3b 00        nop     
0f3c 00        nop     
0f3d 00        nop     
0f3e 00        nop     
0f3f 00        nop     
0f40 00        nop     
0f41 83        add     a,e
0f42 a3        and     e
0f43 8b        adc     a,e
0f44 00        nop     
0f45 00        nop     
0f46 00        nop     
0f47 00        nop     
0f48 00        nop     
0f49 00        nop     
0f4a 00        nop     
0f4b 00        nop     
0f4c 00        nop     
0f4d 00        nop 
    
0f4e f5        push    af
0f4f e5        push    hl
0f50 21e52f    ld      hl,2fe5h
0f53 7e        ld      a,(hl)
0f54 fe01      cp      01h
0f56 c26d0f    jp      nz,0f6dh
0f59 3e50      ld      a,50h
0f5b 32622f    ld      (2f62h),a
0f5e 3e01      ld      a,01h
0f60 326a2f    ld      (2f6ah),a
0f63 21692f    ld      hl,2f69h
0f66 7e        ld      a,(hl)
0f67 f680      or      80h
0f69 77        ld      (hl),a
0f6a c3700f    jp      0f70h
0f6d 3e02      ld      a,02h
0f6f 77        ld      (hl),a
0f70 e1        pop     hl
0f71 f1        pop     af
0f72 c9        ret     

; Called from Init
0f73 97        sub     a
0f74 32252e    ld      (2e25h),a
0f77 c9        ret     

0f78 3a262e    ld      a,(2e26h)
0f7b e608      and     08h
0f7d c8        ret     z

0f7e 21252e    ld      hl,2e25h
0f81 7e        ld      a,(hl)
0f82 a7        and     a
0f83 c8        ret     z

0f84 47        ld      b,a
0f85 3e04      ld      a,04h
0f87 cd7d12    call    127dh					; send ClockData message
0f8a 78        ld      a,b
0f8b cd8412    call    1284h
0f8e f3        di      
0f8f 7e        ld      a,(hl)
0f90 90        sub     b
0f91 77        ld      (hl),a
0f92 fb        ei      
0f93 c34001    jp      0140h	; back to top of main loop

; Called from Init
0f96 21222e    ld      hl,2e22h
0f99 3600      ld      (hl),00h
0f9b 21232e    ld      hl,2e23h
0f9e 3681      ld      (hl),81h
0fa0 23        inc     hl
0fa1 3681      ld      (hl),81h
0fa3 c9        ret     

; Called from main loop
; Do GPIB stuff
0fa4 cdd20f    call    0fd2h
0fa7 d8        ret     c

0fa8 21342f    ld      hl,2f34h
0fab 1601      ld      d,01h
0fad c3bd0f    jp      0fbdh
0fb0 7a        ld      a,d
0fb1 fe10      cp      10h
0fb3 cac20f    jp      z,0fc2h
0fb6 cdd20f    call    0fd2h
0fb9 dac20f    jp      c,0fc2h
0fbc 14        inc     d
0fbd 70        ld      (hl),b
0fbe 23        inc     hl
0fbf c3b00f    jp      0fb0h
0fc2 3e06      ld      a,06h
0fc4 cd7d12    call    127dh					; Send GPIBData message
0fc7 7a        ld      a,d
0fc8 32332f    ld      (2f33h),a
0fcb 3c        inc     a
0fcc cd9012    call    1290h
0fcf c34001    jp      0140h					; and away we go back to the top of the main loop

0fd2 e5        push    hl
0fd3 21232e    ld      hl,2e23h
0fd6 7e        ld      a,(hl)
0fd7 23        inc     hl
0fd8 be        cp      (hl)
0fd9 caec0f    jp      z,0fech
0fdc 7e        ld      a,(hl)
0fdd c601      add     a,01h
0fdf e61f      and     1fh
0fe1 f680      or      80h
0fe3 f3        di      
0fe4 77        ld      (hl),a
0fe5 262d      ld      h,2dh
0fe7 6f        ld      l,a
0fe8 46        ld      b,(hl)
0fe9 fb        ei      
0fea e1        pop     hl
0feb c9        ret     

0fec 37        scf     
0fed e1        pop     hl
0fee c9        ret     

0fef 21222e    ld      hl,2e22h
0ff2 7e        ld      a,(hl)
0ff3 b7        or      a
0ff4 ca1510    jp      z,1015h
0ff7 35        dec     (hl)
0ff8 c20710    jp      nz,1007h
0ffb 3a2a2e    ld      a,(2e2ah)
0ffe fe02      cp      02h
1000 c20710    jp      nz,1007h
1003 3e08      ld      a,08h
1005 d3be      out     (0beh),a
1007 21212e    ld      hl,2e21h
100a 7e        ld      a,(hl)
100b 34        inc     (hl)
100c 262d      ld      h,2dh
100e c6a0      add     a,0a0h
1010 6f        ld      l,a
1011 7e        ld      a,(hl)
1012 d3bf      out     (0bfh),a
1014 c9        ret     

1015 21272e    ld      hl,2e27h
1018 7e        ld      a,(hl)
1019 f688      or      88h
101b 77        ld      (hl),a
101c c9        ret     

; ISR for ?
101d f5        push    af
101e e5        push    hl
101f 212b2e    ld      hl,2e2bh
1022 dbb8      in      a,(0b8h)
1024 77        ld      (hl),a
1025 23        inc     hl
1026 dbb9      in      a,(0b9h)
1028 77        ld      (hl),a
1029 23        inc     hl
102a dbba      in      a,(0bah)
102c 77        ld      (hl),a
102d 23        inc     hl
102e dbbb      in      a,(0bbh)
1030 77        ld      (hl),a
1031 23        inc     hl
1032 dbbc      in      a,(0bch)
1034 77        ld      (hl),a
1035 23        inc     hl
1036 dbbe      in      a,(0beh)
1038 77        ld      (hl),a
1039 3a2b2e    ld      a,(2e2bh)
103c e630      and     30h
103e c24610    jp      nz,1046h
1041 e1        pop     hl
1042 f1        pop     af
1043 fb        ei      
1044 ed4d      reti    

1046 e620      and     20h
1048 c25110    jp      nz,1051h
104b cdef0f    call    0fefh
104e c34110    jp      1041h
1051 dbbf      in      a,(0bfh)
1053 f5        push    af
1054 21232e    ld      hl,2e23h
1057 7e        ld      a,(hl)
1058 3c        inc     a
1059 e61f      and     1fh
105b f680      or      80h
105d 23        inc     hl
105e be        cp      (hl)
105f ca6c10    jp      z,106ch
1062 2b        dec     hl
1063 77        ld      (hl),a
1064 262d      ld      h,2dh
1066 6f        ld      l,a
1067 f1        pop     af
1068 77        ld      (hl),a
1069 c34110    jp      1041h
106c f1        pop     af
106d c34110    jp      1041h

; Called from Init
1070 219510    ld      hl,1095h
1073 22312e    ld      (2e31h),hl
1076 c9        ret     

; ISR for ?
1077 f5        push    af
1078 e5        push    hl
1079 218310    ld      hl,1083h
107c e5        push    hl
107d dba0      in      a,(0a0h)
107f 2a312e    ld      hl,(2e31h)
1082 e9        jp      (hl)				; jump to previously stored address (2e31h)

1083 219510    ld      hl,1095h
1086 22312e    ld      (2e31h),hl		; func is 1095h
1089 e1        pop     hl
108a f1        pop     af
108b fb        ei      
108c ed4d      reti    

108e 22312e    ld      (2e31h),hl
1091 e1        pop     hl
1092 c38910    jp      1089h

1095 fe6b      cp      6bh
1097 c0        ret     nz

1098 219e10    ld      hl,109eh
109b c38e10    jp      108eh

109e fe6b      cp      6bh
10a0 cac710    jp      z,10c7h
10a3 fe04      cp      04h
10a5 ca8a0d    jp      z,0d8ah
10a8 3d        dec     a
10a9 fe0e      cp      0eh
10ab d0        ret     nc

10ac d5        push    de
10ad 5f        ld      e,a
10ae 07        rlca    
10af 83        add     a,e
10b0 5f        ld      e,a
10b1 1600      ld      d,00h
10b3 21cd10    ld      hl,10cdh
10b6 19        add     hl,de
10b7 3a272e    ld      a,(2e27h)
10ba a6        and     (hl)
10bb 32272e    ld      (2e27h),a
10be 23        inc     hl
10bf 5e        ld      e,(hl)
10c0 23        inc     hl
10c1 56        ld      d,(hl)
10c2 eb        ex      de,hl
10c3 d1        pop     de
10c4 c38e10    jp      108eh

10c7 219e10    ld      hl,109eh
10ca c38e10    jp      108eh

10cd fef4      cp      0f4h			; data table 
10cf 10fb      djnz    10cch
10d1 5e        ld      e,(hl)
10d2 08        ex      af,af'
10d3 f7        rst     30h
10d4 fd10fd    djnz    10d4h
10d7 8a        adc     a,d
10d8 0d        dec     c
10d9 ff        rst     38h
10da 71        ld      (hl),c
10db 11ffa1    ld      de,0a1ffh
10de 11ffda    ld      de,0daffh
10e1 11effa    ld      de,0faefh
10e4 10ff      djnz    10e5h
10e6 0112ff    ld      bc,0ff12h
10e9 3f        ccf     
10ea 12        ld      (de),a
10eb ff        rst     38h
10ec 56        ld      d,(hl)
10ed 12        ld      (de),a
10ee ff        rst     38h
10ef 08        ex      af,af'
10f0 09        add     hl,bc
10f1 fb        ei      
10f2 7d        ld      a,l
10f3 09        add     hl,bc
10f4 cd5c12    call    125ch
10f7 c3200c    jp      0c20h
10fa c3810a    jp      0a81h
10fd 322a2e    ld      (2e2ah),a
1100 210611    ld      hl,1106h
1103 c38e10    jp      108eh
1106 cd5c12    call    125ch


1109 c5        push    bc
110a d5        push    de
110b 47        ld      b,a
110c 3a2a2e    ld      a,(2e2ah)
110f 3d        dec     a
1110 fe04      cp      04h
1112 d25811    jp      nc,1158h
1115 07        rlca    
1116 5f        ld      e,a
1117 1600      ld      d,00h
1119 216b11    ld      hl,116bh
111c 19        add     hl,de
111d 5e        ld      e,(hl)
111e 23        inc     hl
111f 56        ld      d,(hl)
1120 eb        ex      de,hl
1121 e9        jp      (hl)
1122 78        ld      a,b
1123 32222e    ld      (2e22h),a
1126 21332e    ld      hl,2e33h
1129 11a02d    ld      de,2da0h
112c 7e        ld      a,(hl)
112d 23        inc     hl
112e 12        ld      (de),a
112f 13        inc     de
1130 05        dec     b
1131 c22c11    jp      nz,112ch
1134 21212e    ld      hl,2e21h
1137 3600      ld      (hl),00h
1139 cdef0f    call    0fefh
113c d1        pop     de
113d c1        pop     bc
113e c9        ret     

113f 78        ld      a,b
1140 a7        and     a
1141 1f        rra     
1142 47        ld      b,a
1143 21332e    ld      hl,2e33h
1146 7e        ld      a,(hl)
1147 23        inc     hl
1148 c6b8      add     a,0b8h
114a 4f        ld      c,a
114b feb8      cp      0b8h
114d ca6211    jp      z,1162h
1150 7e        ld      a,(hl)
1151 23        inc     hl
1152 ed79      out     (c),a
1154 05        dec     b
1155 c24611    jp      nz,1146h
1158 21272e    ld      hl,2e27h
115b 7e        ld      a,(hl)
115c f688      or      88h
115e 77        ld      (hl),a
115f d1        pop     de
1160 c1        pop     bc
1161 c9        ret     

1162 7e        ld      a,(hl)
1163 23        inc     hl
1164 f630      or      30h
1166 d3b8      out     (0b8h),a
1168 c35411    jp      1154h
116b 221122    ld      (2211h),hl
116e 113f11    ld      de,113fh
1171 cd5c12    call    125ch
1174 c5        push    bc
1175 47        ld      b,a
1176 21332e    ld      hl,2e33h
1179 7e        ld      a,(hl)
117a 0f        rrca    
117b 3a262e    ld      a,(2e26h)
117e d28611    jp      nc,1186h
1181 f602      or      02h
1183 c38811    jp      1188h
1186 e6fd      and     0fdh
1188 32262e    ld      (2e26h),a
118b 05        dec     b
118c ca9d11    jp      z,119dh
118f 23        inc     hl
1190 7e        ld      a,(hl)
1191 32ca2d    ld      (2dcah),a
1194 05        dec     b
1195 ca9d11    jp      z,119dh
1198 23        inc     hl
1199 7e        ld      a,(hl)
119a 32cb2d    ld      (2dcbh),a
119d c1        pop     bc
119e c3380b    jp      0b38h
11a1 c5        push    bc
11a2 cd5c12    call    125ch
11a5 3a332e    ld      a,(2e33h)
11a8 0f        rrca    
11a9 0f        rrca    
11aa 21262e    ld      hl,2e26h
11ad d2bd11    jp      nc,11bdh
11b0 06d1      ld      b,0d1h
11b2 3e02      ld      a,02h
11b4 32e52d    ld      (2de5h),a
11b7 7e        ld      a,(hl)
11b8 f601      or      01h
11ba c3c211    jp      11c2h
11bd 06c0      ld      b,0c0h
11bf 7e        ld      a,(hl)
11c0 e6fe      and     0feh
11c2 77        ld      (hl),a
11c3 3e03      ld      a,03h
11c5 d3b3      out     (0b3h),a
11c7 78        ld      a,b
11c8 d3b3      out     (0b3h),a
11ca 3e10      ld      a,10h
11cc d3b3      out     (0b3h),a
11ce 97        sub     a
11cf 32d52d    ld      (2dd5h),a
11d2 32de2d    ld      (2ddeh),a
11d5 32e62d    ld      (2de6h),a
11d8 c1        pop     bc
11d9 c9        ret     

11da cd5c12    call    125ch
11dd 3a332e    ld      a,(2e33h)
11e0 0f        rrca    
11e1 21262e    ld      hl,2e26h
11e4 7e        ld      a,(hl)
11e5 d2f411    jp      nc,11f4h
11e8 f604      or      04h
11ea 77        ld      (hl),a
11eb 21292e    ld      hl,2e29h
11ee 7e        ld      a,(hl)
11ef f602      or      02h
11f1 c3fd11    jp      11fdh
11f4 e6fb      and     0fbh
11f6 77        ld      (hl),a
11f7 21292e    ld      hl,2e29h
11fa 7e        ld      a,(hl)
11fb e6fd      and     0fdh
11fd d3c8      out     (0c8h),a
11ff 77        ld      (hl),a
1200 c9        ret     

1201 c5        push    bc
1202 cd5c12    call    125ch
1205 47        ld      b,a
1206 21332e    ld      hl,2e33h
1209 7e        ld      a,(hl)
120a 32e12f    ld      (2fe1h),a
120d 3e01      ld      a,01h
120f 32e02f    ld      (2fe0h),a
1212 78        ld      a,b
1213 fe02      cp      02h
1215 c23d12    jp      nz,123dh
1218 23        inc     hl
1219 7e        ld      a,(hl)
121a ee99      xor     99h
121c c23d12    jp      nz,123dh
121f 32eb2f    ld      (2febh),a
1222 32ea2f    ld      (2feah),a
1225 3ae42f    ld      a,(2fe4h)
1228 fe01      cp      01h
122a ca3d12    jp      z,123dh
122d 21e52f    ld      hl,2fe5h
1230 7e        ld      a,(hl)
1231 fe01      cp      01h
1233 c23d12    jp      nz,123dh
1236 3600      ld      (hl),00h
1238 3e02      ld      a,02h
123a 32e72f    ld      (2fe7h),a
123d c1        pop     bc
123e c9        ret     

123f cd5c12    call    125ch
1242 3a332e    ld      a,(2e33h)
1245 0f        rrca    
1246 21262e    ld      hl,2e26h
1249 7e        ld      a,(hl)
124a d25212    jp      nc,1252h
124d f608      or      08h
124f c35412    jp      1254h
1252 e6f7      and     0f7h
1254 77        ld      (hl),a
1255 c9        ret     

1256 21282e    ld      hl,2e28h
1259 b6        or      (hl)
125a 77        ld      (hl),a
125b c9        ret     

125c f5        push    af
125d c5        push    bc
125e 47        ld      b,a
125f 21292e    ld      hl,2e29h
1262 7e        ld      a,(hl)
1263 e6fb      and     0fbh
1265 d3c8      out     (0c8h),a
1267 77        ld      (hl),a
1268 fb        ei      
1269 0ea0      ld      c,0a0h
126b 21332e    ld      hl,2e33h
126e edb2      inir    
1270 f3        di      
1271 21292e    ld      hl,2e29h
1274 7e        ld      a,(hl)
1275 f604      or      04h
1277 d3c8      out     (0c8h),a
1279 77        ld      (hl),a
127a c1        pop     bc
127b f1        pop     af
127c c9        ret     

; sends SOM followed by contents of 'a' to PERQ.
127d f5        push    af			; save current a value (message type?)
127e 3e6b      ld      a,6bh		; load SOM into a
1280 cd8412    call    1284h		; call 1284 to send a
1283 f1        pop     af			; and pop the message type value & send

; sends a single byte in 'a' to the PERQ
1284 f5        push    af			; save a
1285 db88      in      a,(88h)		; read status port?
1287 e640      and     40h
1289 c28512    jp      nz,1285h		; wait for bit 7 of input to go low (clear to send?)
128c f1        pop     af			; restore a value (value to send)
128d d3d0      out     (0d0h),a		; send it.
128f c9        ret			

; Sends a message stored in 2f33h to the PERQ
; 'a' contains the message length
1290 c5        push    bc			; send byte to PERQ?
1291 e5        push    hl
1292 21332f    ld      hl,2f33h		; point HL at the message buffer
1295 47        ld      b,a			; load b with the message length
1296 0ed0      ld      c,0d0h		; this seems extraneous.
1298 db88      in      a,(88h)		
129a e640      and     40h
129c c29812    jp      nz,1298h		; wait for the bit 7 to go low (CTS?)
129f 7e        ld      a,(hl)		; get the current value from the buffer
12a0 d3d0      out     (0d0h),a		; send the value
12a2 23        inc     hl			
12a3 05        dec     b			; move to next byte
12a4 c29812    jp      nz,1298h		; loop until message done.
12a7 e1        pop     hl
12a8 c1        pop     bc
12a9 c9        ret     

; ISR for ? - does nothing.
12aa fb        ei      
12ab ed4d      reti    

; ISR for ?
12ad f5        push    af
12ae e5        push    hl
12af 3e10      ld      a,10h
12b1 d3b1      out     (0b1h),a
12b3 e1        pop     hl
12b4 f1        pop     af
12b5 fb        ei      
12b6 ed4d      reti    

12b8 e5        push    hl
12b9 f5        push    af
12ba ed57      ld      a,i
12bc e2c712    jp      po,12c7h
12bf 21cd2d    ld      hl,2dcdh
12c2 97        sub     a
12c3 be        cp      (hl)
12c4 c2c312    jp      nz,12c3h
12c7 dbb1      in      a,(0b1h)
12c9 e604      and     04h
12cb cac712    jp      z,12c7h
12ce f1        pop     af
12cf d3b0      out     (0b0h),a
12d1 e1        pop     hl
12d2 c9        ret     

12d3 7e        ld      a,(hl)
12d4 a7        and     a
12d5 c8        ret     z

12d6 cdb812    call    12b8h
12d9 23        inc     hl
12da c3d312    jp      12d3h
12dd c5        push    bc
12de 47        ld      b,a
12df 07        rlca    
12e0 07        rlca    
12e1 e603      and     03h
12e3 cdf712    call    12f7h
12e6 78        ld      a,b
12e7 0f        rrca    
12e8 0f        rrca    
12e9 0f        rrca    
12ea cdf712    call    12f7h
12ed 78        ld      a,b
12ee cdf712    call    12f7h
12f1 c1        pop     bc
12f2 3e20      ld      a,20h
12f4 c3b812    jp      12b8h
12f7 e607      and     07h
12f9 c630      add     a,30h
12fb c3b812    jp      12b8h
