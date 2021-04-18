DECLARE SUB config ()
DECLARE FUNCTION filerequest$ ()
DECLARE FUNCTION zpx! ()
DECLARE FUNCTION zpy! ()
DECLARE FUNCTION abso! ()
DECLARE FUNCTION absx! ()
DECLARE FUNCTION absy! ()
DECLARE FUNCTION pre! ()
DECLARE FUNCTION post! ()
DECLARE FUNCTION zp! ()
DECLARE FUNCTION breakpoint! ()
DECLARE SUB op.bvs ()
DECLARE SUB op.bvc ()
DECLARE SUB op.sbc (typ!)
DECLARE SUB op.brk ()
DECLARE SUB op.php ()
DECLARE SUB op.plp ()
DECLARE SUB op.cpy (typ!)
DECLARE SUB op.bit (typ!)
DECLARE SUB op.cpx (typ!)
DECLARE SUB op.eor (typ!)
DECLARE SUB crappygfxengine ()
DECLARE SUB op.bcc ()
DECLARE SUB patterntables ()
DECLARE SUB op.rti ()
DECLARE SUB op.dec (typ!)
DECLARE SUB op.sty (typ!)
DECLARE SUB op.stx (typ!)
DECLARE SUB op.and (typ!)
DECLARE SUB op.bcs ()
DECLARE SUB op.inc (typ!)
DECLARE SUB op.ldy (typ!)
DECLARE SUB op.cmp (typ!)
DECLARE SUB op.rts ()
DECLARE FUNCTION pop% ()
DECLARE SUB op.jsr ()
DECLARE SUB push (r%)
DECLARE SUB moveup ()
DECLARE SUB loadfont ()
DECLARE FUNCTION anybyte% (posi!)
DECLARE SUB putbyte (byte%, posi!)
DECLARE FUNCTION getbyte% ()
DECLARE SUB hagla (x, y, text$)
DECLARE SUB op.tya ()
DECLARE SUB cycle (r%)
DECLARE SUB op.adc (typ!)
DECLARE SUB op.bpl ()
DECLARE SUB op.bmi ()
DECLARE SUB op.jmp (typ!)
DECLARE SUB op.txs ()
DECLARE SUB header ()
DECLARE SUB op.bne ()
DECLARE SUB op.beq ()
DECLARE SUB utskrift ()
DECLARE SUB op.ldx (typ!)
DECLARE SUB op.rol (typ!)
DECLARE SUB op.ror (typ!)
DECLARE SUB op.asl (typ!)
DECLARE SUB op.sec ()
DECLARE SUB op.sed ()
DECLARE SUB op.cli ()
DECLARE SUB op.clv ()
DECLARE SUB op.clc ()
DECLARE SUB op.cld ()
DECLARE SUB op.sei ()
DECLARE SUB op.ora (typ!)
DECLARE SUB op.lsr (typ!)
DECLARE SUB op.sta (typ!)
DECLARE SUB op.pla ()
DECLARE SUB op.pha ()
DECLARE SUB op.tsx ()
DECLARE SUB op.txa ()
DECLARE SUB op.dex ()
DECLARE SUB op.dey ()
DECLARE SUB op.lda (typ)
DECLARE SUB op.inx ()
DECLARE SUB op.iny ()
DECLARE SUB op.tax ()
DECLARE SUB op.tay ()
DECLARE SUB emulate ()
DEFINT C, R
'Status: SV1BDIZC
' Alla register m†ste vara share-ade...
loadfont       'Ladda in fonten...
DIM SHARED reg.a, reg.x, reg.y, reg.s, pc, reg.sp, fil$, byte$, opcode$, cy
DIM SHARED cycles, nmi, ut, bokstav(52, 5), bytes, rabies, vramposi, vblank
DIM SHARED ppu1, brk, sprramposi, sprram$, ppustat, sprite0(1), lines, cyc
DIM SHARED joy1emu, joy1reads, joy1reset, ppu2, firstread, bpoint, skip
firstread = 1
sprram$ = STRING$(256, CHR$(0))
'uNESsential Nintendo-emulator. Version 0.14
'
'Nyheter:
' CPU-emulering f”rb„ttrad.
' STA(?) och LSR fixade av misstag :)
'    Pacman-titelbilden.
' ROL fixad. Anv„nde reg.a som reg.s.
' ROR fixad. Samma fel.
' ADC och SBC fixade. Fel p† C-flagen
'    Ice Climber, Ice Hockey mm.
' N†gonting sabbat..
'    Tennis resettar hela tiden.
' PLA fixad. Žndrade inte reg.s. <-Inte i release.
'
'
'Allm„n info:
'RAM-minne emuleras genom filen MEMORY.NIN.
'VRAM emuleras genom VRAM.NIN.
'Sprite-RAM emuleras genom sprram$.
'Alla opcode-funktioner heter op.???(?).
'Register emuleras genom variabler.
'Alla register-variabler heter reg.?. (undantag: pc, som inte f†r vara INT)

'Variabler:
'fil$ - Filnamn p† rommen som k”rs.
'byte$ - variabel f”r h„mtning av en byte.
'WARNING! Sharea inte "byte$". D† blir det haveri. <---Snack! Tv„rtom!

byte$ = CHR$(0)
CLS
'Ladda in en rom...
config
fil$ = COMMAND$
IF fil$ = "" THEN fil$ = filerequest$
IF fil$ = "" THEN END
SCREEN 12

'Rita lite saker...
CLS
'LINE (0, 0)-(256, 224), 8, B
'F”rberedelser...
joy1reads = 1
FOR r = 1 TO 253
   MID$(sprram$, r, 1) = CHR$(&HF0)
NEXT r
COLOR 12
hagla 1, 248, "joypads"
hagla 1, 290, "pattern tables"
OPEN fil$ FOR BINARY AS #1         'Rommen har filnummer #1.
OPEN "MEMORY.NIN" FOR BINARY AS #2 'Minnet har filnummer #2. OBS Byte 1=0
OPEN "VRAM.NIN" FOR BINARY AS #4   'VRAM har filnummer #4.
reg.s = &H2C                         'Status-bit 6=1
pc = 17                            'Skippa 16-bytes-headern...
reg.sp = 255                       'Stack-pekaren m†ste vara 255.
header
LINE (459, 407)-(639, 407), 8, , 21845
patterntables
emulate                            'B”rja emulera!

FUNCTION abso
pc = pc + 1
GET #2, pc, byte$
a = ASC(byte$)
pc = pc + 1
GET #2, pc, byte$
b = ASC(byte$)
a = a + b * 256
abso = a

END FUNCTION

FUNCTION absx
pc = pc + 1
GET #2, pc, byte$
a = ASC(byte$)
pc = pc + 1
GET #2, pc, byte$
b = ASC(byte$)
a = a + b * 256
a = a + reg.x
IF (a AND 65536) THEN a = a AND 65535
absx = a
END FUNCTION

FUNCTION absy
pc = pc + 1
GET #2, pc, byte$
a = ASC(byte$)
pc = pc + 1
GET #2, pc, byte$
b = ASC(byte$)
a = a + b * 256
a = a + reg.y
IF (a AND 65536) THEN a = a AND 65535
absy = a
END FUNCTION

FUNCTION anybyte% (posi)
GET #2, posi + 1, byte$
anybyte% = ASC(byte$)

IF posi >= &H2000 THEN
IF posi <= &H2007 THEN

'IF posi = &H2001 THEN anybyte% = ppu1
'IF posi = &H2002 THEN anybyte% = ppu2
IF posi = &H2002 THEN anybyte% = ppustat: ppustat = ppustat AND 127
IF posi = &H2004 THEN anybyte% = ASC(MID$(sprram$, sprramposi + 1))
IF posi = &H2007 THEN
   okn = 1
   IF (ppu1 AND 4) THEN okn = 32
   GET #4, vramposi + 1, byte$: anybyte% = ASC(byte$)
   IF firstread = 0 THEN
      vramposi = vramposi + okn
   END IF
   firstread = 0
END IF

END IF
END IF

IF posi >= &H4000 THEN
IF posi <= &H4017 THEN

   IF posi = &H4016 THEN
      IF (joy1emu AND joy1reads) = joy1reads THEN anybyte% = 1
      joy1reads = joy1reads * 2
      IF joy1reads > 256 THEN joy1reads = 256
   END IF

END IF
END IF

END FUNCTION

FUNCTION breakpoint
hagla 260, 10, "breakpoint:"
DO WHILE u = 0
   SELECT CASE INKEY$
      CASE "4"
         bp1 = bp1 + 1
         IF bp1 = 16 THEN bp1 = 0
         GOSUB up
      CASE "3"
         bp2 = bp2 + 16
         IF bp2 = 256 THEN bp2 = 0
         GOSUB up
      CASE "2"
         bp3 = bp3 + 256
         IF bp3 = 4096 THEN bp3 = 0
         GOSUB up
      CASE "1"
         bp4 = bp4 + 4096
         IF bp4 = 65536 THEN bp4 = 0
         GOSUB up
      CASE CHR$(13): u = 1
      CASE CHR$(27): u = 2
   END SELECT
LOOP

IF u = 1 THEN breakpoint = bp4 + bp3 + bp2 + bp1
EXIT FUNCTION
up:
LINE (310, 10)-(330, 18), 0, BF
hagla 310, 10, LCASE$(HEX$(bp4 \ 4096) + HEX$(bp3 \ 256) + HEX$(bp2 \ 16) + HEX$(bp1))
RETURN
END FUNCTION

SUB config
OPEN "uness.cfg" FOR INPUT AS #10
INPUT #10, o$
CLOSE
IF o$ = "-" THEN
   OPEN "uness.cfg" FOR INPUT AS #10
   PRINT "Welcome to uNESsential v0.14"
   PRINT "Please tell me where your roms are stored (example: c:\roms)."
   INPUT "->", roms$
   IF RIGHT$(roms$, 1) = "\" THEN GOTO ooo
      roms$ = roms$ + "\"
ooo:
   CLOSE
   OPEN "uness.cfg" FOR OUTPUT AS #10
   PRINT #10, roms$
   CLOSE
   EXIT SUB
END IF
END SUB

DEFINT A-B, X-Y
SUB crappygfxengine 'F”rmodligen den s„msta grafikmotor som n†gonsin skapats...
'Bakgrund.
'IF (ppu2 AND 8) THEN
DIM tile(7, 7)
IF (ppu1 AND 16) = 16 THEN okning = 128
o = ppu1 AND 3
IF o = 0 THEN adr = &H2000
IF o = 1 THEN adr = &H2400
IF o = 2 THEN adr = &H2800
IF o = 3 THEN adr = &H2C00
bytes$ = STRING$(960, " ")
GET #4, adr + 1, bytes$
FOR c = 1 TO 960
   'LOCATE 1, 40: PRINT ASC(MID$(bytes$, c, 1))
   a = okning + ((ASC(MID$(bytes$, c, 1)) AND 15) * 8)
   b = 300 + ((ASC(MID$(bytes$, c, 1)) AND 240) / 2)
   GET (a, b)-(a + 7, b + 7), tile
   'PSET (a, b), 12
   PUT (x, y), tile, PSET
   x = x + 8
   IF x = 256 THEN y = y + 8: x = 0
NEXT c
'END IF
'Sprites.
'IF (ppu2 AND 16) THEN
asize = (ppu1 AND 32)
okning = (ppu1 AND 8) * 16
IF asize = 0 THEN
   FOR c = 1 TO 253 STEP 4
      y = ASC(MID$(sprram$, c))
      IF y < &HF0 THEN
         x = ASC(MID$(sprram$, c + 3))
         attr = ASC(MID$(sprram$, c + 2))
         IF (attr AND 128) THEN
            sy = 7: sys = -1
         ELSE
            sy = 0: sys = 1
         END IF
         IF (attr AND 64) THEN
            sx = 7: sxs = -1
         ELSE
            sx = 0: sxs = 1
         END IF
         rp = (attr AND 32)
         a = okning + ((ASC(MID$(sprram$, c + 1, 1)) AND 15) * 8)
         b = 300 + ((ASC(MID$(sprram$, c + 1, 1)) AND 240) \ 2)
         FOR cy = sy TO (sy XOR 7) STEP sys
            FOR cx = sx TO (sx XOR 7) STEP sxs
               IF rp = 0 THEN IF POINT(a + cx, b + cy) > 0 THEN PSET (x + ax, y + bx + 1), POINT(a + cx, b + cy)
               IF rp = 32 THEN IF POINT(x + ax, y + bx + 1) = 0 THEN PSET (x + ax, y + bx + 1), POINT(a + cx, b + cy)
               ax = ax + 1
            NEXT cx
            bx = bx + 1: ax = 0
         NEXT cy
         ax = 0: bx = 0
      END IF
   NEXT c
END IF
IF asize = 32 THEN
   FOR c = 1 TO 253 STEP 4
      y = ASC(MID$(sprram$, c))
      IF y < &HF0 THEN
         x = ASC(MID$(sprram$, c + 3))
         attr = ASC(MID$(sprram$, c + 2))
         IF (attr AND 128) THEN
            sy = 7: sys = -1
         ELSE
            sy = 0: sys = 1
         END IF
         rp = (attr AND 32)
         IF (attr AND 64) = 0 THEN
            a = okning + ((ASC(MID$(sprram$, c + 1, 1)) AND 15) * 8)
            b = 300 + ((ASC(MID$(sprram$, c + 1, 1)) AND 240) \ 2)
            FOR spr = 8 TO 0 STEP -8
            FOR cy = sy TO (sy XOR 7) STEP sys
               FOR cx = 0 TO 7
                  IF rp = 0 THEN IF POINT(a + cx, b + cy) > 0 THEN PSET (x + ax, y + bx + 1 + spr), POINT(a + cx, b + cy)
                  IF rp = 32 THEN IF POINT(x + ax, y + bx + 1 + spr) = 0 THEN PSET (x + ax, y + bx + 1 + spr), POINT(a + cx, b + cy)
                  'IF POINT(a + cx, b + cy) > 0 THEN PSET (x + ax, y + bx + 1 + spr), POINT(a + cx, b + cy)
                  ax = ax + 1
               NEXT cx
               bx = bx + 1: ax = 0
            NEXT cy
            ax = 0: bx = 0
            a = a - 8
            IF a = 120 OR a = -8 THEN a = a + 128: b = b - 8
            NEXT spr
         END IF
      END IF
   NEXT c
END IF



END SUB

DEFSNG A-B, X-Y
SUB cycle (r)
cycles = cycles + r
IF vblank = 1 THEN
   IF cycles >= 4326 THEN
      BEEP: BEEP: BEEP: BEEP: BEEP: BEEP
      cycles = 7
      ppustat = ppustat AND 127
      vblank = 0
      'ut = 0
   END IF
END IF
IF cycles >= 25503 THEN
   'ut = 1
   cycles = 0
   ppustat = ppustat OR 128
   ppustat = ppustat AND 191
   vblank = 1
   'ut = 1
   IF (ppu1 AND 128) THEN
      BEEP: BEEP: BEEP: BEEP: BEEP: BEEP
      cycles = 7
      'L„gg pc p† stacken.
      b% = (pc AND 65280) / 256
      a% = (pc AND 255)
      push b%
      push a%
      push reg.s        'L„gg status p† stacken...
      reg.s = reg.s OR 4
      'opcode$ = "vblank: hoppar till " + HEX$(nmi)
      pc = nmi
   END IF
   crappygfxengine    'Vederv„rdig grafikmotor som borde f”rbjudas!
END IF
END SUB

SUB emulate
byte$ = " "
'ut = 1
'OPEN "debug.dat" FOR BINARY AS #9
DO
GET #2, pc, byte$
bytes = ASC(byte$)
'En miljard case-ar!
'OBS! B”r sorteras i ordningen: vanligast f”rst - ovanligast sist...
SELECT CASE ASC(byte$)
   CASE &HA9
      reg.a = getbyte
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2  'Z?
      IF reg.a > 127 THEN reg.s = reg.s OR 128'S?
      cycle 2
   CASE &HA5: cy = 3: op.lda (getbyte)
   CASE &HB5: cy = 4: op.lda (zpx)
   CASE &HAD: cy = 4: op.lda (abso)': IF abso = &H2002 THEN opcode$ = "LDA $2002"
   CASE &HBD: cy = 4: op.lda (absx)
   CASE &HB9: cy = 4: op.lda (absy)
   CASE &HA1: cy = 6: op.lda (pre)
   CASE &HB1: cy = 5: op.lda (post)

   CASE &HD0: op.bne
   CASE &HF0: op.beq
   CASE &H10: op.bpl
   CASE &H30: op.bmi
   CASE &H90: op.bcc
   CASE &HB0: op.bcs
   CASE &H50: op.bvc
   CASE &H70: op.bvs

   CASE &HAA: op.tax
   CASE &H8A: op.txa
   CASE &HA8: op.tay
   CASE &H98: op.tya
   CASE &H9A: op.txs
   CASE &HBA: op.tsx
  
   CASE &HE6: cy = 5: op.inc (getbyte)
   CASE &HF6: cy = 6: op.inc (zpx)
   CASE &HEE: cy = 6: op.inc (abso)
   CASE &HFE: cy = 7: op.inc (absx)

   CASE &HC6: cy = 5: op.dec (getbyte)
   CASE &HD6: cy = 6: op.dec (zpx)
   CASE &HCE: cy = 6: op.dec (abso)
   CASE &HDE: cy = 7: op.dec (absx)

   CASE &HE8: op.inx
   CASE &HC8: op.iny

   CASE &H85: cy = 2: op.sta (getbyte)
   CASE &H95: cy = 3: op.sta (zpx)
   CASE &H8D: cy = 3: op.sta (abso)
   CASE &H9D: cy = 4: op.sta (absx)
   CASE &H99: cy = 5: op.sta (absy)
   CASE &H81: cy = 6: op.sta (pre)
   CASE &H91: cy = 6: op.sta (post)

   CASE &H86: cy = 3: op.stx (getbyte)
   CASE &H96: cy = 4: op.stx (zpy)
   CASE &H8E: cy = 4: op.stx (abso)
  
   CASE &H84: cy = 3: op.sty (getbyte)
   CASE &H94: cy = 4: op.sty (zpx)
   CASE &H8C: cy = 4: op.sty (abso)

   CASE &HA2: reg.x = getbyte
      reg.s = reg.s AND 125
      IF reg.x = 0 THEN reg.s = reg.s OR 2  'Z?
      IF reg.x > 127 THEN reg.s = reg.s OR 128'S?
      cycle 2
   CASE &HA6: cy = 3: op.ldx (getbyte)
   CASE &HB6: cy = 4: op.ldx (zpy)
   CASE &HAE: cy = 4: op.ldx (abso)
   CASE &HBE: cy = 4: op.ldx (absy)

   CASE &HA0: reg.y = getbyte
      reg.s = reg.s AND 125
      IF reg.y = 0 THEN reg.s = reg.s OR 2  'Z?
      IF reg.y > 127 THEN reg.s = reg.s OR 128'S?
      cycle 2
   CASE &HA4: cy = 3: op.ldy (getbyte)
   CASE &HB4: cy = 4: op.ldy (zpx)
   CASE &HAC: cy = 4: op.ldy (abso)
   CASE &HBC: cy = 4: op.ldy (absx)
  
   CASE &HCA: op.dex
   CASE &H88: op.dey

   CASE &H48: op.pha
   CASE &H68: op.pla
   CASE &H8: op.php
   CASE &H28: op.plp

   CASE &H4A: reg.s = reg.s AND 254: reg.s = (reg.s OR (reg.a AND 1))
   reg.a = reg.a \ 2: reg.s = reg.s AND 125
   IF reg.a = 0 THEN reg.s = reg.s OR 2
   cycle 2
   CASE &H46: cy = 5: op.lsr (getbyte)
   CASE &H56: cy = 6: op.lsr (zpx)
   CASE &H4E: cy = 6: op.lsr (abso)
   CASE &H5E: cy = 7: op.lsr (absx)

   CASE &HA: reg.s = reg.s AND 254: reg.a = reg.a * 2
      IF (reg.a AND 256) THEN reg.a = reg.a - 256: reg.s = reg.s OR 1
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2
      IF reg.a > 127 THEN reg.s = reg.s OR 128
      cycle 2
   CASE &H6: op.asl (getbyte)
   CASE &H16: op.asl (zpx)
   CASE &HE: op.asl (abso)
   CASE &H1E: op.asl (absx)

   CASE &H29
      r = getbyte
      reg.a = reg.a AND r
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2
      IF reg.a > 127 THEN reg.s = reg.s OR 128
      cycle 2
   CASE &H25: op.and (getbyte): cycle 3
   CASE &H35: op.and (zpx): cycle 4
   CASE &H2D: op.and (abso): cycle 4
   CASE &H3D: op.and (absx): cycle 4
   CASE &H39: op.and (absy): cycle 4
   CASE &H21: op.and (pre): cycle 6
   CASE &H31: op.and (post): cycle 5

   CASE &H9
      r = getbyte
      reg.a = reg.a OR r
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2
      IF reg.a > 127 THEN reg.s = reg.s OR 128
      cycle 2
   CASE &H5: op.ora (getbyte): cycle 3
   CASE &H15: op.ora (zpx): cycle 4
   CASE &HD: op.ora (abso): cycle 4
   CASE &H1D: op.ora (absx): cycle 4
   CASE &H19: op.ora (absy): cycle 4
   CASE &H1: op.ora (pre): cycle 6
   CASE &H11: op.ora (post): cycle 5

   CASE &H49
      r = getbyte
      reg.a = reg.a XOR r
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2      'Z?
      IF reg.a > 127 THEN reg.s = reg.s OR 128  'S?
      cycle 2
   CASE &H45: op.eor (getbyte): cycle 3
   CASE &H55: op.eor (zpx): cycle 4
   CASE &H4D: op.eor (abso): cycle 4
   CASE &H5D: op.eor (absx): cycle 4
   CASE &H59: op.eor (absy): cycle 4

   CASE &H2A
      reg.a = reg.a + reg.a + (reg.s AND 1)
      IF reg.a < 256 THEN
         reg.s = reg.s AND 254
      ELSE
         reg.a = reg.a - 256: reg.s = reg.s OR 1
      END IF
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2
      IF reg.a > 127 THEN reg.s = reg.s OR 128
      cycle 2
   CASE &H26: op.rol (getbyte): cycle 5
   CASE &H36: op.rol (zpx): cycle 6
   CASE &H2E: op.rol (abso): cycle 6
   CASE &H3E: op.rol (absx): cycle 7

   CASE &H6A
      reg.a = reg.a + (reg.s AND 1) * 256
      reg.s = reg.s AND 254
      reg.s = (reg.s OR (reg.a AND 1))
      reg.a = reg.a \ 2
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2
      IF reg.a > 127 THEN reg.s = reg.s OR 128
      cycle 2
   CASE &H66: op.ror (getbyte): cycle 5
   CASE &H76: op.ror (zpx): cycle 6
   CASE &H6E: op.ror (abso): cycle 6
   CASE &H7E: op.ror (absx): cycle 7

   CASE &H38: op.sec
   CASE &HF8: op.sed
   CASE &H78: op.sei
   
   CASE &H18: op.clc
   CASE &HD8: op.cld
   CASE &H58: op.cli
   CASE &HB8: op.clv

   CASE &H4C: op.jmp (1)
   CASE &H6C: op.jmp (2)
   CASE &H20: op.jsr

   CASE &H60: op.rts

   CASE &HC9
      r = getbyte
      reg.s = reg.s AND 124
      IF r = reg.a THEN reg.s = reg.s OR 3
      IF ((reg.a - r) AND 128) THEN reg.s = reg.s OR 128
      IF r < reg.a THEN reg.s = reg.s OR 1
      cycle 2
   CASE &HC5: op.cmp (getbyte): cycle 3
   CASE &HD5: op.cmp (zpx): cycle 4
   CASE &HCD: op.cmp (abso): cycle 4
   CASE &HDD: op.cmp (absx): cycle 4
   CASE &HD9: op.cmp (absy): cycle 4
   CASE &HC1: op.cmp (pre): cycle 6
   CASE &HD1: op.cmp (post): cycle 5

   CASE &HE0
      r = getbyte
      reg.s = reg.s AND 124
      IF r = reg.x THEN reg.s = reg.s OR 3
      IF ((reg.x - r) AND 128) THEN reg.s = reg.s OR 128
      IF r < reg.x THEN reg.s = reg.s OR 1
      cycle 2
   CASE &HE4: op.cpx (getbyte): cycle 3
   CASE &HEC: op.cpx (abso): cycle 4
  
   CASE &HC0
      r = getbyte
      reg.s = reg.s AND 124
      IF r = reg.y THEN reg.s = reg.s OR 3
      IF ((reg.y - r) AND 128) THEN reg.s = reg.s OR 128
      IF r < reg.y THEN reg.s = reg.s OR 1
      cycle 2

   CASE &HC4: op.cpy (getbyte): cycle 3
   CASE &HCC: op.cpy (abso): cycle 4
  
   CASE &H24: op.bit (getbyte): cycle 3
   CASE &H2C: op.bit (abso): cycle 4

   CASE &H69: op.adc (pc): pc = pc + 1: cycle 2
   CASE &H65: op.adc (getbyte): cycle 3
   CASE &H75: op.adc (zpx): cycle 4
   CASE &H6D: op.adc (abso): cycle 4
   CASE &H7D: op.adc (absx): cycle 4
   CASE &H79: op.adc (absy): cycle 4
   CASE &H61: op.adc (pre): cycle 6
   CASE &H71: op.adc (post): cycle 5

   CASE &HE9: op.sbc (pc): pc = pc + 1: cycle 2
   CASE &HE5: op.sbc (getbyte): cycle 3
   CASE &HF5: op.sbc (zpx): cycle 4
   CASE &HED: op.sbc (abso): cycle 4
   CASE &HFD: op.sbc (absx): cycle 4
   CASE &HF9: op.sbc (absy): cycle 4

   CASE &H0: op.brk
  
   CASE &H40: op.rti

   CASE &HEA: opcode$ = "NOP": cycle 2
  
   CASE ELSE: opcode$ = "Unimplemented :(": ut = 1
END SELECT
IF ut = 1 THEN utskrift
SELECT CASE INKEY$
   CASE "j": pc = 53332
   CASE "o", "p": ut = ut XOR 1
   CASE "n": cycles = 27007
   CASE "r": CLS : patterntables
   CASE "d": crappygfxengine
   CASE "b": bpoint = breakpoint
   CASE "z": joy1emu = joy1emu XOR 1: PSET (21, 262), POINT(21, 262) XOR 9
   CASE "x": joy1emu = joy1emu XOR 2: PSET (19, 262), POINT(19, 262) XOR 9
   CASE "c": joy1emu = joy1emu XOR 4: PSET (14, 262), POINT(14, 262) XOR 9
   CASE "v": joy1emu = joy1emu XOR 8: PSET (16, 262), POINT(16, 262) XOR 9
   CASE CHR$(0) + "H": joy1emu = joy1emu XOR 16: PSET (10, 260), POINT(10, 260) XOR 9
   CASE CHR$(0) + "P": joy1emu = joy1emu XOR 32: PSET (10, 262), POINT(10, 262) XOR 9
   CASE CHR$(0) + "K": joy1emu = joy1emu XOR 64: PSET (9, 261), POINT(9, 261) XOR 9
   CASE CHR$(0) + "M": joy1emu = joy1emu XOR 128: PSET (11, 261), POINT(11, 261) XOR 9
   CASE "q": CLOSE : CLOSE : CLOSE : END
END SELECT
IF pc = bpoint THEN ut = 1

'o$ = CHR$(pc \ 256)'High byte pc
'PUT #9, , o$
'o$ = CHR$(pc AND 255)'Low byte pc
'PUT #9, , o$
'o$ = CHR$(reg.a)'A-register
'PUT #9, , o$
'o$ = CHR$(reg.x)'X-register
'PUT #9, , o$
'o$ = CHR$(reg.y)'Y-register
'PUT #9, , o$
'o$ = CHR$(reg.s)'Status-register
'PUT #9, , o$

pc = pc + 1


LOOP

END SUB

FUNCTION filerequest$
DIM FileName$(255)
OPEN "uness.cfg" FOR INPUT AS #10
INPUT #10, path$
CLOSE
Listing$ = "*.nes"
Relist:
LET chc = 1
CLS
SCREEN 0, 0, 3
COLOR 0, 0
CLS
FILES path$ + Listing$
COLOR 7, 0
LET TextRow = 0
LET FileNumber = 0
FOR a = 1 TO 255
LET FileName$(a) = ""
NEXT
LET TextRow = TextRow + 1
FOR a = 1 TO 255
LET TextRow = TextRow + 1
FOR h = 0 TO 3
LET FileNumber = FileNumber + 1
FOR G = 1 TO 12
IF SCREEN(TextRow, G + (18 * h)) = 32 AND G = 1 THEN LET done = 1: LET G = 12: LET h = 3: LET a = 255
IF CHR$(SCREEN(TextRow, G + (18 * h))) <> " " THEN LET FileName$(FileNumber) = FileName$(FileNumber) + CHR$(SCREEN(TextRow, G + (18 * h)))
NEXT
NEXT
NEXT
CLS
GOSUB DrawFileRequester
DO
k$ = INKEY$
SELECT CASE k$
 CASE CHR$(0) + "H": GOSUB MoveCursorUpFR
 CASE CHR$(0) + "P": GOSUB MoveCursorDownFR
 CASE "": LET EscPressed = 1: LET KeyBoardCommandsUpdated = 0: GOSUB DrawFileRequester: LET FileRequesterStatus = 1: filerequester$ = "_NoFile": GOTO EndThisSub
END SELECT
IF k$ > "" THEN
 IF ASC(k$) = 13 THEN LET EnterPressed = 1: LET KeyBoardCommandsUpdated = 0: LET FileRequesterStatus = 0: GOSUB DrawFileRequester: LET ChosenFile$ = path$ + FileName$(chc + page): filerequest$ = ChosenFile$: GOTO EndThisSub
END IF
LOOP
'
MoveCursorUpFR:
IF chc - 1 = 0 AND page = 0 THEN RETURN
LET chc = chc - 1
IF chc = 0 THEN LET page = page - 1: LET chc = 1
GOSUB DrawFileRequester
RETURN
'
MoveCursorDownFR:
IF chc + 1 > 18 AND FileName$(chc + 1) = "" THEN RETURN
IF FileName$(chc + 1 + page) = "" THEN RETURN
LET chc = chc + 1
IF chc = 19 THEN LET page = page + 1: LET chc = 18
GOSUB DrawFileRequester
RETURN
'
DrawFileRequester:
PCOPY 0, 3
SCREEN 0, 0, 3
COLOR 7, 0
IF ScreenCleared = 0 THEN ScreenCleared = 1: CLS
LOCATE 2, 12
COLOR 9, 1: PRINT "²±²±²±²±²±²±²±²±²±²±²±²±²±²±²±²"
LOCATE 2, 13
COLOR 15, 1: PRINT topic$: COLOR 9, 1
FOR a = 1 TO 19
LOCATE 2 + a, 12
PRINT "²                             ²"
NEXT
LOCATE 21, 12: PRINT "²±²±²±²±²±²±²±²±²±²±²±²±²±²±²±²"
LOCATE 22, 12: PRINT "²                             ²"
LOCATE 22, 14: COLOR 15, 9: PRINT Listing$: COLOR 9, 1
LOCATE 23, 12: PRINT "²±²±²±²±²±²±²±²±²±²±²±²±²±²±²±²"
FOR a = 1 TO 18
LOCATE 2 + a, 13
IF chc = a THEN COLOR 15, 15
IF chc <> a THEN COLOR 9, 15
LOCATE 2 + a, 13: PRINT "±²±²±²±²±²±²±²±²±²±²±²±²±²±²±"
LOCATE 2 + a, 14: PRINT FileName$(a + page)
NEXT
IF KeyBoardCommandsUpdated = 1 THEN GOTO BlitScreen
LET KeyBoardCommandsUpdated = 1
LOCATE 2, 46: COLOR 9, 0: PRINT "Keyboard Commands"
LOCATE 4, 46
IF EscPressed = 1 THEN COLOR 12, 0: PRINT "(A)bort                       (ESC)"
IF EscPressed = 0 THEN COLOR 9, 0: PRINT "(A)bort                       (ESC)"
LOCATE 5, 46
IF EnterPressed = 1 THEN COLOR 10, 0: PRINT "(L)oad file                 (Enter)"
IF EnterPressed = 0 THEN COLOR 9, 0: PRINT "(L)oad file                 (Enter)"
LOCATE 6, 46
COLOR 9, 0: PRINT "(C)hange filetype to list     (TAB)"
BlitScreen:
SCREEN 0, 0, 0
PCOPY 3, 0
RETURN
'
EnterNewFileSpec:
LOCATE 22, 12: COLOR 9, 1: PRINT "²                             ²": COLOR 15, 9
LOCATE 22, 14: INPUT "", Listing$
LET KeyBoardCommandsUpdated = 0
KILL ProgPath$ + "fr.ini"
OPEN ProgPath$ + "fr.ini" FOR OUTPUT AS #6
PRINT #6, Listing$
CLOSE #6
ScreenCleared = 0: GOTO Relist
'
EndThisSub:

END FUNCTION

FUNCTION getbyte%
pc = pc + 1
GET #2, pc, byte$
getbyte = ASC(byte$)
END FUNCTION

SUB hagla (x, y, text$)
FOR o = 1 TO LEN(text$)
   IF ASC(MID$(text$, o, 1)) > 96 AND ASC(MID$(text$, o, 1)) < 123 THEN
      hus = ASC(MID$(text$, o, 1)) - 97
      GOTO rita
   END IF
   IF ASC(MID$(text$, o, 1)) > 48 AND ASC(MID$(text$, o, 1)) < 58 THEN
      hus = ASC(MID$(text$, o, 1)) - 20
      GOTO rita
   END IF
   SELECT CASE MID$(text$, o, 1)   'Bara 18 CASEar! Fjantigt! :)
      CASE "†": hus = 26
      CASE "„": hus = 27
      CASE "”": hus = 28
      CASE "0": hus = 38
      CASE "(": hus = 39
      CASE ")": hus = 40
      CASE "$": hus = 41
      CASE ",": hus = 42
      CASE "+": hus = 43
      CASE "-": hus = 44
      CASE "*": hus = 45
      CASE "/": hus = 46
      CASE "=": hus = 47
      CASE ":": hus = 48
      CASE ".": hus = 49
      CASE "!": hus = 50
      CASE "?": hus = 51
      CASE "#": hus = 52
      CASE " ": x = x + 2: GOTO mellanmjoelk
   END SELECT
rita:
   FOR h = 0 TO 5
      IF bokstav(hus, h) = 0 THEN x = x + 1: EXIT FOR
      IF (bokstav(hus, h) AND 1) THEN PSET (x, y)
      IF (bokstav(hus, h) AND 2) THEN PSET (x, y + 1)
      IF (bokstav(hus, h) AND 4) THEN PSET (x, y + 2)
      IF (bokstav(hus, h) AND 8) THEN PSET (x, y + 3)
      IF (bokstav(hus, h) AND 16) THEN PSET (x, y + 4)
      IF (bokstav(hus, h) AND 32) THEN PSET (x, y + 5)
      IF (bokstav(hus, h) AND 64) THEN PSET (x, y + 6)
      x = x + 1
   NEXT h
mellanmjoelk:
NEXT o

END SUB

SUB header
'Inl„sning av headern f”r att kolla mappers och storlekar...
'Just nu „r bara antaled prg-rom-bankar viktigt...
bytes$ = STRING$(32767, CHR$(0))
PUT #2, 1, bytes$
PUT #2, 32768, bytes$
PUT #4, 1, bytes$

GET #1, 5, byte$
bytes$ = ""
prgs = ASC(byte$)
bytes$ = STRING$(16384, " ")
IF prgs = 1 THEN
   GET #1, 17, bytes$             'Plocka ett prg-block.
   PUT #2, 32769, bytes$          'Stoppa det i b†de $8000
   PUT #2, 49153, bytes$          'och $C000.
   bytes$ = STRING$(8192, " ")
   GET #1, 16401, bytes$          'Plocka Pattern Tablesarna.
   PUT #4, 1, bytes$              'Stoppa dem i VRAMen.
ELSEIF prgs = 2 THEN
   GET #1, 17, bytes$             'Plocka ett prg-block.
   PUT #2, 32769, bytes$          'Stoppa det i $8000
   GET #1, 16401, bytes$          'Plocka ett till
   PUT #2, 49153, bytes$          'och stoppa det i $C000.
   bytes$ = STRING$(8192, " ")
   GET #1, 32785, bytes$          'Plocka Pattern Tablesarna.
   PUT #4, 1, bytes$              'Stoppa dem i VRAMen.
ELSE
   SCREEN 0
   PRINT "Unsupported mapper, or not a NES-file."
   CLOSE : CLOSE : CLOSE
   END
END IF
a = getbyte
'Reset vektor: FFFC (+1)
'Kolla reset-position:
byte$ = " "
GET #2, 65533, byte$
a = ASC(byte$)
GET #2, 65534, byte$
b = ASC(byte$)
pc = a + b * 256 + 1
GET #2, 65531, byte$
a = ASC(byte$)
GET #2, 65532, byte$
b = ASC(byte$)
a = a + b * 256
nmi = a
GET #2, 65535, byte$
a = ASC(byte$)
GET #2, 65536, byte$
b = ASC(byte$)
a = a + b * 256
brk = a
byte$ = " "
COLOR 13
hagla 306, 1, "mapper:"
hagla 295, 9, "prg banks:"
hagla 295, 17, "chr banks:"
hagla 280, 25, "reset vector:"
hagla 292, 33, "nmi vector:"
hagla 290, 41, "brk vector:"
COLOR 11
hagla 340, 1, "$0"
hagla 340, 9, "$" + LCASE$(HEX$(prgs))
hagla 340, 17, "$1"
hagla 340, 25, "$" + LCASE$(HEX$(pc - 1))
hagla 340, 33, "$" + LCASE$(HEX$(nmi))
hagla 340, 41, "$" + LCASE$(HEX$(brk))
END SUB

DEFSNG C, R
SUB loadfont
OPEN "font.dat" FOR BINARY AS #7
byte$ = " "
hus = -1
10 hus = hus + 1: del = -1
11 del = del + 1
GET #7, , byte$
bokstav(hus, del) = ASC(byte$)
IF ASC(byte$) = 0 THEN del = 0: GOTO 10
IF ASC(byte$) = 255 THEN bokstav(hus, del) = 0: GOTO 12
GOTO 11
12 CLOSE #7

END SUB

DEFINT C, R
SUB moveup
DIM block(180, 14)
FOR y = 280 TO 392 STEP 14
   GET (460, y)-(639, y + 14), block
   PUT (460, y - 14), block, PSET
NEXT y
GET (460, 409)-(639, 423), block
PUT (460, 392), block, PSET
LINE (460, 409)-(639, 423), 0, BF
END SUB

DEFINT A
SUB op.adc (typ)
a = anybyte(typ)
cc = (reg.s AND 1)
reg.s = reg.s AND 60
ca = reg.a
IF ca > 127 THEN ca = ca - 256
co = a
IF co > 127 THEN co = co - 256
ca = ca + co + cc
IF ca > 127 OR ca < -128 THEN reg.s = reg.s OR 64
IF reg.a + a + cc > 255 THEN reg.s = reg.s OR 1
IF ca < 0 THEN ca = ca + 256
reg.a = ca
IF reg.a > 127 THEN reg.s = reg.s OR 128
IF reg.a = 0 THEN reg.s = reg.s OR 2                         'Z?
END SUB

DEFSNG A
SUB op.and (typ)
'AND: ANDa Ackumulatorn med minne/operand.
r = anybyte(typ)
reg.a = reg.a AND r
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
END SUB

SUB op.asl (typ)
'ASL: Shift Left...
reg.s = reg.s AND 254
b% = anybyte(typ)
b% = b% * 2
IF (b% AND 256) THEN b% = b% AND 255: reg.s = reg.s OR 1
putbyte b%, typ
reg.s = reg.s AND 125
IF b% = 0 THEN reg.s = reg.s OR 2
IF b% > 127 THEN reg.s = reg.s OR 128
cycle cy
END SUB

DEFINT A
SUB op.bcc
'BCC: Brancha om c=0...
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 1) = 0 THEN pc = pc + a
cycle 2                                'OBS! Konstig.



END SUB

SUB op.bcs
'BCS: Brancha om c=1
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 1) THEN pc = pc + a
cycle 2                                'OBS! Konstig.
END SUB

SUB op.beq
'BEQ: Brancha om Z=1
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 2) THEN pc = pc + a
cycle 2                                'OBS! Konstig.
END SUB

DEFINT B
DEFSNG A
SUB op.bit (typ)
'BIT: ANDa minnemed ackumulatorn och f”r ”ver bit 7 och 8
'     till status-registret(!?)
b = anybyte(typ)
reg.s = reg.s AND 61
reg.s = (reg.s OR (b AND 192))
a = (reg.a AND b)
IF a = 0 THEN reg.s = reg.s OR 2

END SUB

DEFINT A
DEFSNG B
SUB op.bmi
'BMI: Brancha om s=1
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 128) THEN pc = pc + a
cycle 2                                   'OBS!
END SUB

SUB op.bne
'BNE: Brancha om Z=0
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 2) = 0 THEN pc = pc + a
cycle 2                                'OBS!
END SUB

SUB op.bpl
'BPL: Brancha om s=0
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 128) = 0 THEN pc = pc + a
cycle 2                                   'OBS!
END SUB

DEFSNG A
SUB op.brk
'BRK: Brk helt enkelt... :(
IF (reg.s AND 4) = 0 THEN
   pc = pc + 2
   b% = (pc AND 65280) / 256
   a% = (pc AND 255)
   push b%
   push a%
   push reg.s OR 16             'L„gg status p† stacken med b-biten.
   reg.s = reg.s OR 4
   pc = brk
END IF
END SUB

DEFINT A
SUB op.bvc
'BVC: Brancha om V=0.
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 1) = 0 THEN pc = pc + a
cycle 2                                'OBS! Konstig.
END SUB

SUB op.bvs
'BVS: Brancha om V=1
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 1) THEN pc = pc + a
cycle 2                                'OBS! Konstig.
END SUB

DEFSNG A
SUB op.clc
'CLC: Rensa carry-flagen...
reg.s = reg.s AND 254
cycle 2
END SUB

SUB op.cld
'CLD: Rensa Decimal-flagen.
reg.s = reg.s AND 247
cycle 2
END SUB

SUB op.cli
'CLI: Rensa Interrupt disable-flagen.
reg.s = reg.s AND 251
cycle 2
END SUB

SUB op.clv
'CLV: Rensa Owerflow-flagen.
reg.s = reg.s AND 191
cycle 2
END SUB

SUB op.cmp (typ)
'CMP J„mf”r operand och reg.a. (reg.a-operand)...
r = anybyte(typ)
reg.s = reg.s AND 124
IF r = reg.a THEN reg.s = reg.s OR 3
IF ((reg.a - r) AND 128) THEN reg.s = reg.s OR 128
IF r < reg.a THEN reg.s = reg.s OR 1
END SUB

SUB op.cpx (typ)
'CPX: J„mf”r X-registret och minne/operand.
r = anybyte(typ)
reg.s = reg.s AND 124
IF r = reg.x THEN reg.s = reg.s OR 3
IF ((reg.x - r) AND 128) THEN reg.s = reg.s OR 128
IF r < reg.x THEN reg.s = reg.s OR 1
END SUB

SUB op.cpy (typ)
'CPY: J„mf”r Y-registret och minne/operand.
r = anybyte(typ)
reg.s = reg.s AND 124
IF r = reg.y THEN reg.s = reg.s OR 3
IF ((reg.y - r) AND 128) THEN reg.s = reg.s OR 128
IF r < reg.y THEN reg.s = reg.s OR 1
END SUB

DEFINT B
SUB op.dec (typ)
'DEC: Minska byte i minnet med 1.
b = anybyte(typ)
b = b - 1
IF b = -1 THEN b = 255
reg.s = reg.s AND 125
IF b = 0 THEN reg.s = reg.s OR 2
IF b > 127 THEN reg.s = reg.s OR 128
putbyte b, typ
cycle cy
END SUB

DEFSNG B
SUB op.dex
'DEX: X-registret minskas med 1.
reg.x = reg.x - 1
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x = -1 THEN reg.x = 255
IF reg.x > 127 THEN reg.s = reg.s OR 128
cycle 2
END SUB

SUB op.dey
'DEY: Y-registret minskas med 1.
reg.y = reg.y - 1
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2                 'Z?
IF reg.y = -1 THEN reg.y = 255
IF reg.y > 127 THEN reg.s = reg.s OR 128  'S?
cycle 2
END SUB

DEFINT B
SUB op.eor (typ)
'EOR: XORa minne/operand med ackumulatorn.
b = anybyte(typ)
reg.a = reg.a XOR b
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2      'Z?
IF reg.a > 127 THEN reg.s = reg.s OR 128  'S?
END SUB

SUB op.inc (typ)
'INC: ™ka byte i minnet med 1.
b = anybyte(typ)
b = b + 1
reg.s = reg.s AND 125
IF b > 255 THEN b = 0: reg.s = reg.s OR 2
IF b > 127 THEN reg.s = reg.s OR 128
putbyte b, typ
cycle cy
END SUB

DEFSNG B
SUB op.inx
'INX: X-registret ”kas med 1.
reg.x = reg.x + 1
IF reg.x = 256 THEN reg.x = 0    'Kanske overflow...

reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128

cycle 2
END SUB

SUB op.iny
'INY: Y-registret ”kas med 1.
reg.y = reg.y + 1
IF reg.y = 256 THEN reg.y = 0
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg.s = reg.s OR 128
cycle 2
END SUB

SUB op.jmp (typ)
'JMP: Hoppa till ny position...
'Det h„r ser konstigt ut i och med att 1 inte l„ggs till p† adressen.
'Det beror p† att pc ”kas med ett efter varje opcode „nd†... :)
SELECT CASE typ
   CASE 1                          'Direkt. Tv† operander.
      pc = abso
      cycle 3
CASE 2                             'Indirekt. Tv† operander.
      b = abso
      a = anybyte(b)
      b = anybyte(b + 1)
      pc = a + b * 256
      cycle 5
END SELECT
END SUB

SUB op.jsr
'JSR: L„gg pc p† stacken och hoppa.
a = getbyte
b = getbyte
pc = pc - 1
push ((pc AND 65280) / 256)
push (pc AND 255)
pc = a + b * 256
cycle 6
END SUB

SUB op.lda (typ)
'LDA: Ladda Ackumulatorn med [operand(er)]
reg.a = anybyte(typ)
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2  'Z?
IF reg.a > 127 THEN reg.s = reg.s OR 128'S?
cycle cy

END SUB

SUB op.ldx (typ)
reg.x = anybyte(typ)
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
cycle cy
END SUB

SUB op.ldy (typ)
'LDY: Stoppa reg.y fullsmockat!
reg.y = anybyte(typ)
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg.s = reg.s OR 128
cycle cy
END SUB

SUB op.lsr (typ)
'LSR: Skifta h”ger (Dela med tv†).
reg.s = reg.s AND 254
b% = anybyte(typ)
reg.s = (reg.s OR (b% AND 1)) 'Fixa carry.
b% = b% \ 2
putbyte b%, typ
reg.s = reg.s AND 125
IF b% = 0 THEN reg.s = reg.s OR 2
cycle cy
END SUB

DEFINT B
SUB op.ora (typ)
'ORA: ORa Ackumulatorn med operand/minne. Reultat i Ackumulatorn.
b = anybyte(typ)
reg.a = reg.a OR b
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
END SUB

DEFSNG B
SUB op.pha
'PHA: L„gg Ackumulatorn p† stacken...
push reg.a 'Se ovan...
cycle 3
END SUB

SUB op.php
'PHP: L„gg statusregistret p† stacken(!)
push reg.s
cycle 3
END SUB

SUB op.pla
'PLA: Stackens ”versta byte l„ggs i Ackumulatorn.
reg.a = pop
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
'OBS! Byten som l„ses in ligger fortfarande kvar i stacken.
'Detta leder till h”gre hastighet, men kanske ocks† haveri. Troligtvis inte.

cycle 4
END SUB

SUB op.plp
'PLP: Plocka status-registret fr†n stacken.
reg.s = pop
cycle 4
END SUB

DEFINT B
SUB op.rol (typ)
'ROL: Rotera v„nster.
b = anybyte(typ)
b = b + b + (reg.s AND 1)
IF b < 256 THEN
   reg.s = reg.s AND 254
ELSE
   b = b - 256: reg.s = reg.s OR 1
END IF
putbyte b, typ
reg.s = reg.s AND 125
IF b = 0 THEN reg.s = reg.s OR 2
IF b > 127 THEN reg.s = reg.s OR 128
END SUB

DEFSNG B
SUB op.ror (typ)
'ROR: Rotera h”ger.
'N„stan alla varianter „r snabbt ihopsl„ngda, och inneh†ller
'f”rmodligen fler buggar „n den att de inte wrappar...
'^Felaktig info...
c = anybyte(typ)
c = c + (reg.s AND 1) * 256
reg.s = reg.s AND 254
reg.s = (reg.s OR (c AND 1))
c = c \ 2
putbyte c, typ
reg.s = reg.s AND 125
IF c = 0 THEN reg.s = reg.s OR 2
IF c > 127 THEN reg.s = reg.s OR 128
END SUB

SUB op.rti
'RTI: Return from Interrupt
reg.s = pop AND 239
a = pop
b = pop
pc = a + b * 256
END SUB

SUB op.rts
'RTS: L„s pc fr†n stacken.
a = pop              'F”rst den l†ga byten
b = pop              'Sedan den h”ga :)
pc = a + b * 256 + 1
cycle 6
END SUB

DEFINT A
SUB op.sbc (typ)
'SBC: Ackumulatorn - operanden/minnesbyten - C xor 1
'a = anybyte(typ)
'IF (a AND 128) THEN a = a - 256
'IF (reg.a AND 128) THEN reg.a = reg.a - 256
'reg.a = (reg.a - a) - ((reg.s AND 1) XOR 1)
'reg.s = reg.s AND 60
'IF reg.a < -128 OR reg.a > 127 THEN reg.s = reg.s OR 64
'IF a >= 0 THEN
'   IF reg.a >= 0 THEN IF (reg.a + a + ((reg.s AND 1) XOR 1)) >= 0 THEN reg.s = reg.s OR 1
'ELSE
'   IF reg.a >= 0 THEN IF (reg.a + a + ((reg.s AND 1) XOR 1)) < 0 THEN reg.s = reg.s OR 1
'END IF
'IF reg.a < 0 THEN reg.a = reg.a + 256
'IF reg.a = 0 THEN reg.s = reg.s OR 2
''IF reg.a > 255 THEN PRINT "Ufo!": reg.a = reg.a - 256
'IF reg.a > 127 THEN reg.s = reg.s OR 128
a = anybyte(typ)
cc = (reg.s AND 1) XOR 1
reg.s = reg.s AND 60
ca = reg.a
IF ca > 127 THEN ca = ca - 256
co = a
IF co > 127 THEN co = co - 256
ca = ca - co - cc
IF ca < -128 OR ca > 127 THEN reg.s = reg.s OR 64
IF co > 0 THEN IF ca >= 0 THEN reg.s = reg.s OR 1
reg.a = reg.a - co - cc
IF reg.a < 0 THEN reg.a = reg.a + 256
IF reg.a > 255 THEN reg.s = reg.s OR 1: reg.a = reg.a - 256
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
END SUB

DEFSNG A
SUB op.sec
'SEC: S„tt Carry-flagen.
reg.s = reg.s OR 1
cycle 2
END SUB

SUB op.sed
'SED: S„tt Decimal mode-flagen.
reg.s = reg.s OR 8
cycle 2
END SUB

SUB op.sei
'SEI: I-flagen s„tts till 1.
reg.s = reg.s OR 4
cycle 2
END SUB

SUB op.sta (typ)
'STA: Stoppa Ackumulatorn i minnet.
putbyte reg.a, typ
cycle cy
END SUB

SUB op.stx (typ)
'Stoppa reg.x i minnet...
putbyte reg.x, typ
cycle cy
END SUB

SUB op.sty (typ)
'STY: Stoppa reg.y i minnet.
putbyte reg.y, typ
cycle cy
END SUB

SUB op.tax
'TAX: Inneh†llet i Ackumulatorn f”rs ”ver till X-registret.
reg.x = reg.a
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
cycle 2
END SUB

SUB op.tay
'TAY: Inneh†llet i Ackumulatorn f”rs ”ver till Y-registret.
reg.y = reg.a
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg.s = reg.s OR 128
cycle 2
END SUB

DEFSNG C, R
SUB op.tsx
'TSX: Stack-pekarens inneh†ll f”rs ”ver till X-registret.
reg.x = reg.sp
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
cycle 2
END SUB

DEFINT C, R
SUB op.txa
'TXA: Inneh†llet i X-registret f”rs ”ver till Ackumulatorn.
reg.a = reg.x
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
cycle 2
END SUB

SUB op.txs
'TXS X-registret f”rs ”ver till stackpekaren...
reg.sp = reg.x
cycle 2
END SUB

SUB op.tya
'TYA: Inneh†llet i Y-registret f”rs ”ver till ackumulatorn.
reg.a = reg.y
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
cycle 2
END SUB

SUB patterntables
DIM linje(7)
'Rita upp patterntablesarna som ligger i VRAMen...
bytes$ = STRING$(8192, " ")
GET #4, 1, bytes$
a = 1
FOR table = 0 TO 1
   FOR ty = 0 TO 120 STEP 8
      FOR tx = 0 TO 120 STEP 8
         FOR y = 0 TO 7
            FOR planes = 0 TO 1
               IF (ASC(MID$(bytes$, a + planes * 8, 1)) AND 128) = 128 THEN linje(0) = linje(0) + planes + 1
               IF (ASC(MID$(bytes$, a + planes * 8, 1)) AND 64) = 64 THEN linje(1) = linje(1) + planes + 1
               IF (ASC(MID$(bytes$, a + planes * 8, 1)) AND 32) = 32 THEN linje(2) = linje(2) + planes + 1
               IF (ASC(MID$(bytes$, a + planes * 8, 1)) AND 16) = 16 THEN linje(3) = linje(3) + planes + 1
               IF (ASC(MID$(bytes$, a + planes * 8, 1)) AND 8) = 8 THEN linje(4) = linje(4) + planes + 1
               IF (ASC(MID$(bytes$, a + planes * 8, 1)) AND 4) = 4 THEN linje(5) = linje(5) + planes + 1
               IF (ASC(MID$(bytes$, a + planes * 8, 1)) AND 2) = 2 THEN linje(6) = linje(6) + planes + 1
               IF (ASC(MID$(bytes$, a + planes * 8, 1)) AND 1) = 1 THEN linje(7) = linje(7) + planes + 1
            NEXT planes
            FOR b = 0 TO 7
               PSET (table * 128 + tx + b, 300 + ty + y), linje(b)
               linje(b) = 0
            NEXT b
            a = a + 1
         NEXT y
         a = a + 8
      NEXT tx
   NEXT ty
NEXT table

END SUB

FUNCTION pop%
reg.sp = reg.sp + 1
IF reg.sp = 256 THEN reg.sp = 0
GET #2, 257 + reg.sp, byte$
pop% = ASC(byte$)
END FUNCTION

FUNCTION post
c = getbyte
a = anybyte(INT(c))
b = anybyte(c + 1)
a = a + (b * 256) + reg.y
IF (a AND 65536) THEN a = a AND 65535
post = a
END FUNCTION

FUNCTION pre
c = getbyte
c = c + reg.x
IF (c AND 256) THEN c = c AND 255
a = anybyte(INT(c))
b = anybyte(c + 1)
pre = a + b * 256
END FUNCTION

SUB push (r)
byte$ = CHR$(r)
PUT #2, 257 + reg.sp, byte$
reg.sp = reg.sp - 1
IF reg.sp < 0 THEN reg.sp = 255
END SUB

SUB putbyte (byte%, posi)
byte$ = CHR$(byte%)
'Kolla om n†gon speciell adress „r „ndrad.
IF posi >= &H2000 THEN              'Mer „n $1FFF?
   IF posi <= &H2007 THEN           'Mindre „n $2008?
     
      IF posi = &H2000 THEN ppu1 = byte%
      IF posi = &H2001 THEN ppu2 = byte%
      IF posi = &H2003 THEN sprramposi = byte%
      IF posi = &H2004 THEN
         'LOCATE 10 + sprramposi, 40: PRINT sprramposi; "="; byte%
         sprramposi = sprramposi + 1
         MID$(sprram$, sprramposi, 1) = byte$
      END IF
      IF posi = &H2006 THEN         'Žndra adress f”r VRAM-l„sning/skrivning.
         vramposi = (vramposi AND 255) * 256 + byte%
         firstread = 1
      END IF
      okn = 1
      IF (ppu1 AND 4) THEN okn = 32
      IF posi = &H2007 THEN PUT #4, vramposi + 1, byte$: vramposi = vramposi + okn: IF vramposi > 32767 THEN vramposi = vramposi - 32768': BEEP
      'LOCATE 3, 40: PRINT HEX$(vramposi) + " = " + HEX$(byte%)
                        'Skriv till VRAM, och ”ka VRAM-"pointern".
      EXIT SUB
   END IF
END IF

IF posi >= &H4000 THEN
IF posi <= &H4017 THEN

   IF posi = &H4014 THEN
      c = byte% * 256 + 1
      GET #2, c, sprram$
      cycle 513
   END IF
   IF posi = &H4016 THEN
      IF (byte% AND 1) THEN
         IF joy1reset = 0 THEN joy1reset = 1
      ELSE
         IF joy1reset = 1 THEN joy1reset = 0: joy1reads = 1
      END IF
   END IF
  
   EXIT SUB

END IF
END IF
posi = posi + 1
PUT #2, posi, byte$
'PUT #2, posi + &H800, byte$
'PUT #2, posi + &H1000, byte$
'PUT #2, posi + &H1800, byte$

END SUB

SUB utskrift
IF INKEY$ <> "" THEN cycles = 27007
moveup
COLOR 9
hagla 460, 409, LCASE$("$" + HEX$(bytes) + " " + opcode$)
'PRINT bytes; opcode$
hagla 460, 416, LCASE$("a:$" + HEX$(reg.a) + " x:$" + HEX$(reg.x) + " y:$" + HEX$(reg.y) + " sp:$" + HEX$(reg.sp) + " pc:$" + HEX$(pc))
'PRINT "A:"; HEX$(reg.a); " X:"; HEX$(reg.x); " Y:"; HEX$(reg.y); " S:"; HEX$(reg.sp); " PC:"; HEX$(pc); " ";
'Status: SV1BDIZC
COLOR 8
IF (reg.s AND 1) = 1 THEN COLOR 7
hagla 635, 416, "c"
COLOR 8
IF (reg.s AND 2) = 2 THEN COLOR 7
hagla 630, 416, "z"
COLOR 8
IF (reg.s AND 4) = 4 THEN COLOR 7
hagla 628, 416, "i"
COLOR 8
IF (reg.s AND 8) = 8 THEN COLOR 7
hagla 623, 416, "d"
COLOR 8
IF (reg.s AND 16) = 16 THEN COLOR 7
hagla 618, 416, "b"
COLOR 8
IF (reg.s AND 64) = 64 THEN COLOR 7
hagla 613, 416, "v"
COLOR 8
IF (reg.s AND 128) = 128 THEN COLOR 7
hagla 608, 416, "s"
'PRINT "     Cycles:"; cycles
'PRINT
'DO WHILE INKEY$ = "": LOOP
SLEEP
END SUB

FUNCTION zp
a = getbyte
zp = a
END FUNCTION

DEFINT A
FUNCTION zpx
a = getbyte
a = a + reg.x
IF (a AND 256) THEN a = a AND 255
zpx = a
END FUNCTION

FUNCTION zpy
a = getbyte
a = a + reg.y
IF (a AND 256) THEN a = a AND 255
zpy = a
END FUNCTION

