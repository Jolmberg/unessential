DECLARE SUB save ()
DECLARE SUB vb ()
DECLARE SUB config ()
DECLARE FUNCTION filerequest$ ()
DECLARE FUNCTION zpx& ()
DECLARE FUNCTION zpy& ()
DECLARE FUNCTION abso& ()
DECLARE FUNCTION absx& ()
DECLARE FUNCTION absy& ()
DECLARE FUNCTION pre& ()
DECLARE FUNCTION post& ()
DECLARE FUNCTION zp& ()
DECLARE FUNCTION breakpoint! ()
DECLARE SUB op.bvs ()
DECLARE SUB op.bvc ()
DECLARE SUB op.sbc (typ&)
DECLARE SUB op.brk ()
DECLARE SUB op.php ()
DECLARE SUB op.plp ()
DECLARE SUB op.cpy (typ&)
DECLARE SUB op.bit (typ&)
DECLARE SUB op.cpx (typ&)
DECLARE SUB op.eor (typ&)
DECLARE SUB crappygfxengine ()
DECLARE SUB op.bcc ()
DECLARE SUB patterntables ()
DECLARE SUB op.rti ()
DECLARE SUB op.dec (typ&)
DECLARE SUB op.sty (typ&)
DECLARE SUB op.stx (typ&)
DECLARE SUB op.and (typ&)
DECLARE SUB op.bcs ()
DECLARE SUB op.inc (typ&)
DECLARE SUB op.ldy (typ&)
DECLARE SUB op.cmp (typ&)
DECLARE SUB op.rts ()
DECLARE FUNCTION pop% ()
DECLARE SUB op.jsr ()
DECLARE SUB push (r%)
DECLARE SUB moveup ()
DECLARE SUB loadfont ()
DECLARE FUNCTION anybyte% (posi&)
DECLARE SUB putbyte (byte%, posi&)
DECLARE FUNCTION getbyte% ()
DECLARE SUB hagla (x, y, text$)
DECLARE SUB op.tya ()
DECLARE SUB op.adc (typ&)
DECLARE SUB op.bpl ()
DECLARE SUB op.bmi ()
DECLARE SUB op.jmp (typ&)
DECLARE SUB op.txs ()
DECLARE SUB header ()
DECLARE SUB op.bne ()
DECLARE SUB op.beq ()
DECLARE SUB utskrift ()
DECLARE SUB op.ldx (typ&)
DECLARE SUB op.rol (typ&)
DECLARE SUB op.ror (typ&)
DECLARE SUB op.asl (typ&)
DECLARE SUB op.sec ()
DECLARE SUB op.sed ()
DECLARE SUB op.cli ()
DECLARE SUB op.clv ()
DECLARE SUB op.clc ()
DECLARE SUB op.cld ()
DECLARE SUB op.sei ()
DECLARE SUB op.ora (typ&)
DECLARE SUB op.lsr (typ&)
DECLARE SUB op.sta (typ&)
DECLARE SUB op.pla ()
DECLARE SUB op.pha ()
DECLARE SUB op.tsx ()
DECLARE SUB op.txa ()
DECLARE SUB op.dex ()
DECLARE SUB op.dey ()
DECLARE SUB op.lda (typ&)
DECLARE SUB op.inx ()
DECLARE SUB op.iny ()
DECLARE SUB op.tax ()
DECLARE SUB op.tay ()
DECLARE SUB emulate ()
DEFINT C, R
'Status: SV1BDIZC
' All registers must be shared
loadfont
DIM SHARED reg.a AS INTEGER, reg.x AS INTEGER, reg.y AS INTEGER
DIM SHARED reg.s AS INTEGER, pc AS LONG, reg.sp AS INTEGER
DIM SHARED nmi AS LONG, brk AS LONG
DIM SHARED fil$, byte$, opcode$
DIM SHARED ut AS INTEGER
DIM SHARED bokstav(52, 5) AS INTEGER, rabies
DIM SHARED vramposi AS INTEGER, vblank AS INTEGER, ppu1 AS INTEGER
DIM SHARED sprramposi AS INTEGER, sprram(256) AS INTEGER
DIM SHARED ppustat AS INTEGER, sprite0(1) AS INTEGER, lines AS INTEGER
DIM SHARED s AS INTEGER, joy1emu AS INTEGER, joy1reads AS INTEGER
DIM SHARED joy1reset AS INTEGER, ppu2 AS INTEGER, firstread AS INTEGER
DIM SHARED bpoint AS LONG, skip AS INTEGER
DIM SHARED mirroring AS INTEGER, scrollwrite AS INTEGER
DIM SHARED hscroll AS INTEGER, vscroll AS INTEGER
DIM SHARED pal(63, 2) AS INTEGER, colour(15) AS INTEGER
DIM SHARED spr(15) AS INTEGER, palemu AS INTEGER, frame AS INTEGER
DIM SHARED bank1(16383) AS INTEGER, bank2(16383) AS INTEGER
DIM SHARED ram(2047) AS INTEGER, vram(16383) AS INTEGER
ON ERROR GOTO 14
GOTO 7
14
SCREEN 0
PRINT "D'oh!"
PRINT "If this was caused by the file requester, don't worry about it."
PRINT "If it was caused by something else, please mail us."
END
7 firstread = 1
skip = 2
'uNESsential NES emulator. Version 0.20+
'
'Changes:
' Comments translated into English
' BVC/BVS fixed, were copies of BCC/BCS
' Use arrays instead of files to emulate memory
'
'General info:
'opcode functions are called op.???(?).
'All register variables are called reg.?. (exception: pc, which cannot be INT)

'Variables:
'fil$ - Filname of the current rom.
'byte$ - variable for reading a byte from memory.

byte$ = CHR$(0)
CLS
'Load a rom
config
fil$ = COMMAND$
IF fil$ = "" THEN fil$ = filerequest$
IF fil$ = "" THEN END
SCREEN 12

CLS

'Preparations
joy1reads = 1
FOR r = 1 TO 253
   sprram(r) = &HF0
NEXT r
COLOR 12
hagla 1, 270, "joypads"
hagla 1, 290, "pattern tables"
OPEN fil$ FOR BINARY AS #1         'The rom has file number #1.
reg.s = &H2C                       'Status bit 6=1 (?)
reg.sp = 255                       'Stack pointer should be 255(!?)
header
LINE (459, 407)-(639, 407), 8, , 21845
patterntables
emulate                            'Start emulating!

FUNCTION abso&
pc = pc + 1
a = anybyte(pc)
pc = pc + 1
b = anybyte(pc)
abso = a + b * 256
END FUNCTION

FUNCTION absx&
pc = pc + 1
a = anybyte(pc)
pc = pc + 1
b = anybyte(pc)
absx = (a + b * 256 + reg.x) AND 65535
END FUNCTION

FUNCTION absy&
pc = pc + 1
a = anybyte(pc)
pc = pc + 1
b = anybyte(pc)
absy = (a + b * 256 + reg.y) AND 65535
END FUNCTION

FUNCTION anybyte% (posi AS LONG)
IF (posi AND &H8000) THEN
   IF (posi AND &H4000) THEN
      anybyte% = bank2(posi AND &H3FFF)
   ELSE
      anybyte% = bank1(posi AND &H3FFF)
   END IF
ELSEIF posi < &H800 THEN
   anybyte% = ram(posi)
ELSEIF posi = &H2002 THEN
   anybyte% = ppustat: ppustat = ppustat AND 127
ELSEIF posi = &H2004 THEN
   anybyte% = sprram(sprramposi)
ELSEIF posi = &H2007 THEN
   okn = 1
   IF (ppu1 AND 4) THEN okn = 32
   anybyte% = vram(vramposi)
   IF firstread = 0 THEN
      vramposi = vramposi + okn
   END IF
   firstread = 0
ELSEIF posi = &H4016 THEN
   IF (joy1emu AND joy1reads) = joy1reads THEN anybyte% = 1
   joy1reads = joy1reads * 2
   IF joy1reads > 256 THEN joy1reads = 256
END IF
END FUNCTION

FUNCTION breakpoint
COLOR 14
hagla 400, 1, "breakpoint:"
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
LINE (450, 1)-(470, 9), 0, BF
hagla 450, 1, LCASE$(HEX$(bp4 \ 4096) + HEX$(bp3 \ 256) + HEX$(bp2 \ 16) + HEX$(bp1))
RETURN
END FUNCTION

SUB config                          'Read the cfg file (rom directory) :)
OPEN "uness.cfg" FOR INPUT AS #10
INPUT #10, o$
CLOSE
IF o$ = "-" THEN
   OPEN "uness.cfg" FOR INPUT AS #10
   PRINT "Welcome to uNESsential v0.20"
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
SUB crappygfxengine        'Probably the worst graphics engine ever created!
'Background.
'IF (ppu2 AND 8) THEN
DIM imgpal(15)
DIM tile(7, 7)
DIM bk(63, 59)
DIM attr(31, 31)
IF (ppu1 AND 16) = 16 THEN okning = 128
o = ppu1 AND 3
IF o = 0 THEN adr = &H2000
IF o = 1 THEN adr = &H2400
IF o = 2 THEN adr = &H2800
IF o = 3 THEN adr = &H2C00
'Set palette(s).
LOCATE 1, 63
ERASE colour
FOR c = 0 TO 15
   r = vram(&H3F00 + c) AND 15
   colour(r) = c
   IF palemu = 1 THEN PALETTE c, 65536 * pal(r, 2) + 256 * pal(r, 1) + pal(r, 0)
   'COLOR c
   'PRINT "ß";
NEXT c
LOCATE 2, 63
FOR c = 0 TO 15
   r = vram(&H3F10 + c) AND 15
   'COLOR colour(r)
   spr(c) = colour(r)
   'PRINT "ß";
NEXT c

'Build the 32*30 tile matrix.
FOR cy = 0 TO 29
   FOR cx = 0 TO 31
      bk(cx, cy) = vram(&H2000 + cy * 32 + cx)
   NEXT cx
NEXT cy
FOR cy = 0 TO 29
   FOR cx = 32 TO 63
      bk(cx, cy) = vram(&H2400 + cy * 32 + cx - 32)
   NEXT cx
NEXT cy
FOR cy = 30 TO 59
   FOR cx = 0 TO 31
      bk(cx, cy) = vram(&H2800 + (cy - 30) * 32 + cx)
   NEXT cx
NEXT cy
FOR cy = 30 TO 59
   FOR cx = 32 TO 63
      bk(cx, cy) = vram(&H2C00 + (cy - 30) * 32 + cx - 32)
   NEXT cx
NEXT cy
FOR cy = 0 TO 7
   FOR cx = 0 TO 7
      c = vram(&H23C0 + cy * 8 + cx)
      attr(cx * 2, cy * 2) = c AND 3
      attr(cx * 2 + 1, cy * 2) = (c \ 4) AND 3
      attr(cx * 2, cy * 2 + 1) = (c \ 16) AND 3
      attr(cx * 2 + 1, cy * 2 + 1) = c \ 64
   NEXT cx
NEXT cy
FOR cy = 0 TO 7
   FOR cx = 8 TO 15
      c = vram(&H27C0 + cy * 8 + cx - 8)
      attr(cx * 2, cy * 2) = c AND 3
      attr(cx * 2 + 1, cy * 2) = (c \ 4) AND 3
      attr(cx * 2, cy * 2 + 1) = (c \ 16) AND 3
      attr(cx * 2 + 1, cy * 2 + 1) = c \ 64
   NEXT cx
NEXT cy
FOR cy = 8 TO 15
   FOR cx = 0 TO 7
      c = vram(&H2BC0 + (cy - 8) * 8 + cx)
      attr(cx * 2, cy * 2 - 1) = c AND 3
      attr(cx * 2 + 1, cy * 2 - 1) = (c \ 4) AND 3
      attr(cx * 2, cy * 2) = (c \ 16) AND 3
      attr(cx * 2 + 1, cy * 2) = c \ 64
   NEXT cx
NEXT cy
FOR cy = 8 TO 15
   FOR cx = 8 TO 15
      c = vram(&H2FC0 + (cy - 8) * 8 + cx - 8)
      attr(cx * 2, cy * 2) = c AND 3
      attr(cx * 2 + 1, cy * 2) = (c \ 4) AND 3
      attr(cx * 2, cy * 2 + 1) = (c \ 16) AND 3
      attr(cx * 2 + 1, cy * 2 + 1) = c \ 64
   NEXT cx
NEXT cy
cx = hscroll \ 8
cy = vscroll \ 8
cxa = hscroll \ 16
cya = vscroll \ 16
cxb = hscroll AND 15
cyb = vscroll AND 15
IF o = 1 THEN cx = cx + 32: cxa = cxa + 16
IF o = 2 THEN cy = cy + 30: cya = cya + 15
cxm = hscroll AND 7
cym = vscroll AND 7
y = 15
x = 15
FOR b = 0 TO 15
   FOR a = 0 TO 15
      c = a + cxa
      d = b + cya
      IF c > 31 THEN c = c - 32
      IF d > 29 THEN d = d - 30
      'LOCATE 10, 40: PRINT c
      LINE ((cxb XOR 15) + a * 16, (cyb XOR 15) + b * 16)-((cxb XOR 15) + a * 16 + 15, (cym XOR 15) + b * 16 + 15), attr(c, d) * 4, BF
   NEXT a
NEXT b

FOR bb = cy TO cy + 29
   FOR ab = cx TO cx + 31
      bbb = bb
      IF bbb > 59 THEN bbb = bbb - 60
      abb = ab
      IF abb > 63 THEN abb = abb - 64
      'LOCATE 1, 40: PRINT ASC(MID$(bytes$, c, 1))
      a = okning + ((bk(abb, bbb) AND 15) * 8)
      b = 300 + ((bk(abb, bbb) AND 240) / 2)
      GET (a, b)-(a + 7, b + 7), tile
      'PSET (a, b), 12
      PUT (x - cxm, y - cym), tile, OR
      x = x + 8
      IF x = 271 THEN y = y + 8: x = 15
   NEXT ab
NEXT bb
'END IF

'Sprites.
'IF (ppu2 AND 16) THEN
asize = (ppu1 AND 32)
okning = (ppu1 AND 8) * 16
IF asize = 0 THEN
   FOR c = 0 TO 252 STEP 4
      y = sprram(c)
      IF y < &HF0 THEN
         y = y + 15
         x = sprram(c + 3) + 15
         attr = sprram(c + 2)
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
         rpal = (attr AND 3) * 4
         a = okning + ((sprram(c + 1) AND 15) * 8)
         b = 300 + ((sprram(c + 1) AND 240) \ 2)
         FOR cy = sy TO (sy XOR 7) STEP sys
            FOR cx = sx TO (sx XOR 7) STEP sxs
               IF rp = 0 THEN IF POINT(a + cx, b + cy) > 0 THEN PSET (x + ax, y + bx + 1), spr(POINT(a + cx, b + cy) OR rpal)
               IF rp = 32 THEN IF POINT(x + ax, y + bx + 1) = 0 THEN PSET (x + ax, y + bx + 1), spr(POINT(a + cx, b + cy) OR rpal)
               ax = ax + 1
            NEXT cx
            bx = bx + 1: ax = 0
         NEXT cy
         ax = 0: bx = 0
      END IF
   NEXT c
END IF
IF asize = 32 THEN
   FOR c = 0 TO 252 STEP 4
      y = sprram(c)
      IF y < &HF0 THEN
      y = y + 15
         x = sprram(c + 3) + 15
         attr = sprram(c + 2)
         IF (attr AND 128) THEN
            sy = 7: sys = -1
         ELSE
            sy = 0: sys = 1
         END IF
         rp = (attr AND 32)
         rpal = (attr AND 3) * 4
         IF (attr AND 64) = 0 THEN
            a = okning + ((sprram(c + 1) AND 15) * 8)
            b = 300 + ((sprram(c + 1) AND 240) \ 2)
            FOR spr = 8 TO 0 STEP -8
            FOR cy = sy TO (sy XOR 7) STEP sys
               FOR cx = 0 TO 7
                  IF rp = 0 THEN IF POINT(a + cx, b + cy) > 0 THEN PSET (x + ax, y + bx + 1 + spr), (POINT(a + cx, b + cy) OR rpal)
                  IF rp = 32 THEN IF POINT(x + ax, y + bx + 1 + spr) = 0 THEN PSET (x + ax, y + bx + 1 + spr), (POINT(a + cx, b + cy) OR rpal)
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
SUB emulate
byte$ = " "
'ut = 1
'OPEN "debug.log" FOR BINARY AS #9
DO
byte% = anybyte(pc)
'Note! May want to place the most common ones first
SELECT CASE byte%
   CASE &HA9: pc = pc + 1: op.lda (pc): s = s + 2
   CASE &HA5: op.lda (getbyte): s = s + 3
   CASE &HB5: op.lda (zpx): s = s + 4
   CASE &HAD: op.lda (abso): s = s + 4
   CASE &HBD: op.lda (absx): s = s + 4
   CASE &HB9: op.lda (absy): s = s + 4
   CASE &HA1: op.lda (pre): s = s + 6
   CASE &HB1: op.lda (post): s = s + 5

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
  
   CASE &HE6: op.inc (getbyte): s = s + 5
   CASE &HF6: op.inc (zpx): s = s + 6
   CASE &HEE: op.inc (abso): s = s + 6
   CASE &HFE: op.inc (absx): s = s + 7

   CASE &HC6: op.dec (getbyte): s = s + 5
   CASE &HD6: op.dec (zpx): s = s + 6
   CASE &HCE: op.dec (abso): s = s + 6
   CASE &HDE: op.dec (absx): s = s + 7

   CASE &HE8: op.inx
   CASE &HC8: op.iny

   CASE &H85: op.sta (getbyte): s = s + 2
   CASE &H95: op.sta (zpx): s = s + 3
   CASE &H8D: op.sta (abso): s = s + 3
   CASE &H9D: op.sta (absx): s = s + 4
   CASE &H99: op.sta (absy): s = s + 5
   CASE &H81: op.sta (pre): s = s + 6
   CASE &H91: op.sta (post): s = s + 6

   CASE &H86: op.stx (getbyte): s = s + 3
   CASE &H96: op.stx (zpy): s = s + 4
   CASE &H8E: op.stx (abso): s = s + 4
  
   CASE &H84: op.sty (getbyte): s = s + 3
   CASE &H94: op.sty (zpx): s = s + 4
   CASE &H8C: op.sty (abso): s = s + 4

   CASE &HA2: pc = pc + 1: op.ldx (pc): s = s + 2
   CASE &HA6: op.ldx (getbyte): s = s + 3
   CASE &HB6: op.ldx (zpy): s = s + 4
   CASE &HAE: op.ldx (abso): s = s + 4
   CASE &HBE: op.ldx (absy): s = s + 4

   CASE &HA0: pc = pc + 1: op.ldy (pc): s = s + 2
   CASE &HA4: op.ldy (getbyte): s = s + 3
   CASE &HB4: op.ldy (zpx): s = s + 4
   CASE &HAC: op.ldy (abso): s = s + 4
   CASE &HBC: op.ldy (absx): s = s + 4
  
   CASE &HCA: op.dex
   CASE &H88: op.dey

   CASE &H48: op.pha
   CASE &H68: op.pla
   CASE &H8: op.php
   CASE &H28: op.plp

   CASE &H4A: reg.s = reg.s AND 254: reg.s = (reg.s OR (reg.a AND 1))
   reg.a = reg.a \ 2: reg.s = reg.s AND 125
   IF reg.a = 0 THEN reg.s = reg.s OR 2
   s = s + 2
   CASE &H46: op.lsr (getbyte): s = s + 5
   CASE &H56: op.lsr (zpx): s = s + 6
   CASE &H4E: op.lsr (abso): s = s + 6
   CASE &H5E: op.lsr (absx): s = s + 7

   CASE &HA: reg.s = reg.s AND 254: reg.a = reg.a * 2
      IF (reg.a AND 256) THEN reg.a = reg.a - 256: reg.s = reg.s OR 1
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2
      IF reg.a > 127 THEN reg.s = reg.s OR 128
      s = s + 2
   CASE &H6: op.asl (getbyte): s = s + 5
   CASE &H16: op.asl (zpx): s = s + 6
   CASE &HE: op.asl (abso): s = s + 6
   CASE &H1E: op.asl (absx): s = s + 7

   CASE &H29: pc = pc + 1: op.and (pc): s = s + 2
   CASE &H25: op.and (getbyte): s = s + 3
   CASE &H35: op.and (zpx): s = s + 4
   CASE &H2D: op.and (abso): s = s + 4
   CASE &H3D: op.and (absx): s = s + 4
   CASE &H39: op.and (absy): s = s + 4
   CASE &H21: op.and (pre): s = s + 6
   CASE &H31: op.and (post): s = s + 5

   CASE &H9: pc = pc + 1: op.ora (pc): s = s + 2
   CASE &H5: op.ora (getbyte): s = s + 3
   CASE &H15: op.ora (zpx): s = s + 4
   CASE &HD: op.ora (abso): s = s + 4
   CASE &H1D: op.ora (absx): s = s + 4
   CASE &H19: op.ora (absy): s = s + 4
   CASE &H1: op.ora (pre): s = s + 6
   CASE &H11: op.ora (post): s = s + 5

   CASE &H49: pc = pc + 1: op.eor (pc): s = s + 2
   CASE &H45: op.eor (getbyte): s = s + 3
   CASE &H55: op.eor (zpx): s = s + 4
   CASE &H4D: op.eor (abso): s = s + 4
   CASE &H5D: op.eor (absx): s = s + 4
   CASE &H59: op.eor (absy): s = s + 4

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
      s = s + 2
   CASE &H26: op.rol (getbyte): s = s + 5
   CASE &H36: op.rol (zpx): s = s + 6
   CASE &H2E: op.rol (abso): s = s + 6
   CASE &H3E: op.rol (absx): s = s + 7

   CASE &H6A
      reg.a = reg.a + (reg.s AND 1) * 256
      reg.s = reg.s AND 254
      reg.s = (reg.s OR (reg.a AND 1))
      reg.a = reg.a \ 2
      reg.s = reg.s AND 125
      IF reg.a = 0 THEN reg.s = reg.s OR 2
      IF reg.a > 127 THEN reg.s = reg.s OR 128
      s = s + 2
   CASE &H66: op.ror (getbyte): s = s + 5
   CASE &H76: op.ror (zpx): s = s + 6
   CASE &H6E: op.ror (abso): s = s + 6
   CASE &H7E: op.ror (absx): s = s + 7

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

   CASE &HC9: pc = pc + 1: op.cmp (pc): s = s + 2
   CASE &HC5: op.cmp (getbyte): s = s + 3
   CASE &HD5: op.cmp (zpx): s = s + 4
   CASE &HCD: op.cmp (abso): s = s + 4
   CASE &HDD: op.cmp (absx): s = s + 4
   CASE &HD9: op.cmp (absy): s = s + 4
   CASE &HC1: op.cmp (pre): s = s + 6
   CASE &HD1: op.cmp (post): s = s + 5

   CASE &HE0: pc = pc + 1: op.cpx (pc): s = s + 2
   CASE &HE4: op.cpx (getbyte): s = s + 3
   CASE &HEC: op.cpx (abso): s = s + 4
  
   CASE &HC0: pc = pc + 1: op.cpy (pc): s = s + 2
   CASE &HC4: op.cpy (getbyte): s = s + 3
   CASE &HCC: op.cpy (abso): s = s + 4
  
   CASE &H24: op.bit (getbyte): s = s + 3
   CASE &H2C: op.bit (abso): s = s + 4

   CASE &H69: pc = pc + 1: op.adc (pc): s = s + 2
   CASE &H65: op.adc (getbyte): s = s + 3
   CASE &H75: op.adc (zpx): s = s + 4
   CASE &H6D: op.adc (abso): s = s + 4
   CASE &H7D: op.adc (absx): s = s + 4
   CASE &H79: op.adc (absy): s = s + 4
   CASE &H61: op.adc (pre): s = s + 6
   CASE &H71: op.adc (post): s = s + 5

   CASE &HE9: pc = pc + 1: op.sbc (pc): s = s + 2
   CASE &HE5: op.sbc (getbyte): s = s + 3
   CASE &HF5: op.sbc (zpx): s = s + 4
   CASE &HED: op.sbc (abso): s = s + 4
   CASE &HFD: op.sbc (absx): s = s + 4
   CASE &HF9: op.sbc (absy): s = s + 4

   CASE &H0: op.brk
  
   CASE &H40: op.rti

   CASE &HEA: opcode$ = "NOP": s = s + 2
  
   CASE ELSE: opcode$ = "Unimplemented :(": ut = 1
END SELECT
IF s > 113.85 THEN
   s = s - 113.85
   lines = lines + 1
   IF (ppustat AND 64) = 0 THEN
      IF (ppu2 AND 16) THEN
         IF sprram(0) + 1 = lines THEN ppustat = ppustat OR 64
      END IF
   END IF
   IF lines = 224 THEN vb
   IF lines = 262 THEN vb: lines = 0
END IF


SELECT CASE INKEY$
   CASE "j": pc = 53332
   CASE "o", "i": ut = ut XOR 1
   CASE "p": palemu = palemu XOR 1: PALETTE
   CASE "n": cycles = 27007
   'CASE "r": CLS : patterntables
   CASE "d": crappygfxengine
   CASE "b": bpoint = breakpoint
   CASE "+": skip = skip + 1
   CASE "-": IF skip > 1 THEN skip = skip - 1
   CASE "z": joy1emu = joy1emu XOR 1: PSET (21, 284), POINT(21, 284) XOR 9
   CASE "x": joy1emu = joy1emu XOR 2: PSET (19, 284), POINT(19, 284) XOR 9
   CASE "c": joy1emu = joy1emu XOR 4: PSET (14, 284), POINT(14, 284) XOR 9
   CASE "v": joy1emu = joy1emu XOR 8: PSET (16, 284), POINT(16, 284) XOR 9
   CASE "s": save
   CASE CHR$(0) + "H": joy1emu = joy1emu XOR 16: PSET (10, 282), POINT(10, 282) XOR 9
   CASE CHR$(0) + "P": joy1emu = joy1emu XOR 32: PSET (10, 284), POINT(10, 284) XOR 9
   CASE CHR$(0) + "K": joy1emu = joy1emu XOR 64: PSET (9, 283), POINT(9, 283) XOR 9
   CASE CHR$(0) + "M": joy1emu = joy1emu XOR 128: PSET (11, 283), POINT(11, 283) XOR 9
   CASE "q": CLOSE : CLOSE : CLOSE : END
END SELECT

'DO WHILE INKEY$ = "": LOOP
'DO WHILE INKEY$ = "": LOOP
'DO WHILE INKEY$ = "": LOOP
pc = pc + 1
'patterntables
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
'o$ = CHR$(reg.sp)'SP-register
'PUT #9, , o$
'o$ = CHR$(reg.s)'Status-register
'PUT #9, , o$
'o$ = CHR$(byte%)'opcode
'PUT #9, , o$


IF pc = bpoint THEN ut = 1
IF ut = 1 THEN utskrift

LOOP

END SUB

FUNCTION filerequest$
' FileRequester 1.10 - New features : PageUp and PageDown can now be
' used.
' This is the NEW version.
DIM FileName$(255)
OPEN "uness.cfg" FOR INPUT AS #10
INPUT #10, path$
CLOSE #10
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
 CASE CHR$(0) + "Q": GOSUB PageDownFR
 CASE CHR$(0) + "I": GOSUB PageUpFR
 CASE "": LET EscPressed = 1: LET KeyBoardCommandsUpdated = 0: GOSUB DrawFileRequester: LET FileRequesterStatus = 1: filerequester$ = "_NoFile": GOTO EndThisSub
END SELECT
IF k$ > "" THEN
 IF ASC(k$) = 13 THEN LET EnterPressed = 1: LET KeyBoardCommandsUpdated = 0: LET FileRequesterStatus = 0: GOSUB DrawFileRequester: LET ChosenFile$ = path$ + FileName$(chc + page): filerequest$ = ChosenFile$: GOTO EndThisSub
END IF
LOOP
'
PageUpFR:
IF chc + page - 18 < 0 THEN RETURN
LET page = page - 18
GOSUB DrawFileRequester
RETURN
'
PageDownFR:
IF FileName$(chc + page + 18) = "" THEN RETURN
LET page = page + 18
GOSUB DrawFileRequester
RETURN
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
getbyte% = anybyte(pc)
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
'Read the header to check mapper and rom size
'Currently only mapper 0 is supported so mostly just need
'to check the number of prg banks

GET #1, 5, byte$
prgs = ASC(byte$)
GET #1, 7, byte$
mirroring = ASC(byte$) AND 1

IF prgs > 2 THEN
   SCREEN 0
   PRINT "Unsupported mapper, or not a NES-file."
   END
END IF

posi = 17
FOR x = 0 TO 16383
   GET #1, posi, byte$
   bank1(x) = ASC(byte$)                  'Read prg bank 1
   IF prgs = 1 THEN bank2(x) = ASC(byte$) 'Bank 2 is the same
   posi = posi + 1
NEXT x
IF prgs = 2 THEN                          'Bank 2 is different
   FOR x = 0 TO 16383
      GET #1, posi, byte$
      bank2(x) = ASC(byte$)
      posi = posi + 1
   NEXT x
END IF

FOR x = 0 TO 8191                         'Read pattern tables
   GET #1, posi, byte$
   vram(x) = ASC(byte$)
   posi = posi + 1
NEXT x
'Reset vector: FFFC
'Check reset position:
pc = bank2(16381)
pc = pc * 256 + bank2(16380)
nmi = bank2(16379)
nmi = nmi * 256 + bank2(16378)
brk = bank2(16383)
brk = brk * 256 + bank2(16382)
COLOR 13
hagla 306, 1, "mapper:"
hagla 297, 9, "mirroring:"
hagla 295, 17, "prg banks:"
hagla 295, 25, "chr banks:"
hagla 280, 33, "reset vector:"
hagla 292, 41, "nmi vector:"
hagla 290, 49, "brk vector:"
COLOR 11
hagla 340, 1, "$0"
IF mirroring = 0 THEN
   hagla 340, 9, "horizontal"
ELSE
   hagla 340, 9, "vertical"
END IF
hagla 340, 17, "$" + LCASE$(HEX$(prgs))
hagla 340, 25, "$1"
hagla 340, 33, "$" + LCASE$(HEX$(pc - 1))
hagla 340, 41, "$" + LCASE$(HEX$(nmi))
hagla 340, 49, "$" + LCASE$(HEX$(brk))

'Palette
FOR o = 0 TO 48 STEP 16
   pal(0 + o, 0) = 47
   pal(0 + o, 1) = 47
   pal(0 + o, 2) = 47
   pal(1 + o, 0) = 0
   pal(1 + o, 1) = 29
   pal(1 + o, 2) = 60
   pal(2 + o, 0) = 8
   pal(2 + o, 1) = 14
   pal(2 + o, 2) = 60
   pal(3 + o, 0) = 33
   pal(3 + o, 1) = 0
   pal(3 + o, 2) = 62
   pal(4 + o, 0) = 47
   pal(4 + o, 1) = 0
   pal(4 + o, 2) = 47
   pal(5 + o, 0) = 58
   pal(5 + o, 1) = 0
   pal(5 + o, 2) = 23
   pal(6 + o, 0) = 56
   pal(6 + o, 1) = 10
   pal(6 + o, 2) = 0
   pal(7 + o, 0) = 52
   pal(7 + o, 1) = 19
   pal(7 + o, 2) = 2
   pal(8 + o, 0) = 35
   pal(8 + o, 1) = 29
   pal(8 + o, 2) = 0
   pal(9 + o, 0) = 0
   pal(9 + o, 1) = 39
   pal(9 + o, 2) = 0
   pal(10 + o, 0) = 0
   pal(10 + o, 1) = 43
   pal(10 + o, 2) = 0
   pal(11 + o, 0) = 0
   pal(11 + o, 1) = 37
   pal(11 + o, 2) = 14
   pal(12 + o, 0) = 0
   pal(12 + o, 1) = 33
   pal(12 + o, 2) = 35
NEXT o
FOR b = 16 TO 48 STEP 16
   FOR o = 0 TO 15
      pal(b + o, 0) = pal(o, 0)
      pal(b + o, 1) = pal(o, 1)
      pal(b + o, 2) = pal(o, 2)
   NEXT o
NEXT b


END SUB

DEFSNG C, R
SUB loadfont
OPEN "FONT.DAT" FOR BINARY AS #7
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
SUB op.adc (typ AS LONG)
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
SUB op.and (typ AS LONG)
'AND: AND A with byte in memory
r = anybyte(typ)
reg.a = reg.a AND r
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
END SUB

SUB op.asl (typ AS LONG)
'ASL: Shift byte in memory left
reg.s = reg.s AND 254
b% = anybyte(typ)
b% = b% * 2
IF (b% AND 256) THEN b% = b% AND 255: reg.s = reg.s OR 1
putbyte b%, typ
reg.s = reg.s AND 125
IF b% = 0 THEN reg.s = reg.s OR 2
IF b% > 127 THEN reg.s = reg.s OR 128
END SUB

DEFINT A
SUB op.bcc
'BCC: Branch if c=0
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 1) = 0 THEN pc = pc + a
s = s + 2                             'TODO: Check page boundary
END SUB

SUB op.bcs
'BCS: Branch if c=1
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 1) THEN pc = pc + a
s = s + 2                             'TODO: Check page boundary
END SUB

SUB op.beq
'BEQ: Branch if Z=1
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 2) THEN pc = pc + a
s = s + 2                             'TODO: Check page boundary
END SUB

DEFINT B
DEFSNG A
SUB op.bit (typ AS LONG)
'BIT: AND byte in memory with A and transfer bit 7 and 8
'     to the status register(!?)
b = anybyte(typ)
reg.s = reg.s AND 61
reg.s = (reg.s OR (b AND 192))
a = (reg.a AND b)
IF a = 0 THEN reg.s = reg.s OR 2

END SUB

DEFINT A
DEFSNG B
SUB op.bmi
'BMI: Branch if S=1
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 128) THEN pc = pc + a
s = s + 2                                'TODO: Check page boundary
END SUB

SUB op.bne
'BNE: Branch if Z=0
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 2) = 0 THEN pc = pc + a
s = s + 2                                'TODO: Check page boundary
END SUB

SUB op.bpl
'BPL: Branch if S=0
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 128) = 0 THEN pc = pc + a
s = s + 2                                'TODO: Check page boundary
END SUB

DEFSNG A
SUB op.brk
'BRK: Just BRK... :(
IF (reg.s AND 4) = 0 THEN
   pc = pc + 2
   b% = (pc AND 65280) / 256
   a% = (pc AND 255)
   push b%
   push a%
   push reg.s OR 16             'Push status with B set
   reg.s = reg.s OR 4
   pc = brk - 1
END IF
END SUB

DEFINT A
SUB op.bvc
'BVC: Branch if V=0
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 64) = 0 THEN pc = pc + a
s = s + 2                                'TODO: Check page boundary
END SUB

SUB op.bvs
'BVS: Branch if V=1
a = getbyte
IF a > 127 THEN a = a - 256
IF (reg.s AND 64) THEN pc = pc + a
s = s + 2                                'TODO: Check page boundary
END SUB

DEFSNG A
SUB op.clc
'CLC: Clear carry flag
reg.s = reg.s AND 254
s = s + 2
END SUB

SUB op.cld
'CLD: Clear decimal flag
reg.s = reg.s AND 247
s = s + 2
END SUB

SUB op.cli
'CLI: Clear interrupt disable flag
reg.s = reg.s AND 251
s = s + 2
END SUB

SUB op.clv
'CLV: Clear overflow flag
reg.s = reg.s AND 191
s = s + 2
END SUB

SUB op.cmp (typ AS LONG)
'CMP Compare A with byte in memory
r = anybyte(typ)
reg.s = reg.s AND 124
IF r = reg.a THEN reg.s = reg.s OR 3
IF ((reg.a - r) AND 128) THEN reg.s = reg.s OR 128
IF r < reg.a THEN reg.s = reg.s OR 1
END SUB

SUB op.cpx (typ AS LONG)
'CPX: Compare X with byte in memory
r = anybyte(typ)
reg.s = reg.s AND 124
IF r = reg.x THEN reg.s = reg.s OR 3
IF ((reg.x - r) AND 128) THEN reg.s = reg.s OR 128
IF r < reg.x THEN reg.s = reg.s OR 1
END SUB

SUB op.cpy (typ AS LONG)
'CPY: Compare Y with byte in memory
r = anybyte(typ)
reg.s = reg.s AND 124
IF r = reg.y THEN reg.s = reg.s OR 3
IF ((reg.y - r) AND 128) THEN reg.s = reg.s OR 128
IF r < reg.y THEN reg.s = reg.s OR 1
END SUB

DEFINT B
SUB op.dec (typ AS LONG)
'DEC: Decrement byte in memory by 1
b = anybyte(typ)
b = b - 1
IF b = -1 THEN b = 255
reg.s = reg.s AND 125
IF b = 0 THEN reg.s = reg.s OR 2
IF b > 127 THEN reg.s = reg.s OR 128
putbyte b, typ
END SUB

DEFSNG B
SUB op.dex
'DEX: Decrement X by 1
reg.x = reg.x - 1
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x = -1 THEN reg.x = 255
IF reg.x > 127 THEN reg.s = reg.s OR 128
s = s + 2
END SUB

SUB op.dey
'DEY: Decrement Y by 1
reg.y = reg.y - 1
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2                 'Z?
IF reg.y = -1 THEN reg.y = 255
IF reg.y > 127 THEN reg.s = reg.s OR 128  'S?
s = s + 2
END SUB

DEFINT B
SUB op.eor (typ AS LONG)
'EOR: XOR A with byte in memory
b = anybyte(typ)
reg.a = reg.a XOR b
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2      'Z?
IF reg.a > 127 THEN reg.s = reg.s OR 128  'S?
END SUB

SUB op.inc (typ AS LONG)
'INC: Increment byte in memory by 1
b = anybyte(typ)
b = b + 1
reg.s = reg.s AND 125
IF b > 255 THEN b = 0: reg.s = reg.s OR 2
IF b > 127 THEN reg.s = reg.s OR 128
putbyte b, typ
END SUB

DEFSNG B
SUB op.inx
'INX: Increment X by 1
reg.x = reg.x + 1
IF reg.x = 256 THEN reg.x = 0    'Overflow?

reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128

s = s + 2
END SUB

SUB op.iny
'INY: Increment Y by 1
reg.y = reg.y + 1
IF reg.y = 256 THEN reg.y = 0
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg.s = reg.s OR 128
s = s + 2
END SUB

SUB op.jmp (typ AS LONG)
'JMP: Jump to a new position
'Note that pc is incremented by 1 after each instruction
SELECT CASE typ
   CASE 1                          'Direct. Two operand bytes.
      pc = abso - 1
      s = s + 3
   CASE 2                          'Indirect. Two operand bytes.
      b& = abso
      a = anybyte(b&)
      b = anybyte(b& + 1)
      pc = a + b * 256 - 1
      s = s + 5
END SELECT
END SUB

SUB op.jsr
'JSR: Push pc and jump
a = getbyte
b = getbyte
push ((pc AND 65280) / 256)
push (pc AND 255)
pc = a + b * 256 - 1
s = s + 6
END SUB

SUB op.lda (typ AS LONG)
'LDA: Load A
reg.a = anybyte(typ)
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2  'Z?
IF reg.a > 127 THEN reg.s = reg.s OR 128'S?
END SUB

SUB op.ldx (typ AS LONG)
'LDX: Load X
reg.x = anybyte(typ)
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
END SUB

SUB op.ldy (typ AS LONG)
'LDY: Load Y like crazy
reg.y = anybyte(typ)
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg.s = reg.s OR 128
END SUB

SUB op.lsr (typ AS LONG)
'LSR: Shift byte in memory right
reg.s = reg.s AND 254
b% = anybyte(typ)
reg.s = (reg.s OR (b% AND 1)) 'Fix carry.
b% = b% \ 2
putbyte b%, typ
reg.s = reg.s AND 125
IF b% = 0 THEN reg.s = reg.s OR 2
END SUB

DEFINT B
SUB op.ora (typ AS LONG)
'ORA: OR A with byte in memory
b = anybyte(typ)
reg.a = reg.a OR b
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
END SUB

DEFSNG B
SUB op.pha
'PHA: Push A
push reg.a
s = s + 3
END SUB

SUB op.php
'PHP: Push status register(!)
push reg.s
s = s + 3
END SUB

SUB op.pla
'PLA: Pull A
reg.a = pop
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
'Note! The byte that was read will remain on the stack.
'Which is of course perfectly normal and expected. :)
s = s + 4
END SUB

SUB op.plp
'PLP: Pull status register
reg.s = pop
s = s + 4
END SUB

DEFINT B
SUB op.rol (typ AS LONG)
'ROL: Rotate byte in memory left
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
SUB op.ror (typ AS LONG)
'ROR: Rotate byte in memory right
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
'RTI: Return from interrupt
reg.s = pop AND 239
a = pop
b = pop
pc = a + b * 256
END SUB

SUB op.rts
'RTS: Pull pc
a = pop              'First the low byte
b = pop              'Then the high one :)
pc = a + b * 256
s = s + 6
END SUB

DEFINT A
SUB op.sbc (typ AS LONG)
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
reg.a = reg.a - a - cc
IF reg.a >= 0 THEN
   reg.s = reg.s OR 1
ELSE
   reg.a = reg.a + 256
END IF
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
END SUB

DEFSNG A
SUB op.sec
'SEC: Set carry flag
reg.s = reg.s OR 1
s = s + 2
END SUB

SUB op.sed
'SED: Set decimal mode flag
reg.s = reg.s OR 8
s = s + 2
END SUB

SUB op.sei
'SEI: Set interupt disable flag
reg.s = reg.s OR 4
s = s + 2
END SUB

SUB op.sta (typ AS LONG)
'STA: Store A
putbyte reg.a, typ
END SUB

SUB op.stx (typ AS LONG)
'STX: Store X
putbyte reg.x, typ
END SUB

SUB op.sty (typ AS LONG)
'STY: Store Y
putbyte reg.y, typ
END SUB

SUB op.tax
'TAX: Transfer A to X
reg.x = reg.a
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
s = s + 2
END SUB

SUB op.tay
'TAY: Transfer A to Y
reg.y = reg.a
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg.s = reg.s OR 128
s = s + 2
END SUB

DEFSNG C, R
SUB op.tsx
'TSX: Transfer stack pointer to X
reg.x = reg.sp
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
s = s + 2
END SUB

DEFINT C, R
SUB op.txa
'TXA: Transfer X to A
reg.a = reg.x
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
s = s + 2
END SUB

SUB op.txs
'TXS: Transfer X to stack pointer
reg.sp = reg.x
s = s + 2
END SUB

SUB op.tya
'TYA: Transfer Y to A
reg.a = reg.y
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
s = s + 2
END SUB

SUB patterntables
DIM linje(7)
'Draw pattern tables from VRAM
a = 0
FOR table = 0 TO 1
   FOR ty = 0 TO 120 STEP 8
      FOR tx = 0 TO 120 STEP 8
         FOR y = 0 TO 7
            FOR planes = 0 TO 1
               b% = vram(a + planes * 8)
               IF b% AND 128 THEN linje(0) = linje(0) + planes + 1
               IF b% AND 64 THEN linje(1) = linje(1) + planes + 1
               IF b% AND 32 THEN linje(2) = linje(2) + planes + 1
               IF b% AND 16 THEN linje(3) = linje(3) + planes + 1
               IF b% AND 8 THEN linje(4) = linje(4) + planes + 1
               IF b% AND 4 THEN linje(5) = linje(5) + planes + 1
               IF b% AND 2 THEN linje(6) = linje(6) + planes + 1
               IF b% AND 1 THEN linje(7) = linje(7) + planes + 1
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
pop% = ram(256 + reg.sp)
END FUNCTION

FUNCTION post&
c = getbyte
a = anybyte(INT(c))
b = anybyte(c + 1)
post = (a + (b * 256) + reg.y) AND 65535
END FUNCTION

FUNCTION pre&
c = getbyte
c = c + reg.x
IF (c AND 256) THEN c = c AND 255
a = anybyte(INT(c))
b = anybyte(c + 1)
pre = a + b * 256
END FUNCTION

SUB push (r AS INTEGER)
ram(256 + reg.sp) = r
reg.sp = reg.sp - 1
IF reg.sp < 0 THEN reg.sp = 255
END SUB

SUB putbyte (byte%, posi AS LONG)
IF posi >= &H2000 THEN
   IF posi <= &H2007 THEN
      IF posi = &H2000 THEN ppu1 = byte%
      IF posi = &H2001 THEN ppu2 = byte%
      IF posi = &H2003 THEN sprramposi = byte%
      IF posi = &H2004 THEN
         sprram(sprramposi) = byte%
         sprramposi = sprramposi + 1
      END IF
      IF posi = &H2005 THEN
         IF scrollwrite = 1 THEN
            vscroll = byte%
            scrollwrite = 0
         ELSE
            hscroll = byte%
            scrollwrite = 1
         END IF
      END IF
      IF posi = &H2006 THEN        'Change address for VRAM read/write
         vramposi = (vramposi AND 63) * 256 + byte%
         firstread = 1
      END IF
      IF posi = &H2007 THEN        'Write to VRAM, increment "pointer"
         okn = 1
         IF (ppu1 AND 4) THEN okn = 32
         vram(vramposi) = byte%
         vramposi = vramposi + okn
         IF vramposi > 32767 THEN vramposi = vramposi - 32768

                        
      END IF
      EXIT SUB
   END IF
END IF

IF posi >= &H4000 THEN
IF posi <= &H4017 THEN

   IF posi = &H4014 THEN
      c = byte% * 256
      FOR x = 0 TO 255
         sprram(x) = ram(c + x)
      NEXT x
      s = s + 513
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
ram(posi) = byte%
'PUT #2, posi + &H800, byte$
'PUT #2, posi + &H1000, byte$
'PUT #2, posi + &H1800, byte$

END SUB

SUB save
OPEN LEFT$(fil$, LEN(fil$) - 4) + ".ust" FOR BINARY AS #14
o$ = CHR$(reg.a)
o$ = o$ + CHR$(reg.x)
o$ = o$ + CHR$(reg.y)
o$ = o$ + CHR$(reg.s)
o$ = o$ + CHR$(reg.sp)
o$ = o$ + CHR$(pc \ 256)
o$ = o$ + CHR$(pc AND 255)
PUT #14, , o$
CLOSE #14

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

SUB vb
IF vblank = 0 THEN
   'ut = 1
   ppustat = ppustat OR 128
   vblank = 1
   'ut = 1
   IF (ppu1 AND 128) THEN
      
      s = s + 7
      'Push pc
      b% = (pc AND 65280) / 256
      a% = (pc AND 255)
      push b%
      push a%
      push reg.s        'Push status
      reg.s = reg.s OR 4
      'opcode$ = "vblank: jumping to " + HEX$(nmi)
      pc = nmi - 1
   END IF
   frame = frame + 1
   IF frame = skip THEN frame = 0: crappygfxengine 'Draw graphics...
ELSE
   s = s + 7
   ppustat = ppustat AND 63
   vblank = 0
   'ut = 0
END IF
END SUB

FUNCTION zp&
a = getbyte
zp = a
END FUNCTION

DEFINT A
FUNCTION zpx&
a = getbyte
a = a + reg.x
IF (a AND 256) THEN a = a AND 255
zpx = a
END FUNCTION

FUNCTION zpy&
a = getbyte
a = a + reg.y
IF (a AND 256) THEN a = a AND 255
zpy = a
END FUNCTION

