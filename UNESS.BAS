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
DECLARE SUB emulera ()
DEFINT C, R
SCREEN 12
'Status: SV1BDIZC
' Alla register m†ste vara share-ade...
loadfont       'Ladda in fonten...
DIM SHARED reg.a, reg.x, reg.y, reg.s, pc, reg.sp, fil$, byte$, opcode$
DIM SHARED cycles, nmi, ut, bokstav(52, 5), bytes, rabies, vramposi, vblank
DIM SHARED ppu1, brk, sprramposi, sprram$, ppustat
sprram$ = STRING$(256, CHR$(0))
'uNESsential Nintendo-emulator. Version 0.110
'
'Nyheter:
' Zero Page-ANDarna. Lode Runner-titelbilden funkar.
' De tv† sista DECarna.
' 1 SBC.
' BIT lite fixad. Var helt trasig.
' Diverse sm†saker.
' VRAM-pekaren ”kas „ven vid l„sning.
' 8x8-sprites.
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
COLOR 9
INPUT "Load ROM:", fil$


'Rita lite saker...
CLS
'LINE (0, 0)-(256, 224), 8, B

'ppu1 = 128                         'Nmi ska vara p†.
'F”rberedelser...
OPEN fil$ FOR BINARY AS #1         'Rommen har filnummer #1.
OPEN "MEMORY.NIN" FOR BINARY AS #2 'Minnet har filnummer #2. OBS Byte 1=0
OPEN "VRAM.NIN" FOR BINARY AS #4   'VRAM har filnummer #4.
reg.s = 32                         'Status-bit 6=1
pc = 17                            'Skippa 16-bytes-headern...
reg.sp = 255                       'Stack-pekaren m†ste vara 255.
header
LINE (459, 407)-(639, 407), 8, , 21845
patterntables
emulera                            'B”rja emulera!

FUNCTION anybyte% (posi)
GET #2, posi, byte$
anybyte = ASC(byte$)

IF posi > &H2000 THEN
IF posi < &H2009 THEN

IF posi = &H2003 THEN anybyte% = ppustat
IF posi = &H2005 THEN anybyte% = ASC(MID$(sprram$, sprramposi + 1))
IF posi = &H2008 THEN
   okn = 1
   IF (ppu1 AND 4) THEN okn = 32
   GET #4, vramposi + 1, anybyte%
   vramposi = vramposi + okn
END IF

END IF
END IF
END FUNCTION

SUB crappygfxengine 'F”rmodligen den s„msta grafikmotor som n†gonsin skapats...
'Bakgrund.
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
   LOCATE 1, 40: PRINT ASC(MID$(bytes$, c, 1))
   a = okning + ((ASC(MID$(bytes$, c, 1)) AND 15) * 8)
   b = 300 + ((ASC(MID$(bytes$, c, 1)) AND 240) / 2)
   GET (a, b)-(a + 7, b + 7), tile
   'PSET (a, b), 12
   
   PUT (x, y), tile, PSET
   x = x + 8
   IF x = 256 THEN y = y + 8: x = 0
NEXT c
'Sprites.
FOR c = 1 TO 256 STEP 4
   y = ASC(MID$(sprram$, c))
   x = ASC(MID$(sprram$, c + 3))
   a = ((ASC(MID$(sprram$, c + 1, 1)) AND 15) * 8)
   b = 300 + ((ASC(MID$(sprram$, c + 1, 1)) AND 240) / 2)
   FOR cy = 0 TO 7
      FOR cx = 0 TO 7
         IF POINT(a + cx, b + cy) > 0 THEN PSET (x + cx, y + cy + 1), POINT(a + cx, b + cy)
      NEXT cx
   NEXT cy
NEXT c
END SUB

SUB cycle (r)
cycles = cycles + r
IF vblank = 1 THEN
IF cycles >= 2599 THEN
   BEEP: BEEP: BEEP: BEEP: BEEP: BEEP
   cycles = 7
   a% = anybyte(&H2003)             'fixa VBlank-biten...
   a% = a% AND 127
   putbyte a%, &H2003
   vblank = 0
   'ut = 0
END IF
END IF
IF cycles >= 27007 THEN
   cycles = 0
   a% = anybyte(&H2003)
   a% = a% OR 128
   putbyte a%, &H2003
   vblank = 1
   IF (ppu1 AND 128) = 128 THEN
      BEEP: BEEP: BEEP: BEEP: BEEP: BEEP
      cycles = 7
      'ut = 1
      'L„gg pc p† stacken.
      b% = (pc AND 65280) / 256
      a% = (pc AND 255)
      push b%
      push a%
      push reg.s        'L„gg status p† stacken...
      'opcode$ = "vblank: hoppar till " + HEX$(nmi)
      pc = nmi
   END IF
   crappygfxengine    'Vederv„rdig grafikmotor som borde f”rbjudas!
END IF
END SUB

SUB emulera
byte$ = " "
'ut = 1
DO
GET #2, pc, byte$
bytes = ASC(byte$)
'En miljard case-ar!
'OBS! B”r sorteras i ordningen: vanligast f”rst - ovanligast sist...

SELECT CASE ASC(byte$)
   CASE &HAA: op.tax
   CASE &H8A: op.txa
   CASE &HA8: op.tay
   CASE &H98: op.tya
   CASE &H9A: op.txs
   CASE &HBA: op.tsx
  
   CASE &HE6: op.inc (1)
   CASE &HF6: op.inc (2)
   CASE &HEE: op.inc (3)
   CASE &HFE: op.inc (4)

   CASE &HC6: op.dec (1)
   CASE &HD6: op.dec (2)
   CASE &HCE: op.dec (3)
   CASE &HDE: op.dec (4)

   CASE &HE8: op.inx
   CASE &HC8: op.iny

   CASE &H85: op.sta (1)
   CASE &H95: op.sta (2)
   CASE &H8D: op.sta (3)
   CASE &H9D: op.sta (4)
   CASE &H99: op.sta (5)
   CASE &H81: op.sta (6)
   CASE &H91: op.sta (7)

   CASE &H86: op.stx (1)
   CASE &H96: op.stx (2)
   CASE &H8E: op.stx (3)
  
   CASE &H84: op.sty (1)
   CASE &H94: op.sty (2)
   CASE &H8C: op.sty (3)
  
   CASE &HA9: op.lda (1)
   CASE &HA5: op.lda (2)
   CASE &HB5: op.lda (3)
   CASE &HAD: op.lda (4)
   CASE &HBD: op.lda (5)
   CASE &HB9: op.lda (6)
   CASE &HA1: op.lda (7)
   CASE &HB1: op.lda (8)

   CASE &HA2: op.ldx (1)
   CASE &HA6: op.ldx (2)
   CASE &HB6: op.ldx (3)
   CASE &HAE: op.ldx (4)
   CASE &HBE: op.ldx (5)

   CASE &HA0: op.ldy (1)
   CASE &HA4: op.ldy (2)
   CASE &HB4: op.ldy (3)
   CASE &HAC: op.ldy (4)
   CASE &HBC: op.ldy (5)
  
   CASE &HCA: op.dex
   CASE &H88: op.dey

   CASE &H48: op.pha
   CASE &H68: op.pla
   CASE &H8: op.php
   CASE &H28: op.plp

   CASE &H4A: op.lsr (1)
   CASE &H46: op.lsr (2)
  
   CASE &HA: op.asl (1)
   CASE &H6: op.asl (2)
   CASE &H16: op.asl (3)

   CASE &H29: op.and (1)
   CASE &H25: op.and (2)
   CASE &H35: op.and (3)

   CASE &H9: op.ora (1)
   CASE &H5: op.ora (2)
   CASE &H15: op.ora (3)
   CASE &HD: op.ora (4)
   CASE &H1D: op.ora (5)
   CASE &H19: op.ora (6)

   CASE &H49: op.eor (1)
   CASE &H45: op.eor (2)
   CASE &H55: op.eor (3)
  
   CASE &H2A: op.rol (1)
   CASE &H26: op.rol (2)
   CASE &H36: op.rol (3)

   CASE &H6A: op.ror (1)
   CASE &H66: op.ror (2)
   CASE &H76: op.ror (3)
   CASE &H6E: op.ror (4)
   CASE &H7E: op.ror (5)

   CASE &H38: op.sec
   CASE &HF8: op.sed
   CASE &H78: op.sei
   
   CASE &H18: op.clc
   CASE &HD8: op.cld
   CASE &H58: op.cli
   CASE &HB8: op.clv

   CASE &HF0: op.beq
   CASE &HD0: op.bne
   CASE &H10: op.bpl
   CASE &H30: op.bmi
   CASE &HB0: op.bcs
   CASE &H90: op.bcc

   CASE &H4C: op.jmp (1)
   CASE &H6C: op.jmp (2)
   CASE &H20: op.jsr

   CASE &H60: op.rts

   CASE &HC9: op.cmp (1)
   CASE &HC5: op.cmp (2)
   CASE &HD5: op.cmp (3)
   CASE &HCD: op.cmp (4)
   CASE &HDD: op.cmp (5)

   CASE &HE0: op.cpx (1)
   CASE &HE4: op.cpx (2)
   CASE &HEC: op.cpx (3)
  
   CASE &HC0: op.cpy (1)
   CASE &HC4: op.cpy (2)
   CASE &HCC: op.cpy (3)
  
   CASE &H24: op.bit (1)
   CASE &H2C: op.bit (2)

   CASE &H69: op.adc (1)
   CASE &H65: op.adc (2)
   CASE &H75: op.adc (3)

   CASE &HE9: op.sbc (1)

   CASE &H0: op.brk
  
   CASE &H40: op.rti

   CASE &HEA: cycle 2
  
   CASE ELSE: opcode$ = "Oimplementerad :(": PRINT HEX$(ASC(byte$)): PRINT HEX$(pc - 1): SLEEP
END SELECT
SELECT CASE INKEY$
   CASE "u": ut = 1
   CASE "a": ut = 0
   CASE "q": END
END SELECT
IF ut = 1 THEN utskrift
'PRINT "Cycles:"; cycles
pc = pc + 1


LOOP

END SUB

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
   PRINT "Inte mapper #0-rom!"
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
PRINT "Resettat till "; HEX$(pc - 1); " /"; pc - 1; "(+1)..."
GET #2, 65531, byte$
a = ASC(byte$)
GET #2, 65532, byte$
b = ASC(byte$)
a = a + b * 256
nmi = a
PRINT "NMI-Position:"; HEX$(a); " /"; a; "(+1)..."
GET #2, 65535, byte$
a = ASC(byte$)
GET #2, 65536, byte$
b = ASC(byte$)
a = a + b * 256
brk = a
PRINT "BRK-Position:"; HEX$(a); " /"; a; "(+1)..."

byte$ = " "
END SUB

DEFSNG C, R
SUB loadfont
OPEN "°±²ÛÛ²±°.ÜÛß" FOR BINARY AS #7
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

SUB op.adc (typ)
'ADC: L„gg till operanden (minnes-byten) och C-flagen till Ackumulatorn
SELECT CASE typ
   CASE 1               'Direkt. En operand.
      a = getbyte + (reg.s AND 1)
      opcode$ = "ADC #$" + HEX$(a)
      cy = 2
   CASE 2 TO 3
      a = getbyte
      opcode$ = "ADC $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.x
         IF a > 255 THEN a = a - 256
         opcode$ = opcode$ + ", X"
      END IF
      a = anybyte(a + 1)
      cy = 2
END SELECT
IF (a AND 128) THEN a = a - 256
reg.a = reg.a + a
reg.s = reg.s AND 60
IF (reg.a > 255) OR (reg.a) < 0 THEN reg.s = reg.s OR 64    'V?
IF reg.a > 255 THEN reg.a = reg.a - 256: reg.s = reg.s OR 1 'C?
IF reg.a = 0 THEN reg.s = reg.s OR 2                        'Z?
IF reg.a < 0 THEN reg.a = reg.a + 256: reg.s = reg.s OR 128 'S?
cycle cy
END SUB

SUB op.and (typ)
'AND: ANDa Ackumulatorn med minne/operand.
SELECT CASE typ
   CASE 1
      r = getbyte
      opcode$ = "AND #$" + HEX$(r)
      cy = 2
   CASE 2 TO 3
      a = getbyte
      opcode$ = "AND $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.x
         IF a > 255 THEN a = a - 256
         opcode$ = opcode$ + ", X"
      END IF
      cy = typ + 1
END SELECT
IF typ > 1 THEN r = anybyte(a + 1)
reg.a = reg.a AND r
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
cycle cy
END SUB

SUB op.asl (typ)
'ASL: Shift Left...

SELECT CASE typ
   CASE 1
      reg.a = reg.a * 2
      IF reg.a > 255 THEN reg.a = reg.a - 256: reg.s = reg.s OR 1
      b% = reg.a
      'Om utanf”r: S„tt c-flagen...
      opcode$ = "ASL"
      cycle 2
   CASE 2 TO 3                   'Zero Page (X).
      a = getbyte
      IF typ = 3 THEN
         a = a + reg.x
         IF a > 255 THEN a = a - 256
      END IF
      b% = anybyte(a + 1)
      b% = b% * 2
      IF b% > 255 THEN b% = b% - 256: reg.s = reg.s OR 1
      putbyte b%, a + 1
      opcode$ = "ASL $" + HEX$(a)
      IF typ = 3 THEN opcode$ = "ASL $" + HEX$(a - reg.x) + ", X"
      cycle 5
END SELECT
reg.s = reg.s AND 125
IF b% = 0 THEN reg.s = reg.s OR 2
IF b% > 127 THEN reg.s = reg.s OR 128
END SUB

SUB op.bcc
'BCC: Brancha om c=0...
a = getbyte
opcode$ = "BCC $" + HEX$(a)
IF a > 127 THEN a = a - 256
IF (reg.s AND 1) = 0 THEN pc = pc + a
cycle 3                                'OBS! Konstig.



END SUB

SUB op.bcs
'BCS: Brancha om c=1
a = getbyte
opcode$ = "BCS $" + HEX$(a)
IF a > 127 THEN a = a - 256
IF (reg.s AND 1) = 1 THEN pc = pc + a
cycle 3                                'OBS! Konstig.
END SUB

SUB op.beq
'BEQ: Brancha om Z=1
a = getbyte
opcode$ = "BEQ $" + HEX$(a)
IF a > 127 THEN a = a - 256
IF (reg.s AND 2) = 2 THEN pc = pc + a
cycle 3                                'OBS! Konstig.
END SUB

SUB op.bit (typ)
'BIT: ANDa minne/operand med ackumulatorn och f”r ”ver bit 7 och 8
'     till status-registret(!?)
SELECT CASE typ
   CASE 1
      a = getbyte
      b = anybyte(a + 1)
      cy = 3
   CASE 2
      a = getbyte
      b = getbyte
      a = a + b * 256
      b = anybyte(a + 1)
      cy = 4
END SELECT
a = (reg.a AND b)
reg.s = (reg.s OR (b AND 192))
reg.s = reg.s AND 253
IF a = 0 THEN reg.s = reg.s OR 2

END SUB

SUB op.bmi
'BMI: Brancha om s=1
a = getbyte
opcode$ = "BMI $" + HEX$(a)
IF a > 127 THEN a = a - 256
IF (reg.s OR 128) = 128 THEN pc = pc + a
cycle 3                                   'OBS!
END SUB

SUB op.bne
'BNE: Brancha om Z=0
a = getbyte
opcode$ = "BNE $" + HEX$(a)
IF a > 127 THEN a = a - 256
IF (reg.s AND 2) = 0 THEN pc = pc + a
cycle 3                                'OBS!
END SUB

SUB op.bpl
'BPL: Brancha om s=0
a = getbyte
opcode$ = "BPL $" + HEX$(a)
IF a > 127 THEN a = a - 256
IF (reg.s AND 128) = 0 THEN pc = pc + a
cycle 3                                   'OBS!
END SUB

SUB op.brk
'BRK: Brk helt enkelt... :(
   IF (reg.s AND 4) = 0 THEN
   pc = pc + 1
   b% = (pc AND 65280) / 256
   a% = (pc AND 255)
   push b%
   push a%
   push reg.s        'L„gg status p† stacken...
   pc = brk
   END IF
   opcode$ = "BRK"
END SUB

SUB op.clc
'CLC: Rensa carry-flagen...
reg.s = reg.s AND 254
opcode$ = "CLC"
cycle 2
END SUB

SUB op.cld
'CLD: Rensa Decimal-flagen.
reg.s = reg.s AND 247
opcode$ = "CLD"
cycle 2
END SUB

SUB op.cli
'CLI: Rensa Interrupt disable-flagen.
reg.s = reg.s AND 251
opcode$ = "CLI"
cycle 2
END SUB

SUB op.clv
'CLV: Rensa Owerflow-flagen.
reg.s = reg.s AND 191
opcode$ = "CLV"
cycle 2
END SUB

SUB op.cmp (typ)
'CMP J„mf”r operand och reg.a. (reg.a-operand)...
SELECT CASE typ
   CASE 1
      r = getbyte
      opcode$ = "CMP #$" + HEX$(r)
      cy = 2
   CASE 2 TO 3
      a = getbyte
      opcode$ = "CMP $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      r = anybyte(a + 1)
      cy = 1 + typ
   CASE 4 TO 5
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "CMP $" + HEX$(a)
      IF typ = 5 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 65535 THEN a = a - 65536
      END IF
      r = anybyte(a + 1)
      cy = 4                        '5:ans „r konstig...
END SELECT
IF r = reg.a THEN reg.s = reg.s OR 3: cycle cy: EXIT SUB
IF reg.a - r < 0 THEN
   reg.s = reg.s AND 254
   IF 255 + (reg.a - r) > 127 THEN
      reg.s = reg.s OR 128
   ELSE
      reg.s = reg.s AND 127
   END IF
END IF
cycle cy
END SUB

SUB op.cpx (typ)
'CPX: J„mf”r X-registret och minne/operand.
SELECT CASE typ
   CASE 1
      r = getbyte
      opcode$ = "CPX #$" + HEX$(r)
      cy = 2
   CASE 2
      a = getbyte
      opcode$ = "CPX $" + HEX$(a)
      r = anybyte(a + 1)
      cy = 3
   CASE 3
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "CPX $" + HEX$(a)
      r = anybyte(a + 1)
      cy = 4
END SELECT
IF r = reg.x THEN reg.s = reg.s OR 3: EXIT SUB
IF reg.x - r < 0 THEN
   reg.s = reg.s AND 254
   IF 255 + (reg.x - r) > 127 THEN
      reg.s = reg.s OR 128
   ELSE
      reg.s = reg.s AND 127
   END IF
END IF
cycle cy

END SUB

SUB op.cpy (typ)
'CPY: J„mf”r Y-registret och minne/operand.
SELECT CASE typ
   CASE 1
      r = getbyte
      opcode$ = "CPY #$" + HEX$(r)
      cy = 2
   CASE 2
      a = getbyte
      opcode$ = "CPY $" + HEX$(a)
      r = anybyte(a + 1)
      cy = 3
   CASE 3
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "CPY $" + HEX$(a)
      r = anybyte(a + 1)
      cy = 4
END SELECT
IF r = reg.y THEN reg.s = reg.s OR 3: EXIT SUB
IF reg.y - r < 0 THEN
   reg.s = reg.s AND 254
   IF 255 + (reg.y - r) > 127 THEN
      reg.s = reg.s OR 128
   ELSE
      reg.s = reg.s AND 127
   END IF
END IF
cycle cy
END SUB

SUB op.dec (typ)
'DEC: Minska byte i minnet med 1.
SELECT CASE typ
   CASE 1 TO 2
      a = getbyte
      opcode$ = "DEC $" + HEX$(a)
      IF typ = 2 THEN
         a = a + reg.x
         IF a > 255 THEN a = a - 256
         opcode$ = opcode$ + ", X"
      END IF
      cy = 4 + typ
   CASE 3 TO 4
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "DEC $" + HEX$(a)
      IF typ = 2 THEN
         a = a + reg.x
         IF a > 65535 THEN a = a - 65536
         opcode$ = opcode$ + ", X"
      END IF
      cy = typ + 3
END SELECT
b = anybyte(a + 1)
b = b - 1
IF b = -1 THEN b = 255
reg.s = reg.s AND 125
IF b = 0 THEN reg.s = reg.s OR 2
IF b > 127 THEN reg.s = reg.s OR 128
putbyte INT(b), a + 1
cycle cy
END SUB

SUB op.dex
'DEX: X-registret minskas med 1.
reg.x = reg.x - 1
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x = -1 THEN reg.x = 255
IF reg.x > 127 THEN reg.s = reg.s OR 128
opcode$ = "DEX"
cycle 2
END SUB

SUB op.dey
'DEY: Y-registret minskas med 1.
reg.y = reg.y - 1
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2                 'Z?
IF reg.y = -1 THEN reg.y = 255
IF reg.y > 127 THEN reg.s = reg.s OR 128'S?
opcode$ = "DEY"
cycle 2
END SUB

SUB op.eor (typ)
'EOR: XORa minne/operand med ackumulatorn.
SELECT CASE typ
   CASE 1
      a = getbyte
      reg.a = reg.a XOR a
      opcode$ = "EOR #$" + HEX$(a)
      cycle 2
   CASE 2 TO 3
      a = getbyte
      IF typ = 3 THEN
         a = a + reg.x
         IF a > 255 THEN a = a - 256
      END IF
      b = anybyte(a + 1)
      reg.a = reg.a XOR b
      opcode$ = "EOR $" + HEX$(a)
      IF typ = 3 THEN opcode$ = "EOR $" + HEX$(b - reg.x) + ", X"
END SELECT
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2      'Z?
IF reg.a > 127 THEN reg.s = reg.s OR 128  'S?
END SUB

SUB op.inc (typ)
'INC: ™ka byte i minnet med 1.
SELECT CASE typ
   CASE 1 TO 2
      a = getbyte
      opcode$ = "INC $" + HEX$(a)
      IF typ = 2 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      cy = 4 + typ
   CASE 3 TO 4
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "INC $" + HEX$(a)
      IF typ = 4 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 65535 THEN a = a - 65536
      END IF
      cy = 3 + typ
END SELECT
b = anybyte(a + 1)
b = b + 1
reg.s = reg.s AND 125
IF b > 255 THEN b = 0: reg.s = reg.s OR 2
IF b > 127 THEN reg.s = reg.s OR 128
putbyte INT(b), a + 1
cycle cy
END SUB

SUB op.inx
'INX: X-registret ”kas med 1.
reg.x = reg.x + 1
IF reg.x = 256 THEN reg.x = 0    'Kanske overflow...

reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128

opcode$ = "INX"
cycle 2
END SUB

SUB op.iny
'INY: Y-registret ”kas med 1.
reg.y = reg.y + 1
IF reg.y = 256 THEN reg.y = 0

reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg. = reg.s OR 128

opcode$ = "INY"
cycle 2
END SUB

SUB op.jmp (typ)
'JMP: Hoppa till ny position...
'Det h„r ser konstigt ut i och med att 1 inte l„ggs till p† adressen.
'Det beror p† att pc ”kas med ett efter varje opcode „nd†... :)
SELECT CASE typ
   CASE 1                          'Direkt. Tv† operander.
      b = getbyte
      a = getbyte
      pc = b + a * 256
      opcode$ = "JMP $" + HEX$(pc)
      cycle 3
CASE 2                             'Indirekt. Tv† operander.
      b = getbyte
      a = gtebyte
      b = b + a * 256
      a = anybyte(b + 1)
      b = anybyte(b + 2)
      pc = a + b * 256
      opcode$ = "JMP ($" + HEX$(pc) + ")"
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
opcode$ = "JSR $" + HEX$(pc)
cycle 6
END SUB

SUB op.lda (typ)
'LDA: Ladda Ackumulatorn med [operand(er)]

SELECT CASE typ
   CASE 1                          'Direkt. En operand.
      reg.a = getbyte
      opcode$ = "LDA #$" + HEX$(reg.a)
      c = 2
   CASE 2 TO 3                     'Zero Page (x). En operand.
      a = getbyte
      opcode$ = "LDA $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      c = typ + 1
   CASE 4 TO 6                     'Absolut. Tv† operander.
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "LDA $" + HEX$(a)
      IF typ = 5 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 65535 THEN a = a - 65536
        END IF
      IF typ = 6 THEN
         a = a + reg.y
         opcode$ = opcode$ + ", Y"
         IF a > 65535 THEN a = a - 65536
      END IF
      c = 4                                  'OBS! Konstig!
   CASE 7                          'Pre-indexed...
      c = getbyte
      a = c + reg.x
      IF a > 255 THEN a = a - 256
      b = anybyte(a + 1)
      a = anybyte(a + 2)
      a = b + a * 256
      opcode$ = "LDA ($" + HEX$(c) + ", X)"
      c = 6
   CASE 8                          'Post-indexed...
      c = getbyte
      a = anybyte(c + 1)
      b = anybyte(c + 2)
      a = a + (b * 256) + reg.y
      IF a > 65535 THEN a = a - 65536
      opcode$ = "LDA ($" + HEX$(c) + "), Y"
      c = 5                                  'OBS! Konstig...
END SELECT
IF typ > 1 THEN reg.a = anybyte(a + 1)     'Stoppa byte i ackumulatorn...
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2  'Z?
IF reg.a > 127 THEN reg.s = reg.s OR 128'S?
cycle c
END SUB

SUB op.ldx (typ)
SELECT CASE typ
   CASE 1                          'Direkt. En operand.
      reg.x = getbyte
      opcode$ = "LDX #$" + HEX$(reg.x)
      cycle 2
   CASE 2 TO 3                       'Zero Page (Y)
      a = getbyte
      opcode$ = "LDX $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.y
         opcode$ = opcode$ + ", Y"
         IF a > 255 THEN a = a - 256
      END IF
      reg.x = anybyte(reg.x + 1)
      cycle typ + 1
   CASE 4 TO 5
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "LDX $" + HEX$(a)
      IF typ = 5 THEN
         a = a + reg.y
         opcode$ = opcode$ + ", Y"
         IF a > 65535 THEN a = a - 65536
      END IF
      reg.x = anybyte(a + 1)
      cycle 4                             'OBS 5:an „r konstig.
END SELECT
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
END SUB

SUB op.ldy (typ)
'LDY: Stoppa reg.y fullsmockat!
SELECT CASE typ
   CASE 1
      reg.y = getbyte
      opcode$ = "LDY #$" + HEX$(reg.y)
      cycle 2
   CASE 2 TO 3
      a = getbyte
      opcode$ = "LDY $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      reg.y = anybyte(a + 1)
      cycle 2 + typ
   CASE 4 TO 5
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "LDY $" + HEX$(a)
      IF typ = 5 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 65535 THEN a = a - 65536
      END IF
      reg.y = anybyte(a + 1)
      cycle 4                             'OBS 5:an „r konstig.
END SELECT
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg.s = reg.s OR 128
END SUB

SUB op.lsr (typ)
'LSR: Skifta h”ger (Dela med tv†).

SELECT CASE typ
   CASE 1                          'Ackumulatorn...
      z = reg.a / 2                'skifta h”ger  -Misst„nkt fel...
      reg.a = z
      b% = reg.a
      IF z AND .5 THEN reg.s = reg.s OR 1 'Om utanf”r s„tt carry
      opcode$ = "LSR"
      cycle 2
   CASE 2                     'Zero Page
      a = getbyte
      b% = anybyte(a + 1)
      z = b% / 2
      b% = z
      IF z AND .5 THEN reg.s = reg.s OR 1
      putbyte b%, a + 1
      opcode$ = "lsr $" + HEX$(a)
      cycle 5
END SELECT
reg.s = reg.s AND 252
IF b% = 0 THEN reg.s = reg.s OR 2


END SUB

DEFINT A
SUB op.ora (typ)
'ORA: ORa Ackumulatorn med operand/minne. Reultat i Ackumulatorn.
SELECT CASE typ
   CASE 1
      a = getbyte
      reg.a = reg.a OR a  'ORa den med Ackumulatorn.
      opcode$ = "ORA #$" + HEX$(a)
      cy = 2
   CASE 2 TO 3
      a = getbyte
      opcode$ = "ORA $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      cy = typ + 1
   CASE 4 TO 6
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "ORA $" + HEX$(a)
      IF typ = 5 THEN a = a + reg.x: opcode$ = opcode$ + ", X"
      IF typ = 6 THEN a = a + reg.y: opcode$ = opcode$ + ", Y"
      IF a > 65535 THEN a = a - 65536
      cy = 4
END SELECT
IF typ > 1 THEN
   b = anybyte(a + 1)
   reg.a = reg.a OR b
END IF
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
cycle cy
END SUB

DEFSNG A
SUB op.pha
'PHA: L„gg Ackumulatorn p† stacken...
push reg.a 'Se ovan...
opcode$ = "PHA"
cycle 3
END SUB

SUB op.php
'PHP: L„gg statusregistret p† stacken(!)
push reg.s
opcode$ = "PHP"
cycle 3
END SUB

SUB op.pla
'PLA: Stackens ”versta byte l„ggs i Ackumulatorn.
reg.a = pop

'OBS! Byten som l„ses in ligger fortfarande kvar i stacken.
'Detta leder till h”gre hastighet, men kanske ocks† haveri. Troligtvis inte.

opcode$ = "PLA"
cycle 4
END SUB

SUB op.plp
'PLP: Plocka status-registret fr†n stacken.
reg.s = pop
opcode$ = "PLP"
cycle 4
END SUB

SUB op.rol (typ)
'ROL: Rotera v„nster.
SELECT CASE typ
   CASE 1                          'Ackumulatorn.
      reg.a = reg.a * 2
      IF reg.s AND 1 THEN reg.a = reg.a + 1
      IF reg.a < 256 THEN reg.s = reg.s AND 254
      IF reg.a > 255 THEN reg.a = reg.a - 256: reg.s = reg.s OR 1
      'Om utanf”r: K”r runt med carry-flagen...
      opcode$ = "ROL"
      b = reg.a
      cy = 2
   CASE 2 TO 3                     'Zero Page(, X).
      a = getbyte
      opcode$ = "ROL $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      cy = typ + 3
END SELECT
IF typ > 1 THEN
   b = anybyte(a + 1)
   b = b * 2
   IF reg.s AND 1 THEN b = b + 1
   IF b < 256 THEN reg.s = reg.s AND 254
   IF b > 255 THEN b = b - 256: reg.s = reg.s OR 1
   putbyte INT(b), a + 1
END IF
reg.s = reg.s AND 125
IF b = 0 THEN reg.s = reg.s OR 2
IF b > 127 THEN reg.a = reg.a OR 128
cycle cy
END SUB

SUB op.ror (typ)
'ROR: Rotera h”ger.
'N„stan alla varianter „r snabbt ihopsl„ngda, och inneh†ller
'f”rmodligen fler buggar „n den att de inte wrappar...
SELECT CASE typ
   CASE 1
      r = reg.a
      z = reg.a / 2
      reg.a = z
      IF (reg.s AND 1) THEN reg.a = reg.a OR 128
      IF (r AND 1) THEN reg.s = reg.s OR 1
      IF (r AND 1) = 0 THEN reg.s = reg.s AND 254
      ra = reg.a
      opcode$ = "ROR"
      cycle 2
   CASE 2 TO 3
      a = getbyte
      opcode$ = "ROR $" + HEX$(a)
      IF typ = 3 THEN
         a = a + reg.x       'Borde wrappa...
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      r = anybyte(a + 1)
      z = r / 2
      ra = z
      IF (reg.s AND 1) THEN ra = ra OR 128
      IF (r AND 1) THEN reg.s = reg.s OR 1
      IF (r AND 1) = 0 THEN reg.s = reg.s AND 254
      putbyte ra, a + 1
      cycle 5
   CASE 4 TO 5
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "ROR $" + HEX$(a)
      IF typ = 5 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 65535 THEN a = a - 65536
      END IF
      r = anybyte(a + 1)
      z = r / 2
      ra = z
      IF (reg.s AND 1) THEN ra = ra OR 128
      IF (r AND 1) THEN reg.s = reg.s OR 1
      IF (r AND 1) = 0 THEN reg.s = reg.s AND 254
      putbyte ra, a + 1
      cycle 2 + typ
END SELECT
reg.s = reg.s AND 125
IF ra = 0 THEN reg.s = reg.s OR 2
IF ra > 127 THEN reg.a = reg.a OR 128
END SUB

SUB op.rti
'RTI: Return from Interrupt
reg.s = pop
a = pop
b = pop
pc = a + b * 256

opcode$ = "RTI"
END SUB

SUB op.rts
'RTS: L„s pc fr†n stacken.
a = pop              'F”rst den l†ga byten
b = pop              'Sedan den h”ga :)
pc = a + b * 256 + 1
opcode$ = "RTS"
cycle 6
END SUB

SUB op.sbc (typ)
'SBC: Ackumulatorn - operanden/minnesbyten - C
SELECT CASE typ
   CASE 1
      a = getbyte
      cy = 2
END SELECT
IF (a AND 128) THEN a = a - 256
reg.a = reg.a - a
reg.s = reg.s AND 60
IF (reg.a > 255) OR (reg.a) < 0 THEN reg.s = reg.s OR 64    'V?
IF reg.a > 255 THEN reg.a = reg.a - 256: reg.s = reg.s OR 1 'C?
IF reg.a = 0 THEN reg.s = reg.s OR 2                        'Z?
IF reg.a < 0 THEN reg.a = reg.a + 256: reg.s = reg.s OR 128 'S?
cycle cy

END SUB

SUB op.sec
'SEC: S„tt Carry-flagen.
reg.s = reg.s OR 1
opcode$ = "SEC"
cycle 2
END SUB

SUB op.sed
'SED: S„tt Decimal mode-flagen.
reg.s = reg.s OR 8
opcode$ = "SED"
cycle 2
END SUB

SUB op.sei
'SEI: I-flagen s„tts till 1.
reg.s = reg.s OR 4
opcode$ = "SEI"
cycle 2
END SUB

SUB op.sta (typ)
'STA: Stoppa Ackumulatorn i minnet.
SELECT CASE typ
   CASE 1 TO 2                       'Zero Page (X). En operand.
      a = getbyte
      opcode$ = "STA $" + HEX$(a)
      IF typ = 2 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      putbyte reg.a, a + 1
      cycle typ + 2
   CASE 3 TO 5                       'Absolut (X/Y). Tv† operander.
      b = getbyte
      a = getbyte
      a = a * 256 + b
      opcode$ = "STA $" + HEX$(a)
      IF typ = 4 THEN a = a + reg.x: opcode$ = opcode$ + ", X"
      IF typ = 5 THEN a = a + reg.y: opcode$ = opcode$ + ", Y"
      IF a > 65535 THEN a = a - 65536
      putbyte reg.a, a + 1
      IF typ = 3 THEN
         cycle 4
      ELSE
         cycle 5
      END IF
   CASE 6
      c = getbyte
      a = c + reg.x
      IF a > 255 THEN a = a - 256
      b = anybyte(a + 1)
      a = anybyte(a + 2)
      a = b + a * 256
      putbyte reg.a, a + 1
      opcode$ = "STA ($" + HEX$(c) + ", X)"
      cycle 6
   CASE 7
      c = getbyte
      a = anybyte(c + 1)
      b = anybyte(c + 2)
      a = a + b * 256 + reg.y
      IF a > 65535 THEN a = a - 65536
      putbyte reg.a, a + 1
      opcode$ = "STA ($" + HEX$(c) + "), Y"
      cycle 6
END SELECT

END SUB

SUB op.stx (typ)
'Stoppa reg.x i minnet...
SELECT CASE typ
   CASE 1 TO 2
      a = getbyte
      opcode$ = "STX $" + HEX$(a)
      IF typ = 2 THEN
         a = a + reg.y
         opcode$ = opcode$ + ", Y"
         IF a > 255 THEN a = a - 256
      END IF
      cy = 2 + typ
   CASE 3
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "STX $" + HEX$(a)
      cycle 4
END SELECT
putbyte reg.x, a + 1

END SUB

SUB op.sty (typ)
'STY: Stoppa reg.y i minnet.
SELECT CASE typ
   CASE 1 TO 2
      a = getbyte
      opcode$ = "STY $" + HEX$(a)
      IF typ = 2 THEN
         a = a + reg.x
         opcode$ = opcode$ + ", X"
         IF a > 255 THEN a = a - 256
      END IF
      cy = typ + 2
   CASE 3
      a = getbyte
      b = getbyte
      a = a + b * 256
      opcode$ = "STY $" + HEX$(a)
      cy = 4
END SELECT
putbyte reg.y, a + 1
cycle cy
END SUB

SUB op.tax
'TAX: Inneh†llet i Ackumulatorn f”rs ”ver till X-registret.
reg.x = reg.a
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
opcode$ = "TAX"
cycle 2
END SUB

SUB op.tay
'TAY: Inneh†llet i Ackumulatorn f”rs ”ver till Y-regisret.
reg.y = reg.a
reg.s = reg.s AND 125
IF reg.y = 0 THEN reg.s = reg.s OR 2
IF reg.y > 127 THEN reg.s = reg.s OR 128
opcode$ = "TAY"
cycle 2
END SUB

DEFSNG C, R
SUB op.tsx
'TSX: Stack-pekarens inneh†ll f”rs ”ver till X-registret.
reg.x = reg.sp
reg.s = reg.s AND 125
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x > 127 THEN reg.s = reg.s OR 128
opcode$ = "TSX"
cycle 2
END SUB

DEFINT C, R
SUB op.txa
'TXA: Inneh†llet i X-registret f”rs ”ver till Ackumulatorn.
reg.a = reg.x
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
opcode$ = "TXA"
cycle 2
END SUB

SUB op.txs
'TXS X-registret f”rs ”ver till stackpekaren...
reg.sp = reg.x
opcode$ = "TXS"
cycle 2
END SUB

SUB op.tya
'TYA: Inneh†llet i Y-registret f”rs ”ver till ackumulatorn.
reg.a = reg.y
reg.s = reg.s AND 125
IF reg.a = 0 THEN reg.s = reg.s OR 2
IF reg.a > 127 THEN reg.s = reg.s OR 128
opcode$ = "TYA"
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
GET #2, 257 + reg.sp, byte$
pop% = ASC(byte$)
END FUNCTION

SUB push (r)
byte$ = CHR$(r)
PUT #2, 257 + reg.sp, byte$
reg.sp = reg.sp - 1
END SUB

SUB putbyte (byte%, posi)
byte$ = CHR$(byte%)

'Kolla om n†gon speciell adress „r „ndrad.
IF posi >= &H2001 THEN              'Mer „n $1FFF?
   IF posi <= &H2008 THEN           'Mindre „n $2008?
     
      IF posi = &H2001 THEN ppu1 = byte%
    
      IF posi = &H2003 THEN ppustat = byte%
      IF posi = &H2004 THEN sprramposi = byte%
      IF posi = &H2005 THEN sprramposi = sprramposi + 1: MID$(sprram$, sprramposi, 1) = byte$
     
      IF posi = &H2007 THEN         'Žndra adress f”r VRAM-l„sning/skrivning.
         vramposi = (vramposi AND 255) * 256 + byte%
         'IF rabies = 1 THEN
         '   vramposi = (vramposi + ASC(byte$)): rabies = 0
         '   LOCATE 3, 40: PRINT vramposi
         'ELSE
         '   vramposi = ASC(byte$)         'Slintmeck pga slint-QBasic
         '   vramposi = vramposi * 256
         '   LOCATE 3, 40: PRINT vramposi
         '   '''vramposi = (ASC(byte$) * 256): rabies = 1
         'END IF
      END IF
      okn = 1
      IF (ppu1 AND 4) THEN okn = 32
      IF posi = &H2008 THEN PUT #4, vramposi + 1, byte$: vramposi = vramposi + okn: BEEP
      LOCATE 3, 40: PRINT HEX$(vramposi) + " = " + HEX$(byte%)
                        'Skriv till VRAM, och ”ka VRAM-"pointern".
      IF posi = &H4015 THEN PRINT "Transfer!"
      EXIT SUB
   END IF
END IF

IF posi >= &H4001 THEN
IF posi <= &H4018 THEN
EXIT SUB
END IF
END IF

PUT #2, posi, byte$
PUT #2, posi + &H800, byte$
PUT #2, posi + &H1000, byte$
PUT #2, posi + &H1800, byte$

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
SELECT CASE INKEY$
   CASE "u": ut = 1
   CASE "a": ut = 0
END SELECT
END SUB

