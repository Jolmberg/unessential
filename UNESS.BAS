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
DEFINT R
            
'Status: SV1BDIZC
' Alla register m†ste vara share-ade...
DIM SHARED reg.a, reg.x, reg.y, reg.s, pc, reg.sp, fil$, byte$, opcode$

'uNESsential Nintendo-emulator. Version 0.055
'
'Nyheter:
' En ADC-opcode...
' Fixat JMP-opcoden tror jag (l„gger till 1)...
' L„ser NMI-vektorn... (men anv„nder den inte)...
'
'Minne emuleras genom filen MEMORY.NIN.
'Stacken emuleras genom filen STACK.NIN.
'Alla opcode-funktioner heter op_???(_?).
'Register emuleras genom variabler.
'Alla register-variabler heter reg.?. (undantag: pc, som inte f†r vara INT)

'Variabler:
'fil$ - Filnamn p† rommen som k”rs.
'byte - variabel f”r h„mtning av en byte.



'WARNING! Sharea inte "byte$". D† blir det haveri. <---Snack!



byte$ = CHR$(0)
CLS
'Ladda in en rom...
COLOR 9
INPUT "Load ROM:", fil$


'Rita lite saker...
CLS
'LINE (0, 0)-(256, 224), 8, B


'F”rberedelser...
OPEN fil$ FOR BINARY AS #1         'Rommen har filnummer #1.
OPEN "MEMORY.NIN" FOR BINARY AS #2 'Minnet har filnummer #2. OBS Byte 1=0
OPEN "STACK.NIN" FOR BINARY AS #3  'Stacken har filnummer #3.
reg.s = 32                         'Status-bit 6=1
pc = 17                            'Skippa 16-bytes-headern...
reg.sp = 255 'Kanske 256...        'Stack-pekaren m†ste vara 255.
header
emulera                            'B”rja emulera!

SUB emulera
byte$ = " "
DO

GET #2, pc, byte$
bytes$ = HEX$(ASC(byte$)) + " "
'En miljard case-ar!
'OBS! B”r sorteras i ordningen: vanligast f”rst - ovanligast sist...

SELECT CASE ASC(byte$)
   CASE &HAA: op.tax
   CASE &H8A: op.txa
   CASE &HA8: op.tay
   CASE &H9A: op.txs
   CASE &HBA: op.tsx
  
   CASE &HE8: op.inx
   CASE &HC8: op.iny

   CASE &H85: op.sta (1)
   CASE &H95: op.sta (2)
   CASE &H8D: op.sta (3)
   CASE &H9D: op.sta (4)
   CASE &H99: op.sta (5)

   CASE &HA9: op.lda (1)
   CASE &HA5: op.lda (2)
   CASE &HB5: op.lda (3)
   CASE &HAD: op.lda (4)
   CASE &HBD: op.lda (5)
  
   CASE &HA2: op.ldx (1)
   CASE &HA6: op.ldx (2)
   CASE &HB6: op.ldx (3)
  
   CASE &HCA: op.dex
   CASE &H88: op.dey

   CASE &H48: op.pha
   CASE &H68: op.pla

   CASE &H4A: op.lsr (1)
   CASE &HA: op.asl (1)
   CASE &H9:  op.ora (1)
   CASE &H2A: op.rol (1)
   CASE &H6A: op.ror (1)

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

   CASE &H4C: op.jmp (1)
 
   CASE &H69: op.adc (1)

   CASE ELSE: opcode$ = "Oimplementerad :("
END SELECT
utskrift
pc = pc + 1


LOOP

END SUB

SUB header
'Inl„sning av headern f”r att kolla mappers och storlekar...
'Just nu „r bara antaled prg-rom-bankar viktigt...
bytes$ = STRING$(32767, CHR$(0))
PUT #2, 1, bytes$
PUT #2, 32768, bytes$


GET #1, 5, byte$
bytes$ = ""
prgs = ASC(byte$)
bytes$ = STRING$(16384, " ")
IF prgs = 1 THEN
   GET #1, 17, bytes$              'Plocka ett prg-block.
   PUT #2, 32769, bytes$          'Stoppa det i b†de $8000
   PUT #2, 49153, bytes$          'och $C000.

ELSEIF prgs = 2 THEN
   GET #1, 17, bytes$              'Plocka ett prg-block.
   PUT #2, 32769, bytes$          'Stoppa det i $8000
   GET #1, 16401, bytes$           'Plocka ett till
   PUT #2, 49153, bytes$          'och stoppa det i $C000.

ELSE
   PRINT "Inte mapper #0-rom!"
   END
END IF

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
PRINT "NMI-Position:"; HEX$(a); " /"; a; "(+1)..."


byte$ = " "
END SUB

SUB op.adc (typ)
'ADC: L„gg till operanden (adressen) till Ackumulatorn
SELECT CASE typ
   CASE 1
      pc = pc + 1
      GET #2, pc, byte$
      a = ASC(byte$)
      IF (reg.s AND 1) = 1 THEN a = a + 1
      reg.a = reg.a + a
      PRINT "ADC "; HEX$(ASC(byte$))
END SELECT

IF reg.a > 255 THEN reg.s = reg.s OR 1: reg.a = reg.a - 256: reg.s = reg.s OR 64
IF reg.a = 0 THEN reg.s = reg.s OR 2

END SUB

SUB op.asl (typ)
'ASL: Shift Left...

SELECT CASE typ
   CASE 1
      reg.a = reg.a * 2
      IF reg.a > 255 THEN reg.a = reg.a - 256: reg.s = reg.s OR 1
      'Om utanf”r: S„tt c-flagen...
      opcode$ = "ASL"
END SELECT


reg.s = reg.s AND 253
IF reg.a = 0 THEN reg.s = reg.s OR 2
END SUB

SUB op.beq
'BEQ: Brancha om Z=1
IF (reg.s OR 2) = 2 THEN
   pc = pc + 1
   GET #2, pc, byte$
   a = ASC(byte$)
   IF a > 127 THEN a = a - 256
   pc = pc + a
  END IF
opcode$ = "BEQ $" + HEX$(ASC(byte$))
END SUB

SUB op.bmi
'BMI: Brancha om s=1
IF (reg.s OR 128) = 128 THEN
   pc = pc + 1
   GET #2, pc, byte$
   a = ASC(byte$)
   IF a > 127 THEN a = a - 256
   pc = pc + a
  END IF
opcode$ = "BMI $" + HEX$(ASC(byte$))

END SUB

SUB op.bne
'BEQ: Brancha om Z=0
IF (reg.s AND 2) = 0 THEN
   pc = pc + 1
   GET #2, pc, byte$
   a = ASC(byte$)
   IF a > 127 THEN a = a - 256
   pc = pc + a
END IF
opcode$ = "BNE $" + HEX$(ASC(byte$))


END SUB

SUB op.bpl
'BPL: Brancha om s=0
IF (reg.s AND 128) = 0 THEN
   pc = pc + 1
   GET #2, pc, byte$
   a = ASC(byte$)
   IF a > 127 THEN a = a - 256
   pc = pc + a
  END IF
opcode$ = "BPL $" + HEX$(ASC(byte$))

END SUB

SUB op.clc
'CLC: Rensa carry-flagen...
reg.s = reg.s AND 254
opcode$ = "CLC"
END SUB

SUB op.cld
'CLD: Rensa Decimal-flagen.
reg.s = reg.s AND 247
opcode$ = "CLD"
END SUB

SUB op.cli
'CLI: Rensa Interrupt disable-flagen.
reg.s = reg.s AND 251
opcode$ = "CLI"
END SUB

SUB op.clv
'CLV: Rensa Owerflow-flagen.
reg.s = reg.s AND 191
opcode$ = "CLV"
END SUB

SUB op.dex
'DEX: X-registret minskas med 1.
reg.x = reg.x - 1
reg.s = reg.s AND 253
IF reg.x = 0 THEN reg.s = reg.s OR 2
IF reg.x = -1 THEN reg.x = 255: reg.s = reg.s OR 128  'Kanske n†t konstigt...
opcode$ = "DEX"
END SUB

SUB op.dey
'DEY: Y-registret minskas med 1.
reg.y = reg.y - 1
reg.s = reg.s AND 253
IF reg.y = 0 THEN reg.s = reg.s OR 2                 'Z?
IF reg.y = -1 THEN reg.y = 255: reg.s = reg.s OR 128 'A?
opcode$ = "DEY"
END SUB

SUB op.inx
'INX: X-registret ”kas med 1.
reg.x = reg.x + 1
IF reg.x = 256 THEN reg.x = 0    'Kanske overflow...

reg.s = reg.s AND 253
IF reg.x = 0 THEN reg.s = reg.s OR 2

opcode$ = "INX"
END SUB

SUB op.iny
'INY: Y-registret ”kas med 1.
reg.y = reg.y + 1
IF reg.y = 256 THEN reg.y = 0

reg.s = reg.s AND 253
IF reg.y = 0 THEN reg.s = reg.s OR 2

opcode$ = "INY"

END SUB

SUB op.jmp (typ)
'JMP: Hoppa till by position...
SELECT CASE typ
   CASE 1                          'Direkt. Tv† operander.
      pc = pc + 2
      GET #2, pc, byte$: a = ASC(byte$)
      GET #2, pc - 1, byte$
      pc = ASC(byte$) + a * 256 + 1
      
      opcode$ = "JMP $" + HEX$(pc)
END SELECT
END SUB

SUB op.lda (typ)
'LDA: Ladda Ackumulatorn med [operand(er)]

SELECT CASE typ
   CASE 1                          'Direkt. En operand.
      pc = pc + 1          'Bytepekaren ”kas med 1 (per operand).
      GET #2, pc, byte$
      reg.a = ASC(byte$)
      opcode$ = "LDA #$" + HEX$(ASC(byte$))
   CASE 2 TO 3                     'Zero Page (x). En operand.
      pc = pc + 1
      GET #2, pc, byte$            'Plocka ut operanden
      a = ASC(byte$)
      IF typ = 3 THEN a = a + reg.x
      GET #2, a + 1, byte$         'Plocka byte ur MEMORY.NIN
      reg.a = ASC(byte$)           'L„gg den i Ackumulatorn
      IF typ = 2 THEN opcode$ = "LDA $" + HEX$(a)
      IF typ = 3 THEN opcode$ = "LDA $" + HEX$(a - reg.x) + ",X  =$" + HEX$(a)
      
  
   CASE 4 TO 5                     'Absolut. Tv† operander.
      pc = pc + 2
      GET #2, pc - 1, byte$: a = ASC(byte$)
      GET #2, pc, byte$: b = ASC(byte$)
      a = a + b * 256
      IF typ = 5 THEN a = a + reg.x
      GET #2, a + 1, byte$
      reg.a = ASC(byte$)
      IF typ = 4 THEN opcode$ = "LDA $" + HEX$(a)
      IF typ = 5 THEN opcode$ = "LDA $" + HEX$(a - reg.x) + ",X  =$" + HEX$(a)

END SELECT
reg.s = reg.s AND 253
IF reg.a = 0 THEN reg.s = reg.s OR 2  'Z?


END SUB

SUB op.ldx (typ)
SELECT CASE typ
   CASE 1                          'Direkt. En operand.
      pc = pc + 1
      GET #2, pc, byte$
      reg.x = ASC(byte$)
      opcode$ = "LDX #$" + HEX$(reg.x)

   CASE 2, 3                       'Zero Page (Y)
      pc = pc + 1
      GET #2, pc, byte$
      reg.x = ASC(byte$)
      IF typ = 3 THEN reg.x = reg.x + reg.y
      a = reg.x
      GET #2, reg.x + 1, byte$
      reg.x = ASC(byte$)
      IF typ = 2 THEN opcode$ = "LDX $" + HEX$(a)
      IF typ = 3 THEN opcode$ = "LDX $" + HEX$(a - reg.y) + ",X  =" + HEX$(a)

END SELECT
reg.s = reg.s AND 253
IF reg.x = 0 THEN reg.s = reg.s OR 2
END SUB

SUB op.lsr (typ)
'LSR: Skifta h”ger (Dela med tv†).

SELECT CASE typ
   CASE 1                          'Ackumulatorn...
      z = reg.a / 2                'skifta h”ger
      reg.a = z
      IF z AND .5 THEN reg.s = reg.s OR 1 'Om utanf”r s„tt carry
      opcode$ = "LSR"
END SELECT
reg.s = reg.s AND 253
IF reg.a = 0 THEN reg.s = reg.s OR 2


END SUB

DEFINT A
SUB op.ora (typ)
'ORA: ORa Ackumulatorn med operand/minne. Reultat i Ackumulatorn.
SELECT CASE typ
   CASE 1
      pc = pc + 1
      GET #2, pc, byte$            'Plocka operanden
      reg.a = reg.a OR ASC(byte$)  'ORa den med Ackumulatorn.
      opcode$ = "ORA #$" + HEX$(a)
END SELECT

reg.s = reg.s AND 253
IF reg.a = 0 THEN reg.s = reg.s OR 2

END SUB

DEFSNG A
SUB op.pha
'PHA: L„gg Ackumulatorn p† stacken...
reg.sp = reg.sp - 1                'Minska stackpekaren med 1.
PUT #3, reg.sp, reg.a              'Skriv dit Ackumulatorn.
opcode$ = "PHA"
END SUB

SUB op.pla
'PLA: Stackens ”versta byte l„ggs i Ackumulatorn.
GET #3, reg.sp, reg.a              'Plocka byten.
reg.sp = reg.sp + 1                '™ka stackpekaren med 1.
reg.s = reg.s AND 253
IF reg.a = 0 THEN reg.s = reg.s OR 2      'Om noll s„tt z-flagen.
'OBS! Byten som l„ses in ligger fortfarande kvar i stacken.
'Detta leder till h”gre hastighet, men kanske ocks† haveri. Troligtvis inte.
opcode$ = "PLA"
END SUB

SUB op.rol (typ)
'ROL: Rotera v„nster.
SELECT CASE typ
   CASE 1                          'Ackumulatorn.
      reg.a = reg.a * 2
      IF reg.a > 255 THEN reg.a = reg.a - 255': reg.s.c = 1  Vet ej...
      'Om utanf”r: K”r runt med carry-flagen...
      opcode$ = "ROL"
END SELECT

reg.s = reg.s AND 253
IF reg.a = 0 THEN reg.s = reg.s OR 2

END SUB

SUB op.ror (typ)
SELECT CASE typ
   CASE 1
      z = reg.a / 2
      reg.a = z
      IF z AND .5 = .5 THEN reg.a = reg.a + 255': reg.s= reg.s or 1 Vet ej...
      'Om utanf”r: K”r runt med carry.
      opcode$ = "ROR"
END SELECT
reg.s = reg.s AND 253
IF reg.a = 0 THEN reg.s = reg.s OR 2
END SUB

SUB op.sec
'SEC: S„tt Carry-flagen.
reg.s = reg.s OR 1
opcode$ = "SEC"
END SUB

SUB op.sed
'SED: S„tt Decimal mode-flagen.
reg.s = reg.s OR 8
opcode$ = "SED"
END SUB

SUB op.sei
'SEI: I-flagen s„tts till 1.
reg.s = reg.s OR 4
opcode$ = "SEI"
END SUB

SUB op.sta (typ)
'STA: Stoppa Ackumulatorn i minnet.
SELECT CASE typ
   CASE 1 TO 2                       'Zero Page (X). En operand.
      pc = pc + 1
      GET #2, pc, byte$: a = ASC(byte$)'Put-kommandot var tjurigt mot asc
      IF typ = 2 THEN a = a + reg.x
      PUT #2, a + 1, reg.a
      opcode$ = "STA $" + HEX$(ASC(byte$)) + " (+$" + HEX$(reg.x) + ")  =$" + HEX$(a)

   CASE 3 TO 5                       'Absoulut (X/Y). Tv† operander.
      pc = pc + 2
      GET #2, pc - 1, byte$: b = ASC(byte$)
      GET #2, pc, byte$: a = ASC(byte$)
      a = a * 256 + b
      IF typ = 4 THEN a = a + reg.x
      IF typ = 5 THEN a = a + reg.y
      PUT #2, a + 1, reg.a
      opcode$ = "STA $" + HEX$(ASC(byte$) * 256 + b) + " (+$" + HEX$(reg.x) + "/$" + HEX$(reg.y) + ")  =$" + HEX$(a)
END SELECT

END SUB

SUB op.tax
'TAX: Inneh†llet i Ackumulatorn f”rs ”ver till X-registret.
reg.x = reg.a
reg.s = reg.s AND 253
IF reg.x = 0 THEN reg.s = reg.s OR 2
opcode$ = "TAX"
END SUB

SUB op.tay
'TAY: Inneh†llet i Ackumulatorn f”rs ”ver till Y-regisret.
reg.y = reg.a
reg.s = reg.s AND 253
IF reg.y = 0 THEN reg.s = reg.s OR 2
opcode$ = "TAY"
END SUB

DEFSNG R
SUB op.tsx
'TSX: Stack-pekarens inneh†ll f”rs ”ver till X-registret.
reg.x = reg.sp
reg.s = reg.s AND 253
IF reg.x = 0 THEN reg.s = reg.s OR 2
opcode$ = "TSX"
END SUB

DEFINT R
SUB op.txa
'TXA: Inneh†llet i X-registret f”rs ”ver till Ackumulatorn.
reg.a = reg.x
reg.s = reg.s AND 253
IF reg.a = 0 THEN reg.s = reg.s OR 2
opcode$ = "TXA"
END SUB

SUB op.txs
'TXS X-registret f”rs ”ver till stackpekaren...
reg.sp = reg.x
opcode$ = "TXS"
END SUB

SUB utskrift
PRINT bytes$; opcode$
PRINT "A:"; HEX$(reg.a); " X:"; HEX$(reg.x); " Y:"; HEX$(reg.y); " S:"; HEX$(reg.sp); " PC:"; HEX$(pc); " ";
'Status: SV1BDIZC
IF (reg.s AND 1) = 1 THEN PRINT "C";
IF (reg.s AND 2) = 2 THEN PRINT "Z";
IF (reg.s AND 4) = 4 THEN PRINT "I";
IF (reg.s AND 8) = 8 THEN PRINT "D";
IF (reg.s AND 16) = 16 THEN PRINT "B";
IF (reg.s AND 64) = 64 THEN PRINT "V";
IF (reg.s AND 128) = 128 THEN PRINT "S"
PRINT
SLEEP
a$ = INKEY$

END SUB

