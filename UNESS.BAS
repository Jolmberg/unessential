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
'uNESsential Nintendo-emulator. Version 0.020
'
'Nyheter:
' Fler LDA-varianter...
' N†gra STA-varianter...
'Minne emuleras genom filen MEMORY.NIN.
'Stacken emuleras genom filen STACK.NIN.
'Alla opcode-funktioner heter op_???(_?).
'Register emuleras genom variabler.
'Alla register-variabler heter reg.??.

'Variabler:
'fil$ - Filnamn p† rommen som k”rs.
'byte - variabel f”r h„mtning av en byte.

SCREEN 12
' Alla register m†ste vara share-ade...
DIM SHARED reg.a, reg.x, reg.y, reg.s.c, reg.s.z, reg.s.i, reg.s.d
DIM SHARED reg.s.b, reg.s.v, reg.s.s, reg.pc, reg.sp
'WARNING! Sharea inte "byte$". D† blir det haveri.

'™vriga share-ade variabler...
DIM SHARED fil$, byte$
byte$ = STRING$(1, " ")

'Ladda in en rom...
COLOR 9
INPUT "Load ROM:", fil$


'Rita lite saker...
CLS
LINE (0, 0)-(256, 224), 8, B


'F”rberedelser...
OPEN fil$ FOR BINARY AS #1         'Rommen har filnummer #1.
OPEN "MEMORY.NIN" FOR BINARY AS #2 'Minnet har filnummer #2.
OPEN "STACK.NIN" FOR BINARY AS #3  'Stacken har filnummer #3.
reg.pc = 17                        'Skippa 16-bytes-headern...
reg.sp = 255                       'Stack-pekaren m†ste vara 255.
emulera                            'B”rja emulera!

SUB emulera
byte$ = " "
DO

GET #1, reg.pc, byte$

'En miljard case-ar!
'OBS! B”r sorteras i ordningen: vanligast f”rst - ovanligast sist...

SELECT CASE ASC(byte$)
   CASE &HAA: op.tax
   CASE &HA8: op.tay
   CASE &HBA: op.tsx
   CASE &H8A: op.txa

   CASE &HE8: op.inx
   CASE &HC8: op.iny

   CASE &H85: op.sta (1)
   CASE &H95: op.sta (2)
   CASE &H80: op.sta (3)
   CASE &H90: op.sta (4)

   CASE &HA9: op.lda (1)
   CASE &HA5: op.lda (2)
   CASE &HB5: op.lda (3)
   CASE &HAD: op.lda (4)
   CASE &HBD: op.lda (5)
  
   CASE &HCA: op.dex
   CASE &H88: op.dey

   CASE &H48: op.pha
   CASE &H68: op.pla
END SELECT
reg.pc = reg.pc + 1
LOOP

END SUB

SUB op.dex
'DEX: X-registret minskas med 1.
reg.x = reg.x - 1
END SUB

SUB op.dey
'DEY: Y-registret minskas med 1.
reg.y = reg.y - 1
END SUB

SUB op.inx
'INX: X-registret ”kas med 1.
reg.x = reg.x + 1
END SUB

SUB op.iny
'INY: Y-registret ”kas med 1.
reg.y = reg.y
END SUB

SUB op.lda (typ)
'LDA: Ladda Ackumulatorn med [operand(er)]

SELECT CASE typ
   CASE 1                          'Direkt. En operand.
      reg.pc = reg.pc + 1          'Bytepekaren ”kas med 1 (per operand).
      GET #1, reg.pc, byte$        'Kr†ngel utan short int...
      reg.a = ASC(byte$)

   CASE 2                          'Zero Page. En operand.
      reg.pc = reg.pc + 1
      GET #1, reg.pc, byte$        'Plocka ut operanden
      GET #2, ASC(byte$), byte$    'Plocka byte ur MEMORY.NIN
      reg.a = ASC(byte$)           'L„gg den i Ackumulatorn

   CASE 3                          'Zero Page X. En operand
      reg.pc = reg.pc + 1
      GET #1, reg.pc, byte$
      GET #2, ASC(byte$) + reg.x, byte$   'Plocka ur minnet: operand+x
      reg.a = ASC(byte$)

   CASE 4                          'Absolut. Tv† operander.
      reg.pc = reg.pc + 2
      GET #1, reg.pc - 1, byte$: a = ASC(byte$)
      GET #1, reg.pc, byte$: b = ASC(byte$)
      a = a + b * 256
      GET #2, a, byte$
      reg.a = ASC(byte$)

   CASE 5                          'Absolut X. Tv† operander.
      reg.pc = reg.pc + 2
      GET #1, reg.pc - 1, byte$: a = ASC(byte$)
      GET #1, reg.pc, byte$: b = ASC(byte$)
      a = a + b * 256
      GET #2, a + reg.x, byte$
      reg.a = ASC(byte$)

END SELECT
      
IF reg.a = 0 THEN reg.s.z = 1      'Z-flagen s„tts om Ackumulatorn „r 0.


END SUB

SUB op.pha
'PHA: L„gg Ackumulatorn p† stacken...
reg.sp = reg.sp - 1                'Minska stackpekaren med 1.
PUT #3, reg.sp, reg.a              'Skriv dit Ackumulatorn.
END SUB

SUB op.pla
'PLA: Stackens ”versta byte l„ggs i Ackumulatorn.
GET #3, reg.sp, reg.a              'Plocka byten.
reg.sp = reg.sp - 1                '™ka stackpekaren med 1.

'OBS! Byten som l„ses in ligger fortfarande kvar i stacken.
'Detta leder till h”gre hastighet, men det kanske ocks† haveri.
END SUB

SUB op.sta (typ)
SELECT CASE typ
   CASE 1 TO 2                       'Zero Page. En operand.
      reg.pc = reg.pc + 1
      GET #1, reg.pc, byte$: a = ASC(byte$)'Put-kommandot var tjurigt mot asc
      IF typ = 2 THEN a = a + reg.x
      PUT #2, a, reg.a

   CASE 3 TO 4
      reg.pc = reg.pc + 2
      get #1,re.pc-1,


END SUB

SUB op.tax
'TAX: Inneh†llet i Ackumulatorn f”rs ”ver till X-registret.
reg.x = reg.a
END SUB

SUB op.tay
'TAY: Inneh†llet i Ackumulatorn f”rs ”ver till Y-regisret.
reg.y = reg.a
END SUB

SUB op.tsx
'TSX: Stack-pekarens inneh†ll f”rs ”ver till X-registret.
reg.x = reg.sp
END SUB

SUB op.txa
'TXA: Inneh†llet i X-registret f”rs ”ver till Ackumulatorn.
reg.a = reg.x
END SUB

