DEFINT A-Z
DECLARE SUB selectSprite ()
DECLARE SUB userInput ()
DECLARE SUB drawMapApp ()
DECLARE SUB drawSprApp ()
DECLARE SUB handleSprite ()
DECLARE SUB sysConsole ()
DECLARE SUB handleFile ()
DECLARE SUB handleMap ()
DECLARE SUB updScreen ()
DECLARE SUB initEditor ()
DECLARE SUB setToolPos ()

RANDOMIZE TIMER
ON ERROR GOTO BADFILE:

TYPE obj
	x AS INTEGER
	y AS INTEGER
	drawx AS INTEGER
	drawy AS INTEGER
	pickx AS INTEGER
	picky AS INTEGER
	barx AS INTEGER
	bary AS INTEGER
	collide AS INTEGER
	sprName AS STRING * 8
	picked AS INTEGER
	clr AS INTEGER
	spr AS INTEGER
END TYPE

DIM SHARED tool AS obj
DIM SHARED cam  AS obj
DIM SHARED sprite AS obj

DIM SHARED grid(8, 8)
DIM SHARED gridMap(100, 100)

DIM SHARED gSize
DIM SHARED gmSize
DIM SHARED gridColor

DIM SHARED filen AS STRING
DIM SHARED src(8, 8)

DIM SHARED sprBuffer(84)
DIM SHARED toolSpr(84)

DIM SHARED sprNameList(64) AS STRING
DIM SHARED sprTypeData(64)
DIM SHARED barGrid(64, 64)

DIM SHARED console AS STRING
DIM SHARED sysTask AS STRING
DIM SHARED sysApp AS STRING
DIM SHARED setMode AS STRING

DIM SHARED consoleColor
DIM SHARED modeColor
DIM SHARED modeText AS STRING
DIM SHARED sprCount

DIM SHARED sprIndex(2800)
DIM SHARED frameIndex(2800)
DIM SHARED pakName AS STRING

DIM SHARED colorPal(16)
DIM SHARED exitApp

initEditor
BADFILE:
	sysTask = ""
	filen = ""
DO
	userInput
	updScreen
	SLEEP
LOOP UNTIL exitApp = 1
WHILE INKEY$ <> "": WEND
SYSTEM

SUB handleFile

	WHILE INKEY$ <> "": WEND
	COLOR consoleColor
	SELECT CASE sysTask
		CASE "NEW GFX"
			FOR x = 0 TO gSize
				FOR y = 0 TO gSize
					grid(x, y) = 0
				NEXT
			NEXT
			filen = ""

		CASE "SAVE GFX"
			LOCATE 23, 2: INPUT "GFX NAME:", filen
			OPEN filen FOR OUTPUT AS #1
			FOR x = 0 TO 8
				FOR y = 0 TO 8
					IF y < gSize THEN
						PRINT #1, grid(x, y); ",";
					ELSE
						PRINT #1, grid(x, y);
					END IF
				IF y = gSize THEN PRINT #1,
				NEXT
			NEXT
			CLOSE #1

		CASE "LOAD GFX"
			LOCATE 23, 2: INPUT "LOAD GFX:", filen
			OPEN filen FOR INPUT AS #1
			FOR x = 0 TO 8
				FOR y = 0 TO 8
					INPUT #1, grid(x, y)
				NEXT
			NEXT
			CLOSE #1
			updScreen


		CASE "SAVE PAK"
			CLS
			PUT (8, 8), frameIndex, PSET
			ERASE frameIndex
			index = 0
			FOR b = 1 TO 4
				FOR i = 1 TO 38
					index = index + 1
					GET (8 * i, b * 8)-(8 + (8 * i), 8 + (b * 8)), frameIndex(42 * index)
				NEXT
			NEXT

			LOCATE 23, 2: INPUT "PAK NAME:", filen
			OPEN filen FOR OUTPUT AS #1
			total = (SprNum * 42) + 1
			FOR i = 1 TO total
				PRINT #1, frameIndex(i)
			NEXT

			'FOR i = 1 TO SprNum
			' PRINT #1, sprNameList(i)    ' need the names in the game?
			'NEXT

			FOR i = 1 TO SprNum
				PRINT #1, sprTypeData(i)
			NEXT
			CLOSE #1
			filen = ""

		CASE "LOAD PAK"
			LOCATE 23, 2: INPUT "LOAD PAK: ", filen
			OPEN filen FOR INPUT AS #1
			fileLen = (SprNum * 42) + 1
			FOR i = 1 TO fileLen
				INPUT #1, sprIndex(i)
			NEXT
			index = 0
			FOR i = 6386 TO 6537
				index = index + 1
				INPUT #1, sprNameList(index)
			NEXT
			index = 0
			FOR i = 6538 TO 6689
				index = index + 1
				INPUT #1, sprTypeData(index)
			NEXT
			CLOSE #1
			pakName = filen
			filen = ""

		CASE "NEW PAL"
			FOR i = 1 TO 16
				LOCATE 23, 2: INPUT "COLOR:", colorPal(i)
			NEXT

		CASE "LIST PAL"
			FOR i = 1 TO 16
				 COLOR colorPal(i): PRINT colorPal(i)
			NEXT
			SLEEP

		CASE "SAVE PAL"
			LOCATE 23, 2: INPUT "PAL NAME:", filen
			OPEN filen FOR OUTPUT AS #1
			FOR i = 1 TO 16
				PRINT #1, colorPal(i)
			NEXT
			CLOSE #1
			filen = ""

		CASE "LOAD PAL"
			LOCATE 23, 2: INPUT "LOAD PAL:", filen
			OPEN filen FOR INPUT AS #1
			FOR i = 1 TO 16
				INPUT #1, colorPal(i)
			NEXT
			CLOSE #1
			filen = ""

		CASE "NEW MAP"
			FOR x = 0 TO gmSize
				FOR y = 0 TO gmSize
					gridMap(x, y) = 0
				NEXT
			NEXT

		CASE "SAVE MAP"
				LOCATE 23, 2: INPUT "MAP NAME: ", filen
			OPEN filen FOR OUTPUT AS #1
			FOR x = 0 TO gmSize
				FOR y = 0 TO gmSize
					IF y < gmSize THEN
						PRINT #1, gridMap(x, y); ",";
					ELSE
						PRINT #1, gridMap(x, y);
					END IF
				IF y = gmSize THEN PRINT #1,
				NEXT
			NEXT
			CLOSE #1

		CASE "LOAD MAP"
			LOCATE 23, 2: INPUT "LOAD MAP: ", filen
			OPEN filen FOR INPUT AS #1
			FOR x = 0 TO gmSize
				FOR y = 0 TO gmSize
					INPUT #1, gridMap(x, y)
				NEXT
			NEXT
			CLOSE #1
	END SELECT
	sysTask = ""

END SUB

SUB handleMap

	WHILE INKEY$ <> "": WEND
	SELECT CASE sysTask
		CASE "REPL TILE"
			COLOR consoleColor
			LOCATE 23, 2: INPUT "REPLACE:", sprite0
			LOCATE 23, 2: INPUT "WITH:", sprite1
			FOR x = 0 TO gSize
				FOR y = 0 TO gSize
					IF gridMap(x + 20, y + 12) = sprite0 * 42 THEN gridMap(x + 20, y + 12) = sprite1 * 42
				NEXT
			NEXT

		CASE "FILL"
			COLOR consoleColor
			LOCATE 23, 2: INPUT "COORDx:", coord0
			LOCATE 23, 2: INPUT "COORDy:", coord1
			LOCATE 23, 2: INPUT "FILL SPR#:", getSpr
			FOR x = 0 TO coord0
				FOR y = 0 TO coord1
					gridMap(x + 20, y + 12) = getSpr * 42
				NEXT
			NEXT

		CASE "RAND"
			COLOR consoleColor
			LOCATE 23, 2: INPUT "COORDx:", coord0
			LOCATE 23, 2: INPUT "COORDy:", coord1
			LOCATE 23, 2: INPUT "#1-255:", noise
			LOCATE 23, 2: INPUT "SPR#:", getSpr
			FOR x = 0 TO coord0
				FOR y = 0 TO coord1
					IF INT(RND * noise) + 1 = 1 THEN gridMap(x + 20, y + 12) = getSpr * 42
				NEXT
			NEXT
	END SELECT
	sysTask = ""

END SUB

SUB handleSprite

	WHILE INKEY$ <> "": WEND
	COLOR consoleColor
	SELECT CASE sysTask
		CASE "DEL PIXEL"
			grid(tool.x - 8, tool.y - 16) = 0

		CASE "REPL PIXEL"
			LOCATE 23, 2: INPUT "REPLACE:", color0
			LOCATE 23, 2: INPUT "WITH:", color1
			FOR x = 0 TO gSize
				FOR y = 0 TO gSize
					IF grid(x, y) = color0 THEN grid(x, y) = color1
				NEXT
			NEXT
			
		CASE "PLACE"
			barGrid(tool.barx, tool.bary) = tool.barx * tool.bary
			index = barGrid(tool.barx, tool.bary)
			GET (110, 80)-(118, 88), frameIndex(42 * index)
	END SELECT
	sysTask = ""

END SUB

SUB initEditor

	SCREEN 13
	consoleColor = 43
	GET (110, 80)-(118, 88), sprBuffer(42)
	
	sysApp = "SPRITE"
	setMode = "DRAW"
	modeColor = 52
	modeText = "MODE:SPRITE"
	setToolPos

	tool.spr = 42
	tool.clr = modeColor
		
	gridColor = 21
	gmSize = 100
	gSize = 7
	
	cam.x = 30
	cam.y = 11
	
	SprNum = 64

	FOR i = 1 TO 16
		colorPal(i) = i
	NEXT

	OPEN "TOOL.GFX" FOR INPUT AS #1
	FOR x = 0 TO 8
		FOR y = 0 TO 8
			INPUT #1, src(x, y)
		NEXT
	NEXT
	CLOSE #1
	FOR x = 0 TO 8
		FOR y = 0 TO 8
			PSET (x, y), src(x, y)
		NEXT
	NEXT
	GET (0, 0)-(8, 8), toolSpr(42)
	ERASE src
	CLS
	updScreen

END SUB

SUB setToolPos

	SELECT CASE setMode
		CASE "DRAW"
			tool.x = 8
			tool.y = 16
			tool.drawx = 1
			tool.drawy = 1

		CASE "PICK COLOR"
			tool.x = 18
			tool.y = 12
			tool.pickx = 1
			tool.pickx = 1
			
		CASE "SPRITE BAR"
			tool.x = 3
			tool.y = 3
			tool.barx = 1
			tool.bary = 1
	END SELECT

END SUB

SUB sysConsole

	LOCATE 23, 8: COLOR 17: PRINT "'HELP' FOR COMMANDS"
	COLOR consoleColor
	
	WHILE INKEY$ <> "": WEND
	LOCATE 23, 2: INPUT ">>>: ", console
	WHILE INKEY$ <> "": WEND
	SELECT CASE console
		CASE "mem"
			LOCATE 23, 2: PRINT "USED:"; 354418 - FRE(-1); "/"; 354418
			SLEEP

		CASE "exit"
			exitApp = 1

		CASE "save pak"
			IF sysApp = "SPRITE" THEN sysTask = "SAVE PAK"
			handleFile

		CASE "new pal"
			IF sysApp = "SPRITE" THEN sysTask = "NEW PAL"
			handleFile

		CASE "list pal"
			IF sysApp = "SPRITE" THEN sysTask = "LIST PAL"
			handleFile
			
		CASE "load pal"
			IF sysApp = "SPRITE" THEN sysTask = "LOAD PAL"
			handleFile

		CASE "save pal"
			IF sysApp = "SPRITE" THEN sysTask = "SAVE PAL"
			handleFile

		CASE "list 256"
			IF sysApp = "SPRITE" THEN
				FOR i = 0 TO 128
					COLOR i: PRINT i;
				NEXT
				SLEEP
				FOR i = 128 TO 255
					COLOR i: PRINT i;
				NEXT
				SLEEP
			END IF

		CASE "color"
			IF sysApp = "SPRITE" THEN
				LOCATE 23, 2: INPUT "COLOR:", num
				tool.clr = num
			END IF

		CASE "load pak"
			sysTask = "LOAD PAK"
			handleFile

		CASE "new"
			IF sysApp = "MAPPING" THEN
				sysTask = "NEW MAP"
			ELSE
				sysTask = "NEW GFX"
			END IF
			handleFile

		CASE "load"
			IF sysApp = "MAPPING" THEN
				sysTask = "LOAD MAP"
			ELSE
				sysTask = "LOAD GFX"
			END IF
			handleFile

		CASE "save"
			IF sysApp = "MAPPING" THEN
				sysTask = "SAVE MAP"
			ELSE
				sysTask = "SAVE GFX"
			END IF
			handleFile

		CASE "repl"
			IF sysApp = "MAPPING" THEN
				sysTask = "REPL TILE"
				handleMap
			ELSEIF sysApp = "SPRITE" THEN
				sysTask = "REPL PIXEL"
				handleSprite
			END IF

		CASE "add rnd"
			IF sysApp = "MAPPING" THEN sysTask = "RANDOM"
			handleMap

		CASE "fill"
			IF sysApp = "MAPPING" THEN sysTask = "FILL"
			handleMap

		CASE "dir"
			FILES "\*.*"
			SLEEP

	END SELECT
	console = ""
	CLS
	updScreen

END SUB

SUB updScreen

	COLOR modeColor
	LINE (0, 0)-(320, 16), modeColor, BF
	LOCATE 2, 2: PRINT "FILE:"; UCASE$(filen)
	LOCATE 2, 29: PRINT modeText

	IF sysApp = "MAPPING" THEN
		LINE (176, 24)-(312, 160), 0, BF
		FOR x = 22 TO 38
			FOR y = 3 TO 19
				sprType = gridMap(x + cam.x, y + cam.y)
				IF sprType = 0 THEN PUT (x * 8, y * 8), sprIndex(420)
			NEXT
		NEXT
		LOCATE 23, 30: COLOR modeColor: PRINT "x"; cam.x; "y"; cam.y
		PUT (240, 88), toolSpr(tool.spr)
	END IF

	
	IF sysApp = "SPRITE" THEN
		LINE (24, 24)-(88, 88), 0, BF
		FOR x = 1 TO 8
			FOR y = 1 TO 8
				index = barGrid(x, y)
				IF index THEN PUT ((x + 3) * 8, (y + 3) * 8), frameIndex(42 * index)
			NEXT
		NEXT
	
		FOR x = 8 TO 15
			FOR y = 16 TO 23
				IF grid(x - 8, y - 16) THEN
					LOCATE x, y: COLOR grid(x - 8, y - 16): PRINT CHR$(254)
				ELSE
					LOCATE x, y: COLOR gridColor: PRINT CHR$(254)
				END IF
			NEXT
		NEXT
		
		FOR x = 0 TO 8
			FOR y = 0 TO 8
				PSET (x + 110, y + 80), grid(y, x)
			NEXT
		NEXT

		FOR i = 1 TO 16
			LOCATE 18, i + 12: COLOR colorPal(i): PRINT CHR$(254)
		NEXT
		
		IF setMode = "DRAW" THEN LOCATE tool.x, tool.y: COLOR tool.clr: PRINT CHR$(254)
		
		' DEBUG_
		LOCATE 23, 28: COLOR consoleColor: PRINT tool.barx; tool.bary; tool.barx * tool.bary
	END IF
	
END SUB

SUB userInput

	k = INP(96)
	SELECT CASE k
	CASE 41
	' open console (~)
		sysConsole

	CASE 59
	' spr mode (F1)
		sysApp = "SPRITE"
		setMode = "DRAW"
		modeText = "MODE:SPRITE"
		modeColor = 52
		setToolPos
		CLS

	CASE 60
	' map mode (F2)
		sysApp = "MAPPING"
		setMode = "MAP GRID"
		modeText = "MODE:MAPPER"
		modeColor = 39
		setToolPos
		CLS
		
	CASE 15
	' (TAB)
		IF sysApp = "SPRITE" THEN setMode = "SPRITE BAR"
		setToolPos

	CASE 82
	' pos tool to pick a color in spr mode (INS)
		IF sysApp = "SPRITE" THEN setMode = "PICK COLOR"
		setToolPos

	CASE 73
	' pos tool for map and draw mode, depends on app state (PAGE UP)
		IF sysApp = "MAPPING" THEN setMode = "MAP GRID"
		IF sysApp = "SPRITE" THEN setMode = "DRAW"
		setToolPos
		
	CASE 81
	' store current spr on draw grid on screen (PAGE DOWN)
		IF sysApp = "SPRITE" AND setMode = "SPRITE BAR" THEN sysTask = "PLACE"
		handleSprite
	
	CASE 79
	' rem sprite/pixel on tool pos (END)
		IF sysApp = "SPRITE" AND setMode = "DRAW" THEN sysTask = "DEL PIXEL"
		handleSprite

	CASE 17
	' move tool (W)
		IF sysApp = "MAPPING" AND cam.y > 0 THEN cam.y = cam.y - 1
		IF sysApp = "SPRITE" THEN
' *** draw mode x and y are flipped??

			IF setMode = "DRAW" AND tool.x > 8 AND tool.x < 16 THEN tool.x = tool.x - 1
			IF setMode = "SPRITE BAR" AND tool.bary > 0 THEN tool.bary = tool.bary - 1
		END IF

	CASE 31
	' move tool (S)
		IF sysApp = "MAPPING" AND cam.y < 80 THEN cam.y = cam.y + 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND tool.x > 7 AND tool.x < 15 THEN tool.x = tool.x + 1
			IF setMode = "SPRITE BAR" AND tool.bary < 8 THEN tool.bary = tool.bary + 1
		END IF

	CASE 30
	' move tool (A)
		IF sysApp = "MAPPING" AND cam.x > 0 THEN cam.x = cam.x - 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND tool.y > 16 AND tool.y < 24 THEN tool.y = tool.y - 1
			IF setMode = "PICK COLOR" AND tool.y > 12 AND tool.y < 29 THEN tool.y = tool.y - 1
			IF setMode = "SPRITE BAR" AND tool.barx > 0 THEN tool.barx = tool.barx - 1
		END IF

	CASE 32
	' move tool (D)
		IF sysApp = "MAPPING" AND cam.x < 80 THEN cam.x = cam.x + 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND tool.y > 15 AND tool.y < 23 THEN tool.y = tool.y + 1
			IF setMode = "PICK COLOR" AND tool.y > 11 AND tool.y < 28 THEN tool.y = tool.y + 1
			IF setMode = "SPRITE BAR" AND tool.barx < 8 THEN tool.barx = tool.barx + 1
		END IF
											 
	CASE 71
	' pick/place objs in editor (HOME)
		IF sysApp = "MAPPING" THEN gridMap(cam.x + tool.x, cam.y + tool.y) = 420     ' tool.picked  
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" THEN grid(tool.x - 8, tool.y - 16) = tool.clr
			IF setMode = "PICK COLOR" THEN
				FOR y = 13 TO 29
					IF tool.x = 18 AND tool.y = y THEN tool.clr = colorPal(y - 12)
				NEXT
			END IF
		END IF
	END SELECT

END SUB

