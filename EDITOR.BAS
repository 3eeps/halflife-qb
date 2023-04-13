DEFINT A-Z
DECLARE SUB popMenu ()
DECLARE SUB startMenu ()
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
DECLARE SUB spriteMenu ()
DECLARE SUB setToolPos ()

RANDOMIZE TIMER
ON ERROR GOTO BADFILE:

TYPE obj
	x AS INTEGER
	y AS INTEGER
	picked AS INTEGER
	clr AS INTEGER
	spr AS INTEGER
END TYPE

DIM SHARED tool AS obj
DIM SHARED cam  AS obj

DIM SHARED loadMap
DIM SHARED gridx
DIM SHARED gridy
DIM SHARED grid(8, 8)
DIM SHARED gridMap(120, 120)

DIM SHARED gSize
DIM SHARED gmSize

DIM SHARED pakName AS STRING

DIM SHARED filen AS STRING
DIM SHARED src(8, 8)

DIM SHARED sprNameList(152) AS STRING
DIM SHARED sprTypeData(152)

DIM SHARED console AS STRING
DIM SHARED sysTask AS STRING
DIM SHARED sysApp AS STRING
DIM SHARED setMode AS STRING

DIM SHARED sprIndex(6800)
DIM SHARED frameIndex(6800)

DIM SHARED toolSpr(84)
DIM SHARED colorPal(16)
DIM SHARED exitApp
DIM SHARED SprNum
DIM SHARED tileMenu
DIM SHARED sprGrid
DIM SHARED objTypes

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
	SELECT CASE sysTask

		CASE "NEW GFX"

			FOR x = 0 TO gSize
				FOR y = 0 TO gSize
					grid(x, y) = 0
				NEXT
			NEXT
			filen = ""

		CASE "SAVE GFX"

			LOCATE 23, 2: COLOR 44: INPUT "GFX NAME:", filen
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

			LOCATE 23, 2: COLOR 44: INPUT "LOAD GFX:", filen
			OPEN filen FOR INPUT AS #1
			FOR x = 0 TO 8
				FOR y = 0 TO 8
					INPUT #1, grid(x, y)
				NEXT
			NEXT
			CLOSE #1
			updScreen

'============================= PAK

		CASE "SAVE PAK"
		' create a pak file from user made grid

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

			FOR i = 1 TO SprNum
				PRINT #1, sprNameList(i)
			NEXT

			FOR i = 1 TO SprNum
				PRINT #1, sprTypeData(i)
			NEXT

			CLOSE #1
			filen = ""

		CASE "LOAD PAK"

			LOCATE 23, 2: COLOR 44: INPUT "LOAD PAK: ", filen
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

' ========================== PAL

		CASE "NEW PAL"

			FOR i = 1 TO 16
				LOCATE 23, 2: COLOR 44: INPUT "COLOR:", colorPal(i)
			NEXT

		CASE "LIST PAL"

			FOR i = 1 TO 16
				 COLOR colorPal(i): PRINT colorPal(i)
			NEXT
			SLEEP

		CASE "SAVE PAL"

			LOCATE 23, 2: COLOR 44: INPUT "PAL NAME:", filen
			OPEN filen FOR OUTPUT AS #1
			FOR i = 1 TO 16
				PRINT #1, colorPal(i)
			NEXT
			CLOSE #1
			filen = ""

		CASE "LOAD PAL"

			LOCATE 23, 2: COLOR 44: INPUT "LOAD PAL:", filen
			OPEN filen FOR INPUT AS #1
			FOR i = 1 TO 16
				INPUT #1, colorPal(i)
			NEXT
			CLOSE #1
			filen = ""

' ======================== MAP

		CASE "NEW MAP"

			FOR x = 0 TO gmSize
				FOR y = 0 TO gmSize
					gridMap(x, y) = 0
				NEXT
			NEXT

		CASE "SAVE MAP"

			COLOR 44: LOCATE 23, 2: INPUT "MAP NAME: ", filen
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

			COLOR 44: LOCATE 23, 2: INPUT "LOAD MAP: ", filen
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

		CASE "DEL TILE"

' ***this crashes editor
			'grid(cam.x + (tool.x + 8), cam.y + (tool.y - 8)) = 0

		CASE "REPL TILE"

			LOCATE 23, 2: INPUT "REPLACE:", sprite0
			LOCATE 23, 2: INPUT "WITH:", sprite1

			FOR x = 0 TO gSize
				FOR y = 0 TO gSize
					IF gridMap(x + 20, y + 12) = sprite0 * 42 THEN gridMap(x + 20, y + 12) = sprite1 * 42
				NEXT
			NEXT

		CASE "FILL"

			LOCATE 23, 2: INPUT "COORDx:", coord0
			LOCATE 23, 2: INPUT "COORDy:", coord1
			LOCATE 23, 2: INPUT "FILL SPR#:", getSpr

			FOR x = 0 TO coord0
				FOR y = 0 TO coord1
					gridMap(x + 20, y + 12) = getSpr * 42
				NEXT
			NEXT

		CASE "RAND"

			LOCATE 23, 2: INPUT "COORDx:", coord0
			LOCATE 23, 2: INPUT "COORDy:", coord1
			LOCATE 23, 2: INPUT "#1-255:", noiseNum
			LOCATE 23, 2: INPUT "SPR#:", getSpr

			FOR x = 0 TO coord0
				FOR y = 0 TO coord1
					IF INT(RND * noiseNum) + 1 = 1 THEN gridMap(x + 20, y + 12) = getSpr * 42
				NEXT
			NEXT

	END SELECT
	sysTask = ""

END SUB

SUB handleSprite

	WHILE INKEY$ <> "": WEND
	SELECT CASE sysTask

		CASE "DEL PIXEL"

			grid(tool.x - 8, tool.y - 16) = 0

		CASE "IREM"

			tool.clr = grid(tool.x - 8, tool.y - 16)

		CASE "REPL PIXEL"

			LOCATE 23, 2: COLOR 44: INPUT "REPLACE:", color0
			LOCATE 23, 2: INPUT "WITH:", color1

			FOR x = 0 TO gSize
				FOR y = 0 TO gSize
					IF grid(x, y) = color0 THEN grid(x, y) = color1
				NEXT
			NEXT

		CASE "DROP"

			index = 0
			FOR i = 0 TO 3
				IF tool.x = 18 + i THEN
					index = (index + i) * 38
					LOCATE 23, 2: COLOR 52: INPUT "NAME:", sprNameList(tool.y + index)
					LOCATE 23, 2: INPUT "TYPE:", sprTypeData(tool.y + index)
				END IF
			NEXT

			PUT (8, 144), frameIndex, PSET
			FOR x = 0 TO 8
				FOR y = 0 TO 8
					PSET (x + (tool.y * 8), y + (tool.x * 8)), grid(y, x)
				NEXT
			NEXT
			GET (8, 144)-(312, 176), frameIndex

	END SELECT
	sysTask = ""

END SUB

SUB initEditor

	SCREEN 13

	sysApp = "SPRITE"
	setMode = "DRAW"
	tool.x = 8
	tool.y = 16

	objTypes = 10

	tool.spr = 42
	tool.clr = 2

	gmSize = 100
	gSize = 7

	cam.x = 12
	cam.y = 20

	SprNum = 152

	FOR i = 1 TO 16
		colorPal(i) = i
	NEXT

	GET (8, 144)-(312, 176), frameIndex

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

SUB popMenu

	IF sysApp = "MAPPING" AND tileMenu = 1 THEN

		LOCATE 16, 3: COLOR 44: PRINT UCASE$(pakName);
		index = 0
		FOR i = 0 TO objTypes
			IF tool.y = 16 + i THEN
				index = (index + i) * 38
				LOCATE 16, 28: COLOR 48: PRINT sprNameList(tool.x + index); "|"; sprTypeData(tool.x + index)
			END IF
		NEXT

		index = 0
		FOR b = 1 TO 4
			FOR i = 1 TO 38
				index = index + 1
				PUT (8 * i, 120 + (b * 8)), sprIndex(42 * index), PSET
			NEXT
		NEXT

	ELSEIF sysApp = "SPRITE" AND sprGrid = 1 THEN

		index = 0
		FOR i = 0 TO objTypes
			IF tool.x = 18 + i THEN
					index = (index + i) * 38
			LOCATE 16, 28: COLOR 48: PRINT sprNameList(tool.y + index); "|"; sprTypeData(tool.y + index)
			END IF
		NEXT
		PUT (tool.y * 8, tool.x * 8), toolSpr(tool.spr)
		IF setMode = "DRAW" THEN LOCATE tool.x, tool.y: COLOR 44: PRINT CHR$(249)
	END IF
					 
END SUB

SUB setToolPos

	SELECT CASE setMode

		CASE "DRAW"

			tool.x = 8
			tool.y = 16

		CASE "MAP GRID"

			tool.x = 20
			tool.y = 12

		CASE "TILE MENU"

			tool.x = 12
			tool.y = 16

		CASE "GRID"

			tool.x = 21
			tool.y = 2

		CASE "PICK"

			tool.x = 18
			tool.y = 12

	END SELECT

END SUB

SUB sysConsole

	WHILE INKEY$ <> "": WEND
	LOCATE 23, 8: COLOR 17: PRINT "'HELP' FOR COMMANDS"
	LOCATE 23, 2: COLOR 44: INPUT ">>>: ", console
	SELECT CASE console

		CASE "mem"

			LOCATE 23, 2: COLOR 48: PRINT "USED:"; 354418 - FRE(-1); "/"; 354418
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
				WHILE INKEY$ <> "": WEND
				LOCATE 23, 2: COLOR 8: INPUT "COLOR:", num
				tool.clr = num
			END IF

		CASE "load pak"

			sysTask = "LOAD PAK"
			handleFile

		CASE "new"

			IF sysApp = "MAPPING" THEN sysTask = "NEW MAP"
			IF sysApp = "SPRITE" THEN sysTask = "NEW GFX"
			handleFile

		CASE "load"

			IF sysApp = "MAPPING" THEN sysTask = "LOAD MAP"
			IF sysApp = "SPRITE" THEN sysTask = "LOAD GFX"
			handleFile

		CASE "save"

			IF sysApp = "MAPPING" THEN sysTask = "SAVE MAP"
			IF sysApp = "SPRITE" THEN sysTask = "SAVE GFX"
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

	IF sysApp = "MAPPING" THEN
	 
		LINE (224, 0)-(320, 200), 39, BF
		LINE (224, 16)-(312, 192), 0, BF

		FOR x = 1 TO 32
			gridx = x + cam.x
			FOR y = 2 TO 21
				gridy = y + cam.y
				sprType = gridMap(gridx, gridy)
				IF sprType THEN PUT (x * 8, y * 8), sprIndex(sprType)
			NEXT
		NEXT

		PUT (tool.x * 8, tool.y * 8), toolSpr(tool.spr)

		LOCATE 2, 2: COLOR 44: PRINT "FILE:"; UCASE$(filen)
		LOCATE 2, 29: COLOR 39: PRINT "MODE:LEVEL "
		LOCATE 23, 30: COLOR 39: PRINT "x"; cam.x; "y"; cam.y
	END IF

	IF sysApp = "SPRITE" THEN

		LINE (224, 0)-(320, 200), 52, BF
		LINE (224, 16)-(312, 192), 0, BF

		FOR x = 8 TO 15
			FOR y = 16 TO 23
				IF grid(x - 8, y - 16) THEN
					LOCATE x, y: COLOR grid(x - 8, y - 16): PRINT CHR$(254)
				ELSE
					LOCATE x, y: COLOR 20: PRINT CHR$(254)
				END IF
			NEXT
		NEXT

		LOCATE 23, 30: COLOR 52: PRINT "x"; tool.x; "y"; tool.y

		FOR x = 0 TO 8
			FOR y = 0 TO 8
				PSET (x + 110, y + 80), grid(y, x)
			NEXT
		NEXT

		LOCATE tool.x, tool.y: COLOR tool.clr: PRINT CHR$(254)

		FOR i = 1 TO 16
			LOCATE 18, i + 12: COLOR colorPal(i): PRINT CHR$(254)
		NEXT

		LOCATE 2, 2: COLOR 44: PRINT "FILE:"; UCASE$(filen)
		LOCATE 2, 29: COLOR 52: PRINT "MODE:SPRITE"
	END IF	
	popMenu

END SUB

SUB userInput

	k = INP(96)
	SELECT CASE k

	CASE 41
	' open console (~)

		sysConsole

	CASE 59
	' set app to sprite mode (F1)

		sysApp = "SPRITE"
		setMode = "DRAW"
		setToolPos
		CLS

	CASE 60
	' set app to map mode (F2)

		sysApp = "MAPPING"
		setMode = "MAP GRID"
		setToolPos
		CLS

	CASE 18
	' open sprite menu in map mode (E)

		loadMap = 1

		IF sysApp = "MAPPING" AND pakName <> "" THEN
			IF tileMenu = 1 THEN
				tileMenu = 0
				setMode = "MAP GRID"
				setToolPos
			ELSE
				tileMenu = 1
				setMode = "TILE MENU"
				setToolPos
			END IF
		END IF

	CASE 15
	' open sprite grid in sprite mode (TAB)

		IF sysApp = "SPRITE" THEN
			IF sprGrid = 1 THEN
				sprGrid = 0
				setMode = "DRAW"
				setToolPos
			ELSE
				sprGrid = 1
				setMode = "GRID"
				setToolPos
				PUT (8, 144), frameIndex
			END IF
		END IF

	CASE 81
	' place current open sprite onto sprite grid (PAGE DOWN)

		IF sysApp = "SPRITE" AND setMode = "GRID" THEN
			sysTask = "DROP"
			handleSprite
		END IF

	CASE 82
	' position tool to pick a color in sprite mode (INS)

		IF sysApp = "SPRITE" AND setMode <> "GRID" THEN
			setMode = "PICK"
			setToolPos
		END IF

	CASE 73
	' position tool for map and draw mode, depends on app state (PAGE UP)

		IF sysApp = "MAPPING" THEN
			setMode = "MAP GRID"
			setToolPos
		ELSEIF sysApp = "SPRITE" THEN
			setMode = "DRAW"
			setToolPos
		END IF

	CASE 79
	' remove sprite/pixel on tool position, depends on app state (END)

		IF sysApp = "MAPPING" AND setMode = "MAP GRID" THEN
			sysTask = "DEL TILE"
			handleMap

		ELSEIF sysApp = "SPRITE" AND setMode = "DRAW" THEN
			sysTask = "DEL PIXEL"
			handleSprite
		END IF

	CASE 17
	' move tool (W)

		IF sysApp = "MAPPING" THEN
			IF setMode = "MAP GRID" AND cam.y > 0 THEN cam.y = cam.y - 1
			IF setMode = "TILE MENU" AND tool.y > 16 THEN tool.y = tool.y - 1

		ELSEIF sysApp = "SPRITE" THEN
			IF setMode = "GRID" AND tool.x > 18 THEN tool.x = tool.x - 1
			IF setMode = "DRAW" AND tool.x > 8 AND tool.x < 16 THEN tool.x = tool.x - 1
		END IF

	CASE 31
	' move tool (S)

		IF sysApp = "MAPPING" THEN
			IF setMode = "MAP GRID" AND cam.y < 80 THEN cam.y = cam.y + 1
			IF setMode = "TILE MENU" AND tool.y < 19 THEN tool.y = tool.y + 1

		ELSEIF sysApp = "SPRITE" THEN
			IF setMode = "GRID" AND tool.x < 21 THEN tool.x = tool.x + 1
			IF setMode = "DRAW" AND tool.x > 7 AND tool.x < 15 THEN tool.x = tool.x + 1
		END IF

	CASE 30
	' move tool (A)

		IF sysApp = "MAPPING" THEN
			IF setMode = "MAP GRID" AND cam.x > 0 THEN cam.x = cam.x - 1
			IF setMode = "TILE MENU" AND tool.x > 1 THEN tool.x = tool.x - 1

		ELSEIF sysApp = "SPRITE" THEN
			IF setMode = "GRID" AND tool.y > 1 THEN tool.y = tool.y - 1
			IF setMode = "DRAW" AND tool.y > 16 AND tool.y < 24 THEN tool.y = tool.y - 1
			IF setMode = "PICK" AND tool.y > 12 AND tool.y < 29 THEN tool.y = tool.y - 1
		END IF

	CASE 32
	' move tool (D)

		IF sysApp = "MAPPING" THEN
			IF setMode = "MAP GRID" AND cam.x < 80 THEN cam.x = cam.x + 1
			IF setMode = "TILE MENU" AND tool.x < 38 THEN tool.x = tool.x + 1
			 

		ELSEIF sysApp = "SPRITE" THEN
			IF setMode = "GRID" AND tool.y < 38 THEN tool.y = tool.y + 1
			IF setMode = "DRAW" AND tool.y > 15 AND tool.y < 23 THEN tool.y = tool.y + 1
			IF setMode = "PICK" AND tool.y > 11 AND tool.y < 28 THEN tool.y = tool.y + 1
		END IF
											 
	CASE 71
	' main key to place and select colors and sprites (HOME)
		
		IF sysApp = "MAPPING" THEN
			IF setMode = "TILE MENU" AND tileMenu = 1 THEN
			
				index = 0
				FOR i = 0 TO 3
					IF tool.y = 16 + i THEN
						index = 38 * i
						tool.picked = (tool.x + index) * 42
					END IF
				NEXT
			END IF

			IF setMode = "MAP GRID" THEN gridMap(cam.x + tool.x, cam.y + tool.y) = tool.picked

		ELSEIF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" THEN grid(tool.x - 8, tool.y - 16) = tool.clr
			IF setMode = "PICK" THEN
				FOR y = 13 TO 29
					IF tool.x = 18 AND tool.y = y THEN tool.clr = colorPal(y - 12)
				NEXT
			END IF
		END IF

	END SELECT

END SUB

