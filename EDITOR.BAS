DEFINT A-Z
DECLARE SUB userInput ()
DECLARE SUB handleSprite ()
DECLARE SUB handleGame ()
DECLARE SUB sysConsole ()
DECLARE SUB handleFile ()
DECLARE SUB updScreen ()
DECLARE SUB initEditor ()
DECLARE SUB setToolPos ()
DECLARE SUB updActors ()
RANDOMIZE TIMER
ON ERROR GOTO BADFILE:

TYPE obj
	x AS INTEGER
	y AS INTEGER
	barx AS INTEGER
	bary AS INTEGER
	picked AS INTEGER
	clr AS INTEGER
	spr AS INTEGER
	hp AS INTEGER
	direction AS INTEGER
	steps AS INTEGER
	energy AS INTEGER
	state AS STRING * 4
	target AS INTEGER
	ftile AS INTEGER
END TYPE

DIM SHARED win AS obj
DIM SHARED user AS obj
DIM SHARED actor(32) AS obj
DIM SHARED vdistObjList(3) AS obj
DIM SHARED tool AS obj
DIM SHARED cam  AS obj

DIM SHARED maxTarget
DIM SHARED minDist
DIM SHARED targetType
DIM SHARED vdist
DIM SHARED maxAi

DIM SHARED grid(8, 8)
DIM SHARED gridMap(100, 100)
DIM SHARED gSize
DIM SHARED gmSize

DIM SHARED gridColor
DIM SHARED consoleColor
DIM SHARED modeColor

DIM SHARED frameIndex(3200)
DIM SHARED colorPal(16)
DIM SHARED src(8, 8)
DIM SHARED toolSpr(84)
DIM SHARED barGrid(8, 8)
DIM SHARED sideBarIndex(8, 8)

DIM SHARED console AS STRING
DIM SHARED sysTask AS STRING
DIM SHARED sysApp AS STRING
DIM SHARED setMode AS STRING
DIM SHARED modeText AS STRING
DIM SHARED exitApp

DIM SHARED filen AS STRING
DIM SHARED SprNum
DIM SHARED pakName AS STRING

initEditor
BADFILE:
	sysTask = ""
	filen = ""
DO
	userInput
	updActors
	updScreen
	SLEEP
LOOP UNTIL exitApp = 1
WHILE INKEY$ <> "": WEND
SYSTEM

SUB handleFile

	WHILE INKEY$ <> "": WEND
	COLOR consoleColor
	SELECT CASE sysTask
		CASE "NEW"
			IF sysApp = "SPRITE" THEN
				FOR x = 0 TO gSize
					FOR y = 0 TO gSize
						grid(x, y) = 0
					NEXT
				NEXT
			ELSE
				FOR x = 0 TO gmSize
					FOR y = 0 TO gmSize
						gridMap(x, y) = 0
					NEXT
				NEXT
			END IF
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
			LOCATE 23, 2: INPUT "PAK NAME:", filen
			OPEN filen FOR OUTPUT AS #1
			fileLen = (SprNum * 44)
			FOR i = 1 TO fileLen
				PRINT #1, frameIndex(i)
			NEXT
			
			'FOR x = 1 TO 8
			' FOR y = 1 TO 8
			'   sprPos = barGrid(x, y)
			'   sprPos = sprPos * 44
			'   PRINT #1, sprPos
			' NEXT
			'NEXT
			CLOSE #1
			filen = ""

		CASE "LOAD PAK"
			LOCATE 23, 2: INPUT "LOAD PAK: ", filen
			OPEN filen FOR INPUT AS #1
			fileLen = (SprNum * 44)
			FOR i = 1 TO fileLen
				INPUT #1, frameIndex(i)
			NEXT
			
			FOR x = 1 TO 8
				FOR y = 1 TO 8
					INPUT #1, barGrid(x, y)
				NEXT
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

SUB handleGame

	FOR i = 1 TO maxAi
		actor(i).x = 30
		actor(i).y = 45
		actor(i).hp = 35
		actor(i).state = "wand"
		actor(i).energy = 45
		actor(i).target = -1
		actor(i).steps = 0
		actor(i).direction = RND * 4
		actor(i).spr = 44 * 1
		actor(i).ftile = gridMap(actor(i).x, actor(i).y)
		gridMap(actor(i).x, actor(i).y) = actor(i).spr
		
		FOR x = 0 TO gmSize
			FOR y = 0 TO gmSize
				index = gridMap(x, y)
				index = index * 44
				gridMap(x, y) = index
			NEXT
		NEXT
		
	NEXT
	
END SUB

SUB handleSprite

	COLOR consoleColor
	WHILE INKEY$ <> "": WEND
	SELECT CASE sysTask
		CASE "DEL PIXEL"
			grid(tool.x - 8, tool.y - 16) = 0
			
		CASE "PLACE"
			index = sideBarIndex(tool.barx, tool.bary)
			barGrid(tool.barx, tool.bary) = index
			GET (110, 80)-(118, 88), frameIndex(44 * index)
			
	END SELECT
	sysTask = ""

END SUB

SUB initEditor

	SCREEN 13
	consoleColor = 43
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
	
	maxTarget = 4
	minDist = 10000
	maxAi = 1
	vdist = 5
	user.x = 20
	user.y = 12
	user.spr = 44 * 1
	win.x = 30 - user.x
	win.y = 49 - user.y
	
	FOR x = 1 TO 8
		FOR y = 1 TO 8
			index = index + 1
			sideBarIndex(x, y) = index
		NEXT
	NEXT

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

		CASE "PICK COLOR"
			tool.x = 18
			tool.y = 13
			
		CASE "SPRITE BAR"
			tool.barx = 1
			tool.bary = 1
			
		CASE "MAP GRID"
			tool.x = 30
			tool.y = 11
	END SELECT

END SUB

SUB sysConsole

	LOCATE 23, 8: COLOR 17: PRINT "'HELP' FOR COMMANDS"
	COLOR consoleColor
	
	WHILE INKEY$ <> "": WEND
	LOCATE 23, 2: INPUT ">>>: ", console
	WHILE INKEY$ <> "": WEND
	SELECT CASE console
		CASE "exit"
			exitApp = 1

		CASE "save pak"
			sysTask = "SAVE PAK"
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
			sysTask = "NEW"
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

		CASE "dir"
			FILES "\*.*"
			SLEEP

	END SELECT
	console = ""
	CLS
	updScreen

END SUB

SUB updActors

	IF sysApp = "GAME" THEN
	FOR i = 1 TO maxAi
	
		IF actor(i).hp <= 0 THEN
			actor(i).state = "dead"
			GOTO skipAi
		END IF

		gridMap(actor(i).x, actor(i).y) = actor(i).ftile
		currPosx = actor(i).x
		currPosy = actor(i).x
	
		actor(i).energy = actor(i).energy - RND * 2
		IF actor(i).energy < 1 THEN actor(i).hp = actor(i).hp - RND * 2

		IF actor(i).energy < 25 THEN
		actor(i).target = 420
		actor(i).state = "find"
		ELSE
		actor(i).state = "wand"
		END IF
				 
		IF actor(i).target = -1 THEN actor(i).state = "wand"
	
		SELECT CASE actor(i).state

			CASE "wand"
		
				IF actor(i).direction = 0 THEN
					dx = 1: dy = 0
				ELSEIF actor(i).direction = 1 THEN
					dx = -1: dy = 0
				ELSEIF actor(i).direction = 2 THEN
					dx = 0: dy = -1
				ELSEIF actor(i).direction = 3 THEN
					dx = 0: dy = 1
				END IF
				newx = actor(i).x + dx
				newy = actor(i).y + dy
		
				IF gridMap(newx, newy) = 0 AND actor(i).steps < 4 THEN
					actor(i).x = newx
					actor(i).y = newy
					actor(i).steps = actor(i).steps + 1
				ELSE
					actor(i).steps = 0
					rand = RND * 4
					actor(i).direction = rand
				END IF

			CASE "find"
			
				FOR x = actor(i).x - vdist TO actor(i).x + vdist
					FOR y = actor(i).y - vdist TO actor(i).y + vdist
						IF gridMap(x, y) = actor(i).target THEN
							dx = actor(i).x - x
							dy = actor(i).y - y
							IF targetCount < maxTarget THEN
								targetCount = targetCount + 1
								vdistObjList(targetCount).x = dx
								vdistObjList(targetCount).y = dy
							END IF
						END IF
					NEXT
				NEXT
	
				IF targetCount > 0 THEN
					FOR j = 1 TO targetCount
						dx = vdistObjList(j).x - actor(i).x
						dy = vdistObjList(j).y - actor(i).y
						distSqr = dx ^ 2 + dy ^ 2
						IF distSqr < minDist ^ 2 THEN
							closestTarget = j
							minDist = SQR(distSqr)
						END IF
					NEXT
					dx = vdistObjList(closestTarget).x
					dy = vdistObjList(closestTarget).y
					ERASE vdistObjList
				END IF
			
				IF dx < 0 AND dy < 0 THEN
					rand = RND * 2
					IF rand = 1 THEN
						actor(i).x = actor(i).x + 1
					ELSE
						actor(i).y = actor(i).y + 1
					END IF
				ELSEIF dx > 0 AND dy > 0 THEN
					rand = RND * 2
					IF rand = 1 THEN
						actor(i).x = actor(i).x - 1
					ELSE
						actor(i).y = actor(i).y - 1
					END IF
				ELSEIF dx < 0 AND dy > 0 THEN
					rand = RND * 2
					IF rand = 1 THEN
						actor(i).x = actor(i).x + 1
					ELSE
						actor(i).y = actor(i).y - 1
					END IF
				ELSEIF dx > 0 AND dy < 0 THEN
					rand = RND * 2
					IF rand = 1 THEN
						actor(i).x = actor(i).x - 1
					ELSE
						actor(i).y = actor(i).y + 1
					END IF
				ELSEIF dx = 0 AND dy < 0 THEN actor(i).y = actor(i).y + 1
				ELSEIF dx = 0 AND dy > 0 THEN actor(i).y = actor(i).y - 1
				ELSEIF dx < 0 AND dy = 0 THEN actor(i).x = actor(i).x + 1
				ELSEIF dx > 0 AND dy = 0 THEN actor(i).x = actor(i).x - 1
				END IF
			
				IF gridMap(actor(i).x + 1, actor(i).y) = actor(i).target THEN
					foundTarget = 1
				ELSEIF gridMap(actor(i).x - 1, actor(i).y) = actor(i).target THEN
					foundTarget = 1
				ELSEIF gridMap(actor(i).x, actor(i).y + 1) = actor(i).target THEN
					foundTarget = 1
				ELSEIF gridMap(actor(i).x, actor(i).y - 1) = actor(i).target THEN
					foundTarget = 1
				END IF
		
				IF foundTarget THEN
					actor(i).x = currPosx
					actor(i).x = currPosy
					actor(i).target = -1
					gridMap(actor(i).x, actor(i).y) = 0
					actor(i).energy = actor(i).energy + 25
				END IF
		END SELECT
		actor(i).ftile = gridMap(actor(i).x, actor(i).y)
		gridMap(actor(i).x, actor(i).y) = actor(i).spr
	
skipAi:
	NEXT
END IF

END SUB

SUB updScreen

	COLOR modeColor
	LINE (0, 0)-(320, 16), modeColor, BF
	LOCATE 2, 2: PRINT "FILE:"; UCASE$(filen)
	LOCATE 2, 29: PRINT modeText
	WAIT &H3DA, 8
	
	IF sysApp = "GAME" THEN
		LINE (176, 24)-(312, 160), 0, BF
		FOR x = 22 TO 38
			FOR y = 3 TO 19
				sprIndex = gridMap(x + win.x, y + win.y)
				IF sprIndex THEN PUT (x * 8, y * 8), frameIndex(sprIndex)
			NEXT
		NEXT
		LOCATE 23, 30: COLOR modeColor: PRINT "x"; win.x; "y"; win.y
		PUT (240, 88), frameIndex(user.spr)
		
		'DEBUG_
		LOCATE 17: COLOR 39: PRINT "energy:"; actor(1).energy
		LOCATE 18: COLOR 44: PRINT "hp:"; actor(1).hp
		LOCATE 19: COLOR 8: PRINT "target:"; actor(1).target
		LOCATE 20: COLOR 48: PRINT "state:"; actor(1).state
	ELSE
		LINE (24, 24)-(88, 88), 0, BF
		FOR x = 1 TO 8
			FOR y = 1 TO 8
				sprIndex = barGrid(x, y)
				IF sprIndex THEN PUT ((x + 2) * 8, (y + 2) * 8), frameIndex(44 * sprIndex)
			NEXT
		NEXT
		IF setMode = "SPRITE BAR" THEN PUT ((tool.barx + 2) * 8, (tool.bary + 2) * 8), toolSpr(42)
	END IF

	IF sysApp = "MAPPING" THEN
		LINE (176, 24)-(312, 160), 0, BF
		FOR x = 22 TO 38
			FOR y = 3 TO 19
				sprIndex = gridMap(x + cam.x, y + cam.y)
				IF sprIndex THEN PUT (x * 8, y * 8), frameIndex(44 * sprIndex)
			NEXT
		NEXT
		LOCATE 23, 30: COLOR modeColor: PRINT "x"; cam.x; "y"; cam.y
		PUT (240, 88), toolSpr(tool.spr)
	END IF

	IF sysApp = "SPRITE" THEN
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
		IF setMode = "PICK COLOR" THEN LOCATE tool.x, tool.y: COLOR consoleColor: PRINT CHR$(248)
	
END SUB

SUB userInput

	IF sysApp = "GAME" AND setMode = "PLAY" THEN
		gridMap(win.x + user.x, win.y + user.y) = user.ftile
		SLEEP
	END IF

	k = INP(96)
	SELECT CASE k
	CASE 17
	' move tool (W)
		IF setMode = "PLAY" AND win.y > 0 THEN win.y = win.y - 1
		IF setMode = "SPRITE BAR" AND tool.bary - 1 > 0 THEN tool.bary = tool.bary - 1
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" AND cam.y > 0 THEN cam.y = cam.y - 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND tool.x > 8 AND tool.x < 16 THEN tool.x = tool.x - 1
		END IF

	CASE 31
	' move tool (S)
		IF setMode = "PLAY" AND win.y < 70 THEN win.y = win.y + 1
		IF setMode = "SPRITE BAR" AND tool.bary + 1 < 9 THEN tool.bary = tool.bary + 1
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" AND cam.y < 80 THEN cam.y = cam.y + 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND tool.x > 7 AND tool.x < 15 THEN tool.x = tool.x + 1
		END IF

	CASE 30
	' move tool (A)
		IF setMode = "PLAY" AND win.x > 0 THEN win.x = win.x - 1
		IF setMode = "SPRITE BAR" AND tool.barx - 1 > 0 THEN tool.barx = tool.barx - 1
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" AND cam.x > 0 THEN cam.x = cam.x - 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND tool.y > 16 AND tool.y < 24 THEN tool.y = tool.y - 1
			IF setMode = "PICK COLOR" AND tool.y > 13 AND tool.y < 29 THEN tool.y = tool.y - 1
		END IF

	CASE 32
	' move tool (D)
		IF setMode = "PLAY" AND win.x < 65 THEN win.x = win.x + 1
		IF setMode = "SPRITE BAR" AND tool.barx + 1 < 9 THEN tool.barx = tool.barx + 1
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" AND cam.x < 80 THEN cam.x = cam.x + 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND tool.y > 15 AND tool.y < 23 THEN tool.y = tool.y + 1
			IF setMode = "PICK COLOR" AND tool.y > 11 AND tool.y < 28 THEN tool.y = tool.y + 1
		END IF
	
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
		
	CASE 61
	' game mode (F3)
		sysApp = "GAME"
		setMode = "PLAY"
		modeText = "MODE:GAME"
		modeColor = 47
		handleGame
		CLS
		
	CASE 15
	' pos tool to sprite bar (TAB)
		setMode = "SPRITE BAR"
		setToolPos

	CASE 82
	' pos tool to pick a color in spr mode (INS)
		IF sysApp = "SPRITE" THEN setMode = "PICK COLOR"
		setToolPos

	CASE 73
	' pos tool for map and draw mode, depends on app state (PAGE UP)
		IF sysApp = "SPRITE" THEN setMode = "DRAW"
		IF sysApp = "MAPPING" THEN setMode = "MAP GRID"
		setToolPos
		
	CASE 81
	' store current spr on draw grid on screen (PAGE DOWN)
		IF sysApp = "SPRITE" AND setMode = "SPRITE BAR" THEN sysTask = "PLACE"
		handleSprite
	
	CASE 79
	' rem pixel on tool pos (END)
		sysTask = "DEL PIXEL"
		handleSprite

	CASE 71
	' pick/place objs in editor (HOME)
		IF sysApp = "MAPPING" AND setMode = "SPRITE BAR" THEN tool.picked = sideBarIndex(tool.barx, tool.bary)
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" THEN gridMap(cam.x + tool.x, cam.y + tool.y) = tool.picked
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" THEN grid(tool.x - 8, tool.y - 16) = tool.clr
			IF setMode = "PICK COLOR" THEN
				FOR y = 13 TO 29
					IF tool.x = 18 AND tool.y = y THEN tool.clr = colorPal(y - 12)
				NEXT
			END IF
		END IF
	END SELECT
	IF sysApp = "GAME" AND setMode = "PLAY" THEN
		user.ftile = gridMap(win.x + user.x, win.y + user.y)
		gridMap(win.x + user.x, win.y + user.y) = user.spr
	END IF

END SUB

