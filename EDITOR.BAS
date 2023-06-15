DEFINT A-Z
DECLARE SUB getInput ()
DECLARE SUB handleSprite ()
DECLARE SUB handleGame ()
DECLARE SUB handleFile ()
DECLARE SUB updScreen ()
DECLARE SUB initEditor ()
DECLARE SUB setCursorPos ()
DECLARE SUB updActor ()
DECLARE SUB sysConsole ()
DECLARE SUB switchApp (appName$)

RANDOMIZE TIMER
ON ERROR GOTO BADFILE:

TYPE obj
	x AS INTEGER
	y AS INTEGER
	oldx AS INTEGER
	oldy AS INTEGER
	barx AS INTEGER
	bary AS INTEGER
	picked AS INTEGER
	clr AS INTEGER
	spr AS INTEGER
	hp AS INTEGER
	direction AS INTEGER
	steps AS INTEGER
	state AS INTEGER
	lastTile AS INTEGER
END TYPE

DIM SHARED user AS obj, actor(16) AS obj, cursor AS obj

DIM SHARED mapGrid(40, 25), drawGrid(8, 8), spriteGrid(8, 8), spriteGridIndex(8, 8), dataArray(8, 8)
DIM SHARED spriteIndex(3200), cursorSprite(84)
DIM SHARED colorPalette(16)

DIM SHARED maxSprite, spriteSize
DIM SHARED mapSizeX, mapSizeY
DIM SHARED gridSizeX, gridSizeY
DIM SHARED maxActor, actorNum, visDist

DIM SHARED gridColor, consoleColor, appColor, helpTextColor
DIM SHARED consoleText$, appText$, fileName$
DIM SHARED sysExit, drawSprGrid, sysTask$, sysApp$, setMode$

initEditor
BADFILE:
	sysTask$ = ""
	fileName$ = ""
DO
	getInput
	updActor
	updScreen
	SLEEP
LOOP UNTIL sysExit = 1
WHILE INKEY$ <> "": WEND
SYSTEM

SUB switchApp (appName$)

		IF appName$ = "MAPPING" THEN 
			sysApp$ = "MAPPING"
			setMode$ = "MAP GRID"
			appText$ = "MODE:MAPPER"
			appColor = 39			
		ELSEIF appName$ = "GAME" THEN
			sysApp$ = "GAME"
			setMode$ = "PLAY"
			appText$ = "MODE:GAME"
			appColor = 47
			handleGame
		ELSEIF appName$ = "SPRITE" THEN
			sysApp$ = "SPRITE"
			setMode$ = "DRAW"
			appText$ = "MODE:SPRITE"
			appColor = 52
		END IF
		setCursorPos
		CLS		
		
END SUB

SUB getInput

	user.lastTile = mapGrid(user.x, user.y)
	
	k = INP(96)	
	SELECT CASE k
	CASE 17
	' move cursor (W)
		IF setMode$ = "PLAY" THEN 
			user.y = user.y - 1
		END IF
		IF setMode$ = "SPRITE BAR" AND cursor.bary - 1 > 0 THEN cursor.bary = cursor.bary - 1
		IF sysApp$ = "MAPPING" AND setMode$ = "MAP GRID" THEN user.y = user.y - 1
		IF sysApp$ = "SPRITE" THEN
			IF setMode$ = "DRAW" AND cursor.x > 8 AND cursor.x < 16 THEN cursor.x = cursor.x - 1
		END IF

	CASE 31
	' move cursor (S)
		IF setMode$ = "PLAY" THEN 
			user.y = user.y + 1
		END IF
		IF setMode$ = "SPRITE BAR" AND cursor.bary + 1 < 9 THEN cursor.bary = cursor.bary + 1
		IF sysApp$ = "MAPPING" AND setMode$ = "MAP GRID" THEN user.y = user.y + 1
		IF sysApp$ = "SPRITE" THEN
			IF setMode$ = "DRAW" AND cursor.x > 7 AND cursor.x < 15 THEN cursor.x = cursor.x + 1
		END IF

	CASE 30
	' move cursor (A)
		IF setMode$ = "PLAY" THEN 
			user.x = user.x - 1
		END IF	
		IF setMode$ = "SPRITE BAR" AND cursor.barx - 1 > 0 THEN cursor.barx = cursor.barx - 1
		IF sysApp$ = "MAPPING" AND setMode$ = "MAP GRID" THEN user.x = user.x - 1
		IF sysApp$ = "SPRITE" THEN
			IF setMode$ = "DRAW" AND cursor.y > 16 AND cursor.y < 24 THEN cursor.y = cursor.y - 1
			IF setMode$ = "PICK COLOR" AND cursor.y > 13 AND cursor.y < 29 THEN cursor.y = cursor.y - 1
		END IF

	CASE 32
	' move cursor (D)
		IF setMode$ = "PLAY" THEN 
			user.x = user.x + 1
		END IF
		IF setMode$ = "SPRITE BAR" AND cursor.barx + 1 < 9 THEN cursor.barx = cursor.barx + 1
		IF sysApp$ = "MAPPING" AND setMode$ = "MAP GRID" THEN user.x = user.x + 1
		IF sysApp$ = "SPRITE" THEN
			IF setMode$ = "DRAW" AND cursor.y > 15 AND cursor.y < 23 THEN cursor.y = cursor.y + 1
			IF setMode$ = "PICK COLOR" AND cursor.y > 11 AND cursor.y < 28 THEN cursor.y = cursor.y + 1
		END IF
	
	CASE 41
	' open console (~)
		sysConsole

	CASE 59
	' spr mode (F1)
		switchApp("SPRITE")

	CASE 60
	' map mode (F2)
		switchApp("MAPPING")
		
	CASE 61
	' game mode (F3)
		switchApp("GAME")
		
	CASE 15
	' pos cursor to sprite bar (TAB)
		IF sysApp$ <> "GAME" THEN 
			setMode$ = "SPRITE BAR"
			drawSprGrid = 1
			setCursorPos
		END IF

	CASE 82
	' pos cursor to pick a color in spr mode (INS)
		IF sysApp$ = "SPRITE" THEN setMode$ = "PICK COLOR"
		drawSprGrid = 0
		setCursorPos

	CASE 73
	' pos cursor for map and draw mode, depends on app state (PAGE UP)
		IF sysApp$ = "SPRITE" THEN setMode$ = "DRAW"
		IF sysApp$ = "MAPPING" THEN setMode$ = "MAP GRID"
		drawSprGrid = 0
		setCursorPos
		
	CASE 81
	' store current spr on draw grid (PAGE DOWN)
		IF sysApp$ = "SPRITE" AND setMode$ = "SPRITE BAR" THEN sysTask$ = "PLACE"
		handleSprite
	
	CASE 79
	' rem pixel on cursor pos (END)
		IF sysApp$ = "SPRITE" AND setMode$ = "DRAW" THEN sysTask$ = "DEL PIXEL"
		handleSprite

	CASE 71
	' pick/place objs in editor (HOME)
		IF sysApp$ = "MAPPING" AND setMode$ = "SPRITE BAR" THEN cursor.picked = spriteGridIndex(cursor.barx, cursor.bary)
		IF sysApp$ = "MAPPING" AND setMode$ = "MAP GRID" THEN mapGrid(cursor.x, cursor.y) = cursor.picked
		IF sysApp$ = "SPRITE" THEN
			IF setMode$ = "DRAW" THEN drawGrid(cursor.x - 8, cursor.y - 16) = cursor.clr
			IF setMode$ = "PICK COLOR" THEN
				FOR y = 13 TO 29
					IF cursor.x = 18 AND cursor.y = y THEN cursor.clr = colorPalette(y - 12)
				NEXT
			END IF
		END IF
	END SELECT
	mapGrid(user.x, user.y) = user.spr
END SUB

SUB handleFile

	COLOR consoleColor
	WHILE INKEY$ <> "": WEND
	SELECT CASE sysTask$
		CASE "NEW"
			' clears current map or sprite grid
			IF sysApp$ = "SPRITE" THEN
				ERASE drawGrid
			ELSE
				ERASE mapGrid
			END IF
			fileName$ = ""

		CASE "SAVE GFX"
			' this needs to be redone!
			LOCATE 23, 2: INPUT "GFX NAME:", fileName$
			OPEN fileName$ FOR OUTPUT AS #1
			FOR x = 0 TO 8
				FOR y = 0 TO 8
					IF y < spriteSize THEN
						PRINT #1, drawGrid(x, y); ",";
					ELSE
						PRINT #1, drawGrid(x, y);
					END IF
				IF y = spriteSize THEN PRINT #1,
				NEXT
			NEXT
			CLOSE #1

		CASE "LOAD GFX"
			' load sprites from .GFX file
			LOCATE 23, 2: INPUT "LOAD GFX:", fileName$
			OPEN fileName$ FOR INPUT AS #1
			FOR x = 0 TO 8
				FOR y = 0 TO 8
					INPUT #1, drawGrid(x, y)
				NEXT
			NEXT
			CLOSE #1

		CASE "SAVE PAK"
			' saves all sprites found in current sprite grid into a .PAK file
			LOCATE 23, 2: INPUT "PAK NAME:", fileName$
			OPEN fileName$ FOR OUTPUT AS #1
			fileLen = (maxSprite * 44)
			FOR i = 1 TO fileLen
				PRINT #1, spriteIndex(i)
			NEXT
			
			FOR x = 1 TO 8
				FOR y = 1 TO 8
					PRINT #1, spriteGrid(x, y)
				NEXT
			NEXT
			CLOSE #1
			fileName$ = ""

		CASE "LOAD PAK"
			LOCATE 23, 2: INPUT "LOAD PAK: ", fileName$
			OPEN fileName$ FOR INPUT AS #1
			fileLen = (maxSprite * 44)
			FOR i = 1 TO fileLen
				INPUT #1, spriteIndex(i)
			NEXT
			
			FOR x = 1 TO 8
				FOR y = 1 TO 8
					INPUT #1, spriteGrid(x, y)
				NEXT
			NEXT
			CLOSE #1
			fileName$ = ""

		CASE "NEW PAL"
			FOR i = 1 TO 16
				LOCATE 23, 2: INPUT "COLOR:", colorPalette(i)
			NEXT

		CASE "LIST PAL"
			FOR i = 1 TO 16
				 COLOR colorPalette(i): PRINT colorPalette(i)
			NEXT
			SLEEP

		CASE "SAVE PAL"
			LOCATE 23, 2: INPUT "PAL NAME:", fileName$
			OPEN fileName$ FOR OUTPUT AS #1
			FOR i = 1 TO 16
				PRINT #1, colorPalette(i)
			NEXT
			CLOSE #1
			fileName$ = ""

		CASE "LOAD PAL"
			LOCATE 23, 2: INPUT "LOAD PAL:", fileName$
			OPEN fileName$ FOR INPUT AS #1
			FOR i = 1 TO 16
				INPUT #1, colorPalette(i)
			NEXT
			CLOSE #1
			fileName$ = ""

		CASE "SAVE MAP"
			LOCATE 23, 2: INPUT "MAP NAME: ", fileName$
			OPEN fileName$ FOR OUTPUT AS #1
			FOR x = 0 TO mapSize
				FOR y = 0 TO mapSize
					PRINT #1, mapGrid(x, y)
				NEXT
			NEXT
			CLOSE #1

		CASE "LOAD MAP"
			LOCATE 23, 2: INPUT "LOAD MAP: ", fileName$
			OPEN fileName$ FOR INPUT AS #1
			FOR x = 0 TO mapSize
				FOR y = 0 TO mapSize
					INPUT #1, mapGrid(x, y)
				NEXT
			NEXT
			CLOSE #1
	END SELECT
	sysTask$ = ""

END SUB

SUB handleGame

	user.x = 20
	user.y = 11
	user.spr = 33 * 44 ' gordon!
	user.hp = 50	
	user.lastTile = mapGrid(user.x, user.y)
	
	' generate actors using user made map
	maxActor = 16	
	actorNum = 0		
	FOR x = 1 to mapSizeX
		FOR y = 1 to mapSizeY
			IF mapGrid(x, y) = 2 and actorNum < maxActor THEN
				actorNum = actorNum + 1
				actor(actorNum).spr = 2 * 44	' zombie!			
				actor(actorNum).x = x
				actor(actorNum).y = y
				actor(actorNum).state = 1
				actor(actorNum).hp = 25
				actor(actorNum).steps = 0
				actor(actorNum).direction = RND * 4				
				mapGrid(actor(actorNum).x, actor(actorNum).y) = actor(actorNum).spr			
			END IF
		NEXT
	NEXT
	
	FOR x = 1 to mapSizeX
		FOR y = 1 to mapSizeY
			sprite = mapGrid(x, y)
			IF sprite THEN mapGrid(x, y) = sprite * 44
		NEXT
	NEXT
	
END SUB

SUB handleSprite

	WHILE INKEY$ <> "": WEND
	SELECT CASE sysTask$
		CASE "DEL PIXEL"
			drawGrid(cursor.x - 8, cursor.y - 16) = 0		
			
		CASE "PLACE"
			drawSprGrid = 1
			index = spriteGridIndex(cursor.barx, cursor.bary)
			spriteGrid(cursor.barx, cursor.bary) = index
			GET (110, 80)-(118, 88), spriteIndex(44 * index)
	END SELECT
	sysTask$ = ""

END SUB

SUB initEditor

	SCREEN 13	
	helpTextColor = 17
	consoleColor = 43
	cursor.spr = 42	
	switchApp("SPRITE")
		
	gridColor = 5
	gridSizeX = 8
	gridSizeY = 8
	
	mapSizeX = 38
	mapSizeY = 23
	
	' fix this issue too, should be 8 (saving sprites to file prob)
	spriteSize = 7
	maxSprite = 64 
	visDist = 5
		
	FOR x = 1 TO gridSizeX
		FOR y = 1 TO gridSizeY
			index = index + 1
			spriteGridIndex(x, y) = index
		NEXT
	NEXT

	FOR i = 1 TO 16
		colorPalette(i) = i
	NEXT

	' redo this to a better file format
	OPEN "CURSOR.GFX" FOR INPUT AS #1
	FOR x = 0 TO 8 
		FOR y = 0 TO 8
			INPUT #1, dataArray(x, y)
		NEXT
	NEXT
	CLOSE #1
	FOR x = 0 TO 8 
		FOR y = 0 TO 8
			PSET (x, y), dataArray(x, y)
		NEXT
	NEXT
	GET (0, 0)-(8, 8), cursorSprite(42)
	ERASE dataArray
	CLS
	updScreen

END SUB

SUB setCursorPos

	' this needs rework
	SELECT CASE setMode$
		CASE "DRAW"
			cursor.x = 8
			cursor.y = 16
		CASE "PICK COLOR"
			cursor.x = 18
			cursor.y = 13			
		CASE "SPRITE BAR"
			cursor.barx = 1
			cursor.bary = 1			
		CASE "MAP GRID"
			user.x = 20
			user.y = 11
	END SELECT

END SUB

SUB sysConsole

	LOCATE 23, 8: COLOR helpTextColor: PRINT "'HELP' FOR COMMANDS"
	COLOR consoleColor
	
	WHILE INKEY$ <> "": WEND
	LOCATE 23, 2: INPUT ">>>: ", consoleText$
	WHILE INKEY$ <> "": WEND
	SELECT CASE consoleText$
		CASE "exit"
			sysExit = 1

		CASE "save pak"
			sysTask$ = "SAVE PAK"
			handleFile

		CASE "new pal"
			IF sysApp$ = "SPRITE" THEN sysTask$ = "NEW PAL"
			handleFile

		CASE "list pal"
			IF sysApp$ = "SPRITE" THEN sysTask$ = "LIST PAL"
			handleFile
			
		CASE "load pal"
			IF sysApp$ = "SPRITE" THEN sysTask$ = "LOAD PAL"
			handleFile

		CASE "save pal"
			IF sysApp$ = "SPRITE" THEN sysTask$ = "SAVE PAL"
			handleFile

		CASE "list 256"
			IF sysApp$ = "SPRITE" THEN
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
			IF sysApp$ = "SPRITE" THEN
				LOCATE 23, 2: INPUT "COLOR:", num
				cursor.clr = num
			END IF

		CASE "load pak"
			sysTask$ = "LOAD PAK"
			handleFile

		CASE "new"
			sysTask$ = "NEW"
			handleFile

		CASE "load"
			IF sysApp$ = "MAPPING" THEN
				sysTask$ = "LOAD MAP"
			ELSE
				sysTask$ = "LOAD GFX"
			END IF
			handleFile

		CASE "save"
			IF sysApp$ = "MAPPING" THEN
				sysTask$ = "SAVE MAP"
			ELSE
				sysTask$ = "SAVE GFX"
			END IF
			handleFile

		CASE "dir"
			FILES "\*.*"
			SLEEP
	END SELECT
	consoleText$ = ""
	CLS
	updScreen

END SUB

SUB updActor

	IF sysApp$ = "GAME" THEN
	FOR i = 1 TO actorNum
		IF actor(i).hp <= 0 THEN GOTO skipAi		
		
		IF actor(i).state = 1 THEN
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
			
			IF mapGrid(newx, newy) = 0 AND actor(i).steps < RND * 3 THEN
				actor(i).x = newx
				actor(i).y = newy
				actor(i).steps = actor(i).steps + 1	
			ELSE
				actor(i).steps = 0
				actor(i).direction = RND * 4				
			END IF
			actor(i).state = 2
						
		ELSEIF actor(i).state = 2 THEN
			currentx = actor(i).x
			currenty = actor(i).y	
			FOR x = actor(i).x - visDist TO actor(i).x + visDist
				FOR y = actor(i).y - visDist TO actor(i).y + visDist						
					IF mapGrid(x, y) = 49 THEN	
						dx = actor(i).x - x
						dy = actor(i).y - y
						actor(i).state = 2
					ELSEIF mapGrid(x, y) = 3 THEN	
						dx = actor(i).x - x
						dy = actor(i).y - y
						actor(i).state = 2
					ELSEIF mapGrid(x, y) = user.spr THEN	
						dx = actor(i).x - x
						dy = actor(i).y - y
						actor(i).state = 2
					ELSE
						actor(i).state = 1
					END IF				
				NEXT
			NEXT		
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
			ELSEIF dx = 0 AND dy = 0 THEN 
			END IF
			
			collide = mapGrid(actor(i).x, actor(i).y)											
			IF collide THEN
			' setup a manager to control this stuff seperate?
			' send coordinates and the 'action' to manager to update sprites on the  grid or wwahhtever
				IF collide = user.spr THEN user.hp = user.hp - 10 ' hurt player
				IF collide = 49 THEN mapGrid(actor(i).x, actor(i).y) = 36 ' turn snarks to green splat
				IF collide = 3 THEN mapGrid(actor(i).x, actor(i).y) = 35 ' turn secguard body to red blood
				actor(i).x = currentx
				actor(i).y = currenty		
				actor(i).state = 1
			END IF
		END IF
		mapGrid(actor(i).x, actor(i).y) = actor(i).spr	
skipAi:
	NEXT
END IF

END SUB

SUB updScreen

	COLOR appColor
	LINE (0, 0)-(320, 16), appColor, BF
	LOCATE 2, 2: PRINT "FILE:"; 
	LOCATE 2, 29: PRINT appText$
	WAIT &H3DA, 8
	
	' render map app
	IF sysApp$ = "MAPPING" THEN
		LINE (176, 24)-(312, 160), 0, BF
		FOR x = 1 TO mapSizeX
			FOR y = 1 TO mapSizeY
				index = mapGrid(x, y)
				IF index THEN PUT (x * 8, y * 8), spriteIndex(44 * index)
			NEXT
		NEXT
		LOCATE 22, 28: COLOR appColor: PRINT "sprindex"; spriteGridIndex(cursor.barx, cursor.bary)
		LOCATE 23, 30: COLOR appColor: PRINT "x"; user.x; "y"; user.y
		PUT (240, 88), cursorSprite(cursor.spr)
	END IF
	
	' render sprite app
	IF sysApp$ = "SPRITE" THEN
		FOR x = 8 TO 15
			FOR y = 16 TO 23
				IF drawGrid(x - 8, y - 16) THEN
					LOCATE x, y: COLOR drawGrid(x - 8, y - 16): PRINT CHR$(254)
				ELSE
					LOCATE x, y: COLOR gridColor: PRINT CHR$(254)
				END IF
			NEXT
		NEXT		

		FOR x = 0 TO gridSizeX
			FOR y = 0 TO gridSizeY
				PSET (x + 110, y + 80), drawGrid(y, x)
			NEXT
		NEXT		
		FOR i = 1 TO 16
			LOCATE 18, i + 12: COLOR colorPalette(i): PRINT CHR$(254)
		NEXT				
		IF setMode$ = "DRAW" THEN LOCATE cursor.x, cursor.y: COLOR cursor.clr: PRINT CHR$(254)
		IF setMode$ = "PICK COLOR" THEN LOCATE cursor.x, cursor.y: COLOR cursor.y - 12: PRINT CHR$(248)
	END IF
	
	' render game app
	IF sysApp$ = "GAME" THEN
		FOR x = 1 TO mapSizeX
			FOR y = 1 TO mapSizeY		
				index = mapGrid(x, y)				
				IF index THEN PUT (x * 8, y * 8), spriteIndex(index)
			NEXT
		NEXT

		LOCATE 23, 30: COLOR appColor: PRINT "x"; user.x; "y";user.y
		
	ELSE
		' render sprite grid
		LINE (24, 24)-(88, 88), 0, BF
		FOR x = 1 TO gridSizeX
			FOR y = 1 TO gridSizeY
				index = spriteGrid(x, y)
				IF index THEN PUT ((x + 2) * 8, (y + 2) * 8), spriteIndex(44 * index)
			NEXT
		NEXT
		IF setMode$ = "SPRITE BAR" THEN PUT ((cursor.barx + 2) * 8, (cursor.bary + 2) * 8), cursorSprite(42)
	END IF
	
END SUB

