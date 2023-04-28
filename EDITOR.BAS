DEFINT A-Z
DECLARE SUB getInput ()
DECLARE SUB handleSprite ()
DECLARE SUB handleGame ()
DECLARE SUB handleFile ()
DECLARE SUB updScreen ()
DECLARE SUB initEditor ()
DECLARE SUB setCursorPos ()
DECLARE SUB updEntity ()
DECLARE SUB sysConsole ()
DECLARE SUB switchApp (appName$)

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
	state AS INTEGER
	lastTile AS INTEGER
END TYPE

DIM SHARED user AS obj
DIM SHARED entity(16) AS obj
DIM SHARED cursor AS obj
DIM SHARED gameCam  AS obj
DIM SHARED mapWindow AS obj

DIM SHARED mapGrid(100, 100)
DIM SHARED drawGrid(8, 8)
DIM SHARED spriteGrid(8, 8)
DIM SHARED spriteGridIndex(8, 8)
DIM SHARED loadArray(8, 8)
DIM SHARED spriteIndex(3200)
DIM SHARED colorPalette(16)
DIM SHARED cursorSprite(84)
DIM SHARED maxSprite
DIM SHARED spriteSize
DIM SHARED mapSize
DIM SHARED maxActor
DIM SHARED visDist
DIM SHARED entityCount

DIM SHARED gridColor
DIM SHARED consoleColor
DIM SHARED appColor

DIM SHARED consoleText AS STRING
DIM SHARED appText AS STRING
DIM SHARED sysTask AS STRING
DIM SHARED sysApp AS STRING
DIM SHARED setMode AS STRING
DIM SHARED fileName AS STRING
DIM SHARED closeApp

initEditor
BADFILE:
	sysTask = ""
	fileName = ""
DO
	getInput
	updEntity
	updScreen
	SLEEP
LOOP UNTIL closeApp = 1
WHILE INKEY$ <> "": WEND
SYSTEM

SUB switchApp (appName$)

		IF appName$ = "MAPPING" THEN 
			sysApp = "MAPPING"
			setMode = "MAP GRID"
			appText = "MODE:MAPPER"
			appColor = 39			
		ELSEIF appName$ = "GAME" THEN
			sysApp = "GAME"
			setMode = "PLAY"
			appText = "MODE:GAME"
			appColor = 47
			handleGame
		ELSEIF appName$ = "SPRITE" THEN
			sysApp = "SPRITE"
			setMode = "DRAW"
			appText = "MODE:SPRITE"
			appColor = 52
		END IF
		setCursorPos
		CLS
		
END SUB

SUB getInput

	IF sysApp = "GAME" THEN mapGrid(mapWindow.x + user.x, mapWindow.y + user.y) = user.lastTile	
	k = INP(96)
	SELECT CASE k
	CASE 17
	' move cursor (W)
		IF setMode = "PLAY" AND mapWindow.y > 0 THEN mapWindow.y = mapWindow.y - 1
		IF setMode = "SPRITE BAR" AND cursor.bary - 1 > 0 THEN cursor.bary = cursor.bary - 1
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" AND gameCam.y > 0 THEN gameCam.y = gameCam.y - 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND cursor.x > 8 AND cursor.x < 16 THEN cursor.x = cursor.x - 1
		END IF

	CASE 31
	' move cursor (S)
		IF setMode = "PLAY" AND mapWindow.y < 70 THEN mapWindow.y = mapWindow.y + 1
		IF setMode = "SPRITE BAR" AND cursor.bary + 1 < 9 THEN cursor.bary = cursor.bary + 1
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" AND gameCam.y < 80 THEN gameCam.y = gameCam.y + 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND cursor.x > 7 AND cursor.x < 15 THEN cursor.x = cursor.x + 1
		END IF

	CASE 30
	' move cursor (A)
		IF setMode = "PLAY" AND mapWindow.x > 0 THEN mapWindow.x = mapWindow.x - 1
		IF setMode = "SPRITE BAR" AND cursor.barx - 1 > 0 THEN cursor.barx = cursor.barx - 1
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" AND gameCam.x > 0 THEN gameCam.x = gameCam.x - 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND cursor.y > 16 AND cursor.y < 24 THEN cursor.y = cursor.y - 1
			IF setMode = "PICK COLOR" AND cursor.y > 13 AND cursor.y < 29 THEN cursor.y = cursor.y - 1
		END IF

	CASE 32
	' move cursor (D)
		IF setMode = "PLAY" AND mapWindow.x < 65 THEN mapWindow.x = mapWindow.x + 1
		IF setMode = "SPRITE BAR" AND cursor.barx + 1 < 9 THEN cursor.barx = cursor.barx + 1
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" AND gameCam.x < 80 THEN gameCam.x = gameCam.x + 1
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" AND cursor.y > 15 AND cursor.y < 23 THEN cursor.y = cursor.y + 1
			IF setMode = "PICK COLOR" AND cursor.y > 11 AND cursor.y < 28 THEN cursor.y = cursor.y + 1
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
		IF sysApp <> "GAME" THEN 
			setMode = "SPRITE BAR"
			renderSprGrid = 1
			setCursorPos
		END IF

	CASE 82
	' pos cursor to pick a color in spr mode (INS)
		IF sysApp = "SPRITE" THEN setMode = "PICK COLOR"
		renderSprGrid = 0
		setCursorPos

	CASE 73
	' pos cursor for map and draw mode, depends on app state (PAGE UP)
		IF sysApp = "SPRITE" THEN setMode = "DRAW"
		IF sysApp = "MAPPING" THEN setMode = "MAP GRID"
		renderSprGrid = 0
		setCursorPos
		
	CASE 81
	' store current spr on draw grid on screen (PAGE DOWN)
		IF sysApp = "SPRITE" AND setMode = "SPRITE BAR" THEN sysTask = "PLACE"
		handleSprite
	
	CASE 79
	' rem pixel on cursor pos (END)
		IF sysApp = "SPRITE" AND setMode = "DRAW" THEN sysTask = "DEL PIXEL"
		handleSprite

	CASE 71
	' pick/place objs in editor (HOME)
		IF sysApp = "MAPPING" AND setMode = "SPRITE BAR" THEN cursor.picked = spriteGridIndex(cursor.barx, cursor.bary)
		IF sysApp = "MAPPING" AND setMode = "MAP GRID" THEN mapGrid(gameCam.x + cursor.x, gameCam.y + cursor.y) = cursor.picked
		IF sysApp = "SPRITE" THEN
			IF setMode = "DRAW" THEN drawGrid(cursor.x - 8, cursor.y - 16) = cursor.clr
			IF setMode = "PICK COLOR" THEN
				FOR y = 13 TO 29
					IF cursor.x = 18 AND cursor.y = y THEN cursor.clr = colorPalette(y - 12)
				NEXT
			END IF
		END IF
	END SELECT
	IF sysApp = "GAME" THEN
		user.lastTile = mapGrid(mapWindow.x + user.x, mapWindow.y + user.y)
		mapGrid(mapWindow.x + user.x, mapWindow.y + user.y) = user.spr		
	END IF

END SUB

SUB handleFile

	COLOR consoleColor
	WHILE INKEY$ <> "": WEND
	SELECT CASE sysTask
		CASE "NEW"
			IF sysApp = "SPRITE" THEN
				ERASE drawGrid
			ELSE
				ERASE mapGrid
			END IF
			fileName = ""

		CASE "SAVE GFX"
			LOCATE 23, 2: INPUT "GFX NAME:", fileName
			OPEN fileName FOR OUTPUT AS #1
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
			LOCATE 23, 2: INPUT "LOAD GFX:", fileName
			OPEN fileName FOR INPUT AS #1
			FOR x = 0 TO 8
				FOR y = 0 TO 8
					INPUT #1, drawGrid(x, y)
				NEXT
			NEXT
			CLOSE #1

		CASE "SAVE PAK"
			LOCATE 23, 2: INPUT "PAK NAME:", fileName
			OPEN fileName FOR OUTPUT AS #1
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
			fileName = ""

		CASE "LOAD PAK"
			LOCATE 23, 2: INPUT "LOAD PAK: ", fileName
			OPEN fileName FOR INPUT AS #1
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
			fileName = ""

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
			LOCATE 23, 2: INPUT "PAL NAME:", fileName
			OPEN fileName FOR OUTPUT AS #1
			FOR i = 1 TO 16
				PRINT #1, colorPalette(i)
			NEXT
			CLOSE #1
			fileName = ""

		CASE "LOAD PAL"
			LOCATE 23, 2: INPUT "LOAD PAL:", fileName
			OPEN fileName FOR INPUT AS #1
			FOR i = 1 TO 16
				INPUT #1, colorPalette(i)
			NEXT
			CLOSE #1
			fileName = ""

		CASE "SAVE MAP"
			LOCATE 23, 2: INPUT "MAP NAME: ", fileName
			OPEN fileName FOR OUTPUT AS #1
			FOR x = 0 TO mapSize
				FOR y = 0 TO mapSize
					PRINT #1, mapGrid(x, y)
				NEXT
			NEXT
			CLOSE #1

		CASE "LOAD MAP"
			LOCATE 23, 2: INPUT "LOAD MAP: ", fileName
			OPEN fileName FOR INPUT AS #1
			FOR x = 0 TO mapSize
				FOR y = 0 TO mapSize
					INPUT #1, mapGrid(x, y)
				NEXT
			NEXT
			CLOSE #1
	END SELECT
	sysTask = ""

END SUB

SUB handleGame

	maxActor = 16	
	user.spr = 33 ' gordon!
	user.hp = 50	
	entityCount = 0	
	FOR x = 1 to mapSize
		FOR y = 1 to mapSize
			IF mapGrid(x, y) = 2 and entityCount < maxActor THEN
				entityCount = entityCount + 1
				entity(entityCount).spr = 2	' zombie!			
				entity(entityCount).x = x
				entity(entityCount).y = y
				entity(entityCount).state = 1
				entity(entityCount).hp = 25
				entity(entityCount).steps = 0
				entity(entityCount).direction = RND * 4				
				mapGrid(entity(entityCount).x, entity(entityCount).y) = entity(entityCount).lastTile	
				mapGrid(entity(entityCount).x, entity(entityCount).y) = entity(entityCount).spr			
			END IF
		NEXT
	NEXT
	
END SUB

SUB handleSprite

	WHILE INKEY$ <> "": WEND
	SELECT CASE sysTask
		CASE "DEL PIXEL"
			 drawGrid(cursor.x - 8, cursor.y - 16) = 0
			 
		CASE "PLACE"
			renderSprGrid = 1
			index = spriteGridIndex(cursor.barx, cursor.bary)
			spriteGrid(cursor.barx, cursor.bary) = index
			GET (110, 80)-(118, 88), spriteIndex(44 * index)
	END SELECT
	sysTask = ""

END SUB

SUB initEditor

	SCREEN 13	
	' game coords
	user.x = 20
	user.y = 11	
	mapWindow.x = 30 - user.x
	mapWindow.y = 49 - user.y	
	' editor stuff
	consoleColor = 43
	sysApp = "SPRITE"
	setMode = "DRAW"
	appColor = 52
	appText = "MODE:SPRITE"
	setCursorPos
	cursor.spr = 42
	cursor.clr = appColor
	gridColor = 5
	mapSize = 100
	spriteSize = 7
	gameCam.x = 30
	gameCam.y = 11
	maxSprite = 64
	
	visDist = 5
		
	FOR x = 1 TO 8
		FOR y = 1 TO 8
			index = index + 1
			spriteGridIndex(x, y) = index
		NEXT
	NEXT

	FOR i = 1 TO 16
		colorPalette(i) = i
	NEXT

	OPEN "CURSOR.GFX" FOR INPUT AS #1
	FOR x = 0 TO 8
		FOR y = 0 TO 8
			INPUT #1, loadArray(x, y)
		NEXT
	NEXT
	CLOSE #1
	FOR x = 0 TO 8
		FOR y = 0 TO 8
			PSET (x, y), loadArray(x, y)
		NEXT
	NEXT
	GET (0, 0)-(8, 8), cursorSprite(42)
	ERASE loadArray
	CLS
	updScreen

END SUB

SUB setCursorPos

	' this all needs rework
	SELECT CASE setMode
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
			cursor.x = 30
			cursor.y = 11
	END SELECT

END SUB

SUB sysConsole

	LOCATE 23, 8: COLOR 17: PRINT "'HELP' FOR COMMANDS"
	COLOR consoleColor
	
	WHILE INKEY$ <> "": WEND
	LOCATE 23, 2: INPUT ">>>: ", consoleText
	WHILE INKEY$ <> "": WEND
	SELECT CASE consoleText
		CASE "exit"
			closeApp = 1

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
				cursor.clr = num
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
	consoleText = ""
	CLS
	updScreen

END SUB

SUB updEntity

	IF sysApp = "GAME" THEN
	FOR i = 1 TO entityCount
		IF entity(i).hp <= 0 THEN GOTO skipAi		
		mapGrid(entity(i).x, entity(i).y) = entity(i).lastTile
		
		IF entity(i).state = 1 THEN
			IF entity(i).direction = 0 THEN
				dx = 1: dy = 0
			ELSEIF entity(i).direction = 1 THEN
				dx = -1: dy = 0
			ELSEIF entity(i).direction = 2 THEN
					dx = 0: dy = -1
			ELSEIF entity(i).direction = 3 THEN
				dx = 0: dy = 1
			END IF				
			newx = entity(i).x + dx
			newy = entity(i).y + dy		
			IF mapGrid(newx, newy) = 0 AND entity(i).steps < RND * 3 THEN
				entity(i).x = newx
				entity(i).y = newy
				entity(i).steps = entity(i).steps + 1	
			ELSE
				entity(i).steps = 0
				entity(i).direction = RND * 4				
			END IF
			entity(i).state = 2
						
		ELSEIF entity(i).state = 2 THEN
			currentx = entity(i).x
			currenty = entity(i).y	
			FOR x = entity(i).x - visDist TO entity(i).x + visDist
				FOR y = entity(i).y - visDist TO entity(i).y + visDist						
					IF mapGrid(x, y) = user.spr THEN	
						dx = entity(i).x - x
						dy = entity(i).y - y
						entity(i).state = 2
					ELSE
						entity(i).state = 1
					END IF				
				NEXT
			NEXT		
			IF dx < 0 AND dy < 0 THEN
				rand = RND * 2
				IF rand = 1 THEN
					entity(i).x = entity(i).x + 1						
				ELSE
					entity(i).y = entity(i).y + 1
				END IF
			ELSEIF dx > 0 AND dy > 0 THEN
				rand = RND * 2
				IF rand = 1 THEN
					entity(i).x = entity(i).x - 1
				ELSE
					entity(i).y = entity(i).y - 1
				END IF
			ELSEIF dx < 0 AND dy > 0 THEN
				rand = RND * 2
				IF rand = 1 THEN
					entity(i).x = entity(i).x + 1
				ELSE
					entity(i).y = entity(i).y - 1
				END IF
			ELSEIF dx > 0 AND dy < 0 THEN
				rand = RND * 2
				IF rand = 1 THEN						
					entity(i).x = entity(i).x - 1
				ELSE
					entity(i).y = entity(i).y + 1
				END IF
			ELSEIF dx = 0 AND dy < 0 THEN entity(i).y = entity(i).y + 1	
			ELSEIF dx = 0 AND dy > 0 THEN entity(i).y = entity(i).y - 1					
			ELSEIF dx < 0 AND dy = 0 THEN entity(i).x = entity(i).x + 1				
			ELSEIF dx > 0 AND dy = 0 THEN entity(i).x = entity(i).x - 1						
			ELSEIF dx = 0 AND dy = 0 THEN 
			END IF
			
			collide = mapGrid(entity(i).x, entity(i).y)											
			IF collide THEN
				IF collide = user.spr THEN user.hp = user.hp - 5
				entity(i).x = currentx
				entity(i).y = currenty		
				entity(i).state = 1
			END IF
		END IF
		entity(i).lastTile = mapGrid(entity(i).x, entity(i).y)
		mapGrid(entity(i).x, entity(i).y) = entity(i).spr	
skipAi:
	NEXT
END IF

END SUB

SUB updScreen

	COLOR appColor
	LINE (0, 0)-(320, 16), appColor, BF
	LOCATE 2, 2: PRINT "FILE:"; 
	LOCATE 2, 29: PRINT appText
	WAIT &H3DA, 8	
	
	' render game tab
	IF sysApp = "GAME" THEN
		LINE (80, 24)-(212, 160), 0, BF
		FOR x = 10 TO 26
			FOR y = 3 TO 19		
				index = mapGrid(x + mapWindow.x, y + mapWindow.y)				
				IF index THEN PUT (x * 8, y * 8), spriteIndex(44 * index)
			NEXT
		NEXT				
		LOCATE 22, 30: COLOR consoleColor: PRINT "HP:"; user.hp
		LOCATE 23, 30: COLOR appColor: PRINT "x"; mapWindow.x; "y"; mapWindow.y			
		'DEBUG_	
		LOCATE 22, 2: COLOR 48: PRINT "state:"; entity(1).state
	ELSE
		' render sprite grid
		LINE (24, 24)-(88, 88), 0, BF
		FOR x = 1 TO 8
			FOR y = 1 TO 8
				index = spriteGrid(x, y)
				IF index THEN PUT ((x + 2) * 8, (y + 2) * 8), spriteIndex(44 * index)
			NEXT
		NEXT
		IF setMode = "SPRITE BAR" THEN PUT ((cursor.barx + 2) * 8, (cursor.bary + 2) * 8), cursorSprite(42)
	END IF
	
	' render map tab
	IF sysApp = "MAPPING" THEN
		LINE (176, 24)-(312, 160), 0, BF
		FOR x = 22 TO 38
			FOR y = 3 TO 19
				index = mapGrid(x + gameCam.x, y + gameCam.y)
				IF index THEN PUT (x * 8, y * 8), spriteIndex(44 * index)
			NEXT
		NEXT
		LOCATE 22, 28: COLOR appColor: PRINT "sprindex"; spriteGridIndex(cursor.barx, cursor.bary)
		LOCATE 23, 30: COLOR appColor: PRINT "x"; gameCam.x; "y"; gameCam.y
		PUT (240, 88), cursorSprite(cursor.spr)
	END IF
	
	' render sprite tab
	IF sysApp = "SPRITE" THEN
		FOR x = 8 TO 15
			FOR y = 16 TO 23
				IF drawGrid(x - 8, y - 16) THEN
					LOCATE x, y: COLOR drawGrid(x - 8, y - 16): PRINT CHR$(254)
				ELSE
					LOCATE x, y: COLOR gridColor: PRINT CHR$(254)
				END IF
			NEXT
		NEXT		
		
' fix this?
		FOR x = 0 TO 8
			FOR y = 0 TO 8
				PSET (x + 110, y + 80), drawGrid(y, x)
			NEXT
		NEXT		
		FOR i = 1 TO 16
			LOCATE 18, i + 12: COLOR colorPalette(i): PRINT CHR$(254)
		NEXT				
		IF setMode = "DRAW" THEN LOCATE cursor.x, cursor.y: COLOR cursor.clr: PRINT CHR$(254)
		IF setMode = "PICK COLOR" THEN LOCATE cursor.x, cursor.y: COLOR cursor.y - 12: PRINT CHR$(248)
	END IF	
	
END SUB

