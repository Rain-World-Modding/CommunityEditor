global gLOprops, gLevel, gLEprops, gLoadedName, levelName, gPrioCam, snapToGrid, preciseSnap, stg, ps, lvlPropOutput, showControls, hideHelpClick

on exitFrame me
  if (showControls) then
    sprite(120).blend = 100
    sprite(121).blend = 100
    sprite(150).blend = 100
    sprite(151).blend = 100
  else
    sprite(120).blend = 0
    sprite(121).blend = 0
    sprite(150).blend = 0
    sprite(151).blend = 0
  end if
  
  gLOprops.lastMouse = gLOprops.mouse
  gLOprops.mouse = _mouse.mouseDown
  if (gLOprops.mouse)*(gLOprops.lastMouse=0) then
    gLOprops.mouseClick = 1
  end if
  
  if gLOprops.mouse = 0 then
    gLOprops.mouseClick = 0
  end if
  
  lc = _mouse.mouseLoc+point(30,30)
  if lc.locH > 1366-300 then
    lc.locH = 1366-300
  end if
  
  --sprite(50).loc = lc
  
  me.goToEditor()
  
  -- put _key.keyCode
  
  go the frame
end


on buttonClicked me, bttn
  case bttn of
    "button geometry editor":
      _movie.go(15)
    "button tile editor":
      _movie.go(25)
    "button effects editor":
      _movie.go(34)
    "button light editor":
      _movie.go(38)
    "button render level":
      global gViewRender
      gViewRender = 1 - getBoolConfig("Fast render")
      _movie.go(43)
    "button test render":
      newMakeLevel(gLoadedName)
      _movie.go(8)
    "button save project":
      levelName = gLoadedName
      member("projectNameInput").text = gLoadedName
      _movie.go(11)
    "button load project":
      _movie.go(2)
    "button standard medium":
      gLevel.defaultTerrain = 1-gLevel.defaultTerrain
      sprite(112).loc = point(312,312)+point(-1000+1000*gLevel.defaultTerrain, 0)
    "button light type":
      gLOprops.light = 1-gLOprops.light
    "button mass render":
      global   massRenderSelectL 
      massRenderSelectL = []
      _movie.go(4)
      
    "button prop editor":
      _movie.go(23)
      
    "button level size":
      _movie.go(19)
      
      member("widthInput").text = gLOprops.size.locH
      member("heightInput").text = gLOprops.size.locV
      
      global newSize, extraBufferTiles
      newSize = [gLOprops.size.locH, gLOprops.size.locV, 0, 0]
      extraBufferTiles = gLOProps.extraTiles.duplicate()
      
      member("extraTilesLeft").text = extraBufferTiles[1]
      member("extraTilesTop").text = extraBufferTiles[2]
      member("extraTilesRight").text = extraBufferTiles[3]
      member("extraTilesBottom").text = extraBufferTiles[4]
      
      member("addTilesTop").text = "0"
      member("addTilesLeft").text = "0"
      
      
    "button cameras":
      _movie.go(32)
      
    "button environment editor":
      _movie.go(30)
      
    "button exit lock":
      if _movie.window.sizeState <> #minimized and _movie.exitLock = TRUE then
        _movie.exitLock = FALSE
      else
        _movie.exitLock = TRUE
      end if
      
    "button grid snap":
      if snapToGrid = 0 and preciseSnap = 0 then
        snapToGrid = 1
        preciseSnap = 0
        stg = 1
        ps = 0
      else if snapToGrid = 1 and preciseSnap = 0 then
        snapToGrid = 0
        preciseSnap = 1
        stg = 0
        ps = 1
      else if preciseSnap = 1 and snapToGrid = 0 then
        snapToGrid = 0
        preciseSnap = 0
        stg = 0
        ps = 0
      else if preciseSnap = 1 and snapToGrid = 1 then
        snapToGrid = 0
        preciseSnap = 0
        stg = 0
        ps = 0
      end if
      
    "button update preview":
      
      
      
      repeat with a = 1 to 3 then
        miniLvlEditDraw(a)
      end repeat
      
      sav = gLeProps.camPos
      gLeProps.camPos = point(0,0)
      cols = gLOprops.size.loch
      rows = gLOprops.size.locv
      member("levelEditImageShortCuts").image = image(cols*5, rows*5, 1)
      drawShortCutsImg(rect(1,1,cols,rows), 5, 1)
      gLeProps.camPos = sav
      
    "button prio cam":
      
      gPrioCam = gPrioCam + 1
      global gCameraProps
      if(gPrioCam > gCameraProps.cameras.count)then
        gPrioCam = 0
      end if
      
      if(gPrioCam = 0) then
        member("PrioCamText").text = "NONE"
        sprite(123).rect = rect(-100, -100, -100, -100)
      else 
        member("PrioCamText").text = "Will render camera " & gPrioCam & " first"
        mapRect = sprite(115).rect
        cPos = gCameraProps.cameras[gPrioCam]+point(0.001, 0.001)
        
        -- relativeRect = rect(cPos.locH / (gLOprops.size.locH*20), cPos.locV / (gLOprops.size.locV*20), (cPos.locH+1400) / (gLOprops.size.locH*20), (cPos.locV + 800) / (gLOprops.size.locV*20))
        
        sprite(123).rect = rect(lerp(mapRect.left, mapRect.right, cPos.locH / (gLOprops.size.locH*20)) , lerp(mapRect.top, mapRect.bottom, cPos.locV / (gLOprops.size.locV*20)), lerp(mapRect.left, mapRect.right, (cPos.locH + 1366) / (gLOprops.size.locH*20)), lerp(mapRect.top, mapRect.bottom, (cPos.locV + 768) / (gLOprops.size.locV*20)))
        
      end if
      
    "button lvlPropOutput":
      if lvlPropOutput = FALSE then
        lvlPropOutput = TRUE
      else
        lvlPropOutput = FALSE
      end if
      
  end case
  
end


on goToEditor me 
  if dontRunStuff() then return
  
  goFrm = 0
  if checkCustomKeybind(#GoTooverview, "1") then
    goFrm = 9
  else  if checkCustomKeybind(#GoTogeometry, "2") then
    goFrm = 15
  else  if checkCustomKeybind(#GoTotiles, "3") then
    goFrm = 25
  else  if checkCustomKeybind(#GoTocameras, "4") then
    goFrm = 32
  else  if checkCustomKeybind(#GoTolight, "5") then
    goFrm = 38
  else  if checkCustomKeybind(#GoToroomsize, "6") then
    goFrm = 19
    member("widthInput").text = gLOprops.size.locH
    member("heightInput").text = gLOprops.size.locV
    
    global newSize, extraBufferTiles
    newSize = [gLOprops.size.locH, gLOprops.size.locV, 0, 0]
    extraBufferTiles = gLOProps.extraTiles.duplicate()
    
    member("extraTilesLeft").text = extraBufferTiles[1]
    member("extraTilesTop").text = extraBufferTiles[2]
    member("extraTilesRight").text = extraBufferTiles[3]
    member("extraTilesBottom").text = extraBufferTiles[4]
    
    member("addTilesTop").text = "0"
    member("addTilesLeft").text = "0"
  else  if checkCustomKeybind(#GoToeffects, "7") then
    goFrm = 34
  else  if checkCustomKeybind(#GoToprops, "8") then
    goFrm = 23
  else  if checkCustomKeybind(#GoToenvironment, "9") then
    goFrm = 30
  else if checkMinimize() then
    _player.appMinimize()
  else if checkExit() then
    _player.quit()
    --else if _key.keyPressed(59) and _movie.window.sizeState <> #minimized then
    --if showControls = FALSE then
    --  showControls = TRUE
    --end if
    
  else if checkCustomKeybind(#SaveLevel, "0") then
    levelName = gLoadedName
    member("projectNameInput").text = gLoadedName
    goFrm = 13
    --  else if _key.keyPressed(BACKSPACE) and _movie.window.sizeState <> #minimized then
    --    hideHelpClick = TRUE
    
  else if checkCustomKeybind(#Render, 111) then
    global gViewRender
    gViewRender = 1
    goFrm = 43
  else if checkCustomKeybind(#ExportGeometry, VOID) then
    newMakeLevel(gLoadedName)
    goFrm = 8
  end if
  
  checkDebugKeybinds()
  
  if goFrm <> 0 then
    repeat with q = 1 to 22 then
      sprite(q).visibility = 1
    end repeat
    repeat with q = 800 to 820 then
      sprite(q).visibility = (goFrm = 15)
    end repeat
    _movie.go(goFrm)
  end if
end