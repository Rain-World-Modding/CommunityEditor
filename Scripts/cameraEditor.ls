global gLOprops, gCameraProps, SHOWQUADS, facH, facV, showControls

on exitFrame me
  if showControls then
    sprite(91).blend = 100
  else
    sprite(91).blend = 0
  end if
  
  if dontRunStuff() then
    go the frame
    return
  end if
  
  if (getBoolConfig("Camera editor border fix")) then
    facH = 1366.0/gLOprops.size.locH
    facV = 768.0/gLOprops.size.locV
  else
    if (gLOprops.size.locH > gLOprops.size.locV) then
      facH = 1366.0/gLOprops.size.locH
      facV = 1366.0/gLOprops.size.locH
    else
      facH = 768.0/gLOprops.size.locV
      facV = 768.0/gLOprops.size.locV
    end if
  end if
  
  if(facH > 16) then 
    facH = 16
  end if
  if(facV > 16) then 
    facV = 16
  end if
  
  rct = rect(1366/2, 768/2, 1366/2, 768/2) + rect(-gLOprops.size.locH*0.5*facH, -gLOprops.size.locV*0.5*facV, gLOprops.size.locH*0.5*facH, gLOprops.size.locV*0.5*facV)
  
  sprite(90).rect = rct + rect(gLOprops.extraTiles[1]*facH, gLOprops.extraTiles[2]*facV, -gLOprops.extraTiles[3]*facH, -gLOprops.extraTiles[4]*facV)
  
  repeat with q = 2 to 8 then
    sprite(q).rect = rct
  end repeat
  
  sprite(13).rect = rct
  
  if (checkKey("n")) then
    gCameraProps.cameras.add(point(gLOprops.size.locH*10, gLOprops.size.locV*10))
    gCameraProps.quads.add([[0,0],[0,0],[0,0],[0,0]])
    gCameraProps.selectedCamera = gCameraProps.cameras.count
  end if
  
  if(gCameraProps.selectedCamera > 0) then
    mouseOverCamera = gCameraProps.selectedCamera
  else
    mouseOverCamera = 0
    smallstDist = 10000
    repeat with q = 1 to gCameraProps.cameras.count then
      pos = point(1366/2, 768/2) + point(-gLOprops.size.locH*0.5*facH, -gLOprops.size.locV*0.5*facV) + point((gCameraProps.cameras[q]/20).locH*facH, (gCameraProps.cameras[q]/20).locV*facV) + point(35*facH, 20*facV)
      if (diag(pos,_mouse.mouseLoc) < smallstDist) then
        mouseOverCamera = q
        smallstDist = diag(pos,_mouse.mouseLoc)
      end if
    end repeat
  end if
  
  if(mouseOverCamera > 0)then
    pos = point(1366/2, 768/2) + point(-gLOprops.size.locH*0.5*facH, -gLOprops.size.locV*0.5*facV) + point((gCameraProps.cameras[mouseOverCamera]/20).locH*facH, (gCameraProps.cameras[mouseOverCamera]/20).locV*facV)
    smallstDist = 10000
    closestCorner = 0
    repeat with q = 1 to 4 then
      if(diag(_mouse.mouseLoc, pos + [point(0,0), point(70*facH,0), point(70*facH, 40*facV), point(0, 40*facV)][q]) < smallstDist) then
        smallstDist = diag(_mouse.mouseLoc, pos + [point(0,0), point(70*facH,0), point(70*facH, 40*facV), point(0, 40*facV)][q])
        closestCorner = q
      end if
    end repeat
    
    if(closestCorner > 0) then
      cornerPos = pos + [point(0,0), point(70*facH,0), point(70*facH, 40*facV), point(0, 40*facV)][closestCorner]
      
      linePos = pos+point(35*facH, 20*facV)
      
      if(SHOWQUADS > 0) then SHOWQUADS = SHOWQUADS - 1
      
      if(checkCustomKeybind(#CameraAngleLeft, "J"))then
        gCameraProps.quads[mouseOverCamera][closestCorner][1] = gCameraProps.quads[mouseOverCamera][closestCorner][1] - 2
        linePos = cornerPos
        SHOWQUADS = 20
      else if (checkCustomKeybind(#CameraAngleRight, "L"))then
        gCameraProps.quads[mouseOverCamera][closestCorner][1] = gCameraProps.quads[mouseOverCamera][closestCorner][1] + 2
        linePos = cornerPos
        SHOWQUADS = 20
      end if
      if (checkCustomKeybind(#CameraAngleIncrease, "I")) then
        gCameraProps.quads[mouseOverCamera][closestCorner][2] = gCameraProps.quads[mouseOverCamera][closestCorner][2] + (1.0/20.0)
        if ( gCameraProps.quads[mouseOverCamera][closestCorner][2] > 1 and not checkCustomKeybind(#CameraAngleUnlock, " "))then 
          gCameraProps.quads[mouseOverCamera][closestCorner][2] = 1
        end if
        SHOWQUADS = 20
      else if (checkCustomKeybind(#CameraAngleDecrease, "K")) then
        gCameraProps.quads[mouseOverCamera][closestCorner][2] = gCameraProps.quads[mouseOverCamera][closestCorner][2] - (1.0/20.0)
        if ( gCameraProps.quads[mouseOverCamera][closestCorner][2] < 0)then 
          gCameraProps.quads[mouseOverCamera][closestCorner][2] = 0
        end if
        SHOWQUADS = 20
      end if
      
      cornerPos = cornerPos + degToVecFac2(gCameraProps.quads[mouseOverCamera][closestCorner][1], facH, facV)*4*gCameraProps.quads[mouseOverCamera][closestCorner][2]
      
      sprite(89).rect = rect(linePos, cornerPos)
      sprite(89).member.lineDirection = ((linePos.locH > cornerPos.locH)or(linePos.locV > cornerPos.locV))and((linePos.locH < cornerPos.locH)or(linePos.locV < cornerPos.locV))
    end if
  end if
  
  if (gCameraProps.selectedCamera <> 0)then
    gCameraProps.cameras[gCameraProps.selectedCamera] = (_mouse.mouseloc/point(1366.0, 768.0))*point(gLOprops.size.locH*20, gLOprops.size.locV*20) - point(35.0*20, 20.0*20)
    
    -- Snapping in X and Y
    -- Finding closest camera in either coordinate
    snapX = 0
    snapY = 0
    repeat with q = 1 to gCameraProps.cameras.count then
      -- Aligning on the X axis means finding something close in the Y coordinate
      if (checkCustomKeybind(#CameraSnapVertical, 124) or checkCustomKeybind(#CameraSnapBoth, "o"))and(gCameraProps.selectedCamera <> q) then
        if (snapY = 0) then
          if (abs(gCameraProps.cameras[gCameraProps.selectedCamera].locv - gCameraProps.cameras[q].locv)<80) then
            snapY = q
          end if
        else if (snapY <> 0) then
          if (abs(gCameraProps.cameras[gCameraProps.selectedCamera].locv-gCameraProps.cameras[q].locv) < abs(gCameraProps.cameras[gCameraProps.selectedCamera].locv-gCameraProps.cameras[snapY].locv)) then
            snapY = q
          end if
        end if
      end if
      
      if (checkCustomKeybind(#CameraSnapHorizontal, 126) or checkCustomKeybind(#CameraSnapBoth, "o"))and(gCameraProps.selectedCamera <> q) then
        if (snapX = 0) then
          if (abs(gCameraProps.cameras[gCameraProps.selectedCamera].loch-gCameraProps.cameras[q].loch)<80) then
            snapX = q
          end if
        else if (snapX <> 0) then
          if (abs(gCameraProps.cameras[gCameraProps.selectedCamera].loch-gCameraProps.cameras[q].loch) < abs(gCameraProps.cameras[gCameraProps.selectedCamera].loch-gCameraProps.cameras[snapX].loch)) then
            snapX = q
          end if
        end if
      end if
    end repeat
    
    -- Apply snapping if found camera
    if (snapY <> 0) then
      gCameraProps.cameras[gCameraProps.selectedCamera].locv = gCameraProps.cameras[snapY].locv
    end if
    if (snapX <> 0) then
      gCameraProps.cameras[gCameraProps.selectedCamera].loch = gCameraProps.cameras[snapX].loch
    end if
    -- Snapping end
    
    if (checkKey("d"))and( gCameraProps.cameras.count > 1) then
      gCameraProps.cameras.Deleteat(gCameraProps.selectedCamera)
      gCameraProps.quads.Deleteat(gCameraProps.selectedCamera)
      sprite(67+gCameraProps.selectedCamera).blend = 0
      gCameraProps.selectedCamera = 0
    end if
    
    if (checkKey("p")) then
      gCameraProps.selectedCamera = 0
    end if
    
  else if (checkKey("e"))and(mouseOverCamera>0) then
    gCameraProps.selectedCamera =  mouseOverCamera
  end if
  
  me.drawAll()
  
  script("levelOverview").goToEditor()
  
  go the frame
end

on drawAll me
  repeat with q = 1 to gCameraProps.cameras.count then
    pos = point(1366/2, 768/2) + point(-gLOprops.size.locH*0.5*facH, -gLOprops.size.locV*0.5*facV) + point((gCameraProps.cameras[q]/20).locH*facH, (gCameraProps.cameras[q]/20).locV*facV)
    sprite(23+q).rect =  rect(pos, pos)+rect(0, 0, 70*facH, 40*facV)
    --sprite(44+q).rect =  rect(pos, pos)+rect(9.3, 0.8, 60.7*facH, 39.2*facV)
    sprite(44+q).rect =  rect(pos, pos)+rect(9.4*facH, 0.8*facV, 60.6*facH, 39.2*facV)
    
    QD = [pos, pos+point(70*facH,0), pos+point(70*facH, 40*facV), pos+point(0, 40*facV)]
    repeat with c = 1 to 4 then
      QD[c] = QD[c] + degToVecFac2(gCameraProps.quads[q][c][1], facH, facV)*4*gCameraProps.quads[q][c][2]
    end repeat
    
    sprite(67+q).quad = QD
    sprite(67+q).blend = 15 + (SHOWQUADS/20.0) * 40
  end repeat
  repeat with q = gCameraProps.cameras.count+1 to 10 then
    sprite(23+q).rect =  rect(-100, -100, -100, -100)
    sprite(44+q).rect =  rect(-100, -100, -100, -100)
    sprite(67+q).rect =  rect(-100, -100, -100, -100)
  end repeat
end


on checkKey(key)
  rtrn = 0
  
  kb = VOID
  case key of
    "n":
      kb = #NewCamera
    "d":
      kb = #DeleteCamera
    "p":
      kb = #PlaceCamera
    "e":
      kb = #GrabCamera
  end case
  
  gCameraProps.keys[symbol(key)] = checkCustomKeybind(kb, key)
  if (gCameraProps.keys[symbol(key)])and(gCameraProps.lastKeys[symbol(key)]=0) then
    rtrn = 1
  end if
  gCameraProps.lastKeys[symbol(key)] = gCameraProps.keys[symbol(key)]
  return rtrn
end