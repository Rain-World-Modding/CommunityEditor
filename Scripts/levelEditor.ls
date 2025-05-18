global gLEProps, gDirectionKeys, gLOprops, gEnvEditorProps, showControls, gFSLastTm, gFSFlag


on exitFrame me
  if (showControls) then
    sprite(93).blend = 100
  else
    sprite(93).blend = 0
  end if
  
  if dontRunStuff() then
    gFSLastTm = _system.milliseconds
    go the frame
    return
  end if
  
  gFSFlag = false
  if _system.milliseconds - gFSLastTm > 10 then
    gFSFlag = true
    gFSLastTm = _system.milliseconds
  end if
  
  
  repeat with q = 1 to 4 then
    if (me.getDirection(q)) then
      if gFSFlag then
        if (gDirectionKeys[q] = 0) or (gDirectionKeys[q] > 20 and (gDirectionKeys[q] mod 2) = 0) then
          fast = checkCustomKeybind(#MoveFast, 83)
          faster = checkCustomKeybind(#MoveFaster, 85)
          gLEProps.camPos = gLEProps.camPos + [point(-1, 0), point(0,-1), point(1,0), point(0,1)][q] * (1 + 9 * fast + 34 * faster)
          if not checkCustomKeybind(#MoveOutside, 92) then
            if gLEProps.camPos.loch < -1 then
              gLEProps.camPos.loch = -1
            end if
            if gLEProps.camPos.locv < -1 then
              gLEProps.camPos.locv = -1
            end if  
            if gLEProps.camPos.loch > gLEprops.matrix.count-51 then
              gLEProps.camPos.loch = gLEprops.matrix.count-51
            end if
            if gLEProps.camPos.locv > gLEprops.matrix[1].count-39 then
              gLEProps.camPos.locv = gLEprops.matrix[1].count-39
            end if
          end if
          
          lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 1)
          lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 2)
          lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 3)
          drawShortCutsImg(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 16) 
        end if
        gDirectionKeys[q] = gDirectionKeys[q] + 1
      end if
    else
      gDirectionKeys[q] = 0
    end if
  end repeat
  
  call(#newUpdate, gLEProps.levelEditors)
  
  rct = rect(0,0,gLOprops.size.loch, gLOprops.size.locv) + rect(gLOProps.extraTiles[1], gLOProps.extraTiles[2], -gLOProps.extraTiles[3], -gLOProps.extraTiles[4]) - rect(gLEProps.camPos, gLEProps.camPos)
  sprite(172).rect = (rct.intersect(rect(0,0,52,40))+rect(11, 1, 11, 1))*rect(16,16,16,16)
  
  if gEnvEditorProps.waterLevel = -1 then
    sprite(169).rect = rect(0,0,0,0)
  else
    rct = rect(0, gLOprops.size.locv-gEnvEditorProps.waterLevel-gLOProps.extraTiles[4], gLOprops.size.loch, gLOprops.size.locv) - rect(gLEProps.camPos, gLEProps.camPos)
    sprite(169).rect = ((rct.intersect(rect(0,0,52,40))+rect(11, 1, 11, 1))*rect(16,16,16,16))+rect(0, -8, 0, 0)
  end if
  
  script("levelOverview").goToEditor()
  go the frame
end



on getDirection me, q
  -- check order: left, up, right, down
  k = [#MoveLeft, #MoveUp, #MoveRight, #MoveDown][q]
  orig = [86, 91, 88, 84][q]
  return checkCustomKeybind(k, orig)
end



