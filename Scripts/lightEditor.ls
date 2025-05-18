global gTEprops, gLEProps, gTiles, gEEProps, gEffects, gLightEProps, geverysecond, firstFrame, glgtimgQuad, gDirectionKeys, showControls, gCustomLights, gLastImported, gFSLastTm, gFSFlag


on exitFrame me
  global gLELastMouse, gLELastMoving
  
  if (showControls) then
    sprite(189).blend = 100
  else
    sprite(189).blend = 0
  end if
  
  if dontRunStuff() then
    gLightEProps.lastTm = _system.milliseconds
    gFSLastTm = _system.milliseconds
    go the frame
    return
  end if
  
  gFSFlag = false
  if _system.milliseconds - gFSLastTm > 10 then
    gFSFlag = true
    gFSLastTm = _system.milliseconds
  end if
  
  --  if checkKey("N") then
  --    gLightEProps.lightObjects.add([#ps:point(1040/2, 800/2), #sz:point(50, 70), #rt:0, #clr:0])
  --    gLightEProps.edObj = gLightEProps.lightObjects.count
  --  end if
  
  repeat with q = 1 to 4 then
    if (me.getDirection(q)) then
      if gFSFlag then
        if (gDirectionKeys[q] = 0) or (gDirectionKeys[q] > 20 and (gDirectionKeys[q] mod 2) = 0) then
          fast = checkCustomKeybind(#MoveFast, 83)
          faster = checkCustomKeybind(#MoveFaster, 85)
          gLEProps.camPos = gLEProps.camPos + [point(-1, 0), point(0,-1), point(1,0), point(0,1)][q] * (1 + 9 * fast + 34 * faster)
          if not checkCustomKeybind(#MoveOutside, 92) then
            if gLEProps.camPos.loch < -26 then
              gLEProps.camPos.loch = -26
            end if
            if gLEProps.camPos.locv < -18 then
              gLEProps.camPos.locv = -18
            end if  
            if gLEProps.camPos.loch > gLEprops.matrix.count-56 then
              gLEProps.camPos.loch = gLEprops.matrix.count-56
            end if
            if gLEProps.camPos.locv > gLEprops.matrix[1].count-37 then
              gLEProps.camPos.locv = gLEprops.matrix[1].count-37
            end if
          end if
        end if
        gDirectionKeys[q] = gDirectionKeys[q] + 1
      end if
    else
      gDirectionKeys[q] = 0
    end if
    --script("propEditor").renderPropsImage()
  end repeat
  
  
  
  if me.checkKey("Z") then
    gLightEProps.col = (1-gLightEProps.col)
  end if
  if _mouse.rightmouseDown then
    gLightEProps.rot = lookAtPoint(gLightEProps.pos, _mouse.mouseLoc)
  else
    gLightEProps.pos = _mouse.mouseLoc
  end if
  
  if checkCustomKeybind(#LightMapStretchTL, "C") then
    glgtimgQuad[1] = _mouse.mouseLoc + gLEProps.camPos*20
  else if checkCustomKeybind(#LightMapStretchTR, "V") then
    glgtimgQuad[2] = _mouse.mouseLoc + gLEProps.camPos*20
  else if checkCustomKeybind(#LightMapStretchBL, "B") then
    glgtimgQuad[3] = _mouse.mouseLoc + gLEProps.camPos*20
  else if checkCustomKeybind(#LightMapStretchBR, "N") then
    glgtimgQuad[4] = _mouse.mouseLoc + gLEProps.camPos*20
  end if
  
  if me.checkKey("M") then
    dupl = image(member("lightImage").image.width, member("lightImage").image.height, 1)--member("lightImage").image.duplicate()
    dupl.copypixels(  member("lightImage").image, dupl.rect, dupl.rect)
    --  put glgtimgQuad+[point(108,108),point(108,108),point(108,108),point(108,108)]
    era = image(member("lightImage").image.width, member("lightImage").image.height, 1)
    member("lightImage").image.copypixels(era, member("lightImage").image.rect, era.rect)
    member("lightImage").image.copypixels(dupl, glgtimgQuad, dupl.rect)
    glgtimgQuad = [point(0,0), point(member("lightImage").image.width,0), point(member("lightImage").image.width,member("lightImage").image.height), point(0,member("lightImage").image.height)]
    
  end if
  
  if _system.milliseconds -  gLightEProps.lastTm > 10 then
    if checkCustomKeybind(#LightScaleVerticalIncrease, "W") then
      gLightEProps.sz.locV = gLightEProps.sz.locV + 1
    else if checkCustomKeybind(#LightScaleVerticalDecrease, "S") then
      gLightEProps.sz.locV = gLightEProps.sz.locV - 1
    end if
    
    if checkCustomKeybind(#LightScaleHorizontalIncrease, "D") then
      gLightEProps.sz.locH = gLightEProps.sz.locH + 1
    else if checkCustomKeybind(#LightScaleHorizontalDecrease, "A") then
      gLightEProps.sz.locH = gLightEProps.sz.locH - 1
    end if
    
    if checkCustomKeybind(#LightRotateLeft, "Q") then
      gLightEProps.rot = gLightEProps.rot - 1
    else if checkCustomKeybind(#LightRotateRight, "E") then
      gLightEProps.rot = gLightEProps.rot + 1
    end if
    
    if checkCustomKeybind(#LightAngleLeft, "J") then
      gLightEProps.lightAngle = restrict( gLightEProps.lightAngle -1, 0, 360)--( gLightEProps.lightAngle -1, 90, 180)
      if gLightEProps.lightAngle = 0 then
        gLightEProps.lightAngle = 360
      end if
    else if checkCustomKeybind(#LightAngleRight, "L") then
      gLightEProps.lightAngle = restrict( gLightEProps.lightAngle +1, 0, 360)--( gLightEProps.lightAngle +1, 90, 180)
      if gLightEProps.lightAngle = 360 then
        gLightEProps.lightAngle = 0
      end if
    end if
    
    if geverysecond then
      if checkCustomKeybind(#LightAngleDecrease, "I") then
        gLightEProps.flatness = restrict( gLightEProps.flatness - 1, 1, 10)
      else if checkCustomKeybind(#LightAngleIncrease, "K") then
        gLightEProps.flatness = restrict( gLightEProps.flatness + 1, 1, 10)
      end if
      geverysecond = 0
    else 
      geverysecond = 1
    end if
    
    gLightEProps.lastTm = _system.milliseconds
  end if
  
  
  l = ["pxl", "squareLightEmpty", "bigCircle", "discLightEmpty", "leaves", "oilyLight", "directionalLight", "blobLight1", "blobLight2", "wormsLight", "crackLight", "squareishLight", "holeLight", "roundedRectLight", "roundedRectLightEmpty", "triangleLight", "triangleLightEmpty", "curvedTriangleLight", "curvedTriangleLightEmpty", "pentagonLight", "pentagonLightEmpty", "hexagonLight", "hexagonLightEmpty", "octagonLight", "octagonLightEmpty", "DR1DestLight", "DR2DestLight", "DR3DestLight"]
  curr = 1
  repeat with s = 1 to l.count then
    if l[s]=gLightEProps.paintShape then
      curr = s
      exit repeat
    end if 
  end repeat
  repeat with s = 1 to gCustomLights.count then
    if gCustomLights[s] = gLastImported then
      curr = s + l.count
      exit repeat
    end if
  end repeat
  
  mv = 0
  if me.checkKey("r") then
    mv = -1
  else if me.checkKey("f") then
    mv = 1
  end if
  
  if mv <> 0 then
    curr = restrict(curr + mv, 1, l.count + gCustomLights.count)
    if curr <= l.count then
      sprite(181).member = member(l[curr])
      sprite(182).member = member(l[curr])
      gLightEProps.paintShape = l[curr]
    else
      lightMem = member("previewImprt")
      member("previewImprt").importFileInto("Lights\" & gCustomLights[curr - l.count])
      lightMem.name = "previewImprt"
      lmiw = lightMem.image.width-1
      lmih = lightMem.image.height-1
      --lightMem.image.copypixels(lightMem.image, [point(lmiw,0),point(0,0),point(0,lmih),point(lmiw,lmih)], lightMem.image.rect, {#ink:0})
      gLastImported = gCustomLights[curr - l.count]
      sprite(181).member = lightMem
      sprite(182).member = lightMem
      gLightEProps.paintShape = "previewImprt"
    end if
  end if
  
  
  --    
  --  end if
  --  
  --  member("lightImage").image.copypixels(member("pxl").image, rect(0,0,1040,800), rect(0,0,1,1))
  --  
  --  repeat with obj in gLightEProps.lightObjects then
  dir1 = degToVec(gLightEProps.rot)
  dir2 = degToVec(gLightEProps.rot+90)
  
  angleAdd = degToVec(gLightEProps.lightAngle)*(gLightEProps.flatNess*10)--*2.8*(gLightEProps.flatNess+1)*10
  
  dspPos = point((1366/2)-(member("lightImage").image.width/2), (768/2)-(member("lightImage").image.height/2))
  
  dspPos = dspPos - point(150, 150)
  
  q = [gLightEProps.pos, gLightEProps.pos, gLightEProps.pos, gLightEProps.pos] + [gLEProps.camPos*20, gLEProps.camPos*20,gLEProps.camPos*20, gLEProps.camPos*20]
  q = q + [-dir2*gLightEProps.sz.locH - dir1*gLightEProps.sz.locV, dir2*gLightEProps.sz.locH - dir1*gLightEProps.sz.locV, dir2*gLightEProps.sz.locH + dir1*gLightEProps.sz.locV, -dir2*gLightEProps.sz.locH + dir1*gLightEProps.sz.locV]
  if curr > l.count then
    q = flipQuadV(q)
    q = flipQuadH(q)
  end if
  
  --    
  --    member("lightImage").image.copypixels(member("pxl").image, q, rect(0,0,1,1), {#color:obj.clr})
  --  end repeat
  
  gLightEProps.keys.m1 = _mouse.mouseDown and _movie.window.sizeState <> #minimized
  if (gLightEProps.keys.m1)and(firstFrame<>1) then
    member("lightImage").image.copypixels(member(gLightEProps.paintShape).image, q - [dspPos, dspPos, dspPos, dspPos], member(gLightEProps.paintShape).image.rect, {#color:gLightEProps.col*255, #ink:36})
  end if
  if gLightEProps.keys.m1 = 0 then
    firstFrame = 0
  end if
  gLightEProps.lastKeys.m1 = gLightEProps.keys.m1
  
  
  
  sprite(181).quad = q - [gLEProps.camPos*20, gLEProps.camPos*20,gLEProps.camPos*20, gLEProps.camPos*20]
  sprite(182).quad = q - [gLEProps.camPos*20, gLEProps.camPos*20,gLEProps.camPos*20, gLEProps.camPos*20]
  sprite(181).color = color((1-gLightEProps.col)*255, (1-gLightEProps.col)*255, (1-gLightEProps.col)*255)
  
  sprite(180).quad = glgtimgQuad - [gLEProps.camPos*20, gLEProps.camPos*20,gLEProps.camPos*20, gLEProps.camPos*20] + [dspPos, dspPos, dspPos, dspPos]
  
  sprite(185).rect = rect(point(850, 650), point(850, 650)) + rect(-50, -50, 50, 50)
  
  rad = gLightEProps.flatNess*10--*0.1*50
  
  sprite(186).rect = rect(point(850, 650), point(850, 650)) + rect(-rad, -rad, rad, rad)
  sprite(187).loc = point(850, 650) - degToVec(gLightEProps.lightAngle)*rad
  
  
  sprite(176).loc = point(1366/2, 768/2) - point(150, 150) + (angleAdd*2) - gLEProps.camPos*20
  sprite(179).loc = point(1366/2, 768/2) - point(150, 150) - gLEProps.camPos*20
  
  sprite(175).loc = point(1366/2, 768/2) - gLEProps.camPos*20 
  sprite(178).loc = point(1366/2, 768/2) - gLEProps.camPos*20
  
  -- sprite(18).rect = 
  
  -- Move the entire lightmap
  if checkCustomKeybind(#LightMapMove, "X") and not (checkCustomKeybind(#LightMapStretchTL, "C") or checkCustomKeybind(#LightMapStretchTR, "V") or checkCustomKeybind(#LightMapStretchBL, "B") or checkCustomKeybind(#LightMapStretchBR, "N")) then
    gLELastMoving = true
    mouseMV = _mouse.mouseLoc - gLELastMouse
    sprite(176).loc = sprite(176).loc + mouseMV
    sprite(179).loc = sprite(179).loc + mouseMV
  else if gLELastMoving then
    gLELastMoving = false
    mouseMV = _mouse.mouseLoc - gLELastMouse
    newQd = [point(0,0), point(member("lightImage").image.width,0), point(member("lightImage").image.width,member("lightImage").image.height), point(0,member("lightImage").image.height)]
    newQd = newQd + [mouseMV, mouseMV, mouseMV, mouseMV]
    dupl = image(member("lightImage").image.width, member("lightImage").image.height, 1)
    dupl.copypixels(member("lightImage").image, dupl.rect, dupl.rect)
    era = image(member("lightImage").image.width, member("lightImage").image.height, 1)
    member("lightImage").image.copypixels(era, member("lightImage").image.rect, era.rect)
    member("lightImage").image.copypixels(dupl, newQd, dupl.rect)
  else
    gLELastMouse = _mouse.mouseLoc
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


on checkKey me, key
  rtrn = 0
  
  kb = VOID
  case key of
    "Z":
      kb = #LightSwitchMode
    "M":
      kb = #LightMapStretchApply
    "f":
      kb = #LightSelectNext
    "r":
      kb = #LightSelectPrev
  end case
  
  gLightEProps.keys[symbol(key)] = checkCustomKeybind(kb, key)
  if (gLightEProps.keys[symbol(key)])and(gLightEProps.lastKeys[symbol(key)]=0) then
    rtrn = 1
  end if
  gLightEProps.lastKeys[symbol(key)] = gLightEProps.keys[symbol(key)]
  return rtrn
end





