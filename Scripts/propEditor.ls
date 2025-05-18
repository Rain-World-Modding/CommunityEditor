global gLEProps, TEdraw, gDirectionKeys, gLOprops, gPEprops, gProps, gPEblink, gPEcounter, peScrollPos, peSavedRotat, peSavedFlip, peFreeQuad, peMousePos, lastPeMouse, mouseStill, propSettings, editSettingsProp, peSavedStretch
global ropeModel, settingsPropType, gPEcolors, closestProp, longPropPlacePos, snapToGrid, preciseSnap, stg, ps, showControls, gFSLastTm, gFSFlag


on exitFrame me
  if (showControls) then
    sprite(261).blend = 100
  else
    sprite(261).blend = 0
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
  
  script("levelOverview").exitFrame(me)
  lastPeMouse = peMousePos
  gPEblink = gPEblink + 1
  if(gPEblink > 800)then
    gPEblink = 0
  end if
  
  if(gPEcounter > 0)then
    gPEcounter = gPEcounter - 1
  end if
  
  if(editSettingsProp < 0)then
    member("propMenu").alignment = #left
  else
    member("propMenu").alignment = #center
  end if
  
  if((IsDecal(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV]) = 0)and(propPlaceLayer() <= 5)and(propPlaceLayer() + gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].depth >= 6))then
    
    member("Prop editor warning text").text = "WARNING - this prop will intersect with the play layer!"
    
    
    if (gPEblink < 600)then
      sprite(267).visibility = true
    else
      sprite(267).visibility = false
    end if
    if(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].tp = "antimatter")then
      member("Prop editor warning text").text = "Antimatter prop intersecting play layer - remember to use a restore effect on affected play relevant terrain"
      sprite(266).color = color(255,255,255)
      sprite(267).color = color(255,255,255)
    else
      member("Prop editor warning text").text = "WARNING - this prop will intersect with the play layer!"
      sprite(266).color = color(255, 0, 0)
      sprite(267).color = color(255, 0, 0)
    end if
  else
    sprite(267).visibility = false
    sprite(266).color = color(255,255,255)
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
          
          repeat with l = 1 to 3 then
            lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), l)
            TEdraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), l)
          end repeat
          drawShortCutsImg(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 16)
          renderPropsImage()
        end if
        gDirectionKeys[q] = gDirectionKeys[q] + 1
      end if
    else
      gDirectionKeys[q] = 0
    end if
  end repeat
  
  
  sprite(257).visibility = false
  
  script("levelOverview").goToEditor()
  
  if(editSettingsProp = -1)then
    
    if checkCustomKeybind(#PropRotateSnap, " ") then 
      
      keyW = checkCustomKeybind(#PropSelectUp, "W")
      keyS = checkCustomKeybind(#PropSelectDown, "S")
      keyA = checkCustomKeybind(#PropSelectLeft, "A")
      keyD = checkCustomKeybind(#PropSelectRight, "D")
      
      if keyW and keyA then
        gPEprops.propRotation = 270 + 45
        gPEcounter = 100
      else if keyW and keyD then
        gPEprops.propRotation =  45
        gPEcounter = 100
      else if keyS and keyD then
        gPEprops.propRotation =  90+45
        gPEcounter = 100
      else if keyS and keyA then
        gPEprops.propRotation =  180+45
        gPEcounter = 100
      else if (gPEcounter = 0)then
        if keyW then
          gPEprops.propRotation = 0
        else if keyA then
          gPEprops.propRotation = 270
        else if keyS then
          gPEprops.propRotation = 180
        else if keyD then
          gPEprops.propRotation = 90
        end if
      end if
    else
      if me.checkKey("W") then
        updatePropMenu(point(0, -1))
      end if
      if me.checkKey("S") then
        updatePropMenu(point(0, 1))
      end if
      if me.checkKey("A") then
        updatePropMenu(point(-1, 0))
      end if
      if me.checkKey("D") then
        updatePropMenu(point(1, 0))
      end if
    end if
  else
    if(editSettingsProp > gPEprops.props.count)then
      editSettingsProp = -1
      updatePropMenu(point(0, 0))
    else if _movie.window.sizeState <> #minimized then
      if me.checkKey("W") then
        updatePropSettings(point(0, -1))
      end if
      if me.checkKey("S") then
        updatePropSettings(point(0, 1))
      end if
      if me.checkKey("A") then
        updatePropSettings(point(-1, 0))
      end if
      if me.checkKey("D") then
        updatePropSettings(point(1, 0))
      end if
    end if
  end if
  
  
  
  
  if me.checkKey("Z") then
    gPEprops.color = gPEprops.color + 1
    if(gPEprops.color > gPEcolors.count)then
      gPEprops.color = 0
    end if
    if gPEprops.color = 0 then
      member("Prop Color Text").text = "PROP COLOR: " &"NONE"
      sprite(270).color = color(150, 150, 150)
    else
      member("Prop Color Text").text = "PROP COLOR: " & gPEcolors[gPEprops.color][1]
      sprite(270).color = gPEcolors[gPEprops.color][2]
    end if
  end if
  if(gPEprops.color = 1)then
    sprite(270).color = color(random(255), random(255), random(255))
  end if
  
  if me.checkKey("N") then
    if(editSettingsProp = -1)then
      editSettingsProp = 0
      --  DuplicatePropSettings()
      updatePropSettings(point(0,0))
    else
      editSettingsProp = -1
      updatePropMenu(point(0, 0))
    end if
  end if
  
  if checkCustomKeybind(#PropRotateLeft, "Q") then
    gPEprops.propRotation = gPEprops.propRotation - 0.01
    if checkCustomKeybind(#PropRotateFaster, " ") then
      gPEprops.propRotation = gPEprops.propRotation - 0.1
    end if
    mouseStill = 0
  else if checkCustomKeybind(#PropRotateRight, "E") then
    gPEprops.propRotation = gPEprops.propRotation + 0.01
    if checkCustomKeybind(#PropRotateFaster, " ") then 
      gPEprops.propRotation = gPEprops.propRotation + 0.1
    end if
    mouseStill = 0
  end if
  
  if(gPEprops.propRotation < 0)then
    gPEprops.propRotation = gPEprops.propRotation + 360
  else if (gPEprops.propRotation>=360) then
    gPEprops.propRotation = gPEprops.propRotation - 360
  end if
  
  if checkCustomKeybind(#PropResetFlipVertical, ["Y", " "])then
    gPEprops.propFlipY = 1
  else if  checkCustomKeybind(#PropFlipVertical, ["H", " "])then
    gPEprops.propFlipY = -1
  end if
  if checkCustomKeybind(#PropResetFlipHorizontal, ["G", " "])then
    gPEprops.propFlipX = 1
  else if  checkCustomKeybind(#PropFlipHorizontal, ["J", " "])then
    gPEprops.propFlipX = -1
  end if
  
  stretchSpeed = 0.002
  if checkCustomKeybind(#PropStretchVerticalIncrease, ["Y", "NOT", " "]) then
    gPEprops.propStretchY = gPEprops.propStretchY + stretchSpeed
    mouseStill = 0
  else if checkCustomKeybind(#PropStretchVerticalDecrease, ["H", "NOT", " "]) then
    gPEprops.propStretchY = gPEprops.propStretchY - stretchSpeed
    mouseStill = 0
  end if
  if checkCustomKeybind(#PropStretchHorizontalDecrease, ["G", "NOT", " "]) then
    gPEprops.propStretchX = gPEprops.propStretchX - stretchSpeed
    mouseStill = 0
  else if checkCustomKeybind(#PropStretchHorizontalIncrease, ["J", "NOT", " "]) then
    gPEprops.propStretchX = gPEprops.propStretchX + stretchSpeed
    mouseStill = 0
  end if
  
  if checkCustomKeybind(#PropResetStretch, "T") then
    gPEprops.propStretchX = 1
    gPEprops.propStretchY = 1
  end if
  if checkCustomKeybind(#PropResetTransformations, "R") then
    gPEprops.propStretchX = 1
    gPEprops.propStretchY = 1
    gPEprops.propFlipX = 1
    gPEprops.propFlipY = 1
    gPEprops.propRotation = 0
  end if
  
  if(gPEprops.propStretchY < 0.1) then
    gPEprops.propStretchY = 0.1
  else if (gPEprops.propStretchY > 20) then
    gPEprops.propStretchY = 20
  end if
  
  if(gPEprops.propStretchX < 0.1) then
    gPEprops.propStretchX = 0.1
  else if (gPEprops.propStretchX > 20) then
    gPEprops.propStretchX = 20
  end if
  
  actn1 = 0
  actn2 = 0
  
  if member("propMenu").text = member("propBaseMenu").text then
    if _mouse.mouseDown then
      editSettingsProp = -1
      updatePropMenu(point(0, 0))
    end if
  end if
  
  gPEprops.keys.m1 = _mouse.mouseDown
  if (gPEprops.keys.m1)and(gPEprops.lastKeys.m1=0) then
    actn1 = 1
  end if
  gPEprops.lastKeys.m1 = gPEprops.keys.m1
  
  gPEprops.keys.m2 = _mouse.rightmouseDown
  if (gPEprops.keys.m2)and(gPEprops.lastKeys.m2=0) then
    actn2 = 1
  end if
  gPEprops.lastKeys.m2 = gPEprops.keys.m2
  
  if checkCustomKeybind(#PropVariationMode, "F") then
    if(propSettings.findPos(#variation) <> void) and ((actn1)or(actn2)) then
      propSettings.variation = propSettings.variation + actn1 - actn2
      mn = (1 - settingsPropType.random)
      if(propSettings.variation < mn)then
        propSettings.variation = settingsPropType.vars
      else if(propSettings.variation > settingsPropType.vars)then
        propSettings.variation = mn
      end if
      updateVariedPreview(settingsPropType, propSettings.variation)
      updateCursorText()
    end if
    
    actn1 = 0
    actn2 = 0
  end if
  
  if(actn2)then
    -- Right click to change depth (probably not making custom keybind for)
    if _key.keyPressed(SPACE) then 
      gPEprops.depth  = gPEprops.depth  - 1
    else 
      gPEprops.depth = gPEprops.depth  + 1
    end if
    if(gPEprops.depth < 0)then
      gPEprops.depth = 9
    else if(gPEprops.depth  > 9)then
      gPEprops.depth  = 0
    end if
    updateWorkLayerText()
  end if
  
  
  
  if checkCustomKeybind(#ClearAllProps, ["C","X",48]) then
    -- Clear all props
    sprite(268).visible = true
    sprite(268).color = color(random(255), 0, 0)
    if(actn1)and(_mouse.mouseLoc.inside(rect(25,25,52,52)))then
      clearAllProps()
    end if
    actn1 = 0
  else
    sprite(268).visible = false
  end if
  
  if me.checkKey("L") then
    
    gPEprops.workLayer = gPEprops.workLayer +1
    if gPEprops.workLayer > 3 then
      gPEprops.workLayer = 1
    end if
    
    
    if gPEprops.workLayer = 2 then
      sprite(250).blend = 40
      sprite(251).blend = 40
      
      sprite(252).blend = 90
      sprite(253).blend = 90
      sprite(254).blend = 10
      sprite(255).blend = 10
    else if gPEprops.workLayer = 1 then
      sprite(250).blend = 20
      sprite(251).blend = 20
      sprite(252).blend = 40
      sprite(253).blend = 40
      sprite(254).blend = 90
      sprite(255).blend = 90
    else
      sprite(250).blend = 90
      sprite(251).blend = 90
      sprite(252).blend = 10
      sprite(253).blend = 10
      sprite(254).blend = 10
      sprite(255).blend = 10
    end if
    
    updateWorkLayerText()
    renderPropsImage()
    
  end if
  
  
  
  if(gPEprops.propRotation = 0)then
    dir = point(0, -1)
    perp = point(1, 0)
  else if (gPEprops.propRotation = 90)then
    dir = point(1, 0)
    perp = point(0, 1)
  else if (gPEprops.propRotation = 180)then
    dir = point(0, 1)
    perp = point(-1, 0)
  else if (gPEprops.propRotation = 270)then
    dir = point(-1, 0)
    perp = point(0, -1)
  else
    dir = DegToVec(gPEprops.propRotation)
    perp = giveDirFor90degrToLine(-dir, dir)
  end if
  
  --  if _key.keyPressed(56) and _movie.window.sizeState <> #minimized and preciseSnap = 0 then
  --    preciseSnap = 1
  --    waitSeconds(1)
  --  else if _key.keyPressed(56) and _movie.window.sizeState <> #minimized and preciseSnap = 1 then
  --    preciseSnap = 0
  --    snapToGrid = 1
  --  end if
  
  if (checkCustomKeybind(#PropFreeformTL, "U")=0)and(checkCustomKeybind(#PropFreeformTR, "I")=0)and(checkCustomKeybind(#PropFreeformBR, "O")=0)and(checkCustomKeybind(#PropFreeformBL, "P")=0)and(checkCustomKeybind(#PauseRopeSimulation, "X")=0) then
    peMousePos = _mouse.mouseLoc
    if(preciseSnap)then
      peMousePos.loch = ((peMousePos.locH / 8.0)-0.4999).integer * 8
      peMousePos.locv = ((peMousePos.locv / 8.0)-0.4999).integer * 8
      --point(((gProps[gPEprops.tmPos.locH].tls[gPEprops.tmPos.locV].sz.locH*0.5)+0.4999).integer,((gProps[gPEprops.tmPos.locH].tls[gPEprops.tmPos.locV].sz.locV*0.5)+0.4999).integer)
    end if
    if(snapToGrid)then
      peMousePos.loch = ((peMousePos.locH / 16.0)-0.4999).integer * 16
      peMousePos.locv = ((peMousePos.locv / 16.0)-0.4999).integer * 16
    end if
  end if
  
  if snapToGrid = 0 and preciseSnap = 0 then
    member("buttonSnapText").text = "None"
  else if snapToGrid = 1 and preciseSnap = 0 then
    member("buttonSnapText").text = "Snap To Grid"
  else if preciseSnap = 1 and snapToGrid = 0 then
    member("buttonSnapText").text = "Precise Snap"
  else if preciseSnap = 1 and snapToGrid = 1 then
    member("buttonSnapText").text = "Both tags active, remove one"
  end if
  
  prop = VOID
  if (gPEprops.pmPos.locH <= gProps.count) and (gPEprops.pmPos.locH > 0) and (gPEprops.pmPos.locV > 0) and (gPEprops.pmPos.locV <= gProps[gPEprops.pmPos.locH].prps.count) then
    prop = gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV]
  end if
  
  if (prop <> VOID) and ((prop.tp = "long") or (prop.tp = "customLong")) then
    if(longPropPlacePos = void)then
      gPEprops.propRotation = 0
      gPEprops.propStretchX = 1
      gPEprops.propStretchY = 1
      gPEprops.propFlipX = 1
      gPEprops.propFlipY = 1
      if(actn1)and(_mouse.mouseLoc.inside(rect(16, 16, 848, 656)))then
        longPropPlacePos = peMousePos + (point(-16, -16) + gLEProps.camPos*16)
        actn1 = 0
      end if
    else
      gPEprops.propRotation = lookAtpoint(longPropPlacePos - (point(-16, -16) + gLEProps.camPos*16), peMousePos)+90
      gPEprops.propStretchX = Diag(longPropPlacePos - (point(-16, -16) + gLEProps.camPos*16), peMousePos)/(200.0 * (16.0/20.0))
      peMousePos = (longPropPlacePos - (point(-16, -16) + gLEProps.camPos*16) + peMousePos)/2.0
      gPEprops.propStretchY = 1
      gPEprops.propFlipX = 1
      gPEprops.propFlipY = 1
      if(actn1)and(_mouse.mouseLoc.inside(rect(16, 16, 848, 656)))then
        longPropPlacePos = void
      end if
    end if
  end if
  
  
  
  
  
  lastClosest = closestProp
  closestProp = 0
  
  if(gPEprops.props.count > 0)and((checkCustomKeybind(#PropDelete, "V"))or(checkCustomKeybind(#PropSample, "B"))or(checkCustomKeybind(#PropSampleWithSettings, [" ", "B"]))or(checkCustomKeybind(#PropOptionsPlaced, "M")) ) then
    closestProp = findClosestProp()
  end if
  
  if(editSettingsProp > 0)then
    closestProp = editSettingsProp
  end if
  
  if(closestProp <> lastClosest)and(closestProp < 1)then
    if(["variedDecal", "variedSoft", "variedStandard"].getPos(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].tp)>0)then
      updateVariedPreview(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV], propSettings.variation)
    end if
  end if
  
  if(closestProp > 0)then
    
    qd = gPEprops.props[closestProp][4] 
    offSetPnt = point(16, 16) - gLEProps.camPos*16
    qd = qd + [offSetPnt, offSetPnt, offSetPnt, offSetPnt]
    if(closestProp <> lastClosest)then
      sprite(264).member = propPreviewMember(gProps[gPEprops.props[closestProp][3].locH].prps[gPEprops.props[closestProp][3].locV])
      if(["variedDecal", "variedSoft", "variedStandard"].getPos(gProps[gPEprops.props[closestProp][3].locH].prps[gPEprops.props[closestProp][3].locV].tp)>0)then
        if closestProp = 0 then
          var = propSettings.variation
        else
          var = gPEprops.props[closestProp][5].settings.variation
        end if
        updateVariedPreview(gProps[gPEprops.props[closestProp][3].locH].prps[gPEprops.props[closestProp][3].locV], var)
      end if
    end if
    
    sprite(266).loc = point(-100, -100)
    
    if checkCustomKeybind(#PropDelete, "V") then
      sprite(264).color = color(255,0,0)
      sprite(264).foreColor = 6
      if(actn1) then
        gPEprops.props.deleteAt(closestProp)
        renderPropsImage()
      end if
    else if checkCustomKeybind(#PropSampleWithSettings, [" ", "B"]) then
      sprite(264).color = color(0,100,255)
      sprite(264).foreColor = color(255,255,255)
      if(actn1) then
        -- Sample prop
        gPEprops.pmPos = gPEprops.props[closestProp][3]
        updatePropMenu(point(0, 0))
        
        propSettings = gPEprops.props[closestProp][5].settings
        settingsPropType = gProps[gPEprops.pmPos.loch].prps[gPEprops.pmPos.locV]
        
        editSettingsProp = -1
        
        -- Reset our transforms
        gPEprops.propStretchX = 1
        gPEprops.propStretchY = 1
        gPEprops.propFlipX = 1
        gPEprops.propFlipY = 1
        gPEprops.propRotation = 0
        
        -- Now copy its transform
        newPrp = gProps[gPEprops.props[closestProp][3].locH].prps[gPEprops.props[closestProp][3].locV]
        if ((newPrp.tp <> "long") and (newPrp.tp <> "customLong")) then
          -- Figure out scale, flip, rotation, and free quad :monksilly:
          pq = gPEProps.props[closestProp][4]
          ps = propPreviewMember(newPrp)
          
          -- Start with scale
          pw = (diag(pq[1], pq[2]) + diag(pq[3], pq[4])) / 2
          ph = (diag(pq[1], pq[4]) + diag(pq[2], pq[3])) / 2
          gPEprops.propStretchX = pw / ps.rect.width * 20.0/16.0
          gPEprops.propStretchY = ph / ps.rect.height * 20.0/16.0
          
          -- Figure out flip
          flpX = (pq[1].locH+pq[4].locH)/2 > (pq[2].locH+pq[3].locH)/2
          flpY = (pq[1].locV+pq[2].locV)/2 > (pq[3].locV+pq[4].locV)/2
          if flpX then gPEprops.propFlipX = -1
          if flpY then gPEprops.propFlipY = -1
          
          -- Rotation
          slpeX = (pq[2].locH+pq[3].locH)/2 - (pq[1].locH+pq[4].locH)/2
          slpeY = (pq[2].locV+pq[3].locV)/2 - (pq[1].locV+pq[4].locV)/2
          if slpeX = 0 then
            gPEprops.propRotation = 90
          else
            gPEprops.propRotation = atan(slpeY / slpeX) * 180 / PI
            if gPEprops.propRotation < 0 then gPEprops.propRotation = gPEprops.propRotation + 360
          end if
          
          -- Finally: the remaining quad
          if flpX then pw = -pw
          if flpY then ph = -ph
          pqMid = (pq[1]+pq[2]+pq[3]+pq[4])/4
          peFreeQuad = (pq - [pqMid,pqMid,pqMid,pqMid]) - rotateToQuadFix(rect(-pw/2,ph/2,pw/2,-ph/2), 180+gPEprops.propRotation)
        else
          peFreeQuad = [point(0,0), point(0,0), point(0,0), point(0,0)]
        end if
        
        -- Copy layer, maybe
        cpyDpth = -gPEProps.props[closestProp][1]
        gPEprops.depth = cpyDpth mod 10
        gPEprops.workLayer = (cpyDpth - gPEprops.depth) / 10 + 1
        
        -- Now that we *might* have changed the layer, we also have to update all the graphics and stuff
        if gPEprops.workLayer = 2 then
          sprite(250).blend = 40
          sprite(251).blend = 40
          sprite(252).blend = 90
          sprite(253).blend = 90
          sprite(254).blend = 10
          sprite(255).blend = 10
        else if gPEprops.workLayer = 1 then
          sprite(250).blend = 20
          sprite(251).blend = 20
          sprite(252).blend = 40
          sprite(253).blend = 40
          sprite(254).blend = 90
          sprite(255).blend = 90
        else
          sprite(250).blend = 90
          sprite(251).blend = 90
          sprite(252).blend = 10
          sprite(253).blend = 10
          sprite(254).blend = 10
          sprite(255).blend = 10
        end if
        
        updateWorkLayerText()
        renderPropsImage()
      end if
    else if checkCustomKeybind(#PropSample, "B") then
      sprite(264).color = color(0,255,255)
      sprite(264).foreColor = color(255, 255, 255)
      if(actn1) then
        gPEprops.pmPos = gPEprops.props[closestProp][3]
        updatePropMenu(point(0, 0))
        
        propSettings = gPEprops.props[closestProp][5].settings
        settingsPropType = gProps[gPEprops.pmPos.loch].prps[gPEprops.pmPos.locV]
        
        editSettingsProp = -1
      end if
    else if (editSettingsProp > 0) then
      sprite(264).color = color(0,255,0)
      sprite(264).foreColor = 187
    else if checkCustomKeybind(#PropOptionsPlaced, "M") then
      sprite(264).color = color(0,0,255)
      sprite(264).foreColor = 62
      if(actn1) then
        editSettingsProp = closestProp
        propSettings = gPEprops.props[closestProp][5].settings
        settingsPropType = gProps[gPEprops.props[closestProp][3].loch].prps[gPEprops.props[closestProp][3].locV]
        updatePropSettings(point(0,0))
      end if
    end if
    if(editSettingsProp < 1)then
      sprite(264).blend = restrict(50 + 50*sin((gPEblink/800.0)*PI*4.0), 0, 100)
    else
      sprite(264).blend = restrict(50 + 50*sin((gPEblink/800.0)*PI*8.0), 0, 100)
    end if
  else
    mem = propPreviewMember(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV])
    
    scaleFac = 16.0/20.0
    
    qd = [peMousePos, peMousePos, peMousePos, peMousePos]
    qd[1] = qd[1] + (dir*mem.rect.height * 0.5 * gPEprops.propStretchY * scaleFac * gPEprops.propFlipY) - (perp*mem.rect.width * 0.5 * gPEprops.propStretchX * scaleFac * gPEprops.propFlipX)
    qd[2] = qd[2] + (dir*mem.rect.height * 0.5 * gPEprops.propStretchY * scaleFac * gPEprops.propFlipY) + (perp*mem.rect.width * 0.5 * gPEprops.propStretchX * scaleFac * gPEprops.propFlipX)
    qd[3] = qd[3] - (dir*mem.rect.height * 0.5 * gPEprops.propStretchY * scaleFac * gPEprops.propFlipY) + (perp*mem.rect.width * 0.5 * gPEprops.propStretchX * scaleFac * gPEprops.propFlipX)
    qd[4] = qd[4] - (dir*mem.rect.height * 0.5 * gPEprops.propStretchY * scaleFac * gPEprops.propFlipY) - (perp*mem.rect.width * 0.5 * gPEprops.propStretchX * scaleFac * gPEprops.propFlipX)
    
    if(checkCustomKeybind(#PropFreeformTL, "U"))then
      peFreeQuad[1] = _mouse.mouseLoc - qd[1]
    else if(checkCustomKeybind(#PropFreeformTR, "I"))then
      peFreeQuad[2] = _mouse.mouseLoc - qd[2]
    else if(checkCustomKeybind(#PropFreeformBR, "O"))then
      peFreeQuad[3] = _mouse.mouseLoc - qd[3]
    else if(checkCustomKeybind(#PropFreeformBL, "P"))then
      peFreeQuad[4] = _mouse.mouseLoc - qd[4]
    else if(checkCustomKeybind(#PropResetFreeform, "K")) or (checkCustomKeybind(#PropResetTransformations, "R")) then
      peFreeQuad = [point(0,0), point(0,0), point(0,0), point(0,0)]
    end if
    
    qd = qd + peFreeQuad
    
    if(actn1)and(_mouse.mouseLoc.inside(rect(16, 16, 848, 656)))then
      offSetPnt = point(-16, -16) + gLEProps.camPos*16
      placeProp(qd + [offSetPnt, offSetPnt, offSetPnt, offSetPnt])
    end if
    sprite(264).blend = 50
    sprite(264).color = color(0,0,0)
    sprite(264).foreColor = 255
    sprite(264).member = mem
    sprite(266).loc = peMousePos + point(40, 20)
  end if
  
  sprite(264).quad = qd
  
  propTp = gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].tp
  if (propTp = "rope") or (propTp = "customRope") then
    viewRope = false
    
    
    if(lastPeMouse <> peMousePos) then
      mouseStill = 0
    else
      mouseStill = mouseStill + 1
    end if
    
    if(editSettingsProp > 0)or(checkCustomKeybind(#PropOptionsPlaced, "M"))then
      mouseStill = 0
    end if
    
    if(checkCustomKeybind(#PauseRopeSimulation, "X")=0)then
      if(mouseStill = 10)then
        ropeFrames = 0
        resetRopeProp()
      else if (mouseStill > 10)then
        ropeFrames = ropeFrames + 1
        updateCursorText()
        script("ropeModel").modelRopeUpdate(1, gLEProps.camPos, 16.0/20.0)
        viewRope = true
      end if
    else
      viewRope = true 
    end if
    
    sprite(269).visibility = viewRope
  else
    ropeFrames = 0
    sprite(269).visibility = false
  end if
  
  go the frame
end


on findClosestProp()
  closestProp = 0
  smallestDist = 10000
  
  offSetMousePnt = (peMousePos - point(16, 16) + gLEProps.camPos*16.0) --* 20.0/16.0
  
  repeat with p = 1 to gPEprops.props.count then
    pos = (gPEprops.props[p][4][1] + gPEprops.props[p][4][2] + gPEprops.props[p][4][3] + gPEprops.props[p][4][4])/4.0
    -- pos = pos * 20.0/16.0
    if(diag(offSetMousePnt, pos) < smallestDist)then
      smallestDist = diag(offSetMousePnt, pos)
      closestProp = p
    end if
  end repeat
  
  return closestProp
end

on updateWorkLayerText()
  txt = "Work Layer:" && string(gPEprops.workLayer)
  put RETURN after txt
  put "Prop depth: " & propPlaceLayer() after txt
  member("layerText").text = txt
  if(gPEprops.pmPos.locH > gProps.count) then
    gPEprops.pmPos.locH = 1
  end if
  if(gPEprops.pmPos.locV > gProps[gPEprops.pmPos.locH].prps.count) then
    gPEprops.pmPos.locV = 1
  end if
  updateCursorText()
end

on updateCursorText()
  txt = "Prop depth: " & propPlaceLayer() & " to " & (propPlaceLayer() +  gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].depth)
  if(propSettings <> void)then
    if (propSettings.findPos(#variation) <> void) then
      put RETURN after txt
      put "Variation: " after txt
      if (propSettings.variation = 0)then
        put "Random" after txt
      else
        put propSettings.variation after txt
      end if
    end if
  end if
  
  member("Prop Depth Text").text = txt
end

--on updateCursorTextWhenClosestProp()
--  repeat with p = 1 to gPEprops.props.count then
--    pos = (gPEprops.props[p][5][2][1]
--    -- pos = pos * 20.0/16.0
--    if(diag(offSetMousePnt, pos) < smallestDist)then
--      smallestDist = diag(offSetMousePnt, pos)
--      closestProp = p
--    end if
--  end repeat
--  txt = "Prop depth: " & propPlaceLayer() & " to " & (propPlaceLayer() +  gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].depth)
--  if(propSettings <> void)then
--    if (propSettings.findPos(#variation) <> void) then
--      put RETURN after txt
--      put "Variation: " after txt
--      if (propSettings.variation = 0)then
--        put "Random" after txt
--      else
--        put propSettings.variation after txt
--      end if
--    end if
--  end if
--  
--  member("Prop Depth Text").text = txt
--end

on propPlaceLayer()
  return ((gPEprops.workLayer-1) * 10) + gPEprops.depth
end

on placeProp(qd)
  -- member("propsImage").image.copyPixels(mem.image, qd, mem.image.rect, {#ink:36})
  prop = [-propPlaceLayer(), gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].nm, gPEprops.pmPos, qd, [#settings:propSettings.duplicate()]]
  
  -- if(prop[5] <> void) then
  if(prop[5].settings.findpos(#color) <> void) then
    if gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].tags.getPos("customColorRainBow") > 0 then
      gPEprops.color = 1
    end if
    prop[5].settings.color = gPEprops.color
  end if
  -- end if
  
  case (gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].tp)of
    "rope", "customRope":
      prop[5].addProp(#points, [])
      repeat with q = 1 to ropeModel.segments.count then
        prop[5].points.add(script("ropeModel").SmoothedPos(q))
      end repeat
    "variedDecal", "variedSoft", "variedStandard":
      if(prop[5].settings.variation = 0)then
        prop[5].settings.variation = random(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].vars)
      end if
  end case
  
  
  gPEprops.props.add(prop)
  gPEprops.props.sort()
  renderPropsImage()
  
  if(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].tp = "variedDecal")or(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].tp = "variedSoft")or(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].tp = "variedStandard")then
    if(propSettings.variation = 0)then
      updateVariedPreview(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV], 0)
    end if
  end if
  
  ApplyTransformationTags()
end 

on clearAllProps()
  gPEprops.props = []
  renderPropsImage()
end

on renderPropsImage()
  --gLEProps.camPos
  member("propsImage").image = image(52*16, 40*16, 16)
  member("propsImage2").image = image(52*16, 40*16, 16)
  
  camPosQuad = [gLEProps.camPos*16, gLEProps.camPos*16, gLEProps.camPos*16, gLEProps.camPos*16]
  
  displayLayer = (gPEprops.workLayer-1) * 10
  layer = 29
  
  repeat with p = 1 to gPEprops.props.count then
    prop = gPEprops.props[p]
    propData = gProps[prop[3].locH].prps[prop[3].locV]
    propLayer = -prop[1]
    mem = propPreviewMember(propData)
    blnd = 100 - IsDecal(propData)*40
    
    if(propLayer >= displayLayer) then
      
      
      if(propLayer < layer) then
        repeat with q = 1 to layer - propLayer then
          member("propsImage").image.copyPixels(member("pxl").image, rect(0,0,52*16, 40*16),rect(0,0,1,1), {#blend:10, #color:color(255, 255, 255)})
        end repeat
        layer = propLayer
        
      end if
      
      clr = color(0,0,0)
      if(propData.settings.findpos(#color) <> void) then
        if propData.settings.color > 0 then
          clr = gPEcolors[propData.settings.color]
        end if
      end if
      
      case (propData.tp) of
        "rope", "customRope":
          member("propsImage").image.copyPixels(mem.image, prop[4]-camPosQuad, mem.image.rect, {#ink:36, #blend:blnd})
          q = 1
          repeat while q < prop[5].points.count then
            adaptedPos = prop[5].points[q]
            adaptedPos = adaptedPos - gLEProps.camPos*20.0
            adaptedPos = adaptedPos * 16.0/20.0
            member("propsImage").image.copyPixels(member("pxl").image, rect(adaptedPos-point(1,1), adaptedPos+point(2,2)), rect(0,0,1,1), {#color:propData.previewColor})
            q = q + propData.previewEvery
          end repeat
        "variedDecal", "variedSoft", "variedStandard":
          updateVariedPreview(propData, prop[5].settings.variation)
          member("propsImage").image.copyPixels(mem.image, prop[4]-camPosQuad, mem.image.rect, {#ink:36, #blend:blnd, #color:clr})
        otherwise:
          member("propsImage").image.copyPixels(mem.image, prop[4]-camPosQuad, mem.image.rect, {#ink:36, #blend:blnd, #color:clr})
      end case
      
      
    else
      case (propData.tp) of   
        "rope", "customRope":
          member("propsImage2").image.copyPixels(mem.image, prop[4]-camPosQuad, mem.image.rect, {#ink:36, #blend:blnd, #color:clr})
          q = 1
          repeat while q < prop[5].points.count then
            adaptedPos = prop[5].points[q]
            adaptedPos = adaptedPos - gLEProps.camPos*20.0
            adaptedPos = adaptedPos * 16.0/20.0
            member("propsImage2").image.copyPixels(member("pxl").image, rect(adaptedPos-point(1,1), adaptedPos+point(2,2)), rect(0,0,1,1), {#color:propData.previewColor})
            q = q + propData.previewEvery
          end repeat
        "variedDecal", "variedSoft", "variedStandard":
          updateVariedPreview(propData, prop[5].settings.variation)
          member("propsImage2").image.copyPixels(mem.image, prop[4]-camPosQuad, mem.image.rect, {#ink:36, #blend:blnd, #color:clr})
        otherwise:
          member("propsImage2").image.copyPixels(mem.image, prop[4]-camPosQuad, mem.image.rect, {#ink:36, #blend:blnd, #color:clr})
      end case
    end if
  end repeat
  
  repeat with q = displayLayer to layer  then
    member("propsImage").image.copyPixels(member("pxl").image, rect(0,0,52*16, 40*16),rect(0,0,1,1), {#blend:10, #color:color(255, 255, 255)})
  end repeat
  
  if (propSettings <> void)then
    if(["variedDecal", "variedSoft", "variedStandard"].getPos(gProps[gPEprops.pmpos.locH].prps[gPEprops.pmPos.locV].tp) > 0) then
      updateVariedPreview(gProps[gPEprops.pmpos.locH].prps[gPEprops.pmPos.locV], propSettings.variation)
    end if
  end if
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
  hasFastScroll = false
  fastScrollRate = 4
  case key of
    "W":
      kb = #PropSelectUp
      hasFastScroll = true
    "S":
      kb = #PropSelectDown
      hasFastScroll = true
    "A":
      kb = #PropCategoryPrev
      hasFastScroll = true
      fastScrollRate = 8
    "D":
      kb = #PropCategoryNext
      hasFastScroll = true
      fastScrollRate = 8
    "Z":
      kb = #PropColor
    "N":
      kb = #PropOptionsSelected
    "L":
      kb = #PropChangeLayer
  end case
  
  rslt = checkCustomKeybind(kb, key) and not dontRunStuff()
  if hasFastScroll then
    -- fast scroll
    if rslt then
      gPEProps.keys[symbol(key)] = gPEProps.keys[symbol(key)] + gFSFlag
    else
      gPEProps.keys[symbol(key)] = 0
    end if
    
    if (gPEProps.keys[symbol(key)] = 1) or ((gPEProps.keys[symbol(key)] > 20) and ((gPEProps.keys[symbol(key)] mod fastScrollRate) = 0)) then
      rtrn = gFSFlag
    end if
  else
    -- no fast scroll
    gPEProps.keys[symbol(key)] = rslt
    if (gPEProps.keys[symbol(key)])and(gPEProps.lastKeys[symbol(key)]=0) then
      rtrn = 1
    end if
  end if
  gPEProps.lastKeys[symbol(key)] = gPEProps.keys[symbol(key)]
  return rtrn
end


on IsDecal(prop)
  if(prop.tp = "simpleDecal")or(prop.tp = "variedDecal")then
    return 1
  else
    return 0
  end if
end

--on updateTileMenu(mv)
--  if(mv = void)or(mv = script("tileEditor"))then
--    mv = point(0,0)
--  end if
--  
--  gTEprops.tmPos = gTEprops.tmPos + mv
--  if mv.locH <> 0 then
--    if gTEprops.tmPos.locH < 1 then
--      gTEprops.tmPos.locH = gTiles.count
--    else if gTEprops.tmPos.locH > gTiles.count then
--      gTEprops.tmPos.locH = 1
--    end if 
--    gTEprops.tmPos.locV = gTEprops.tmSavPosL[gTEprops.tmPos.locH]
--  else if mv.locV <> 0 then
--    if gTEprops.tmPos.locV < 1 then
--      gTEprops.tmPos.locV = gTiles[gTEprops.tmPos.locH].tls.count
--    else if gTEprops.tmPos.locV > gTiles[gTEprops.tmPos.locH].tls.count then
--      gTEprops.tmPos.locV = 1
--    end if
--    gTEprops.tmSavPosL[gTEprops.tmPos.locH] = gTEprops.tmPos.locV
--  end if
--  
--  gTEprops.tmPos.locH = restrict(gTEprops.tmPos.locH, 1, gTiles.count)
--  gTEprops.tmPos.locV = restrict(gTEprops.tmPos.locV, 1, gTiles[gTEprops.tmPos.locH].tls.count)
--  
--  txt = ""
--  put "[" && gTiles[gTEprops.tmPos.locH].nm && "]" after txt
--  put RETURN after txt
--  
--  repeat with tl = 1 to gTiles[gTEprops.tmPos.locH].tls.count then
--    if tl = gTEprops.tmPos.locV then
--      put "-" && gTiles[gTEprops.tmPos.locH].tls[tl].nm && "-" && RETURN after txt
--    else
--      put gTiles[gTEprops.tmPos.locH].tls[tl].nm && RETURN after txt
--    end if
--  end repeat
--  
--  member("tileMenu").text = txt
--  
--  
--  if gTiles[gTEprops.tmPos.locH].nm = "materials" then
--    sprite(19).visibility = 1
--    gTEprops.toolType = "material"
--    gTEprops.toolData = gTiles[gTEprops.tmPos.locH].tls[gTEprops.tmPos.locV].nm
--    member("tilePreview").image = image(1,1,1)
--    if _key.keyPressed("E") and _movie.window.sizeState <> #minimized then
--      if  gTEprops.defaultMaterial <> gTiles[gTEprops.tmPos.locH].tls[gTEprops.tmPos.locV].nm then
--        gTEprops.defaultMaterial = gTiles[gTEprops.tmPos.locH].tls[gTEprops.tmPos.locV].nm
--        --  put "set" &&     gTEprops.defaultMaterial && "as default material"
--        member("default material").text = "Default material:" && gTEprops.defaultMaterial && "(Press 'E' to change)"
--      end if
--    end if
--  else if gTiles[gTEprops.tmPos.locH].nm = "special" then
--    gTEprops.toolType = "special"
--    gTEprops.toolData = gTiles[gTEprops.tmPos.locH].tls[gTEprops.tmPos.locV].nm
--    member("tilePreview").image = image(1,1,1)
--  else
--    if gTEprops.specialEdit = 0 then
--      sprite(19).visibility = 0
--    end if
--    gTEprops.toolType = "tile"
--    gTEprops.toolData = "TILE"--gTEprops.tmPos--gTiles[gTEprops.tmPos.locH].tls[gTEprops.tmPos.locV]
--    drawTilePreview()
--  end if
--  
--  isTilePositionLegal(gTEprops.lstMsPs)
--  
--end

on updatePropMenu(mv)
  
  if(mv = void)or(mv = script("propEditor"))then
    mv = point(0,0)
  end if
  
  gPEprops.pmPos = gPEprops.pmPos + mv
  if mv.locH <> 0 then
    if gPEprops.pmPos.locH < 1 then
      gPEprops.pmPos.locH = gProps.count
    else if gPEprops.pmPos.locH > gProps.count then
      gPEprops.pmPos.locH = 1
    end if 
    gPEprops.pmPos.locV = gPEprops.pmSavPosL[gPEprops.pmPos.locH]
  else if mv.locV <> 0 then
    if gPEprops.pmPos.locV < 1 then
      gPEprops.pmPos.locV = gProps[gPEprops.pmPos.locH].prps.count
    else if gPEprops.pmPos.locV > gProps[gPEprops.pmPos.locH].prps.count then
      gPEprops.pmPos.locV = 1
    end if
    gPEprops.pmSavPosL[gPEprops.pmPos.locH] = gPEprops.pmPos.locV
  end if
  
  if(gPEprops.pmPos.locV - 5 < peScrollPos) then
    peScrollPos = gPEprops.pmPos.locV - 5
  else if  (gPEprops.pmPos.locV - 15 > peScrollPos) then
    peScrollPos = gPEprops.pmPos.locV - 15
  end if
  
  peScrollPos = restrict(peScrollPos, 0, gProps[gPEprops.pmPos.locH].prps.count)
  
  txt = ""
  put "[" && gProps[gPEprops.pmPos.locH].nm && "]" after txt
  put RETURN after txt
  
  repeat with pr = 1+peScrollPos to 21+peScrollPos then
    if(pr > gProps[gPEprops.pmPos.locH].prps.count)then
      exit repeat
    else
      if pr = gPEprops.pmPos.locV then
        put "-" && gProps[gPEprops.pmPos.locH].prps[pr].nm && "-" && RETURN after txt
      else
        put gProps[gPEprops.pmPos.locH].prps[pr].nm && RETURN after txt
      end if
    end if
  end repeat
  
  prp = VOID
  if (gPEprops.pmPos.locH <= gProps.count) and (gPEprops.pmPos.locH > 0) and (gPEprops.pmPos.locV > 0) and (gPEprops.pmPos.locV <= gProps[gPEprops.pmPos.locH].prps.count) then
    prp = gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV]
  end if
  
  if (prp <> VOID) and (prp.notes.count > 0) then
    put RETURN after txt
    put RETURN after txt
    put "NOTES" after txt
    put RETURN after txt
    repeat with nt in prp.notes then
      put nt after txt
      put RETURN after txt
    end repeat
  end if
  
  member("propMenu").text = txt
  --editSettingsProp = -1
  --updatePropMenu(point(0, 0))
  
  -- put "propPreview TestProp" && gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].nm
  newPropSelected()
  
end

global settingCursor
on updatePropSettings(mv)
  if(editSettingsProp = 0)then
    editedPropTemplate = gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV]
  else
    adress = gPEprops.props[editSettingsProp][3]
    editedPropTemplate = gProps[adress.locH].prps[adress.locV]
  end if
  
  if(propSettings = void)then
    DuplicatePropSettings()
  end if
  
  settingCursor = settingCursor + mv.locV
  if mv.locV <> 0 then
    if settingCursor < 1 then
      settingCursor = propSettings.count
    else if settingCursor > propSettings.count then
      settingCursor = 1
    end if
  end if
  
  if(mv.locH <> 0)then
    case(propSettings.getPropAt(settingCursor).string)of
      "release":
        propSettings[settingCursor] = restrict( propSettings[settingCursor] + mv.locH, -1, 1)
      "renderOrder":
        propSettings[settingCursor] = propSettings[settingCursor] + mv.locH
      "seed":
        propSettings[settingCursor] = (_system.milliSeconds mod 1000)
      "renderTime":
        if(propSettings[settingCursor] = 0)then
          propSettings[settingCursor] = 1
        else
          propSettings[settingCursor] = 0
        end if
      "thickness":
        propSettings[settingCursor] = restrict( propSettings[settingCursor] + mv.locH*0.25, 1, 5)
      "variation":
        propSettings[settingCursor] = propSettings[settingCursor] + mv.locH
        mn = (1 - editedPropTemplate.random)
        if(propSettings[settingCursor] < mn)then
          propSettings[settingCursor] = settingsPropType.vars
        else if(propSettings[settingCursor] > settingsPropType.vars)then
          propSettings[settingCursor] = mn
        end if
        updateVariedPreview(settingsPropType, propSettings[settingCursor])
      "customDepth":
        propSettings[settingCursor] = propSettings[settingCursor] + mv.locH
        if(propSettings[settingCursor] < 1)then
          propSettings[settingCursor] = 30
        else if(propSettings[settingCursor] > 30)then
          propSettings[settingCursor] = 1
        end if
      "applyColor":
        propSettings[settingCursor] = 1 - propSettings[settingCursor]
      "color":
        propSettings[settingCursor] = propSettings[settingCursor] + mv.locH
        if(propSettings[settingCursor] < 0)then
          propSettings[settingCursor] = gPEcolors.count
        else if(propSettings[settingCursor] > gPEcolors.count)then
          propSettings[settingCursor] = 0
        end if
    end case
  end if
  
  txt = ""
  put editedPropTemplate.nm after txt
  put RETURN after txt
  put "SETTINGS"after txt
  put RETURN after txt
  put "(press 'N' to exit)" after txt
  put RETURN after txt
  repeat with st = 1 to propSettings.count then
    nm = propSettings.getPropAt(st).string
    put nm & " " after txt
    put RETURN after txt
    p = propSettings[st]
    t = ""
    case(nm)of
      "release":
        if(p = -1)then
          t = "left"
        else if (p = 1) then
          t = "right"
        else
          t = "none"
        end if
      "renderTime":
        if(p = 0)then
          t = "Pre Effects"
        else
          t = "Post Effects"
        end if
      "variation":
        if(propSettings[st] = 0)then
          t = "random"
        else
          t = propSettings[st].string
        end if
      "applyColor":
        if propSettings[st] = 0 then
          t = "NO"
        else
          t = "YES"
        end if
      "color":
        if propSettings[st] = 0 then
          t = "NONE"
        else
          t = gPEcolors[propSettings[st]][1]
        end if
      otherwise:
        t = propSettings[st].string
    end case
    
    if(st = settingCursor)then
      put ">" & t & "<   "after txt
    else
      put t after txt
    end if
    put RETURN after txt
    put RETURN after txt
  end repeat
  
  member("propMenu").text = txt
  
  -- put "propPreview TestProp" && gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV].nm
  -- newPropSelected()
end

on newPropSelected()
  resetTransformation()
  DuplicatePropSettings()
  prop = VOID
  if (gPEprops.pmPos.locH <= gProps.count) and (gPEprops.pmPos.locH > 0) and (gPEprops.pmPos.locV > 0) and (gPEprops.pmPos.locV <= gProps[gPEprops.pmPos.locH].prps.count) then
    prop = gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV]
  end if
  if(peSavedRotat <> -1)then
    gPEprops.propRotation = peSavedRotat
  end if
  
  if peSavedStretch.locH <> 0 then
    gPEprops.propStretchX = peSavedStretch.locH
  end if
  
  if peSavedStretch.locV <> 0 then
    gPEprops.propStretchY = peSavedStretch.locV
  end if
  
  if(peSavedFlip.locH <> 0)then
    gPEprops.propFlipX = peSavedFlip.locH
  end if
  
  if(peSavedFlip.locV <> 0)then
    gPEprops.propFlipY = peSavedFlip.locV
  end if
  
  propSettings.renderTime = 0
  snapToGrid = 0
  preciseSnap = 0
  if stg = 1 then
    snapToGrid = 1
  end if
  if ps = 1 then
    preciseSnap = 1 
  end if
  
  if (prop <> VOID) then
    repeat with q = 1 to prop.tags.count then
      case prop.tags[q] of
        "postEffects":
          propSettings.renderTime = 1
        "snapToGrid":
          snapToGrid = 1
        "preciseSnap":
          preciseSnap = 1
      end case
    end repeat
    
    ApplyTransformationTags()
    
    if(prop.tp = "rope") or (prop.tp = "customRope")then
      resetRopeProp()
    end if
    
    if(["variedDecal", "variedSoft", "variedStandard"].getPos(prop.tp)>0)then
      updateVariedPreview(prop, propSettings.variation)
    end if
    
    updateWorkLayerText()
  end if
end


on ApplyTransformationTags()
  resetTransformation()
  
  peSavedRotat = -1
  peSavedFlip = point(0, 0)
  peSavedStretch = point(0, 0)
  
  prop = gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV]
  repeat with q = 1 to prop.tags.count then
    case prop.tags[q] of
      "randomRotat":
        peSavedRotat = gPEprops.propRotation
        gPEprops.propRotation = random(360)
      "randomFlipX":
        if(random(2)=1)then
          peSavedFlip.locH = gPEprops.propFlipX
          gPEprops.propFlipX = -gPEprops.propFlipX
        end if
      "randomFlipY":
        if(random(2)=1)then
          peSavedFlip.locV = gPEprops.propFlipY
          gPEprops.propFlipY = -gPEprops.propFlipY
        end if
    end case
  end repeat
  
  case prop.tp of
    "long", "customLong":
      peSavedStretch = point(gPEprops.propStretchX, gPEprops.propStretchY)
      gPEprops.propRotation = 0
      gPEprops.propFlipX = 1
      gPEprops.propFlipY = 1
      gPEprops.propStretchX = 1
      gPEprops.propStretchY = 1
  end case
  
  
end

on resetTransformation()
  if(peSavedRotat <> -1)then
    gPEprops.propRotation  = peSavedRotat
  end if
  if(peSavedFlip.locH <> 0)then
    gPEprops.propFlipX  = peSavedFlip.locH
  end if
  if(peSavedFlip.locV <> 0)then
    gPEprops.propFlipY  = peSavedFlip.locV
  end if
  if(peSavedStretch.locH <> 0)then
    gPEprops.propStretchX  = peSavedStretch.locH
  end if
  if(peSavedStretch.locV <> 0)then
    gPEprops.propStretchX  = peSavedStretch.locV
  end if
end


on resetRopeProp()
  prop = gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV]
  -- ropePropPV = [#segments:[]]
  -- adaptedSegmentLength = (prop.segmentLength.float / 20.0)*16.0
  
  if(gPEprops.propRotation = 0)then
    dir = point(0, -1)
    perp = point(1, 0)
  else if (gPEprops.propRotation = 90)then
    dir = point(1, 0)
    perp = point(0, 1)
  else if (gPEprops.propRotation = 180)then
    dir = point(0, 1)
    perp = point(-1, 0)
  else if (gPEprops.propRotation = 270)then
    dir = point(-1, 0)
    perp = point(0, -1)
  else
    dir = DegToVec(gPEprops.propRotation)
    perp = giveDirFor90degrToLine(-dir, dir)
  end if
  
  mem = propPreviewMember(gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV])
  scaleFac = 16.0/20.0
  
  qd = [peMousePos, peMousePos, peMousePos, peMousePos]
  qd[1] = qd[1] + (dir*mem.rect.height * 0.5 * gPEprops.propStretchY * scaleFac * gPEprops.propFlipY) - (perp*mem.rect.width * 0.5 * gPEprops.propStretchX * scaleFac * gPEprops.propFlipX)
  qd[2] = qd[2] + (dir*mem.rect.height * 0.5 * gPEprops.propStretchY * scaleFac * gPEprops.propFlipY) + (perp*mem.rect.width * 0.5 * gPEprops.propStretchX * scaleFac * gPEprops.propFlipX)
  qd[3] = qd[3] - (dir*mem.rect.height * 0.5 * gPEprops.propStretchY * scaleFac * gPEprops.propFlipY) + (perp*mem.rect.width * 0.5 * gPEprops.propStretchX * scaleFac * gPEprops.propFlipX)
  qd[4] = qd[4] - (dir*mem.rect.height * 0.5 * gPEprops.propStretchY * scaleFac * gPEprops.propFlipY) - (perp*mem.rect.width * 0.5 * gPEprops.propStretchX * scaleFac * gPEprops.propFlipX)
  
  offSetPnt = point(-16, -16) + gLEProps.camPos*16
  qd = qd + [offSetPnt, offSetPnt, offSetPnt, offSetPnt]
  
  --changing quad to 20*20 scale
  qd = qd * (20.0/16.0)
  
  pA = (qd[1] + qd[4])/2.0
  pB = (qd[2] + qd[3])/2.0
  
  collDep = ((gPEprops.workLayer-1) * 10) + gPEprops.depth + prop.collisionDepth
  if(collDep < 10)then
    cd = 1
  else if (collDep < 20)then
    cd = 2
  else
    cd = 3
  end if
  
  
  script("ropeModel").resetRopeModel(pA, pB, prop, gPEprops.propStretchY, cd, propSettings.release)
  
end

on updateVariedPreview(prop, var)
  mem = propPreviewMember(prop)
  
  tileAsProp = 0
  repeat with q = 1 to prop.tags.count then
    if prop.tags[q] = "Tile" then
      tileAsProp = 1
      exit repeat
    end if
  end repeat
  
  imprtMem = member("previewImprt")
  if(tileAsProp)then
    member("previewImprt").importFileInto("Graphics\" &prop.nm&".png")
  else
    member("previewImprt").importFileInto("Props\" &prop.nm&".png")
  end if
  imprtMem.name = "previewImprt"
  --INTERNAL
  if (checkDRInternal(prop.nm)) then
    imprtMem.image = member(prop.nm).image
  end if
  
  if prop.tp = "variedStandard" then
    sz = prop.sz*20.0
  else
    sz = prop.pxlSize
  end if
  
  mem.image = image(sz.locH, sz.locV, 16)
  if prop.tp = "variedStandard" then
    if(var = 0) then
      repeat with v2 = 1 to prop.vars then
        repeat with c = 1 to prop.repeatL.count then
          c2 = prop.repeatL.count + 1 - c
          getRect = rect(prop.sz.locH*20*(v2-1), (c2-1)*prop.sz.locV*20, prop.sz.locH*20*v2, c2*prop.sz.locV*20)+rect(0,1,0,1)
          mem.image.copyPixels(member("pxl").image, mem.image.rect, rect(0,0,1,1), {#color:color(255, 255, 255), #blend:80.0/prop.repeatL.count})
          mem.image.copyPixels(imprtMem.image, mem.image.rect, getRect, {#ink:36})
        end repeat
      end repeat
    else
      repeat with c = 1 to prop.repeatL.count then
        c2 = prop.repeatL.count + 1 - c
        getRect = rect(prop.sz.locH*20*(var-1), (c2-1)*prop.sz.locV*20, prop.sz.locH*20*var, c2*prop.sz.locV*20)+rect(0,1,0,1)
        mem.image.copyPixels(member("pxl").image, mem.image.rect, rect(0,0,1,1), {#color:color(255, 255, 255), #blend:80.0/prop.repeatL.count})
        mem.image.copyPixels(imprtMem.image, mem.image.rect, getRect, {#ink:36})
      end repeat
    end if
  else
    if(var = 0)then
      repeat with v2 = 1 to prop.vars then
        mem.image.copyPixels(imprtMem.image, mem.image.rect, rect(sz.locH * (v2-1), 0, sz.locH * v2, sz.locV)+rect(0,1,0,1), {#ink:36})
      end repeat
    else
      --   put rect(prop.pxlSize.locH * (var-1), 0, prop.pxlSize.locV, prop.pxlSize.locH * var)
      mem.image.copyPixels(imprtMem.image, mem.image.rect, rect(sz.locH * (var-1), 0, sz.locH * var, sz.locV)+rect(0,1,0,1))
    end if
  end if
end


on propPreviewMember(prop)
  global loadedPropPreviews
  if(loadedPropPreviews = void) then
    loadedPropPreviews = []
  end if
  
  repeat with q = 1 to loadedPropPreviews.count then
    if loadedPropPreviews[q] = prop.nm then
      return member("propPreview" && prop.nm)
    end if
  end repeat
  
  tileAsProp = 0
  repeat with q = 1 to prop.tags.count then
    if prop.tags[q] = "Tile" then
      tileAsProp = 1
      exit repeat
    end if
  end repeat
  
  sav2 = member("previewImprt")
  if(tileAsProp)then
    member("previewImprt").importFileInto("Graphics\" &prop.nm&".png")
  else
    member("previewImprt").importFileInto("Props\" &prop.nm&".png")
  end if
  sav2.name = "previewImprt"
  --INTERNAL
  if (checkDRInternal(prop.nm)) then
    sav2.image = member(prop.nm).image
    member("previewImprt").image = member(prop.nm).image
  end if
  
  newMem = new(#bitmap, castLib "customMems")
  
  case prop.tp of
    "standard":
      newMem.image = image(prop.sz.locH*20, prop.sz.locV*20, 16)
      
      repeat with c = 1 to prop.repeatL.count then
        c2 = prop.repeatL.count + 1 - c
        getRect = rect(0, (c2-1)*prop.sz.locV*20, prop.sz.locH*20, c2*prop.sz.locV*20)+rect(0,1,0,1)
        newMem.image.copyPixels(member("pxl").image, newMem.image.rect, rect(0,0,1,1), {#color:color(255, 255, 255), #blend:80.0/prop.repeatL.count})
        newMem.image.copyPixels(member("previewImprt").image, newMem.image.rect, getRect, {#ink:36})
      end repeat
      
    "simpleDecal", "soft", "softEffect", "antimatter", "coloredSoft":
      newMem.image = image(member("previewImprt").image.width, member("previewImprt").image.height, 16)
      newMem.image.copyPixels(member("previewImprt").image, newMem.image.rect, member("previewImprt").image.rect)
      
    "variedDecal", "variedSoft":
      newMem.image = image(prop.pxlSize.locH, prop.pxlSize.locV, 16)
      newMem.image.copyPixels(member("previewImprt").image, newMem.image.rect, rect(0,0,prop.pxlSize.locH, prop.pxlSize.locV))
      
    "variedStandard":
      newMem.image = image(prop.sz.locH*20*prop.vars, prop.sz.locV*20, 16)
      repeat with c = 1 to prop.repeatL.count then
        --repeat with vari = 1 to prop.settings.variation then
        c2 = prop.repeatL.count + 1 - c
        getRect = rect(0, (c2-1)*prop.sz.locV*20, prop.sz.locH*20*prop.vars, c2*prop.sz.locV*20)+rect(0,1,0,1)
        newMem.image.copyPixels(member("pxl").image, newMem.image.rect, rect(0,0,1,1), {#color:color(255, 255, 255), #blend:80.0/prop.repeatL.count})
        newMem.image.copyPixels(member("previewImprt").image, newMem.image.rect, getRect, {#ink:36})
        --end repeat
      end repeat
      
    "rope", "long", "customRope", "customLong":
      newMem.image = image(member("previewImprt").image.width, member("previewImprt").image.height, 16)
      newMem.image.copyPixels(member("previewImprt").image, newMem.image.rect, member("previewImprt").image.rect)
  end case
  
  newMem.name = "propPreview" && prop.nm
  
  loadedPropPreviews.add(prop.nm)
  
  return newMem
end







on DuplicatePropSettings()
  doIt = (settingsPropType = void)
  prp = VOID
  if (gPEprops.pmPos.locH <= gProps.count) and (gPEprops.pmPos.locH > 0) and (gPEprops.pmPos.locV > 0) and (gPEprops.pmPos.locV <= gProps[gPEprops.pmPos.locH].prps.count) then
    prp = gProps[gPEprops.pmPos.locH].prps[gPEprops.pmPos.locV]
  end if
  if (prp <> VOID) then
    if(doIt = 0)then
      -- put settingsPropType.nm && prp.nm
      if (settingsPropType.nm <> prp.nm) then
        doIt = 1
      end if
    end if
    if (doIt) then
      propSettings = prp.settings.duplicate()
      propSettings.seed = Random(1000)
      settingsPropType = prp
    end if
  end if
end









