global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, chOp, fatOp, gradAf, effectIn3D, gAnyDecals, gRotOp, slimeFxt, DRDarkSlimeFix, DRWhite, DRPxl, DRPxlRect, colrIntensity, skyRootsFix

--dakras
on ApplySideKelp(me, q, c)
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of
    "All":
      d = random(29)
    "1":
      d = random(9)
    "2":
      d = random(10) - 1 + 10
    "3":
      d = random(10) - 1 + 20
    "1:st and 2:nd":
      d = random(19)
    "2:nd and 3:rd":
      d = random(20) - 1 + 10
  end case
  lr = 1 + (d > 9) + (d > 19)
  blnd = 1
  blnd2 = 1
  if (gLEprops.matrix[q2][c2][lr][1] = 0) then
    mdPnt = giveMiddleOfTile(point(q, c))
    headPos = mdPnt + point(-11 + random(21), -11 + random(21))
    pnt = point(headPos.locH, headPos.locV)
    lastDir = 180 - 101 + random(201)
    points = [pnt]
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000
      dir = 180 - 31 + random(61)
      dir = lerp(lastDir, dir, 0.75)
      lastPnt = pnt
      pnt = pnt + degToVec(dir) * 30.0
      lastDir = dir
      rctR = (lastPnt + pnt) / 2.0
      rct = rect(rctR, rctR) + rect(-30, -25, 30, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      points.add(pnt)
      var = random(13)
      rectDk = rect((var - 1) * 40, 1, var * 40, 51)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rectDk, blnd])
      else
        member("layer" & string(d)).image.copyPixels(member("sidekelpgraf").image, qd, rectDk, {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qd, "sidekelpgrad", rectDk, 0.5, blnd)
      end if
      
      blnd = blnd * 0.85
      
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if (tlPos.inside(rect(1, 1, gLOprops.size.loch + 1, gLOprops.size.locv + 1)) = 0) then
        exit repeat
      else if solidAfaMv(tlPos, lr) = 1 then
        exit repeat
      end if
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer" & string(d)).image.copyPixels(member("sidekelpgraf").image, qdd[1], qdd[2], {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "sidekelpgrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
    if (blnd2 > 0) then
      rctR = (lastPnt + pnt) / 2.0
      rct = rect(rctR, rctR) + rect(-12, -36, 12, 36)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, blnd2)
      blnd2 = blnd2 - 0.15
    end if
  end if
end

on ApplyFlipSideKelp(me, q, c)
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of
    "All":
      d = random(29)
    "1":
      d = random(9)
    "2":
      d = random(10) - 1 + 10
    "3":
      d = random(10) - 1 + 20
    "1:st and 2:nd":
      d = random(19)
    "2:nd and 3:rd":
      d = random(20) - 1 + 10
  end case
  lr = 1 + (d > 9) + (d > 19)
  blnd = 1
  blnd2 = 1
  if (gLEprops.matrix[q2][c2][lr][1] = 0) then
    mdPnt = giveMiddleOfTile(point(q, c))
    headPos = mdPnt + point(-11 + random(21), -11 + random(21))
    pnt = point(headPos.locH, headPos.locV)
    lastDir = 180 - 101 + random(201)
    points = [pnt]
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000
      dir = 180 - 31 + random(61)
      dir = lerp(lastDir, dir, 0.75)
      lastPnt = pnt
      pnt = pnt + degToVec(dir) * 30.0
      lastDir = dir
      rctR = (lastPnt + pnt) / 2.0
      rct = rect(rctR, rctR) + rect(-30, -25, 30, 25)
      qd = flipQuadH(rotateToQuad(rct, lookAtPoint(lastPnt, pnt)))
      points.add(pnt)
      var = random(13)
      rectDk = rect((var - 1) * 40, 1, var * 40, 51)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rectDk, blnd])
      else
        member("layer" & string(d)).image.copyPixels(member("sidekelpgraf").image, qd, rectDk, {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qd, "sidekelpgrad", rectDk, 0.5, blnd)
      end if
      
      blnd = blnd * 0.85
      
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if (tlPos.inside(rect(1, 1, gLOprops.size.loch + 1, gLOprops.size.locv + 1)) = 0) then
        exit repeat
      else if solidAfaMv(tlPos, lr) = 1 then
        exit repeat
      end if
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer" & string(d)).image.copyPixels(member("sidekelpgraf").image, qdd[1], qdd[2], {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "sidekelpgrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
    if (blnd2 > 0) then
      rctR = (lastPnt + pnt) / 2.0
      rct = rect(rctR, rctR) + rect(-12, -36, 12, 36)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, blnd2)
      blnd2 = blnd2 - 0.15
    end if
  end if
end

on ApplyMixKelp(me, q, c)
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of
    "All":
      d = random(29)
    "1":
      d = random(9)
    "2":
      d = random(10) - 1 + 10
    "3":
      d = random(10) - 1 + 20
    "1:st and 2:nd":
      d = random(19)
    "2:nd and 3:rd":
      d = random(20) - 1 + 10
  end case
  lr = 1 + (d > 9) + (d > 19)
  blnd = 1
  blnd2 = 1
  if (gLEprops.matrix[q2][c2][lr][1] = 0) then
    mdPnt = giveMiddleOfTile(point(q, c))
    headPos = mdPnt + point(-11 + random(21), -11 + random(21))
    pnt = point(headPos.locH, headPos.locV)
    lastDir = 180 - 101 + random(201)
    points = [pnt]
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000
      dir = 180 - 31 + random(61)
      dir = lerp(lastDir, dir, 0.75)
      lastPnt = pnt
      pnt = pnt + degToVec(dir) * 30.0
      lastDir = dir
      rctR = (lastPnt + pnt) / 2.0
      rct = rect(rctR, rctR) + rect(-30, -25, 30, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      if (random(2) = 1) then
        qd = flipQuadH(qd)
      end if
      points.add(pnt)
      var = random(13)
      rectDk = rect((var - 1) * 60, 1, var * 60, 51)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rectDk, blnd])
      else
        member("layer" & string(d)).image.copyPixels(member("fsidekelpgraf").image, qd, rectDk, {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qd, "fsidekelpgrad", rectDk, 0.5, blnd)
      end if
      
      blnd = blnd * 0.85
      
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if (tlPos.inside(rect(1, 1, gLOprops.size.loch + 1, gLOprops.size.locv + 1)) = 0) then
        exit repeat
      else if solidAfaMv(tlPos, lr) = 1 then
        exit repeat
      end if
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer" & string(d)).image.copyPixels(member("fsidekelpgraf").image, qdd[1], qdd[2], {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "fsidekelpgrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
    if (blnd2 > 0) then
      rctR = (lastPnt + pnt) / 2.0
      rct = rect(rctR, rctR) + rect(-12, -36, 12, 36)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, blnd2)
      blnd2 = blnd2 - 0.15
    end if
  end if
end

on ApplyBubbleGrower(me, q, c)
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of
    "All":
      d = random(29)
    "1":
      d = random(9)
    "2":
      d = random(10) - 1 + 10
    "3":
      d = random(10) - 1 + 20
    "1:st and 2:nd":
      d = random(19)
    "2:nd and 3:rd":
      d = random(20) - 1 + 10
  end case
  lr = 1 + (d > 9) + (d > 19)
  if (gLEprops.matrix[q2][c2][lr][1] = 0) then
    mdPnt = giveMiddleOfTile(point(q, c))
    headPos = mdPnt + point(-11 + random(21), -11 + random(21))
    pnt = point(headPos.locH, headPos.locV)
    lastDir = 180 - 61 + random(121)
    blnd = 1
    blnd2 = 1
    wdth = 0.5
    searchBase = 50
    quadsToDraw = []
    repeat while pnt.locV < 30000
      dir = 180 - 61 + random(121)
      dir = lerp(lastDir, dir, 0.35)
      lastPnt = pnt
      pnt = pnt + degToVec(dir) * 30.0
      if (searchBase > 0) then
        moveDir = point(0, 0)
        repeat with tst in [point(-1, 0), point(1, 0), point(1, 1), point(0, 1), point(-1, 1)]
          tstPnt = giveGridPos(lastPnt) + gRenderCameraTilePos + tst
          if (tstPnt.locH > 0) and (tstPnt.locH < gLOprops.size.locH - 1) and (tstPnt.locV > 0) and (tstPnt.locV < gLOprops.size.locV - 1) then
            moveDir = moveDir + tst * gEEprops.effects[r].mtrx[tstPnt.locH][tstPnt.locV]
          end if
        end repeat
        pnt = pnt + (moveDir / 100.0) * searchBase
        searchBase = searchBase - 1.5
        pnt = lastPnt + moveToPoint(lastPnt, pnt, 30.0)
      end if
      lastDir = dir
      rctR = (lastPnt + pnt) / 2.0
      rct = rect(rctR, rctR) + rect(-10 * wdth, -25, 10 * wdth, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      if (random(2) = 1) then
        qd = flipQuadH(qd)
      end if
      wdth = wdth + (random(2000) / 400.0) / 2.5 - 0.5
      if (wdth > 1.7) then
        wdth = 1.7
      else if (wdth < 0.3) then
        wdth = 0.3
      end if
      var = random(13)
      rectDk = rect((var - 1) * 20, 1, var * 20, 50 + 1)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rectDk, blnd])
      else
        member("layer" & string(d)).image.copyPixels(member("bubblegrowergraf").image, qd, rectDk, {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qd, "bubblegrowergrad", rectDk, 0.5, blnd)
      end if
      
      blnd = blnd * 0.90
      if (blnd2 > 0) then
        rctR = (lastPnt + pnt) / 2.0
        rct = rect(rctR, rctR) + rect(-12, -36, 12, 36)
        qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
        copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, blnd2)
        blnd2 = blnd2 - 0.4
      end if
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if (tlPos.inside(rect(1, 1, gLOprops.size.loch + 1, gLOprops.size.locv + 1)) = 0) then
        exit repeat
      else if (solidAfaMv(tlPos, lr) = 1) then
        exit repeat
      end if     
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer" & string(d)).image.copyPixels(member("bubblegrowergraf").image, qdd[1], qdd[2], {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "bubblegrowergrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
  end if
end


on applyClubMoss me, q, c, amount
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  mdPnt = giveMiddleOfTile(point(q,c))
  
  case lrSup of
    "All":
      dmin = 0
      dmax = 29
    "1":
      dmin = 0
      dmax = 6
    "2":
      dmin = 10
      dmax = 16
    "3":
      dmin = 20
      dmax = 29
    "1:st and 2:nd":
      dmin = 0
      dmax = 16
    "2:nd and 3:rd":
      dmin = 10
      dmax = 29
    otherwise:
      dmin = 0
      dmax = 29
  end case
  
  repeat with a = 1 to amount/2 then
    dp = random(28)-1
    if(dp > 3)then
      dp = dp + 2
    end if
    
    lr = 3
    rad = random(100)*0.2*lerp(0.2, 1.0, amount/100)
    
    if(dp < 10)then
      lr = 1
    else if (dp < 20) then
      lr = 2
    end if
    
    startPos = mdPnt+point(-11+random(21), -11+random(21))
    
    
    solid = 0
    
    if(solidAfaMv(point(q2,c2), lr) = 1)then
      solid = 1
    end if
    
    if(solid = 0)and(lr < 3)and(dp - (lr-1)*10 > 6)then
      if(solidAfaMv(point(q2,c2), lr+1) = 1)then
        solid = 1
      end if
    end if
    
    if(solid = 0)then
      repeat with dr in [point(-1,0), point(0,-1), point(0,1), point(1,0)]then
        if(solidAfaMv(giveGridPos(startPos + dr*rad)+gRenderCameraTilePos, lr) = 1)then
          solid = 1
          exit repeat
        end if
      end repeat
    end if
    
    if(solid = 0)and(dp < 27)and(rad > 1.2)then
      repeat with dr in [point(0,0), point(-1,0), point(0,-1), point(0,1), point(1,0)]then
        if( member("layer"&string(dp+2)).getPixel(startPos.locH + dr.locH*rad*0.5, startPos.locV + dr.locV*rad*0.5) <> -1)then --compare it to -1 here, not to white
          rad = rad / 2
          solid = 1
          exit repeat
        end if
      end repeat
    end if
    
    if(solid = 1)then
      if(dp <= dmax) and (dp >= dmin) then
        vari = random(13)
        rtRect = rotateToQuad(rect(startPos, startPos) + rect(-rad, -rad, rad, rad), random(360))
        dtRect = rect((vari - 1) * 20, 1, vari * 20, 14)
        member("layer" & string(dp)).image.copyPixels(member("clubMossGraf").image, rtRect, dtRect, {#color:colr, #ink:36})
        if (gdLayer <> "C") then
          member("gradient" & gdLayer & string(dp)).image.copyPixels(member("clubMossGrad").image, rtRect, dtRect, {#ink:39})
        end if 
      end if
    end if
  end repeat
end

on applyMossWall me, q, c, amount
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  mdPnt = giveMiddleOfTile(point(q,c))
  
  case lrSup of
    "All":
      dmin = 0
      dmax = 29
    "1":
      dmin = 0
      dmax = 6
    "2":
      dmin = 10
      dmax = 16
    "3":
      dmin = 20
      dmax = 29
    "1:st and 2:nd":
      dmin = 0
      dmax = 16
    "2:nd and 3:rd":
      dmin = 10
      dmax = 29
    otherwise:
      dmin = 0
      dmax = 29
  end case
  
  repeat with a = 1 to amount/2 then
    dp = random(28)-1
    if(dp > 3)then
      dp = dp + 2
    end if
    
    lr = 3
    rad = random(100)*0.2*lerp(0.2, 1.0, amount/100)
    
    if(dp < 10)then
      lr = 1
    else if (dp < 20) then
      lr = 2
    end if
    
    startPos = mdPnt+point(-11+random(21), -11+random(21))
    
    
    solid = 0
    
    if(solidAfaMv(point(q2,c2), lr) = 1)then
      solid = 1
    end if
    
    if(solid = 0)and(lr < 3)and(dp - (lr-1)*10 > 6)then
      if(solidAfaMv(point(q2,c2), lr+1) = 1)then
        solid = 1
      end if
    end if
    
    if(solid = 0)then
      repeat with dr in [point(-1,0), point(0,-1), point(0,1), point(1,0)]then
        if(solidAfaMv(giveGridPos(startPos + dr*rad)+gRenderCameraTilePos, lr) = 1)then
          solid = 1
          exit repeat
        end if
      end repeat
    end if
    
    if(solid = 0)and(dp < 27)and(rad > 1.2)then
      repeat with dr in [point(0,0), point(-1,0), point(0,-1), point(0,1), point(1,0)]then
        if( member("layer"&string(dp+2)).getPixel(startPos.locH + dr.locH*rad*0.5, startPos.locV + dr.locV*rad*0.5) <> -1)then --compare it to -1 here, not to white
          rad = rad / 2
          solid = 1
          exit repeat
        end if
      end repeat
    end if
    
    if(solid = 1)then
      if(dp <= dmax) and (dp >= dmin) then
        vari = random(13)
        nRn = random(90) --needed to prevent RND change
        rtRect = rect(startPos, startPos) + rect(-rad, -rad, rad, rad)
        dtRect = rect((vari - 1) * 20, 1, vari * 20, 12)
        member("layer"&string(dp)).image.copyPixels(member("mossSideGraf").image, rtRect, dtRect, {#color:colr, #ink:36})
        if (gdLayer <> "C") then
          member("gradient"&gdLayer&string(dp)).image.copyPixels(member("mossSideGrad").image, rtRect, dtRect, {#ink:39})
        end if
      end if
    end if
  end repeat
end
--end dakras


