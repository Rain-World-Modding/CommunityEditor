global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, chOp, fatOp, gradAf, effectIn3D, gAnyDecals, gRotOp, slimeFxt, DRDarkSlimeFix, DRWhite, DRPxl, DRPxlRect, colrIntensity, skyRootsFix


on ApplyFuzzyGrower(me, q, c)
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
    otherwise:
      d = random(29)
  end case
  lr = 1 + (d > 9) + (d > 19)
  if (gLEprops.matrix[q2][c2][lr][1] = 0) then
    mdPnt = giveMiddleOfTile(point(q, c))
    headPos = mdPnt + point(-11 + random(21), -11 + random(21))
    pnt = point(headPos.locH, headPos.locV)
    lastDir = 180 - 61 + random(121)
    blnd = 1
    blnd2 = 1
    wdth = 0.3
    searchBase = 50
    quadsToDraw = []
    repeat while pnt.locV < 30000
      dir = 180 - 150 + random(300)
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
      rct = (lastPnt + pnt) / 2.0
      rct = rect(rct, rct)
      rct = rct + rect(-10 * wdth, -25, 10 * wdth, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      if (random(2) = 1) then
        qd = flipQuadH(qd)
      end if
      wdth = wdth + (random(1000) / 1000.0) / 5.0
      if (wdth > 1) then
        wdth = 1
      end if
      var = random(13)
      tdRect = rect((var - 1) * 20, 1, var * 20, 51)
      
      
      if skyRootsFix then
        quadsToDraw.add([qd, tdRect, blnd])
      else
        member("layer" & string(d)).image.copyPixels(member("fuzzyBushGraf").image, qd, tdRect, {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qd, "fuzzyBushGrad", tdRect, 0.5, blnd)
      end if
      
      blnd = blnd * 0.5
      if (blnd2 > 0) then
        rctR = (lastPnt + pnt) / 2.0
        rct = rect(rctR, rctR) + rect(-12, -36, 12, 36)
        qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
        copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, blnd2)
        blnd2 = blnd2 - 0.15
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
        member("layer" & string(d)).image.copyPixels(member("fuzzyBushGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "fuzzyBushGrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
  end if
end


on ApplyHorrorGrower me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(29)
    "1":
      d = random(9)
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(19)
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(29)
  end case
  lr = 1+(d>9)+(d>19)
  
  
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    lastDir = 180 - 61 + random(121)
    blnd = 1
    blnd2 = 0.75
    
    wdth = 0.5
    
    searchBase = 250
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000 then
      dir = 180 - 250 + random(500)
      dir = lerp(lastDir, dir, 0.35)
      lastPnt = pnt
      pnt = pnt + degToVec(dir)*30.0
      
      if(searchBase > 0)then
        moveDir = point(0,0)
        repeat with tst in [point(-1,0), point(1,0), point(1,1), point(0,1), point(-1, 1)] then
          tstPnt = giveGridPos(lastPnt) + gRenderCameraTilePos + tst
          if(tstPnt.locH > 0)and(tstPnt.locH < gLOprops.size.locH-1)and(tstPnt.locV > 0)and(tstPnt.locV < gLOprops.size.locV-1)then
            moveDir = moveDir + tst*gEEprops.effects[r].mtrx[tstPnt.locH][tstPnt.locV]
          end if
        end repeat
        pnt = pnt + (moveDir/100.0)*searchBase
        searchBase = searchBase - 1.5
        pnt = lastPnt + moveToPoint(lastPnt, pnt, 30.0)
      end if
      
      lastDir = dir
      
      rct = (lastPnt + pnt)/2.0
      rct = rect(rct, rct)
      rct = rct + rect(-10*wdth, -25, 10*wdth, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      if(random(2)=1)then
        qd = flipQuadH(qd)
      end if
      
      wdth = wdth + (random(1000)/1000.0)/5.0
      if(wdth > 1)then
        wdth = 1
      end if
      
      var = random(13)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rect((var-1)*20, 1, var*20, 50+1), blnd])
      else
        member("layer"&string(d)).image.copyPixels(member("fuzzyBushGraf").image, qd, rect((var-1)*20, 1, var*20, 50+1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qd, "fuzzyBushGrad", rect((var-1)*20, 1, var*20, 50+1), 0.5, blnd)
      end if
      
      blnd = blnd * 0.1
      
      if(blnd2 > 0)then
        rct = (lastPnt + pnt)/2.0
        rct = rect(rct, rct)
        rct = rct + rect(-12, -36, 12, 36)
        qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
        
        copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, blnd2)
        
        blnd2 = blnd2 - 0.09
      end if
      
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if tlPos.inside(rect(1,1,gLOprops.size.loch+1,gLOprops.size.locv+1)) = 0 then
        exit repeat
      else if solidAfaMv(tlPos, lr) = 1 then
        exit repeat
      end if
      
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer"&string(d)).image.copyPixels(member("fuzzyBushGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "fuzzyBushGrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
  end if
end


on ApplyMeatGrower me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(29)
    "1":
      d = random(9)
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(19)
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(29)
  end case
  lr = 1+(d>9)+(d>19)
  
  
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    lastDir = 180 - 100 + random(200)
    blnd = 1
    blnd2 = 1
    
    wdth = 0.3
    
    searchBase = 100
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000 then
      dir = 180 - 200 + random(400)
      dir = lerp(lastDir, dir, 0.25)
      lastPnt = pnt
      pnt = pnt + degToVec(dir)*30.0
      
      
      
      lastDir = dir
      
      rct = (lastPnt + pnt)/2.0
      rct = rect(rct, rct)
      rct = rct + rect(-30*wdth, -25, 30*wdth, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      if(random(2)=1)then
        qd = flipQuadH(qd)
      end if
      
      wdth = wdth + (random(1000)/1000.0)/5.0
      if(wdth > 1)then
        wdth = 1
      end if
      
      var = random(13)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rect((var-1)*60, 1, var*60, 50+1), blnd])
      else
        member("layer"&string(d)).image.copyPixels(member("meatGrowGraf").image, qd, rect((var-1)*60, 1, var*60, 50+1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qd, "meatGrowGrad", rect((var-1)*60, 1, var*60, 50+1), 0.5, blnd)
      end if
      
      blnd = blnd * 0.90
      
      if(blnd2 > 0)then
        rct = (lastPnt + pnt)/2.0
        rct = rect(rct, rct)
        rct = rct + rect(-12, -36, 12, 36)
        qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
        
        
        blnd2 = blnd2 - 0.15
      end if
      
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if tlPos.inside(rect(1,1,gLOprops.size.loch+1,gLOprops.size.locv+1)) = 0 then
        exit repeat
      else if solidAfaMv(tlPos, lr) = 1 then
        exit repeat
      end if
      
    end repeat 
    
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer"&string(d)).image.copyPixels(member("meatGrowGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "meatGrowGrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
    
  end if
end


on ApplyLeafGrower me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(29)
    "1":
      d = random(9)
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(19)
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(29)
  end case
  
  lr = 1+(d>9)+(d>19)
  
  
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    lastDir = 180 - 100 + random(200)
    blnd = 1
    blnd2 = 1
    
    wdth = 0.65
    
    searchBase = 50
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000 then
      dir = 180 - 61 + random(121)
      dir = lerp(lastDir, dir, 0.35)
      lastPnt = pnt
      pnt = pnt + degToVec(dir)*30.0
      
      if(searchBase > 0)then
        moveDir = point(0,0)
        repeat with tst in [point(-1,0), point(1,0), point(1,1), point(0,1), point(-1, 1)] then
          tstPnt = giveGridPos(lastPnt) + gRenderCameraTilePos + tst
          if(tstPnt.locH > 0)and(tstPnt.locH < gLOprops.size.locH-1)and(tstPnt.locV > 0)and(tstPnt.locV < gLOprops.size.locV-1)then
            moveDir = moveDir + tst*gEEprops.effects[r].mtrx[tstPnt.locH][tstPnt.locV]
          end if
        end repeat
        pnt = pnt + (moveDir/100.0)*searchBase
        searchBase = searchBase - 1.5
        pnt = lastPnt + moveToPoint(lastPnt, pnt, 30.0)
      end if
      
      lastDir = dir
      
      rct = (lastPnt + pnt)/2.0
      rct = rect(rct, rct)
      rct = rct + rect(-10*wdth, -25, 10*wdth, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      if(random(2)=1)then
        qd = flipQuadH(qd)
      end if
      
      wdth = wdth + (random(1000)/1000.0)/5.0
      if(wdth > 1)then
        wdth = 1
      end if
      
      var = random(13)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rect((var-1)*20, 1, var*20, 50+1), blnd])
      else
        member("layer"&string(d)).image.copyPixels(member("leafBushGraf").image, qd, rect((var-1)*20, 1, var*20, 50+1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qd, "leafBushGrad", rect((var-1)*20, 1, var*20, 50+1), 0.5, blnd)
      end if
      
      blnd = blnd * 0.85
      
      if(blnd2 > 0)then
        rct = (lastPnt + pnt)/2.0
        rct = rect(rct, rct)
        rct = rct + rect(-12, -36, 12, 36)
        qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
        
        copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, blnd2)
        
        blnd2 = blnd2 - 0.15
      end if
      
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if tlPos.inside(rect(1,1,gLOprops.size.loch+1,gLOprops.size.locv+1)) = 0 then
        exit repeat
      else if solidAfaMv(tlPos, lr) = 1 then
        exit repeat
      end if
      
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer"&string(d)).image.copyPixels(member("leafBushGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "leafBushGrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
  end if
end

on ApplyCoralGrower me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(29)
    "1":
      d = random(9)
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(19)
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(29)
  end case
  lr = 1+(d>9)+(d>19)
  
  
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    lastDir = 180 - 10 + random(20)
    blnd = 1
    blnd2 = 1
    
    wdth = 0.35
    
    searchBase = 100
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000 then
      dir = 180 - 30 + random(60)
      dir = lerp(lastDir, dir, 0.35)
      lastPnt = pnt
      pnt = pnt + degToVec(dir)*30.0
      
      if(searchBase > 0)then
        moveDir = point(0,0)
        repeat with tst in [point(-1,0), point(1,0), point(1,1), point(0,1), point(-1, 1)] then
          tstPnt = giveGridPos(lastPnt) + gRenderCameraTilePos + tst
          if(tstPnt.locH > 0)and(tstPnt.locH < gLOprops.size.locH-1)and(tstPnt.locV > 0)and(tstPnt.locV < gLOprops.size.locV-1)then
            moveDir = moveDir + tst*gEEprops.effects[r].mtrx[tstPnt.locH][tstPnt.locV]
          end if
        end repeat
        pnt = pnt + (moveDir/100.0)*searchBase
        searchBase = searchBase - 1.5
        pnt = lastPnt + moveToPoint(lastPnt, pnt, 30.0)
      end if
      
      lastDir = dir
      
      rct = (lastPnt + pnt)/2.0
      rct = rect(rct, rct)
      rct = rct + rect(-15*wdth, -25, 15*wdth, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      if(random(2)=1)then
        qd = flipQuadH(qd)
      end if
      
      wdth = wdth + (random(1000)/1000.0)/5.0
      if(wdth > 1)then
        wdth = 1
      end if
      
      var = random(13)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rect((var-1)*20, 1, var*20, 50+1), blnd])
      else
        member("layer"&string(d)).image.copyPixels(member("coralBushGraf").image, qd, rect((var-1)*20, 1, var*20, 50+1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qd, "coralBushGrad", rect((var-1)*20, 1, var*20, 50+1), 0.5, blnd)
      end if
      
      blnd = blnd * 0.7
      
      if(blnd2 > 0)then
        rct = (lastPnt + pnt)/2.0
        rct = rect(rct, rct)
        rct = rct + rect(-12, -36, 12, 36)
        qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
        
        
        blnd2 = blnd2 - 0.15
      end if
      
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if tlPos.inside(rect(1,1,gLOprops.size.loch+1,gLOprops.size.locv+1)) = 0 then
        exit repeat
      else if solidAfaMv(tlPos, lr) = 1 then
        exit repeat
      end if
      
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer"&string(d)).image.copyPixels(member("coralBushGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "coralBushGrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
  end if
end