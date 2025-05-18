global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, chOp, fatOp, gradAf, effectIn3D, gAnyDecals, gRotOp, slimeFxt, DRDarkSlimeFix, DRWhite, DRPxl, DRPxlRect, colrIntensity, skyRootsFix


on applyDarkSlime me, q, c, effectR
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  cls = [color(255, 0,0), color(0,255, 0), color(0,0,255)]
  
  fc = solidAfaMv(point(q2,c2), 1)
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      dmin = 0
      dmax = 29
    "1":
      dmin = 0
      dmax = 9
    "2":
      dmin = 10
      dmax = 19
    "3":
      dmin = 20
      dmax = 29
    "1:st and 2:nd":
      dmin = 0
      dmax = 19
    "2:nd and 3:rd":
      dmin = 10
      dmax = 29
    otherwise:
      dmin = 0
      dmax = 29
  end case
  repeat with d = 0 to 29
    case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
      "All":
        lr = d
      "1":
        lr = restrict(d, 0, 9)
      "2":
        lr = restrict(d, 10, 19)
      "3":
        lr = restrict(d, 20, 29)
      "1:st and 2:nd":
        lr = restrict(d, 0, 19)
      "2:nd and 3:rd":
        lr = restrict(d, 10, 29)
      otherwise:
        lr = d
    end case
    if (lr=0)or(lr = 10)or(lr=20) then
      lraddc = 1+(lr>9)+(lr>19)
      sld = (solidMtrx[q2][c2][ lraddc ])
      if (DRDarkSlimeFix) then
        fc = solidAfaMv(point(q2,c2), lraddc)
      else
        fc = solidAfaMv(point(q2,c2)+gRenderCameraTilePos, lraddc)
      end if
    end if
    deepEffect = 0
    
    if (lr = 0)or(lr=10)or(lr=20)or(sld=0)then
      deepEffect = 1
    end if
    endofloop = effectR.mtrx[q2][c2]*(0.2 + (0.8*deepEffect))*0.01*80*fc
    repeat with cntr = 1 to endofloop
      if deepEffect then
        pnt = (point(q-1, c-1)*20)+point(random(20), random(20))
      else
        if random(2)=1 then
          pnt = (point(q-1, c-1)*20)+point(1 + 19*(random(2)-1), random(20))
        else 
          pnt = (point(q-1, c-1)*20)+point(random(20), 1 + 19*(random(2)-1))
        end if
      end if
      layerd = member("layer"&string(d)).image
      if (layerd.getPixel(pnt) <> DRWhite) and (d >= dmin) and (d <= dmax) then
        lgt = random(40)
        if (layerd.getPixel(pnt+point(0,lgt)) <> DRWhite)  and (d >= dmin) and (d <= dmax) then
          clr = cls[random(3)]
          layerlr = member("layer"&string(lr)).image
          layerlr.copyPixels(DRPxl, rect(pnt, pnt+point(1, lgt)), DRPxlRect, {#color:clr})
          if random(2)=1 then
            layerlr.copyPixels(DRPxl, rect(pnt, pnt+point(1, lgt))+rect(-1, 1, -1, -1), DRPxlRect, {#color:clr})
          else
            layerlr.copyPixels(DRPxl, rect(pnt, pnt+point(1, lgt))+rect(1, 1, 1, -1), DRPxlRect, {#color:clr})
          end if
        end if
      end if
    end repeat
  end repeat
end


on applyHugeFlower me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30)-1
    "1":
      d = random(10)-1
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(20)-1
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(30)-1
  end case
  lr = 1+(d>9)+(d>19)
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    startQuadToDraw = []
    quadsToDraw = []
    
    startQuadToDraw.add(rect(pnt.locH-3, pnt.locV-3, pnt.locH+3, mdPnt.locV+3))
    
    if not skyRootsFix then
      member("layer"&string(d)).image.copyPixels(member("flowerhead").image, rect(pnt.locH-3, pnt.locV-3, pnt.locH+3, mdPnt.locV+3), member("flowerhead").image.rect, {#color:colr, #ink:36})
    end if
    
    h = pnt.locV
    
    repeat while h < 30000 then
      h = h + 1
      pnt.locH = pnt.locH -2 + random(3)
      
      if skyRootsFix then
        quadsToDraw.add(rect(pnt.locH-1, h, pnt.locH+2, h+2))
      else
        member("layer"&string(d)).image.copyPixels(member("pxl").image, rect(pnt.locH-1, h, pnt.locH+2, h+2), member("pxl").image.rect, {#color:colr})
      end if
      
      tlPos = giveGridPos(point(pnt.locH, h)) + gRenderCameraTilePos
      
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
        member("layer"&string(d)).image.copyPixels(member("pxl").image, qdd, member("pxl").image.rect, {#color:colr})
      end repeat
      member("layer"&string(d)).image.copyPixels(member("flowerhead").image, startQuadToDraw[1], member("flowerhead").image.rect, {#color:colr, #ink:36})
    end if
    
    copyPixelsToEffectColor(gdLayer, d, rect(headPos.locH-37, headPos.locV-37, headPos.locH+37, h+10), "hugeFlowerMaskMask", member("hugeFlowerMask").image.rect, 0.8)
    
  end if
end


on ApplyArmGrower me, q, c, eftc
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
    
    lastDir = 180 - 101 + random(201)
    
    points = [pnt]
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000 then
      dir = 180 - 31 + random(61)
      dir = lerp(lastDir, dir, 0.75)
      lastPnt = pnt
      pnt = pnt + degToVec(dir)*30.0
      lastDir = dir
      
      rct = (lastPnt + pnt)/2.0
      rct = rect(rct, rct)
      rct = rct + rect(-10, -25, 10, 25)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      if(random(2)=1)then
        qd = flipQuadH(qd)
      end if
      
      points.add(pnt)
      
      var = random(13)
      
      if skyRootsFix then
        quadsToDraw.add([qd, rect((var-1)*20, 1, var*20, 50+1)])
      else
        member("layer"&string(d)).image.copyPixels(member("ArmGrowerGraf").image, qd, rect((var-1)*20, 1, var*20, 50+1), {#color:colr, #ink:36} )
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
        member("layer"&string(d)).image.copyPixels(member("ArmGrowerGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36} )
      end repeat
    end if
    
    if(points.count > 2)then
      repeat with p = 1 to points.count-1 then
        rct = (points[p] + points[p+1])/2.0
        rct = rect(rct, rct)
        rct = rct + rect(-12, -36, 12, 36)
        qd = rotateToQuad(rct, lookAtPoint(points[p], points[p+1]))
        
        copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, power((points.count-p.float+1)/points.count.float, 1.5))
      end repeat
    end if
    
    
    -- copyPixelsToEffectColor(gdLayer, d, rect(headPos.locH-37, headPos.locV-37, headPos.locH+37, h+10), "hugeFlowerMaskMask", member("hugeFlowerMask").image.rect, 0.8)
    
  end if
end


on ApplyThornGrower me, q, c, eftc
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
    blnd2 = 1
    
    wdth = 0.5
    
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
        member("layer"&string(d)).image.copyPixels(member("thornBushGraf").image, qd, rect((var-1)*20, 1, var*20, 50+1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qd, "thornBushGrad", rect((var-1)*20, 1, var*20, 50+1), 0.5, blnd)
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
        member("layer"&string(d)).image.copyPixels(member("thornBushGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "thornBushGrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
    -- copyPixelsToEffectColor(gdLayer, d, rect(headPos.locH-37, headPos.locV-37, headPos.locH+37, h+10), "hugeFlowerMaskMask", member("hugeFlowerMask").image.rect, 0.8)
    
  end if
end


on ApplyGarbageSpiral me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  frontWall = 1
  backWall = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(29)
      if(d <= 5)then
        backWall = 5
      else if (d >= 6)then
        frontWall = 6
      end if
    "1":
      d = random(9)
      if(d <= 5)then
        backWall = 5
      else if (d >= 6)then
        frontWall = 6
      end if
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(19)
      if(d <= 5)then
        backWall = 5
      else if (d >= 6)then
        frontWall = 6
      end if
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(29)
      if(d <= 5)then
        backWall = 5
      else if (d >= 6)then
        frontWall = 6
      end if
  end case
  lr = 1+(d>9)+(d>19)
  
  
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    dir = random(360)
    dirAdd = 40+random(20)
    if(random(2)=1)then
      dirAdd = -dirAdd
    end if
    
    grav = -0.7
    
    spiralWait = 15 + random(15)
    spiral = 1.0
    searchBase = -8--12
    
    loseSpiralTime = 60 + random(300)
    
    spiralFac = lerp(0.95, 0.91, (gEEprops.effects[r].mtrx[q2][c2]/100.0) * (random(1000)/1000.0))
    
    dpthSpeed = lerp(-1.0, 1.0, random(1000)/1000.0)/20.0
    
    conPoints = [[pnt, d, 0]]
    points = [[pnt, d, 1]]
    
    cntr = 0
    repeat while pnt.locV < 30000 then
      cntr = cntr + 1
      dir = dir + dirAdd
      
      dirAdd = dirAdd * spiralFac
      spiralFac = spiralFac + 0.0013
      if(spiralFac > 0.993)then
        spiralFac = 0.993
      end if
      
      lastPnt = pnt
      pnt = pnt + degToVec(dir)*3.0*power(spiral, 0.5)
      
      spiralWait = spiralWait - 1
      if(spiralWait < 0)then
        moveDir = point(0,0)
        repeat with dst = 1 to 7 then
          repeat with tst in [point(-1,0), point(1,0), point(1,1), point(0,1), point(-1, 1)] then
            tstPnt = giveGridPos(lastPnt) + gRenderCameraTilePos + tst*dst
            if(tstPnt.locH > 0)and(tstPnt.locH < gLOprops.size.locH-1)and(tstPnt.locV > 0)and(tstPnt.locV < gLOprops.size.locV-1)then
              moveDir = moveDir + (tst*gEEprops.effects[r].mtrx[tstPnt.locH][tstPnt.locV])
            end if
          end repeat
        end repeat
        pnt = pnt + (moveDir/4600.0)*searchBase*(1.0-power(spiral, 0.5))
        searchBase = searchBase + 0.15
        if(searchBase > 12)then
          searchBase = 12
        end if
        
        
        pnt.locV = pnt.locV + grav * (1.0-power(spiral, 0.5))
        grav = grav + 0.2 * (1.0-power(spiral, 0.5))--(abs(grav) + 0.8) * 0.009 * (1.0-power(spiral, 0.5))
        
        
        spiral = spiral - (1.0/loseSpiralTime.float)
        if(spiral < 0)then
          spiral = 0
          d = d + dpthSpeed
          if(d < frontWall)then
            d = frontWall
          else if (d > backWall) then
            d = backWall
          end if
          
        end if
      end if
      
      if(random(1000) < power(spiral, 4.0)*1000)then
        conPoints.add([pnt, d, cntr])
      end if
      
      pnt = lastPnt + moveToPoint(lastPnt, pnt, 3.0)
      
      points.add([pnt, d, spiral])
      
      
      
      
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
    
    repeat with cntr = 1 to conPoints.count then
      a = conPoints[random(conPoints.count)][1]
      blnd = (1.0-power(restrict(conPoints[cntr][3].float / points.count, 0, 1), 1.3))
      useD = restrict(conPoints[cntr][2].integer, frontWall, backWall)
      if(random(10)=1)then
        qd = rect(a.locH, a.locV, a.locH+1, a.locv+random(random(100)))
        member("layer"&string(points[1][2])).image.copyPixels(member("pxl").image, qd, rect(0,0,1,1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, useD, qd, "pxl", rect(0,0,1,1), 0.5, blnd)
      else
        b = conPoints[random(conPoints.count)][1]
        dir = moveToPoint(a, b, 1.0)
        perp = giveDirFor90degrToLine(-dir, dir)*0.5
        qd = [a - perp, a + perp, b + perp, b-perp]
        member("layer"&string(points[1][2])).image.copyPixels(member("pxl").image, qd, rect(0,0,1,1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, useD, qd, "pxl", rect(0,0,1,1), 0.5, blnd)
      end if
    end repeat
    
    lastPnt = points[1][1]
    lastUseD = points[1][2]
    repeat with q = 1 to points.count then
      pnt = points[q][1]
      rct = (lastPnt + pnt)/2.0
      rct = rect(rct, rct)
      rct = rct + rect(-1, -2, 1, 2)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      
      useD = restrict(points[q][2].integer, frontWall, backWall)
      blnd = 1.0-power(restrict(q.float / points.count, 0, 1), 1.3)
      blnd = lerp(blnd, 0.5, points[q][3])
      member("layer"&string(useD)).image.copyPixels(member("pxl").image, qd, rect(0,0,1,1), {#color:colr, #ink:36} )
      copyPixelsToEffectColor(gdLayer, useD, qd, "pxl", rect(0,0,1,1), 0.5, blnd)
      
      if(lastUseD <> useD)then
        member("layer"&string(lastUseD)).image.copyPixels(member("pxl").image, qd, rect(0,0,1,1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, lastUseD, qd, "pxl", rect(0,0,1,1), 0.5, blnd)
      end if
      
      lastUseD = useD
      lastPnt = pnt
    end repeat
    
    
    
    -- copyPixelsToEffectColor(gdLayer, d, rect(headPos.locH-37, headPos.locV-37, headPos.locH+37, h+10), "hugeFlowerMaskMask", member("hugeFlowerMask").image.rect, 0.8)
    
  end if
end




on ApplyRoller me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  frontWall = 1
  backWall = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"
    "All":
      d = random(29)
      if(d <= 5)then
        backWall = 5
      else if (d >= 6)then
        frontWall = 6
      end if
    "1":
      d = random(9)
      if(d <= 5)then
        backWall = 5
      else if (d >= 6)then
        frontWall = 6
      end if
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(19)
      if(d <= 5)then
        backWall = 5
      else if (d >= 6)then
        frontWall = 6
      end if
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(29)
      if(d <= 5)then
        backWall = 5
      else if (d >= 6)then
        frontWall = 6
      end if
  end case
  lr = 1+(d>9)+(d>19)
  
  
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    dir = random(360)
    dirAdd = (10+random(30))*0.3
    if(random(2)=1)then
      dirAdd = -dirAdd
    end if
    
    dspeed = (-11+random(21))/100.0
    
    lastUseD = d
    
    grav = 0.7
    
    points = [[pnt, d]]
    
    seedChance = 1.0
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000 then
      dir = dir - 11 + random(21) + dirAdd
      
      dspeed = restrict(dspeed + (-11+random(21))/1000.0, -0.1, 0.1)
      
      d = d + dspeed
      if(d < frontWall)then
        d = frontWall
        dspeed = random(10)/100.0
      else if (d > backWall)then
        d = backWall
        dspeed = -random(10)/100.0
      end if
      
      lastPnt = pnt
      pnt = pnt + degToVec(dir)*5.0
      pnt.locV = pnt.locV + grav
      
      grav = grav + 0.001
      
      
      
      rct = (lastPnt + pnt)/2.0
      rct = rect(rct, rct)
      rct = rct + rect(-1.5, -3.5, 1.5, 3.5)
      qd = rotateToQuad(rct, lookAtPoint(lastPnt, pnt))
      --      if(random(2)=1)then
      --        qd = flipQuadH(qd)
      --      end if
      
      
      --  var = random(13)
      
      useD = restrict(d.integer, frontWall, backWall)
      
      if(seedChance > 0)then
        repeat with a = 1 to 8 then
          if(random(1000)<power(seedChance, 1.5)*1000)then
            seedPos = pnt + MoveToPoint(pnt, lastPnt, (diag(pnt, lastPnt)*random(1000)).float/1000.0) + degToVec(random(360))*random(3)
            seedLr = restrict(useD - 2 + random(3), frontWall, backWall)
            
            if skyRootsFix then
              quadTryAdd = [0, seedLr, rect(seedPos,seedPos), -1]
              
              if(random(3) > 1) then
                seedLr = restrict(seedLr - 1, frontWall, backWall)
                quadTryAdd[4] = seedLr
              end if
              
              quadsToDraw.add(quadTryAdd)
            else
              member("layer"&string(seedLr)).image.copyPixels(member("rustDot").image, rect(seedPos,seedPos)+rect(-2, -2, 2, 2), member("rustDot").image.rect, {#color:colr, #ink:36} )
              copyPixelsToEffectColor(gdLayer, seedLr, rect(seedPos,seedPos)+rect(-2, -2, 2, 2), "rustDot", member("rustDot").image.rect, 0.8, 1)
              
              if(random(3) > 1)then
                seedLr = restrict(seedLr - 1, frontWall, backWall)
                member("layer"&string(seedLr)).image.copyPixels(member("pxl").image, rect(seedPos,seedPos)+rect(-1, -1, 1, 1), member("pxl").image.rect, {#color:colr} )
                copyPixelsToEffectColor(gdLayer, seedLr, rect(seedPos,seedPos)+rect(-1, -1, 1, 1), "pxl", member("pxl").image.rect, 0.8, 1)
              else
                member("layer"&string(seedLr)).image.copyPixels(member("pxl").image, rect(seedPos,seedPos)+rect(-1, -1, 1, 1), member("pxl").image.rect, {#color:color(255, 0, 0)} )
              end if
            end if
            
            
            
          end if
        end repeat
      end if
      seedChance = seedChance - random(100).float/2200.0
      
      points.add([pnt, useD])
      
      
      
      if skyRootsFix then
        quadsToDraw.add([1, useD, qd])
      else
        member("layer"&string(useD)).image.copyPixels(member("pxl").image, qd, rect(0,0,1,1), {#color:colr} )
      end if
      
      if(lastUseD <> useD)then
        if skyRootsFix then
          quadsToDraw.add([1, lastUseD, qd])
        else
          member("layer"&string(lastUseD)).image.copyPixels(member("pxl").image, qd, rect(0,0,1,1), {#color:colr} )
        end if
      end if
      
      lastUseD = useD
      
      tlPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
        exit
      end if
      
      if tlPos.inside(rect(1,1,gLOprops.size.loch+1,gLOprops.size.locv+1)) = 0 then
        exit repeat
      else if solidAfaMv(tlPos, 1 + (useD > 9) + (useD > 19)) = 1 then
        exit repeat
      end if
      
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        if (qdd[1]) then
          member("layer"&string(qdd[2])).image.copyPixels(member("pxl").image, qdd[3], rect(0,0,1,1), {#color:colr} )
        else
          member("layer"&string(qdd[2])).image.copyPixels(member("rustDot").image, qdd[3]+rect(-2, -2, 2, 2), member("rustDot").image.rect, {#color:colr, #ink:36} )
          copyPixelsToEffectColor(gdLayer, qdd[2], qdd[3]+rect(-2, -2, 2, 2), "rustDot", member("rustDot").image.rect, 0.8, 1)
          
          if(qdd[4] >= 0)then
            member("layer"&string(qdd[4])).image.copyPixels(member("pxl").image, qdd[3]+rect(-1, -1, 1, 1), member("pxl").image.rect, {#color:colr} )
            copyPixelsToEffectColor(gdLayer, qdd[4], qdd[3]+rect(-1, -1, 1, 1), "pxl", member("pxl").image.rect, 0.8, 1)
          else
            member("layer"&string(qdd[2])).image.copyPixels(member("pxl").image, qdd[3]+rect(-1, -1, 1, 1), member("pxl").image.rect, {#color:color(255, 0, 0)} )
          end if
        end if
      end repeat
    end if
    
    
    if(points.count > 2)then
      repeat with p = 1 to points.count-1 then
        rct = (points[p][1] + points[p+1][1])/2.0
        rct = rect(rct, rct)
        rct = rct + rect(-1.5, -3.5, 1.5, 3.5)
        qd = rotateToQuad(rct, lookAtPoint(points[p][1], points[p+1][1]))
        --  copyPixelsToEffectColor(gdLayer, useD, qd, "pxl", rect(0,0,1,1), 0.8)
        copyPixelsToEffectColor(gdLayer, points[p][2], qd, "pxl", rect(0,0,1,1), 0.8, power((points.count-p.float+1)/points.count.float, 1.5))
      end repeat
    end if
    
    
  end if
end


on applyHangRoots me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30)-1
    "1":
      d = random(10)-1
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(20)-1
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(30)-1
  end case
  lr = 1+(d>9)+(d>19)
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    -- member("layer"&string(d)).image.copyPixels(member("flowerhead").image, rect(pnt.locH-3, pnt.locV-3, pnt.locH+3, mdPnt.locV+3), member("flowerhead").image.rect, {#color:colr, #ink:36})
    lftBorder = mdPnt.locH-10
    rgthBorder =  mdPnt.locH+10
    
    quadsToDraw = []
    
    repeat while pnt.locV+gRenderCameraTilePos.locV*20 > -100 then
      
      -- member("layer"&string(d)).image.copyPixels(member("pxl").image, rect(pnt.locH-1, h, pnt.locH+2, h+2), member("pxl").image.rect, {#color:colr})
      lstPos = pnt
      pnt = pnt + degToVec(-45+random(90))*(2+random(6))
      pnt.locH = restrict(pnt.locH, lftBorder, rgthBorder)
      dir = moveToPoint(pnt, lstPos, 1.0)
      crossDir = giveDirFor90degrToLine(-dir, dir)
      qd = [pnt-crossDir, pnt+crossDir, lstPos+crossDir, lstPos-crossDir]
      
      
      if skyRootsFix then
        quadsToDraw.add(qd)
      else
        member("layer"&string(d)).image.copyPixels(member("pxl").image, qd, member("pxl").image.rect, {#color:color(255, 0, 0)})
      end if
      
      if solidAfaMv(giveGridPos(lstPos) + gRenderCameraTilePos, lr) = 1 then
        exit repeat
      end if
      
      if skyRootsFix and withinBoundsOfLevel(giveGridPos(lstPos) + gRenderCameraTilePos) = 0 then
        exit
      end if
      
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer"&string(d)).image.copyPixels(member("pxl").image, qdd, member("pxl").image.rect, {#color:color(255, 0, 0)})
      end repeat
    end if
    
  end if
end


on applyThickRoots me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  frontWall = 0
  backWall = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"
    "All":
      d = random(30)-1
    "1":
      d = random(10)-1
      backWall = 9
    "2":
      d = random(10)-1 + 10
      frontWall = 10
      backWall = 19
    "3":
      d = random(10)-1 + 20
      frontWall = 20
    "1:st and 2:nd":
      d = random(20)-1
      backWall = 19
    "2:nd and 3:rd":
      d = random(20)-1 + 10
      frontWall = 10
    otherwise:
      d = random(30)-1
  end case
  
  if(d > 5)then
    frontWall = 5+3
    d = restrict(d, frontWall, 29)
  else
    backWall = 5
  end if
  
  
  if (gLEprops.matrix[q2][c2][(1+(d>9)+(d>19))][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    health = 6
    points = [[pnt, d, health]]
    
    dir = 0
    
    floatDpth = d
    
    thickness = (gEEprops.effects[r].mtrx[q2][c2]/100.0)*power(random(10000)/10000.0, 0.3)
    
    repeat while pnt.locV+gRenderCameraTilePos.locV*20 > -100 then
      
      floatDpth = floatDpth + lerp(-0.3, 0.3, random(1000)/1000.0)
      if(floatDpth < frontWall)then
        floatDpth = frontWall
      else if(floatDpth > backWall)then
        floatDpth = backWall
      end if
      d = restrict(floatDpth.integer, frontWall, backWall)
      
      lstPos = pnt
      dir = lerp(dir, -45+random(90), 0.5)
      pnt = pnt + degToVec(dir)*(2+random(6))
      
      lstGridPos = giveGridPos(lstPos) + gRenderCameraTilePos
      gridPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      
      tlt = 0
      repeat with q = -1 to 1 then
        if (q<>0)and (gridPos.locH + q > 0)and(gridPos.locH + q < gEEprops.effects[r].mtrx.count)and(gridPos.locV-1 > 0)and(gridPos.locV-1 < gEEprops.effects[r].mtrx[1].count) and (lstGridPos.locH + q > 0)and(lstGridPos.locH + q < gEEprops.effects[r].mtrx.count)and(lstGridPos.locV-1 > 0)and(lstGridPos.locV-1 < gEEprops.effects[r].mtrx[1].count)then
          tlt = tlt + gEEprops.effects[r].mtrx[lstGridPos.locH+q][lstGridPos.locV-1]*q
        end if
      end repeat
      pnt.locH = pnt.locH + (tlt/100.0)*2.0
      gridPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      
      if(lstGridPos.locH <> gridPos.locH) then
        if (gridPos.locH > 0)and(gridPos.locH < gEEprops.effects[r].mtrx.count)and(gridPos.locV > 0)and(gridPos.locV < gEEprops.effects[r].mtrx[1].count) and (lstGridPos.locH > 0)and(lstGridPos.locH < gEEprops.effects[r].mtrx.count)and(lstGridPos.locV > 0)and(lstGridPos.locV < gEEprops.effects[r].mtrx[1].count) then
          if (gEEprops.effects[r].mtrx[gridPos.locH][gridPos.locV] = 0)and(gEEprops.effects[r].mtrx[lstGridPos.locH][lstGridPos.locV] > 0) then
            pnt.locH = restrict(pnt.locH, giveMiddleOfTile(giveGridPos(lstPos)).locH-9, giveMiddleOfTile(giveGridPos(lstPos)).locH+9)
          end if
        end if
      end if
      
      
      points.add([pnt, d, health])
      
      if solidAfaMv(lstGridPos, (1+(d>9)+(d>19))) = 1 then
        health = health - 1
        if(health < 1) then
          exit repeat
        end if
      else
        health = restrict(health+1, 0, 6)
      end if
      
      if skyRootsFix and withinBoundsOfLevel(lstGridPos) = 0 then
        exit
      end if
      
    end repeat
    
    lstPos = points[1][1] + point(0,1)
    lastRad = 0
    lastPerp = point(0,0)
    repeat with q = 1 to points.count then
      f = q.float / points.count.float
      pnt = points[q][1]
      d = points[q][2]
      dir = moveToPoint(pnt, lstPos, 1.0)
      perp = giveDirFor90degrToLine(-dir, dir)
      rad = 0.6 + f*8.0*(points[q][3].float/6.0)*lerp(0.8, 1.2, random(1000)/1000.0)*lerp(thickness, 0.5, 0.2)
      
      repeat with c in [[0, 1.0], [1, 0.7], [2, 0.3]] then
        if(d - c[1] >= 0)and((rad*c[2] > 0.8)or(c[1]=0))then
          qd = [pnt-perp*rad*c[2], pnt+perp*rad*c[2], lstPos+dir+lastPerp*lastRad*c[2], lstPos+dir-lastPerp*lastRad*c[2]]
          member("layer"&string(d - c[1])).image.copyPixels(member("pxl").image, qd, member("pxl").image.rect, {#color:color(0,255,0)})
        end if
      end repeat
      
      lstPos = pnt
      lastPerp = perp
      lastRad = rad
    end repeat
    
  end if
end


on applyShadowPlants me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  frontWall = 0
  backWall = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30)-1
    "1":
      d = random(10)-1
      backWall = 9
    "2":
      d = random(10)-1 + 10
      frontWall = 10
      backWall = 19
    "3":
      d = random(10)-1 + 20
      frontWall = 20
    "1:st and 2:nd":
      d = random(20)-1
      backWall = 19
    "2:nd and 3:rd":
      d = random(20)-1 + 10
      frontWall = 10
    otherwise:
      d = random(30)-1
  end case
  
  if(d > 5)then
    frontWall = 5+3
    d = restrict(d, frontWall, 29)
  else
    backWall = 5
  end if
  
  
  if (gLEprops.matrix[q2][c2][(1+(d>9)+(d>19))][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    health = 6
    points = [[pnt, d, health]]
    
    dir = 180
    
    -- floatDpth = d
    
    
    
    cycle = lerp(6.0, 12.0, random(10000)/10000.0)
    cntr = random(50)
    
    tltFac = 0.0
    
    repeat while pnt.locV+gRenderCameraTilePos.locV*20 > -100 then
      cntr = cntr + 1
      --      floatDpth = floatDpth + lerp(-0.3, 0.3, random(1000)/1000.0)
      --      if(floatDpth < frontWall)then
      --        floatDpth = frontWall
      --      else if(floatDpth > backWall)then
      --        floatDpth = backWall
      --      end if
      --      d = restrict(floatDpth.integer, frontWall, backWall)
      
      lstPos = pnt
      dir = lerp(dir, 180-45+random(90), 0.1)
      dir = dir + sin((cntr/cycle)*PI*2.0)*8
      cycle = cycle + 0.1
      if(cycle > 35) then cycle = 35
      pnt = pnt + degToVec(dir)*3
      
      lstGridPos = giveGridPos(lstPos) + gRenderCameraTilePos
      gridPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      
      tlt = 0
      repeat with q = -1 to 1 then
        if (q<>0)and (lstGridPos.locH + q > 0)and(lstGridPos.locH + q < gEEprops.effects[r].mtrx.count)and(lstGridPos.locV+1 > 0)and(lstGridPos.locV+1 < gEEprops.effects[r].mtrx[1].count) then
          tlt = tlt + gEEprops.effects[r].mtrx[lstGridPos.locH+q][lstGridPos.locV+1]*q
        end if
      end repeat
      pnt.locH = pnt.locH + (tlt/100.0)*lerp(-2.0, 1.0, power(tltFac, 0.85))
      gridPos = giveGridPos(pnt) + gRenderCameraTilePos
      
      tltFac = tltFac + 0.002
      if(tltFac > 1.0)then tltFac = 1.0
      --      
      --      
      --      if(lstGridPos.locH <> gridPos.locH) then
      --        if (gridPos.locH > 0)and(gridPos.locH < gEEprops.effects[r].mtrx.count)and(gridPos.locV > 0)and(gridPos.locV < gEEprops.effects[r].mtrx[1].count) then
      --          if (lstGridPos.locH  > 0)and(lstGridPos.locH < gEEprops.effects[r].mtrx.count)and(lstGridPos.locV > 0)and(lstGridPos.locV < gEEprops.effects[r].mtrx[1].count) then
      --            if (gEEprops.effects[r].mtrx[gridPos.locH][gridPos.locV] = 0)and(gEEprops.effects[r].mtrx[lstGridPos.locH][lstGridPos.locV] > 0) then
      --              pnt.locH = restrict(pnt.locH, giveMiddleOfTile(giveGridPos(lstPos)).locH-9, giveMiddleOfTile(giveGridPos(lstPos)).locH+9)
      --            end if
      --          end if
      --        end if
      --      end if
      
      
      points.add([pnt, d, health])
      
      if solidAfaMv(lstGridPos, (1+(d>9)+(d>19))) = 1 then
        health = health - 1
        if(health < 1) then
          exit repeat
        end if
      else
        health = restrict(health+1, 0, 6)
      end if
      
      if skyRootsFix and withinBoundsOfLevel(lstGridPos) = 0 then
        exit
      end if
      
    end repeat
    
    fuzzLength = 20+random(50)
    
    thickness = (gEEprops.effects[r].mtrx[q2][c2]/100.0)*power(random(10000)/10000.0, 0.3)
    thickness = lerp(thickness, restrict(points.count.float, 20.0, 180.0)/180.0, 0.5)
    
    lstPos = points[1][1] + point(0,1)
    lastRad = 0
    lastPerp = point(0,0)
    repeat with q = 1 to points.count then
      f = q.float / points.count.float
      pnt = points[q][1]
      d = points[q][2]
      dir = moveToPoint(pnt, lstPos, 1.0)
      perp = giveDirFor90degrToLine(-dir, dir)
      --rad = 0.6 + f*8.0*(points[q][3].float/6.0)*lerp(0.8, 1.2, random(1000)/1000.0)*lerp(thickness, 0.5, 0.2)
      f =  sin(f*PI*0.5)
      rad = 1.1 + f*7.0*(points[q][3].float/6.0)*lerp(thickness, 0.5, 0.2)
      
      
      
      
      repeat with c in [[0, 1.0], [1, 0.7], [2, 0.3]] then
        if(d - c[1] >= 0)and((rad*c[2] > 0.8)or(c[1]=0))then
          qd = [pnt-perp*rad*c[2], pnt+perp*rad*c[2], lstPos+dir+lastPerp*lastRad*c[2], lstPos+dir-lastPerp*lastRad*c[2]]
          member("layer"&string(d - c[1])).image.copyPixels(member("pxl").image, qd, member("pxl").image.rect, {#color:color(0,0,255)})
          
          if(random(30) = 1)then
            me.sporeGrower(pnt + MoveToPoint(pnt, lstPos, diag(pnt, lstPos)*random(10000)/10000.0), 15 + random(50) * (1.0-f), d - c[1], color(0,0,255))
          end if
          
          if(q < fuzzLength) and(random(fuzzLength) > q)and(random(6)=1) then
            f2 = q.float / fuzzLength.float
            me.sporeGrower(pnt + MoveToPoint(pnt, lstPos, diag(pnt, lstPos)*random(10000)/10000.0), 65 + random(50) * (1.0-f2), d - c[1], color(0,0,255))
          end if
        end if
      end repeat
      
      lstPos = pnt
      lastPerp = perp
      lastRad = rad
    end repeat
    
  end if
end

on sporeGrower me, pos, lngth, layer, col
  dir = point(0, -1)
  
  repeat with q = 1 to lngth then
    otherCol = member("layer"&layer).image.getPixel(pos.locH-1, pos.locV-1)
    if(otherCol <> col)and(otherCol <> color(255, 255, 255))then
      exit repeat
    else
      member("layer"&layer).image.setPixel(pos.locH-1, pos.locV-1, col)
      pos = pos + dir
      
      if(dir.locV = -1)and(random(2)=1)then
        if(random(2)=1)then
          dir = point(-1, 0)
        else
          dir = point(1, 0)
        end if
      else 
        dir = point(0, -1)
      end if
    end if
  end repeat
end


on applyDaddyCorruption me, q, c, amount
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  mdPnt = giveMiddleOfTile(point(q,c))
  global daddyCorruptionHoles
  
  extraHoleChance = 1
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      dmin = 0
      dmax = 29
      dmax2 = 26
    "1":
      dmin = 0
      dmax = 6
      dmax2 = 6
    "2":
      dmin = 10
      dmax = 16
      dmax2 = 16
    "3":
      dmin = 20
      dmax = 29
      dmax2 = 26
    "1:st and 2:nd":
      dmin = 0
      dmax = 16
      dmax2 = 16
    "2:nd and 3:rd":
      dmin = 10
      dmax = 29
      dmax2 = 26
    otherwise:
      dmin = 0
      dmax = 29
      dmax2 = 26
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
      repeat with d = 0 to 2 then
        if(dp+d <= dmax) and (dp+d >= dmin) then
          if(rad <= 10)then
            member("layer"&string(dp+d)).image.copyPixels(member("DaddyBulb").image, rect(startPos, startPos)+rect(-rad,-rad,rad,rad), rect(0, 1+d*20, 20, 1+(d+1)*20), {#ink:36})
          else
            member("layer"&string(dp+d)).image.copyPixels(member("DaddyBulb").image, rect(startPos, startPos)+rect(-rad,-rad,rad,rad), rect(20, 1+d*40, 60, 1+(d+1)*40), {#ink:36})
          end if
        else
          exit repeat
        end if
      end repeat
      
      if((random(3) = 1)or(extraHoleChance=1))and(dp <= dmax2) and (dp >= dmin)then
        daddyCorruptionHoles.add([startPos, rad * (50+random(50))*0.01, random(360), dp, amount])
        extraHoleChance = 0
      end if
    end if
  end repeat  
end

on applyCorruptionNoEye me, q, c, amount
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
      repeat with d = 0 to 2 then
        if(dp+d <= dmax) and (dp+d >= dmin) then
          if(rad <= 10)then
            member("layer"&string(dp+d)).image.copyPixels(member("CNEBulb").image, rect(startPos, startPos)+rect(-rad,-rad,rad,rad), rect(0, 1+d*20, 20, 1+(d+1)*20), {#ink:36})
          else
            member("layer"&string(dp+d)).image.copyPixels(member("CNEBulb").image, rect(startPos, startPos)+rect(-rad,-rad,rad,rad), rect(20, 1+d*40, 60, 1+(d+1)*40), {#ink:36})
          end if
        else
          exit repeat
        end if
      end repeat
    end if
  end repeat
end


on applyWire(me, q, c, eftc)
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  global gCurrentRenderCamera
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30) - 1
    "1":
      d = random(10) - 1
    "2":
      d = random(10) - 1 + 10
    "3":
      d = random(10) - 1 + 20
    "1:st and 2:nd":
      d = random(20) - 1
    "2:nd and 3:rd":
      d = random(20) - 1 + 10
    otherwise:
      d = random(30) - 1
  end case
  lr = 1 + (d > 9) + (d > 19)
  if (gLEprops.matrix[q2][c2][lr][1] = 0) then
    layerd = member("layer" & string(d)).image
    member("wireImage").image = image(layerd.width, layerd.height, 1)
    wireImg = member("wireImage").image
    mdPnt = giveMiddleOfTile(point(q, c))
    startPos = mdPnt+point(-11 + random(21), -11 + random(21))
    myCamera = me.closestCamera(startPos + gRenderCameraTilePos * 20)
    if (myCamera = 0) then
      exit
    end if
    fatness = 1
    case fatOp of
      "2px":
        fatness = 2
      "3px":
        fatness = 3
      "random":
        fatness = random(3)
    end case
    a = 1.0 + random(100) + random(random(random(900)))
    keepItFromToForty = random(30)
    a = ((a * keepItFromToForty) + 40.0) / (keepItFromToForty + 1.0)
    addNRct = rect(-(fatness > 1), -(fatness > 1), (fatness = 3), (fatness = 3))
    wireImg.copypixels(DRPxl, rect(startPos.locH, startPos.locV - 1, startPos.locH + 1, startPos.locV + 1) + addNRct, rect(0, 0, 1, 1), {#color:color(0, 0, 0)})
    goodStops = 0
    repeat with dir = 0 to 1
      pnt = point(startPos.locH, startPos.locV)
      lastPnt = point(startPos.locH, startPos.locV)
      repeat with rep = 1 to 1000
        pnt.locH = startPos.locH + (-1 + 2 * dir) * rep
        pnt.locV = startPos.locV + a - (power(2.71828183, rep / a) + power(2.71828183, -rep / a)) * (a / 2.0)
        dr = moveToPoint(lastPnt, pnt, fatness.float)
        wireImg.copypixels(DRPxl, rect(pnt.locH, pnt.locV, pnt.locH + 1, lastPnt.locV + 1) + addNRct, rect(0, 0, 1, 1), {#color:color(0, 0, 0)})
        lastPnt = point(pnt.locH, pnt.locV)
        tlPos = giveGridPos(point(pnt.locH, pnt.locV)) + gRenderCameraTilePos
        if (tlPos.inside(rect(1, 1, gLOprops.size.loch + 1, gLOprops.size.locv + 1)) = 0) then
          exit repeat
        else 
          if(myCamera = gCurrentRenderCamera)and(me.seenByCamera(myCamera, pnt + gRenderCameraTilePos)=1) then
            if (gLEprops.matrix[tlPos.locH][tlPos.locV][lr][1] = 1) then
              if (layerd.getPixel(pnt) <> DRWhite) then
                goodStops = goodStops + 1
                exit repeat
              end if
            end if
          else
            if (solidAfaMv(tlPos, lr)) then
              goodStops = goodStops + 1
              exit repeat
            end if
          end if
        end if
      end repeat
    end repeat
    if (goodStops = 2) then
      layerd.copyPixels(wireImg, wireImg.rect, wireImg.rect, {#color:color(255, 0, 0), #ink:36})
    end if
  end if
end

on applyChain me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  global gCurrentRenderCamera
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30)-1
    "1":
      d = random(10)-1
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(20)-1
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(30)-1
  end case
  
  
  lr = 1+(d>9)+(d>19)
  
  big = 0
  
  case chOp of
    "FAT":
      big = 1
  end case
  
  
  --repeat with lmao = 0 to 100 then
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    member("wireImage").image = image(member("layer"&string(d)).image.width, member("layer"&string(d)).image.height, 1)
    mdPnt = giveMiddleOfTile(point(q,c))
    startPos = mdPnt+point(-11+random(21), -11+random(21))
    
    myCamera = me.closestCamera(startPos+gRenderCameraTilePos*20)
    if(myCamera = 0)then
      exit
    end if
    
    
    a = 1.0+random(100)+random(random(random(900)))
    keepItFromToForty = random(30)
    a = ((a*keepItFromToForty)+40.0)/(keepItFromToForty+1.0)
    
    if big then
      a = a + 10
    end if
    
    origOrnt = random(2)-1
    
    goodStops = 0
    repeat with dir = 0 to 1 then
      pnt = point(startPos.locH, startPos.locV)
      lastPnt = point(startPos.locH, startPos.locV)
      if dir = 0 then
        ornt = origOrnt
      else
        ornt = 1-origOrnt
      end if
      repeat with rep = 1 to 4000 then
        checkterrain = 0
        
        pnt.locH = startPos.locH +(-1 + 2*dir)*rep*0.25
        pnt.locV = startPos.locV + a - (power(2.71828183, (rep*0.25)/a)+power(2.71828183, -(rep*0.25)/a))*(a/2.0)
        
        if big = 0 then
          if diag(pnt, lastPnt)>=7 then
            if ornt then
              pos = (pnt+lastPnt)*0.5
              rct = rect(pos,pos)+rect(-3,-5,3,5)
              gtRect = rect(0,0,6,10)
              ornt = 0
            else
              pos = (pnt+lastPnt)*0.5
              rct = rect(pos,pos)+rect(-1,-5,1,5)
              gtRect = rect(7,0,8,10)
              ornt = 1
            end if
            member("wireImage").image.copypixels(member("chainSegment").image, rotateToQuad(rct, lookAtPoint(lastPnt,pnt)), gtRect, {#color:color(0,0,0), #ink:36})
            lastPnt = point(pnt.locH, pnt.locV)
            checkterrain = 1
          end if
        else
          if diag(pnt, lastPnt)>=12 then
            if ornt then
              pos = (pnt+lastPnt)*0.5
              rct = rect(pos,pos)+rect(-6,-10,6,10)
              gtRect = rect(0,0,12,20)
              ornt = 0
            else
              pos = (pnt+lastPnt)*0.5
              rct = rect(pos,pos)+rect(-2,-10,2,10)
              gtRect = rect(13,0,16,20)
              ornt = 1
            end if
            member("wireImage").image.copypixels(member("bigChainSegment").image, rotateToQuad(rct, lookAtPoint(lastPnt,pnt)), gtRect, {#color:color(0,0,0), #ink:36})
            lastPnt = point(pnt.locH, pnt.locV)
            checkterrain = 1
          end if
        end if
        
        if checkterrain then
          tlPos = giveGridPos(point(pnt.locH, pnt.locV)) + gRenderCameraTilePos
          if tlPos.inside(rect(1,1,gLOprops.size.loch+1,gLOprops.size.locv+1)) = 0 then
            exit repeat
          else 
            if(myCamera = gCurrentRenderCamera)and(me.seenByCamera(myCamera, pnt + gRenderCameraTilePos)=1) then
              if gLEprops.matrix[tlPos.locH][tlPos.locV][lr][1] = 1 then
                if member("layer"&string(d)).image.getPixel(pnt) <> color(255,255,255) then
                  goodStops = goodStops + 1
                  exit repeat
                end if
              end if
            else
              if solidAfaMv(tlPos, lr) then
                goodStops = goodStops + 1
                exit repeat
              end if
            end if
          end if
        end if
        
        
      end repeat
    end repeat
    
    
    if goodStops = 2 then
      member("layer"&string(d)).image.copyPixels(member("wireImage").image, member("wireImage").image.rect, member("wireImage").image.rect, {#color:color(255, 0, 0), #ink:36})
    end if
  end if
  --end repeat
end


on applyFungiFlower me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      layer = random(3)
    "1":
      layer = 1
    "2":
      layer = 2
    "3":
      layer = 3
    "1:st and 2:nd":
      layer = random(2)
    "2:nd and 3:rd":
      layer = random(2) + 1
    otherwise:
      layer = random(3)
  end case
  
  lr = ((layer-1)*10) + random(9) - 1
  
  
  
  if (afaMvLvlEdit(point(q2,c2), layer)=0) then
    rnd = 0
    if (afaMvLvlEdit(point(q2,c2+1), layer)=1) then
      rnd = gEffectProps.list[gEffectProps.listPos]
      flp = random(2)-1
      closestEdge = 1000
      repeat with a = - 5 to 5 then
        if (afaMvLvlEdit(point(q2+a,c2+1), layer)<>1) then
          if abs(a) <= abs(closestEdge) then
            flp = (a>0)
            closestEdge = a
            if a = 0 then
              flp = random(2)-1
            end if
          end if
        end if
      end repeat
      
      pnt = giveMiddleOfTile(point(q,c))+point(-10+random(20), 10)
    else if (afaMvLvlEdit(point(q2+1,c2), layer)=1) then
      rnd = 1
      flp = 0
      pnt = giveMiddleOfTile(point(q,c))+point(10, -random(10))
    else if (afaMvLvlEdit(point(q2-1,c2), layer)=1) then
      rnd = 1
      flp = 1
      pnt = giveMiddleOfTile(point(q2,c2))+point(-10, -random(10))
    end if
    
    
    if rnd <> 0 then
      rct = rect(pnt, pnt) + rect(-80, -80, 80, 80)
      gtRect = rect((rnd-1)*160, 0, rnd*160, 160)+rect(1,0,1,0)
      if flp then
        rct = vertFlipRect(rct)
      end if
      member("layer"&string(lr)).image.copyPixels(member("fungiFlowersGraf").image, rct, gtRect, {#ink:36})
    end if
  end if
  
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [2,3,4,5]
    l2 = []
    repeat with a = 1 to 4 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end


on applyLHFlower me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      layer = random(3)
    "1":
      layer = 1
    "2":
      layer = 2
    "3":
      layer = 3
    "1:st and 2:nd":
      layer = random(2)
    "2:nd and 3:rd":
      layer = random(2) + 1
    otherwise:
      layer = random(3)
  end case
  lr = ((layer-1)*10) + random(9) - 1
  if (afaMvLvlEdit(point(q2,c2), layer)=0) then
    
    rnd = gEffectProps.list[gEffectProps.listPos]
    flp = random(2)-1
    pnt = giveMiddleOfTile(point(q,c))+point(-10+random(20), 10)
    
    rct = rect(pnt, pnt) + rect(-40, -160, 40, 20)
    gtRect = rect((rnd-1)*80, 0, rnd*80, 180)+rect(1,0,1,0)
    if flp then
      rct = vertFlipRect(rct)
    end if
    member("layer"&string(lr)).image.copyPixels(member("lightHouseFlowersGraf").image, rct, gtRect, {#ink:36})
  end if
  
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [1,2,3,4,5,6,7,8]
    l2 = []
    repeat with a = 1 to 8 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end


on applyBlackGoo me, q, c, eftc
  sPnt = giveMiddleOfTile(point(q,c))+point(-10,-10)
  rct = member("blob").image.rect
  repeat with d = 1 to 10 then
    repeat with e = 1 to 10 then
      ps = point(sPnt.locH + d*2, sPnt.locV + e*2)
      if member("layer0").image.getPixel(ps) = color(255, 255, 255) then
        member("blackOutImg1").image.copyPixels(member("blob").image, rect(ps.locH-6-random(random(11)),ps.locV-6-random(random(11)),ps.locH+6+random(random(11)),ps.locV+6+random(random(11))), rct, {#color:0, #ink:36})
        member("blackOutImg2").image.copyPixels(member("blob").image, rect(ps.locH-7-random(random(14)),ps.locV-7-random(random(14)),ps.locH+7+random(random(14)),ps.locV+7+random(random(14))), rct, {#color:0, #ink:36})
      end if 
    end repeat
  end repeat
end

on applyRestoreEffect me, q, c, q2, c2, eftc
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      layers = [1,2,3]
    "1":
      layers = [1]
    "2":
      layers = [2]
    "3":
      layers = [3]
    "1:st and 2:nd":
      layers = [1,2]
    "2:nd and 3:rd":
      layers = [2,3]
    otherwise:
      layers = [1,2,3]
  end case
  
  repeat with layer in layers then
    if(afaMvLvlEdit(point(q2, c2), layer)=1)then
      mdPoint = giveMiddleOfTile(point(q,c))
      tlRct = rect(mdPoint+point(-10, -10), mdPoint+point(10,10))
      
      --        member("layer" & lr).image.copyPixels(member("pxl").image, rect(mdPoint-point(10, 10), mdPoint+point(10,10)), rect(0,0,1,1), {#color:color(255, 0, 0)})
      --    
      
      A = 2
      B = 1
      
      U = A
      if(me.isTileSolidAndAffected(point(q2-1, c2), layer) = 1)then
        U = B
      end if
      repeat with lr = ((layer-1)*10) + 4 to ((layer-1)*10) + 6 then
        member("layer" & lr).image.copyPixels(member("pxl").image, rect(mdPoint+point(-10, -10), mdPoint+point(-10+U,10)), rect(0,0,1,1), {#color:color(255, 0, 0)})
      end repeat
      me.draw3DBeams(q2, c2, layer, tlRct, [1,4], U)
      
      U = A
      if(me.isTileSolidAndAffected(point(q2+1, c2), layer) = 1)then
        U = B
      end if
      repeat with lr = ((layer-1)*10) + 4 to ((layer-1)*10) + 6 then
        member("layer" & lr).image.copyPixels(member("pxl").image, rect(mdPoint+point(10-U, -10), mdPoint+point(10,10)), rect(0,0,1,1), {#color:color(255, 0, 0)})
      end repeat
      me.draw3DBeams(q2, c2, layer, tlRct, [2,3], U)
      
      U = A
      if(me.isTileSolidAndAffected(point(q2, c2-1), layer) = 1)then
        U = B
      end if
      repeat with lr = ((layer-1)*10) + 4 to ((layer-1)*10) + 6 then
        member("layer" & lr).image.copyPixels(member("pxl").image, rect(mdPoint+point(-10, -10), mdPoint+point(10,-10+U)), rect(0,0,1,1), {#color:color(255, 0, 0)})
      end repeat
      me.draw3DBeams(q2, c2, layer, tlRct, [1,2], U)
      
      U = A
      if(me.isTileSolidAndAffected(point(q2, c2+1), layer) = 1)then
        U = B
      end if
      repeat with lr = ((layer-1)*10) + 4 to ((layer-1)*10) + 6 then
        member("layer" & lr).image.copyPixels(member("pxl").image, rect(mdPoint+point(-10, 10-U), mdPoint+point(10,10)), rect(0,0,1,1), {#color:color(255, 0, 0)})
      end repeat
      me.draw3DBeams(q2, c2, layer, tlRct, [3,4], U)
      
    end if
    reDrawPoles(point(q2,c2), layer, q, c, ((layer-1)*10) + 4)
  end repeat
end

on draw3DBeams me, q2, c2, layer, tlRct, crnrs, U
  if(layer > 1) then
    if(me.isTileSolidAndAffected(point(q2, c2), layer-1) = 1)then
      repeat with crnr in crnrs then
        rct = CornerRect(tlRct, crnr, U)
        repeat with lr = ((layer-1)*10) - 5 to ((layer-1)*10) + 5 then
          member("layer" & lr).image.copyPixels(member("pxl").image, rct, rect(0,0,1,1), {#color:color(255, 0, 0)})
        end repeat
      end repeat
    end if
  end if
  if(layer < 3) then
    if(me.isTileSolidAndAffected(point(q2, c2), layer+1) = 1)then
      rct = CornerRect(tlRct, crnr, U)
      repeat with crnr in crnrs then
        repeat with lr = ((layer-1)*10) + 5 to ((layer-1)*10) + 15 then
          member("layer" & lr).image.copyPixels(member("pxl").image, rct, rect(0,0,1,1), {#color:color(255, 0, 0)})
        end repeat
      end repeat
    end if
  end if
end


on CornerRect(tlRct, crnr, U)
  -- tlRct = tlRct+rect(1,1,-1,-1)
  case crnr of
    1:
      return rect(tlRct.left, tlRct.top, tlRct.left+U, tlRct.top+U)
    2:
      return rect(tlRct.right-U, tlRct.top, tlRct.right, tlRct.top+U)
    3:
      return rect(tlRct.right-U, tlRct.bottom-U, tlRct.right, tlRct.bottom)
    4:
      return rect(tlRct.left, tlRct.bottom-U, tlRct.left+U, tlRct.bottom)
  end case
end

on isTileSolidAndAffected me, tl, layer
  if(afaMvLvlEdit(point(tl.locH, tl.locV), layer)<>1)or(tl.locH<1)or(tl.locV<1)or(tl.locH > gLOprops.size.locH)or(tl.locV > gLOprops.size.locV)then
    return 0
  else if (gEEprops.effects[r].mtrx[tl.locH][tl.locV] > 0)then
    return 1
  else
    return 0
  end if
end



on reDrawPoles(pos, layer, q, c, drawLayer)
  global gLEProps, gLOprops
  if pos.inside(rect(1,1,gLOprops.size.loch+1,gLOprops.size.locv+1)) then
    repeat with t in gLEProps.matrix[pos.locH][pos.locV][layer][2] then
      case t of
        1:
          rct = rect((q-1)*20, (c-1)*20, q*20, c*20)+rect(0, 8, 0, -8)--rect(gRenderCameraTilePos,gRenderCameraTilePos)*20
          member("layer" & drawLayer).image.copyPixels(member("pxl").image, rct, member("pxl").image.rect, {color:color(255, 0, 0)})
        2:
          rct = rect((q-1)*20, (c-1)*20, q*20, c*20)+rect(8, 0, -8, 0)--rect(gRenderCameraTilePos,gRenderCameraTilePos)*20
          member("layer" & drawLayer).image.copyPixels(member("pxl").image, rct, member("pxl").image.rect, {color:color(255, 0, 0)})
      end case
    end repeat
  end if
end



on closestCamera me, pos
  global gCameraProps
  closest = 1000
  bestCam = 0
  repeat with camNum = 1 to gCameraProps.cameras.count then
    if(me.seenByCamera(camNum, pos) = 1)and(diag(pos, gCameraProps.cameras[camNum]+point(1400/2, 800/2)) < closest )then
      closest = diag(pos, gCameraProps.cameras[camNum]+point(1400/2, 800/2))
      bestCam = camNum
    end if
  end repeat
  
  return bestCam
end

on seenByCamera me, camNum, pos
  global gCameraProps
  
  cameraPos = gCameraProps.cameras[camNum]
  
  if pos.inside(rect(cameraPos.locH, cameraPos.locV, cameraPos.locH+1400, cameraPos.locV+800)+(rect(-15, -10, 15, 10)*20))then
    return 1
  else
    return 0
  end if
  
end

on applyBigPlant me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      layer = random(3)
    "1":
      layer = 1
    "2":
      layer = 2
    "3":
      layer = 3
    "1:st and 2:nd":
      layer = random(2)
    "2:nd and 3:rd":
      layer = random(2) + 1
    otherwise:
      layer = random(3)
  end case
  
  mem = "fern"
  case gEEprops.effects[r].nm of
    "Fern":
    "Giant Mushroom":
      mem = "giantMushroom"
  end case
  
  lr = ((layer-1)*10) + random(9) - 1
  if (afaMvLvlEdit(point(q2,c2), layer)=0) then
    
    rnd = gEffectProps.list[gEffectProps.listPos]
    flp = random(2)-1
    pnt = giveMiddleOfTile(point(q,c))+point(-10+random(20), 10)
    
    rct = rect(pnt, pnt) + rect(-50, -80, 50, 20)
    gtRect = rect((rnd-1)*100, 0, rnd*100, 100)+rect(1,1,1,1)
    if flp then
      rct = vertFlipRect(rct)
    end if
    member("layer"&string(lr)).image.copyPixels(member(mem&"Graf").image, rct, gtRect, {#ink:36, #color:colr})
    
    pnt = depthPnt(pnt, lr-5)
    rct = rect(pnt, pnt) + rect(-50, -80, 50, 20)
    if flp then
      rct = vertFlipRect(rct)
    end if
    copyPixelsToEffectColor(gdLayer, lr, rct, mem&"Grad",rect((rnd-1)*100, 0, rnd*100, 100)+rect(1,1,1,1), 0.5)
  end if
  
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [1,2,3,4,5,6,7,8]
    l2 = []
    repeat with a = 1 to 8 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end











