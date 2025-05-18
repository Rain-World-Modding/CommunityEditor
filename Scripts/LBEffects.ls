global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, chOp, fatOp, gradAf, effectIn3D, gAnyDecals, gRotOp, slimeFxt, DRDarkSlimeFix, DRWhite, DRPxl, DRPxlRect, colrIntensity, skyRootsFix


on applyWLPlant(me, q, c)
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV 
  amount = 20
  case lrSup of
    "All":
      lsL = [1,2,3]
    "1":
      lsL = [1]
    "2":
      lsL = [2]
    "3":
      lsL = [3]
    "1:st and 2:nd":
      lsL = [1,2]
    "2:nd and 3:rd":
      lsL = [2,3]
    otherwise:
      lsL = [1,2,3]
  end case
  bldr = gEEprops.effects[r].mtrx[q2][c2] * 1.5
  repeat with layer in lsL
    editM = afaMvLvlEdit(point(q2, c2), layer)
    if (editM = 1) or (editM = 2) or (editM = 3) or (editM = 4) or (editM = 5) or (editM = 6) then
      repeat with cntr = 1 to gEEprops.effects[r].mtrx[q2][c2] * 0.01 * amount
        pnt = giveMiddleOfTile(point(q, c)) - point([-1, 1, -2, 2][random(4)], [-1, 1, -2, 2][random(4)])
        edit = afaMvLvlEdit(point(q2, c2 + 1), layer)
        bttmR = ((edit = 0) or (edit = 7) or (edit = 8) or (edit = 9) or (edit = -1) or (edit = 6))
        edit = afaMvLvlEdit(point(q2, c2 - 1), layer)
        tpR = ((edit = 0) or (edit = 7) or (edit = 8) or (edit = 9) or (edit = -1) or (edit = 6))
        edit = afaMvLvlEdit(point(q2 + 1, c2), layer)
        rghtR = ((edit = 0) or (edit = 7) or (edit = 8) or (edit = 9) or (edit = -1) or (edit = 6))
        edit = afaMvLvlEdit(point(q2 - 1, c2), layer)
        lftR = ((edit = 0) or (edit = 7) or (edit = 8) or (edit = 9) or (edit = -1) or (edit = 6))
        case editM of
          2:
            tpR = 0
            rghtR = 0
            slp1 = random(21) - 11
            rand2 = random(2)
            baseRect = rect(pnt, pnt) + rect(-1 + slp1, -1 + slp1, 1 + slp1, 1 + slp1) + rect(-rand2, -rand2, rand2, rand2)
            repeat with rep = 2 to 9
              rct = rotateToQuad(baseRect, random(360))
              rubbl = random(4)
              varLr = (layer - 1) * 10 + rep
              member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
              if (gdLayer <> "C") then
                member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
              end if                           
            end repeat
          3:
            tpR = 0
            lftR = 0
            slp1 = random(21) - 11
            rand2 = random(2)
            baseRect = rect(pnt, pnt) + rect(-1 + slp1, -1 - slp1, 1 + slp1, 1 - slp1) + rect(-rand2, -rand2, rand2, rand2)
            repeat with rep = 2 to 9
              rct = rotateToQuad(baseRect, random(360))
              rubbl = random(4)
              varLr = (layer - 1) * 10 + rep
              member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
              if (gdLayer <> "C") then
                member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
              end if                           
            end repeat
          4:
            bttmR = 0
            rghtR = 0
            slp1 = random(21) - 11
            rand2 = random(2)
            baseRect = rect(pnt, pnt) + rect(-1 - slp1, -1 + slp1, 1 - slp1, 1 + slp1) + rect(-rand2, -rand2, rand2, rand2)
            repeat with rep = 2 to 9
              rct = rotateToQuad(baseRect, random(360))
              rubbl = random(4)
              varLr = (layer - 1) * 10 + rep
              member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
              if (gdLayer <> "C") then
                member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
              end if                           
            end repeat
          5:
            bttmR = 0
            lftR = 0
            slp1 = random(21) - 11
            rand2 = random(2)
            baseRect = rect(pnt, pnt) + rect(-1 - slp1, -1 - slp1, 1 - slp1, 1 - slp1) + rect(-rand2, -rand2, rand2, rand2)
            repeat with rep = 2 to 9
              rct = rotateToQuad(baseRect, random(360))
              rubbl = random(4)
              varLr = (layer - 1) * 10 + rep
              member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
              if (gdLayer <> "C") then
                member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
              end if                           
            end repeat
          6:
            tpR = 0
            lftR = 0
            bttmR = 0
            rghtR = 0
            rand = random(10) - random(10)
            rand2 = random(2)
            baseRect = rect(pnt, pnt) + rect(-1 + rand, -11, 1 + rand, -9) + rect(-rand2, -rand2, rand2, rand2)
            repeat with rep = 7 to 9
              rct = rotateToQuad(baseRect, random(360))
              rubbl = random(4)
              varLr = (layer - 1) * 10 + rep
              member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
              if (gdLayer <> "C") then
                member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
              end if                         
            end repeat
        end case
        if (rghtR = 1) then
          rand = random(10) - random(10)
          rand2 = random(2)
          baseRect = rect(pnt, pnt) + rect(9, -1 + rand, 11, 1 + rand) + rect(-rand2, -rand2, rand2, rand2)
          repeat with rep = 2 to 9
            rct = rotateToQuad(baseRect, random(360))
            rubbl = random(4)
            varLr = (layer - 1) * 10 + rep
            member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
            if (gdLayer <> "C") then
              member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
            end if                           
          end repeat
        end if
        if (lftR = 1) then
          rand = random(10) - random(10)
          rand2 = random(2)
          baseRect = rect(pnt, pnt) + rect(-11, -1 + rand, -9, 1 + rand) + rect(-rand2, -rand2, rand2, rand2)
          repeat with rep = 2 to 9
            rct = rotateToQuad(baseRect, random(360))
            rubbl = random(4) - 1
            varLr = (layer - 1) * 10 + rep
            member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
            if (gdLayer <> "C") then
              member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
            end if              
          end repeat
        end if
        if (bttmR = 1) then
          rand = random(10) - random(10)
          rand2 = random(2)
          baseRect = rect(pnt, pnt) + rect(-1 + rand, 9, 1 + rand, 11) + rect(-rand2, -rand2, rand2, rand2)
          repeat with rep = 2 to 9
            rct = rotateToQuad(baseRect, random(360))
            rubbl = random(4)
            varLr = (layer - 1) * 10 + rep
            member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
            if (gdLayer <> "C") then
              member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
            end if                          
          end repeat
        end if
        if (tpR = 1) then
          rand = random(10) - random(10)
          rand2 = random(2)
          baseRect = rect(pnt, pnt) + rect(-1 + rand, -11, 1 + rand, -9) + rect(-rand2, -rand2, rand2, rand2)
          repeat with rep = 2 to 9
            rct = rotateToQuad(baseRect, random(360))
            rubbl = random(4)
            varLr = (layer - 1) * 10 + rep
            member("layer" & string(varLr)).image.copyPixels(member("denseMoldGraf").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#color:colr, #ink:36})
            if (gdLayer <> "C") then
              member("gradient" & gdLayer & string(varLr)).image.copyPixels(member("denseMoldGrad").image, rct, rect(0 + 10 * rubbl, 1, 10 + 10 * rubbl, 11), {#ink:39, #blend:bldr})
            end if                         
          end repeat
        end if
      end repeat
    end if
  end repeat
end


on applyMiniGrowers me, q, c, eftc
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
    
    quadsToDraw = []
    
    --member("layer"&string(d)).image.copyPixels(member("flowerhead").image, rect(pnt.locH-3, pnt.locV-3, pnt.locH+3, mdPnt.locV+3), member("flowerhead").image.rect, {#color:colr, #ink:36})
    
    h = pnt.locV
    
    repeat while h < 30000 then
      h = h + 1
      pnt.locH = pnt.locH -2 + random(3)
      
      if skyRootsFix then
        quadsToDraw.add(rect(pnt.locH-1, h, pnt.locH, h+2))
      else
        member("layer"&string(d)).image.copyPixels(member("pxl").image, rect(pnt.locH-1, h, pnt.locH, h+2), member("pxl").image.rect, {#color:colr})
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
    end if
    
    copyPixelsToEffectColor(gdLayer, d, rect(headPos.locH-37, headPos.locV-37, headPos.locH+37, h+10), "hugeFlowerMaskMask", member("hugeFlowerMask").image.rect, 0.8)
  end if
end


on ApplySpinets me, q, c, eftc
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
        member("layer"&string(d)).image.copyPixels(member("spinetsGraf").image, qd, rect((var-1)*20, 1, var*20, 50+1), {#color:colr, #ink:36} )
        copyPixelsToRootEffectColor(gdLayer, d, qd, "spinetsGrad", rect((var-1)*20, 1, var*20, 50+1), 0.5, blnd)
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
        member("layer"&string(d)).image.copyPixels(member("spinetsGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36} )
        copyPixelsToRootEffectColor(gdLayer, d, qdd[1], "spinetsGrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
    -- copyPixelsToEffectColor(gdLayer, d, rect(headPos.locH-37, headPos.locV-37, headPos.locH+37, h+10), "hugeFlowerMaskMask", member("hugeFlowerMask").image.rect, 0.8)
    
  end if
end

on applyColoredHangRoots me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  frontWall = 0
  backWall = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30)-1
    "1":
      d = random(10)-1
      -- frontWall = 1 
    "2":
      d = random(10)-1 + 10
    "3":
      d = random(10)-1 + 20
    "1:st and 2:nd":
      d = random(20)-1
      -- frontWall = 1 
    "2:nd and 3:rd":
      d = random(20)-1 + 10
    otherwise:
      d = random(30)-1
      -- frontWall = 1 
  end case
  
  --  if(d > 5)then
  --    frontWall = 5+3
  --    d = restrict(d, frontWall, 29)
  --  else
  --    backWall = 5
  --  end if
  
  lr = 1+(d>9)+(d>19)
  quadsToDraw = []
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then--and(afaMvLvlEdit(point(q,c+1), 1)=1) then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    -- member("layer"&string(d)).image.copyPixels(member("flowerhead").image, rect(pnt.locH-3, pnt.locV-3, pnt.locH+3, mdPnt.locV+3), member("flowerhead").image.rect, {#color:colr, #ink:36})
    lftBorder = mdPnt.locH-10
    rgthBorder =  mdPnt.locH+10
    
    
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
        member("layer"&string(d)).image.copyPixels(member("pxl").image, qd, member("pxl").image.rect, {#color:colr, #ink:10})
        if (gdLayer <> "C") then
          member("gradient" & gdLayer & string(d)).image.copyPixels(member("pxlDR200").image, qd, member("pxlDR200").image.rect, {ink:39})
        end if
      end if
      
      --copyPixelsToRootEffectColor(gdLayer, d, qd, "pxl", member("pxl").image.rect, 0.5)
      
      if solidAfaMv(giveGridPos(lstPos) + gRenderCameraTilePos, lr) = 1 then
        exit repeat
      end if
      
      if skyRootsFix and withinBoundsOfLevel(giveGridPos(lstPos) + gRenderCameraTilePos) = 0 then
        exit
      end if
      
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer"&string(d)).image.copyPixels(member("pxl").image, qdd, member("pxl").image.rect, {#color:colr, #ink:10})
        if (gdLayer <> "C") then
          member("gradient" & gdLayer & string(d)).image.copyPixels(member("pxlDR200").image, qdd, member("pxlDR200").image.rect, {ink:39})
        end if
      end repeat
    end if
    
  end if
end


on applyColoredThickRoots me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  frontWall = 0
  backWall = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30)-1
    "1":
      d = random(10)-1
      -- frontWall = 1
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
      -- frontWall = 1
      backWall = 19
    "2:nd and 3:rd":
      d = random(20)-1 + 10
      frontWall = 10
    otherwise:
      d = random(30)-1
      -- frontWall = 1
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
        if (gridPos.locH > 0)and(gridPos.locH < gEEprops.effects[r].mtrx.count)and(gridPos.locV > 0)and(gridPos.locV < gEEprops.effects[r].mtrx[1].count) and (lstGridPos.locH > 0)and(lstGridPos.locH < gEEprops.effects[r].mtrx.count)and(lstGridPos.locV > 0)and(lstGridPos.locV < gEEprops.effects[r].mtrx[1].count)then
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
          member("layer"&string(d - c[1])).image.copyPixels(member("pxl").image, qd, member("pxl").image.rect, {#color:colr, ink:36})
          if (gdLayer <> "C") then
            member("gradient" & gdLayer & string(d - c[1])).image.copyPixels(member("pxlDR200").image, qd, member("pxlDR200").image.rect, {ink:39})
          end if
          --copyPixelsToRootEffectColor(gdLayer, d, qd, "pxl", member("pxl").image.rect, 0.5)
          -- headPos), health, thickness, tlt, lastRad, rad, perp, lastPerp, gridPos, lstPos, lstGridpos, dir, pnt, floatDpth, mdPnt, q2, q, c2, c, frontWall), backWall)
        end if
      end repeat
      
      lstPos = pnt
      lastPerp = perp
      lastRad = rad
    end repeat
    
    
    
  end if
end



on applyColoredShadowPlants me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  frontWall = 0
  backWall = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30)-1
    "1":
      d = random(9)-1
      -- frontWall = 1
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
      -- frontWall = 1
      backWall = 19
    "2:nd and 3:rd":
      d = random(20)-1 + 10
      frontWall = 10
    otherwise:
      d = random(30)-1
      -- frontWall = 1
  end case
  
  if(d > 5)then
    frontWall = 5+3
    d = restrict(d, frontWall, 29)
  else
    backWall = 5
  end if
  
  if (gLEprops.matrix[q2][c2][(1+(d>9)+(d>19))][1]=0)then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    health = 6
    points = [[pnt, d, health]]
    
    dir = 180
    
    cycle = lerp(6.0, 12.0, random(10000)/10000.0)
    cntr = random(50)
    
    tltFac = 0.0
    
    repeat while pnt.locV+gRenderCameraTilePos.locV*20 > -100 then
      cntr = cntr + 1
      
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
          member("layer"&string(d - c[1])).image.copyPixels(member("pxl").image, qd, member("pxl").image.rect, {#color:colr, ink:36})
          if (gdLayer <> "C") then
            member("gradient" & gdLayer & string(d - c[1])).image.copyPixels(member("pxlDR200").image, qd, member("pxlDR200").image.rect, {ink:39})
          end if
          --copyPixelsToRootEffectColor(gdLayer, d, qd, "pxl", member("pxl").image.rect, 0.5)
          
          if(random(30) = 1)then
            me.coloredSporeGrower(pnt + MoveToPoint(pnt, lstPos, diag(pnt, lstPos)*random(10000)/10000.0), 15 + random(50) * (1.0-f), d - c[1], colr)
          end if
          
          if(q < fuzzLength) and(random(fuzzLength) > q)and(random(6)=1) then
            f2 = q.float / fuzzLength.float
            me.coloredSporeGrower(pnt + MoveToPoint(pnt, lstPos, diag(pnt, lstPos)*random(10000)/10000.0), 65 + random(50) * (1.0-f2), d - c[1], colr)
          end if
        end if
      end repeat
      
      lstPos = pnt
      lastPerp = perp
      lastRad = rad
    end repeat
  end if
end


on coloredSporeGrower me, pos, lngth, layer, col
  dir = point(0, -1)
  
  repeat with q = 1 to lngth then
    otherCol = member("layer"&layer).image.getPixel(pos.locH-1, pos.locV-1)
    if(otherCol <> col)and(otherCol <> color(255, 255, 255))then
      exit repeat
    else
      member("layer"&layer).image.setPixel(pos.locH-1, pos.locV-1, col)
      if (gdLayer <> "C") then
        member("gradient"&gdLayer&layer).image.setPixel(pos.locH-1, pos.locV-1, color(50, 50, 50))
      end if
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


on applyRootPlants me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  frontWall = 0
  backWall = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      d = random(30)-1
    "1":
      d = random(9)-1
      -- frontWall = 1
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
      -- frontWall = 1
      backWall = 19
    "2:nd and 3:rd":
      d = random(20)-1 + 10
      frontWall = 10
    otherwise:
      d = random(30)-1
      -- frontWall = 1
  end case
  
  if(d > 5)then
    frontWall = 5+3
    d = restrict(d, frontWall, 29)
  else
    backWall = 5
  end if
  
  if (gLEprops.matrix[q2][c2][(1+(d>9)+(d>19))][1]=0)then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    health = 6
    points = [[pnt, d, health]]
    
    dir = 180
    
    cycle = lerp(6.0, 12.0, random(10000)/10000.0)
    cntr = random(50)
    
    tltFac = 0.0
    
    repeat while pnt.locV+gRenderCameraTilePos.locV*20 > -100 then
      cntr = cntr + 1
      
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
          member("layer"&string(d - c[1])).image.copyPixels(member("pxl").image, qd, member("pxl").image.rect, {#color:colr})
          if (gdLayer <> "C") then
            member("gradient" & gdLayer & string(d - c[1])).image.copyPixels(member("pxlDR200").image, qd, member("pxlDR200").image.rect, {ink:39})
          end if
          --copyPixelsToRootEffectColor(gdLayer, d, qd, "pxl", member("pxl").image.rect, 0.5)
        end if
      end repeat
      
      lstPos = pnt
      lastPerp = perp
      lastRad = rad
    end repeat
  end if
end




on applyWastewaterMold me, q, c, amount
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
            member("layer"&string(dp+d)).image.copyPixels(member("wastewaterMoldGraf").image, rect(startPos, startPos)+rect(-rad,-rad,rad,rad), rect(0, 1+d*20, 20, 1+(d+1)*20), {#color:colr, #ink:36})
            if gdLayer <> "C" then
              member("gradient"&gdLayer&string(dp+d)).image.copyPixels(member("wastewaterMoldGrad").image, rect(startPos, startPos)+rect(-rad,-rad,rad,rad), rect(0, 1+d*20, 20, 1+(d+1)*20), {#ink:39})
            end if
          else
            member("layer"&string(dp+d)).image.copyPixels(member("wastewaterMoldGraf").image, rect(startPos, startPos)+rect(-rad,-rad,rad,rad), rect(20, 1+d*40, 60, 1+(d+1)*40), {#color:colr, #ink:36})
            if gdLayer <> "C" then
              member("gradient"&gdLayer&string(dp+d)).image.copyPixels(member("wastewaterMoldGrad").image, rect(startPos, startPos)+rect(-rad,-rad,rad,rad), rect(20, 1+d*40, 60, 1+(d+1)*40), {#ink:39})
            end if 
          end if
        else
          exit repeat
        end if
      end repeat
    end if
  end repeat
end


on applyFlowers me, q, c, amount
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  mdPnt = giveMiddleOfTile(point(q,c))
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
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
    randRot1 = rect(startPos, startPos)+rect(-rad,-rad,rad,rad)
    randRot2 = rect(startPos, startPos)+rect(-rad,-rad,rad,rad)
    if (gRotOp) then
      randRot1 = rotateToQuad(randRot1, random(360))
      randRot2 = rotateToQuad(randRot2, random(360))
    end if  
    if(solid = 1)then
      repeat with d = 0 to 2 then
        if(dp+d <= dmax) and (dp+d >= dmin) then
          if(rad <= 10)then
            member("layer"&string(dp+d)).image.copyPixels(member("flowerGraf2").image, randRot1, rect(0, 1+d*20, 20, 1+(d+1)*20), {#color:colr, #ink:36})
            if gdLayer <> "C" then
              member("gradient"&gdLayer&string(dp+d)).image.copyPixels(member("flowerGrad2").image, randRot1, rect(0, 1+d*20, 20, 1+(d+1)*20), {#ink:39}) 
            end if
            member("layer"&string(dp+d)).image.copyPixels(member("flowerGraf1").image, randRot2, rect(0, 1+d*20, 20, 1+(d+1)*20), {#color:colrDetail, #ink:36})
            if gdDetailLayer <> "C" then
              member("gradient"&gdDetailLayer&string(dp+d)).image.copyPixels(member("flowerGrad1").image, randRot2, rect(0, 1+d*20, 20, 1+(d+1)*20), {#ink:39})
            end if
          else
            member("layer"&string(dp+d)).image.copyPixels(member("flowerGraf2").image, randRot1, rect(20, 1+d*40, 60, 1+(d+1)*40), {#color:colr, #ink:36})
            if gdLayer <> "C" then
              member("gradient"&gdLayer&string(dp+d)).image.copyPixels(member("flowerGrad2").image, randRot1, rect(20, 1+d*40, 60, 1+(d+1)*40), {#ink:39}) 
            end if
            member("layer"&string(dp+d)).image.copyPixels(member("flowerGraf1").image, randRot2, rect(20, 1+d*40, 60, 1+(d+1)*40), {#color:colrDetail, #ink:36})
            if gdDetailLayer <> "C" then
              member("gradient"&gdDetailLayer&string(dp+d)).image.copyPixels(member("flowerGrad1").image, randRot2, rect(20, 1+d*40, 60, 1+(d+1)*40), {#ink:39})
            end if
          end if
        else
          exit repeat
        end if
      end repeat
    end if
  end repeat
end



on applyColoredWires me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  global gCurrentRenderCamera
  
  case lrSup of
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
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    member("wireImage").image = image(member("layer"&string(d)).image.width, member("layer"&string(d)).image.height, 1)
    
    mdPnt = giveMiddleOfTile(point(q,c))
    startPos = mdPnt+point(-11+random(21), -11+random(21))
    
    myCamera = me.closestCamera(startPos+gRenderCameraTilePos*20)
    if(myCamera = 0)then
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
    
    a = 1.0+random(100)+random(random(random(900)))
    keepItFromToForty = random(30)
    a = ((a*keepItFromToForty)+40.0)/(keepItFromToForty+1.0)
    
    member("wireImage").image.copypixels(member("pxl").image, rect(startPos.locH, startPos.locV-1, startPos.locH+1, startPos.locV+1)+rect(-(fatness>1), -(fatness>1), (fatness=3), (fatness=3)), rect(0,0,1,1), {#color:color(0,0,0)})
    goodStops = 0
    
    repeat with dir = 0 to 1 then
      pnt = point(startPos.locH, startPos.locV)
      lastPnt = point(startPos.locH, startPos.locV)
      repeat with rep = 1 to 1000 then
        
        pnt.locH = startPos.locH +(-1 + 2*dir)*rep
        pnt.locV = startPos.locV + a - (power(2.71828183, rep/a)+power(2.71828183, -rep/a))*(a/2.0)
        
        dr = moveToPoint(lastPnt, pnt, fatness.float)
        
        member("wireImage").image.copypixels(member("pxl").image, rect(pnt.locH, pnt.locV, pnt.locH+1, lastPnt.locV+1)+rect(-(fatness>1), -(fatness>1), (fatness=3), (fatness=3)), rect(0,0,1,1), {#color:color(0,0,0)})
        
        lastPnt = point(pnt.locH, pnt.locV)
        
        
        
        
        
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
        
      end repeat
    end repeat
    
    if goodStops = 2 then
      member("layer"&string(d)).image.copyPixels(member("wireImage").image, member("wireImage").image.rect, member("wireImage").image.rect, {#color:colrInd, #ink:36})
      member("gradient"&gdIndLayer&string(d)).image.copyPixels(member("wireImage").image, member("wireImage").image.rect, member("wireImage").image.rect, {#ink:39})
    end if
  end if
end

on applyColoredChains me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  global gCurrentRenderCamera
  
  case lrSup of
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
      member("layer"&string(d)).image.copyPixels(member("wireImage").image, member("wireImage").image.rect, member("wireImage").image.rect, {#color:colrInd, #ink:36})
      member("gradient"&gdIndLayer&string(d)).image.copyPixels(member("wireImage").image, member("wireImage").image.rect, member("wireImage").image.rect, {#ink:39})
    end if
  end if
end

on applyRingChains me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  global gCurrentRenderCamera
  
  case lrSup of
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
        
        if diag(pnt, lastPnt)>=12 then
          if ornt then
            pos = (pnt+lastPnt)*0.5
            rct = rect(pos,pos)+rect(-7,-9,7,9)
            gtRect = rect(0,0,14,14)
            ornt = 0
          else
            pos = (pnt+lastPnt)*0.5
            rct = rect(pos,pos)+rect(-2,-9,2,9)
            gtRect = rect(15,0,18,14)
            ornt = 1
          end if
          member("wireImage").image.copypixels(member("ringChainSegment").image, rotateToQuad(rct, lookAtPoint(lastPnt,pnt)), gtRect, {#color:color(0,0,0), #ink:36})
          lastPnt = point(pnt.locH, pnt.locV)
          checkterrain = 1
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
      if (gdIndLayer <> "C") then
        member("layer"&string(d)).image.copyPixels(member("wireImage").image, member("wireImage").image.rect, member("wireImage").image.rect, {#color:colrInd, #ink:36})
        member("gradient"&gdIndLayer&string(d)).image.copyPixels(member("wireImage").image, member("wireImage").image.rect, member("wireImage").image.rect, {#ink:39})
      else
        member("layer"&string(d)).image.copyPixels(member("wireImage").image, member("wireImage").image.rect, member("wireImage").image.rect, {#color:color(255, 0, 0), #ink:36})
      end if
    end if
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


on applyColoredFungiFlower me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  case lrSup of
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
      member("layer"&string(lr)).image.copyPixels(member("fungiFlowersGraf2").image, rct, gtRect, {#color:colr, #ink:36})
      copyPixelsToRootEffectColor(gdLayer, lr, rct, "fungiFlowersGrad", gtRect, 0.5)
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



on applyColoredLHFlower me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  case lrSup of
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
    --writeMessage(q)
    --writeMessage(c)
    rct = rect(pnt, pnt) + rect(-40, -160, 40, 20)
    gtRect = rect((rnd-1)*80, 0, rnd*80, 180)+rect(1,0,1,0)
    if flp then
      rct = vertFlipRect(rct)
    end if
    member("layer"&string(lr)).image.copyPixels(member("lightHouseFlowersGraf2").image, rct, gtRect, {#color:colr, #ink:36})
    copyPixelsToRootEffectColor(gdLayer, lr, rct, "lightHouseFlowersGrad", gtRect, 0.5)
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

on applyAssortedTrash me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  
  case lrSup of
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
  repeat with varAttr = 0 to 2 then
    if (afaMvLvlEdit(point(q2,c2), layer)=0) then
      
      rnd = gEffectProps.list[gEffectProps.listPos]
      flp = random(2)-1 --unused but can't remove because it would change rng
      pnt = giveMiddleOfTile(point(q,c))+point(-10+random(20), 10)
      
      rct = rect(pnt, pnt) + rect(-25, -25, 25, 25)
      rct2 = rotateToQuad(rct, random(360))
      gtRect = rect((rnd-1)*50, 0, rnd*50, 50)+rect(1,0,1,0)
      if (gdIndLayer = "C") then
        member("layer"&string(lr)).image.copyPixels(member("assortedTrash").image, rct2, gtRect, {#color:[color(255,0,0), color(0,255,0), color(0,0,255)][random(3)], #ink:36})
      else
        member("layer"&string(lr)).image.copyPixels(member("assortedTrash").image, rct2, gtRect, {#color:colrInd, ink:36})
        member("gradient"&gdIndLayer&string(lr)).image.copyPixels(member("assortedTrash").image, rct2, gtRect, {#ink:39})
      end if
    end if
  end repeat
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48]
    l2 = []
    repeat with a = 1 to 48 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end


on applyResRoots me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  case lrSup of
    "All":
      lrmin = 1 
      lrmax = 3
    "1":
      lrmin = 1
      lrmax = 1
    "2":
      lrmin = 2
      lrmax = 2
    "3":
      lrmin = 3
      lrmax = 3
    "1:st and 2:nd":
      lrmin = 1
      lrmax = 2
    "2:nd and 3:rd":
      lrmin = 2
      lrmax = 3
    otherwise:
      lrmin = 1 
      lrmax = 3
  end case
  sourceRect = member("CloverRoot1").rect
  if (gdLayer = "C") then
    repeat with tr = lrmin to lrmax
      if (afaMvLvlEdit(point(q2,c2), tr)<>0) and (gEEprops.effects[r].mtrx[q2][c2] >= 50)then
        pnt = giveMiddleOfTile(point(q,c))
        rct = rect(pnt, pnt)+ rect(-10, -10, 10, 10)
        rct2 = rect(pnt, pnt)+ [rect(-15, -15, 5, 5), rect(-5, -5, 15, 15)][random(2)]
        rct3 = rect(pnt, pnt)+ rect(-5, -5, 15, 15)
        lay = member("layer"&string((tr-1)*10)).image
        lay.copyPixels(member("CloverRoot" & string(random(6))).image, rct, sourceRect, {#color:color(0,0,255), #ink:36})
        lay.copyPixels(member("CloverRoot" & string(random(6))).image, rct2, sourceRect, {#color:color(0,0,255), #ink:36})
        lay = member("layer"&string((tr-1)*10+5)).image
        lay.copyPixels(member("CloverRoot" & string(random(6))).image, rct, sourceRect, {#color:color(0,0,255), #ink:36})
        lay.copyPixels(member("CloverRoot" & string(random(6))).image, rct2, sourceRect, {#color:color(0,0,255), #ink:36})
      end if
    end repeat
  else
    repeat with tr = lrmin to lrmax
      if (afaMvLvlEdit(point(q2,c2), tr)<>0) and (gEEprops.effects[r].mtrx[q2][c2] >= 15)then
        pnt = giveMiddleOfTile(point(q,c))
        rct = rect(pnt, pnt)+ rect(-10, -10, 10, 10)
        rct2 = rect(pnt, pnt)+ [rect(-15, -15, 5, 5), rect(-5, -5, 15, 15)][random(2)]
        rct3 = rect(pnt, pnt)+ rect(-5, -5, 15, 15)
        var1 = "CloverRoot" & string(random(6))
        var2 = "CloverRoot" & string(random(6))
        var3 = "CloverRoot" & string(random(6))
        var4 = "CloverRoot" & string(random(6))
        strtr = string((tr-1)*10)
        lay = member("layer"&strtr).image
        grd = member("gradient"&gdLayer&strtr).image
        lay.copyPixels(member(var1).image, rct, sourceRect, {#color:colr, #ink:36})
        grd.copyPixels(member(var1 & "G").image, rct, sourceRect, {#ink:39})
        lay.copyPixels(member(var2).image, rct2, sourceRect, {#color:colr, #ink:36})
        grd.copyPixels(member(var2 & "G").image, rct2, sourceRect, {#ink:39})
        strtr = string((tr-1)*10 + 5)
        lay = member("layer"&strtr).image
        grd = member("gradient"&gdLayer&strtr).image
        lay.copyPixels(member(var3).image, rct, sourceRect, {#color:colr, #ink:36})
        grd.copyPixels(member(var3 & "G").image, rct, sourceRect, {#ink:39})
        lay.copyPixels(member(var4).image, rct2, sourceRect, {#color:colr, #ink:36})
        grd.copyPixels(member(var4 & "G").image, rct2, sourceRect, {#ink:39})
      end if
    end repeat
  end if
end

on applyFoliage me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  
  case lrSup of
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
    
    rct = rect(pnt, pnt) + rect(-80, -320, 80, 40)
    gtRect = rect((rnd-1)*160, 0, rnd*160, 360)+rect(1,0,1,0)
    if flp then
      rct = vertFlipRect(rct)
    end if
    member("layer"&string(lr)).image.copyPixels(member("foliageGraf3").image, rct, gtRect, {#color:colr, #ink:36})
    copyPixelsToRootEffectColor(gdLayer, lr, rct, "foliageGrad3", gtRect, 0.5)
  end if
  
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28]
    l2 = []
    repeat with a = 1 to 28 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end 

on applyMistletoe me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  
  case lrSup of
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
    
    rct = rect(pnt, pnt) + rect(-80, -320, 80, 40)
    gtRect = rect((rnd-1)*160, 0, rnd*160, 360)+rect(1,0,1,0)
    if flp then
      rct = vertFlipRect(rct)
    end if
    member("layer"&string(lr)).image.copyPixels(member("mistletoeGraf2").image, rct, gtRect, {#color:colr, #ink:36})
    copyPixelsToRootEffectColor(gdLayer, lr, rct, "mistletoeGrad2", gtRect, 0.5)
  end if
  
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [1,2,3,4,5,6]
    l2 = []
    repeat with a = 1 to 6 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end 

on applyHighFern me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  
  case lrSup of
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
    member("layer"&string(lr)).image.copyPixels(member("highFernGraf").image, rct, gtRect, {#color:colr, #ink:36})
    copyPixelsToRootEffectColor(gdLayer, lr, rct, "highFernGrad3", gtRect, 0.5)
  end if
  
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [1,2]
    l2 = []
    repeat with a = 1 to 2 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end 

on applyHighGrass me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  
  case lrSup of
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
    member("layer"&string(lr)).image.copyPixels(member("highGrassGraf").image, rct, gtRect, {#color:colr, #ink:36})
    copyPixelsToRootEffectColor(gdLayer, lr, rct, "highGrassGrad3", gtRect, 0.5)
  end if
  
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [1,2,3,4]
    l2 = []
    repeat with a = 1 to 4 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end 

on applySmallSprings me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  lr = 1
  case lrSup of
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
    member("layer"&string(lr)).image.copyPixels(member("smallSpringsGraf").image, rct, gtRect, {#color:colr, #ink:36})
    copyPixelsToRootEffectColor(gdLayer, lr, rct, "smallSpringsGrad", gtRect, 0.5)
  end if
  
  
  gEffectProps.listPos = gEffectProps.listPos + 1
  if gEffectProps.listPos > gEffectProps.list.count then
    l = [1,2,3,4,5,6,7]
    l2 = []
    repeat with a = 1 to 7 then
      val = l[random(l.count)]
      l2.add(val)
      l.deleteOne(val)
    end repeat
    gEffectProps = [#list:l2, #listPos:1]
  end if
end

