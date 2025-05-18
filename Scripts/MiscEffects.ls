global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, chOp, fatOp, gradAf, effectIn3D, gAnyDecals, gRotOp, slimeFxt, DRDarkSlimeFix, DRWhite, DRPxl, DRPxlRect, colrIntensity, fruitDensity, leafDensity, mshrSzW, mshrSz, hasFlowers, skyRootsFix


--leo
on applyIvy me, q, c, eftc
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
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    lftBorder = mdPnt.locH-10
    rgthBorder =  mdPnt.locH+10
    ivygrad = member("IvyLeafGradMed").image
    fruitpercent = 50
    case fruitDensity of
      "H":
        fruitpercent = 75
      "M":
        fruitpercent = 50
      "L":
        fruitpercent = 25
      "N":
        fruitpercent = 0
    end case
    case colrIntensity of
      "H":
        ivygrad = member("IvyLeafGraf").image
      "M":
        ivygrad = member("IvyLeafGradMed").image
      "L":
        ivygrad = member("IvyLeafGradLow").image
      "N":
        ivygrad = member("pxl").image
      "R":
        if(colrIntensity="R")then
          ivyrngrad = Random(4)
          case ivyrngrad of
            1:
              ivygrad = member("IvyLeafGraf").image
            2:
              ivygrad = member("IvyLeafGradMed").image
            3:
              ivygrad = member("IvyLeafGradLow").image
            4:
              ivygrad = member("pxl").image
          end case
        end if
    end case
    ivyrandom = Random(100)
    quadsToDraw = []
    repeat while pnt.locV+gRenderCameraTilePos.locV*20 > -100 then
      ivyrandom = Random(100)
      fruitrandom = Random(100)
      lstPos = pnt
      pnt = pnt + degToVec(-45+random(90))*(2+random(6))
      pnt.locH = restrict(pnt.locH, lftBorder, rgthBorder)
      dir = moveToPoint(pnt, lstPos, 1.0)
      crossDir = giveDirFor90degrToLine(-dir, dir)
      qd = [pnt-crossDir, pnt+crossDir, lstPos+crossDir, lstPos-crossDir]
      test = [pnt-crossDir-2, pnt+crossDir, lstPos+crossDir-2, lstPos-crossDir]
      fruitytest = [pnt-crossDir-1, pnt+crossDir, lstPos+crossDir-1, lstPos-crossDir]
      fuck = test + member("IvyLeafGraf").image.rect
      fruity = fruitytest + member("IvyFruit").image.rect
      
      if skyRootsFix then
        quadsToDraw.add([qd, fruitrandom<fruitpercent, ivyrandom<leafDensity, fruity, fuck, ivygrad])
      else
        member("layer"&string(d)).image.copyPixels(member("pxl").image, qd, member("pxl").image.rect, {#color:colr, #ink:10})
        if(fruitrandom<fruitpercent)then
          member("layer"&string(d)).image.copyPixels(member("IvyFruit").image, fruity, member("IvyFruit").image.rect, {#color:color(255, 0, 255), #ink:10})
          member("gradientA"&string(d)).image.copyPixels(member("IvyFruit").image, fruity, member("IvyFruit").image.rect, {ink:39}) 
        end if
        if(ivyrandom<leafDensity)then
          member("layer"&string(d)).image.copyPixels(member("IvyLeafGraf").image, fuck, member("IvyLeafGraf").image.rect, {#color:colr, #ink:10})
          if (colrIntensity <> "N" and gdLayer <> "C") then
            member("gradient"&gdLayer&string(d)).image.copyPixels(ivygrad, fuck, ivygrad.rect, {ink:39}) 
          end if
        end if
      end if
      
      if(colrIntensity="R")then
        ivyrngrad = Random(4)
        case ivyrngrad of
          1:
            ivygrad = member("IvyLeafGraf").image
          2:
            ivygrad = member("IvyLeafGradMed").image
          3:
            ivygrad = member("IvyLeafGradLow").image
          4:
            ivygrad = member("pxl").image
        end case
      end if
      
      if skyRootsFix and withinBoundsOfLevel(giveGridPos(lstPos) + gRenderCameraTilePos) = 0 then
        exit
      end if
      
      if solidAfaMv(giveGridPos(lstPos) + gRenderCameraTilePos, lr) = 1 then
        exit repeat
      end if
    end repeat
    
    if skyRootsFix then
      repeat with qdd in quadsToDraw
        member("layer"&string(d)).image.copyPixels(member("pxl").image, qdd[1], member("pxl").image.rect, {#color:colr, #ink:10})
        if(qdd[2])then
          member("layer"&string(d)).image.copyPixels(member("IvyFruit").image, qdd[4], member("IvyFruit").image.rect, {#color:color(255, 0, 255), #ink:10})
          member("gradientA"&string(d)).image.copyPixels(member("IvyFruit").image, qdd[4], member("IvyFruit").image.rect, {ink:39}) 
        end if
        if(qdd[3])then
          member("layer"&string(d)).image.copyPixels(member("IvyLeafGraf").image, qdd[5], member("IvyLeafGraf").image.rect, {#color:colr, #ink:10})
          if (colrIntensity <> "N" and gdLayer <> "C") then
            member("gradient"&gdLayer&string(d)).image.copyPixels(qdd[6], qdd[5], qdd[6].rect, {ink:39}) 
          end if
        end if
      end repeat
    end if
    
  end if
end
-- end leo


-- tronsx
on ApplyThunderGrower me, q, c, eftc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  case lrSup of
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
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    mdPnt = giveMiddleOfTile(point(q,c))
    headPos = mdPnt+point(-11+random(21), -11+random(21))
    pnt = point(headPos.locH, headPos.locV)
    
    lastDir = 180 - 61 + random(121)
    blnd = 1
    blnd2 = 1
    
    wdth = 0.2
    
    searchBase = 50
    
    quadsToDraw = []
    
    repeat while pnt.locV < 30000 then
      dir = 180 - 170 + random(300)
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
        member("layer"&string(d)).image.copyPixels(member("thunderBushGraf").image, qd, rect((var-1)*20, 1, var*20, 50+1), {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qd, "thunderBushGrad", rect((var-1)*20, 1, var*20, 50+1), 0.5, blnd)
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
        member("layer"&string(d)).image.copyPixels(member("thunderBushGraf").image, qdd[1], qdd[2], {#color:colr, #ink:36} )
        copyPixelsToEffectColor(gdLayer, d, qdd[1], "thunderBushGrad", qdd[2], 0.5, qdd[3])
      end repeat
    end if
    
  end if
end
-- end tronsx


-- ludocrypt
on applyMushroomStubs me, q, c, amount
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  mdPnt = giveMiddleOfTile(point(q,c))
  
  case lrSup of
    "All":
      dmin = 1
      dmax = 29
    "1":
      dmin = 1
      dmax = 6
    "2":
      dmin = 10
      dmax = 16
    "3":
      dmin = 20
      dmax = 29
    "1:st and 2:nd":
      dmin = 1
      dmax = 16
    "2:nd and 3:rd":
      dmin = 10
      dmax = 29
    otherwise:
      dmin = 1
      dmax = 29
  end case
  
  fullamount = amount
  
  case mshrSz of
    "S":
      dij = 0
      dj = 0
    "M":
      dij = 1
      dj = 1
    "R":
      dij = 0
      dj = 1
      fullamount = amount/3
    otherwise:
      dij = 1
      dj = 1
  end case
  
  case mshrSzW of
    "S":
      wminscale = 0.2
      wmaxscale = 0.5
    "M":
      wminscale = 0.5
      wmaxscale = 1.0
    "L":
      wminscale = 1.0
      wmaxscale = 1.3
    "R":
      wminscale = 0.5
      wmaxscale = 1.0
    otherwise:
      wminscale = 0.5
      wmaxscale = 1.0
  end case
  
  repeat with di = dij to dj then
    case di of
      0:
        mminsize = 10
        mmaxsize = 20
      1:
        mminsize = 20
        mmaxsize = 40
      otherwise:
        mminsize = 20
        mmaxsize = 40
    end case
    
    repeat with a = 1 to fullamount/(mmaxsize/10.0) then
      dp = random(28)-1
      if(dp > 3)then
        dp = dp + 2
      end if
      
      lr = 3
      
      rad = lerp(mminsize, mmaxsize, random(90)/120.0)
      
      radw = lerp(wminscale, wmaxscale, random(100)/100.0)
      
      if(dp < 10)then
        lr = 1
      else if (dp < 20) then
        lr = 2
      end if
      
      startPos = mdPnt+(point(-11+random(21), -11+random(21)))
      
      solid = 0
      
      if(solidAfaMv(point(q2,c2), lr) = 1)then
        solid = 1
      end if
      
      if(dp-1 >= 0) then
        pixelColor = member("layer"&string(dp)).image.getPixel(startPos)
        if pixelColor <> rgb(255, 255, 255) then
          pixelColor = member("layer"&string(dp-1)).image.getPixel(startPos)
          if pixelColor = rgb(255, 255, 255) then
            solid = 0
          end if
        end if
      end if
      
      if(solid = 1)then
        repeat with d = 0 to 2 then
          if(dp+d <= dmax) and (dp+d >= dmin) then
            if(rad <= 10)then
              member("layer"&string(dp+d)).image.copyPixels(member("mushroomStubsGraf").image, rect(startPos, startPos)+(rect(-rad,-rad,rad,rad) * rect(radw, 1, radw, 1)), rect(0, 1+d*20, 20, 1+(d+1)*20), {#color:colr, #ink:36})
              if gdLayer <> "C" then
                member("gradient"&gdLayer&string(dp+d)).image.copyPixels(member("mushroomStubsGrad").image, rect(startPos, startPos)+(rect(-rad,-rad,rad,rad) * rect(radw, 1, radw, 1)), rect(0, 1+d*20, 20, 1+(d+1)*20), {#ink:39})
              end if
            else
              member("layer"&string(dp+d)).image.copyPixels(member("mushroomStubsGraf").image, rect(startPos, startPos)+(rect(-rad,-rad,rad,rad) * rect(radw, 1, radw, 1)), rect(20, 1+d*40, 60, 1+(d+1)*40), {#color:colr, #ink:36})
              if gdLayer <> "C" then
                member("gradient"&gdLayer&string(dp+d)).image.copyPixels(member("mushroomStubsGrad").image, rect(startPos, startPos)+(rect(-rad,-rad,rad,rad) * rect(radw, 1, radw, 1)), rect(20, 1+d*40, 60, 1+(d+1)*40), {#ink:39})
              end if 
            end if
          else
            exit repeat
          end if
        end repeat
      end if
    end repeat
  end repeat
  
end
-- end ludocrypt