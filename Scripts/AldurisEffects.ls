global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, colrIntensity, fruitDensity, leafDensity, hasFlowers, effSide, fingerLen, fingerSz, gCustomEffects, gEffects, gLastImported

on ApplyMosaicPlant me, q, c
  global mosaicPlantStarts
  
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  -- Layers option is intentionally limited here
  lr = lrSup
  if (lrSup <> "1") and (lrSup <> "2") then
    if solidMtrx[q2][c2][2] then lr = "1"
    else lr = "2"
  end if
  
  if lr = "1" then
    dmin = 9
    dmax = 19
    lr = 1
    if solidMtrx[q2][c2][1] then
      return
    end if
  else
    dmin = 19
    dmax = 29
    lr = 2
    if solidMtrx[q2][c2][2] then
      return
    end if
  end if
  
  case colrIntensity of
    "H":
      maxblnd = 0.8
    "M":
      maxblnd = 0.5
    "L":
      maxblnd = 0.2
    "R":
      maxblnd = random(3) * 0.3 - 0.1
    "N":
      maxblnd = 0
    otherwise:
      maxblnd = 0.5
  end case
  
  -- Figure out if a plant spawns here
  ind = 0
  repeat with i = 1 to mosaicPlantStarts.count then
    if mosaicPlantStarts[i].locH.integer = q2 and mosaicPlantStarts[i].locV.integer = c2 then
      ind = i
      exit repeat
    end if
  end repeat
  
  -- Ok now draw it if it does
  if ind > 0 then
    startPt = mosaicPlantStarts[ind]
    
    if solidMtrx[startPt.locH.integer][startPt.locV.integer][lr+1] = 0 then
      return
    end if
    
    -- What is the closest other?
    clsPt = point(-100000, -100000)
    repeat with i = 1 to mosaicPlantStarts.count then
      if (startPt <> mosaicPlantStarts[i]) then
        if diag(startPt, clsPt) > diag(startPt, mosaicPlantStarts[i]) then clsPt = mosaicPlantStarts[i]
      end if
    end repeat
    
    -- Figure out leaf locations
    leaves = []
    maxIter = random(1500) + 500
    ofst = random(360.0)
    repeat with i = 1 to maxIter then
      ds = sqrt(i) * 0.3
      ang = i * PI * (3.0 - sqrt(5.0)) -- golden angle
      pt = startPt + point(cos(ang + ofst) * ds, sin(ang + ofst) * ds)
      
      if (pt.locH < 1) or (pt.locH > gLOprops.size.locH) or (pt.locV < 1) or (pt.locV > gLOprops.size.locV) then next repeat
      
      str = gEEprops.effects[r].mtrx[pt.locH.integer][pt.locV.integer]
      if ((str < 1.0 and i > 10 + random(6)) or solidMtrx[pt.locH.integer][pt.locV.integer][lr]) then exit repeat
      if diag(startPt, pt) > diag(clsPt, pt) then
        if random(4) = 1 then exit repeat
        else next repeat
      end if
      
      leaves.append(pt)
    end repeat
    
    -- Filter out further out leaves
    oldL = leaves.count
    repeat with i = oldL down to 1 then
      if random(oldL) < i - 8 then
        leaves.deleteAt(i)
      end if
    end repeat
    
    -- FINALLY we get to draw the leaves :3
    lr = dmin
    grafSz = point(3, 6.5) -- mult by 2 to get actual size
    
    repeat with i = leaves.count down to 1 then -- reverse order for drawing reasons (outwards-in)
      leafPt = leaves[i]
      -- stem
      tl = (startPt + leafPt) / 2.0 - gRenderCameraTilePos
      tl = tl * 20.0 + point(10.0, 10.0)
      sz = point(1, (diag(startPt, leafPt) * 20.0).integer) / 2.0
      qd = rotateToQuadFix(rect(tl, tl) + rect(-sz, sz), lookAtPoint(leafPt, startPt))
      member("layer"&string(lr)).image.copypixels(member("pxl").image, qd, rect(0,0,1,1), {#color:color(255,0,0), #ink:36})
      -- leaf
      tl = (leafPt - gRenderCameraTilePos) * 20.0 + point(10.0, 10.0)
      qd = rotateToQuadFix(rect(tl, tl) + rect(-grafSz, grafSz), lookAtPoint(leafPt, startPt))
      member("layer"&string(lr-1)).image.copypixels(member("mosaicLeafGraf").image, qd, rect(0,0,6,11), {#color:colr, #ink:36})
      if colrIntensity <> "N" then
        copyPixelsToEffectColor(gdLayer, lr-1, qd, "mosaicLeafGraf", rect(6, 0, 12, 11), 0.5, maxblnd * (1 - (i.float / leaves.count.float) * 0.5))
      end if
    end repeat
    
    -- Guess what: flowers
    repeat with i = 1 to (leaves.count / 50.0).integer + 1 then
      if hasFlowers and random(3) = 1 then
        ds = random(diag(startPt, leaves[leaves.count])) * 0.45
        ang = random(360) / PI
        tl = startPt + point(cos(ang) * ds, sin(ang) * ds) - gRenderCameraTilePos
        tl = (tl * 20.0) + point(10.0, 10.0)
        
        ang = random(360/PI)
        amt = random(3) + 3
        repeat with j = 1 to amt then
          tl2 = tl + (point(cos(ang), sin(ang)) * 6.5)
          qd = rotateToQuadFix(rect(tl2, tl2) + rect(-grafSz, grafSz), lookAtPoint(tl2, tl))
          member("layer"&string(lr-2)).image.copypixels(member("mosaicLeafGraf").image, qd, rect(0,0,6,11), {#color:colrDetail, #ink:36})
          copyPixelsToEffectColor(gdDetailLayer, lr-2, qd, "mosaicLeafGraf", rect(6, 0, 12, 11), 0.5, (random(10) + 90.0) / 100.0)
          ang = ang + (2 * PI / amt)
        end repeat
      end if
    end repeat
  end if
end

on InitMosaicPlants me
  global mosaicPlantStarts
  mtrx = gEEprops.effects[r].mtrx
  
  savSeed = the randomSeed
  the randomSeed = effectSeed
  
  -- Figure out where the local maximum values are
  peakVals = []
  counters = []
  repeat with i = 1 to gLOprops.size.locH then
    repeat with j = 1 to gLOprops.size.locV then
      
      if mtrx[i][j] > 0 then
        test = 1
        if i > 1 then
          if mtrx[i - 1][j] > mtrx[i][j] then test = 0
        end if
        if i < gLOprops.size.locH then
          if mtrx[i + 1][j] > mtrx[i][j] then test = 0
        end if
        if j > 1 then
          if mtrx[i][j - 1] > mtrx[i][j] then test = 0
        end if
        if j < gLOprops.size.locV then
          if mtrx[i][j + 1] > mtrx[i][j] then test = 0
        end if
        
        if (test = 1) then
          peakVals.append(point(i.float, j.float))
          counters.append(1)
        end if
        
      end if
      
    end repeat
  end repeat
  
  -- Fiddle with the points (remove duplicates and randomize)
  repeat with k = 1 to 3 then
    
    -- Remove duplicates
    repeat with i = 1 to peakVals.count - 1 then
      actv = peakVals[i]
      repeat with j = i + 1 to peakVals.count then
        curr = peakVals[j]
        
        if diag(actv, curr) < 5 then
          actv = (actv * counters[i] + curr * counters[j]) / (counters[i] + counters[j])
          peakVals[i] = actv
          peakVals.deleteAt(j)
          counters[i] = counters[i] + 1
          counters.deleteAt(j)
          j = j - 1 -- the next one to look at will be in the same place
        end if
        
      end repeat
    end repeat
    
    -- Randomize slightly
    repeat with i = 1 to peakVals.count then
      peakVals[i] = peakVals[i] + point(random(3.0) - 2.0, random(3.0) - 2.0)
      peakVals[i] = point(max(1, min(gLOprops.size.locH, peakVals[i].locH)), max(1, min(gLOprops.size.locV, peakVals[i].locV)))
    end repeat
    
  end repeat
  
  -- One more passthrough to randomly remove a few low ones
  repeat with i = peakVals.count down to 1 then
    temp = mtrx[peakVals[i].locH.integer][peakVals[i].locV.integer]
    if power(random(100) / 100.0, 3) * 100.0 > temp then
      peakVals.deleteAt(i)
      counters.deleteAt(i) -- technically I don't need this but it's useful when debugging
    end if
  end repeat
  
  mosaicPlantStarts = peakVals
  
  -- Reset random
  the randomSeed = savSeed
end

on ApplyCobweb me, q, c
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
  
  if solidMtrx[q2][c2][lr] = 1 then
    return
  end if
  
  startPt = giveMiddleOfTile(point(q, c))+point(-11+random(21), -11+random(21)) + gRenderCameraTilePos * 20.0
  
  -- Find branching points
  rot = random(360)
  angs = []
  repeat with i = 1 to 60 then
    pt = startPt
    repeat with j = 1 to 20 * 5 / 2 then -- 20 * max tile radius, moving two pixels at a time
      pt = pt + point(cos(rot * PI / 180) * 2, sin(rot * PI / 180) * 2)
      ipt = point(((pt.locH + 10) / 20).integer, ((pt.locV + 10) / 20).integer)
      if ipt.locH < 1 or ipt.locH > gLOprops.size.locH or ipt.locV < 1 or ipt.locV > gLOprops.size.locV then exit repeat
      if solidMtrx[ipt.locH][ipt.locV][lr] <> 0 then
        angs.append([rot, pt])
        exit repeat
      end if
    end repeat
    
    rot = (rot + 6) mod 360
  end repeat
  
  if angs.count < 3 then -- we preferably want at least 3 points
    return
  end if
  
  -- Pick our angles
  picked = [angs[random(angs.count)]]
  maxpts = random(min(8, angs.count - 2)) + 2
  repeat with i = 2 to maxpts then
    -- Weed out close angles (they will look bad)
    repeat with j = angs.count down to 1 then
      dif = abs(picked[picked.count][1] - angs[j][1])
      if dif > 180 then dif = 360 - dif
      if dif < 15 then angs.deleteAt(j)
    end repeat
    
    -- Pick a random remaining angle
    if angs.count < 1 and i <= 3 then exit
    if angs.count < 1 then exit repeat
    picked.append(angs[random(angs.count)])
  end repeat
  
  picked.sort()
  
  -- Figure out colors
  case gdIndLayer of
    "A":
      webcl = color(150,0,150)
    "B":
      webcl = color(0,150,150)
    otherwise:
      webcl = color(255,0,0)
  end case
  case colrIntensity of
    "H":
      webin = 0.8
    "M":
      webin = 0.5
    "L":
      webin = 0.2
    "N":
      webin = 0
    otherwise:
      webin = 0.5
  end case
  
  -- Draw the base lines connecting to edges
  mn = 9999
  repeat with i = 1 to picked.count then
    len = diag(startPt, picked[i][2])
    mn = min(len, mn)
    qd = (startPt + picked[i][2]) / 2.0 - gRenderCameraTilePos * 20.0
    qd = rect(qd, qd) + rect(-0.5, -len/2.0, 0.5, len/2.0)
    qd = rotateToQuad(qd, lookAtpoint(startPt, picked[i][2]))
    member("layer"&string(d)).image.copypixels(member("pxl").image, qd, rect(0,0,1,1), {#color:webcl, #ink:36})
    
    if (gdIndLayer = "A" or gdIndLayer = "B") and (colrIntensity <> "N") then
      copyPixelsToEffectColor(gdIndLayer, d, qd, "pxl", rect(0, 0, 1, 1), 0.5, webin)
    end if
  end repeat
  
  -- Draw inner spiral lines
  pxlSpc = random(4) + 4.0 -- range: 5-8 (inclusive)
  repeat with i = 1 to mn / pxlSpc then
    repeat with j = 1 to picked.count then
      -- Random chance to skip
      if random(10) = 1 then next repeat
      
      -- Math to figure out what points we're at
      cur = picked[j]
      nxt = picked[1]
      if j <> picked.count then nxt = picked[j+1]
      
      dif = nxt[1] - cur[1]
      if nxt[1] < cur[1] then dif = 360 - cur[1] + nxt[1]
      if dif >= 150 then next repeat
      
      clrp = (i + (j.float / picked.count)) * pxlSpc / diag(startPt, cur[2])
      nlrp = (i + ((j.float + 1.0) / picked.count)) * pxlSpc / diag(startPt, nxt[2])
      
      cur = point(lerp(startPt.locH, cur[2].locH, clrp), lerp(startPt.locV, cur[2].locV, clrp)) - gRenderCameraTilePos * 20.0
      nxt = point(lerp(startPt.locH, nxt[2].locH, nlrp), lerp(startPt.locV, nxt[2].locV, nlrp)) - gRenderCameraTilePos * 20.0
      ang = lookAtpoint(cur, nxt)
      
      -- Draw the line
      len = diag(cur, nxt)
      qd = (cur + nxt) / 2.0
      qd = rect(qd, qd) + rect(-0.5, -len/2.0, 0.5, len/2.0)
      qd = rotateToQuad(qd, ang)
      member("layer"&string(d)).image.copypixels(member("pxl").image, qd, rect(0,0,1,1), {#color:webcl, #ink:36})
      
      if (gdIndLayer = "A" or gdIndLayer = "B") and (colrIntensity <> "N") then
        copyPixelsToEffectColor(gdIndLayer, d, qd, "pxl", rect(0, 0, 1, 1), 0.5, webin)
      end if
    end repeat
  end repeat
end

on ApplyFingers me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
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
  
  case fingerSz of
    "S":
      amount = 12
    "M":
      amount = 10
    "L":
      amount = 7
    otherwise:
      amount = random(2) + 7
  end case
  
  repeat with layer in lsL then
    dDn = (solidMtrx[q2][c2][layer]<>1) and (solidAfaMv(point(q2,c2+1), layer)=1)
    dLf = (solidMtrx[q2][c2][layer]<>1) and (solidAfaMv(point(q2-1,c2), layer)=1)
    dRg = (solidMtrx[q2][c2][layer]<>1) and (solidAfaMv(point(q2+1,c2), layer)=1)
    
    if (dDn <> 1 and dLf <> 1 and dRg <> 1) then next repeat
    
    mdPnt = giveMiddleOfTile(point(q,c))
    repeat with cntr = 1 to gEEprops.effects[r].mtrx[q2][c2]*0.01*amount then
      -- Position and angle
      potential = []
      if dDn then
        ang = random(11) - 6
        if dLf and dRg then
          ang = random(81) - 41
        else if dLf then
          ang = random(40)
        else if dRg then
          ang = random(40) - 41
        end if
        potential.append([mdPnt + point(random(21) - 11, 10), ang])
      end if
      if dLf then
        potential.append([mdPnt + point(-10, random(21) - 11), random(30) + 20])
      end if
      if dRg then
        potential.append([mdPnt + point(10, random(21) - 11), random(30) - 51])
      end if
      picked = potential[random(potential.count)]
      pnt = picked[1]
      ang = picked[2]
      
      -- Sublayer, girth, and length
      lr = random(9) + (layer-1)*10
      case fingerSz of
        "S":
          sz = random(2) + 2
        "M":
          sz = random(3) + 3
        "L":
          sz = random(4) + 5
        "?":
          sz = random(7) + 2
      end case
      case fingerLen of
        "S":
          len = random(4) + 2
        "M":
          len = random(4) + 4
        "L":
          len = random(6) + 8
        "?":
          len = random(12) + 2
      end case
      if sz < 5 then
        len = ((len - 2) * 0.65).integer + 2
      end if
      len = ((len / 2) + (len/2 * gEEprops.effects[r].mtrx[q2][c2]*0.01)).integer
      
      -- Draw the finger
      wdth = sz
      hght = sz * 1.5
      tipGrad = (random(20) + 40) / 100.0
      lastRing = FALSE
      repeat with seg = 1 to len then
        qd = rotateToQuadFix(rect(point(-wdth/2, -hght/2), point(wdth/2, hght/2)) + rect(pnt, pnt), ang)
        
        thisCol = colr
        if (seg > 1) and (seg < len) and (not lastRing) and (random(3) = 1) then
          if (random(2) = 1) then
            thisCol = color(0,0,255)
          else
            thiscol = color(255,0,0)
          end if
          lastRing = TRUE
        else if lastRing then
          lastRing = FALSE
        end if
        
        member("layer"&string(lr)).image.copyPixels(member("blob").image, qd, member("blob").image.rect, {#ink:36, #color:thisCol})
        if colr <> color(0,255,0) then
          segGrad = tipGrad * (0.333 + seg * 0.667 / len)
          copyPixelsToEffectColor(gdLayer, lr, rotateToQuad(rect(pnt, pnt) + rect(-wdth,-hght/1.5,wdth,hght/1.5), ang), "softBrush1", member("softBrush1").rect, 0.5, segGrad)
        end if
        
        pnt = pnt + degToVec(ang) * 3
      end repeat
    end repeat
  end repeat
end






