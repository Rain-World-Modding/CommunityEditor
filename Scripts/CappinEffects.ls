global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, gradAf, colrIntensity
global blobSize, growOnWalls, needsAttach

-- effectStrength is always 0
on applyFezTree me, screenX, screenY, effectStrength
  treeLayer = 0
  
  case lrSup of --["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      treeLayer = randomRange(8, 10)
    "1":
      treeLayer = randomRange(8, 10)
    "2":
      treeLayer = randomRange(13, 20)
    "3":
      treeLayer = randomRange(23, 29)
    "1:st and 2:nd":
      treeLayer = randomRange(8, 10)
    "2:nd and 3:rd":
      treeLayer = randomRange(13, 20)
    otherwise:
      treeLayer = randomRange(8, 13)
  end case
  
  -- offset down so that it's attached to the base of the tile.
  treeBasePos = giveMiddleOfTile(point(screenX, screenY)) + point(0, 11)
  
  -- leaves pos is from 2 to 10 tiles above the base position, and offset randomly horizontally by like 2 tiles
  treeLeavesPos = point(treeBasePos.locH, treeBasePos.locV) + point(randomRange(-40, 40), -randomRange(100, 200))
  treeLeavesAngle = randomRange(-40, 40)
  leavesRadiusX = randomRange(20, 50)
  leavesRadiusY = leavesRadiusX + randomRange(-10, 10)
  treeLeavesSize = point(leavesRadiusX, leavesRadiusY)
  
  --on drawFezTreeAtPosition treeBasePos, treeBaseAngle, treeLeavesPos, treeLeavesSize, treeLeavesAngle, treeLayer, effectLayer, leafDensity
  drawFezTreeAtPosition(treeBasePos, 1, treeLeavesPos, treeLeavesSize, treeLeavesAngle, treeLayer, gdLayer, 1.0)
end

on applyBrainGrowers me, screenX, screenY, amount, upsideDown
  tileX = screenX + gRenderCameraTilePos.locH
  tileY = screenY + gRenderCameraTilePos.locV
  
  midPoint = giveMiddleOfTile(point(screenX, screenY))
  tilePoint = midPoint + gRenderCameraTilePos
  
  layerMin = 0
  layerMax = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      layerMin = 0
      layerMax = 29
    "1":
      layerMin = 0
      layerMax = 6
    "2":
      layerMin = 10
      layerMax = 16
    "3":
      layerMin = 20
      layerMax = 29
    "1:st and 2:nd":
      layerMin = 0
      layerMax = 16
    "2:nd and 3:rd":
      layerMin = 10
      layerMax = 29
    otherwise:
      layerMin = 0
      layerMax = 29
  end case
  
  maxAmount = 3
  countInTile = 0
  repeat with i = 0 to maxAmount do 
    randomNum = randomRange(0, 150)
    if randomNum < amount then
      countInTile = countInTile + 1
    end if
  end repeat
  
  if countInTile <= 1 then
    return
  end if
  
  countInTile = countInTile * max(1, (layerMin - layerMax) / 10.0)
  
  repeat with plant = 0 to countInTile do
    plantPos = midPoint + point(randomRange(-10, 10), 10)
    if (upsideDown) then
      plantPos = midPoint + point(randomRange(-10, 10), -10)
    end if
    plantLayer = randomRange(layerMin, layerMax)
    plantGeoLayer = 0
    
    if(plantLayer < 10)then
      plantGeoLayer = 1
    else if (plantLayer < 20) then
      plantGeoLayer = 2
    else
      plantGeoLayer = 3
    end if
    
    shouldPlace = solidAfaMv(giveGridPos(midPoint) + gRenderCameraTilePos, plantGeoLayer) <> 1
    if upsideDown then
      shouldPlace = shouldPlace and (solidAfaMv(giveGridPos(midPoint + point(0, -20)) + gRenderCameraTilePos, plantGeoLayer) = 1)
    else
      shouldPlace = shouldPlace and (solidAfaMv(giveGridPos(midPoint + point(0, 20)) + gRenderCameraTilePos, plantGeoLayer) = 1)
    end if
    
    if shouldPlace then
      plantSize = (randomRange(0, amount) + randomRange(0, amount)) / 200.0
      plantSize = plantSize * plantSize
      plantRadius = lerp(2, 4, plantSize)
      
      segmentPos = plantPos
      segmentCount = lerp(2, 10, plantSize)
      segments = []
      
      offsetRand = (randomRange(0, 10000)/ 10000.0) * 3.141592 * 2
      
      repeat with segment = 0 to segmentCount do
        segments.add(segmentPos)
        sine = sin(segment * 10.0 + offsetRand)
        if upsideDown then
          segmentPos = segmentPos - (degToVec(sine * 50 + lerp(-5.0, 5.0, randomRange(0, 10000) / 10000.0)) * randomRange(5, 60))
        else
          segmentPos = segmentPos + (degToVec(sine * 50 + lerp(-5.0, 5.0, randomRange(0, 10000) / 10000.0)) * randomRange(5, 60))
        end if
      end repeat
      
      
      segmentDeriv = [point(0, -1)]
      repeat with segmentIndex = 2 to segments.count - 1
        dir = segments[segmentIndex + 1] - segments[segmentIndex - 1]
        --dir = dir / mag(dir) -- normalize
        segmentDeriv.add(dir * 0.2)
      end repeat
      segmentDeriv[segments.count] = segmentDeriv[segments.count - 1]
      
      stemEffectFactor = 0.4
      blossomEffectFactor = 0.9
      
      curvePoints = []
      segmentRadius = plantRadius * 2.0
      startEffectFactor = 0.0
      repeat with seg = 2 to segments.count do
        factor = 1.0 - (seg / segments.count)
        endRadius = lerp(plantRadius, plantRadius * 0.7, 1.0 - (factor * factor)) + lerp(-0.5, 0.5, (randomRange(0, 10000) / 10000.0))
        endEffectFactor = lerp(stemEffectFactor, 0.0, factor)
        deriv = point(0, -1)
        if upsideDown then
          deriv = point(0, 1)
        end if
        
        drawWeirdLittleWiggle(0.5, segments[seg - 1], plantLayer, segmentRadius, deriv * mag(segmentDeriv[seg - 1]) * 0.5, sqrt(startEffectFactor), segments[seg], plantLayer, endRadius, deriv * mag(segmentDeriv[seg]) * 0.5, sqrt(endEffectFactor), colr, curvePoints)
        segmentRadius = endRadius
        startEffectFactor = endEffectFactor
      end repeat
      
      -- "leaves" of the grower, drawn at the head.
      leavesPos = segments[segments.count]
      leavesPos2 = leavesPos + (segmentDeriv[segments.count] / mag(segmentDeriv[segments.count])) * lerp(20, 50, plantSize)
      
      
      
      dir = leavesPos2 - leavesPos
      dir = dir / mag(dir)
      leafDeg = vecToDeg(dir)
      leafCount = randomRange(2, 6)
      if upsideDown then
        leafDeg = leafDeg + 180
      end if
      
      if randomRange(0, 3) <> 1 then
        repeat with leafBlob = -2 to 8 do
          factor = (leafBlob / 9.0)
          blobPos = lerpVector(leavesPos, leavesPos2, factor)
          factor2 = 4 * (factor - factor * factor)
          blobSize = lerp(plantRadius * 1.0, plantRadius * 1.2, factor2 * factor2)
          efct = lerp(sqrt(stemEffectFactor), blossomEffectFactor, factor2)
          if (factor > 0.5) then
            efct = lerp(0.0, blossomEffectFactor, factor2)
          end if
          
          drawRec = rect(blobPos.locH - blobSize, blobPos.locV - blobSize, blobPos.locH + blobSize, blobPos.locV + blobSize)
          drawRec = rotateToQuad(drawRec, random(360))
          member( "layer" & plantLayer).image.copyPixels(member("lumpyCircle").image, drawRec, member("lumpyCircle").image.rect, {#color:colr, #ink: 36})
          copyPixelsToEffectColor(gdLayer, plantLayer, drawRec, "EfctGradient", rect(0, ((1 - efct) * 100).integer, 1, ((1 - efct) * 100).integer + 1), 0.5)
          
        end repeat
        
        repeat with leaf = 1 to leafCount do 
          factor = (leaf / float(leafCount + 1.0))
          leafPos = lerpVector(leavesPos, leavesPos2, clamp(factor + (randomRange(-100, 100) / 1000.0), 0, 1))
          leafSize = lerp(0.1, 0.8, 6.75 * (factor*factor - factor*factor*factor))
          drawRec = rect(leafPos.locH - 12*leafSize, leafPos.locV - 6*leafSize, leafPos.locH + 12*leafSize, leafPos.locV + 6*leafSize)
          drawRec = rotateToQuad(drawRec, leafDeg + randomRange(-5, 5))
          efct = lerp(sqrt(stemEffectFactor), blossomEffectFactor, 4 * (factor - factor * factor))
          if (factor > 0.5) then
            efct = lerp(0.0, blossomEffectFactor, 4 * (factor - factor * factor))
          end if
          
          
          variant = randomRange(0, 2)
          spriteRect = rect(0, 1 + (variant*12), 24, 1 + ( (variant+1) * 12))
          
          member("layer" & plantLayer).image.copyPixels(member("BrainGrowerGraf").image, drawRec, spriteRect, {#color: colr, #ink: 36})
          --use: copyPixelsToEffectColor(effect color letter from "Color" option (A, B, C=none), depth layer (from 0 to 29), final rectangle, gradient image name, source rectangle, blend modifier(from 0 to 1))
          copyPixelsToEffectColor(gdLayer, plantLayer, drawRec, "EfctGradient", rect(0, ((1 - efct) * 100).integer, 1, ((1 - efct) * 100).integer + 1), 0.5)
          copyPixelsToEffectColor(gdLayer, plantLayer, drawRec, "BrainGrowerGrad", spriteRect, 0.5)
        end repeat
      end if
      
      if (plantSize > 0.3) and randomRange(1, 5) <> 2 then
        
        weirdLeafSpanCount = randomRange(0, 3)
        repeat with weirdLeafSpan = 0 to weirdLeafSpanCount do
          spanSize = randomRange(10, 70)
          spanStart = randomRange(2, curvePoints.count - (spanSize + 1))
          prevPnt = curvePoints[spanStart][1]
          
          distanceSinceLastLeaf = -1
          repeat with leaf = 0 to spanSize do
            factor = leaf / float(spanSize)
            curveIndex = spanStart + leaf
            pnt = curvePoints[curveIndex][1]
            distanceSinceLastLeaf = distanceSinceLastLeaf + mag(pnt - prevPnt)
            
            if (distanceSinceLastLeaf = -1) or (distanceSinceLastLeaf > 8) then
              distanceSinceLastLeaf = 0
              
              leafSize = lerp(0.5, 1.0, 4 * (factor - factor * factor))
              leafDeg = vecToDeg(curvePoints[curveIndex - 1][1] - curvePoints[curveIndex + 1][1])
              if upsideDown then
                leafDeg = leafDeg + 180
              end if
              drawRec = rect(pnt.locH - 12*leafSize, pnt.locV - 6*leafSize, pnt.locH + 12*leafSize, pnt.locV + 6*leafSize)
              drawRec = rotateToQuad(drawRec, leafDeg + randomRange(-10, 10))
              efct = curvePoints[curveIndex][3]
              variant = randomRange(0, 2)
              spriteRect = rect(0, 1 + (variant*12), 24, 13 + (variant*12))
              member("layer" & plantLayer).image.copyPixels(member("BrainGrowerGraf").image, drawRec, spriteRect, {#color: colr, #ink: 36})
              copyPixelsToEffectColor(gdLayer, plantLayer, drawRec, "EfctGradient", rect(0, ((1 - efct) * 100).integer, 1, ((1 - efct) * 100).integer + 1), 0.5)
              copyPixelsToEffectColor(gdLayer, plantLayer, drawRec, "BrainGrowerGrad", spriteRect, 0.5)
            end if
            
            prevPnt = pnt
          end repeat
        end repeat
        
      end if
      
      
    end if
  end repeat
end

on drawWeirdLittleWiggle res, pnt1, lyr1, rad1, v1, e1, pnt2, lyr2, rad2, v2, e2, col, curvePoints
  pointDist = distanceBetweenPoints(pnt1, pnt2)
  iterations = pointDist / res
  
  repeat with iteration = 0 to iterations.integer
    factor = iteration.float / iterations.float
    pnt = hermiteInterpolation(pnt1, v1, pnt2, v2, factor)
    
    lyr = lerp(lyr1, lyr2, factor).integer
    rad = lerp(rad1, rad2, factor)
    
    offsetSine = sin(factor * 3.141592 * 4)
    offsetAmount = rad * 0.2
    rad = rad + lerp(-offsetAmount, offsetAmount, offsetSine)
    
    efct = lerp(e1, e2, factor + lerp(-0.1, 0.1, random(1000) / 1000) + lerp(-0.05, 0.05, offsetSine))
    
    curvePoints.add([pnt, rad, efct])
    
    drawRec = rect(pnt.locH - rad, pnt.locV - rad, pnt.locH + rad, pnt.locV + rad)
    drawRec = rotateToQuad(drawRec, random(360))
    member( "layer" & lyr).image.copyPixels(member("lumpyCircle").image, drawRec, member("lumpyCircle").image.rect, {#color:col, #ink: 36})
    copyPixelsToEffectColor(gdLayer, lyr, drawRec, "EfctGradient", rect(0, ((1.0 - efct) * 100).integer, 1, ((1.0 - efct) * 100).integer + 1), 0.5)
  end repeat
end 

on applyMeatBlobs me, screenX, screenY, amount
  tileX = screenX + gRenderCameraTilePos.locH
  tileY = screenY + gRenderCameraTilePos.locV
  
  midPoint = giveMiddleOfTile(point(screenX, screenY))
  tilePoint = midPoint + gRenderCameraTilePos
  
  --writeMessage("meat blob start " & tileX & ", " & tileY & " : " & locX & ", " & locY)
  
  layerMin = 0
  layerMax = 29
  
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      layerMin = 0
      layerMax = 29
    "1":
      layerMin = 0
      layerMax = 6
    "2":
      layerMin = 10
      layerMax = 16
    "3":
      layerMin = 20
      layerMax = 29
    "1:st and 2:nd":
      layerMin = 0
      layerMax = 16
    "2:nd and 3:rd":
      layerMin = 10
      layerMax = 29
    otherwise:
      layerMin = 0
      layerMax = 29
  end case
  
  --"Blob Size":
  --blobSize = [3, 2, 1][["Big", "Medium", "Small"].getPos(op[3])]
  --"Should grow on layer behind":
  --growOnWalls = (op[3] = "Yes")
  --"Needs to be attached to walls":
  --needsAttach = (op[3] = "Yes")
  
  
  --lerp(1, 1.8, amount / 100.0)
  countInTile = amount / 100.0
  countInTile = countInTile * countInTile * 50.0
  --writeMessage("meat blobs count: " & amount & ", " & countInTile)
  
  blobMinSize = 10
  blobMaxSize = 12.5
  blobCutoffSize = 6
  initialSize = 1.8
  if blobSize = 2 then
    blobMinSize = 10
    blobMaxSize = 12.5
    initialSize = 1.0
    blobCutoffSize = 4
  else if blobSize = 1 then
    blobMinSize = 5
    blobMaxSize = 6.25
    initialSize = 1.2
    blobCutoffSize = 3
  end if
  
  repeat with blob = 1 to countInTile then
    blobSize = randomRange(blobMinSize, blobMaxSize)
    blobDepth = mapRange(blobMinSize, blobMaxSize, 3, 6, blobSize).integer
    blobLayer = randomRange(layerMin + integer(blobDepth*0.5), layerMax - blobDepth)
    blobPos = midPoint + point(randomRange(-10, 10), randomRange(-10, 10))
    
    blobGeoLayer = 3
    
    if(blobLayer - (blobDepth*0.6) < 10)then
      blobGeoLayer = 1
    else if (blobLayer - (blobDepth*0.6) < 20) then
      blobGeoLayer = 2
    end if
    
    solid = 0
    growDanglyBits = 0
    sizeMultiplier = initialSize
    
    
    repeat with dir in [point(0,-1), point(-1,0), point(1,0), point(0,1)]then
      if(solidAfaMv(giveGridPos(blobPos + dir * blobSize)+gRenderCameraTilePos, blobGeoLayer) = 1)then
        if (dir = point(0,-1)) then
          sizeMultiplier = sizeMultiplier * 2
          growDanglyBits = 1
        else if (dir = point(-1, 1)) or (dir = point(1, 1)) then
          sizeMultiplier = sizeMultiplier * 1.33
        end if
        solid = 1
        exit repeat
      end if
    end repeat
    
    if (growOnWalls = true) and solid <> 1 then
      if solidAfaMv(giveGridPos(blobPos) + gRenderCameraTilePos, min(3, blobGeoLayer + 1)) then
        solid = 1
        growDanglyBits = 1
        sizeMultiplier = sizeMultiplier * 1.1
      end if
    end if
    
    -- if we're in the air, check surrounding walls again but with, like, a new distance factor based off the layer position
    -- hopefully this will make it look like the layers kind of spread into each other 
    if solid = 0 and solidAfaMv(giveGridPos(blobPos) + gRenderCameraTilePos, min(3, blobGeoLayer + 1)) and blobLayer > 5 and (needsAttach <> true) then
      baseLayer = (blobGeoLayer - 1) * 10
      distanceFromBaseLayer = abs(baseLayer - blobLayer) / 10.0
      testOffset = randomRange(blobSize, max(blobSize, distanceFromBaseLayer * 60))
      sizeMultiplier = lerp(initialSize, initialSize * 0.7, distanceFromBaseLayer)
      repeat with dir in [point(0,-1), point(-1,0), point(1,0), point(0,1)]then
        if(solidAfaMv(giveGridPos(blobPos + dir * testOffset)+gRenderCameraTilePos, blobGeoLayer) = 1)then
          if (dir = point(0,-1)) then
            sizeMultiplier = sizeMultiplier * 2
          else if (dir = point(-1, 1)) or (dir = point(1, 1)) then
            sizeMultiplier = sizeMultiplier * 1.33
          end if
          solid = 1
          exit repeat
        end if
      end repeat
      
      growDanglyBits = 0
    end if
    
    if needsAttach <> true then solid = 1
    
    -- if our current position is solid, skip!
    if(solidAfaMv(giveGridPos(blobPos) + gRenderCameraTilePos, blobGeoLayer) = 1)then
      solid = 0
    end if
    
    if (random(100) = 2) then blobSize = blobSize + lerp(2, 5, random(10000).float / 10000.0)
    
    --on drawLumpySphereAtPoint pnt, layer, rad, depthRad, col 
    if solid = 1 and (blobSize * sizeMultiplier) > blobCutoffSize then drawLumpySphereAtPoint(blobPos, blobLayer, blobSize * sizeMultiplier, blobDepth * min(sizeMultiplier, 1), color(0, 255, 0))
    
    if growDanglyBits = 1 and solid = 1 then
      shouldDraw = randomRange(0, 3)
      if shouldDraw = 1 then
        lerpFactor = random(10000).float / 10000.0
        danglePos = blobPos
        danglePosBottom = danglePos + point(0, randomRange(20, 100))
        dangleLayer = randomRange(blobLayer, blobLayer - blobDepth)
        deriv = point(0, 1)
        dangleRadius = randomRange(0.2, 1)
        dangleEndRadius = dangleRadius + randomRange(1.2, 4)
        --drawFezTreeBranchSegment res, pnt1, lyr1, rad1, dpthRad1, v1, pnt2, lyr2, rad2, dpthRad2, v2, lumpinessMin, lumpinessMax, col, curvePoints
        drawLumpySphereLine(1, danglePos, dangleLayer, dangleRadius, 0, danglePosBottom, dangleLayer, dangleEndRadius, 2, color(0, 255, 0))
        --drawFezTreeBranchSegment(1.5, danglePos, dangleLayer + 1, dangleRadius, 0, deriv, danglePosBottom, dangleLayer, dangleEndRadius, 2, deriv, -1, 2, color(0, 255, 0), [])
      end if  
    end if
  end repeat
  
  -- draw some funny meat ropes
  if amount > 25 then
    attached = false
    attachmentDirection = point(0, 0)
    ropeLayer = randomRange(layerMin, layerMax)
    ropePos = midPoint
    
    ropeGeoLayer = 3
    if(ropeLayer < 10)then
      ropeGeoLayer = 1
    else if (ropeLayer < 20) then
      ropeGeoLayer = 2
    end if
    
    repeat with dir in [point(0,-1)]then
      if(solidAfaMv(giveGridPos(ropePos + dir * 20) + gRenderCameraTilePos, ropeGeoLayer) = 1)then
        attached = true
        attachmentDirection = dir
        exit repeat
      end if
    end repeat
    
    if(solidAfaMv(giveGridPos(ropePos) + gRenderCameraTilePos, ropeGeoLayer) = 1)then
      attached = false
    end if
    
    foundEnd = false
    ropeEndLayer = 0
    ropeEndPos = point(0, 0)
    
    if attached = true then
      maxAttempts = 100
      repeat with attempt = 1 to maxAttempts then
        ropeEndPos = ropePos + (point(randomRange(-10, 10), randomRange(-3, 3)) * 20)
        ropeEndLayer = randomRange((ropeGeoLayer - 1) * 10, ropeGeoLayer * 10)
        ropeEndGeoLayer = ropeGeoLayer
        
        if (solidAfaMv(giveGridPos(ropeEndPos) + gRenderCameraTilePos, ropeEndGeoLayer) <> 1) then
          repeat with dir in [point(0, -1)]then
            if(solidAfaMv(giveGridPos(ropeEndPos + dir * 20) + gRenderCameraTilePos, ropeEndGeoLayer) = 1)then
              foundEnd = true
              ropeEndPos = ropeEndPos + (dir * 10)
              exit repeat
            end if
          end repeat
        end if
        
        if foundEnd = true then exit repeat
      end repeat
      
      
    end if
    
    if foundEnd = true then
      writeMessage("rope!")
      ropePos = ropePos + attachmentDirection * 10
      slackPos = lerpVector(ropePos, ropeEndPos, 0.5) + point(0, randomRange(120, 200))
      
      wireRadiusMin = randomRange(1, 2)
      wireRadiusMax = wireRadiusMin + 5
      
      lastPos = ropePos
      lastLayer = ropeLayer
      lastWireRadius = wireRadiusMax
      repeat with t = 1 to 8
        f = t.float / 8.0
        
        -- basic quadratic bezier
        lerp1p = lerpVector(ropePos, slackPos, f)
        lerp2p = lerpVector(slackPos, ropeEndPos, f)
        currentPos = lerpVector(lerp1p, lerp2p, f)
        currentLayer = lerp(ropeLayer, ropeEndLayer, f).integer
        
        -- 1 at the ends, 0 in the middle
        wireRadius = abs(f - 0.5) * 2.0
        wireRadius = clamp(mapRange(1.0, 0.3, 1.0, 0.0, wireRadius), 0.0, 1.0)
        wireRadius = lerp(wireRadiusMin, wireRadiusMax, wireRadius * wireRadius)
        wireRadius = wireRadius + randomRange(-1, 0)
        drawLumpySphereLine(1, lastPos, lastLayer, lastWireRadius, 0, currentPos, currentLayer, wireRadius, 0, color(0, 255, 0))
        lastPos = currentPos
        lastLayer = currentLayer
        lastWireRadius = wireRadius
      end repeat
    end if
  end if
end