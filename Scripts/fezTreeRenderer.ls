
on drawLeaf pnt, lyr, rot, xRot, glowIntensity, effectLayer
  variant = random(7) - 1
  drawArea = rect(pnt.locH - 12, pnt.locV - 24, pnt.locH + 12, pnt.locV + 24)
  drawArea = rotateToQuad(drawArea, rot)
  
  col = color(0, 255, 0)
  if effectLayer = "B" then
    col = color(0, 255, 255)
  else if effectLayer = "A" then
    col = color(255, 0, 255)
  end if
  
  uOffset = variant * 24
  vOffset = xRot * 48
  imgArea = rect(0 + uOffset, 1 + vOffset, 24 + uOffset, 1 + 48 + vOffset)
  member( "layer" & lyr).image.copyPixels(member("fezTreeGraf").image, drawArea, imgArea, {#color:col, #ink: 36})
  copyPixelsToEffectColor(effectLayer, lyr, drawArea, "fezTreeGrad", imgArea, 1)
  --use: copyPixelsToEffectColor(effect color letter from "Color" option (A, B, C=none), depth layer (from 0 to 29), final rectangle, gradient image name, source rectangle, blend modifier(from 0 to 1))
end

on drawFezTreeBranchSegment res, pnt1, lyr1, rad1, dpthRad1, v1, pnt2, lyr2, rad2, dpthRad2, v2, lumpinessMin, lumpinessMax, col, curvePoints
  pointDist = distanceBetweenPoints(pnt1, pnt2)
  iterations = pointDist / res
  repeat with iteration = 0 to iterations.integer
    factor = iteration.float / iterations.float
    pnt = hermiteInterpolation(pnt1, v1, pnt2, v2, factor)
    
    lyr = lerp(lyr1, lyr2, factor).integer
    dpthRad = lerp(dpthRad1, dpthRad2, factor)
    rad = lerp(rad1, rad2, factor)
    
    curvePoints.add([pnt, lyr])
    
    drawLumpySphereAtPoint(pnt, lyr, rad + randomRange(lumpinessMin, lumpinessMax), dpthRad, col)
  end repeat
end 

on drawFezTreeAtPosition treeBasePos, treeBaseAngle, treeLeavesPos, treeLeavesSize, treeLeavesAngle, treeLayer, effectLayer, leafDensity
  leavesRadiusX = treeLeavesSize.locH
  leavesRadiusY = treeLeavesSize.locV
  -- average area: ~4900
  leavesArea = (leavesRadiusX * 2) * (leavesRadiusY * 2)
  -- average perimeter: ~210
  leavesPerimeter = (leavesRadiusX * 2) + (leavesRadiusY * 4)
  
  depthOscillations = randomRange(1, 3)
  depthMaxOffset = randomRange(1, 7)
  
  -- ok time for some bullshit
  -- first let's do the main "branch"
  
  -- creating a wiggly path
  -- first we start with a line.
  branchPath = [treeBasePos, treeLeavesPos]
  
  -- then, randomly add a new point in the middle of two points and offset it
  -- and repeat for however many wiggles we want
  offsetAmountX = 0.8
  offsetAmountY = 0.1
  repeat with iterations = 0 to 1
    subdivideIndex = random(branchPath.count - 1)
    newBranchPath = []
    offsetIndex = 1
    ind = 1
    repeat with ind = 1 to branchPath.count
      currentPoint = branchPath[ind]
      newBranchPath.add(currentPoint)
      
      -- subdivide if i should
      if subdivideIndex = ind then
        nextPoint = branchPath[ind + 1]
        
        middlePoint = LerpVector(currentPoint, nextPoint, 0.5)
        pointDist = distanceBetweenPoints(currentPoint, nextPoint)
        amountToOffsetX = pointDist * 0.5 * offsetAmountX
        amountToOffsetY = pointDist * 0.5 * offsetAmountY
        
        middlePoint = middlePoint + point(randomRange(-amountToOffsetX, amountToOffsetX), randomRange(-amountToOffsetY, amountToOffsetY))
        
        newBranchPath.add(middlePoint)
      end if
    end repeat
    
    branchPath = newBranchPath
  end repeat
  
  -- next we smooth out the path with some weird catmull-rom spline smoothing
  
  -- calculate the "directions" of each point for catmull-rom smoothing...
  -- special cases for the first / last point because they dont have two neighbors :P
  branchPathDirections = [degToVec(treeBaseAngle + randomRange(-10, 10)) * -25] --point(randomRange(-20, 20), -20)
  repeat with branchIndex = 2 to branchPath.count - 1
    dir = branchPath[branchIndex + 1] - branchPath[branchIndex - 1]
    originalDirMag = mag(dir)
    dir = dir * point(2, 1) -- i want more horizontal curving
    dir = (dir / mag(dir)) * originalDirMag -- reset the length
    branchPathDirections.add(dir * 0.2)
  end repeat
  branchPathDirections[branchPath.count] = degToVec(treeLeavesAngle.float) * 30
  
  -- approximation of total distance for interpolating branch size etc
  -- not 100% accurate beacuse of spline interpolation. truly do not give a shit.
  totalDistance = 0
  repeat with branchIndex = 1 to branchPath.count - 1
    totalDistance = totalDistance + distanceBetweenPoints(branchPath[branchIndex], branchPath[branchIndex + 1])
  end repeat
  
  currentDistance = 0
  
  currentDistances = []
  curvePoints = []
  repeat with branchIndex = 1 to branchPath.count - 1
    pnt1 = branchPath[branchIndex]
    pnt2 = branchPath[branchIndex + 1]
    
    v1 = branchPathDirections[branchIndex]
    v2 = branchPathDirections[branchIndex + 1]
    
    factor1 = currentDistance / totalDistance
    currentDistances.add(currentDistance)
    currentDistance = currentDistance + distanceBetweenPoints(pnt1, pnt2)
    factor2 = currentDistance / totalDistance
    
    currentDistances.add(currentDistance)
    
    layer1 = max(1, (sin(depthOscillations * 3.141592 * factor1) * depthMaxOffset) + treeLayer).integer
    layer2 = max(1, (sin(depthOscillations * 3.141592 * factor2) * depthMaxOffset) + treeLayer).integer
    
    thickness1 = 6 + 12 * (1 - sqrt(factor1)) --smoothstep(6, 20, 1 - factor1)
    thickness2 = 6 + 12 * (1 - sqrt(factor2)) --smoothstep(6, 20, 1 - factor2)
    
    avgThick = (thickness1 + thickness2) * 0.5
    lumpinessMin = 0--(avgThick * -0.08).integer
    lumpinessMax = (avgThick *  0.23).integer
    
    drawFezTreeBranchSegment(2.5, pnt1, layer1, thickness1, 2, v1, pnt2, layer2, thickness2, 2, v2, lumpinessMin, lumpinessMax, color(0, 255, 0), curvePoints)
  end repeat
  
  -- draw some weird vines that go between random points
  wireCount = randomRange(0, 8)
  repeat with w = 1 to wireCount
    wireRadiusMin = randomRange(1, 2)
    wireRadiusMax = wireRadiusMin + 2
    
    index1 = random(curvePoints.count - 6)
    startPoint = curvePoints[index1]
    endPoint = curvePoints[randomRange(index1 + 5, curvePoints.count)]
    slackPoint = [lerpVector(startPoint[1], endPoint[1], 0.5), lerp(startPoint[2], endPoint[2], 0.5).integer]
    slackLength = randomRange(40, 150)
    slackPoint[1] = slackPoint[1] + point(0, slackLength)
    
    lastPos = startPoint[1]
    lastLayer = startPoint[2]
    lastWireRadius = wireRadiusMax
    repeat with t = 1 to 8
      f = t.float / 8.0
      
      -- basic quadratic bezier
      lerp1p = lerpVector(startPoint[1], slackPoint[1], f)
      lerp2p = lerpVector(slackPoint[1], endPoint[1], f)
      currentPos = lerpVector(lerp1p, lerp2p, f)
      currentLayer = lerp(startPoint[2], endPoint[2], f).integer
      
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
  end repeat
  
  -- roots
  rootCount = randomRange(2, 12)
  repeat with root = 1 to rootCount
    -- only use the first few nodes of the tree
    connectionIndex = randomRange(1, 12)
    startPos = curvePoints[connectionIndex][1]
    startLayer = curvePoints[connectionIndex][2]
    
    hOffset = randomRange(-30, 30)
    hDirection = degToVec(treeBaseAngle + 90)
    vDirection = -degToVec(treeBaseAngle)
    endPos = treeBasePos + hDirection * hOffset + vDirection * 20
    endLayer = clamp(startLayer + randomRange(-10, 10), 1, 29)
    
    startCurveDerivative = hDirection*sign(hOffset)*30 + vDirection*10
    endCurveDerivative = hDirection*sign(hOffset)*5 + vDirection*20
    --drawFezTreeBranchSegment res, pnt1, lyr1, rad1, dpthRad1, v1, pnt2, lyr2, rad2, dpthRad2, v2, lumpinessMin, lumpinessMax, col, curvePoints
    drawFezTreeBranchSegment(1.5, startPos, startLayer, 6, 2, startCurveDerivative, endPos, endLayer, 2, 2, endCurveDerivative, -1, 2, color(0, 255, 0), [])
  end repeat
  
  -- leaves!
  -- first draw some big old blobs to fill in the leaf cube
  blLeafPoint = treeLeavesPos + (degToVec(treeLeavesAngle.float + 90) * leavesRadiusX)
  brLeafPoint = treeLeavesPos + (degToVec(treeLeavesAngle.float - 90) * leavesRadiusX)
  tlLeafPoint = blLeafPoint + (degToVec(treeLeavesAngle.float) * leavesRadiusY * 2)
  trLeafPoint = brLeafPoint + (degToVec(treeLeavesAngle.float) * leavesRadiusY * 2)
  
  leafBlobCount = (leavesArea * 0.0075).integer + 1
  repeat with lblob = 1 to leafBlobCount
    blobSize = randomRange(12, 25)
    
    -- offset the min and max positions so its not *too* lumpy...
    blobOffsetRadius = blobSize * 0.8
    tBlobOffset = degToVec(treeLeavesAngle.float) * -blobOffsetRadius
    lBlobOffset = degToVec(treeLeavesAngle.float + 90) * -blobOffsetRadius
    rBlobOffset = lBlobOffset * -1
    
    xLerp = random(10000).float / 10000.0
    yLerp = random(10000).float / 10000.0
    blerp = lerpVector(blLeafPoint + lBlobOffset, brLeafPoint + rBlobOffset, xLerp)
    tlerp = lerpVector(tlLeafPoint + lBlobOffset + tBlobOffset, trLeafPoint + rBlobOffset + tBlobOffset, xLerp)
    
    blobPos = lerpVector(blerp, tlerp, ylerp)
    blobDepth = mapRange(12, 25, 3, 6, blobSize).integer
    blobLayer = randomRange(treeLayer - (10 - blobDepth), treeLayer)
    --drawLumpySphereAtPoint pnt, layer, rad, depthRad, col
    drawLumpySphereAtPoint(blobPos, blobLayer, blobSize, blobDepth, color(0, 255, 0))
  end repeat
  
  -- actual leaves
  -- first the front leaves
  fleafCount = (leavesArea * 0.02 * leafDensity).integer
  
  repeat with fleaf = 1 to fleafCount
    xLerp = random(10000).float / 10000.0
    yLerp = random(10000).float / 10000.0
    blerp = lerpVector(blLeafPoint, brLeafPoint, xLerp)
    tlerp = lerpVector(tlLeafPoint, trLeafPoint, xLerp)
    
    leafPos = lerpVector(blerp, tlerp, ylerp) + point(randomRange(-20, 20) * (1 - yLerp), randomRange(-5, 5))
    leafSpacePnt = point(xLerp - 0.5, yLerp)
    normalizedPoint = leafSpacePnt / mag(leafSpacePnt)
    leafRot = atan(-normalizedPoint.locH / normalizedPoint.locV) * 57.2958
    leafRot = leafRot + treeLeavesAngle + randomRange(-15, 15)
    
    leafLayer = randomRange(treeLayer - 12, treeLayer - 10)
    
    weirdDistToEdge = max(abs(leafSpacePnt.locH * 2), leafSpacePnt.locV)
    leafXRot = (mapRange(0, 1, 3, 0, weirdDistToEdge)).integer
    
    drawLeaf(leafPos, clamp(leafLayer, 0, 29).integer, leafRot, leafxRot, 0.5, effectLayer)
  end repeat
  
  -- side leaves
  sLeafCount = (leavesPerimeter * 0.57 * leafDensity).integer
  repeat with sleaf = 1 to sleafCount
    leafSide = random(3)
    
    sideLerp = random(10000).float / 10000.0
    leafPos = treeLeavesPos
    leafSpacePnt = point(0, 0)
    leafHorizontalOffsetVector = point(0, 0)
    if leafSide = 1 then -- left
      leafPos = lerpVector(blLeafPoint, tlLeafPoint, sideLerp)
      leafSpacePnt = point(-0.5, leafSide)
      leafHorizontalOffsetVector = degToVec(treeLeavesAngle.float + 90)
    else if leafSide = 2 then -- top
      leafPos = lerpVector(tlLeafPoint, trLeafPoint, sideLerp)
      leafSpacePnt = point(leafSide - 0.5, 1)
    else if leafSide = 3 then -- right
      leafPos = lerpVector(brLeafPoint, trLeafPoint, sideLerp)
      leafSpacePnt = point(0.5, leafSide)
      leafHorizontalOffsetVector = degToVec(treeLeavesAngle.float - 90)
    end if
    normalizedPoint = (leafPos - treeLeavesPos) / mag((leafPos - treeLeavesPos))
    leafRot = atan(-normalizedPoint.locH / normalizedPoint.locV) * 57.2958
    leafRot = leafRot-- + treeLeavesAngle --+ treeLeavesAngle-- + randomRange(-15, 15)
    leafxRot = 0--randomRange(0, 1)
    
    leafLayer = randomRange(treeLayer - 10, treeLayer + 3)
    hOffsetAmount = power(1 - sideLerp, 3) * power(random(10000).float / 10000.0, 4) * 20
    vOffsetAmount = power(random(10000).float / 10000.0, 5) * 20
    leafPos = leafPos + (leafHorizontalOffsetVector * hOffsetAmount) + (degToVec(treeLeavesAngle.float) * maxAbs(randomRange(-5, 5), vOffsetAmount))
    drawLeaf(leafPos, clamp(leafLayer, 0, 29).integer, leafRot, leafxRot, 0.5, effectLayer)
  end repeat
  
  -- finally. weird dangly bits.
  dangleCount = randomRange(2, 5)
  repeat with dangleIndex = 0 to dangleCount
    lerpFactor = random(10000).float / 10000.0
    danglePos = lerpVector(blLeafPoint, brLeafPoint, lerpFactor) + (randomRange(0, -5) * degToVec(treeLeavesAngle.float))
    danglePosBottom = danglePos + point(0, randomRange(30, 80))
    dangleLayer = randomRange(treeLayer - 5, treeLayer)
    deriv = point(0, 1)
    dangleRadius = randomRange(0.2, 1)
    dangleEndRadius = dangleRadius + 1
    --drawFezTreeBranchSegment res, pnt1, lyr1, rad1, dpthRad1, v1, pnt2, lyr2, rad2, dpthRad2, v2, lumpinessMin, lumpinessMax, col, curvePoints
    drawFezTreeBranchSegment(1.5, danglePos, dangleLayer, dangleRadius, 0, deriv, danglePosBottom, dangleLayer, dangleEndRadius, 1, deriv, -1, 2, color(0, 255, 0), [])
  end repeat
end
