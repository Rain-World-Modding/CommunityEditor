on randomRange minimum, maximum
  return min(minimum, maximum) + random(abs(maximum - minimum))
end

on randomRangeInclusive minimum, maximum
  return min(minimum, maximum) + (random(abs(maximum - minimum) + 1) - 1)
end

on offsetQuad quadToOffset, offsetVector
  repeat with vertIndex = 1 to 4
    quadToOffset[vertIndex] = quadToOffset[vertIndex] + offsetVector
  end repeat
  
  return quadToOffset
end

on hermiteInterpolation p1, v1, p2, v2, t
  t2 = t * t
  t3 = t2 * t
  return bezier(p1, p1 + v1 * 3, p2, p2 + v2 * -3, t)--((2*t3 - 3*t2 + 1) * p1) + ((t3 - 2*t2 + t) * v1) + ((-2*t3 + 3*t2) * p2) + ((t3 - t2) * v2)
end

on mag pnt
  return sqrt(pnt.locH * pnt.locH + pnt.locV * pnt.locV)
end

on distanceBetweenPoints p1, p2
  dif = p1 - p2
  return mag(dif)
end

on smoothstep mn, mx, t
  return mn + ((t * t * (3.0 - 2.0 * t)) * (mx - mn))
end

on clamp a, mn, mx
  return min(max(a, mn), mx)
end

on mapRange a1, a2, b1, b2, s
  return b1 + ((s - a1)*(b2 - b1))/(a2 - a1)
end

on mapRangeClamped a1, a2, b1, b2, s
  return mapRange(a1, a2, b1, b2, clamp(s, a1, a2))
end

on sign a
  if a > 0 then
    return 1
  else if a < 0 then
    return -1
  end if
  
  return 0
end

on maxAbs a, b
  if abs(a) > abs(b) then
    return a
  end if
  
  return b
end

on vecToDeg pnt 
  normalizedPoint = pnt / mag(pnt)
  return atan(-normalizedPoint.locH / normalizedPoint.locV) * 57.2958
end


on drawLumpySphereAtPoint pnt, layer, rad, depthRad, col 
  lyr = clamp(layer, 0, 29)
  
  if depthRad = 0 then
    drawRec = rect(pnt.locH - rad, pnt.locV - rad, pnt.locH + rad, pnt.locV + rad)
    drawRec = rotateToQuad(drawRec, random(360))
    member( "layer" & lyr).image.copyPixels(member("lumpyCircle").image, drawRec, member("lumpyCircle").image.rect, {#color:col, #ink: 36})
    return
  end if
  
  repeat with layerOffset = 0 to depthRad
    -- we add 1 to depthRad so that we don't get layers with 0 radius and it's more consistent to work with :P
    factor = float(layerOffset) / float(depthRad + 1.0)
    currentRad = rad * (1.0 - power(factor, 1.5))
    
    drawRec = rect(pnt.locH - currentRad, pnt.locV - currentRad, pnt.locH + currentRad, pnt.locV + currentRad)
    drawRec = rotateToQuad(drawRec, random(360))
    
    currentLayer = clamp(lyr - layerOffset, 0, 29)
    
    member("layer" & currentLayer).image.copyPixels(member("lumpyCircle").image, drawRec, member("lumpyCircle").image.rect, {#color: col, #ink: 36})
  end repeat
end

on drawLumpySphereLine res, pnt1, lyr1, rad1, dpthRad1, pnt2, lyr2, rad2, dpthRad2, col
  pointDist = distanceBetweenPoints(pnt1, pnt2)
  iterations = pointDist / res
  
  repeat with iteration = 0 to iterations.integer
    factor = iteration.float / iterations.float
    pnt = lerpVector(pnt1, pnt2, factor)
    lyr = lerp(lyr1, lyr2, factor).integer
    dpthRad = lerp(dpthRad1, dpthRad2, factor)
    rad = lerp(rad1, rad2, factor)
    
    
    drawLumpySphereAtPoint(pnt, lyr, rad, dpthRad, col)
  end repeat
end



