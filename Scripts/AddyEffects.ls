global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, chOp, fatOp, gradAf, effectIn3D, gAnyDecals, gRotOp, slimeFxt, DRDarkSlimeFix, DRWhite, DRPxl, DRPxlRect, colrIntensity, hasFlowers, skyRootsFix, frondSz, gdFrondLayer, colrFrond, swSz



-- Addy
on ApplyRipcords me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      lr = random(3)
    "1":
      lr= 1
    "2":
      lr= 2
    "3":
      lr= 3
    "1:st and 2:nd":
      lr = random(2)
    "2:nd and 3:rd":
      lr = random(2)+1
    otherwise:
      lr = random(3)
  end case
  
  dmax = (lr*10)-1
  dmin = (lr*10)-9
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    midPoint = giveMiddleOfTile(point(q, c))
    midPoint = midPoint + point(lerp(-10, 10, random(100).float/100), lerp(-10, 10, random(100).float/100))
    qd = midPoint
    plantAngle = lerp(160, 200, random(100).float/100)
    points = []
    points.add(midPoint)
    RepeatFlag = True
    plantOffset = lerp(10, 20, random(100).float/100)
    
    
    zigZagCounter = lerp(5, 10, random(100).float/100).integer
    Repeat While RepeatFlag then
      
      qd = qd + degToVecFac2(plantAngle, plantOffset, plantOffset)
      points.add(qd)
      
      --Zigzag behaviour in here
      if zigZagCounter > 1 then
        --Zags the plant the other way
        if plantAngle > 180 then
          plantAngle = plantAngle - lerp(90, 130, random(100).float/100)
        else
          plantAngle = plantAngle + lerp(90, 130, random(100).float/100)
        end if
        plantOffset = lerp(25, 40, random(100).float/100)
        --
      else if zigZagCounter = 1 then
        --The transition
        if plantAngle > 180 then
          plantAngle = plantAngle - lerp(40, 90, random(100).float/100)
        else
          plantAngle = plantAngle + lerp(40, 90, random(100).float/100)
        end if
        plantOffset = lerp(15, 25, random(100).float/100)
        --
      else
        --Stem will rarely zig a little
        stemVariation = (random(100).float/100)
        if random(2) = 1 then
          plantAngle = lerp(180, 220, stemVariation*stemVariation)     
        else
          plantAngle = lerp(180, 140, stemVariation*stemVariation)     
        end if  
        --
        plantOffset = lerp(10, 20, random(100).float/100)
      end if
      --
      
      if withinBoundsOfLevel(giveGridPos(qd) + gRenderCameraTilePos) = 0 then
        if skyRootsFix then
          exit 
        end if
        repeatFlag = false
      end if
      
      if afaMvLvlEdit(giveGridPos(qd) + gRenderCameraTilePos, lr) = 1 then
        repeatFlag = false
      end if
      zigZagCounter = zigZagCounter - 1
    end repeat
    
    
    layer = lerp(dmin, dmax, random(100).float/100).integer
    -- Not allowed to intersect the player
    if layer = 5 then 
      layer = layer + 1
    end if
    -- Obscures the player less
    if layer < 5 then 
      if random(2) = 2 then
        exit
      end if
    end if
    --
    
    totalDist = 0
    totali = 0
    repeat with i2 = 1 to points.count then
      if i2 > 1 then
        totalDist = totalDist + sqrt(power(points[i2].locv - points[i2-1].locv, 2) + power(points[i2].loch - points[i2-1].loch, 2))
      end if
    end repeat
    
    -- Root thickness is determined by total length and some subtle randomness
    thicknessAmp = 1+random(3)
    maxThickness = lerp(5, 13,  ((thicknessAmp*((restrict(totalDist, 100, 400))/4))+random(100).float)/(thicknessAmp+1))
    --
    
    -- At smaller lengths, the effect looks really bad, kills him when that happens :D
    if totalDist < 100 then
      exit
    end if
    --
    
    Repeat with i = 1 to points.count then
      if i > 1 then
        dist  = sqrt(power(points[i].locv - points[i-1].locv, 2) + power(points[i].loch - points[i-1].loch, 2))
        
        repeat with i2 = 1 to dist then
          percent = i2.float/dist
          totalPercent = totali.float/totalDist
          thickness = lerp(2, maxThickness, totalPercent*totalPercent)
          qd = lerpPnt(points[i-1], points[i], percent)
          qd2 = rect(qd.loch-thickness, qd.locv-thickness, qd.loch+thickness, qd.locv+thickness)
          
          -- Adds the heads
          if i = 2 and i2 = 2 then
            qd3 = rect(-16, -24, 16, 0) 
            qd3 = rotateRectAroundPoint(qd3, qd, lerp(-20, 20, random(100).float/100))
            ripcordRandom = random(4)
            ripcordqd = rect((ripcordRandom-1)*32, 0, ripcordRandom*32, 24)+rect(1,0,1,0)
            member("layer"&string(layer)).image.copyPixels(member("RipcordGraf").image, qd3, ripcordqd, {#color:colr, #ink:36})
            copyPixelsToEffectColor(gdLayer, layer, qd3, "RipcordGrad", ripcordqd, 0.5, 1)
          end if
          
          -- Draws the stem
          member("layer"&string(layer)).image.copyPixels(member("blob").image, qd2, member("blob").image.rect, {#color:colr, #ink:36})
          ripcordIntensity = lerp(0.25, 0, totalPercent)
          thickness2 = thickness*2
          qd2 = rect(qd.loch-thickness2, qd.locv-thickness2, qd.loch+thickness2, qd.locv+thickness2)
          copyPixelsToEffectColor(gdLayer, layer, qd2, "softBrush1", rect(0, 0, 15, 16), 0.5, ripcordIntensity)
          
          -- Draws the stem layers
          if thickness > 6 then
            thickness = thickness*0.6
            qd2 = rect(qd.loch-thickness, qd.locv-thickness, qd.loch+thickness, qd.locv+thickness)
            member("layer"&string(restrict(layer-1, dmin, dmax))).image.copyPixels(member("blob").image, qd2, member("blob").image.rect, {#color:colr, #ink:36})
            thickness2 = thickness*2
            qd2 = rect(qd.loch-thickness2, qd.locv-thickness2, qd.loch+thickness2, qd.locv+thickness2)
            copyPixelsToEffectColor(gdLayer, (restrict(layer-1, dmin, dmax)), qd2, "softBrush1", rect(0, 0, 15, 16), 0.5, ripcordIntensity*0.7)
          end if
          
          totali = totali + 1
        end repeat
      end if
      
    end repeat
    
  end if
end



on ApplySpudBuds me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      lr = random(3)
    "1":
      lr= 1
    "2":
      lr= 2
    "3":
      lr= 3
    "1:st and 2:nd":
      lr = random(2)
    "2:nd and 3:rd":
      lr = random(2)+1
    otherwise:
      lr = random(3)
  end case
  
  dmax = (lr*10)-1
  dmin = (lr*10)-9
  
  case frondSz of
    "N":
      frondSize = 0
    "S":
      frondSize = 1
    "L":
      frondSize = 2
    otherwise:
      frondSize = 1
  end case
  
  case gdFrondLayer of
    "A":
      frondcl = color(150,0,150)
    "B":
      frondcl = color(0,150,150)
    otherwise:
      frondcl = color(0,0,255)
  end case
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    
    --Plants with larger fronds lean more at the tip
    frondWeight = frondSize
    if frondWeight = 1 then
      frondWeight = lerp(frondWeight, 0, random(100).float/100)*45
    else if frondWeight = 2 then
      frondWeight = lerp(frondWeight, 0, power(random(100).float/100, 2))*45
    end if
    plantAngle = lerp(180-frondWeight, 180+frondWeight, random(100).float/100)
    frondAngle = plantAngle
    --
    midPoint = giveMiddleOfTile(point(q, c))
    midPoint = midPoint + point(lerp(-10, 10, random(100).float/100), lerp(-10, 10, random(100).float/100))
    qd = midPoint
    points = []
    points.add(midPoint)
    RepeatFlag = True
    plantOffset = lerp(5, 10, random(100).float/100)
    plantWiggleCounter = 0
    plantWiggler = 0
    wigglerRandom = random(3)+2
    previousPlantAngle = plantAngle
    wiggledSide = 0
    wiggledSideBuffer = 0
    
    repeat while RepeatFlag then
      qd = qd + degToVecFac2(plantAngle, plantOffset, plantOffset)
      points.add(qd)
      
      --Wiggles the stalk occasionally
      if plantWiggleCounter > 5 and plantWiggler > 0 then
        
        plantWiggler = plantWiggler + 1
        --Stops the plant wiggler
        if plantWiggler > wigglerRandom then
          plantWiggler = 0
          wiggledSide = wiggledSideBuffer
        end if
        --
        
        if wiggledSide = 1 then
          plantAngle = (plantAngle*2 + lerp(120, 90, restrict((plantWiggler/10)+random(100).float/100, 0, 1)))/3
          wiggledSideBuffer = 2
        else if wiggledSide = 2 then
          plantAngle = (plantAngle*2 + lerp(240, 270, restrict((plantWiggler/10)+random(100).float/100, 0, 1)))/3
          wiggledSideBuffer = 1
          
        else
          if previousPlantAngle > 180 then
            plantAngle = (plantAngle*2 + lerp(240, 270, restrict((plantWiggler/10)+random(100).float/100, 0, 1)))/3
            wiggledSideBuffer = 1
          else
            plantAngle = (plantAngle*2 + lerp(120, 90, restrict((plantWiggler/10)+random(100).float/100, 0, 1)))/3
            wiggledSideBuffer = 2
          end if
        end if
        
      else if plantWiggleCounter > 5 and plantWiggler = 0 then
        --Occasionally starts the initiator if its not running already
        plantWiggler = restrict(random(4)-3, 0, 1)
        --
        
        plantAngle = (plantAngle*2 + lerp(160, 200, random(100).float/100))/3
        previousPlantAngle = plantAngle
      else 
        --Initial curve based on frond weight
        plantAngle = (plantAngle*2 + lerp(160, 200, random(100).float/100))/3
        --
      end if
      --
      
      if withinBoundsOfLevel(giveGridPos(qd) + gRenderCameraTilePos) = 0 then
        if skyRootsFix then
          exit 
        end if
        repeatFlag = false
      end if
      
      if afaMvLvlEdit(giveGridPos(qd) + gRenderCameraTilePos, lr) = 1 then
        repeatFlag = false
        rootPoint = points.getLast()
      end if
      plantWiggleCounter = plantWiggleCounter +1
      rootAngle = plantAngle
    end repeat
    
    layer = lerp(dmin, dmax, random(100).float/100).integer
    -- Not allowed to intersect the player
    if layer = 5 then 
      layer = layer + 1
    end if
    -- Obscures the player less
    if layer < 5 then 
      if random(2) = 2 then
        exit
      end if
    end if
    --
    
    totalDist = 0
    totali = 0
    repeat with i2 = 1 to points.count then
      if i2 > 1 then
        totalDist = totalDist + sqrt(power(points[i2].locv - points[i2-1].locv, 2) + power(points[i2].loch - points[i2-1].loch, 2))
      end if
    end repeat
    maxThickness = lerp(2, 2.5+frondSize, random(100).float/100)
    
    if totalDist < 40 then
      exit
    end if
    
    Repeat with i = 1 to points.count then
      if i > 1 then
        dist  = sqrt(power(points[i].locv - points[i-1].locv, 2) + power(points[i].loch - points[i-1].loch, 2))
        
        repeat with i2 = 1 to dist then
          percent = i2.float/dist
          totalPercent = totali.float/totalDist
          thickness = lerp(1, maxThickness, totalPercent)
          qd = lerpPnt(points[i-1], points[i], percent)
          qd2 = rect(qd.loch-thickness, qd.locv-thickness, qd.loch+thickness, qd.locv+thickness)
          
          
          -- Draws the stem
          member("layer"&string(layer)).image.copyPixels(member("blob").image, qd2, member("blob").image.rect, {#color:colr, #ink:36})
          budStemIntensity = lerp(0, 0.25, 1-(totalPercent*totalPercent))
          thickness2 = thickness*2
          qd2 = rect(qd.loch-thickness2, qd.locv-thickness2, qd.loch+thickness2, qd.locv+thickness2)
          copyPixelsToEffectColor(gdLayer, layer, qd2, "softBrush1", rect(0, 0, 15, 16), 0.5, budStemIntensity)
          --
          
          -- Draws the fronds
          if i = 2 and i2 = 2 and frondSize > 0 then
            if totalDist > 220 then
              frondRandomAmp = 2
            else if totalDist > 200 then
              frondRandomAmp = 1
            else if totalDist > 180 then
              frondRandomAmp = 0
            else if totalDist > 160 then
              frondRandomAmp = -1
            else
              if random(2) = 1 then
                frondRandomAmp = -2
              else
                frondRandomAmp = -1
              end if
              
            end if
            
            if frondSize = 2 and random(15) = 15 then
              frondCount = restrict(frondRandomAmp+frondSize+4+random(3), 0, 14)
            else if frondSize = 1 then
              frondCount = restrict(frondRandomAmp+frondSize+(random(2)-1), 0, 7)
            else
              frondCount = restrict(frondRandomAmp+frondSize+1, 0, 7)
            end if
            
            frondArticulation = lerp(-frondSize*10, frondSize*10, random(100).float/100)
            frondCountRepetition = frondCount
            frondFlag = true
            frondSpacing = lerp(5+(restrict(frondCount*3, 0, 20)), 40-restrict(frondCount, 0, 10), random(100).float/100)
            
            if frondCount > 3 then
              frondClamp = 4
            else
              frondClamp = 0
            end if
            
            repeat while frondFlag then
              if frondCount > 0 then
                
                if frondCountRepetition = 1 and frondCount > 3 then
                  frondClamp = 4
                end if
                
                if frondSize = 1 then
                  frondRandom = random(4)+2+frondRandomAmp
                else
                  frondRandom = random(4)+6+frondRandomAmp-frondClamp
                end if
                
                qd3 = rect(-7, -40, 7, 0) 
                qd3 = rotateRectAroundPoint(qd3, qd, ((frondAngle+180+frondSpacing*frondCountRepetition-frondSpacing*(frondCount/2)) mod 360))
                frondqd = rect((frondRandom-1)*15, 0, frondRandom*15, 40)+rect(1,0,1,0)
                member("layer"&string(layer)).image.copyPixels(member("SpudBudFrondGraf").image, qd3, frondqd, {#color:colrFrond, #ink:36})
                copyPixelsToEffectColor(gdFrondLayer, layer, qd3, "SpudBudFrondGrad", frondqd, 0.5, 1-lerp(0, 0.2, random(100).float/100))
                
                frondClamp = 0
                frondCountRepetition = frondCountRepetition-1
                if frondCountRepetition < 1 then
                  frondFlag = false
                end if
                
              else
                frondFlag = false
              end if
            end repeat
            --
            
            
            --Draws the tubers
            rootAmp = restrict(maxThickness-4, -4, 4)
            rootOffset = random(7)
            
            qd4 = rect(-8-rootAmp, -5-rootOffset-rootAmp, 8+rootAmp, 15-rootOffset+rootAmp) 
            qd4 = rotateRectAroundPoint(qd4, rootPoint, rootAngle)
            member("layer"&string(layer)).image.copyPixels(member("blob").image, qd4, member("blob").image.rect, {#color:colr, #ink:36})
            copyPixelsToEffectColor(gdLayer, layer, qd4, "softBrush1", rect(0, 0, 15, 16), 0.5, budStemIntensity*0.5)
            
            if totalDist > 150 then
              
              thickness = thickness+lerp(1, 2, random(100).float/100)
              qd5 = rect(restrict(-8+thickness-rootAmp, -40, 0), restrict(-5-rootOffset+thickness-rootAmp, -40, 0), restrict(8-thickness+rootAmp, 0, 40), restrict(15-rootOffset-thickness+rootAmp, 0, 40))
              qd5 = rotateRectAroundPoint(qd5, rootPoint, rootAngle)
              member("layer"&string(restrict(layer-1, dmin, dmax))).image.copyPixels(member("blob").image, qd5, member("blob").image.rect, {#color:colr, #ink:36})
              
              thickness2 = thickness/4
              qd5 = rect(restrict(-8+thickness2-rootAmp, -40, 0), restrict(-5-rootOffset+thickness2-rootAmp, -40, 0), restrict(8-thickness2+rootAmp, 0, 40), restrict(15-rootOffset-thickness2+rootAmp, 0, 40))
              qd5 = rotateRectAroundPoint(qd5, rootPoint, rootAngle)
              copyPixelsToEffectColor(gdLayer, (restrict(layer-1, dmin, dmax)), qd5, "softBrush1", rect(0, 0, 15, 16), 0.5, budStemIntensity*0.4)
              -- fix the shading for fuck sake
              
            end if
            ---
            
          end if
          
          
          totali = totali + 1
        end repeat
      end if
      
    end repeat
    
  end if
end


on ApplyCrossRoses me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      lr = random(3)
    "1":
      lr= 1
    "2":
      lr= 2
    "3":
      lr= 3
    "1:st and 2:nd":
      lr = random(2)
    "2:nd and 3:rd":
      lr = random(2)+1
    otherwise:
      lr = random(3)
  end case
  
  dmax = (lr*10)-1
  dmin = (lr*10)-9
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    midPoint = giveMiddleOfTile(point(q, c))
    midPoint = midPoint + point(lerp(-10, 10, random(100).float/100), lerp(-10, 10, random(100).float/100))
    qd = midPoint
    plantAngle = lerp(120, 240, random(100).float/100)
    points = []
    points.add(midPoint)
    RepeatFlag = True
    plantOffset = lerp(10, 20, random(100).float/100)
    plantBender = 0
    plantBias = lerp(155, 205, random(100).float/100)
    
    Repeat While RepeatFlag then
      
      qd = qd + degToVecFac2(plantAngle, plantOffset, plantOffset)
      points.add(qd)
      
      if plantBender < 4 and random(2) = 1 then
        plantAngle = ((plantAngle + lerp(-2, 2, random(100).float/100)) mod 360)
        plantOffset = lerp(5, 10, random(100).float/100)
        plantBender = plantBender+1
      else
        
        
        --One in six chance to bend the plant, otherwise two in three chance to bend the plant away from its bias direction, otherwise randomly tracks downwards
        if random(6) = 1 then
          
          if plantBias - plantAngle > 0 then
            plantAngle = ((plantAngle - lerp(80, 110, random(100).float/100)) mod 360)
          else
            plantAngle = ((plantAngle + lerp(80, 110, random(100).float/100)) mod 360)
          end if
          
        else
          --idk
          if random(3) > 1 then
            if plantAngle > plantBias then
              plantAngle = ((plantAngle - lerp(80, 110, random(100).float/100)) mod 360)
            else
              plantAngle = ((plantAngle + lerp(80, 110, random(100).float/100)) mod 360)
            end if
          else
            plantAngle = lerp(160, 200, random(100).float/100)
          end if
          --
        end if
        --
        
        
        plantOffset = lerp(10, 20, random(100).float/100)
        plantBender = 0
      end if
      
      
      if withinBoundsOfLevel(giveGridPos(qd) + gRenderCameraTilePos) = 0 then
        if skyRootsFix then
          exit 
        end if
        repeatFlag = false
      end if
      
      if afaMvLvlEdit(giveGridPos(qd) + gRenderCameraTilePos, lr) = 1 then
        repeatFlag = false
      end if
      
    end repeat
    
    layer = lerp(dmin, dmax, random(100).float/100).integer
    -- Not allowed to intersect the player
    if layer = 5 then 
      layer = layer + 1
    end if
    -- Obscures the player less
    if layer < 5 then 
      if random(3) = 3 then
        exit
      end if
    end if
    --
    totalDist = 0
    totali = 0
    repeat with i2 = 1 to points.count then
      if i2 > 1 then
        totalDist = totalDist + sqrt(power(points[i2].locv - points[i2-1].locv, 2) + power(points[i2].loch - points[i2-1].loch, 2))
      end if
    end repeat
    maxThickness = lerp(2+(gEEprops.effects[r].mtrx[q2][c2]/100), 2+(gEEprops.effects[r].mtrx[q2][c2]/25)+restrict(totalDist/500, 1, 4), random(100).float/100)
    
    if totalDist < 20 then
      exit
    end if
    
    Repeat with i = 1 to points.count then
      if i > 1 then
        dist  = sqrt(power(points[i].locv - points[i-1].locv, 2) + power(points[i].loch - points[i-1].loch, 2))
        
        repeat with i2 = 1 to dist then
          percent = i2.float/dist
          totalPercent = totali.float/totalDist
          thickness = lerp(1+(gEEprops.effects[r].mtrx[q2][c2]/100), maxThickness, totalPercent*totalPercent)
          thickness2 = thickness*2
          thickness3 = lerp(1+(gEEprops.effects[r].mtrx[q2][c2]/100), maxThickness*2, totalPercent)
          effectIntensity = (gEEprops.effects[r].mtrx[q2][c2]/100)/4
          intensity = lerp(0, 0.7-effectIntensity, (1-totalPercent)*(1-totalPercent))
          qd = lerpPnt(points[i-1], points[i], percent)
          qd2 = rect(qd.loch-thickness, qd.locv-thickness, qd.loch+thickness, qd.locv+thickness)
          
          --Thorns
          if i2 = 2 then
            crossThornRandom = random(4)
            thornParentqd = rect(-2.5-thickness3, -2.5-thickness3, 2.5+thickness3, 2.5+thickness3)
            thornParentqd = rotateRectAroundPoint(thornParentqd, point(points[i].loch, points[i].locv), lerp(0, 359, random(100).float/100))
            thornqd = rect((crossThornRandom-1)*15, 0, crossThornRandom*15, 15)+rect(1,0,1,0)
            member("layer"&string(layer)).image.copyPixels(member("CrossRoseThornGraf").image, thornParentqd, thornqd, {#color:colr, #ink:36})
            copyPixelsToEffectColor(gdLayer, layer, thornParentqd, "CrossRoseThornGraf", thornqd, 0.5, intensity)
          end if
          --
          
          -- Draws the stem
          member("layer"&string(layer)).image.copyPixels(member("blob").image, qd2, member("blob").image.rect, {#color:colr, #ink:36})
          qd2 = rect(qd.loch-thickness2, qd.locv-thickness2, qd.loch+thickness2, qd.locv+thickness2)
          copyPixelsToEffectColor(gdLayer, layer, qd2, "softBrush1", rect(0, 0, 15, 16), 0.5, intensity)
          --
          
          -- Adds the heads
          if i = 2 and i2 = 2 then
            qd3 = rect(-15.5, -15.5, 15.5, 15.5) 
            qd3 = rotateRectAroundPoint(qd3, qd, lerp(-20, 20, random(100).float/100))
            crossRosePetalSize = (restrict(totalDist.float / 250, 0, 6).integer)
            crossRoseRandom = restrict(random(3)+crossRosePetalSize, 1, 8)
            crossRoseqd = rect((crossRoseRandom-1)*31, 0, crossRoseRandom*31, 31)+rect(1,0,1,0)
            layerJump = random(2)-1
            member("layer"&string(restrict(layer-layerJump, dmin, dmax))).image.copyPixels(member("CrossRoseGraf").image, qd3, crossRoseqd, {#color:colr, #ink:36})
            copyPixelsToEffectColor(gdLayer, restrict(layer-layerJump, dmin, dmax), qd3, "CrossRoseGrad", crossRoseqd, 0.5, 1-effectIntensity*2)
          end if
          --
          
          
          totali = totali + 1
        end repeat
      end if
    end repeat
    
  end if
end



on ApplyCable me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      lr = random(3)
    "1":
      lr= 1
    "2":
      lr= 2
    "3":
      lr= 3
    "1:st and 2:nd":
      lr = random(2)
    "2:nd and 3:rd":
      lr = random(2)+1
    otherwise:
      lr = random(3)
  end case
  
  dmax = (lr*10)-1
  dmin = (lr*10)-9
  
  case fatOp of
    "1px":
      fatness = 1
    "2px":
      fatness = 2
    "3px":
      fatness = 3
    "random":
      fatness = random(3)
  end case
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    midPoint = giveMiddleOfTile(point(q, c))
    midPoint = midPoint + point(lerp(-10, 10, random(100).float/100), lerp(-10, 10, random(100).float/100))
    horizontalPreferenceness = lerp(0, 45, random(100).float/100)
    tubeInitialAngle = lerp(0+horizontalPreferenceness, 180-horizontalPreferenceness, random(100).float/100)
    tubeOffset = lerp(10, 15, random(100).float/100)
    
    
    --my dogshit implementation  part 1
    qd = midPoint
    RepeatFlag1 = True
    RepeatFlag2 = false
    points1 = []
    points1.add(midPoint)
    Repeat While RepeatFlag1 then
      tubeAngle = tubeInitialAngle
      qd = qd + degToVecFac2(tubeAngle, tubeOffset, tubeOffset)
      points1.add(qd)
      
      
      
      if withinBoundsOfLevel(giveGridPos(qd) + gRenderCameraTilePos) = 0 then
        repeatFlag1 = false
        exit 
      end if
      
      if afaMvLvlEdit(giveGridPos(qd) + gRenderCameraTilePos, lr) = 1 then
        repeatFlag1 = false
        repeatFlag2 = true
      end if
      
    end repeat
    --
    
    --part 2
    qd = midPoint
    points2 = []
    points2.add(midPoint)
    Repeat While RepeatFlag2 then
      tubeAngle = tubeInitialAngle + 180
      qd = qd + degToVecFac2(tubeAngle, tubeOffset, tubeOffset)
      points2.add(qd)
      
      if withinBoundsOfLevel(giveGridPos(qd) + gRenderCameraTilePos) = 0 then
        repeatFlag2 = false
        exit 
      end if
      
      if afaMvLvlEdit(giveGridPos(qd) + gRenderCameraTilePos, lr) = 1 then
        repeatFlag2 = false
      end if
      
    end repeat
    --
    
    layer = lerp(dmin, dmax, random(100).float/100).integer
    thickness = (fatness.float)/2
    dist  = sqrt(power(points1[points1.count].locv - points2[points2.count].locv, 2) + power(points1[points1.count].loch - points2[points2.count].loch, 2))
    intensity = lerp(0.8, 1, random(100).float/100)
    if colrInd = color(0, 255, 0) then
      colrInd2 = color(255, 0, 0)
    else
      colrInd2 = colrInd
    end if
    
    qdh = (sin (tubeInitialAngle+90))*thickness
    qdv = sqrt(thickness*thickness-qdh*qdh)
    
    qd2 = rect(points1[points1.count].locv-qdv, points1[points1.count].loch+qdh, points2[points2.count].locv+qdv, points2[points2.count].loch-qdh)
    
    
    repeat with i2 = 1 to dist*4 then
      percent = i2.float/(dist*4)
      qd = lerpPnt(points1[points1.count], points2[points2.count], percent)
      qd2 = rect(qd.loch-thickness, qd.locv-thickness, qd.loch+thickness, qd.locv+thickness)
      member("layer"&string(layer)).image.copyPixels(member("pxl").image, qd2, member("pxl").image.rect, {#color:colrInd2, #ink:36})
      qd2 = rect(qd.loch-fatness, qd.locv-fatness, qd.loch+fatness, qd.locv+fatness)
      copyPixelsToEffectColor(gdIndLayer, layer, qd2, "softBrush1", rect(0, 0, 15, 16), 0.5, intensity)
    end repeat
    
    
  end if
end


on ApplySmokeWeed me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      lr = random(3)
    "1":
      lr= 1
    "2":
      lr= 2
    "3":
      lr= 3
    "1:st and 2:nd":
      lr = random(2)
    "2:nd and 3:rd":
      lr = random(2)+1
    otherwise:
      lr = random(3)
  end case
  
  dmax = (lr*10)-1
  if lr = 1 then
    dmin = (lr*10)-4
    dmin2 = (lr*10)-9
  else
    dmin = (lr*10)-9
    dmin2 = (lr*10)-9
  end if
  
  
  
  
  
  if (gLEprops.matrix[q2][c2][lr][1]=0)then
    midPoint = giveMiddleOfTile(point(q, c))
    midPoint = midPoint + point(lerp(-5, 5, random(100).float/100), lerp(-10, 10, random(100).float/100))
    qd = midPoint
    plantAngle = lerp(120, 240, random(100).float/100)
    initialPlantAngle = plantAngle
    points = []
    points.add(midPoint)
    RepeatFlag = True
    plantOffset = lerp(5, 10, random(100).float/100)
    behaviourTimer = random(5)+2
    behaviourWaitFlag = true
    
    
    Repeat While RepeatFlag then
      --Starts a decided behaviour if the cooldown has ran out and no behaviour is currently running
      if behaviourTimer < 1 and behaviourWaitFlag then
        behaviourChooser = restrict(random(4)-1, 1, 4)
        behaviourWaitFlag = false
      end if
      --
      
      -- Main behaviour module
      if behaviourWaitFlag then
        qd = qd + degToVecFac2(plantAngle, plantOffset, plantOffset)
        points.add(qd)
        plantAngle = (plantAngle + lerp(155, 205, random(100).float/100))/2
        plantOffset = lerp(5, 10, random(100).float/100)
        behaviourTimer = behaviourTimer - 1
        needsChoice = true
      else
        if behaviourChooser = 3 then -- The Goer Upper
          if needsChoice then
            behaviourPermittedLifetime = 3+random(6)
            if plantAngle > 180 then
              plantBias = (plantAngle + lerp(95, 140, random(100).float/100) mod 360)
            else
              plantBias = (plantAngle - lerp(95, 140, random(100).float/100) mod 360)
            end if
          end if
          if behaviourPermittedLifetime < 3+behaviourTimer then
            plantAngle = (plantAngle*(1+behaviourPermittedLifetime-behaviourTimer) + lerp(155, 205, random(100).float/100))/(2+behaviourPermittedLifetime-behaviourTimer)
            
          else
            plantAngle = ((plantAngle*3 + plantBias)/4+lerp(-10, 10, random(100).float/100) mod 360)
          end if
          
          plantOffset = lerp(5, 8, random(100).float/100)
          qd = qd + degToVecFac2(plantAngle, plantOffset, plantOffset)
          points.add(qd)
          
          behaviourTimer = behaviourTimer + 1
          
        else if behaviourChooser = 2 then -- The Curler
          if needsChoice then
            behaviourPermittedLifetime = 10+random(4)
            plantBias = plantAngle
            curlBias = random(2)
            curlTightness = 16+random(16)
          end if
          if curlBias = 1 then
            plantAngle = plantAngle - lerp(curlTightness-6,  curlTightness+6, random(100).float/100)
          else
            plantAngle = plantAngle + lerp(curlTightness-6, curlTightness+6, random(100).float/100)
          end if
          plantOffset = lerp(3, 10, random(100).float/100)
          qd = qd + degToVecFac2(plantAngle, plantOffset, plantOffset)
          points.add(qd)
          
          if plantAngle > 340 or plantAngle < 20 then
            if random(2) = 1 then
              behaviourTimer = behaviourTimer + 4
            end if
          end if
          
          behaviourTimer = behaviourTimer + 1
          
        else -- Fuck do i know.
          if needsChoice then
            behaviourPermittedLifetime = 4+random(5)
          end if
          plantAngle = lerp(0, 360, random(100).float/100)
          plantOffset = lerp(0, 0.5, random(100).float/100)
          qd = qd + degToVecFac2(plantAngle, plantOffset, plantOffset)
          points.add(qd)
          
          behaviourTimer = behaviourTimer + 1
          
        end if
        --Stops behaviour and limits incurred debt
        needsChoice = false
        if behaviourTimer > behaviourPermittedLifetime then
          if behaviourChooser = 3 then
            behaviourTimer = restrict(behaviourTimer, 0, 7)
          else if behaviourChooser = 2 then
            behaviourTimer = restrict(behaviourTimer, 0, 10)
          else
            behaviourTimer = restrict(behaviourTimer, 0, 5)
          end if
          
          behaviourChooser = 0
          behaviourWaitFlag = true
        end if
        --
      end if
      --
      
      --"nej idag vansinnig svamp" -Enderzilla
      
      if withinBoundsOfLevel(giveGridPos(qd) + gRenderCameraTilePos) = 0 then
        if skyRootsFix then
          exit 
        end if
        repeatFlag = false
      end if
      
      if afaMvLvlEdit(giveGridPos(qd) + gRenderCameraTilePos, lr) = 1 then
        repeatFlag = false
      end if
    end repeat
    
    layer = lerp(dmin, dmax, random(100).float/100).integer
    
    
    totalDist = 0
    totali = 0
    repeat with i2 = 1 to points.count then
      if i2 > 1 then
        totalDist = totalDist + sqrt(power(points[i2].locv - points[i2-1].locv, 2) + power(points[i2].loch - points[i2-1].loch, 2))
      end if
    end repeat
    
    maxThickness = lerp(15, 25, random(100).float/100)
    bumpFlag = 0
    bumpiness = 0
    lumpRandomness = lerp(5, 20, random(100).float/100)
    lumpRandomness2 = lerp(10, 20, random(100).float/100)
    
    -- At smaller lengths, the effect looks really bad, kills him when that happens :D
    if totalDist < 10 then
      exit
    end if
    --
    
    
    Repeat with i = 1 to points.count then
      if i > 1 then
        dist  = sqrt(power(points[i].locv - points[i-1].locv, 2) + power(points[i].loch - points[i-1].loch, 2))
        
        repeat with i2 = 1 to dist then
          
          -- Bumper Initiator
          if i2 > 5 then
            if random(20) = 1 and bumpFlag = 0 then
              bumpAmplitude = random(6)*2+7
              bumpFlag = 1
            end if
            
            -- If bumper has been initiated, bumps
            if bumpFlag > 0 then
              if bumpAmplitude > (bumpAmplitude-1)/2 then
                bumpiness = restrict(bumpiness +1, 0, 100)
                bumpAmplitude = bumpAmplitude - 1
              else
                bumpiness = restrict(bumpiness -1, 0, 100)
                bumpAmplitude = bumpAmplitude - 1
                if bumpiness = 0 then
                  bumpFlag = 0
                end if
              end if
            end if
            --
            
          else
            bumpiness = 0
          end if
          --
          
          percent = i2.float/dist
          totalPercent = totali.float/totalDist
          thickness = lerp(7, maxThickness+bumpiness, (power((1-4*totalPercent*(1-totalPercent)), 2)+totalPercent*totalPercent)/2)*0.75 + restrict((sin((1-power((4*totalPercent*(1-totalPercent)), 2)).float * restrict((totalDist/65), 0, 50)) * 5)+(sin(totalPercent.float * lumpRandomness) * 5)-(sin(totalPercent.float * lumpRandomness2) * 10), 0, 1000)*0.6
          
          qd = lerpPnt(points[i-1], points[i], percent)
          qd2 = rect(qd.loch-thickness, qd.locv-thickness, qd.loch+thickness, qd.locv+thickness)
          
          plantLayer = restrict(Layer + sin(totalPercent.float * restrict((totalDist/50), 0, 20)) * 3, dmin2, dmax).integer
          
          
          -- Draws the stem
          member("layer"&string(plantLayer)).image.copyPixels(member("smokeWeedStem").image, qd2, member("smokeWeedStem").image.rect, {#color:colr, #ink:36})
          dangleController = 1
          if thickness > 3 then
            qd2 = rect(qd.loch-thickness+1, qd.locv-thickness+1, qd.loch+thickness-1, qd.locv+thickness-1)
            member("layer"&string(restrict(plantLayer-1, dmin2, dmax))).image.copyPixels(member("smokeWeedStem").image, qd2, member("smokeWeedStem").image.rect, {#color:colr, #ink:36})
            dangleController = 2
            if thickness > 7 then
              qd2 = rect(qd.loch-thickness+5, qd.locv-thickness+5, qd.loch+thickness-5, qd.locv+thickness-5)
              member("layer"&string(restrict(plantLayer-2, dmin2, dmax))).image.copyPixels(member("smokeWeedStem").image, qd2, member("smokeWeedStem").image.rect, {#color:colr, #ink:36})
              dangleController = 3
              if thickness > 12 then
                qd2 = rect(qd.loch-thickness+10, qd.locv-thickness+10, qd.loch+thickness-10, qd.locv+thickness-10)
                member("layer"&string(restrict(plantLayer-3, dmin2, dmax))).image.copyPixels(member("smokeWeedStem").image, qd2, member("smokeWeedStem").image.rect, {#color:colr, #ink:36})
                dangleController = 4
              end if
            end if
          end if
          --
          
          --Danglies
          if i2 = 2 then
            
            if i = 2 then --Logic for if they're growing under the exhaust head specifically
              
              rimDanglers = random(4)
              repeat while rimDanglers > 0 
                
                dangleHeight = random(2)
                danglePntOffset = lerp(10, -10, random(100).float/100)
                dangleTilt = ((initialPlantAngle+danglePntOffset*3 mod 360)+lerp(175, 185, random(100).float/100))/2
                exhaustParentingAngle = (-initialPlantAngle+180 mod 360)
                fixedDanglePnt = rotatePntFromOrigo(point(points[i].loch, points[i].locv+10), point(points[i].loch, points[i].locv), exhaustParentingAngle)
                if danglePntOffset < 0 then
                  dangleInverseFixerValue = 180
                else
                  dangleInverseFixerValue = 0
                end if
                dangleLayerModification = random(2)-1
                repeat with dangleHeightIterations = 0 to dangleHeight
                  dangleOffset = lerp(12, 23, random(100).float/100).integer
                  dangleParentqd = rect(-6, -23-(dangleHeightIterations*dangleOffset), 6, 1-(dangleHeightIterations*dangleOffset))
                  dangleParentqd = rotateRectAroundPoint(dangleParentqd, rotatePntFromOrigo(point(fixedDanglePnt.loch, fixedDanglePnt.locv+danglePntOffset), fixedDanglePnt, (exhaustParentingAngle+90+dangleInverseFixerValue mod 360)), dangleTilt)
                  dangleRandom = random(4)+dangleHeightIterations*2
                  dangleqd = rect((12*dangleRandom)-12, 0, 12*dangleRandom, 24)+rect(1,0,1,0)
                  member("layer"&string(restrict(plantLayer-random(dangleController)+1+dangleLayerModification, dmin2, dmax))).image.copyPixels(member("SmokeWeedDangleGraf").image, dangleParentqd, dangleqd, {#color:colr, #ink:36})
                end repeat
                rimDanglers = rimDanglers-1
                
              end repeat
              
            else if random(3) = 1 then --Logic for if they're elsewhere on the stem
              
              dangleHeight = random(2)-1
              dangleTilt = lerp(178, 182, random(100).float/100)
              repeat with dangleHeightIterations = 0 to dangleHeight
                dangleOffset = lerp(12, 23, random(100).float/100).integer
                dangleParentqd = rect(-6, -23-(dangleHeightIterations*dangleOffset), 6, 1-(dangleHeightIterations*dangleOffset))
                dangleParentqd = rotateRectAroundPoint(dangleParentqd, point(points[i].loch, points[i].locv), dangleTilt)
                dangleRandom = random(4)+dangleHeightIterations*4
                dangleqd = rect((12*dangleRandom)-12, 0, 12*dangleRandom, 24)+rect(1,0,1,0)
                member("layer"&string(restrict(plantLayer-random(dangleController)+1, dmin2, dmax))).image.copyPixels(member("SmokeWeedDangleGraf").image, dangleParentqd, dangleqd, {#color:colr, #ink:36})
              end repeat
              
            end if
            
          end if
          --
          
          -- Draws the exhaust
          if i = 2 and i2 = 2 then
            exhaustHeight = 1+random(3)
            exhaustTilt = (initialPlantAngle+180 mod 360)
            repeat with exhaustHeightIterations = 0 to exhaustHeight then
              exhaustTerminator = 0
              if exhaustHeightIterations = 0 then
                exhaustTerminator = 2
              else if exhaustHeightIterations = exhaustHeight then
                exhaustTerminator = 1
              end if
              
              repeat with exhaustLayering = -2 to 3 then
                qd3 = rect(-20, -21-(exhaustHeightIterations*20), 20, 0-(exhaustHeightIterations*20)) 
                qd3 = rotateRectAroundPoint(qd3, qd, exhaustTilt)
                exhaustqd = rect(0+(40*exhaustTerminator), 60-(20*restrict(exhaustLayering, 0, 3)), 40+(40*exhaustTerminator), 81-(20*restrict(exhaustLayering, 0, 3)))+rect(1,0,1,0)
                member("layer"&string(restrict(layer-exhaustLayering-1, 1, dmax))).image.copyPixels(member("SmokeWeedFlowerGraf").image, qd3, exhaustqd, {#ink:36})
              end repeat
              
            end repeat
          end if
          --
          
          totali = totali + 1
        end repeat
      end if
    end repeat
    
    
  end if
end


on ApplyMushroomColony me, q, c
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      layer = random(3)
    "1":
      layer= 1
    "2":
      layer= 2
    "3":
      layer= 3
    "1:st and 2:nd":
      layer = random(2)
    "2:nd and 3:rd":
      layer = random(2)+1
    otherwise:
      layer = random(3)
  end case
  
  dmax = (layer*10)-1
  dmin = (layer*10)-9
  sublayer = dmin+random(7)
  
  if (afaMvLvlEdit(point(q2,c2), layer)=0) then
    if (afaMvLvlEdit(point(q2,c2+1), layer)=1) then
      
      groundPoint = giveMiddleOfTile(point(q, c))
      groundPoint = groundPoint + point(lerp(-10, 10, random(100).float/100), 10)
      friendCount = 2+random(3)
      childCount = (friendCount/(random(2)+1)).integer
      
      repeat with colonycounter = 0 to friendCount
        
        colonyTilt = lerp(-5+(colonyCounter-friendCount/2)*10, 5+(colonyCounter-friendCount/2)*10, random(100).float/100)
        colonyRandom = random(10)
        colonySz = lerp(0.9, 1.5, random(100).float/100)
        qd = rect(-35*colonySz,-56*colonySz,35*colonySz, 4*colonySz)
        qd = rotateRectAroundPoint(qd, groundPoint, colonyTilt)
        if colonycounter < friendCount/2 then
          qd = flipQuadH(qd)
        end if
        
        qd2 = rect((colonyRandom-1)*70, 0, colonyRandom*70, 60)+rect(1,0,1,0)
        layerRandom = random(5)-1
        
        member("layer"&string(restrict(sublayer-2+layerRandom, dmin, dmax))).image.copyPixels(member("MushroomColonyGraf").image, qd, qd2, {#color:colr, #ink:36})
        copyPixelsToEffectColor(gdLayer, restrict(sublayer-2+layerRandom, dmin, dmax), qd, "MushroomColonyGrad", qd2, 0.5, restrict((lerp(0.7, 0.9, random(100).float/100)+colonySz)/2), 0, 1)
      end repeat
      
    end if
  end if
  
  
  
  
  -- the entire implementation goes here
  --need to check for solid terrain on the same layer under the pixel and also that hte pixel is air
  
end if
end
