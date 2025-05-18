global c, dptsL, fogDptsL, gLOprops, gViewRender, keepLooping, gCustomColor, DRFinalImage, DRFogImage, DRDpImage, DRShadowImage, DRRainbowMask, DRFlattenedGradientA, DRFlattenedGradientB, DRFinalDecalImage, gAnyDecals, gDecalColors, gPEcolors, grimeActive, grimeOnGradients, bkgFix

on exitFrame me
  if checkMinimize() then
    _player.appMinimize()
    
  end if
  if checkExit() then
    _player.quit()
  end if
  
  if gViewRender then
    if checkExitRender() then
      _movie.go(9)
    end if
    me.newFrame()
    if keepLooping then
      go the frame
    end if
  else
    repeat while keepLooping
      me.newFrame()
    end repeat
  end if
  
  keepLooping = 1
end

on newFrame me
  sprite(59).locV = c-8
  repeat with q = 1 to 1400 then
    
    layer = 1
    
    getColor = DRFinalImage.getPixel(q-1, c-1)
    if (getColor <> color(255, 255, 255)) then
      if (getColor.green > 7) and (getColor.green < 11) then
        
      else if (getColor = color(0, 11, 0)) then
        -- put "What's this?"
        DRFinalImage.setPixel(q-1, c-1, color(10, 0, 0))
      else
        
        if (getColor = color(255, 255, 255)) then
          layer = 0
        end if
        lowResDepth = dptsL.getPos(DRDpImage.getPixel(q-1, c-1))
        fgDp = fogDptsL.getPos(DRFogImage.getPixel(q-1, c-1))
        fogFac = (255-DRFogImage.getPixel(q-1, c-1).red)/255.0
        fogFac = (fogFac - 0.0275)*(1.0/0.9411)
        rainBowFac = 0
        
        
        -- if member("blackOutImg2").image.getPixel(q-1, c-1) = 0 then
        if fogFac <= 0.2 then
          repeat with dsplc in [point(-2,0), point(0,-2), point(2, 0), point(0,2), point(-1,-1), point(1,-1), point(1, 1), point(-1,1)] then
            otherFogFac = (255-DRFogImage.getPixel(restrict(q-1+dsplc.locH, 0, 1339), restrict(c-1+dsplc.locV, 0, 799)).red)/255.0
            otherFogFac = (otherFogFac - 0.0275)*(1.0/0.9411)
            rainBowFac = rainBowFac + (abs(fogFac-otherFogFac)>0.0333)*(restrict(fogFac - otherFogFac, 0, 1)+1)
            if rainBowFac > 5 then
              exit repeat
            end if
          end repeat
          
          rainBowFac = (rainBowFac>5)
        end if
        -- end if
        
        col = color(0, 0, 0)
        
        transp = false
        
        palCol = 2
        effectColor = 0
        dark = 0
        
        case string(getColor) of
          "color( 255, 0, 0 )":
            palCol = 1
          "color( 0, 255, 0 )":
            palCol = 2
          "color( 0, 0, 255 )":
            palCol = 3
          "color( 255, 0, 255 )":
            palCol = 2
            effectColor = 1
          "color( 0, 255, 255 )":
            palCol = 2
            effectColor = 2
          "color( 255, 150, 255 )":
            palCol = 3
            effectColor = 1
          "color( 150, 255, 255 )":
            palCol = 3
            effectColor = 2
          "color( 150, 0, 0 )":
            palCol = 1
            dark = 1
          "color( 0, 150, 0 )":
            palCol = 2
            dark = 1
          "color( 0, 0, 150 )":
            palCol = 3
            dark = 1
          "color( 150, 0, 150 )":
            palCol = 1
            effectColor = 1
            --dark = 1
          "color( 0, 150, 150 )":
            palCol = 1
            effectColor = 2
            --dark = 1
        end case
        
        if(getColor.green = 255)and(getColor.blue = 150)then
          palCol = 1
          effectColor = 3
        end if
        
        
        --if transp = false then
        col.red = ((palCol-1) * 30) + fgDp
        
        if (DRShadowImage.getPixel(q-1, c-1) <> color( 0, 0, 0 )) then
          col.red = col.red + 90
        end if
        
        greenCol = effectColor
        
        
        if (grimeActive) then
          if (rainBowFac) then
            if (grimeOnGradients) then
              greenCol = greenCol + 4
              me.rainbowifypixel(point(q,c))
            else if (greenCol <> 1) and (greenCol <> 2) and (greenCol <> 3) then
              greenCol = greenCol + 4
              me.rainbowifypixel(point(q,c))
            end if
          else if (DRRainBowMask.getPixel(q-1, c-1) <> color( 0 )) then
            if (grimeOnGradients) then
              greenCol = greenCol + 4
            else if (greenCol <> 1) and (greenCol <> 2) and (greenCol <> 3) then
              greenCol = greenCol + 4
            end if
          end if
        end if
        -- put member("rainBowMask").image.getPixel(q-1, c-1)
        
        
        if (effectColor > 0) then
          if (effectColor = 3) then
            col.blue = getColor.red
          else
            modABEf = [TRUE, FALSE][(effectColor mod 4)]
            if (modABEf) then
              col.blue = 255-DRFlattenedGradientA.getPixel(q-1, c-1).red
            else
              col.blue = 255-DRFlattenedGradientB.getPixel(q-1, c-1).red
            end if
          end if
          if (col.blue >= 255) and (bkgFix) then
            col.blue = 254
          end if
        else
          decalColor = 0
          if(gAnyDecals)then
            dcGet = DRFinalDecalImage.getPixel(q-1, c-1)
            if (dcGet <> color(255, 255, 255))then
              if(dcGet = gPEcolors[1][2])then
                if (grimeActive) then
                  if (grimeOnGradients) then
                    if(doesGreenValueMeanRainbow(greenCol) = 0)then--RAINBOW DECAL COLOR!
                      greenCol = greenCol + 4
                    end if
                  else if (greenCol <> 1) and (greenCol <> 2) and (greenCol <> 3) then
                    if(doesGreenValueMeanRainbow(greenCol) = 0)then--RAINBOW DECAL COLOR!
                      greenCol = greenCol + 4
                    end if
                  end if
                end if
              else
                decalColor = gDecalColors.getPos(dcGet)
                if(decalColor=0)and(gDecalColors.count < 255)then
                  gDecalColors.add(dcGet)
                  decalColor = gDecalColors.count
                end if
                if (bkgFix) and (decalColor < 2) then
                  decalColor = 2
                end if
                col.blue = 256-decalColor
                greenCol = greenCol + 8
              end if
            end if
          end if
          
        end if
        
        col.green = greenCol + (dark*16)
        
        if layer = 0 then
          DRFinalImage.setPixel(q-1, c-1, color(255, 255, 255))
        else
          DRFinalImage.setPixel(q-1, c-1, col)
        end if
        
      end if
      --end if
    end if
  end repeat
  
  c = c + 1
  
  
  if c > 800 then
    c = 1
    keepLooping = 0
  end if
end 

on rainbowifypixel me, pxl
  
  if(pxl.locH < 2)or(pxl.locV < 2)then
    return
  end if
  
  if IsPixelInFinalImageRainbowed(pxl+point(-1, 0)) = 0 then
    currCol = DRFinalImage.getPixel(pxl.locH-1-1, pxl.locV-1)
    DRFinalImage.setPixel(pxl.locH-1-1, pxl.locV-1, color(currCol.red, currCol.green+4, currCol.blue))
  end if
  
  if IsPixelInFinalImageRainbowed(pxl+point(0, -1)) = 0 then
    currCol = DRFinalImage.getPixel(pxl.locH-1, pxl.locV-1-1)
    DRFinalImage.setPixel(pxl.locH-1, pxl.locV-1-1, color(currCol.red, currCol.green+4, currCol.blue))
  end if
  
  --if (pxl.locH >= 0) and (pxl.locV-1 >= 0) and (pxl.locH <= 1400) and (pxl.locV-1 <= 800) then
  DRRainBowMask.setPixel(pxl.locH-1+1, pxl.locV-1, color(0, 0, 0))
  --end if
  --if (pxl.locH-1 >= 0) and (pxl.locV >= 0) and (pxl.locH-1 <= 1400) and (pxl.locV <= 800) then
  DRRainBowMask.setPixel(pxl.locH-1, pxl.locV-1+1, color(0, 0, 0))
  --end if
end

on IsPixelInFinalImageRainbowed(pxl)
  if(pxl.loch < 1)or(pxl.locv < 1)then
    return 0
  else if(DRFinalImage.getPixel(pxl.locH-1, pxl.locV-1) = color(255, 255, 255))then
    return 0
  else
    grn = DRFinalImage.getPixel(pxl.locH-1, pxl.locV-1).green
    return doesGreenValueMeanRainbow(grn)
  end if
  
end

on doesGreenValueMeanRainbow(grn)
  if (grn > 3)and(grn < 8)then
    return 1
  else  if (grn > 11)and(grn < 16)then
    return 1
  else 
    return 0
  end if
end