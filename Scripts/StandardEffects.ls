global vertRepeater, r, gEEprops, solidMtrx, gLEprops, colr, colrDetail, colrInd, gdLayer, gdDetailLayer, gdIndLayer, gLOProps, gLevel, gEffectProps, gRenderCameraTilePos, effectSeed, lrSup, chOp, fatOp, gradAf, effectIn3D, gAnyDecals, gRotOp, slimeFxt, DRDarkSlimeFix, DRWhite, DRPxl, DRPxlRect, effSide, gCustomEffects, gEffects, gLastImported, skyRootsFix, lampColr, lampLayer

on ApplyCustomEffect(me, q, c, effectr, efname)
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  mtrx = effectr.mtrx
  
  -- Find the effect
  cEff = VOID
  if (gCustomEffects.getPos(efname) > 0) then
    repeat with i = 1 to gEffects.count
      iefs = gEffects[i].efs
      repeat with j = 1 to iefs.count
        jef = iefs[j]
        if (jef.nm = efname) then
          cEff = jef
          exit repeat
        end if
      end repeat
      if (cEff <> VOID) then exit repeat
    end repeat
  end if
  
  -- Draw the effect
  if (cEff <> VOID) then
    effGraf = member("previewImprt")
    if (gLastImported <> cEff.nm) then
      member("previewImprt").importFileInto("Effects\" & cEff.nm & ".png")
      effGraf.name = "previewImprt"
      gLastImported = cEff.nm
    end if
    effGraf = effGraf.image
    
    case cEff.tp of
      "standardPlant", "standardHanger", "standardClinger": -- standard plant effect
        -- Get potential layers
        case lrSup of
          "All":
            lsL = [1, 2, 3]
          "1":
            lsL = [1]
          "2":
            lsL = [2]
          "3":
            lsL = [3]
          "1:st and 2:nd":
            lsL = [1, 2]
          "2:nd and 3:rd":
            lsL = [2, 3]
          otherwise:
            lsL = [1, 2, 3]
        end case
        
        -- Get amount
        amount = 17
        if (cEff.findPos("placeAmt") > 0) then
          amount = cEff.placeAmt
        end if
        
        -- Now we place the effect
        repeat with layer in lsL
          solidCheck = solidAfaMv(point(q2,c2+1),layer) 
          if cEff.tp = "standardHanger" then
            solidCheck = solidAfaMv(point(q2,c2-1),layer)
          else if cEff.tp = "standardClinger" then
            solidCheck = solidAfaMv(point(q2-1,c2),layer) + solidAfaMv(point(q2+1,c2),layer)
          end if
          
          if solidMtrx[q2][c2][layer]=0 and solidCheck>=1 then
            repeat with i = 1 to mtrx[q2][c2] * 0.01 * amount then
              pnt = giveGroundPosCustom(q,c,layer, cEff.tp)
              clingerMult = (giveMiddleOfTile(point(q,c))>pnt.locH)
              d = random(9) + ((layer-1)*10)
              
              var = random(cEff.vars)
              if cEff.findPos("strengthAffectVar") then var = random(restrict((cEff.vars*(mtrx[q2][c2]-11+random(21))*0.01).integer, 1, cEff.vars))
              grab = rect(cEff.pxlSz.locH * (var-1), 1, cEff.pxlSz.locH * var, 1+cEff.pxlSz.locV)
              rot = 0
              if cEff.findPos("randRot") then rot = random(cEff.randRot * 2 + 1) - cEff.randRot
              
              sz = (random(41) + 79) / 100.0 -- default range: 0.8 to 1.2 (inclusive)
              if cEff.findPos("szVar") then
                if cEff.szVar[1] = cEff.szVar[2] then
                  sz = cEff.szVar[1]
                else if cEff.findPos("strengthAffectSize") then
                  sz = cEff.szVar[1] * (1.0 - power(mtrx[q2][c2] / 100.0, 0.85)) + cEff.szVar[2] * power(mtrx[q2][c2] / 100.0, 0.85)
                else
                  sz = (random((cEff.szVar[2] * 1000.0 - cEff.szVar[1] * 1000.0)) / 1000.0) + cEff.szVar[1]
                end if
              end if
              
              rot = 0
              if cEff.findPos("rotVar") then
                rot = random(cEff.rotVar * 2 + 1) - cEff.rotVar
              end if
              case cEff.tp of
                "standardHanger":
                  rot = rot + 180
                "standardClinger":
                  if clingerMult = 1 then
                    rot = rot + 90
                  else
                    rot = rot + 270
                  end if
              end case
              
              flp = 0
              if cEff.findPos("randomFlip") then
                if cEff.randomFlip then flp = random(2)-1
              end if
              rootAmt = 5
              if cEff.findPos("rootAmt") then rootAmt = cEff.rootAmt
              
              qd = rotateRectAroundPoint(rect(-(cEff.pxlSz.locH/2.0)*sz, -cEff.pxlSz.locV*sz, (cEff.pxlSz.locH/2.0)*sz, rootAmt), pnt, rot)
              if flp then qd = flipQuadH(qd)
              
              useEffCol = 0
              if cEff.findPos("pickColor") then
                if cEff.pickColor then useEffCol = 1
              end if
              
              if useEffCol then
                member("layer"&string(d)).image.copyPixels(effGraf, qd, grab, {#color:colr, #ink:36})
                if colr <> color(0,255,0) then
                  if cEff.findPos("hasGrad") then
                    if cEff.hasGrad then grab = grab + rect(0, cEff.pxlSz.locV, 0, cEff.pxlSz.locV)
                  end if
                  copyPixelsToEffectColor(gdLayer, d, qd, "previewImprt", grab, 0.5, VOID)
                end if
              else
                member("layer"&string(d)).image.copyPixels(effGraf, qd, grab, {#ink:36})
                if cEff.findPos("forceGrad") then
                  if cEff.forceGrad then
                    grab = grab + rect(0, cEff.pxlSz.locV, 0, cEff.pxlSz.locV)
                    copyPixelsToEffectColor("A", d, qd, "previewImprt", grab, 0.5, VOID)
                    copyPixelsToEffectColor("B", d, qd, "previewImprt", grab, 0.5, VOID)
                  end if
                end if
              end if
            end repeat
          end if
        end repeat
        
      "grower", "hanger", "clinger": -- grower effect and its extended family
        if (random(100) < mtrx[q2][c2]) and (random(3) > 1) then
          
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
          
          -- Figure out grow direction
          case cEff.tp of
            "grower": -- the normal kind.
              growDir = 180
            "hanger": -- growers but they grow upside down.
              growDir = 0
            "clinger": -- growers but they grow from the sides. how fancy!
              side = random(2)-1
              if effSide = "L" then side = 0
              else if effSide = "R" then side = 1
              if side = 1 then growDir = 90
              else growDir = 270
          end case
          
          -- Do we have a tip? If so, do setup
          doingTip = 0
          if cEff.findPos("tipGraf") then
            doingTip = 1
            effGraf = member("previewImprt")
            if gLastImported <> cEff.tipGraf then
              member("previewImprt").importFileInto("Effects\" & cEff.tipGraf & ".png")
              effGraf.name = "previewImprt"
              gLastImported = cEff.tipGraf
            end if
            effGraf = effGraf.image
          end if
          
          -- Set up other variables
          sz = 1.0
          blnd = 1.0
          blnd2 = 1.0
          varBias = 0
          if cEff.findPos("heightAffectVar") > 0 then
            if cEff.heightAffectVar < 0 then
              varBias = 1
            end if
          end if
          mdPnt = giveMiddleOfTile(point(q,c))
          pnt = mdPnt + point(random(21)-11, random(21)-11)
          lastDir = growDir + random(cEff.initRotVar * 2 + 1) - cEff.initRotVar
          
          if cEff.findPos("szChange") then
            sz = cEff.szChange[1]
          end if
          
          quadsToDraw = []
          drawQuad = 0
          
          -- Draw loop: as with every grower, draw from tip to ground (or void)
          repeat while (pnt.locV < gLOprops.size.locV * 20 + 100) and (pnt.locV > -100) and (pnt.locH < gLOprops.size.locH * 20 + 100) and (pnt.locH > -100) then
            if doingTip = 1 then
              vars = cEff.tipVars
              pxlSz = cEff.tipPxlSz
              moveAmt = cEff.tipMoveAmt
            else
              vars = cEff.vars
              pxlSz = cEff.pxlSz
              moveAmt = cEff.segmentMoveAmt
            end if
            
            -- Figure out grow direction and take a step in that direction. The area between the step is the segment.
            dir = growDir + random(cEff.segmentRotVar * 2 + 1) - cEff.segmentRotVar
            dir = lerp(lastDir, dir, cEff.segmentRotPull)
            lastPnt = pnt
            pnt = pnt + degToVec(dir) * moveAmt
            lastDir = dir
            
            -- Set up the quad
            qd = (lastPnt + pnt) / 2.0
            qd = rect(qd, qd) + rect(-pxlSz.locH*sz/2.0,-pxlSz.locV/2.0, pxlSz.locH*sz/2.0, pxlSz.locV/2.0)
            qd = rotateToQuadFix(qd, lookAtpoint(lastPnt, pnt))
            
            flp = 0
            if cEff.findPos("randomFlip") then
              if cEff.randomFlip then flp = random(2)-1
            end if
            if flp then qd = flipQuadH(qd)
            
            -- Figure out variation and effect color
            var = random(vars)
            if cEff.findPos("heightAffectVar") and (doingTip <> 1) then
              varBias = restrict(varBias + cEff.heightAffectVar, 0, 1)
              var = random(restrict((cEff.vars*varBias).integer, 1, cEff.vars))
            end if
            grab = rect(pxlSz.locH*(var-1), 1, pxlSz.locH*var, 1+pxlSz.locV)
            
            useEffCol = 0
            if cEff.findPos("pickColor") then
              if cEff.pickColor then useEffCol = 1
            end if
            
            -- Draw the damn thing
            if skyRootsFix then
              quadToAdd = [qd, effGraf, grab, -1, -1, blnd, blnd2, doingTip]
              
              if useEffCol then
                if colr <> color(0,255,0) then
                  if cEff.findPos("hasGrad") then
                    if cEff.hasGrad then
                      grab = grab + rect(0, pxlSz.locV, 0, pxlSz.locV)
                      quadToAdd[4] = grab
                    end if
                  end if
                  
                  if cEff.findPos("effectFadeOut2") and blnd2 > 0 and doingTip = 0 then
                    qd = (lastPnt + pnt) / 2.0
                    qd = rect(qd, qd) + rect(-pxlSz.locH*sz/1.6,-pxlSz.locV/1.6, pxlSz.locH*sz/1.6, pxlSz.locV/1.6)
                    qd = rotateToQuadFix(qd, lookAtpoint(lastPnt, pnt))
                    if flp then qd = flipQuadH(qd)
                    quadToAdd[5] = qd
                  end if
                end if
              else
                if cEff.findPos("forceGrad") then
                  if cEff.forceGrad then
                    grab = grab + rect(0, pxlSz.locV, 0, pxlSz.locV)
                    quadToAdd[4] = grab
                  end if
                end if
              end if
              
              quadsToDraw.add(quadToAdd)
            else
              if useEffCol then
                member("layer"&string(d)).image.copyPixels(effGraf, qd, grab, {#color:colr, #ink:36})
                if colr <> color(0,255,0) then
                  if cEff.findPos("hasGrad") then
                    if cEff.hasGrad then grab = grab + rect(0, pxlSz.locV, 0, pxlSz.locV)
                  end if
                  copyPixelsToEffectColor(gdLayer, d, qd, "previewImprt", grab, 0.5, blnd)
                  
                  if cEff.findPos("effectFadeOut2") and blnd2 > 0 and doingTip = 0 then
                    qd = (lastPnt + pnt) / 2.0
                    qd = rect(qd, qd) + rect(-pxlSz.locH*sz/1.6,-pxlSz.locV/1.6, pxlSz.locH*sz/1.6, pxlSz.locV/1.6)
                    qd = rotateToQuadFix(qd, lookAtpoint(lastPnt, pnt))
                    if flp then qd = flipQuadH(qd)
                    copyPixelsToEffectColor(gdLayer, d, qd, "softBrush1", member("softBrush1").image.rect, 0.5, blnd2)
                  end if
                end if
              else
                member("layer"&string(d)).image.copyPixels(effGraf, qd, grab, {#ink:36})
                if cEff.findPos("forceGrad") then
                  if cEff.forceGrad then
                    grab = grab + rect(0, pxlSz.locV, 0, pxlSz.locV)
                    copyPixelsToEffectColor("A", d, qd, "previewImprt", grab, 0.5, blnd)
                    copyPixelsToEffectColor("B", d, qd, "previewImprt", grab, 0.5, blnd)
                  end if
                end if
              end if
            end if
            
            
            -- Adjust per-segment variables
            if cEff.findPos("effectFadeOut") then blnd = blnd * cEff.effectFadeOut
            else blnd = blnd * 0.85
            
            if cEff.findPos("effectFadeOut2") then blnd2 = max(0.0, blnd2 - cEff.effectFadeOut2)
            
            if cEff.findPos("szChange") then
              sz = restrict(sz + random(1000)/1000.0 * cEff.szChange[3], min(cEff.szChange[1], cEff.szChange[2]), max(cEff.szChange[1], cEff.szChange[2]))
            end if
            
            -- Switch graphic and reset after tip
            if doingTip = 1 then
              doingTip = 0
              effGraf = member("previewImprt")
              if gLastImported <> cEff.nm then
                member("previewImprt").importFileInto("Effects\" & cEff.nm & ".png")
                effGraf.name = "previewImprt"
                gLastImported = cEff.nm
              end if
              effGraf = effGraf.image
            end if
            
            -- Stop once we hit solid ground
            tlPos = giveGridPos(pnt) + gRenderCameraTilePos
            
            if skyRootsFix and withinBoundsOfLevel(tlPos) = 0 then
              drawQuad = 0
              exit repeat
            end if
            
            if solidAfaMv(tlPos, lr) then
              drawQuad = 1
              exit repeat
            end if
          end repeat
          
          if drawQuad then
            if skyRootsFix then
              repeat with qdd in quadsToDraw
                if useEffCol then
                  member("layer"&string(d)).image.copyPixels(qdd[2], qdd[1], qdd[3], {#color:colr, #ink:36})
                  if colr <> color(0,255,0) then
                    qddg = qdd[3]
                    if cEff.findPos("hasGrad") then
                      if cEff.hasGrad then qddg = qdd[4]
                    end if
                    copyPixelsToEffectColor(gdLayer, d, qdd[1], "previewImprt", qddg, 0.5, qdd[6])
                    
                    if cEff.findPos("effectFadeOut2") and qdd[7] > 0 and qdd[8] = 0 then
                      copyPixelsToEffectColor(gdLayer, d, qdd[5], "softBrush1", member("softBrush1").image.rect, 0.5, qdd[7])
                    end if
                  end if
                else
                  member("layer"&string(d)).image.copyPixels(qdd[2], qdd[1], qdd[3], {#ink:36})
                  if cEff.findPos("forceGrad") then
                    if cEff.forceGrad then
                      copyPixelsToEffectColor("A", d, qdd[1], "previewImprt", qdd[4], 0.5, qdd[6])
                      copyPixelsToEffectColor("B", d, qdd[1], "previewImprt", qdd[4], 0.5, qdd[6])
                    end if
                  end if
                end if
              end repeat
            end if
          end if
          
        end if
        
      "individual", "individualHanger", "individualClinger": -- individual plant effect
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
        
        solidCheck = solidAfaMv(point(q2,c2+1),lr) 
        if cEff.tp = "individualHanger" then
          solidCheck = solidAfaMv(point(q2,c2-1),lr)
        else if cEff.tp = "individualClinger" then
          solidCheck = solidAfaMv(point(q2-1,c2),lr) + solidAfaMv(point(q2+1,c2),lr)
        end if
        
        if solidMtrx[q2][c2][lr]=0 and solidCheck then
          -- Figure out variables
          mdPnt = giveMiddleOfTile(point(q,c))
          pnt = mdPnt + point(random(21)-11, 10)
          if cEff.tp = "individualHanger" then
            pnt = mdPnt + point(random(21)-11, -10)
          else if cEff.tp = "individualClinger" then
            clingerSide = -solidAfaMv(point(q2-1,c2),lr) + solidAfaMv(point(q2+1,c2),lr)
            pnt = mdPnt + point(10*clingerSide, random(21)-11)
          end if
          
          var = random(cEff.vars)
          if cEff.findPos("strengthAffectVar") then var = random(restrict((cEff.vars*(mtrx[q2][c2]-11+random(21))*0.01).integer, 1, cEff.vars))
          grab = rect(cEff.pxlSz.locH*(var-1), 1, cEff.pxlSz.locH*var, 1+cEff.pxlSz.locV)
          
          sz = (random(41) + 79) / 100.0 -- default range: 0.8 to 1.2 (inclusive)
          if cEff.findPos("szVar") then
            if cEff.szVar[1] = cEff.szVar[2] then
              sz = cEff.szVar[1]
            else 
              sz = (random((cEff.szVar[2] * 1000.0 - cEff.szVar[1] * 1000.0)) / 1000.0) + cEff.szVar[1]
            end if
          end if
          
          rot = 0
          if cEff.findPos("rotVar") then
            rot = random(cEff.rotVar * 2 + 1) - cEff.rotVar
          end if
          if cEff.tp = "individualHanger" then
            rot = rot + 180
          else if cEff.tp = "individualClinger" then
            rot = rot + 180 + 90 * clingerSide
          end if
          
          flp = 0
          if cEff.findPos("randomFlip") then
            if cEff.randomFlip then flp = random(2)-1
          end if
          rootAmt = 5
          if cEff.findPos("rootAmt") then rootAmt = cEff.rootAmt
          
          qd = rotateRectAroundPoint(rect(-(cEff.pxlSz.locH/2.0)*sz, -cEff.pxlSz.locV*sz, (cEff.pxlSz.locH/2.0)*sz, rootAmt), pnt, rot)
          if flp then qd = flipQuadH(qd)
          
          -- Draw the thing
          useEffCol = 0
          if cEff.findPos("pickColor") then
            if cEff.pickColor then useEffCol = 1
          end if
          
          if useEffCol then
            member("layer"&string(d)).image.copyPixels(effGraf, qd, grab, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              if cEff.findPos("hasGrad") then
                if cEff.hasGrad then grab = grab + rect(0, cEff.pxlSz.locV, 0, cEff.pxlSz.locV)
              end if
              copyPixelsToEffectColor(gdLayer, d, qd, "previewImprt", grab, 0.5, VOID)
            end if
          else
            member("layer"&string(d)).image.copyPixels(effGraf, qd, grab, {#ink:36})
            if cEff.findPos("forceGrad") then
              if cEff.forceGrad then
                grab = grab + rect(0, cEff.pxlSz.locV, 0, cEff.pxlSz.locV)
                copyPixelsToEffectColor("A", d, qd, "previewImprt", grab, 0.5, VOID)
                copyPixelsToEffectColor("B", d, qd, "previewImprt", grab, 0.5, VOID)
              end if
            end if
          end if
          
        end if
        
      "wall": -- things that get placed on wall
        case lrSup of
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
        
        mdPnt = giveMiddleOfTile(point(q,c))
        amount = 20
        if cEff.findPos("placeAmt") > 0 then
          amount = cEff.placeAmt
        end if
        
        repeat with k = 1 to max(1, (amount * mtrx[q2][c2] / 100.0).integer) then
          -- Figure out where and how big (we need ow big to figure out depth believe it or not)
          pnt = mdPnt + point(random(21)-11, random(21)-11)
          
          sz = (random(41) + 79) / 100.0 -- default range: 0.8 to 1.2 (inclusive)
          if cEff.findPos("szVar") then
            if cEff.szVar[1] = cEff.szVar[2] then
              sz = cEff.szVar[1]
            else if cEff.findPos("strengthAffectSize") then
              x = restrict(mtrx[q2][c2]-11+random(21),1,100)
              sz = cEff.szVar[1] * (1.0 - power(x / 100.0, 0.85)) + cEff.szVar[2] * power(x / 100.0, 0.85)
            else
              sz = (random((cEff.szVar[2] * 1000.0) - (cEff.szVar[1] * 1000.0)+1)-1) / 1000.0 + cEff.szVar[1]
            end if
          end if
          
          -- Figure out depth and if we can actually place it
          canPlace = 0
          d = -1
          lr = 0
          cl = color(255,255,255)
          repeat with dp = dmin to dmax then
            rad = sz/2.0
            repeat with dr in [point(0,0), point(-1,0), point(0,-1), point(0,1), point(1,0)] then
              tempPt = point((pnt.locH + dr.locH*rad).integer, (pnt.locV + dr.locV*rad).integer)
              if (member("layer"&string(dp)).getPixel(tempPt.locH, tempPt.locV) <> color(255,255,255)) then
                canPlace = 1
                cl = member("layer"&string(dp)).getPixel(tempPt.locH, tempPt.locV)
                if (cEff.findPos("can3D")>0) then
                  if cEff.can3D = 1 or (cEff.can3D = 2 and effectIn3D) then
                    d = max(0, dp - 2)
                  else
                    d = dp
                  end if
                else
                  d = dp
                end if
                lr = 1 + (d > 9) + (d > 19)
                exit repeat
              end if
            end repeat
            if canPlace = 1 then exit repeat
          end repeat
          
          if (canPlace=1) and (cEff.findPos("requireSolid") > 0) then
            if cEff.requireSolid = 1 then
              canPlace = solidAfaMv(point(q2,c2),lr)
            end if
          end if
          
          -- Now draw it if we can
          if canPlace = 1 and d > -1 then
            d = restrict(d - 1 + random(2), dmin, dmax)
            
            var = random(cEff.vars)
            if cEff.findPos("strengthAffectVar") then var = random(restrict((cEff.vars*(mtrx[q2][c2]-11+random(21))*0.01).integer, 1, cEff.vars))
            grab = rect(cEff.pxlSz.locH*(var-1), 1, cEff.pxlSz.locH*var, 1+cEff.pxlSz.locV)
            
            rot = 0
            if cEff.findPos("randomRotat") then
              if cEff.randomRotat then rot = random(361) - 1
            end if
            
            flp = 0
            if cEff.findPos("randomFlip") then
              if cEff.randomFlip then flp = random(2)-1
            end if
            
            qd = rect(pnt, pnt) + rect(-(cEff.pxlSz/2.0), cEff.pxlSz/2.0)
            qd = rotateToQuadFix(qd, rot)
            if flp then qd = flipQuadH(qd)
            
            useEffCol = 0
            if cEff.findPos("pickColor") then
              if cEff.pickColor then useEffCol = 1
            end if
            
            if cEff.findPos("outline") then -- outline, if wanted
              if cEff.outline then
                repeat with j in [[point(-1,-1), color(0,0,255)], [point(-0,-1), color(0,0,255)], [point(-1,-0), color(0,0,255)], [point(1,1), color(255,0,0)],[point(0,1), color(255,0,0)],[point(1,0), color(255,0,0)]] then
                  oqd = [qd[1] + j[1], qd[2] + j[1], qd[3] + j[1], qd[4] + j[1]]
                  member("layer"&string(d)).image.copyPixels(effGraf, oqd, grab, {#color:j[2], #ink:36})
                end repeat
              end if
            end if
            
            if useEffCol then -- actually drawing
              member("layer"&string(d)).image.copyPixels(effGraf, qd, grab, {#color:colr, #ink:36})
              if colr <> color(0,255,0) then
                if cEff.findPos("hasGrad") then
                  if cEff.hasGrad then grab = grab + rect(0, cEff.pxlSz.locV, 0, cEff.pxlSz.locV)
                end if
                copyPixelsToEffectColor(gdLayer, d, qd, "previewImprt", grab, 0.5, VOID)
              end if
            else
              member("layer"&string(d)).image.copyPixels(effGraf, qd, grab, {#ink:36})
              if cEff.findPos("forceGrad") then
                if cEff.forceGrad then
                  grab = grab + rect(0, cEff.pxlSz.locV, 0, cEff.pxlSz.locV)
                  copyPixelsToEffectColor("A", d, qd, "previewImprt", grab, 0.5, VOID)
                  copyPixelsToEffectColor("B", d, qd, "previewImprt", grab, 0.5, VOID)
                end if
              end if
            end if
          end if
        end repeat
        
      "texture": --things that add textures to the wall
        
        layerImages = []
        layerImagesA = []
        layerImagesB = []
        
        repeat with dldld = 0 to 29 then
          layerImages.add(member("layer"&string(dldld)).image)
          layerImagesA.add(member("gradientA"&string(dldld)).image)
          layerImagesB.add(member("gradientB"&string(dldld)).image)
        end repeat
        
        case lrSup of
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
        
        clrMask = 2
        
        if (cEff.findPos("clrMask")) then -- masking to specific colours, ie 'only apply this to green pixels'
          clrMask = cEff.clrMask
        end if
        
        maskRed = (bitAnd(clrMask, 1) = 1)
        maskGreen = (bitAnd(clrMask, 2) = 2)
        maskBlue = (bitAnd(clrMask, 4) = 4)
        maskEffA = (bitAnd(clrMask, 8) = 8)
        maskEffB = (bitAnd(clrMask, 16) = 16)
        
        bleed = 0
        
        useEffCol = 0
        if cEff.findPos("pickColor") then
          if cEff.pickColor then useEffCol = 1
        end if
        
        if (cEff.findPos("bleed")) then -- 'bleed' being if the texture can apply through layers
          bleed = cEff.bleed
        end if
        
        placeAmt = 20
        if (cEff.findPos("placeAmt")) then
          placeAmt = cEff.placeAmt
        end if
        
        affop = 0.05
        if (cEff.findPos("affectOpenAreas")) then
          affop = cEff.affectOpenAreas
        end if
        
        requireSolid = 0
        if (cEff.findPos("requireSolid")) then
          requireSolid = cEff.requireSolid
        end if
        
        fc = affop + (1.0-affop)* (1-((1-solidAfaMv(point(q2,c2), 3)) * requireSolid))
        
        repeat with dt = 1 to 30
          lr = 30-dt
          if (lr = 9) or (lr = 19) then
            lraddc = 1+(dt>9)+(dt>19)
            sld = (1-((1-solidMtrx[q2][c2][lraddc]) * requireSolid))
            fc = affop + (1.0 - affop) * (1-((1-solidAfaMv(point(q2,c2), lraddc)) * requireSolid))
          end if
          
          deepEffect = 0
          if (lr = 0) or (lr = 10) or (lr = 20) or (sld = 0) then
            deepEffect = 1
          end if
          
          effSt = mtrx[q2][c2]
          
          placeCount = effSt * (0.2 + (0.8 * deepEffect)) * 0.01 * placeAmt * fc
          
          if (lr >= dmin) and (lr <= dmax) then
            repeat with placed = 1 to placeCount then
              pnt = giveMiddleOfTile(point(q,c)) + point(random(21)-11, random(21)-11)
              
              if deepEffect then
                pnt = (point(q-1, c-1)*20)+point(random(20), random(20))
              else
                if random(2)=1 then
                  pnt = (point(q-1, c-1)*20)+point(1 + 19*(random(2)-1), random(20))
                else 
                  pnt = (point(q-1, c-1)*20)+point(random(20), 1 + 19*(random(2)-1))
                end if
              end if
              
              var = random(cEff.vars)
              
              if (cEff.findPos("strengthAffectVar")) then
                if cEff.strengthAffectVar then
                  var = random(restrict((cEff.vars*(effSt-11+random(21))*0.01).integer, 1, cEff.vars))
                end if
              end if
              
              repeat with lch = 0 to (cEff.pxlSz.locH - 1) then
                repeat with lcv = 0 to (cEff.pxlSz.locV - 1) then
                  gtCl = effGraf.getPixel(lch + (var - 1) * cEff.pxlSz.locH, lcv + 1)
                  if (gtCl <> DRWhite) then
                    repeat with lr2 = lr to restrict(lr + bleed, dmin, 29) then
                      
                      layerlr = layerImages[lr2 + 1]
                      layerlrA = layerImagesA[lr2 + 1]
                      layerlrB = layerImagesB[lr2 + 1]
                      layerlrAB = [layerlrA, layerlrB]
                      
                      lrClr = layerlr.getPixel(pnt.locH - (cEff.pxlSz.locH / 2) + lch, pnt.locV - (cEff.pxlSz.locV / 2) + lcv)
                      
                      if doesColorFitMask(lrClr, maskRed, maskGreen, maskBlue, maskEffA, maskEffB) then
                        if useEffCol then
                          layerlr.setPixel(pnt.locH - (cEff.pxlSz.locH / 2) + lch, pnt.locV - (cEff.pxlSz.locV / 2) + lcv, colr)
                          if (cEff.findPos("hasGrad")) then
                            if cEff.hasGrad then
                              gradClr = effGraf.getPixel(lch + (var - 1) * cEff.pxlSz.locH, lcv + 1 + cEff.pxlSz.locV)
                              if (gdLayer <> "C") then
                                layerlrAB[(gdLayer = "A") + (gdLayer = "B") * 2].setPixel(pnt.locH - (cEff.pxlSz.locH / 2) + lch, pnt.locV - (cEff.pxlSz.locV / 2) + lcv, gradClr)
                              end if
                            end if
                          end if
                        else
                          layerlr.setPixel(pnt.locH - (cEff.pxlSz.locH / 2) + lch, pnt.locV - (cEff.pxlSz.locV / 2) + lcv, gtCl)
                          
                          if (cEff.findPos("forceGrad")) then
                            if (cEff.forceGrad) then
                              gradClr = effGraf.getPixel(lch + (var - 1) * cEff.pxlSz.locH, lcv + 1 + cEff.pxlSz.locV)
                              layerlrA.setPixel(pnt.locH - (cEff.pxlSz.locH / 2) + lch, pnt.locV - (cEff.pxlSz.locV / 2) + lcv, gradClr)
                              layerlrB.setPixel(pnt.locH - (cEff.pxlSz.locH / 2) + lch, pnt.locV - (cEff.pxlSz.locV / 2) + lcv, gradClr)
                            end if
                          end if
                        end if
                      end if
                    end repeat
                  end if
                end repeat
              end repeat
            end repeat
          end if
        end repeat
    end case
  end if
end

on giveGroundPosCustom q, c, l, t
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  mdPnt = giveMiddleOfTile(point(q,c))
  pnt = mdPnt
  case t of
    "standardPlant":
      pnt = mdPnt + point(-11+random(21), 10)
      if (gLEprops.matrix[q2][c2][l][1]=3) then
        pnt.locV = pnt.locv - (pnt.locH-mdPnt.locH) - 5
      else if (gLEprops.matrix[q2][c2][l][1]=2) then
        pnt.locV = pnt.locv - (mdPnt.locH-pnt.locH) - 5
      end if
      
    "standardHanger":
      pnt = mdPnt - point(-11+random(21), 10)
      if (gLEprops.matrix[q2][c2][l][1]=4) then
        pnt.locV = pnt.locv + (pnt.locH-mdPnt.locH) + 5
      else if (gLEprops.matrix[q2][c2][l][1]=5) then
        pnt.locV = pnt.locv + (mdPnt.locH-pnt.locH) + 5
      end if
      
    "standardClinger":
      case effSide of
        "L":
          side = 1
          pnt = mdPnt - point(10, -11+random(21))
        "R":
          side = 2
          pnt = mdPnt + point(10, -11+random(21))
        otherwise:
          side = random(2)
          pnt = mdPnt + point(10 * ((side - 1) * 2 - 1), -11+random(21))
      end case
      
      if (gLEprops.matrix[q2][c2][l][1]=(5-side)) then
        pnt.locH = pnt.locH + ((pnt.locV-mdPnt.locV) + 5) * ((side - 1) * 2 - 1)
      else if (gLEprops.matrix[q2][c2][l][1]=(4+side)) then
        pnt.locH = pnt.locH + ((mdPnt.locV-pnt.locV) + 5) * ((side - 1) * 2 - 1)
      end if
      
  end case
  return pnt
end

on doesColorFitMask clr, maskRed, maskGreen, maskBlue, maskEffA, maskEffB -- if a color fits the mask specified
  if maskRed and clr = color(255, 0, 0) then
    return true
  end if
  
  if maskGreen and clr = color(0, 255, 0) then
    return true
  end if
  
  if maskBlue and clr = color(0, 0, 255) then
    return true
  end if
  
  if maskEffA then
    if maskRed and clr = color(150, 0, 150) then
      return true
    end if
    if maskGreen and clr = color(255, 0, 255) then
      return true
    end if
    if maskBlue and clr = color(255, 150, 255) then
      return true
    end if
  end if
  
  if maskEffB then
    if maskRed and clr = color(0, 150, 150) then
      return true
    end if
    if maskGreen and clr = color(0, 255, 255) then
      return true
    end if
    if maskBlue and clr = color(150, 255, 255) then
      return true
    end if
  end if
  
  return false
end



on applyStandardErosion me, q, c, eftc, tp, effectr
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  affop = effectr.affectOpenAreas
  fc = affop + (1.0-affop)* (     solidAfaMv(point(q2,c2), 3)   )
  
  repeat with d = 1 to 30
    lr = 30-d
    case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
      "All":
        --lrb = lr
        dmin = 0
        dmax = 29
      "1":
        --lrb = restrict(lr, 0, 9)
        dmin = 0
        dmax = 9
      "2":
        --lrb = restrict(lr, 10, 19)
        dmin = 10
        dmax = 19
      "3":
        --lrb = restrict(lr, 20, 29)
        dmin = 20
        dmax = 29
      "1:st and 2:nd":
        --lrb = restrict(lr, 0, 19)
        dmin = 0
        dmax = 19
      "2:nd and 3:rd":
        --lrb = restrict(lr, 10, 29)
        dmin = 10
        dmax = 29
      otherwise:
        --lrb = lr
        dmin = 0
        dmax = 29
    end case
    if (lr = 9)or(lr=19) then
      lraddc = 1+(d>9)+(d>19)
      sld = (solidMtrx[q2][c2][lraddc])
      fc = affop + (1.0-affop)* ( solidAfaMv(point(q2,c2), lraddc) )
    end if
    deepEffect = 0
    
    if (lr = 0)or(lr=10)or(lr=20)or(sld=0)then
      deepEffect = 1
    end if
    mtrxq2c2 = effectr.mtrx[q2][c2]
    strlr = string(lr)
    layerlr = member("layer" & strlr).image
    galr = member("gradientA" & strlr).image
    gblr = member("gradientB" & strlr).image
    dclr = member("layer" & strlr & "dc").image
    endofloop = mtrxq2c2*(0.2 + (0.8*deepEffect))*0.01*effectr.repeats*fc
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
      
      case tp of
        "Rust",  "Barnacles", "Colored Barnacles", "Clovers":
          pnt = pnt+degToVec(random(360))*4
          if (lr > dmax) or (lr < dmin) then
            cl = DRWhite
            clA = DRWhite
            clB = DRWhite
            clDc = DRWhite
          else
            cl = layerlr.getPixel(pnt)
            clA = galr.getPixel(pnt)
            clB = gblr.getPixel(pnt)
            clDc = dclr.getPixel(pnt)
          end if
        "Erode", "Ultra Super Erode":
          pnt = pnt+degToVec(random(360))*2
          if (layerlr.getPixel(pnt) = DRWhite) or (random(108)=1) then
            cl = "G"
          else
            cl = DRWhite
          end if
          if (layerlr.getPixel(pnt) = DRWhite) then
            cl = "N"
          end if
        "Super Erode":
          pnt = pnt+degToVec(random(360))*2
          if (layerlr.getPixel(pnt) = DRWhite) then
            cl = "G"
          else
            cl = DRWhite
          end if
          if (layerlr.getPixel(pnt) = DRWhite) then
            cl = "N"
          end if
        "Destructive Melt", "Impacts":
          if (lr > dmax) or (lr < dmin) then
            cl = DRWhite
            clA = DRWhite
            clB = DRWhite
            clDc = DRWhite
          else
            cl = layerlr.getPixel(pnt)
            clA = galr.getPixel(pnt)
            clB = gblr.getPixel(pnt)
            clDc = dclr.getPixel(pnt)
          end if
          if(cl = DRWhite)then
            cl = "W"
          end if
          if(clA = DRWhite)then
            clA = "W"
          end if
          if(clB = DRWhite)then
            clB = "W"
          end if
          if(clDc = DRWhite)then
            clDc = "W"
          end if
        otherwise:
          if (lr > dmax) or (lr < dmin) then
            cl = DRWhite
            clA = DRWhite
            clB = DRWhite
            clDc = DRWhite
          else
            cl = layerlr.getPixel(pnt)
            clA = galr.getPixel(pnt)
            clB = gblr.getPixel(pnt)
            clDc = dclr.getPixel(pnt)
          end if
      end case
      case tp of
        "Slime", "SlimeX3":
          if (cl <> DRWhite) then
            ofst = random(2) - 1
            lgt = 3 + random(random(random(6)))
            if (effectIn3D) then
              nwLr = get3DLr(lr)
            else
              case lrSup of
                "All":
                  nwLr = restrict(lr -1 + random(2), 0, 29)
                "1":
                  nwLr = restrict(lr -1 + random(2), 0, 9)
                "2":
                  nwLr = restrict(lr -1 + random(2), 10, 19)
                "3":
                  nwLr = restrict(lr -1 + random(2), 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr -1 + random(2), 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr -1 + random(2), 10, 29)
                otherwise:
                  nwLr = restrict(lr -1 + random(2), 0, 29)
              end case
            end if
            if (nwLr > 29) then
              nwLr = 29
            else if (nwLr < 0) then
              nwLr = 0
            end if
            strnwlr = string(nwLr)
            layernwlr = member("layer" & strnwlr).image
            if (gradAf) then
              ondc = (clDc <> DRWhite)
              ona = (clA <> DRWhite)
              onb = (clB <> DRWhite)
              slmRect = rect(pnt, pnt) + rect(0 + ofst, 0, 1 + ofst, lgt)
              layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
              if (ondc) then
                dcnwlr = member("layer" & strnwlr & "dc").image
                dcnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clDc})
              end if
              if (ona) then
                ganwlr = member("gradientA" & strnwlr).image
                ganwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clA})
              end if
              if (onb) then
                gbnwlr = member("gradientB" & strnwlr).image
                gbnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clB})
              end if
              if (random(2) = 1) then
                slmRect = rect(pnt, pnt) + rect(0 + ofst + 1, 1 ,1 + ofst + 1, lgt - 1)
                layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
                if (ondc) then
                  dcnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clDc})
                end if
                if (ona)then
                  ganwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clA})
                end if
                if (onb)then
                  gbnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clB})
                end if
              else
                slmRect = rect(pnt, pnt) + rect(0 + ofst - 1, 1, 1 + ofst - 1, lgt - 1)
                layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
                if (ondc) then
                  dcnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clDc})
                end if
                if (ona) then
                  ganwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clA})
                end if
                if (onb) then
                  gbnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clB})
                end if
              end if
            else if (slimeFxt) then
              slmRect = rect(pnt, pnt) + rect(0 + ofst, 0, 1 + ofst, lgt)
              layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
              ondc = (clDc <> DRWhite)
              if (ondc) then
                dcnwlr = member("layer" & strnwlr & "dc").image
                dcnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clDc})
              end if
              if (random(2) = 1) then
                slmRect = rect(pnt, pnt) + rect(0 + ofst + 1, 1 ,1 + ofst + 1, lgt - 1)
                layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
                if (ondc) then
                  dcnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clDc})
                end if
              else
                slmRect = rect(pnt, pnt) + rect(0 + ofst - 1, 1, 1 + ofst - 1, lgt - 1)
                layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
                if (ondc) then
                  dcnwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:clDc})
                end if
              end if
            else
              slmRect = rect(pnt, pnt) + rect(0 + ofst, 0, 1 + ofst, lgt)
              layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
              if (random(2) = 1) then
                slmRect = rect(pnt, pnt) + rect(0 + ofst + 1, 1 ,1 + ofst + 1, lgt - 1)
                layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
              else
                slmRect = rect(pnt, pnt) + rect(0 + ofst - 1, 1, 1 + ofst - 1, lgt - 1)
                layernwlr.copyPixels(DRPxl, slmRect, DRPxlRect, {#color:cl})
              end if
            end if
          end if
          
        "DecalsOnlySlime":
          if (cl <> DRWhite) and (lr >= dmin) and (lr <= dmax) then
            ofst = random(2)-1
            lgt = 3 + random(random(random(6)))
            ondc = (clDc <> DRWhite)
            onga = (clA <> DRWhite)
            ongb = (clB <> DRWhite)
            if (ondc)then
              dclr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst,0,1+ofst,lgt), DRPxlRect, {#color:clDc})
            end if
            if (onga)then
              galr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst,0,1+ofst,lgt), DRPxlRect, {#color:clA})
            end if
            if (ongb)then
              gblr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst,0,1+ofst,lgt), DRPxlRect, {#color:clB})
            end if
            
            if random(2)=1 then
              if (ondc)then
                dclr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst+1,1,1+ofst+1,lgt-1), DRPxlRect, {#color:clDc})
              end if
              if (onga)then
                galr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst+1,1,1+ofst+1,lgt-1), DRPxlRect, {#color:clA})
              end if
              if (ongb)then
                gblr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst+1,1,1+ofst+1,lgt-1), DRPxlRect, {#color:clB})
              end if
            else
              if (ondc)then
                dclr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst-1,1,1+ofst-1,lgt-1), DRPxlRect, {#color:clDc})
              end if
              if (onga)then
                galr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst-1,1,1+ofst-1,lgt-1), DRPxlRect, {#color:clA})
              end if
              if (ongb)then
                gblr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst-1,1,1+ofst-1,lgt-1), DRPxlRect, {#color:clB})
              end if
            end if
          end if
          
        "Rust":
          if (cl <> DRWhite) then
            ofst = random(2)-1
            if  effectIn3D then
              nwLr = get3DLr(lr)
            else
              -- nwLr = lr
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = lr
                "1":
                  nwLr = restrict(lr, 0, 9)
                "2":
                  nwLr = restrict(lr, 10, 19)
                "3":
                  nwLr = restrict(lr, 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr, 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr, 10, 29)
                otherwise:
                  nwLr = lr
              end case
            end if
            if nwLr > 29 then
              nwLr = 29
            else if nwLr < 0 then
              nwLr = 0
            end if
            strnwlr = string(nwLr)
            rustdot = member("rustDot").image
            member("layer"&strnwlr).image.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:cl, #ink:36})
            if(gradAf)then
              if (clDc <> DRWhite)then
                member("layer"&strnwlr&"dc").image.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:clDc, #ink:36})
              end if
              if (clA <> DRWhite)then
                member("gradientA"&strnwlr).image.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:clA, #ink:36})--comment below
              end if
              if (clB <> DRWhite)then
                member("gradientB"&strnwlr).image.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:clB, #ink:36})--not using 39-darker here because 36 makes things look better
              end if
            end if
          end if
        "Barnacles":
          if (cl <> DRWhite) then
            if  effectIn3D then
              nwLr = get3DLr(lr)
            else
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = restrict(lr -1 + random(2), 0, 29)
                "1":
                  nwLr = restrict(lr -1 + random(2), 0, 9)
                "2":
                  nwLr = restrict(lr -1 + random(2), 10, 19)
                "3":
                  nwLr = restrict(lr -1 + random(2), 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr -1 + random(2), 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr -1 + random(2), 10, 29)
                otherwise:
                  nwLr = restrict(lr -1 + random(2), 0, 29)
              end case
            end if
            if nwLr > 29 then
              nwLr = 29
            else if nwLr < 0 then
              nwLr = 0
            end if
            strnwlr = string(nwLr)
            layernwlr = member("layer" & strnwlr).image
            if random(2)-1 then
              b1 = member("barnacle1").image
              b2 = member("barnacle2").image
              layernwlr.copyPixels(b1, rect(pnt, pnt)+rect(-3,-3,4,4), b1.rect, {#color:cl, #ink:36})
              if(gradAf)then
                if (clDc <> DRWhite)then
                  member("layer"&strnwlr&"dc").image.copyPixels(b1, rect(pnt, pnt)+rect(-3,-3,4,4), b1.rect, {#color:clDc, #ink:36})
                end if
                if (clA <> DRWhite)then
                  member("gradientA"&strnwlr).image.copyPixels(b1, rect(pnt, pnt)+rect(-3,-3,4,4), b1.rect, {#color:clA, #ink:36})
                end if
                if (clB <> DRWhite)then
                  member("gradientB"&strnwlr).image.copyPixels(b1, rect(pnt, pnt)+rect(-3,-3,4,4), b1.rect, {#color:clB, #ink:36})
                end if
              end if
              layernwlr.copyPixels(b2, rect(pnt, pnt)+rect(-2,-2,3,3), b2.rect, {#color:color(255,0,0), #ink:36})
            else
              rustdot = member("rustDot").image
              ofst = random(2)-1
              layernwlr.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:[color(255,0,0),cl][random(2)], #ink:36})
              if(gradAf)then
                if (clDc <> DRWhite)then
                  member("layer"&strnwlr&"dc").image.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:clDc, #ink:36})
                end if
                if (clA <> DRWhite)then
                  member("gradientA"&strnwlr).image.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:clA, #ink:36})
                end if
                if (clB <> DRWhite)then
                  member("gradientB"&strnwlr).image.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:clB, #ink:36})
                end if --same use of 36 and not 39 as rust
              end if
            end if
          end if
          
        "Colored Barnacles":
          if (cl <> DRWhite) then
            if  effectIn3D then
              nwLr = get3DLr(lr)
            else
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = restrict(lr -1 + random(2), 0, 29)
                "1":
                  nwLr = restrict(lr -1 + random(2), 0, 9)
                "2":
                  nwLr = restrict(lr -1 + random(2), 10, 19)
                "3":
                  nwLr = restrict(lr -1 + random(2), 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr -1 + random(2), 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr -1 + random(2), 10, 29)
                otherwise:
                  nwLr = restrict(lr -1 + random(2), 0, 29)
              end case
            end if
            if nwLr > 29 then
              nwLr = 29
            else if nwLr < 0 then
              nwLr = 0
            end if
            strnwlr = string(nwLr)
            layernwlr = member("layer" & strnwlr).image
            if (gdIndLayer = "C") then
              if random(2)-1 then
                b1 = member("barnacle1").image
                b2 = member("barnacle2").image
                layernwlr.copyPixels(b1, rect(pnt, pnt)+rect(-3,-3,4,4), b1.rect, {#color:cl, #ink:36})
                layernwlr.copyPixels(b2, rect(pnt, pnt)+rect(-2,-2,3,3), b2.rect, {#color:color(255,0,0), #ink:36})
              else
                ofst = random(2)-1
                rustdot = member("rustDot").image
                layernwlr.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:[color(255,0,0),cl][random(2)], #ink:36})
              end if
            else
              if random(2)-1 then
                b1 = member("barnacle1").image
                b2 = member("barnacle2").image
                layernwlr.copyPixels(b1, rect(pnt, pnt)+rect(-3,-3,4,4), b1.rect, {#color:cl, #ink:36})
                layernwlr.copyPixels(b2, rect(pnt, pnt)+rect(-2,-2,3,3), b2.rect, {#color:colrInd, #ink:36})
                member("gradient"&gdIndLayer&strnwlr).image.copyPixels(b2, rect(pnt, pnt)+rect(-2,-2,3,3), b2.rect, {#ink:39})
              else
                ofst = random(2)-1
                rustdot = member("rustDot").image
                layernwlr.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:colrInd, #ink:36})
                member("gradient"&gdIndLayer&strnwlr).image.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#ink:39})
              end if
            end if
          end if
          
        "Clovers":
          if (cl <> color(255, 255, 255)) then
            if  effectIn3D then
              nwLr = get3DLr(lr)
            else
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = restrict(lr -1 + random(2), 0, 29)
                "1":
                  nwLr = restrict(lr -1 + random(2), 0, 9)
                "2":
                  nwLr = restrict(lr -1 + random(2), 10, 19)
                "3":
                  nwLr = restrict(lr -1 + random(2), 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr -1 + random(2), 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr -1 + random(2), 10, 29)
                otherwise:
                  nwLr = restrict(lr -1 + random(2), 0, 29)
              end case
            end if
            if nwLr > 29 then
              nwLr = 29
            else if nwLr < 0 then
              nwLr = 0
            end if
            if nwLr <= 9 then
              str = 1
            else
              str = random(2)
            end if
            if str = 1 then
              strnwlr = string(nwLr)
              layernwlr = member("layer"&strnwlr).image
              if (gdLayer = "C") then
                n = [1,1,1,1,1,1,2,1.5][random(8)]
                h1 = -5*n
                h2 = 6*n
                nRect = rotateToQuad(rect(pnt, pnt)+rect(h1,h1,h2,h2), random(360))
                if (random(60) = 1) then
                  LC4 = member("4LCloverGraf").image
                  layernwlr.copyPixels(LC4, nRect, LC4.rect, {#color:[color(255,0,0), color(0,255,0), color(0,0,255)][random(3)], #ink:36})
                else
                  LC3 = member("3LCloverGraf").image
                  layernwlr.copyPixels(LC3, nRect, LC3.rect, {#color:[color(255,0,0), color(0,255,0), color(0,0,255)][random(3)], #ink:36})
                end if
              else
                n = [1,1,1,1,1,1,2,1.5][random(8)]
                h1 = -5*n
                h2 = 6*n
                nRect = rotateToQuad(rect(pnt, pnt)+rect(h1,h1,h2,h2), random(360))
                gradnwlr = member("gradient"&gdLayer&strnwlr).image
                if (random(60) = 1) then
                  LC4 = member("4LCloverGraf").image
                  LCG4 = member("4LCloverGrad").image
                  layernwlr.copyPixels(LC4, nRect, LC4.rect, {#color:colr, #ink:36})
                  gradnwlr.copyPixels(LCG4, nRect, LCG4.rect, {#ink:39})
                else
                  LC3 = member("3LCloverGraf").image
                  LCG3 = member("3LCloverGrad").image
                  layernwlr.copyPixels(LC3, nRect, LC3.rect, {#color:colr, #ink:36})
                  gradnwlr.copyPixels(LCG3, nRect, LCG3.rect, {#ink:39})
                end if
              end if
            end if
          end if
          
        "Erode":
          if (cl <> DRWhite) then
            if(random(6)>1)then
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = lr
                "1":
                  nwLr = restrict(lr, 0, 9)
                "2":
                  nwLr = restrict(lr, 10, 19)
                "3":
                  nwLr = restrict(lr, 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr, 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr, 10, 29)
                otherwise:
                  nwLr = lr
              end case
            else
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = restrict(lr + 1, 0, 29)
                "1":
                  nwLr = restrict(lr + 1, 0, 9)
                "2":
                  nwLr = restrict(lr + 1, 10, 19)
                "3":
                  nwLr = restrict(lr + 1, 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr + 1, 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr + 1, 10, 29)
                otherwise:
                  nwLr = restrict(lr + 1, 0, 29)
              end case
            end if
            if nwLr > 29 then
              nwLr = 29
            else if nwLr < 0 then
              nwLr = 0
            end if
            layernwlr = member("layer"&string(nwLr)).image
            rustdot = member("rustDot").image
            repeat with a = 1 to 6
              pnt = pnt + point(-3+random(5), -3+random(5))
              ofst = random(2)-1
              layernwlr.copyPixels(rustdot, rect(pnt, pnt)+rect(-2+ofst,-2,2+ofst,2), rustdot.rect, {#color:DRWhite, #ink:36})
            end repeat
          end if
          
        "Sand":
          if (cl <> DRWhite) then
            if (random(6) > 1) then
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = lr
                "1":
                  nwLr = restrict(lr, 0, 9)
                "2":
                  nwLr = restrict(lr, 10, 19)
                "3":
                  nwLr = restrict(lr, 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr, 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr, 10, 29)
                otherwise:
                  nwLr = lr
              end case
            else
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = restrict(lr + 1, 0, 29)
                "1":
                  nwLr = restrict(lr + 1, 0, 9)
                "2":
                  nwLr = restrict(lr + 1, 10, 19)
                "3":
                  nwLr = restrict(lr + 1, 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr + 1, 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr + 1, 10, 29)
                otherwise:
                  nwLr = restrict(lr + 1, 0, 29)
              end case
            end if
            if (nwLr > 29) then
              nwLr = 29
            else if (nwLr < 0) then
              nwLr = 0
            end if
            strnwlr = string(nwLr)
            layernwlr = member("layer" & strnwlr).image
            if (gdIndLayer = "A") then
              Cgrad = 1
              ganwlr = member("gradientA" & strnwlr).image
            else if (gdIndLayer = "B") then
              Cgrad = 2
              gbnwlr = member("gradientB" & strnwlr).image
            else
              Cgrad = 0
              redC = (cl = color(255, 0, 0))
              greenC = (cl = color(0, 255, 0))
              blueC = (cl = color(0, 0, 255))
            end if
            repeat with a = 1 to 6
              pnt = pnt + point(random(5) - 3, random(5) - 3)
              ofst = random(2) -- can't remove, would change rng
              prectsn = rect(pnt, pnt) + rect(-0.5, -0.5, 0.5, 0.5)
              if (Cgrad = 0) then
                if (redC) then
                  layernwlr.copyPixels(DRPxl, prectsn, DRPxlRect, {#color:[color(0, 255, 0), color(0, 0, 255), color(0, 150, 0), color(0, 0, 150)][random(4)], #ink:36})
                else if (greenC) then
                  layernwlr.copyPixels(DRPxl, prectsn, DRPxlRect, {#color:[color(255, 0, 0), color(0, 0, 255), color(150, 0, 0), color(0, 0, 150)][random(4)], #ink:36})
                else if (blueC) then
                  layernwlr.copyPixels(DRPxl, prectsn, DRPxlRect, {#color:[color(255, 0, 0), color(0, 255, 0), color(150, 0, 0), color(0, 150, 0)][random(4)], #ink:36})
                else
                  layernwlr.copyPixels(DRPxl, prectsn, DRPxlRect, {#color:[color(255, 0, 0), color(0, 255, 0), color(0, 0, 255), color(150, 0, 0), color(0, 150, 0), color(0, 0, 150)][random(6)], #ink:36})
                end if
              else if (Cgrad = 1) then
                layernwlr.copyPixels(DRPxl, prectsn, DRPxlRect, {#color:[color(255, 0, 255), color(150, 0, 150)][random(2)], #ink:36})
                ganwlr.copyPixels(DRPxl, prectsn, DRPxlRect, {#ink:39})
              else
                layernwlr.copyPixels(DRPxl, prectsn, DRPxlRect, {#color:[color(0, 255, 255), color(0, 150, 150)][random(2)], #ink:36})
                gbnwlr.copyPixels(DRPxl, prectsn, DRPxlRect, {#ink:39})
              end if
            end repeat
          end if
          
        "Super Erode":
          if (cl <> DRWhite) then
            if(random(40 + 4 * lr * (lr > 19))>1)then
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = lr
                "1":
                  nwLr = restrict(lr, 0, 9)
                "2":
                  nwLr = restrict(lr, 10, 19)
                "3":
                  nwLr = restrict(lr, 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr, 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr, 10, 29)
                otherwise:
                  nwLr = lr
              end case
            else
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = restrict(lr -2 + random(3), 0, 29)
                "1":
                  nwLr = restrict(lr -2 + random(3), 0, 9)
                "2":
                  nwLr = restrict(lr -2 + random(3), 10, 19)
                "3":
                  nwLr = restrict(lr -2 + random(3), 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr -2 + random(3), 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr -2 + random(3), 10, 29)
                otherwise:
                  nwLr = restrict(lr -2 + random(3), 0, 29)
              end case
            end if
            if nwLr > 29 then
              nwLr = 29
            else if nwLr < 0 then
              nwLr = 0
            end if
            layernwlr = member("layer"&string(nwLr)).image
            ermask = member("SuperErodeMask").image
            repeat with a = 1 to 6
              pnt = pnt + point(-4+random(7), -4+random(7))
              layernwlr.copyPixels(ermask, rect(pnt, pnt)+rect(-4, -4, 4, 4), ermask.rect, {#color:DRWhite, #ink:36})
            end repeat
          end if
          
        "Ultra Super Erode":
          if(random(40 + 4 * lr * (lr > 19))>1)then
            case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
              "All":
                nwLr = lr
              "1":
                nwLr = restrict(lr, 0, 9)
              "2":
                nwLr = restrict(lr, 10, 19)
              "3":
                nwLr = restrict(lr, 20, 29)
              "1:st and 2:nd":
                nwLr = restrict(lr, 0, 19)
              "2:nd and 3:rd":
                nwLr = restrict(lr, 10, 29)
              otherwise:
                nwLr = lr
            end case
          else
            case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
              "All":
                nwLr = restrict(lr -2 + random(3), 0, 29)
              "1":
                nwLr = restrict(lr -2 + random(3), 0, 9)
              "2":
                nwLr = restrict(lr -2 + random(3), 10, 19)
              "3":
                nwLr = restrict(lr -2 + random(3), 20, 29)
              "1:st and 2:nd":
                nwLr = restrict(lr -2 + random(3), 0, 19)
              "2:nd and 3:rd":
                nwLr = restrict(lr -2 + random(3), 10, 29)
              otherwise:
                nwLr = restrict(lr -2 + random(3), 0, 29)
            end case
          end if
          if nwLr > 29 then
            nwLr = 29
          else if nwLr < 0 then
            nwLr = 0
          end if
          blob = member("Blob").image
          layernwlr = member("layer"&string(nwLr)).image
          repeat with a = 1 to 6 then
            pnt = pnt + point(-4+random(7), -4+random(7))
            rctdel = rect(pnt, pnt)+rect(-8, -8, 8, 8)
            layernwlr.copyPixels(blob, rctdel, blob.rect, {#color:DRWhite, #ink:36})
            layernwlr.copyPixels(blob, rctdel, blob.rect, {#color:DRWhite, #ink:36})
          end repeat
          
        "Melt":
          if (cl <> DRWhite) and (lr >= dmin) and (lr <= dmax) then
            cp = image(4,4,32)
            rct = rect(pnt,pnt)+rect(-2,-2,2,2)
            cp.copyPixels(layerlr, rect(0,0,4,4), rct)
            cp.setPixel(point(0,0), DRWhite)
            cp.setPixel(point(3,0), DRWhite)
            cp.setPixel(point(0,3), DRWhite)
            cp.setPixel(point(3,3), DRWhite)
            layerlr.copyPixels(cp, rct+rect(0,1,0,1), rect(0,0,4,4), {#ink:36})
            member("tst").image = cp
            if (gradAf)then
              cpA = image(4,4,32)
              cpA.copyPixels(galr, rect(0,0,4,4), rct)
              cpA.setPixel(point(0,0), DRWhite)
              cpA.setPixel(point(3,0), DRWhite)
              cpA.setPixel(point(0,3), DRWhite)
              cpA.setPixel(point(3,3), DRWhite)
              galr.copyPixels(cpA, rct+rect(0,1,0,1), rect(0,0,4,4), {#ink:39})
              member("tstGradA").image = cpA
              cpB = image(4,4,32)
              cpB.copyPixels(gblr, rect(0,0,4,4), rct)
              cpB.setPixel(point(0,0), DRWhite)
              cpB.setPixel(point(3,0), DRWhite)
              cpB.setPixel(point(0,3), DRWhite)
              cpB.setPixel(point(3,3), DRWhite)
              gblr.copyPixels(cpB, rct+rect(0,1,0,1), rect(0,0,4,4), {#ink:39})
              member("tstGradB").image = cpB
              cpDc = image(4,4,32)
              cpDc.copyPixels(dclr, rect(0,0,4,4), rct)
              cpDc.setPixel(point(0,0), DRWhite)
              cpDc.setPixel(point(3,0), DRWhite)
              cpDc.setPixel(point(0,3), DRWhite)
              cpDc.setPixel(point(3,3), DRWhite)
              dclr.copyPixels(cpDc, rct+rect(0,1,0,1), rect(0,0,4,4), {#ink:36})
              member("tstDc").image = cpDc
            end if
          end if
          
        "Fat Slime":
          if (cl <> DRWhite) then
            ofst = random(2)-1
            lgt = 3 + random(random(random(6)))
            big = random(3)
            fat = random(2)
            if  effectIn3D then
              nwLr = get3DLr(lr)
            else
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = restrict(lr -1 + random(2), 0, 29)
                "1":
                  nwLr = restrict(lr -1 + random(2), 0, 9)
                "2":
                  nwLr = restrict(lr -1 + random(2), 10, 19)
                "3":
                  nwLr = restrict(lr -1 + random(2), 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr -1 + random(2), 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr -1 + random(2), 10, 29)
                otherwise:
                  nwLr = restrict(lr -1 + random(2), 0, 29)
              end case
            end if
            if nwLr > 29 then
              nwLr = 29
            else if nwLr < 0 then
              nwLr = 0
            end if
            strnwlr = string(nwLr)
            layernwlr = member("layer"&strnwlr).image
            layernwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst,0,big+ofst,fat+lgt), DRPxl.rect, {#color:[cl, cl, cl, cl, cl, cl, cl, color(255, 0, 0), color(0, 255, 0), color(0, 0, 255)][random(10)]})--cl
            if(gradAf)then
              ondc = (clDc <> DRWhite)
              onga = (clA <> DRWhite)
              ongb = (clB <> DRWhite)
              if (ondc)then
                dcnwlr = member("layer"&strnwlr&"dc").image
                dcnwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst,0,big+ofst,fat+lgt), DRPxlRect, {#color:clDc})
              end if
              if (onga)then
                ganwlr = member("gradientA"&strnwlr).image
                ganwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst,0,big+ofst,fat+lgt), DRPxlRect, {#color:clA})
              end if
              if (ongb)then
                gbnwlr = member("gradientB"&strnwlr).image
                gbnwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst,0,big+ofst,fat+lgt), DRPxlRect, {#color:clB})
              end if
            end if
            if random(2)=1 then
              layernwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst+1,1,big+ofst+1,fat+lgt-1), DRPxlRect, {#color:[cl, cl, cl, cl, cl, cl, cl, color(255, 0, 0), color(0, 255, 0), color(0, 0, 255)][random(10)]})--cl
              if(gradAf)then
                if (ondc)then
                  dcnwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst+1,1,big+ofst+1,fat+lgt-1), DRPxlRect, {#color:clDc})
                end if
                if (onga)then
                  ganwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst+1,1,big+ofst+1,fat+lgt-1), DRPxlRect, {#color:clA})
                end if
                if (ongb)then
                  gbnwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst+1,1,big+ofst+1,fat+lgt-1), DRPxlRect, {#color:clB})
                end if
              end if
            else
              layernwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst-1,1,big+ofst-1,fat+lgt-1), DRPxlRect, {#color:[cl, cl, cl, cl, cl, cl, cl, color(255, 0, 0), color(0, 255, 0), color(0, 0, 255)][random(10)]})--cl
              if(gradAf)then
                if (ondc)then
                  dcnwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst-1,1,big+ofst-1,fat+lgt-1), DRPxlRect, {#color:clDc})
                end if
                if (onga)then
                  ganwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst-1,1,big+ofst-1,fat+lgt-1), DRPxlRect, {#color:clA})
                end if
                if (ongb)then
                  gbnwlr.copyPixels(DRPxl, rect(pnt, pnt)+rect(0+ofst-1,1,big+ofst-1,fat+lgt-1), DRPxlRect, {#color:clB})
                end if
              end if
            end if
          end if
          
          
        "Roughen":
          if (lr >= dmin) and (lr <= dmax) then
            if(cl = color(0, 255, 0))then
              roughenImg = member("roughenTexture").image
              var = random(20)
              repeat with lch = 0 to 6
                repeat with lcv = 0 to 6
                  if(layerlr.getPixel(pnt.locH-3+lch, pnt.locV-3+lcv) = color(0, 255, 0))then
                    gtCl = roughenImg.getPixel(lch+(var-1)*7, lcv)
                    if gtCl <> DRWhite then
                      layerlr.setPixel(pnt.locH-3+lch, pnt.locV-3+lcv, gtCl)
                    end if
                  end if
                end repeat
              end repeat
            end if
          end if
          
          
        "Super Melt":
          if (cl <> DRWhite) and (lr >= dmin) and (lr <= dmax) then
            maskImg = member("destructiveMeltMask").image
            pntCal = point(maskImg.width,maskImg.height)/2.0
            cpImg = image(maskImg.width,maskImg.height,32)
            rct = rect(pnt-pntCal, pnt+pntCal)
            cpImg.copyPixels(layerlr, cpImg.rect, rct)
            cpImg.copyPixels(maskImg, cpImg.rect, maskImg.rect, {#ink:36, #color:DRWhite})
            mvDown = random(7)*(mtrxq2c2/100.0)
            if (gradAf) then
              cpAImg = image(maskImg.width,maskImg.height,32)
              cpBImg = image(maskImg.width,maskImg.height,32)
              cpDcImg = image(maskImg.width,maskImg.height,32)
              cpAImg.copyPixels(galr, cpAImg.rect, rct)
              cpAImg.copyPixels(maskImg, cpAImg.rect, maskImg.rect, {#ink:36, #color:DRWhite})
              cpBImg.copyPixels(gblr, cpBImg.rect, rct)
              cpBImg.copyPixels(maskImg, cpBImg.rect, maskImg.rect, {#ink:36, #color:DRWhite})
              cpDcImg.copyPixels(dclr, cpDcImg.rect, rct)
              cpDcImg.copyPixels(maskImg, cpDcImg.rect, maskImg.rect, {#ink:36, #color:DRWhite})
            end if
            if (effectIn3D) then
              nwLr = get3DLr(lr)
            else
              case lrSup of
                "All":
                  nwLr = restrict(lr -1 + random(2), 0, 29)
                "1":
                  nwLr = restrict(lr -1 + random(2), 0, 9)
                "2":
                  nwLr = restrict(lr -1 + random(2), 10, 19)
                "3":
                  nwLr = restrict(lr -1 + random(2), 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr -1 + random(2), 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr -1 + random(2), 10, 29)
                otherwise:
                  nwLr = restrict(lr -1 + random(2), 0, 29)
              end case
            end if
            if((lr > 6)and(nwLr <= 6))or((nwLr > 6)and(lr <= 6))then
              case lrSup of
                "All":
                  nwLr = lr
                "1":
                  nwLr = restrict(lr, 0, 9)
                "2":
                  nwLr = restrict(lr, 10, 19)
                "3":
                  nwLr = restrict(lr, 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr, 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr, 10, 29)
                otherwise:
                  nwLr = lr
              end case
            end if
            if (nwLr > 29) then
              nwLr = 29
            else if (nwLr < 0) then
              nwLr = 0
            end if
            nwRect = rct + rect(0, 0, 0, mvDown)
            strnwlr = string(nwLr)
            member("layer"&strnwlr).image.copyPixels(cpImg, nwRect, cpImg.rect, {#ink:36})
            if (gradAf)then
              member("gradientA"&strnwlr).image.copyPixels(cpAImg, nwRect, cpAImg.rect, {#ink:39})
              member("gradientB"&strnwlr).image.copyPixels(cpBImg, nwRect, cpBImg.rect, {#ink:39})
              member("layer"&strnwlr&"dc").image.copyPixels(cpDcImg, nwRect, cpDcImg.rect, {#ink:36})
            end if
          end if
          
        "Destructive Melt":
          if (cl <> DRWhite) and (lr >= dmin) and (lr <= dmax) then
            maskImg = member("destructiveMeltMask").image
            cpImg = image(maskImg.width,maskImg.height,32)
            rct = rect(pnt-point(maskImg.width,maskImg.height)/2.0, pnt+point(maskImg.width,maskImg.height)/2.0)
            
            cpImg.copyPixels(layerlr, cpImg.rect, rct)
            if (gradAf) then
              cpAImg = image(maskImg.width,maskImg.height,32)
              cpAImg.copyPixels(galr, cpAImg.rect, rct)
              cpBImg = image(maskImg.width,maskImg.height,32)
              cpBImg.copyPixels(gblr, cpBImg.rect, rct)
              cpDcImg = image(maskImg.width,maskImg.height,32)
              cpDcImg.copyPixels(dclr, cpDcImg.rect, rct)
            end if
            pnt = point(-2+random(3), -2+random(3))
            rct = rct + rect(pnt, pnt)
            mvDown = random(7)*(mtrxq2c2/100.0)
            if effectIn3D then
              nwLr = get3DLr(lr)
            else
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = restrict(lr -1 + random(2), 0, 29)
                "1":
                  nwLr = restrict(lr -1 + random(2), 0, 9)
                "2":
                  nwLr = restrict(lr -1 + random(2), 10, 19)
                "3":
                  nwLr = restrict(lr -1 + random(2), 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr -1 + random(2), 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr -1 + random(2), 10, 29)
                otherwise:
                  nwLr = restrict(lr -1 + random(2), 0, 29)
              end case
            end if
            if((lr > 6)and(nwLr <= 6))or((nwLr > 6)and(lr <= 6))then
              case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                "All":
                  nwLr = lr
                "1":
                  nwLr = restrict(lr, 0, 9)
                "2":
                  nwLr = restrict(lr, 10, 19)
                "3":
                  nwLr = restrict(lr, 20, 29)
                "1:st and 2:nd":
                  nwLr = restrict(lr, 0, 19)
                "2:nd and 3:rd":
                  nwLr = restrict(lr, 10, 29)
                otherwise:
                  nwLr = lr
              end case
            end if
            if nwLr > 29 then
              nwLr = 29
            else if nwLr < 0 then
              nwLr = 0
            end if
            strnwlr = string(nwLr)
            destroyImg = member("destructiveMeltDestroy").image
            destroyMask = destroyImg.createMask()
            layerlr.copyPixels(cpImg, rct + rect(0, 0, 0, mvDown), cpImg.rect, {#mask:destroyMask})
            member("layer"&strnwlr).image.copyPixels(cpImg, rct + rect(0, 0, 0, mvDown*0.5), cpImg.rect, {#mask:destroyMask, #ink:36})
            if(cl = "W")then
              layerlr.copyPixels(destroyImg,  rect(rct.left, rct.top, rct.right, rct.bottom+mvDown), destroyImg.rect, {#ink:36, #color: DRWhite})
            end if
            if (gradAf) then
              galr.copyPixels(cpAImg, rct + rect(0, 0, 0, mvDown), cpAImg.rect, {#mask:destroyMask})
              member("gradientA"&strnwlr).image.copyPixels(cpAImg, rct + rect(0, 0, 0, mvDown*0.5), cpAImg.rect, {#mask:destroyMask, #ink:39})
              if(clA = "W")then
                galr.copyPixels(destroyImg, rect(rct.left, rct.top, rct.right, rct.bottom+mvDown), destroyImg.rect, {#ink:36, #color: DRWhite})
              end if
              gblr.copyPixels(cpBImg, rct + rect(0, 0, 0, mvDown), cpBImg.rect, {#mask:destroyMask})
              member("gradientB"&strnwlr).image.copyPixels(cpBImg, rct + rect(0, 0, 0, mvDown*0.5), cpBImg.rect, {#mask:destroyMask, #ink:39})
              if(clB = "W")then
                gblr.copyPixels(destroyImg, rect(rct.left, rct.top, rct.right, rct.bottom+mvDown), destroyImg.rect, {#ink:36, #color: DRWhite})
              end if
              dclr.copyPixels(cpDcImg, rct + rect(0, 0, 0, mvDown), cpDcImg.rect, {#mask:destroyMask})
              member("layer"&strnwlr&"dc").image.copyPixels(cpDcImg, rct + rect(0, 0, 0, mvDown*0.5), cpDcImg.rect, {#mask:destroyMask, #ink:36})
              if(clDc = "W")then
                dclr.copyPixels(destroyImg, rect(rct.left, rct.top, rct.right, rct.bottom+mvDown), destroyImg.rect, {#ink:36, #color: DRWhite})
              end if
            end if
          end if
          
        "Impacts":
          if (lr >= dmin) and (lr <= dmax) then
            chance = random(110)
            if lr <= 9 then
              chance = random(6)
            else if lr <= 19 then
              chance = random(90)
            end if
            if (chance=1) then
              if(cl <> DRWhite)then
                var = random(8)
                repeat with lch = 0 to 19
                  repeat with lcv = 1 to 20
                    if(layerlr.getPixel((pnt.locH-15+lch), (pnt.locV-15+lcv)) <> DRWhite)then
                      repeat with iVar = 1 to 3
                        gtCl = member("Impact"&string(iVar)).image.getPixel(lch+(var-1)*20, lcv)
                        if gtCl <> DRWhite then
                          member("layer"&string(restrict(lr+iVar-1, dmin, dmax))).image.setPixel((pnt.locH-15+lch), (pnt.locV-15+lcv), DRWhite)
                        end if
                      end repeat
                    end if
                  end repeat
                end repeat
              end if
            end if
          end if
      end case
    end repeat
  end repeat
end

on get3DLr(lr)
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
    "All":
      nwLr = restrict(lr -2 + random(3), 0, 29)
    "1":
      nwLr = restrict(lr -2 + random(3), 0, 9)
    "2":
      nwLr = restrict(lr -2 + random(3), 10, 19)
    "3":
      nwLr = restrict(lr -2 + random(3), 20, 29)
    "1:st and 2:nd":
      nwLr = restrict(lr -2 + random(3), 0, 19)
    "2:nd and 3:rd":
      nwLr = restrict(lr -2 + random(3), 10, 29)
    otherwise:
      nwLr = restrict(lr -2 + random(3), 0, 29)
  end case
  if (lr = 6) and (nwLr = 5) then
    nwLr = 6
  else if (lr = 5) and (nwLr = 6) then
    nwLr = 5
  end if
  if (nwLr > 29) then
    return 29
  else if (nwLr < 0) then
    return 0
  end if
  return nwLr
end 


on applyStandardPlant me, q, c, eftc, tp
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  amount = 17
  case tp of
    "Root Grass":
      amount = 12
    "Grass":
      amount = 10
    "Dandelions":
      amount = random(10)
    "Seed Pods":
      amount = random(5)
    "Cacti":
      amount = 3
    "Rain Moss":
      amount = 9
    "rubble":
      amount = 11
    "Colored Rubble":
      amount = 11
    "Horse Tails":
      amount = 1 + random(3)
    "Circuit Plants":
      amount = 2
    "Feather Plants":
      amount = 4
    "Reeds":
      amount = 2
    "Lavenders", "Storm Plants":
      amount = 5
    "Hyacinths":
      amount = 5
    "Seed Grass":
      amount = 5
    "Orb Plants":
      amount = 5
    "Lollipop Mold":
      amount = 5
    "Og Grass":
      amount = 7
  end case
  case lrSup of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
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
  
  -- gradImg = image(10,30,16)
  -- mskImg = image(10,30,16)
  
  repeat with layer in lsL then
    if (solidMtrx[q2][c2][layer]<>1) and (solidAfaMv(point(q2,c2+1), layer)=1) then
      -- mdPnt = giveMiddleOfTile(point(q,c))
      repeat with cntr = 1 to gEEprops.effects[r].mtrx[q2][c2]*0.01*amount then
        pnt = giveGroundPos(q, c, layer)
        lr = random(9) + (layer-1)*10
        
        
        case tp of
          "Grass":
            freeSides = 0
            if (solidAfaMv(point(q2-1,c2+1), layer)=0)then--or(afaMvLvlEdit(point(q,c), layer)=3) then
              --freeSides = freeSides + 1
              amount = amount/2
            end if
            if (solidAfaMv(point(q2+1,c2+1), layer)=0)then--or(afaMvLvlEdit(point(q,c), layer)=2) then
              -- freeSides = freeSides + 1
              
              amount = amount/2
            end if
            
            rct = rect(pnt, pnt) + rect(-10,-20,10, 10)
            
            rnd = random(20)
            
            flp = random(2)-1
            if flp then
              rct = vertFlipRect(rct)
            end if
            
            gtRect = rect((rnd-1)*20, 0, rnd*20, 30)+rect(1,0,1,0)
            member("layer"&string(lr)).image.copyPixels(member("GrassGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              -- pnt = depthPnt(pnt, lr-5)
              rct = rect(pnt, pnt) + rect(-10,-20,10, 10)
              if flp then
                rct = vertFlipRect(rct)
              end if
              
              copyPixelsToEffectColor(gdLayer, lr, rct, "GrassGrad", gtRect, 0.5)
            end if   
            
          "Root Grass":
            freeSides = 0
            if (solidAfaMv(point(q2-1,c2+1), layer)=0)then--or(afaMvLvlEdit(point(q,c), layer)=3) then
              freeSides = freeSides + 1
            end if
            if (solidAfaMv(point(q2+1,c2+1), layer)=0)then--or(afaMvLvlEdit(point(q,c), layer)=2) then
              freeSides = freeSides + 1
            end if
            
            rct = rect(pnt, pnt) + rect(-5,-17,5, 3)
            if (freeSides > 0) or (amount<0.5) then
              rnd = 10+random(5)
            else
              rnd = random(10)
            end if
            
            flp = random(2)-1
            if flp then
              rct = vertFlipRect(rct)
            end if
            
            gtRect = rect((rnd-1)*10, 0, rnd*10, 30)+rect(1,0,1,0)
            member("layer"&string(lr)).image.copyPixels(member("RootGrassGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              rct = rect(pnt, pnt) + rect(-5,-17,5, 3)
              if flp then
                rct = vertFlipRect(rct)
              end if
              copyPixelsToEffectColor(gdLayer, lr, rct, "RootGrassGrad", gtRect, 0.5)
            end if   
            
          "Seed Pods":
            rnd = random(7)
            rct = rect(pnt, pnt) + rect(-10,-77,10, 3)
            flp = random(2)-1
            gtRect = rect((rnd-1)*20, 0, rnd*20, 80)+rect(1,0,1,0)
            if flp then
              rct = vertFlipRect(rct)
            end if
            member("layer"&string(lr)).image.copyPixels(member("SeedPodsGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              rct = rect(pnt, pnt) + rect(-10,-77,10, 3)
              if flp then
                rct = vertFlipRect(rct)
              end if
              copyPixelsToEffectColor(gdLayer, lr, rct, "SeedPodsGrad", gtRect, 0.5)
            end if 
            
          "Dandelions":
            rnd = random(15)
            rct = rect(pnt, pnt) + rect(-6,-28,6, 0)
            flp = random(2)-1
            gtRect = rect((rnd-1)*12, 0, rnd*12, 28)+rect(1,0,1,0)
            if flp then
              rct = vertFlipRect(rct)
            end if
            member("layer"&string(lr)).image.copyPixels(member("dandelionsGraf").image, rct, gtRect, {#color:colr, #ink:36})
            copyPixelsToEffectColor(gdLayer, lr, rct, "dandelionsGrad", gtRect, 0.5)
            
          "Reeds":
            rnd = random(4)
            rndSz = random(30)
            rct = rect(pnt, pnt) + rect(-60, -190 - rndSz * 2, 60, 10)
            flp = random(2) - 1
            gtRect = rect((rnd - 1) * 120, 1, rnd * 120, 201)
            if (flp) then
              rct = vertFlipRect(rct)
            end if
            member("layer" & string(lr)).image.copyPixels(member("reedsGraf2").image, rct, gtRect, {#color:colr, #ink:36})
            if (gdLayer <> "C") then
              member("gradient" & gdLayer & string(lr)).image.copyPixels(member("reedsGrad2").image, rct, gtRect, {#ink:39})
            end if
            
          "Lavenders":
            rnd = random(3)
            rndSz = random(20)
            rct = rect(pnt, pnt) + rect(-4, -103 - rndSz * 2, 4, 3)
            flp = random(2) - 1
            gtRect = rect((rnd - 1) * 8, 1, rnd * 8, 107)
            if (flp) then
              rct = vertFlipRect(rct)
            end if
            member("layer" & string(lr)).image.copyPixels(member("lavendersGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if (colr <> color(0, 255, 0)) then
              copyPixelsToEffectColor(gdLayer, lr, rct, "lavendersGrad", gtRect, 0.5)
            end if
            repeat with cal = 0 to 2
              repeat with rep = -1 to 1
                caler = 0
                if (cal = 2) then
                  caler = 1
                end if
                rand = random(5)
                getRct = rect((rand - 1) * 10, 1 + 10 * caler, rand * 10, 11 + 10 * caler)
                nRect = rect(pnt, pnt) + rect(-5, -105 + cal * 10 - rndSz * 2, 5, -95 + cal * 10 - rndSz * 2)
                newLr = restrict(lr + rep, 0, 29)
                member("layer" & string(newLr)).image.copyPixels(member("lavendersFlowers").image, nRect, getRct, {#color:colr, #ink:36})
                if (colr <> color(0, 255, 0)) then
                  member("gradient" & gdLayer & string(newLr)).image.copyPixels(member("lavendersFlowers").image, nRect, getRct, {#ink:39})
                end if
              end repeat
            end repeat
            
          "Hyacinths":
            rnd = random(15)
            rct = rect(pnt, pnt) + rect(-10,-77,10, 3)
            flp = random(2)-1
            gtRect = rect((rnd-1)*20, 0, rnd*20, 80)+rect(1,0,1,0)
            rct = rotateToQuad(rct, random(50) - 25)
            member("layer"&string(lr)).image.copyPixels(member("hyacinthGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              copyPixelsToEffectColor(gdLayer, lr, rct, "hyacinthGrad", gtRect, 0.5)
            end if   
            
          "Seed Grass":
            rnd = random(15)
            rct = rect(pnt, pnt) + rect(-10,-47,10, 3)
            flp = random(2)-1
            gtRect = rect((rnd-1)*20, 0, rnd*20, 50)+rect(0,1,0,1)
            rct = rotateToQuad(rct, random(50) - 25)
            member("layer"&string(lr)).image.copyPixels(member("seedGrassGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              copyPixelsToEffectColor(gdLayer, lr, rct, "seedGrassGrad", gtRect, 0.5)
            end if    
            
          "Orb Plants":
            rnd = random(15)
            rct = rect(pnt, pnt) + rect(-20,-57,20, 3)
            flp = random(2)-1
            gtRect = rect((rnd-1)*40, 0, rnd*40, 60)+rect(1,0,1,0)
            rct = rotateToQuad(rct, random(50) - 25)
            member("layer"&string(lr)).image.copyPixels(member("orbPlantGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              copyPixelsToEffectColor(gdLayer, lr, rct, "orbPlantGrad", gtRect, 0.5)
            end if    
            
          "Circuit Plants":
            if(random(300) > gEEprops.effects[r].mtrx[q2][c2])then
              rnd = random(restrict((20*(gEEprops.effects[r].mtrx[q2][c2]-11+random(21))*0.01).integer, 1, 16))
              sz = 0.15+0.85*power(gEEprops.effects[r].mtrx[q2][c2]*0.01, 0.85)
              rct = rect(pnt, pnt) + rect(-20*sz,-95*sz,20*sz, 5)
              flp = random(2)-1
              gtRect = rect((rnd-1)*40, 0, rnd*40, 100)+rect(1,0,1,0)
              if flp then
                rct = vertFlipRect(rct)
              end if
              member("layer"&string(lr)).image.copyPixels(member("CircuitPlantGraf").image, rct, gtRect, {#color:colr, #ink:36})
              if(sz < 0.75)then
                member("layer"&string(lr)).image.copyPixels(member("CircuitPlantGraf").image, rct+rect(1,0,1,0), gtRect, {#color:colr, #ink:36})
                member("layer"&string(lr)).image.copyPixels(member("CircuitPlantGraf").image, rct+rect(0,1,0,1), gtRect, {#color:colr, #ink:36})
              end if
              if colr <> color(0,255,0) then
                rct = rect(pnt, pnt) + rect(-20*sz,-95*sz,20*sz, 5)
                if flp then
                  rct = vertFlipRect(rct)
                end if
                copyPixelsToEffectColor(gdLayer, lr, rct, "CircuitPlantGrad", gtRect, 0.5)
              end if 
            end if
          "Storm Plants":
            if(random(300) > gEEprops.effects[r].mtrx[q2][c2])then
              rnd = random(restrict((20*(gEEprops.effects[r].mtrx[q2][c2]-11+random(21))*0.01).integer, 1, 16))
              sz = 0.15+0.85*power(gEEprops.effects[r].mtrx[q2][c2]*0.01, 0.85)
              rct = rect(pnt, pnt) + rect(-20*sz,-95*sz,20*sz, 5)
              flp = random(2)-1
              gtRect = rect((rnd-1)*40, 0, rnd*40, 100)+rect(1,0,1,0)
              if flp then
                rct = vertFlipRect(rct)
              end if
              member("layer"&string(lr)).image.copyPixels(member("StormPlantGraf").image, rct, gtRect, {#color:colr, #ink:36})
              if(sz < 0.75)then
                member("layer"&string(lr)).image.copyPixels(member("StormPlantGraf").image, rct+rect(1,0,1,0), gtRect, {#color:colr, #ink:36})
                member("layer"&string(lr)).image.copyPixels(member("StormPlantGraf").image, rct+rect(0,1,0,1), gtRect, {#color:colr, #ink:36})
              end if
              if colr <> color(0,255,0) then
                rct = rect(pnt, pnt) + rect(-20*sz,-95*sz,20*sz, 5)
                if flp then
                  rct = vertFlipRect(rct)
                end if
                copyPixelsToEffectColor(gdLayer, lr, rct, "StormPlantGrad", gtRect, 0.5)
              end if 
            end if
            
          "Feather Plants":
            if(random(300) > gEEprops.effects[r].mtrx[q2][c2])then
              leanDir = 0
              if(q2 > 1)then
                if(afaMvLvlEdit(point(q2-1,c2), layer)=0)and(afaMvLvlEdit(point(q2-1,c2+1), layer)=1)then
                  leanDir = leanDir + gEEprops.effects[r].mtrx[q2-1][c2]
                else if (afaMvLvlEdit(point(q2-1,c2), layer)=1) then
                  leanDir = leanDir - 90
                end if
              end if
              if(q2 < gLOprops.size.locH-1)then
                if(afaMvLvlEdit(point(q2+1,c2), layer)=0)and(afaMvLvlEdit(point(q2+1,c2+1), layer)=1)then
                  leanDir = leanDir - gEEprops.effects[r].mtrx[q2+1][c2]
                else if (afaMvLvlEdit(point(q2+1,c2), layer)=1) then
                  leanDir = leanDir + 90
                end if
              end if
              
              rnd = random(restrict((20*(gEEprops.effects[r].mtrx[q2][c2]-11+random(21))*0.01).integer, 1, 16))
              sz = 1--0.2+0.8*power(gEEprops.effects[r].mtrx[q2][c2]*0.01, 0.85)
              rct = rect(pnt, pnt) + rect(-20*sz,-90*sz,20*sz, 100*sz)
              gtRect = rect((rnd-1)*40, 0, rnd*40, 190)+rect(1,0,1,0)
              
              rct = rotateToQuad(rct, (65.0*((leanDir - 11 + random(21))/100.0))+0.1)
              
              checkForSolid = (rct[1]+rct[2]+rct[3]+rct[4])/4.0
              if(   member("layer"&string(lr)).image.getPixel(checkForSolid.locH, checkForSolid.locV) <> color(255, 255, 255))then
                
                if(leanDir - 11 + random(21) > 0) then
                  rct = flipQuadH(rct)
                end if
                
                member("layer"&string(lr)).image.copyPixels(member("FeatherPlantGraf").image, rct, gtRect, {#color:colr, #ink:36})
                if colr <> color(0,255,0) then
                  copyPixelsToEffectColor(gdLayer, lr, rct, "FeatherPlantGrad", gtRect, 0.5)
                end if 
              end if
            end if
            
          "Horse Tails":
            rnd = restrict(random(3+(20*gEEprops.effects[r].mtrx[q2][c2]*0.01).integer), 1, 14)
            rct = rect(pnt, pnt) + rect(-10,-48,10, 2)
            flp = random(2)-1
            gtRect = rect((rnd-1)*20, 0, rnd*20, 50)+rect(1,0,1,0)
            if flp then
              rct = vertFlipRect(rct)
            end if
            member("layer"&string(lr)).image.copyPixels(member("HorseTailGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              rct = rect(pnt, pnt) + rect(-10,-48,10, 2)
              if flp then
                rct = vertFlipRect(rct)
              end if
              copyPixelsToEffectColor(gdLayer, lr, rct, "HorseTailGrad", gtRect, 0.5)
            end if 
            
          "Cacti":
            repeat with rep = 1 to random(random(3)) then
              sz = 0.5+(random(gEEprops.effects[r].mtrx[q2][c2]*0.7)*0.01)
              rotat = -45+random(90)
              if (solidAfaMv(point(q2-1,c2+1), layer)=0)or(afaMvLvlEdit(point(q2,c2), layer)=3) then
                rotat = rotat - 10-random(30)
              end if
              if (solidAfaMv(point(q2+1,c2+1), layer)=0)or(afaMvLvlEdit(point(q2,c2), layer)=2) then
                rotat = rotat + 10+random(30)
              end if
              tpPnt = pnt + degToVec(rotat)*15*sz
              
              rct = rotateToQuad( rect((pnt+tpPnt)*0.5,(pnt+tpPnt)*0.5)+rect(-4*sz,-7*sz,4*sz,8*sz) ,lookAtPoint(pnt, tpPnt))
              member("layer"&string(lr)).image.copyPixels(member("bigCircle").image, rct, member("bigCircle").image.rect, {#color:colr, #ink:36})
              if colr <> color(0,255,0) then
                rct = rect(tpPnt,tpPnt)+rect(-9*sz,-6*sz,9*sz,13*sz)+rect(-3,-3,3,3)
                copyPixelsToEffectColor(gdLayer, lr, rct, "softBrush1", member("softBrush1").image.rect, 0.5)
              end if
            end repeat
          "Rubble":
            rct = rect(pnt,pnt)+rect(-3,-3,3,3)+rect(-random(3),-random(3), random(3), random(3))
            rct = rotateToQuad(rct,random(360))
            rubbl = random(4)
            repeat with rep = 1 to 4 then
              if lr+rep-1 > 29 then
                exit repeat
              else
                member("layer"&string(lr+rep-1)).image.copyPixels(member("rubbleGraf"&string(rubbl)).image, rct, member("rubbleGraf"&string(rubbl)).image.rect, {#color:color(0,255,0), #ink:36})
              end if                
            end repeat
            
          "Colored Rubble":
            rct = rect(pnt,pnt)+rect(-3,-3,3,3)+rect(-random(3),-random(3), random(3), random(3))
            rct = rotateToQuad(rct,random(360))
            rubbl = random(4)
            repeat with rep = 1 to 4 then
              if lr+rep-1 > 29 then
                exit repeat
              else
                member("layer"&string(lr+rep-1)).image.copyPixels(member("rubbleGraf"&string(rubbl)).image, rct, member("rubbleGraf"&string(rubbl)).image.rect, {#color:colrInd, #ink:36})
                if (gdIndLayer <> "C") then
                  member("gradient"&gdIndLayer&string(lr+rep-1)).image.copyPixels(member("rubbleGraf"&string(rubbl)).image, rct, member("rubbleGraf"&string(rubbl)).image.rect, {#ink:39})
                end if
              end if                
            end repeat
            
          "Rain Moss":
            pnt = pnt + degToVec(random(360)) * random(random(100)) * 0.04
            rct = rect(pnt, pnt) + rect(-12, -12, 13, 13)
            rct = rotateToQuad(rct, ((random(4) - 1) * 90) + 1)
            gtRect = random(4)
            gtRect = rect((gtRect - 1) * 25, 0, gtRect * 25, 25)
            member("layer" & string(lr)).image.copyPixels(member("rainMossGraf").image, rct, gtRect, {#color:colr, #ink:36})
            if (colr <> color(0,255,0)) then
              tpPnt = depthPnt(pnt, lr - 5) + degToVec(random(360)) * random(6)
              rct = rect(tpPnt, tpPnt) + rect(-20, -20, 20, 20) + rect(0, 0, -15, -15)
              copyPixelsToEffectColor(gdLayer, lr, rct, "softBrush1", member("softBrush1").image.rect, 0.5)
            end if
            
          "Lollipop Mold":
            if(random(300) > gEEprops.effects[r].mtrx[q2][c2])then
              grafSz = point(20,20)
              rnd = random(3)-1
              if (gEEprops.effects[r].mtrx[q2][c2] > 60) then
                rnd = random(5)-1
              end if
              
              sz = 0.5+(random(gEEprops.effects[r].mtrx[q2][c2]*0.5)*0.01)
              ang = random(31)-16.0 -- range: -15 to 15 inclusive
              len = (random(8)+12) / 2.0
              
              -- stem
              pnt = pnt - point(0, len)
              rct = rotateToQuadFix(rect(pnt, pnt) + rect(-0.75, -len, 0.75, len), ang)
              member("layer"&string(lr)).image.copyPixels(DRPxl, rct, rect(0,0,1,1), {#color:color(150,0,0), #ink:36}) -- stems forced as shaded color
              
              -- orb
              pnt = pnt - (point(cos((ang+90) * PI / 180.0), sin((ang+90) * PI / 180.0)) * (len + (10*sz) - 2))
              rct = rotateToQuadFix(rect(pnt, pnt) + (rect(-10,-10,10,10) * sz), ang)
              member("layer"&string(lr)).image.copyPixels(member("lollipopMoldGraf").image, rct, rect(20*rnd, 1, 20*(rnd+1), 20), {#color:colr, #ink:36})
              if (colr <> color(0,255,0)) then
                copyPixelsToEffectColor(gdLayer, lr, rct, "lollipopMoldGraf", rect(20*rnd, 21, 20*(rnd+1), 39), 0.5, (random(20) + 80.0) / 100.0)
              end if
            end if
            
          "Og Grass":
            freeSides = 0
            if (solidAfaMv(point(q2-1,c2+1), layer)=0)then
              freeSides = freeSides + 1
            end if
            if (solidAfaMv(point(q2+1,c2+1), layer)=0)then
              freeSides = freeSides + 1
            end if
            rand = random (3)
            rct = rect(pnt, pnt) + rect(-5*rand,-17*rand,5*rand, 3*rand)
            if (freeSides > 0) or (amount<0.5) then
              rnd = 10+random(5)
            else
              rnd = random(15)
            end if
            flp = random(2)-1
            if flp then
              rct = vertFlipRect(rct)
            end if
            gtRect = rect((rnd-1)*20, 0, rnd*19, 50)+rect(1,0,1,0)
            member("layer"&string(lr)).image.copyPixels(member("grassoggraf").image, rct, gtRect, {#color:colr, #ink:36})
            if colr <> color(0,255,0) then
              rct = rect(pnt, pnt) + rect(-5*rand,-17*rand,5*rand, 3*rand)
              if flp then
                rct = vertFlipRect(rct)
              end if
              copyPixelsToEffectColor(gdLayer, lr, rct, "RootGrassGrad", gtRect, 0.5)
            end if 
        end case
      end repeat
    end if
  end repeat
end

on giveGroundPos q, c,l
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  mdPnt = giveMiddleOfTile(point(q,c))
  pnt = mdPnt + point(-11+random(21), 10)
  if (gLEprops.matrix[q2][c2][l][1]=3) then
    pnt.locV = pnt.locv - (pnt.locH-mdPnt.locH) - 5
  else if (gLEprops.matrix[q2][c2][l][1]=2) then
    pnt.locV = pnt.locv - (mdPnt.locH-pnt.locH) - 5
  end if
  return pnt
end


on apply3Dsprawler me, q, c, effc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  big = 0
  if (c > 1) and ((c2 - 1) > 0) then
    big = (gEEprops.effects[r].mtrx[q2][c2-1] > 0)
  end if
  
  lr = 1
  case lrSup of
    "All":
      layer = random(3)
      lrRange = [0, 29]
    "1":
      layer = 1
    "2":
      layer = 2
      lrRange = [6, 29]
    "3":
      layer = 3
      lrRange = [6, 29]
    "1:st and 2:nd":
      layer = random(2)
      lrRange = [0, 29]
    "2:nd and 3:rd":
      layer = random(2) + 1
      lrRange = [6, 29]
    otherwise:
      layer = random(3)
      lrRange = [0, 29]
  end case
  
  lr = ((layer-1)*10) + random(9) - 1
  
  if layer = 1 then
    if lr < 5 then
      lrRange = [0, 5] 
    else 
      lrRange = [6, 29] 
    end if
  end if
  
  case effc of
    "Sprawlbush":
      sts = [#branches:10+random(10)+15*big, #expectedBranchLife:[#small:20, #big:35, #smallRandom:30, #bigRandom:70], #startTired:0, #avoidWalls:1.0, #generalDir:0.6, #randomDir:1.2, #step:6.0]
    "featherFern":
      sts = [#branches:3+random(3)+3*big, #expectedBranchLife:[#small:130, #big:200, #smallRandom:50, #bigRandom:100], #startTired:-77 - (77*big), #avoidWalls:0.6, #generalDir:1.2, #randomDir:0.6, #step:2.0, #featherCounter:0, #airRoots:0]
    "Fungus Tree":
      sts = [#branches:10+random(10)+15*big, #expectedBranchLife:[#small:30, #big:60, #smallRandom:15, #bigRandom:30], #startTired:0, #avoidWalls:0.8, #generalDir:0.8, #randomDir:1.0, #step:3.0, thickness:(6+random(3))*(1+big*0.4), #branchPoints:[]]
    "Head Lamp":
      sts = [#branches:1, #expectedBranchLife:[#small:80, #big:160, #smallRandom:5, #bigRandom:20], #startTired:0, #avoidWalls:0.8, #generalDir:3, #randomDir:10, #step:3.0, thickness:(10+random(3))*(1+big*0.4), #branchPoints:[]]
      
  end case
  
  
  if (afaMvLvlEdit(point(q2,c2), layer)=0)and(afaMvLvlEdit(point(q2,c2+1), layer)=1) then
    
    pnt = giveMiddleOfTile(point(q,c))+point(-10+random(20), 10)
    
    case effc of
      "Fungus Tree", "Head Lamp":
        
        if big then
          expectedLife = sts.expectedBranchLife.big+random(sts.expectedBranchLife.bigRandom)
        else
          expectedLife = sts.expectedBranchLife.small+random(sts.expectedBranchLife.smallRandom)
        end if
        sts.branchPoints = [[#pos:pnt, #dir:point(0,-1), #thickness:sts.thickness, #layer:lr, #lifeLeft:expectedLife, #tired:sts.startTired]]
        
    end case
    
    repeat with branches = 1 to sts.branches then
      pos = point(pnt.loch, pnt.locv)
      lstPos = point(pnt.loch, pnt.locv)
      generalDir = degToVec(-60+random(120))
      lstAimPnt = generalDir
      brLr = lr
      
      brLrDir = 101 + random(201)
      avoidWalls = 2.0
      
      tiredNess = sts.startTired
      
      if big then
        expectedLife = sts.expectedBranchLife.big+random(sts.expectedBranchLife.bigRandom)
      else
        expectedLife = sts.expectedBranchLife.small+random(sts.expectedBranchLife.smallRandom)
      end if
      
      case effc of
        "featherFern":
          sts.airRoots = 25+15*big
        "Fungus Tree", "Head Lamp":
          branch = sts.branchPoints[random(sts.branchPoints.count)]
          sts.branchPoints.deleteOne(branch)
          
          baseThickness = branch.thickness
          pos = branch.pos
          lstPos = branch.pos
          brLr = branch.layer
          generalDir = branch.dir
          lstAimPnt = branch.dir
          tiredNess = branch.tired
          expectedLife = restrict(branch.lifeLeft - 11 + random(21), 5, 200)
          startLifeTime = expectedLife
      end case
      
      repeat with step = 1 to expectedLife then
        lstPos = pos
        
        case effc of
          "featherFern":
            tiredNess = tiredNess + 0.5 + abs(tiredNess*0.05) - 0.3*big
          "Fungus Tree", "Head Lamp":
            tiredNess = -90*(1.0-((startLifeTime-step)/startLifeTime.float))
        end case
        
        aimPnt = generalDir*sts.generalDir+degToVec(random(360))*sts.randomDir + point(0, tiredNess*0.01)
        
        repeat with dir in [point(-1,0), point(-1,-1), point(0,-1), point(1,-1), point(1,0), point(1,1), point(0,1), point(-1,1)] then
          if (afaMvLvlEdit(giveGridPos(lstPos)+dir+gRenderCameraTilePos, ((brLr/10.0)-0.4999).integer+1)=1) then
            aimPnt = aimPnt - dir*avoidWalls
            avoidWalls = restrict(avoidWalls - 0.06, 0.2, 2)
            step = step + (effc <> "Fungus Tree")
          else
            aimPnt = aimPnt + dir*0.1
          end if
        end repeat
        
        avoidWalls = restrict(avoidWalls + 0.03, 0.2, 2)
        
        lstLayer = brLr
        
        
        brLr = brLr + brLrDir*0.01
        
        smllst = lrRange[1]
        if ((lstLayer/10.0)-0.4999).integer+1 > 1 then
          if (afaMvLvlEdit(giveGridPos(pos)+gRenderCameraTilePos, ((lstLayer/10.0)-0.4999).integer+1-1)=1) then
            wall = ((lstLayer/10.0)-0.4999).integer*10
            if wall > 0 then
              wall = wall - 1 
            end if
            smllst = restrict(smllst, wall, 0)
          end if
        end if
        
        bggst = lrRange[2]
        if ((lstLayer/10.0)-0.4999).integer+1 < 3 then
          if (afaMvLvlEdit(giveGridPos(pos)+gRenderCameraTilePos, ((lstLayer/10.0)-0.4999).integer+1+1)=1) then
            wall = ((restrict(lstLayer, 1, 29)/10.0)+0.4999).integer*10 -1
            bggst = restrict(bggst, 0, wall)
          end if
        end if
        
        if brLr < smllst then
          brLr = smllst
          brLrDir = random(41)
        end if
        if brLr > bggst then
          brLr = bggst
          brLrDir = -random(41)
        end if
        
        
        -- aimPnt = aimPnt + point(0, tiredNess*0.01)
        
        aimPnt = (aimPnt + lstAimPnt + lstAimPnt)/3.0
        
        lstAimPnt = aimPnt
        
        pos = pos + moveToPoint(point(0,0), aimPnt, sts.step)
        
        pstColor = 0
        
        case effc of 
          "featherFern":
            if sts.airRoots > 0 then
              sts.featherCounter = 20
              sts.airRoots = sts.airRoots - 1
            end if
            
            
            sts.featherCounter = sts.featherCounter + diag(pos, lstPos)*0.5 + abs(pos.locH - lstPos.locH) + abs(lstLayer-brLr)
            if sts.featherCounter > 8 + ((expectedLife-step)/expectedLife.float)*12 then
              sts.featherCounter = sts.featherCounter - (8 + ((expectedLife-step)/expectedLife.float)*12)
              
              fc = ((expectedLife-step)/expectedLife.float)
              fc = 1.0-fc
              fc = fc*fc
              fc = 1.0-fc
              
              lngth = sin(fc*PI)*  (abs(pos.locV-pnt.locV) + 120)/3.0
              
              
              repeat with cntr = 1 to sts.airRoots then
                lngth = (lngth*6.0 + (abs(pos.locV-pnt.locV)+4))/7.0
              end repeat
              -- put (expectedLife-step)/expectedLife.float && lngth
              
              repeat with rct in [rect(pos, pos) + rect(0, 0, 1, lngth), rect(pos, pos) + rect(1, 0, 2, lngth-random(random(random(lngth.integer+1))))] then
                member("layer"&string(brLr.integer)).image.copyPixels(member("pxl").image, rct, member("pxl").image.rect, {#ink:36, #color:colr})
              end repeat
              
              copyPixelsToEffectColor(gdLayer, brLr, rect(pos, pos) + rect(-6, 0, 6, lngth+2), "featherFernGradient", member("featherFernGradient").rect, 0.5)
              
              pstColor = 1
            end if
            
            fc = ((expectedLife-step)/expectedLife.float)
            fc = fc*fc
            
            ftness = sin(fc*PI)*(4+1*big)
            rct = rect(pos, pos) + rect(-1, -3, 1, 3)+rect(-ftness, -ftness, ftness, ftness)
            
            
            rct = rotateToQuad( rct ,lookAtPoint(pos, lstPos))
            
            
            brLrDir = brLrDir -4 + random(7)
          "Sprawlbush":
            rct = rect(pos, pos) + rect(-2, -5, 2, 5)
            rct = rotateToQuad( rct ,lookAtPoint(pos, lstPos))
            
            brLrDir = brLrDir -11 + random(21)
            
            pstColor = 1
            
          "Fungus Tree":
            
            thickness = ((startLifeTime-step)/startLifeTime.float)*baseThickness
            
            sts.branchPoints.add([#pos:pos, #dir:moveToPoint(point(0,0), aimPnt, 1.0), #thickness:thickness, #layer:brLr, #lifeLeft:startLifeTime-step, #tired:tiredNess])
            
            
            if step = expectedLife then
              rnd = random(5)
              rct = rect(pos, pos)+rect(-5, -19, 5, 1)
              if random(2)=1 then
                rct = vertFlipRect(rct)
              end if
              member("layer"&string(brLr.integer)).image.copyPixels(member("fungusTreeTops").image, rct, rect((rnd-1)*10, 1, rnd*10, 21), {#ink:36, #color:colr})
              copyPixelsToEffectColor(gdLayer, brLr, rect(pos, pos)+rect(-7, -11, 7, 3), "softBrush1", member("softBrush1").rect, 0.5)
            end if
            
            rct = rect(pos, pos) + rect(-1, -3, 1, 3)+rect(-thickness, -thickness, thickness, thickness)
            rct = rotateToQuad( rct ,lookAtPoint(pos, lstPos))
            
            brLrDir = brLrDir -11 + random(21)
            
            pstColor = 1
            
          "Head Lamp":
            -- Made by April
            
            thickness = ((startLifeTime-step)/startLifeTime.float)*baseThickness
            
            sts.branchPoints.add([#pos:pos, #dir:moveToPoint(point(0,0), aimPnt, 1.0), #thickness:thickness, #layer:brLr, #lifeLeft:startLifeTime-step, #tired:tiredNess])
            if step <= 7 then            
              --for making the stump more lumpy & strange, quite like your mother
              rct=thickness*7
              rct2= rect(-8, 0, 9, 17)
              repeat with circleStep = 0 to 3 then    
                rct=rct-(rct/10)
                --draws circle around stump then places random points along stump 
                stumpRadius= (rct)/2
                rnd=stumpRadius
                repeat with circlePnt = 0 to rnd then
                  randAngle = random(180)
                  randAngle = randAngle * (pi/180)
                  randAnglePos= point(sin(randAngle)*100, cos(randAngle)*100)
                  randAnglePos=randAnglePos*random(stumpRadius)
                  randAnglePos=randAnglePos/100
                  --draw circle
                  repeat with insideStep = 0 to 2 then 
                    if (brLr-(insideStep+circleStep)) < 29 and (brLr-(insideStep+circleStep)) > 0 then
                      member("layer"&string(brLr.integer-(insideStep+circleStep))).image.copyPixels(member("blob").image, rct2+rect(randAnglePos, randAnglePos)+rect(pos, pos), member("blob").rect, {#ink:36, #color:colr})
                    end if
                  end repeat
                end repeat
              end repeat
              
            end if 
            
            if step = expectedLife then
              rnd = random(4)
              headSize= rect(-79, -9, 80, 10)+rect(pos,pos)
              --draw bounds for antennas and fruit
              HeadLampSprite=rect(160*(rnd-1), 0,160*rnd, 19)
              rnd=random(15)+7
              overallrnd=random(80)-40
              rctL=rotateToQuadFix(headSize, rnd+overallrnd)
              rctR=rotateToQuadFix(headSize, -1*rnd+overallrnd+random(5))
              --draw fruits
              fruitRnd=random(6)
              fruitRct=rect(-17, -10, 18, 10)
              fruitRctR=rotateToQuadFix(fruitRct+rect(rctR[3], rctR[3]), (rnd+overallrnd)/2)
              FruitRctL=rotateToQuadFix(fruitRct+rect(rctL[4], rctL[4]), (rnd+overallrnd)/2)
              fruitSpriteRct=rect(35*(fruitRnd-1), 0, 35*fruitRnd, 20)
              
              --peak dogshit to prevent drawing outside valid layers
              if (brLr.integer-1)<0 then
                brLr=brLr+1
              end if
              
              --Right fruits
              member("layer"&string(brLr.integer-1)).image.copyPixels(member("HeadLampFruitGraf").image, fruitRctR, fruitSpriteRct, {#ink:36, #color:lampColr})
              copyPixelsToRootEffectColor(lampLayer, brLr-1, fruitRctR, "HeadLampFruitGraf", fruitSpriteRct, 0.5, 1)
              --left fruit
              fruitRnd=random(6)
              fruitSpriteRct=rect(35*(fruitRnd-1), 0, 35*fruitRnd, 20)
              member("layer"&string(brLr.integer-1)).image.copyPixels(member("HeadLampFruitGraf").image, FruitRctL-rect(0,0,0,3), fruitSpriteRct, {#ink:36, #color:lampColr})
              
              copyPixelsToRootEffectColor(lampLayer, brLr-1, FruitRctL-rect(0,0,0,3), "HeadLampFruitGraf", fruitSpriteRct, 0.5, 1)
              --draw antennas
              member("layer"&string(brLr.integer)).image.copyPixels(member("HeadLampGrafR").image, rctR, HeadLampSprite, {#ink:36, #color:colr})
              member("layer"&string(brLr.integer)).image.copyPixels(member("HeadLampGrafL").image, rctL, HeadLampSprite, {#ink:36, #color:colr})
              --erase any effectcolor below it
              copyPixelsToEffectColor(gdLayer, brLr, rctR, "HeadLampGrafR", HeadLampSprite, 0.5, -1)
              copyPixelsToEffectColor(gdLayer, brLr, rctL, "HeadLampGrafL", HeadLampSprite, 0.5, -1)
              
              copyPixelsToEffectColor(gdLayer, brLr, rctR, "HeadLampGrad", member("HeadLampGrad").rect, 0.5, 1)
              copyPixelsToEffectColor(gdLayer, brLr, rctL, "HeadLampGrad", member("HeadLampGrad").rect, 0.5, 1)
            end if
            
            rct = rect(pos, pos) + rect(-1, -3, 1, 3)+rect(-thickness, -thickness, thickness, thickness)
            rct = rotateToQuad( rct ,lookAtPoint(pos, lstPos))
            
            brLrDir = brLrDir -11 + random(21)
            
            pstColor = 1
        end case
        
        member("layer"&string(brLr.integer)).image.copyPixels(member("blob").image, rct, member("blob").image.rect, {#ink:36, #color:colr})
        member("layer"&string(lstLayer.integer)).image.copyPixels(member("blob").image, rct, member("blob").image.rect, {#ink:36, #color:colr})
        
        if pstColor then
          blnd = (1.0-((expectedLife - step)/expectedLife.float))*25 + random((1.0-((expectedLife - step)/expectedLife.float))*75)
          if effc = "Fungus Tree" then
            blnd = (1.0-((expectedLife - step)/expectedLife.float))*100
          end if
          if effc = "Head Lamp" then
            blnd = (1.0-((expectedLife - step)/expectedLife.float))*50
          end if
          member("softbrush2").image.copypixels(member("pxl").image, member("softbrush2").image.rect, rect(0,0,1,1), {#color:color(255,255,255)})
          member("softbrush2").image.copypixels(member("softbrush1").image, member("softbrush2").image.rect, member("softbrush1").image.rect, {#blend:blnd})
          copyPixelsToEffectColor(gdLayer, brLr, rotateToQuad(rect(pos, pos) + rect(-17, -25, 17, 25),lookAtPoint(pos, lstPos)), "softBrush2", member("softBrush1").rect, 0.5)
        end if
        
      end repeat
    end repeat
    
    
  end if
end


on applyInverse3Dsprawler me, q, c, effc
  q2 = q + gRenderCameraTilePos.locH
  c2 = c + gRenderCameraTilePos.locV
  
  big = 0
  if (c > 1) and ((c2 - 1) > 0) then
    big = (gEEprops.effects[r].mtrx[q2][c2+1] > 0)
  end if
  
  lr = 1
  case lrSup of
    "All":
      layer = random(3)
      lrRange = [0, 29]
    "1":
      layer = 1
    "2":
      layer = 2
      lrRange = [6, 29]
    "3":
      layer = 3
      lrRange = [6, 29]
    "1:st and 2:nd":
      layer = random(2)
      lrRange = [0, 29]
    "2:nd and 3:rd":
      layer = random(2) + 1
      lrRange = [6, 29]
    otherwise:
      layer = random(3)
      lrRange = [0, 29]
  end case
  
  lr = ((layer-1)*10) + random(9) - 1
  
  if layer = 1 then
    if lr < 5 then
      lrRange = [0, 5] 
    else 
      lrRange = [6, 29] 
    end if
  end if
  
  case effc of
    "Sprawlroots":
      sts = [#branches:10+random(10)+15*big, #expectedBranchLife:[#small:20, #big:35, #smallRandom:30, #bigRandom:70], #startTired:0, #avoidWalls:1.0, #generalDir:0.6, #randomDir:1.2, #step:6.0]
    "Fungus Roots":
      sts = [#branches:10+random(10)+15*big, #expectedBranchLife:[#small:30, #big:60, #smallRandom:15, #bigRandom:30], #startTired:0, #avoidWalls:0.8, #generalDir:0.8, #randomDir:1.0, #step:3.0, thickness:(6+random(3))*(1+big*0.4), #branchPoints:[]]
    "Ceiling Lamp":
      sts = [#branches:1, #expectedBranchLife:[#small:80, #big:160, #smallRandom:5, #bigRandom:20], #startTired:0, #avoidWalls:0.8, #generalDir:3, #randomDir:10, #step:3.0, thickness:(10+random(3))*(1+big*0.4), #branchPoints:[]]
  end case
  
  
  if (afaMvLvlEdit(point(q2,c2), layer)=0)and(afaMvLvlEdit(point(q2,c2-1), layer)=1) then
    
    pnt = giveMiddleOfTile(point(q,c))+point(-10+random(20), -10)
    
    case effc of
      "Fungus Roots", "Ceiling Lamp":
        
        if big then
          expectedLife = sts.expectedBranchLife.big+random(sts.expectedBranchLife.bigRandom)
        else
          expectedLife = sts.expectedBranchLife.small+random(sts.expectedBranchLife.smallRandom)
        end if
        sts.branchPoints = [[#pos:pnt, #dir:point(0,1), #thickness:sts.thickness, #layer:lr, #lifeLeft:expectedLife, #tired:sts.startTired]]
        
    end case
    
    repeat with branches = 1 to sts.branches then
      pos = point(pnt.loch, pnt.locv)
      lstPos = point(pnt.loch, pnt.locv)
      generalDir = degToVec(-60+random(120) + 180)
      lstAimPnt = generalDir
      brLr = lr
      
      brLrDir = 101 + random(201)
      avoidWalls = 2.0
      
      tiredNess = sts.startTired
      
      if big then
        expectedLife = sts.expectedBranchLife.big+random(sts.expectedBranchLife.bigRandom)
      else
        expectedLife = sts.expectedBranchLife.small+random(sts.expectedBranchLife.smallRandom)
      end if
      
      case effc of
        "Fungus Roots", "Ceiling Lamp":
          branch = sts.branchPoints[random(sts.branchPoints.count)]
          sts.branchPoints.deleteOne(branch)
          
          baseThickness = branch.thickness
          pos = branch.pos
          lstPos = branch.pos
          brLr = branch.layer
          generalDir = branch.dir
          lstAimPnt = branch.dir
          tiredNess = branch.tired
          expectedLife = restrict(branch.lifeLeft - 11 + random(21), 5, 200)
          startLifeTime = expectedLife
      end case
      
      repeat with step = 1 to expectedLife then
        lstPos = pos
        
        case effc of
          "Fungus Roots", "Ceiling Lamp":
            tiredNess = -90*(1.0-((startLifeTime-step)/startLifeTime.float))
        end case
        
        aimPnt = generalDir*sts.generalDir+degToVec(random(360))*sts.randomDir - point(0, tiredNess*0.01)
        
        repeat with dir in [point(-1,0), point(-1,-1), point(0,-1), point(1,-1), point(1,0), point(1,1), point(0,1), point(-1,1)] then
          if (afaMvLvlEdit(giveGridPos(lstPos)+dir+gRenderCameraTilePos, ((brLr/10.0)-0.4999).integer+1)=1) then
            aimPnt = aimPnt - dir*avoidWalls
            avoidWalls = restrict(avoidWalls - 0.06, 0.2, 2)
            step = step + (effc <> "Fungus Roots")
          else
            aimPnt = aimPnt + dir*0.1
          end if
        end repeat
        
        avoidWalls = restrict(avoidWalls + 0.03, 0.2, 2)
        
        lstLayer = brLr
        
        
        brLr = brLr + brLrDir*0.01
        
        smllst = lrRange[1]
        if ((lstLayer/10.0)-0.4999).integer+1 > 1 then
          if (afaMvLvlEdit(giveGridPos(pos)+gRenderCameraTilePos, ((lstLayer/10.0)-0.4999).integer+1-1)=1) then
            wall = ((lstLayer/10.0)-0.4999).integer*10
            if wall > 0 then
              wall = wall - 1 
            end if
            smllst = restrict(smllst, wall, 0)
          end if
        end if
        
        bggst = lrRange[2]
        if ((lstLayer/10.0)-0.4999).integer+1 < 3 then
          if (afaMvLvlEdit(giveGridPos(pos)+gRenderCameraTilePos, ((lstLayer/10.0)-0.4999).integer+1+1)=1) then
            wall = ((restrict(lstLayer, 1, 29)/10.0)+0.4999).integer*10 -1
            bggst = restrict(bggst, 0, wall)
          end if
        end if
        
        if brLr < smllst then
          brLr = smllst
          brLrDir = random(41)
        end if
        if brLr > bggst then
          brLr = bggst
          brLrDir = -random(41)
        end if
        
        
        -- aimPnt = aimPnt + point(0, tiredNess*0.01)
        
        aimPnt = (aimPnt + lstAimPnt + lstAimPnt)/3.0
        
        lstAimPnt = aimPnt
        
        pos = pos + moveToPoint(point(0,0), aimPnt, sts.step)
        
        pstColor = 0
        
        case effc of 
          "Sprawlroots":
            rct = rect(pos, pos) + rect(-2, -5, 2, 5)
            rct = rotateToQuad( rct ,lookAtPoint(pos, lstPos))
            
            brLrDir = brLrDir -11 + random(21)
            
            pstColor = 1
            
          "Fungus Roots":
            
            thickness = ((startLifeTime-step)/startLifeTime.float)*baseThickness
            
            sts.branchPoints.add([#pos:pos, #dir:moveToPoint(point(0,0), aimPnt, 1.0), #thickness:thickness, #layer:brLr, #lifeLeft:startLifeTime-step, #tired:tiredNess])
            
            
            if step = expectedLife then
              rnd = random(5)
              rct = rect(pos, pos)+rect(-5, -19, 5, 1)
              if random(2)=1 then
                rct = vertFlipRect(rct)
              end if
              member("layer"&string(brLr.integer)).image.copyPixels(member("fungusTreeTops").image, rct, rect((rnd-1)*10, 1, rnd*10, 21), {#ink:36, #color:colr})
              copyPixelsToEffectColor(gdLayer, brLr, rect(pos, pos)+rect(-7, -11, 7, 3), "softBrush1", member("softBrush1").rect, 0.5)
            end if
            
            rct = rect(pos, pos) + rect(-1, -3, 1, 3)+rect(-thickness, -thickness, thickness, thickness)
            rct = rotateToQuad( rct ,lookAtPoint(pos, lstPos))
            
            brLrDir = brLrDir -11 + random(21)
            
            pstColor = 1
            
          "Ceiling Lamp":
            -- Made by April, upside-down-ified by Alduris
            
            thickness = ((startLifeTime-step)/startLifeTime.float)*baseThickness
            
            sts.branchPoints.add([#pos:pos, #dir:moveToPoint(point(0,0), aimPnt, 1.0), #thickness:thickness, #layer:brLr, #lifeLeft:startLifeTime-step, #tired:tiredNess])
            if step <= 7 then            
              --for making the stump more lumpy & strange, quite like your mother
              rct=thickness*7
              rct2= rect(-8, 0, 9, 17)
              repeat with circleStep = 0 to 3 then    
                rct=rct-(rct/10)
                --draws circle around stump then places random points along stump 
                stumpRadius= (rct)/2
                rnd=stumpRadius
                repeat with circlePnt = 0 to rnd then
                  randAngle = random(180)
                  randAngle = randAngle * (pi/180)
                  randAnglePos= point(sin(randAngle)*100, cos(randAngle)*100)
                  randAnglePos=randAnglePos*random(stumpRadius)
                  randAnglePos=randAnglePos/100
                  --draw circle
                  repeat with insideStep = 0 to 2 then 
                    if (brLr-(insideStep+circleStep)) < 29 and (brLr-(insideStep+circleStep)) > 0 then
                      member("layer"&string(brLr.integer-(insideStep+circleStep))).image.copyPixels(member("blob").image, rct2+rect(randAnglePos, randAnglePos)+rect(pos, pos), member("blob").rect, {#ink:36, #color:colr})
                    end if
                  end repeat
                end repeat
              end repeat
              
            end if 
            
            if step = expectedLife then
              rnd = random(4)
              headSize= rect(-79, -9, 80, 10)+rect(pos,pos)
              --draw bounds for antennas and fruit
              HeadLampSprite=rect(160*(rnd-1), 0,160*rnd, 19)
              rnd=random(15)+7
              overallrnd=random(80)-40
              rctL=rotateToQuadFix(headSize, rnd+overallrnd)
              rctR=rotateToQuadFix(headSize, -1*rnd+overallrnd+random(5))
              --draw fruits
              fruitRnd=random(6)
              fruitRct=rect(-17, -10, 18, 10)
              fruitRctR=rotateToQuadFix(fruitRct+rect(rctR[3], rctR[3]), (rnd+overallrnd)/2)
              FruitRctL=rotateToQuadFix(fruitRct+rect(rctL[4], rctL[4]), (rnd+overallrnd)/2)
              fruitSpriteRct=rect(35*(fruitRnd-1), 0, 35*fruitRnd, 20)
              
              --peak dogshit to prevent drawing outside valid layers
              if (brLr.integer-1)<0 then
                brLr=brLr+1
              end if
              
              --Right fruits
              member("layer"&string(brLr.integer-1)).image.copyPixels(member("HeadLampFruitGraf").image, fruitRctR, fruitSpriteRct, {#ink:36, #color:lampColr})
              copyPixelsToRootEffectColor(lampLayer, brLr-1, fruitRctR, "HeadLampFruitGraf", fruitSpriteRct, 0.5, 1)
              --left fruit
              fruitRnd=random(6)
              fruitSpriteRct=rect(35*(fruitRnd-1), 0, 35*fruitRnd, 20)
              member("layer"&string(brLr.integer-1)).image.copyPixels(member("HeadLampFruitGraf").image, FruitRctL-rect(0,0,0,3), fruitSpriteRct, {#ink:36, #color:lampColr})
              
              copyPixelsToRootEffectColor(lampLayer, brLr-1, FruitRctL-rect(0,0,0,3), "HeadLampFruitGraf", fruitSpriteRct, 0.5, 1)
              --draw antennas
              member("layer"&string(brLr.integer)).image.copyPixels(member("HeadLampGrafR").image, rctR, HeadLampSprite, {#ink:36, #color:colr})
              member("layer"&string(brLr.integer)).image.copyPixels(member("HeadLampGrafL").image, rctL, HeadLampSprite, {#ink:36, #color:colr})
              --erase any effectcolor below it
              copyPixelsToEffectColor(gdLayer, brLr, rctR, "HeadLampGrafR", HeadLampSprite, 0.5, -1)
              copyPixelsToEffectColor(gdLayer, brLr, rctL, "HeadLampGrafL", HeadLampSprite, 0.5, -1)
              
              copyPixelsToEffectColor(gdLayer, brLr, rctR, "HeadLampGrad", member("HeadLampGrad").rect, 0.5, 1)
              copyPixelsToEffectColor(gdLayer, brLr, rctL, "HeadLampGrad", member("HeadLampGrad").rect, 0.5, 1)
            end if
            
            rct = rect(pos, pos) + rect(-1, -3, 1, 3)+rect(-thickness, -thickness, thickness, thickness)
            rct = rotateToQuad( rct ,lookAtPoint(pos, lstPos))
            
            brLrDir = brLrDir -11 + random(21)
            
            pstColor = 1
            
        end case
        
        member("layer"&string(brLr.integer)).image.copyPixels(member("blob").image, rct, member("blob").image.rect, {#ink:36, #color:colr})
        member("layer"&string(lstLayer.integer)).image.copyPixels(member("blob").image, rct, member("blob").image.rect, {#ink:36, #color:colr})
        
        if pstColor then
          blnd = (1.0-((expectedLife - step)/expectedLife.float))*25 + random((1.0-((expectedLife - step)/expectedLife.float))*75)
          if effc = "Fungus Roots" then
            blnd = (1.0-((expectedLife - step)/expectedLife.float))*100
          end if
          member("softbrush2").image.copypixels(member("pxl").image, member("softbrush2").image.rect, rect(0,0,1,1), {#color:color(255,255,255)})
          member("softbrush2").image.copypixels(member("softbrush1").image, member("softbrush2").image.rect, member("softbrush1").image.rect, {#blend:blnd})
          copyPixelsToEffectColor(gdLayer, brLr, rotateToQuad(rect(pos, pos) + rect(-17, -25, 17, 25),lookAtPoint(pos, lstPos)), "softBrush2", member("softBrush1").rect, 0.5)
        end if
        
      end repeat
    end repeat
    
    
  end if
end


