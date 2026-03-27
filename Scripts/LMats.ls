global gLOprops, gEEprops, gAnyDecals, gTiles, gTEprops, gLEprops, gRenderCameraTilePos
global DRLastMatImp, DRLastSlpImp, DRLastFlrImP, DRLastTexImp, DRCustomMatList, DRLastTL, DRLastTrshImp, DRLastPipeImp, DRLastDensePipeImp
global DRPxl, DRPxlRect, DRWhite

on LCheckIfATileIsSolidAndSameMaterial(tl, lr, matName)
  tl = point(restrict(tl.locH, 1, gLOprops.size.loch), restrict(tl.locV, 1, gLOprops.size.locv))
  if (gLEprops.matrix[tl.locH][tl.locV][lr][1] = 1) then
    matTile = gTEprops.tlMatrix[tl.locH][tl.locV][lr]
    if (matTile.tp = "material") then
      if (matTile.data = matName) then
        return 1
      end if
    else if (matTile.tp = "default") then
      if (gTEprops.defaultMaterial = matName) then
        return 1
      end if
    end if 
  end if
  return 0
end

on LIsMyTileSetOpenToThisTile(matName, tl, l)
  if (tl.inside(rect(1, 1, gLOprops.size.loch + 1, gLOprops.size.locv + 1))) then
    if ([1, 2, 3, 4, 5].getPos(gLEProps.matrix[tl.locH][tl.locV][l][1]) > 0) then
      tile = gTEprops.tlMatrix[tl.locH][tl.locV][l]
      if (tile.tp = "material") then
        if (tile.data = matName) then
          return 1
        end if
      else if (tile.tp = "default") then
        if (gTEprops.defaultMaterial = matName) then
          return 1
        end if
      end if
    end if
  else if (gTEprops.defaultMaterial = matName) then
    return 1
  end if
  return 0
end

on LRenderTileMaterial(l, nm, frntImg)
  -- Random machines and chaotic stone-like materials (made by Alduris + contributions from Of Incandescence)
  if (DRCustomMatList.count >= 1) then
    matTl = DRCustomMatList[DRLastTL]
    if (matTl.nm <> nm) then
      repeat with inti = 1 to DRCustomMatList.count
        if (DRCustomMatList[inti].nm = nm) then
          matTl = DRCustomMatList[inti]
          DRLastTL = inti
          exit repeat
        end if
      end repeat
    end if
    if (matTl.nm = nm) then
      matPickInfo = matTl.autofit
      pickCats = []
      pickTiles = []
      pickWeights = []
      pickIgnore = []
      savSeed = the randomSeed
      the randomSeed = gLOprops.tileSeed + l
      
      if matPickInfo.findPos(#categories) then
        pickCats = matPickInfo.categories
      end if
      if matPickInfo.findPos(#tiles) then
        pickTiles = matPickInfo.tiles
        pickWeights = matPickInfo.tileWeights
      end if
      if matPickInfo.findPos(#ignoreTiles) then
        pickIgnore = matPickInfo.ignoreTiles
      end if
      
      -- Find tiles with our material
      tlsOrdered = []
      repeat with q = 1 to gLOprops.size.loch
        repeat with c = 1 to gLOprops.size.locv
          LEPropqc = gLEProps.matrix[q][c][l][1]
          if (LEPropqc <> 0) then
            addMe = 0
            TEPropqc = gTEprops.tlMatrix[q][c][l]
            if(TEPropqc.tp = "material") then
              if(TEPropqc.data = matTl.nm) then
                addMe = 1
              end if
            else if (gTEprops.defaultMaterial = matTl.nm)then
              if (TEPropqc.tp = "default")then
                addMe = 1
              end if
            end if
            
            if(addMe)then
              tlsOrdered.add([random(gLOprops.size.loch + gLOprops.size.locV), point(q, c)])
            end if
          end if
        end repeat
      end repeat
      
      tlsOrdered.sort()
      delL = [:]
      tls = []
      repeat with q = 1 to tlsOrdered.count
        tls.add(tlsOrdered[q][2])
      end repeat
      
      -- Figure out which tiles the user wants
      tileSelection = []
      repeat with tlGrp in gTiles then
        repeat with tl in tlGrp.tls then
          pos = pickTiles.getPos(tl.nm)
          if (pickCats.getPos(tlGrp.nm) <> 0 and pickIgnore.getPos(tl.nm) = 0) or (pos <> 0) then
            --tileSelection.add(tl)
            -- Only select tiles with some solid bits
            repeat with spec in tl.specs then
              if spec > 0 then
                weight = 1
                if pos > 0 then
                  weight = pickWeights[pos].integer
                end if
                repeat with q = 1 to weight
                  tileSelection.add(tl)
                end repeat
                exit repeat
              end if
            end repeat
          end if
        end repeat
      end repeat
      
      -- Draw the material
      if tileSelection.count > 0 then
        repeat with tl in tls then
          the randomSeed = seedForTile(tl, gLOprops.tileSeed + l)
          if delL.findPos(tl)=void then
            -- Shuffle tiles
            randomTiles = []
            repeat with thisTl in tileSelection then
              randomTiles.append([random(1000), thisTl])
            end repeat
            randomTiles.sort()
            
            -- Find a tile to place
            repeat with t = 1 to randomTiles.count then
              testTile = randomTiles[t][2]
              
              -- Determine legality of placement
              legalToPlace = true
              repeat with a = 0 to testTile.sz.locH-1 then
                repeat with b = 0 to testTile.sz.locV-1 then
                  testPoint = tl + point(a,b)
                  spec = testTile.specs[(b+1) + (a*testTile.sz.locV)]
                  
                  if spec <= 0 then next repeat -- ignore air and buffer
                  
                  if (tls.getPos(testPoint) = 0) then -- areas where material is not placed
                    legalToPlace = false
                    exit repeat
                  end if
                  
                  geoSpec = afaMvLvlEdit(testPoint, l)
                  if (geoSpec <> spec) then
                    -- spec does not match on non-solid tile
                    legalToPlace = false
                    exit repeat
                  end if
                  
                  if (delL.findPos(testPoint)<>void) then -- tile has been placed here previously
                    legalToPlace = false
                    exit repeat
                  end if
                  
                end repeat
                if (not legalToPlace) then exit repeat
              end repeat
              
              if legalToPlace then
                -- Place tile
                rootPos = tl + point(((testTile.sz.locH.float/2.0) + 0.4999).integer-1, ((testTile.sz.locV.float/2.0) + 0.4999).integer-1)
                if(rootPos.inside(rect(gRenderCameraTilePos, gRenderCameraTilePos+point(100, 60))))then
                  frntImg = drawATileTile(rootPos.loch,rootPos.locV,l,testTile, frntImg, []) -- array argument required for chain holders. do not remove it!
                end if
                
                -- Remove tile ref
                repeat with a = 0 to testTile.sz.locH-1 then
                  repeat with b = 0 to testTile.sz.locV-1 then
                    testPoint = tl + point(a,b)
                    spec = testTile.specs[(b+1) + (a*testTile.sz.locV)]
                    if (spec > -1) then
                      delL[testPoint] = 1
                    end if
                  end repeat
                end repeat
                exit repeat
              end if
            end repeat
          end if
        end repeat
        the randomSeed = savSeed
      end if
    end if
  end if
  return frntImg
end


on LRenderPatternMaterial(l, nm, frntImg)
  global gEEprops

  -- Custom Temple Stone and Tiled Stone materials (made by Of Incandescence)
  if (DRCustomMatList.count >= 1) then
    matTl = DRCustomMatList[DRLastTL]

    if (matTl.nm <> nm) then
      repeat with inti = 1 to DRCustomMatList.count
        if (DRCustomMatList[inti].nm = nm) then
          matTl = DRCustomMatList[inti]
          DRLastTL = inti
          exit repeat
        end if
      end repeat
    end if

    if (matTl.nm = nm) then

      -- Search for Pattern Depth / Chaos effects
      -- Actually a list of matrices, in case one adds more than one instance of the effect on the same layer
      depthMtrx = []
      chaosMtrx = []
      repeat with q = 1 to gEEprops.effects.count then
        eff = gEEprops.effects[q]
        case eff.nm of
          "Pattern Depth":
            case eff.options[2][3] of --["1", "2", "3"]
              "1":
                dmin = 1
              "2":
                dmin = 2
              "3":
                dmin = 3
              otherwise:
                dmin = 1
            end case

            if (dmin = l) then
              case eff.options[3][3] of
                "Increase":
                  mode = false
                "Decrease":
                  mode = true
                otherwise:
                  mode = false
              end case
              depthMtrx.add([eff.mtrx, mode])
            end if

          "Pattern Chaos":
            octaves = 1
            effSeed = 1

            repeat with op in eff.options
              case op[1] of
                "Layers":
                  case op[3] of--["All", "1", "2", "3", "1:st and 2:nd", "2:nd and 3:rd"]
                    "1":
                      dmin = 1
                      dmax = 1
                    "2":
                      dmin = 2
                      dmax = 2
                    "3":
                      dmin = 3
                      dmax = 3
                    "1:st and 2:nd":
                      dmin = 1
                      dmax = 2
                    "2:nd and 3:rd":
                      dmin = 2
                      dmax = 3
                    otherwise:
                      dmin = 1
                      dmax = 3
                  end case

                "Noise Smoothness":
                  octaves = value(op[3])

                "Seed":
                  effSeed = op[3]
              end case
            end repeat

            if (l >= dmin) and (l <= dmax) then
              chaosMtrx.add([eff.mtrx, octaves, effSeed])
            end if
        end case
      end repeat

      matInfo = matTl.pattern
      pickPatterns = []
      pickTiles = []
      patterns = []
      patternWeights = []
      tileSelection = []
      repeatSize = point(1,1)
      savSeed = the randomSeed
      the randomSeed = gLOprops.tileSeed + l

      if matInfo.findPos(#patterns) then
        repeat with pattern in matInfo.patterns
          pickPattern = []
          patternData = []
          repeat with pat in pattern[1]
            pickPattern.add(pat[1])
            patternData.add([pat[2]]) -- We later add data to this
          end repeat
          pickPatterns.add(pickPattern)
          patterns.add(patternData)
          patternWeights.add(pattern[2])
        end repeat
      end if

      if matInfo.findPos(#tiles) then
        pickTiles = matInfo.tiles
        repeat with tl in pickTiles
          tileSelection.add(VOID)
        end repeat
      end if

      if matInfo.findPos(#sz) then
        repeatSize = matInfo.sz
      end if

      -- Find tiles with our material
      tlsOrdered = []
      repeat with q = 1 to gLOprops.size.loch
        repeat with c = 1 to gLOprops.size.locv
          LEPropqc = gLEProps.matrix[q][c][l][1]
          if (LEPropqc <> 0) then
            addMe = 0
            TEPropqc = gTEprops.tlMatrix[q][c][l]
            if(TEPropqc.tp = "material") then
              if(TEPropqc.data = matTl.nm) then
                addMe = 1
              end if
            else if (gTEprops.defaultMaterial = matTl.nm)then
              if (TEPropqc.tp = "default")then
                addMe = 1
              end if
            end if

            if(addMe)then
              tlsOrdered.add([random(gLOprops.size.loch + gLOprops.size.locV), point(q, c)])
            end if
          end if
        end repeat
      end repeat

      tlsOrdered.sort()
      tls = []
      repeat with q = 1 to tlsOrdered.count
        tls.add(tlsOrdered[q][2])
      end repeat

      -- Grab tiles
      geoTiles = [[], [], [], [], []] -- NE, NW, SE, SW, Floor
      repeat with tlGrp in gTiles then
        repeat with tl in tlGrp.tls then
          -- Check misc tiles
          pos = pickTiles.getPos(tl.nm)
          if (pos <> 0) then
            -- Check if slope or floor
            if (tl.sz = point(1, 1)) and (tl.specs[1] > 1) and (tl.specs[1] < 7) then
              geoTiles[tl.specs[1] - 1] = tl
            -- Not slope
            else
              tileSelection[pos] = tl
            end if
          end if

          -- Check pattern tiles
          repeat with pat = 1 to pickPatterns.count
            pickPattern = pickPatterns[pat]
            pattern = patterns[pat]
            pos = pickPattern.getPos(tl.nm)
            repeat while (pos <> 0) then
              if (pattern[pos].count = 1) then
                pattern[pos].add(tl)

                -- Check for tile slopes
                tileCorners = []
                repeat with speci = 1 to tl.specs.count then
                  geo = tl.specs[speci]
                  if (geo > 1) and (geo < 6) then
                    loc = point(((speci - 1) / tl.sz.locV - 0.4999).integer, (speci - 1) mod tl.sz.locV)
                    tileCorners.add([geo - 1, loc])
                  end if
                end repeat

                if (tileCorners.count >= 1) then
                  pattern[pos].add(tileCorners)
                end if
              end if

              -- Patterns almost always have the same tile more than once
              pickPattern[pos] = ""
              pos = pickPattern.getPos(tl.nm)
            end repeat
          end repeat
        end repeat
      end repeat

      patternCorners = [[], [], [], []]
      patterns2 = [[0, patterns[1]]]
      indPos = point(-1, -1)
      delL = [:]

      -- Draw pattern
      repeat with tlPos in tls
        -- Quick discard
        if (delL.findPos(tlPos) <> void) then
          next repeat
        end if

        if (patterns.count > 1) then
          indPos2 = floorPoint(tlPos / (repeatSize * 1.0))
          if (indPos <> indPos2) then
            indPos = indPos2
            the randomSeed = seedForTile(indPos, gLOprops.tileSeed + l)
            patterns2 = []
            repeat with pat = 1 to patterns.count
              randV = random(65536)
              randV = power(randV.float / 65536, patternWeights[pat]) * 65536
              patterns2.append([randV, patterns[pat]])
            end repeat
            patterns2.sort()
          end if
        end if

        modPos = point(tlPos.locH mod repeatSize.locH, tlPos.locV mod repeatSize.locV)

        repeat with pat in patterns2 then
          pattern = pat[2]
          repeat with patTl in pattern then
            if (patTl.count <= 1) then
              next repeat
            end if

            tl = patTl[2]
            mdPnt = ceilPoint(tl.sz*0.5) - point(1,1)
            tlOffs = modPos - patTl[1]

            -- Implemented this way to encourage tiles earlier in the list to be placed first (one of our materials needed this :surv_pleh:)
            if ((tlOffs+mdPnt).inside(rect(point(0,0), tl.sz))) then
              drawn = true
              drawPos = tlPos - tlOffs
              occupy = []

              repeat with x = 0 to tl.sz.locH-1 then
                repeat with y = 0 to tl.sz.locV-1 then
                  -- Only check solid geo
                  if (tl.specs[x * tl.sz.locV + y + 1] <> 1) then
                    next repeat
                  end if

                  loc = point(x,y) - mdPnt
                  if (checkIfATileIsSolidAndSameMaterial(drawPos + loc, l, nm) = 0) then
                    drawn = false
                    exit repeat
                  end if

                  -- Tile is already occupied
                  if (delL.findPos(drawPos + loc) <> void) then
                    drawn = false
                    exit repeat
                  end if

                  occupy.add(loc)
                end repeat
                if (drawn = false) then exit repeat
              end repeat

              if (drawn) then
                frntImg = LDrawADepthTile(drawPos, l, tl, frntImg, depthMtrx, chaosMtrx, occupy)

                -- Corners
                if (patTl.count > 2) then
                  repeat with corner in patTl[3] then
                    loc = drawPos + corner[2] - mdPnt
                    if (checkIfATileIsSolidAndSameMaterial(loc, l, nm)) then
                      patternCorners[corner[1]].add(loc)
                    end if
                    delL[loc] = 1
                  end repeat
                end if

                repeat with occ in occupy
                  delL[drawPos + occ] = 1
                end repeat

                exit repeat
              end if
            end if

          end repeat
        end repeat
      end repeat

      -- Draw remaining corners

      repeat with q = 1 to patternCorners[1].count then
        ind = patternCorners[1].count + 1 - q
        tlPos = patternCorners[1][ind]
        ind2 = patternCorners[4].getPos(tlPos)
        if (ind2 > 0) then
          patternCorners[4].deleteAt(ind2)
          -- patternCorners[1].deleteAt(ind)
          next repeat
        end if
        frntImg = LDrawADepthTile(tlPos, l, geoTiles[4], frntImg, depthMtrx, chaosMtrx, [])
      end repeat

      repeat with q = 1 to patternCorners[2].count then
        ind = patternCorners[2].count + 1 - q
        tlPos = patternCorners[2][ind]
        ind2 = patternCorners[3].getPos(tlPos)
        if (ind2 > 0) then
          patternCorners[3].deleteAt(ind2)
          -- patternCorners[2].deleteAt(ind)
          next repeat
        end if
        frntImg = LDrawADepthTile(tlPos, l, geoTiles[3], frntImg, depthMtrx, chaosMtrx, [])
      end repeat

      repeat with q = 1 to patternCorners[3].count then
        tlPos = patternCorners[3][q]
        frntImg = LDrawADepthTile(tlPos, l, geoTiles[2], frntImg, depthMtrx, chaosMtrx, [])
      end repeat

      repeat with q = 1 to patternCorners[4].count then
        tlPos = patternCorners[4][q]
        frntImg = LDrawADepthTile(tlPos, l, geoTiles[1], frntImg, depthMtrx, chaosMtrx, [])
      end repeat

      -- Prepare for final draw and draw remaining slopes and floors
      tls2 = []
      repeat with tl in tls
        if (delL.findPos(tl) = void) then
          geo = afaMvLvlEdit(point(tl.locH, tl.locV), l)
          if (geo = 1) then
            -- Add to final draw list
            tls2.append(tl)
          else if (geo > 1) and (geo < 7) then
            frntImg = LDrawADepthTile(tl, l, geoTiles[geo - 1], frntImg, depthMtrx, chaosMtrx, [])
            delL[tl] = 1
          end if
        end if
      end repeat

      -- Draw everything else
      repeat with tlPos in tls2
        repeat with tl in tileSelection
          if (tl = void) then
            next repeat
          end if

          drawn = true
          mdPnt = point(((tl.sz.locH*0.5)+0.4999).integer - 1, ((tl.sz.locV*0.5)+0.4999).integer - 1)
          occupy = []

          repeat with x = 0 to tl.sz.locH-1 then
            repeat with y = 0 to tl.sz.locV-1 then
              loc = point(x,y) - mdPnt

              if (checkIfATileIsSolidAndSameMaterial(tlPos + loc, l, nm) = 0) then
                drawn = false
                exit repeat
              end if

              if (delL.findPos(tlPos + loc) <> void) then
                drawn = false
                exit repeat
              end if

              occupy.add(loc)
            end repeat
            if (drawn = false) then exit repeat
          end repeat

          if (drawn) then
            frntImg = LDrawADepthTile(tlPos, l, tl, frntImg, depthMtrx, chaosMtrx, occupy)
            repeat with q = 1 to occupy.count then
              delL[tlPos + occupy[q]] = 1
            end repeat
            exit repeat
          end if

        end repeat
      end repeat
      the randomSeed = savSeed

    end if
  end if
  return frntImg
end

-- Primarily intended for LRenderPatternMaterial, hence why it's here
--  depthMtrx = [[<matrix>, <mode> (FALSE = decrease, TRUE = increase)], ...]
--  chaosMtrx = [[<matrix>, <octaves>, <seed>], ...]
-- occupy specifies extra tiles to sample, relative to `loc`.
on LDrawADepthTile(loc, l, tl, frntImg, depthMtrx, chaosMtrx, occupy)
  offs = 0
  effLoc = loc
  effLoc.locH = restrict(effLoc.locH, 1, gLOprops.size.locH)
  effLoc.locV = restrict(effLoc.locV, 1, gLOprops.size.locV)
  if (occupy.count <= 1) then
    occupy = [point(0,0)]
  end if
  -- Contribution from Pattern Depth
  repeat with mtrx in depthMtrx
    repeat with occ in occupy
      loc2 = point(restrict(loc.locH + occ.locH, 1, gLOprops.size.locH), restrict(loc.locV + occ.locV, 1, gLOprops.size.locV))
      if (mtrx[2] = false) then
        offs = offs + (mtrx[1][loc2.locH][loc2.locV].float / 10.0)
      else
        offs = offs - (mtrx[1][loc2.locH][loc2.locV].float / 10.0)
      end if
    end repeat
  end repeat
  offs = offs / occupy.count
  -- Contribution from Pattern Chaos
  repeat with mtrx in chaosMtrx
    savSeed = the randomSeed
    octaves = mtrx[2]
    noiseVal = 0.0
    -- norm = 0.0
    seedLoc = loc
    repeat with q = 1 to octaves
      if (q = 1) then
        the randomSeed = seedForTile(seedLoc, mtrx[3] + q)
        randVal = (random(10)-5)
      else
        -- Bilinear Filter
        minPnt = floorPoint(seedLoc)
        maxPnt = minPnt + point(1,1)
        uv = seedLoc - minPnt
        the randomSeed = seedForTile(minPnt * q, mtrx[3] + q)
        x0y0 = (random(10)-5).float
        the randomSeed = seedForTile(point(maxPnt.locH, minPnt.locV) * q, mtrx[3] + q)
        x1y0 = (random(10)-5).float
        the randomSeed = seedForTile(maxPnt * q, mtrx[3] + q)
        x1y1 = (random(10)-5).float
        the randomSeed = seedForTile(point(minPnt.locH, maxPnt.locV * q), mtrx[3] + q)
        x0y1 = (random(10)-5).float
        y0 = lerp(x0y0, x1y0, uv.locH)
        y1 = lerp(x0y1, x1y1, uv.locH)
        randVal = lerp(y0, y1, uv.locV)
      end if
      
      -- norm = norm * 0.5 + 1.0
      noiseVal = noiseVal * 0.5 + (randVal / 10.0)
      seedLoc = seedLoc * 0.5
    end repeat
    -- noiseVal = noiseVal / norm
    offs = offs + noiseVal * (mtrx[1][effLoc.locH][effLoc.locV].float / 10.0)
    the randomSeed = savSeed
  end repeat
  return drawATileTile(loc.locH, loc.locV, l, tl, frntImg, [], offs)
end


on LDrawATileMaterial(q, c, l, nm) -- frntImg)
  if (DRCustomMatList.count >= 1) then
    matTl = DRCustomMatList[DRLastTL]
    if (matTl.nm <> nm) then
      repeat with inti = 1 to DRCustomMatList.count
        if (DRCustomMatList[inti].nm = nm) then
          matTl = DRCustomMatList[inti]
          DRLastTL = inti
          exit repeat
        end if
      end repeat
    end if
    if (matTl.nm = nm) then
      case l of
        1: 
          dp = 0
        2: 
          dp = 10
        otherwise:
          dp = 20
      end case
      qcp = point(q, c)
      LEMatrixT = gLEProps.matrix[q][c][l][1]
      
      if (matTl.findPos(#texture) <> VOID) then
        -- Unified texture materials (made by LB)
        mText = matTl.texture
        matFile = member("MatTexImport")
        if (DRLastTexImp <> nm) then
          member("MatTexImport").importFileInto("Materials/" & nm & "Texture.png")
          matFile.name = "MatTexImport"
          DRLastTexImp = nm
        end if
        matImg = matFile.image
        colored = (mText.tags.getPos("colored") > 0)
        if (colored) then
          gAnyDecals = 1
        end if
        effectColorA = (mText.tags.getPos("effectColorA") > 0)
        effectColorB = (mText.tags.getPos("effectColorB") > 0)
        size = mText.sz
        bsRect = rect((q mod size.locH) * 20, (c mod size.locV) * 20 + 1, ((q mod size.locH) + 1) * 20, ((c mod size.locV) + 1) * 20 + 1)
        if (colored) or (effectColorA) or (effectColorB) then
          gradRect = rect(size.locH * 20, 0, size.locH * 20, 0)
        end if
        pstRect = rect((q - 1) * 20, (c - 1) * 20, q * 20, c * 20) - rect(gRenderCameraTilePos, gRenderCameraTilePos) * 20
        case LEMatrixT of
          1:
            d = -1
            repeat with ps = 1 to mText.repeatL.count
              gtRect = bsRect + rect(0, size.locV * 20 * (ps - 1), 0, size.locV * 20 * (ps - 1))
              repeat with ps2 = 1 to mText.repeatL[ps]
                d = d + 1 
                if (d + dp > 29) then
                  exit repeat
                else
                  lstr = string(d + dp)
                  member("layer" & lstr).image.copyPixels(matImg, pstRect, gtRect, {#ink:36})
                  if (colored) then
                    if (effectColorA = 0) and (effectColorB = 0) then
                      member("layer" & lstr & "dc").image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:36})
                    end if
                  end if
                  if (effectColorA) then
                    member("gradientA" & lstr).image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                  end if
                  if (effectColorB) then
                    member("gradientB" & lstr).image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                  end if
                end if
              end repeat
            end repeat
          2, 3, 4, 5:
            rct = rect((q - 1) * 20, (c - 1) * 20, q * 20, c * 20)
            case LEMatrixT of
              5:
                rct = [point(rct.left, rct.top), point(rct.left, rct.top), point(rct.right, rct.bottom), point(rct.left, rct.bottom)]  
              4:
                rct = [point(rct.right, rct.top), point(rct.right, rct.top), point(rct.left, rct.bottom), point(rct.right, rct.bottom)]
              3:
                rct = [point(rct.left, rct.bottom), point(rct.left, rct.bottom), point(rct.right, rct.top), point(rct.left, rct.top)]
              2:
                rct = [point(rct.right, rct.bottom), point(rct.right, rct.bottom), point(rct.left, rct.top), point(rct.right, rct.top)]
            end case
            rct = rct - [gRenderCameraTilePos, gRenderCameraTilePos, gRenderCameraTilePos, gRenderCameraTilePos] * 20
            d = -1
            repeat with ps = 1 to mText.repeatL.count
              gtRect = bsRect + rect(0, size.locV * 20 * (ps - 1), 0, size.locV * 20 * (ps - 1))
              repeat with ps2 = 1 to mText.repeatL[ps]
                d = d + 1 
                if (d + dp > 29) then
                  exit repeat
                else
                  lstr = string(d + dp)
                  lri = member("layer" & lstr).image
                  lri.copyPixels(matImg, pstRect, gtRect, {#ink:36})
                  lri.copyPixels(DRPxl, rct, DRPxlRect, {#color:DRWhite})
                  if (colored) then
                    if (effectColorA = 0) and (effectColorB = 0) then
                      lri = member("layer" & lstr & "dc").image
                      lri.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:36})
                      lri.copyPixels(DRPxl, rct, DRPxlRect, {#color:DRWhite})
                    end if
                  end if
                  if (effectColorA) then
                    lri = member("gradientA" & lstr).image
                    lri.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                    lri.copyPixels(DRPxl, rct, DRPxlRect, {#color:DRWhite})
                  end if
                  if (effectColorB) then
                    lri = member("gradientB" & lstr).image
                    lri.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                    lri.copyPixels(DRPxl, rct, DRPxlRect, {#color:DRWhite})
                  end if
                end if
              end repeat
            end repeat
          6:
            if (mText.tags.getPos("textureOnFloor") > 0) then
              rct = rect((q - 1) * 20, (c - 1) * 20 + 10, q * 20, c * 20) - rect(gRenderCameraTilePos, gRenderCameraTilePos) * 20
              d = -1
              repeat with ps = 1 to mText.repeatL.count
                gtRect = bsRect + rect(0, size.locV * 20 * (ps - 1), 0, size.locV * 20 * (ps - 1))
                repeat with ps2 = 1 to mText.repeatL[ps]
                  d = d + 1 
                  if (d + dp > 29) then
                    exit repeat
                  else
                    lstr = string(d + dp)
                    lri = member("layer" & lstr).image
                    lri.copyPixels(matImg, pstRect, gtRect, {#ink:36})
                    lri.copyPixels(DRPxl, rct, DRPxlRect, {#color:DRWhite})
                    if (colored) then
                      if (effectColorA = 0) and (effectColorB = 0) then
                        lri = member("layer" & lstr & "dc").image
                        lri.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:36})
                        lri.copyPixels(DRPxl, rct, DRPxlRect, {#color:DRWhite})
                      end if
                    end if
                    if (effectColorA) then
                      lri = member("gradientA" & lstr).image
                      lri.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                      lri.copyPixels(DRPxl, rct, DRPxlRect, {#color:DRWhite})
                    end if
                    if (effectColorB) then
                      lri = member("gradientB" & lstr).image
                      lri.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                      lri.copyPixels(DRPxl, rct, DRPxlRect, {#color:DRWhite})
                    end if
                  end if
                end repeat
              end repeat
            end if
        end case
      end if
      case LEMatrixT of
        1:
          if (matTl.findPos(#block) <> VOID) then
            fl = matTl.block
            rct2 = rect((q - 1) * 20 - 5, (c - 1) * 20 - 5, q * 20 + 5, c * 20 + 5) - rect(gRenderCameraTilePos, gRenderCameraTilePos) * 20
            colored = (fl.tags.getPos("colored") > 0)
            if (colored) then
              gAnyDecals = 1
            end if
            effectColorA = (fl.tags.getPos("effectColorA") > 0)
            effectColorB = (fl.tags.getPos("effectColorB") > 0)
            tlRnd = fl.rnd
            rnd = random(tlRnd) - 1
            matFile = member("MatImport")
            if (DRLastMatImp <> nm) then
              member("MatImport").importFileInto("Materials/" & nm & ".png")
              matFile.name = "MatImport"
              DRLastMatImp = nm
            end if
            matImg = matFile.image
            repeat with f = 1 to 4
              case f of
                1:
                  profL = [point(-1, 0), point(0, -1)]
                  gtAtV = 2
                  pstRect = rct2 + rect(0, 0, -10, -10)
                2:
                  profL = [point(1, 0), point(0, -1)]
                  gtAtV = 4
                  pstRect = rct2 + rect(10, 0, 0, -10)
                3:
                  profL = [point(1, 0), point(0, 1)]
                  gtAtV = 6
                  pstRect = rct2 + rect(10, 10, 0, 0)
                otherwise:
                  profL = [point(-1, 0), point(0, 1)]
                  gtAtV = 8
                  pstRect = rct2 + rect(0, 10, -10, 0)
              end case
              ID = ""
              repeat with dr in profL
                ID = ID & string(LIsMyTileSetOpenToThisTile(nm, qcp + dr, l))
              end repeat
              if (ID = "11") then
                if ([1,2,3,4,5].getPos(LIsMyTileSetOpenToThisTile(nm, qcp + profL[1] + profL[2], l)) > 0) then
                  gtAtH = 10
                  gtAtV = 2
                else
                  gtAtH = 8
                end if
              else
                gtAtH = [0, "00", 0, "01", 0, "10"].getPos(ID)
              end if
              if (gtAtH = 4) then
                if (gtAtV = 6) then
                  gtAtV = 4
                else if (gtAtV = 8) then
                  gtAtV = 2
                end if
              else if (gtAtH = 6) then
                if (gtAtV = 4) or (gtAtV = 8) then
                  gtAtV = gtAtV - 2
                end if
              end if
              bsRect = rect((gtAtH - 1) * 10 - 5 + 100 * rnd, (gtAtV - 1) * 10 - 5, gtAtH * 10 + 5 + 100 * rnd, gtAtV * 10 + 5)
              --frntImg.copyPixels(matImg, pstRect, bsRect, {#ink:36})
              if (colored) or (effectColorA) or (effectColorB) then
                gradRect = rect(100 * tlRnd, 0, 100 * tlRnd, 0)
              end if
              d = -1
              repeat with ps = 1 to fl.repeatL.count
                gtRect = bsRect + rect(0, 80 * (ps - 1), 0, 80 * (ps - 1))
                repeat with ps2 = 1 to fl.repeatL[ps]
                  d = d + 1 
                  if (d + dp > 29) then
                    exit repeat
                  else
                    lstr = string(d + dp)
                    member("layer" & lstr).image.copyPixels(matImg, pstRect, gtRect, {#ink:36})
                    if (colored) then
                      if (effectColorA = 0) and (effectColorB = 0) then
                        member("layer" & lstr & "dc").image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:36})
                      end if
                    end if
                    if (effectColorA) then
                      member("gradientA" & lstr).image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                    end if
                    if (effectColorB) then
                      member("gradientB" & lstr).image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                    end if
                  end if
                end repeat
              end repeat
            end repeat
          end if
        2,3,4,5:
          if (matTl.findPos(#slope) <> VOID) then
            matFile = member("MatSlpImport")
            if (DRLastSlpImp <> nm) then
              member("MatSlpImport").importFileInto("Materials/" & nm & "Slopes.png")
              matFile.name = "MatSlpImport"
              DRLastSlpImp = nm
            end if
            fl = matTl.slope
            matImg = matFile.image
            tlRnd = fl.rnd
            rnd = random(tlRnd) - 1
            colored = (fl.tags.getPos("colored") > 0)
            if (colored) then
              gAnyDecals = 1
            end if
            effectColorA = (fl.tags.getPos("effectColorA") > 0)
            effectColorB = (fl.tags.getPos("effectColorB") > 0)
            slp = gLEProps.matrix[q][c][l][1]
            askDirs = [0, [point(-1, 0), point(0, 1)], [point(0, 1), point(1, 0)], [point(-1, 0), point(0, -1)], [point(0, -1), point(1, 0)]]
            myAskDirs = askDirs[slp]
            pstRect = rect((q - 1) * 20 - 5, (c - 1) * 20 - 5, q * 20 + 5, c * 20 + 5) - rect(gRenderCameraTilePos, gRenderCameraTilePos) * 20
            if (colored) or (effectColorA) or (effectColorB) then
              gradRect = rect(120 * tlRnd, 0, 120 * tlRnd, 0)
            end if
            repeat with ad = 1 to myAskDirs.count
              bsRect = rect(5 + 60 * (ad = 2) + 120 * rnd, 5 + 30 * (slp - 2), 35 + 60 * (ad = 2) + 120 * rnd, 35 + 30 * (slp - 2))
              if (LIsMyTileSetOpenToThisTile(nm, qcp + myAskDirs[ad], l)) then
                bsRect = bsRect + rect(30, 0, 30, 0)
              end if
              d = -1
              repeat with ps = 1 to fl.repeatL.count
                gtRect = bsRect + rect(0, 130 * (ps - 1), 0, 130 * (ps - 1))
                repeat with ps2 = 1 to fl.repeatL[ps]
                  d = d + 1 
                  if (d + dp > 29) then
                    exit repeat
                  else
                    lstr = string(d + dp)
                    member("layer" & lstr).image.copyPixels(matImg, pstRect, gtRect, {#ink:36})
                    if (colored) then
                      if (effectColorA = 0) and (effectColorB = 0) then
                        member("layer" & lstr & "dc").image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:36})
                      end if
                    end if
                    if (effectColorA) then
                      member("gradientA" & lstr).image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                    end if
                    if (effectColorB) then
                      member("gradientB" & lstr).image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                    end if
                  end if
                end repeat
              end repeat
            end repeat
          end if
        6:
          if (matTl.findPos(#floor) <> VOID) then
            matFile = member("MatFlrImport")
            if (DRLastFlrImp <> nm) then
              member("MatFlrImport").importFileInto("Materials/" & nm & "Floor.png")
              matFile.name = "MatFlrImport"
              DRLastFlrImp = nm
            end if
            fl = matTl.floor
            matImg = matFile.image
            tlRnd = fl.rnd
            rnd = random(tlRnd) - 1
            colored = (fl.tags.getPos("colored") > 0)
            if (colored) then
              gAnyDecals = 1
            end if
            effectColorA = (fl.tags.getPos("effectColorA") > 0)
            effectColorB = (fl.tags.getPos("effectColorB") > 0)
            vbf = 20 * fl.bfTiles
            pstRect = rect((q - 1) * 20 - vbf, (c - 1) * 20 - vbf, q * 20 + vbf, c * 20 + vbf) - rect(gRenderCameraTilePos, gRenderCameraTilePos) * 20
            bfCal = 20 + 40 * fl.bfTiles
            bsRect = rect(0, 1, bfCal, bfCal + 1)
            bsRect = bsRect + rect(bsRect.width * rnd, 0, bsRect.width * rnd, 0)
            if (colored) or (effectColorA) or (effectColorB) then
              gradRect = rect(bfCal * tlRnd, 0, bfCal * tlRnd, 0)
            end if
            d = -1
            repeat with ps = 1 to fl.repeatL.count
              gtRect = bsRect + rect(0, bfCal * (ps - 1), 0, bfCal * (ps - 1))
              repeat with ps2 = 1 to fl.repeatL[ps]
                d = d + 1 
                if (d + dp > 29) then
                  exit repeat
                else
                  lstr = string(d + dp)
                  member("layer" & lstr).image.copyPixels(matImg, pstRect, gtRect, {#ink:36})
                  if (colored) then
                    if (effectColorA = 0) and (effectColorB = 0) then
                      member("layer" & lstr & "dc").image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:36})
                    end if
                  end if
                  if (effectColorA) then
                    member("gradientA" & lstr).image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                  end if
                  if (effectColorB) then
                    member("gradientB" & lstr).image.copyPixels(matImg, pstRect, gtRect + gradRect, {#ink:39})
                  end if
                end if
              end repeat
            end repeat
          end if
      end case
      
      if (matTl.findPos(#pipelike) <> VOID) then
        -- Pipe-like materials (made by LudoCrypt)
        matPipes = matTl.pipelike
        randCount = matPipes.rnd
        if (matPipes.findPos(#depths) <> VOID) then
          pipeDepths = matPipes.depths
        else
          pipeDepths = [2, 3, 6, 7]
        end if
        matFile = member("MatPipelikeImport")
        if (DRLastPipeImp <> nm) then
          member("MatPipelikeImport").importFileInto("Materials/" & nm & "Pipes.png")
          matFile.name = "MatPipelikeImport"
          DRLastPipeImp = nm
        end if
        matImg = matFile.image
        effectColorA = (matPipes.tags.getPos("effectColorA") > 0)
        effectColorB = (matPipes.tags.getPos("effectColorB") > 0)
        colored = (matPipes.tags.getPos("colored") > 0)
        if (colored) then
          gAnyDecals = 1
        end if
        savSeed = the randomSeed
        the randomSeed = seedForTile(qcp, gLOprops.tileSeed + l)
        gtPos = point(0, 0)
        case LEMatrixT of
          1:
            nbrs = ""
            repeat with dir in [point(-1, 0), point(0, -1), point(1, 0), point(0, 1)]
              if (random(2) = 1) and (afaMvLvlEdit(qcp + dir, l) = 1) then
                nbrs = nbrs & "1"
              else
                nbrs = nbrs & string(LIsMyTileSetOpenToThisTile(nm, qcp + dir, l))
              end if
            end repeat
            case nbrs of
              "0101":
                gtPos = point(2, 2)
              "1010":
                gtPos = point(4, 2)
              "1111":
                gtPos = point(6, 2)
              "0111":
                gtPos = point(8, 2)
              "1101":
                gtPos = point(10, 2)
              "1110":
                gtPos = point(12, 2)
              "1011":
                gtPos = point(14, 2)
              "0011":
                gtPos = point(16, 2)
              "1001":
                gtPos = point(18, 2)
              "1100":
                gtPos = point(20, 2)
              "0110":
                gtPos = point(22, 2)
              "1000":
                gtPos = point(24, 2)
              "0010":
                gtPos = point(26, 2)
              "0100":
                gtPos = point(28, 2)
              "0001":
                gtPos = point(30, 2)
              "0000":
                gtPos = point(40, 2)
            end case
          3:
            gtPos = point(32, 2)
          2:
            gtPos = point(34, 2)
          4:
            gtPos = point(36, 2)
          5:
            gtPos = point(38, 2)
          6:
            gtPos = point(42, 2)   
        end case
        lrm110 = (l - 1) * 10
        gtPos.locV = random(randCount) * 2
        repeat with d = lrm110 to lrm110 + 9
          if (pipeDepths.getPos(d - lrm110) > 0) then
            rct = rect((gtPos.locH - 1) * 20 - 10, (gtPos.locV - 1) * 20 - 9, gtPos.locH * 20 + 10, gtPos.locV * 20 + 11)
            realRect = rect((q - 1 - gRenderCameraTilePos.locH) * 20 - 10, (c - 1 - gRenderCameraTilePos.locV) * 20 - 10, (q - gRenderCameraTilePos.locH) * 20 + 10, (c - gRenderCameraTilePos.locV) * 20 + 10)
            member("layer" & string(d)).image.copyPixels(matImg, realRect, rct, {#ink:36})
            if (effectColorA) then
              member("gradientA" & string(d)).image.copyPixels(matImg, realRect, rct + rect(840, 0, 840, 0), {#ink:39})
            end if
            if (effectColorB) then
              member("gradientB" & string(d)).image.copyPixels(matImg, realRect, rct + rect(840, 0, 840, 0), {#ink:39})
            end if
            if (colored) then
              if (effectColorA = FALSE) then
                if (effectColorB = FALSE) then
                  member("layer" & string(d) & "dc").image.copyPixels(matImg, realRect, rct + rect(840, 0, 840, 0), {#ink:36})
                end if 
              end if
            end if
          else
            gtPos.locV = random(randCount) * 2
          end if
        end repeat
        the randomSeed = savSeed
      end if
      
      if (matTl.findPos(#trash) <> VOID) then
        -- Trash-like materials (made by LudoCrypt)
        matTrash = matTl.trash
        trashRnd = matTrash.rnd
        trashSz = matTrash.pixelSize
        if (matTrash.findPos(#density) <> VOID) then
          trashDensity = matTrash.density
        else
          trashDensity = 1
        end if
        if (matTrash.findPos(#depths) <> VOID) then
          trashDepths = matTrash.depths
        else
          trashDepths = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        end if
        matFile = member("MatTrshImport")
        if (DRLastTrshImp <> nm) then
          member("MatTrshImport").importFileInto("Materials/" & nm & "Trash.png")
          matFile.name = "MatTrshImport"
          DRLastTrshImp = nm
        end if
        matTexImg = matFile.image
        if (trashRnd > 0) then
          savSeed = the randomSeed
          the randomSeed = gLOprops.tileSeed + l + q + c * gLEprops.matrix.count
          clrs = [color(255, 0, 0), color(0, 255, 0), color(0, 0, 255)]
          trashLr = [0, 10, 20][l]
          midTlTr = giveMiddleOfTile(qcp - gRenderCameraTilePos)
          repeat with q = 1 to (2 + (random(trashDensity * 2) - 1) + trashDensity)
            layerOfTrash = random(10) - 1
            if (trashDepths.getPos(layerOfTrash) > 0) then
              gtR = random(trashRnd)
              pntRct = midTlTr - point(11, 11) + point(random(21), random(21))
              trashSzDiv2 = trashSz / 2.0
              member("layer" & string(trashLr + layerOfTrash)).image.copyPixels(matTexImg, rotateToQuadLB(rect(pntRct - trashSzDiv2, pntRct + trashSzDiv2), degToVec(random(360))),  rect(trashSz.locH * (gtR - 1), 1, trashSz.locH * gtR, trashSz.locV + 1), {#color:clrs[random(3)], #ink:36})
            end if
          end repeat
          the randomSeed = savSeed
        end if
      end if
      
      if (matTl.findPos(#densePipelike) <> VOID) then
        -- Circuit-like materials (made by LudoCrypt)
        matDensePipelike = matTl.densePipelike
        
        randCount = matDensePipelike.rnd
        
        if (matDensePipelike.findPos(#depths) <> VOID) then
          densepipeDepths = matDensePipelike.depths
        else
          densepipeDepths = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        end if
        
        if (matDensePipelike.findPos(#shallow) <> VOID) then
          shallowPipes = matDensePipelike.shallow
        else
          shallowPipes = 0
        end if
        
        matFile = member("MatDensePipelikeImport")
        if (DRLastDensePipeImp <> nm) then
          member("MatDensePipelikeImport").importFileInto("Materials/" & nm & "DensePipes.png")
          matFile.name = "MatDensePipelikeImport"
          DRLastDensePipeImp = nm
        end if
        matImg = matFile.image
        
        effectColorA = (matDensePipelike.tags.getPos("effectColorA") > 0)
        effectColorB = (matDensePipelike.tags.getPos("effectColorB") > 0)
        colored = (matDensePipelike.tags.getPos("colored") > 0)
        if (colored) then
          gAnyDecals = 1
        end if
        savSeed = the randomSeed
        the randomSeed = seedForTile(qcp, gLOprops.tileSeed + l)
        
        pos = giveMiddleOfTile(qcp-gRenderCameraTilePos)
        pstLr = DPStartLayerOfTile(qcp, l)
        if (shallowPipes) then
          pstLr = l * 10 - 10
        end if
        
        if(afaMvLvlEdit(qcp, l) > 1)then
          a = afaMvLvlEdit(qcp, l)
          var = 16 
          case a of
            2: var = 20
            3: var = 19
            4: var = 17
            5: var = 18
            6: var = 21
            9: var = 22
          end case
          
          rand = random(randCount)
          
          repeat with d = pstLr to (l * 10)-1 then
            if (densepipeDepths.getPos(d - ((l-1) * 10)) > 0 or (d < ((l-1)*10) and shallowPipes = 0)) then
              member("layer" & string(d)).image.copyPixels(matImg, rect(pos-point(20,20), pos+point(20,20)), rect((var-1)*40,1+(rand-1)*40,var*40,1+rand*40), {#ink:36})
              if (effectColorA) then
                member("gradientA" & string(d)).image.copyPixels(matImg, rect(pos-point(20,20), pos+point(20,20)), rect((var-1)*40,1+(rand-1)*40,var*40,1+rand*40) + rect(840, 0, 840, 0), {#ink:39})
              end if
              if (effectColorB) then
                member("gradientB" & string(d)).image.copyPixels(matImg, rect(pos-point(20,20), pos+point(20,20)), rect((var-1)*40,1+(rand-1)*40,var*40,1+rand*40) + rect(840, 0, 840, 0), {#ink:39})
              end if
              if (colored) then
                if (effectColorA = FALSE) then
                  if (effectColorB = FALSE) then
                    member("layer" & string(d) & "dc").image.copyPixels(matImg, rect(pos-point(20,20), pos+point(20,20)), rect((var-1)*40,1+(rand-1)*40,var*40,1+rand*40) + rect(840, 0, 840, 0), {#ink:36})
                  end if 
                end if
              end if
            end if
          end repeat
        else
          lst = ["0000", "1111", "0101", "1010", "0001", "1000", "0100", "0010", "1001", "1100", "0110", "0011", "1011", "1101", "1110", "0111"]
          
          lftDp = DPStartLayerOfTile(qcp+point(-1,0), l) 
          rghtDp = DPStartLayerOfTile(qcp+point(1,0), l)
          tpDp = DPStartLayerOfTile(qcp+point(0,-1), l)
          bttmDp = DPStartLayerOfTile(qcp+point(0,1), l)
          
          repeat with d = pstLr to (l * 10)-1 then
            if (densepipeDepths.getPos(d - ((l-1) * 10)) > 0 or (d < ((l-1)*10) and shallowPipes = 0)) then
              lft =  solidAfaMv(qcp+point(-1,0), l) * DPCircuitConnection(qcp+point(-1,0), d).locH * (lftDp<=d)
              rght = solidAfaMv(qcp+point(1,0), l) * DPCircuitConnection(qcp, d).locH * (rghtDp<=d) 
              tp =  solidAfaMv(qcp+point(0,-1), l) * DPCircuitConnection(qcp+point(0,-1), d).locV* (tpDp<=d) 
              bttm = solidAfaMv(qcp+point(0,1), l) * DPCircuitConnection(qcp, d).locV * (bttmDp<=d) 
              
              if (shallowPipes) then
                lft =  solidAfaMv(qcp+point(-1,0), l) * DPCircuitConnection(qcp+point(-1,0), d).locH
                rght = solidAfaMv(qcp+point(1,0), l) * DPCircuitConnection(qcp, d).locH
                tp =  solidAfaMv(qcp+point(0,-1), l) * DPCircuitConnection(qcp+point(0,-1), d).locV 
                bttm = solidAfaMv(qcp+point(0,1), l) * DPCircuitConnection(qcp, d).locV 
              end if  
              
              if(afaMvLvlEdit(qcp+point(-1,0), l)>1 and ((afaMvLvlEdit(qcp+point(-1,0), l) <> 9)))then
                lft = 1
              end if
              if(afaMvLvlEdit(qcp+point(1,0), l)>1 and ((afaMvLvlEdit(qcp+point(1,0), l) <> 9)))then
                rght = 1
              end if
              if(afaMvLvlEdit(qcp+point(0,-1), l)>1 and ((afaMvLvlEdit(qcp+point(0,-1), l) <> 9)))then
                tp = 1
              end if
              if(afaMvLvlEdit(qcp+point(0,1), l)>1 and ((afaMvLvlEdit(qcp+point(0,1), l) <> 9)))then
                bttm = 1
              end if
              
              var = lst.getPos((string(lft) & string(tp) & string(rght) & string(bttm)))
              rand = random(randCount)
              
              member("layer" & string(d)).image.copyPixels(matImg, rect(pos-point(20,20), pos+point(20,20)), rect((var-1)*40,1+(rand-1)*40,var*40,1+rand*40), {#ink:36})
              if (effectColorA) then
                member("gradientA" & string(d)).image.copyPixels(matImg, rect(pos-point(20,20), pos+point(20,20)), rect((var-1)*40,1+(rand-1)*40,var*40,1+rand*40) + rect(840, 0, 840, 0), {#ink:39})
              end if
              if (effectColorB) then
                member("gradientB" & string(d)).image.copyPixels(matImg, rect(pos-point(20,20), pos+point(20,20)), rect((var-1)*40,1+(rand-1)*40,var*40,1+rand*40) + rect(840, 0, 840, 0), {#ink:39})
              end if
              if (colored) then
                if (effectColorA = FALSE) then
                  if (effectColorB = FALSE) then
                    member("layer" & string(d) & "dc").image.copyPixels(matImg, rect(pos-point(20,20), pos+point(20,20)), rect((var-1)*40,1+(rand-1)*40,var*40,1+rand*40) + rect(840, 0, 840, 0), {#ink:36})
                  end if 
                end if
              end if
            end if
          end repeat
        end if
        the randomSeed = savSeed
      end if
      
      if (matTl.findPos(#autofit)) then
        -- You should not be here, see LRenderTileMaterial instead
      end if
      
      -- note to future people: this is where more material types go
    end if
  end if
  --return frntImg
end
