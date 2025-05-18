global gTiles, tileSetIndex, gCurrentRenderCamera, gAnyDecals, firstCamRepeat, solidMtrx, gLOprops, gLEprops, gTEprops, gDRMatFixes, gDRInvI, grimeActive, grimeOnGradients, bkgFix, gRRSpreadsMore, slimeFxt
global DRWhite, DRPxl, DRPxlRect, DRDarkSlimeFix

on exitFrame me
  DRPxl = member("pxl").image
  DRPxlRect = DRPxl.rect
  DRWhite = color(255, 255, 255)
  slimeFxt = getBoolConfig("Slime always affects editor decals")
  DRDarkSlimeFix = getBoolConfig("Dark Slime fix")
  gRRSpreadsMore = getBoolConfig("Rough Rock spreads more")
  grimeActive = getBoolConfig("Grime")
  grimeOnGradients = getBoolConfig("Grime on gradients")
  bkgFix = getBoolConfig("Gradients with BackgroundScenes fix")
  gDRMatFixes = getBoolConfig("Material fixes")
  gDRInvI = getBoolConfig("Invisible material fix")
  member("blackOutImg1").image = image(1, 1, 1)
  member("blackOutImg2").image = image(1, 1, 1)
  member("GradientOutput").image = image(1,1,1)
  if checkMinimize() then
    _player.appMinimize()
    
  end if
  if checkExit() then
    _player.quit()
  end if
  if checkExitRender() then
    _movie.go(9)
  end if
  
  put "Start render"
  
  gLOprops.pal = 1
  
  firstCamRepeat = true
  gCurrentRenderCamera = 0
  gAnyDecals = 0
  me.createShortCuts()
  
  tileSetIndex = []
  --repeat with q = 1 to gTiles[1].tls.count then
  --    if gTiles[1].tls[q].renderType = "unified" then
  --      member("tileSet"&string(q)).image = image(1,1,32)
  --      sav = member("tileSet"&string(q))
  --      member("tileSet"&string(q)).importFileInto("Graphics\tileSet" &  gTiles[1].tls[q].nm &".png")
  --      sav.name = "tileSet"&string(q)
  --      tileSetIndex.add(gTiles[1].tls[q].nm)
  --    end if
  --end repeat
  
  
  
  solidMtrx = []
  if (getBoolConfig("Trash and Small pipes non solid")) then
    nonSolidTileSets = ["Small Pipes", "Trash"]--"Invisible"
  else
    nonSolidTileSets = []
  end if
  if (gDRInvI = FALSE) then
    nonSolidTileSets.add("Invisible")
  end if
  repeat with q = 1 to gLOprops.size.loch then
    l = []
    repeat with c = 1 to gLOprops.size.locv then
      cell = []
      repeat with d = 1 to 3 then
        ad = 0
        if (gLEprops.matrix[q][c][d][1] = 1)and(gLEProps.matrix[q][c][d][2].getPos(11)=0) then
          tl = gTEprops.tlMatrix[q][c][d]
          if (tl.tp = "default")or(tl.tp = "material") then
            testMat = tl.data
            if tl.tp = "default" then
              testMat = gTEprops.defaultMaterial
            end if
            --            if testMat = "small pipes" then
            --              put (nonSolidTileSets.getPos(testMat)=0)
            --              put (nonSolidTileSets.getPos(testMat)=0)
            --            end if
            ad = (nonSolidTileSets.getPos(testMat)=0)
          else if (tl.tp = "tileHead")or(tl.tp = "tileBody") then
            tlPs = tl.data[1]
            if tl.tp = "tileBody" then
              tlPs = void
              if (tl.data[1].locV > 0) and (tl.data[1].locH > 0) and (tl.data[1].locV < gLOprops.size.locV) and (tl.data[1].locH < gLOprops.size.locH) then
                if ilk(gTEprops.tlMatrix[tl.data[1].locH][tl.data[1].locV][tl.data[2]].data) = #list then
                  tlPs = gTEprops.tlMatrix[tl.data[1].locH][tl.data[1].locV][tl.data[2]].data[1]
                end if
              end if
            end if
            ad = 1
            
            if (tlPs <> void) then
              if (tlPs.locH > 2) and (tlPs.locH <= gTiles.count) then
                if tlPs.locV <= gTiles[tlPs.locH].tls.count then
                  if gTiles[tlPs.locH].tls[tlPs.locV].tags <> void then
                    ad = (gTiles[tlPs.locH].tls[tlPs.locV].tags.getPos("nonSolid")=0)
                  end if
                end if
              end if
            end if
            
            -- put "added" && (gTiles[tlPs.locH].tls[tlPs.locV].tags.getPos("nonSolid")=0) && "to solidmatrix from tile" && gTiles[tlPs.locH].tls[tlPs.locV].nm
          end if
        end if
        
        cell.add(ad)
      end repeat
      -- l.add([(gLEprops.matrix[q][c][1][1] = 1), (gLEprops.matrix[q][c][2][1] = 1), (gLEprops.matrix[q][c][3][1] = 1)])
      l.add(cell)  
    end repeat
    solidMtrx.add(l)
  end repeat
end



on createShortCuts me
  -- put "init"
  --  member("shortcutdotsImg").image = image(1040, 800, 1)
  
  global gShortcuts
  
  gShortcuts = [#scs:[], #indexL:[]]
  global gLEprops, gLOprops
  
  repeat with q = 2 to gLEprops.matrix.count-1 then
    repeat with c = 2 to gLEprops.matrix[1].count-1 then
      if gLEprops.matrix[q][c][1][2].getPos(4) > 0 then
        didItWork = 1
        tp = "shortCut"
        
        holeDir = point(0,0)
        
        stps = 0
        pos = point(q,c)
        stp = 0
        lastDir = point(0,0)
        rpt = 0
        repeat while stp = 0 then
          rpt = rpt + 1
          if rpt > 1000 then
            didItWork = 0
            stp = 1
          end if
          dirsL = [point(-1, 0), point(0,-1), point(1,0), point(0,1)]
          dirsL.deleteOne(lastDir)
          dirsL.addAt(1, lastDir)
          dirsL.deleteOne(-lastDir)
          repeat with dir in dirsL then
            if (pos+dir).inside(rect(1,1,gLOprops.size.loch+1,gLOprops.size.locv+1))then
              if gLEprops.matrix[pos.locH+dir.locH][pos.locV+dir.locV][1][2].getPos(6)>0 then
                stp = 1
                tp = "playerHole"
                pos = point(q,c)
                
                -- put point(q,c) && "dsgfsd"
                lastDir = dir
                exit repeat
              else if gLEprops.matrix[pos.locH+dir.locH][pos.locV+dir.locV][1][2].getPos(7)>0 then
                stp = 1
                tp = "lizardHole"
                pos = point(q,c)
                lastDir = dir
                exit repeat
              else if gLEprops.matrix[pos.locH+dir.locH][pos.locV+dir.locV][1][2].getPos(19)>0 then
                stp = 1
                tp = "WHAMH"
                pos = point(q,c)
                lastDir = dir
                exit repeat
              else if gLEprops.matrix[pos.locH+dir.locH][pos.locV+dir.locV][1][2].getPos(21)>0 then
                stp = 1
                tp = "scavengerHole"
                pos = point(q,c)
                lastDir = dir
                exit repeat
              else if gLEprops.matrix[pos.locH+dir.locH][pos.locV+dir.locV][1][2].getPos(4)>0 then
                
                stp = 1
                pos = pos+dir
                lastDir = dir
                
                exit repeat
              else if gLEprops.matrix[pos.locH+dir.locH][pos.locV+dir.locV][1][2].getPos(5)>0 then
                stps = stps + 1
                pos = pos+dir
                lastDir = dir
                exit repeat
              end if
            end if
          end repeat
          
          if holeDir = point(0,0) then
            holeDir = lastDir
          end if
        end repeat
        
        if didItWork then
          gShortcuts.indexL.add(point(q,c))
          gShortcuts.scs.add(tp)
        end if
      end if
    end repeat
  end repeat
  
  --  repeat with q = 1 to 52 then
  --    repeat with c = 1 to 40 then
  --      if gMatrix[q][c][1][2].getPos(6)>0 then
  --        gShortcuts.exitsIndxL.add(point(q,c))
  --      end if
  --    end repeat
  --  end repeat
  --  
  --  repeat with scs in gShortcuts.scs then
  --    scs.destIndex = gShortCuts.indexL.getPos(scs.destination)
  --  end repeat
  -- put "shortCutsCreated"
end
