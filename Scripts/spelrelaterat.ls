on giveGridPos(pos)
  return point(((pos.locH.float / 20.0) + 0.4999).integer, ((pos.locV.float / 20.0) + 0.4999).integer)
end

on giveMiddleOfTile(pos)
  return point(pos.locH * 20 - 10, pos.locV * 20 - 10)
end

on restrict(val, low, high)
  if (val < low) then
    return low
  else if (val > high) then
    return high
  else
    return val
  end if
end

on restrictWithFlip(val, low, high)
  if (val < low) then
    return val + (high - low) + 1
  else if (val > high) then
    return val - (high - low) - 1
  else
    return val
  end if
end

on afaMvLvlEdit(pos, layer)
  global gLEProps, gLOprops
  if pos.inside(rect(1, 1, gLOprops.size.locH + 1, gLOprops.size.locV + 1)) then
    return gLEProps.matrix[pos.locH][pos.locV][layer][1]
  else
    return 1
  end if
end

on solidAfaMv(pos, layer)
  global solidMtrx, gLOprops
  if pos.inside(rect(1, 1, gLOprops.size.locH + 1, gLOprops.size.locV + 1)) then
    return solidMtrx[pos.locH][pos.locV][layer]
  else
    return 1
  end if
end

on withinBoundsOfLevel(pos)
  global solidMtrx
  if pos.inside(rect(2, 2, solidMtrx.count, solidMtrx[1].count)) then
    return 1
  else
    return 0
  end if
end

on depthPnt(pnt, dpt)
  return (pnt - point(700, 800 / 3)) / ((10 + dpt * 0.025) * 0.1) + point(700, 800 / 3)
end

on seedForTile(tile, effectSeed)
  global gLEprops
  return effectSeed + tile.locH + tile.locV * gLEprops.matrix.count
end

on copyPixelsToEffectColor(gdLayer, lr, rct, getMember, gtRect, zbleed, blnd)
  global DRPxl
  if (blnd = VOID) then
    blnd = 1.0
  end if
  if (gdLayer <> "C") and (blnd > 0) then
    lr = lr.integer
    if (lr < 0) then lr = 0
    else if (lr > 29) then lr = 29
    gtImg = member(getMember).image
    if (blnd <> 0) and (blnd <> VOID) then
      dmpImg = gtImg.duplicate()
      dmpImg.copyPixels(DRPxl, dmpImg.rect, rect(0, 0, 1, 1), {#blend:100.0 * (1.0 - blnd), #color:color(255, 255, 255)})
      gtImg = dmpImg
    end if   
    member("gradient" & gdLayer & string(lr)).image.copyPixels(gtImg, rct, gtRect, {#ink:39})
    if (zbleed > 0) then
      if (zbleed < 1) then
        dmpImg = gtImg.duplicate()
        dmpImg.copyPixels(DRPxl, dmpImg.rect, rect(0, 0, 1, 1), {#blend:100.0 * (1.0 - zbleed), #color:color(255, 255, 255)})
        gtImg = dmpImg
      end if
      nxt = lr + 1
      if (nxt > 29) then nxt = 29
      member("gradient" & gdLayer & string(nxt)).image.copyPixels(gtImg, rct, gtRect, {#ink:39})
      nxt = lr - 1
      if (nxt < 0) then nxt = 0
      member("gradient" & gdLayer & string(nxt)).image.copyPixels(gtImg, rct, gtRect, {#ink:39})
    end if
  end if
end

on copyPixelsToRootEffectColor(gdLayer, lr, rct, getMember, gtRect, zbleed, blnd)
  global DRPxl
  --use: copyPixelsToRootEffectColor(effect color letter from "Color" option (A, B, C=none), depth layer (from 0 to 29), final rectangle, gradient image name, source rectangle, blend modifier(from 0 to 1))
  if (blnd = VOID) then
    blnd = 1.0
  end if
  if (gdLayer <> "C") and (blnd > 0) then
    lr = lr.integer
    if (lr < 0) then lr = 0
    else if (lr > 29) then lr = 29
    gtImg = member(getMember).image
    if (blnd <> 0) and (blnd <> VOID) then
      dmpImg = gtImg.duplicate()
      dmpImg.copyPixels(DRPxl, dmpImg.rect, rect(0, 0, 1, 1), {#blend:100.0 * (1.0 - blnd), #color:color(255, 255, 255)})
      gtImg = dmpImg
    end if
    member("gradient" & gdLayer & string(lr)).image.copyPixels(gtImg, rct, gtRect, {#ink:39})
    if (zbleed > 0) then
      if (zbleed < 1) then
        dmpImg = gtImg.duplicate()
        dmpImg.copyPixels(DRPxl, dmpImg.rect, rect(0, 0, 1, 1), {#blend:100.0 * (1.0 - zbleed), #color:color(255, 255, 255)})
        gtImg = dmpImg
      end if
      repeat with nxtAdd = 1 to 3
        nxt = lr + nxtAdd
        if (nxt > 29) then nxt = 29
        member("gradient" & gdLayer & string(nxt)).image.copyPixels(gtImg, rct, gtRect, {#ink:39})
        nxt = lr - nxtAdd
        if (nxt < 0) then nxt = 0
        member("gradient" & gdLayer & string(nxt)).image.copyPixels(gtImg, rct, gtRect, {#ink:39})
      end repeat
    end if
  end if
end

on makeSilhoutteFromImg(img, inverted)
  global DRPxl
  inv = image(img.width, img.height, 1)
  inv.copyPixels(DRPxl, img.rect, rect(0, 0, 1, 1), {#color:255})
  inv.copyPixels(img, img.rect, img.rect, {#ink:36, #color:color(255, 255, 255)})
  if (inverted = 0) then
    inv = makeSilhoutteFromImg(inv, 1)
  end if
  return inv
end

on rotateToQuad(rct, deg)
  dir = degToVec(deg.float)
  midPnt = point((rct.left + rct.right) * 0.5, (rct.top + rct.bottom) * 0.5)
  tlr = dir * rct.height * 0.5
  topPnt = midPnt + tlr
  bottomPnt = midPnt - tlr
  tlr = giveDirFor90degrToLine(-dir, dir) * rct.width * 0.5
  return [topPnt + tlr, topPnt - tlr, bottomPnt - tlr, bottomPnt + tlr]
end

on giveDirFor90degrToLineLB(pnt1, pnt2)
  X1 = pnt1.locH
  Y1 = pnt1.locV
  X2 = pnt2.locH
  Y2 = pnt2.locV
  Ydiff = Y1 - Y2
  Xdiff = X1 - X2
  if (Ydiff = 0) then
    return point(0, 1)
  else if (Xdiff = 0) then
    return point(1, 0)
  else
    newPnt = point(1, -1.0 / (Ydiff / Xdiff))
    return newPnt / sqrt(newPnt.locH * newPnt.locH + newPnt.locV * newPnt.locV)
  end if
end 

on dirVecLB(pointA, pointB)
  pointB = pointB - pointA
  if (pointB = point(0, 0)) then
    return point(0, 1)
  else
    return pointB / sqrt(pointB.locH * pointB.locH + pointB.locV * pointB.locV)
  end if
end

on rotateToQuadLB(rct, dir)
  midPnt = point((rct.left + rct.right) * 0.5, (rct.top + rct.bottom) * 0.5)
  tlr = dir * rct.height * 0.5
  topPnt = midPnt + tlr
  bottomPnt = midPnt - tlr
  tlr = giveDirFor90degrToLineLB(-dir, dir) * rct.width * 0.5
  return [topPnt + tlr, topPnt - tlr, bottomPnt - tlr, bottomPnt + tlr]
end

on rotateToQuadFix(rct, deg)
  mdpt = point((rct.left + rct.right) * 0.5, (rct.top + rct.bottom) * 0.5)
  halfWidth = (rct.right - rct.left) / 2.0
  halfHeight = (rct.bottom - rct.top) / 2.0
  if deg.float mod 360.0 = 0.0 then return [point(rct.left, rct.top), point(rct.right, rct.top), point(rct.right, rct.bottom), point(rct.left, rct.bottom)]
  else if deg.float mod 360.0 = 180.0 then return [point(rct.right, rct.bottom), point(rct.left, rct.bottom), point(rct.left, rct.top), point(rct.right, rct.top)]
  else if deg.float mod 360.0 = 90.0  then return [mdpt + point(-halfHeight, halfWidth), mdpt + point(-halfHeight, -halfWidth), mdpt + point(halfHeight, -halfWidth), mdpt + point(halfHeight, halfWidth)]
  else if deg.float mod 360.0 = 270.0 then return [mdpt + point(halfHeight, -halfWidth), mdpt + point(halfHeight, halfWidth),   mdpt + point(-halfHeight, halfWidth), mdpt + point(-halfHeight, -halfWidth)]
  else return rotateToQuad(rct, deg)
end

on rotatePnt(pnt, ang)
  ang = (ang + lookAtpoint(point(0,0), pnt) - 90) * PI / 180.0
  dist = sqrt(pnt.locH*pnt.locH + pnt.locV*pnt.locV)
  return point(cos(ang) * dist, sin(ang) * dist)
end

on rotateRectAroundPoint(rct, pt, ang)
  tl = rotatePnt(point(rct.left, rct.top), ang)
  tr = rotatePnt(point(rct.right, rct.top), ang)
  br = rotatePnt(point(rct.right, rct.bottom), ang)
  bl = rotatePnt(point(rct.left, rct.bottom), ang)
  return [pt + tl, pt + tr, pt + br, pt + bl]
end

on flipQuadH(qd)
  return [qd[2], qd[1], qd[4], qd[3]]
end

on flipQuadV(qd)
  return [qd[3], qd[4], qd[1], qd[2]]
end

on pasteShortCutHole(mem, pnt, dp, cl)
  global gLEProps, gLOprops, gCameraProps, gCurrentRenderCamera, gRenderCameraTilePos, gRenderCameraPixelPos
  rct = giveMiddleOfTile(pnt) - (gRenderCameraTilePos * 20) - gRenderCameraPixelPos
  rct = depthPnt(rct, dp)
  rct = rect(rct, rct) + rect(-10, -10, 10, 10)
  idString = ""
  repeat with dr in [point(-1, 0), point(0, -1), point(1, 0), point(0, 1)]
    if (pnt + dr).inside(rect(1, 1, gLOprops.size.loch, gLOprops.size.locv)) then
      matProp = gLEProps.matrix[pnt.locH + dr.locH][pnt.locV + dr.locV][1][2]
      if (matProp.getPos(5) > 0) or (matProp.getPos(4) > 0) then
        idString = idString & "1"
      else
        idString = idString & "0"
      end if
    else
      idString = idString & "0"
    end if
  end repeat
  ps = ["0101", "1010", "1111", "1100", "0110", "0011", "1001", "1110", "0111", "1011", "1101", "0000"].getPos(idString)
  if (cl = "BORDER") then
    clL = [[color(255, 0, 0), point(-1, 0)], [color(255, 0, 0), point(0, -1)], [color(255, 0, 0), point(-1, -1)], [color(255, 0, 0), point(-2, 0)], [color(255, 0, 0), point(0, -2)], [color(255, 0, 0), point(-2, -2)], [color(0, 0, 255), point(1, 0)], [color(0, 0, 255), point(0, 1)], [color(0, 0, 255), point(1, 1)], [color(0, 0, 255), point(2, 0)], [color(0, 0, 255), point(0, 2)], [color(0, 0, 255), point(2, 2)]]
  else
    clL = [[cl, point(0, 0)]]
  end if
  shortCutsGraf = member("shortCutsGraf").image
  memImage = member(mem).image
  getShCtRect = rect(20 * (ps - 1), 1, 20 * ps, 21)
  repeat with c in clL
    memImage.copyPixels(shortCutsGraf, rct + rect(c[2], c[2]), getShCtRect, {#ink:36, #color:c[1]})
  end repeat
end

on resizeLevel(sze, addTilesLeft, addTilesTop)--nt
  global gLEprops, gLOProps, gTEprops, gEEprops, gPEprops, gCameraProps
  newMatrix = []
  newTEmatrix = []
  
  repeat with q = 1 to sze.locH + addTilesLeft then
    ql = []
    repeat with c = 1 to sze.locV + addTilesTop then
      if (q-addTilesLeft<=gLEprops.matrix.count)and(c-addTilesTop<=gLEprops.matrix[1].count)and(q-addTilesLeft>0)and(c-addTilesTop>0)then
        adder = gLEprops.matrix[q-addTilesLeft][c-addTilesTop]
      else
        adder = [[1, []], [1, []], [1, []]]
      end if
      ql.add(adder)
    end repeat
    newMatrix.add(ql)
  end repeat
  
  repeat with q = 1 to sze.locH + addTilesLeft then
    ql = []
    repeat with c = 1 to sze.locV + addTilesTop then
      if (q+addTilesLeft<=gTEprops.tlMatrix.count)and(c+addTilesTop<=gTEprops.tlMatrix[1].count)and(q-addTilesLeft>0)and(c-addTilesTop>0)then
        adder = gTEprops.tlMatrix[q-addTilesLeft][c-addTilesTop]
        
        -- fix for tiles during resize
        repeat with l = 1 to 3 then
          if adder[l].tp = "tileBody" then
            newPt = adder[l].data[1] + point(addTilesLeft, addTilesTop)
            adder[l].data[1] = newPt
            if newPt.locH < 1 or newPt.locH > sze.locH + addTilesLeft or newPt.locV < 1 or newPt.locV > sze.locV + addTilesTop then
              adder[l] = [#tp:"default", #data:0]
            end if
          end if
        end repeat
      else
        adder = [[#tp:"default", #data:0], [#tp:"default", #data:0], [#tp:"default", #data:0]]
      end if
      ql.add(adder)
    end repeat
    newTEmatrix.add(ql)
  end repeat
  
  
  repeat with effect in gEEprops.effects then
    newEffMtrx = []
    
    repeat with q = 1 to sze.locH + addTilesLeft then
      ql = []
      repeat with c = 1 to sze.locV + addTilesTop then
        if (q+addTilesLeft<=effect.mtrx.count)and(c+addTilesTop<=effect.mtrx[1].count)and(q-addTilesLeft>0)and(c-addTilesTop>0)then
          adder = effect.mtrx[q-addTilesLeft][c-addTilesTop]
        else
          adder = 0
        end if
        ql.add(adder)
      end repeat
      newEffMtrx.add(ql)
    end repeat
    
    effect.mtrx = newEffMtrx
  end repeat
  
  repeat with prop in gPEprops.props then
    repeat with q = 1 to 4 then
      prop[4][q] = prop[4][q] + 16*point(addTilesLeft, addTilesTop)
    end repeat
    if prop[5][#points] <> void then
      repeat with q = 1 to prop[5].points.count then
        prop[5].points[q] = prop[5].points[q] + 20*point(addTilesLeft, addTilesTop)
      end repeat
    end if
  end repeat
  
  repeat with q = 1 to gCameraProps.cameras.count then
    gCameraProps.cameras[q] = gCameraProps.cameras[q] + point(20*addTilesLeft, 20*addTilesTop)
  end repeat
  
  
  gLEprops.matrix = newMatrix
  gTEprops.tlMatrix = newTEmatrix
  gLOprops.size = sze + point(addTilesLeft, addTilesTop) --- (rmvTilesLeft, rmvTilesTop)
  
  global gLASTDRAWWASFULLANDMINI
  gLASTDRAWWASFULLANDMINI = 0
  
  oldimg = member("lightImage").image.duplicate()
  member("lightImage").image = image((gLOprops.size.locH*20)+300,(gLOprops.size.locV*20)+300, 1)
  member("lightImage").image.copypixels(oldimg, oldimg.rect, oldimg.rect)
end

on ResetgEnvEditorProps()
  global gEnvEditorProps
  gEnvEditorProps = [#waterLevel:-1, #waterInFront:1, #waveLength:60, #waveAmplitude:5, #waveSpeed:10]
end

on resetPropEditorProps()
  global gPEprops
  gPEprops = [#props:[], #lastKeys:[], #keys:[], #workLayer:1, #lstMsPs:point(0, 0), pmPos:point(1, 1), #pmSavPosL:[], #propRotation:0, #propStretchX:1, #propStretchY:1, #propFlipX:1, #propFlipY:1, #depth:0, #color:0]
end

on vecToRadLB(vec)
  if (vec.locH = 0) then
    if (vec.locV < 0) then
      return -PI / 2.0
    else
      return PI / 2.0
    end if
  else if (vec.locH < 0) then
    return atan(vec.locV / vec.locH) - PI
  else
    return atan(vec.locV / vec.locH)
  end if
end






