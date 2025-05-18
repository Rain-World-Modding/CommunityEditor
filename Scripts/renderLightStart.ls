global c, tm, dptsL, gLightEProps, pos, gLevel, gLEProps, gLOprops, keepLooping, fogDptsL,gRenderCameraTilePos, gAnyDecals, solidMtrx, DRActiveLight, DRWhite

on exitFrame(me)
  if (checkMinimize()) then
    _player.appMinimize()
  end if
  if (checkExit()) then
    _player.quit()
  end if
  if (checkExitRender()) then
    _movie.go(9)
  end if
  the randomSeed = gLOprops.tileSeed
  blkI2 = member("blackOutImg2").image
  member("layer0dc").image.copyPixels(blkI2, rect(0, 0, 2000, 1200), rect(0, 0, 2000, 1200), {#ink:36, #color:DRWhite})
  repeat with layer = 1 to 3
    repeat with q = 1 to gLOprops.size.loch
      repeat with c = 1 to gLOprops.size.locv
        if (gLEProps.matrix[q][c][layer][2].getPos(3) > 0) and (afaMvLvlEdit(point(q, c), layer) = 0) and (afaMvLvlEdit(point(q, c + 1), layer) = 1) then
          repeat with tp = 1 to 2
            repeat with grss = 1 to 6
              lr = ((layer - 1) * 10) + random(9)
              pos = giveMiddleOfTile(point(q, c) - gRenderCameraTilePos) + point(-10 + random(20), 0)
              if (tp = 2) and (layer = 1) then
                member("layer" & string(lr)).image.copyPixels(member("hiveGrassGraf").image, rect(pos, pos) + rect(-2, random(5) - random(10) - random(random(14)), 3, 10), rect(0, 0, 5, 29), {#ink:36})
              else
                member("layer" & string(lr)).image.copyPixels(member("hiveGrassGraf2").image, rect(pos, pos) + rect(-2, random(5) - random(10) - random(random(14)), 3, 10), rect(0, 0, 5, 29), {#ink:36, #color:color(255, 0, 0)})
              end if  
            end repeat
          end repeat
        end if
      end repeat
    end repeat
  end repeat
  cols = 2000
  rows = 1200
  marginPixels = 150
  marginPixels2 = marginPixels * 2
  if (gAnyDecals) then
    repeat with l = 0 to 29
      me.quadifyMember("layer" & string(l) & "dc", (l - 5) * 1.5)
    end repeat
  end if
  repeat with l = 0 to 29
    lm5 = (l - 5) * 1.5
    strl = string(l)
    me.quadifyMember("layer" & strl, lm5)
    member("layer" & l & "sh").image = image(cols + marginPixels2, rows + marginPixels2, 8)
    me.quadifyMember("gradientA" & strl, lm5)
    me.quadifyMember("gradientB" & strl, lm5)
  end repeat
  member("activeLightImage").image = image(cols + marginPixels2, rows + marginPixels2, 1)
  DRActiveLight = member("activeLightImage").image
  DRActiveLight.setPixel(0, 0, color(0, 0, 0))
  DRActiveLight.setPixel(DRActiveLight.rect.right - 1, DRActiveLight.rect.bottom - 1, color(0, 0, 0))
  inversedLightImage = makeSilhoutteFromImg(member("lightImage").image, 1)
  global gRenderCameraPixelPos
  DRActiveLight.copyPixels(inversedLightImage, rect(0, 0, cols + marginPixels2, rows + marginPixels2), rect(point(0, 0), point(cols + marginPixels2, rows + marginPixels2)) + rect(gRenderCameraTilePos * 20, gRenderCameraTilePos * 20) + rect(150, 150, 150, 150))
  DRActiveLight.copyPixels(blkI2, rect(0, 0, cols, rows) + rect(marginPixels, marginPixels, marginPixels, marginPixels), rect(0, 0, cols, rows), {#ink:36, #color:DRWhite})
  c = 0
  keepLooping = 1
  pos = point(0, 0)
  tm = _system.milliseconds
  repeat with q2 = 0 to 29
    sprite(50 - q2).loc =  point(683 - q2, 384 - q2)
  end repeat
  if (gLOprops.light = 0) then
    _movie.go(66)
  end if
end

on quadifyMember(me, mem, fac)
  global gCameraProps, gCurrentRenderCamera
  newImg = member(mem).image.duplicate()
  qd = [point(0, 0), point(newImg.width, 0), point(newImg.width, newImg.height), point(0, newImg.height)]
  curcam = gCameraProps.quads[gCurrentRenderCamera]
  repeat with q = 1 to 4
    curcamq = curcam[q]
    qd[q] = qd[q] + degToVec(curcamq[1]) * curcamq[2] * fac * 2.5--arbitrary number, seemed to give the right amount
  end repeat
  member(mem).image.copypixels(newImg, qd, newImg.rect)
end