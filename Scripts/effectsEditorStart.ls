global gTEprops, gTiles, gEEprops, gLEprops, gLOprops, gEnvEditorProps

on exitFrame me
  sprite(243).color = color(255, 255, 255)
  
  member("effectsMatrix").image = image(52,40,32)
  
  sprite(212).blend = 30
  sprite(213).blend = 50
  sprite(214).blend = 70
  sprite(215).blend = 100
  
  repeat with spr = 3 to 9 then
    sprite(spr).rect = rect(16,16,53*16, 41*16)
  end repeat
  
  
  
  repeat with q = 1 to gLOprops.size.loch then
    repeat with c = 1 to gLOprops.size.locv then
      member("effectsMatrix").image.setPixel(q-1, c-1, color(0, 0, 0))
    end repeat
  end repeat
  
  member("TEimg1").image = image(52*16, 40*16, 16)
  member("TEimg2").image = image(52*16, 40*16, 16)
  member("TEimg3").image = image(52*16, 40*16, 16)
  member("levelEditImage1").image = image(52*16, 40*16, 16)
  member("levelEditImage2").image = image(52*16, 40*16, 16)
  member("levelEditImage3").image = image(52*16, 40*16, 16)
  member("levelEditImageShortCuts").image = image(52*16, 40*16, 16)
  
  TEdraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 1)
  TEdraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 2)
  TEdraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 3)
  lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 1)
  lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 2)
  lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 3)
  drawShortCutsImg(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 16)
  
  gDirectionKeys = [0,0,0,0]
  
  sprite(212).blend = 30
  sprite(213).blend = 40
  sprite(214).blend = 50
  sprite(215).blend = 60
  sprite(216).blend = 80
  sprite(217).blend = 100
  
  l = [#n:0, #m1:0, m2:0, #w:0, #a:0, #s:0, #d:0, #e:0, #r:0, #f:0]
  gEEprops.lastKeys = l.duplicate()
  gEEprops.keys = l.duplicate()
  
  script("effectsEditor").updateEffectsMenu(point(0,0))
  script("effectsEditor").updateEffectsL(0)
  script("effectsEditor").initMode("createNew")
  if not getBoolConfig("Hide props in effects") then
    sprite(238).visibility = true
    sprite(239).visibility = true
    script("propEditor").renderPropsImage()
  else
    sprite(238).visibility = false
    sprite(239).visibility = false
  end if
  script("effectsEditor").drawEfMtrx(gEEprops.editEffect)
end


