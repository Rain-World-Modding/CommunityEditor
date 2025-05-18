global gTEprops, gTiles, gLEprops, gLOprops, gDirectionKeys

on exitFrame me
  member("tileMenu").alignment = #left
  
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
  
  sprite(1).blend = 10
  sprite(2).blend = 10
  sprite(3).blend = 60
  sprite(4).blend = 10
  sprite(5).blend = 70
  sprite(6).blend = 100
  
  sprite(8).blend = 80
  sprite(8).visibility = 0
  script("tileEditor").changeLayer()
  
  member("default material").text = "Default material:" && gTEprops.defaultMaterial && "(Press 'E' to change)"
  sprite(19).visibility = 1
  l = [#L:0, #m1:0, m2:0, #w:0, #a:0, #s:0, #d:0, #c:0, #q:0]
  gTEprops.lastKeys = l.duplicate()
  gTEprops.keys = l.duplicate()
  
  script("tileEditor").updateTileMenu(point(0,0))

  gTEprops.tmSavPosL = []
  repeat with q = 1 to gTiles.count then
    gTEprops.tmSavPosL.add(1)
  end repeat
end


