global gLOprops, gLevel, gLeProps, gPrioCam, newSize, showControls

on exitFrame me
  newSize[1] = gLOprops.size.loch
  newSize[2] = gLOprops.size.locv
  cols = gLOprops.size.loch
  rows = gLOprops.size.locv
  sprite(43).color = color(255, 0, 255)
  
  sprite(2).loc = point(312,312)+point(-1000+1000*gLevel.defaultTerrain, 0)
  
  sprite(56).visibility = 1
  sprite(57).visibility = 1
  sprite(58).visibility = 1
  sprite(59).visibility = 1
  
  sprite(67).loch = (gLEVEL.waterDrips*8)+50
  sprite(68).loch = (gLEVEL.maxFlies*10)+50
  sprite(69).loch = (gLEVEL.flySpawnRate*4)+50
  member("lightTypeText").text = gLevel.lightType
  sprite(70).loch = (gLOProps.tileseed)+50
  
  repeat with q = 0 to 29 then
    member("layer"&q).image = image(1,1,1)
    member("layer"&q&"sh").image = image(1,1,1)
  end repeat
  
  sprite(22).rect = rect(-100, -100, -100, -100)
  if(gPrioCam = 0) then
    member("PrioCamText").text = ""
  else 
    member("PrioCamText").text = "Will render camera " & gPrioCam & " first"
  end if
  
  the randomSeed = _system.milliseconds
end