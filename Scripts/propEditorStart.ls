global gLOprops, gPEprops, gDirectionKeys, gProps, gPEblink, peScrollPos, editSettingsProp
global peFreeQuad, settingCursor, settingsPropType, peSavedFlip, gPEcolors, peSavedStretch, preciseSnap, snapToGrid, stg, ps

on exitFrame me
  preciseSnap = 0
  snapToGrid = 0
  stg = 0
  ps = 0
  member("propMenu").alignment = #left
  
  member("TEimg1").image = image(52*16, 40*16, 16)
  member("TEimg2").image = image(52*16, 40*16, 16)
  member("TEimg3").image = image(52*16, 40*16, 16)
  member("levelEditImage1").image = image(52*16, 40*16, 16)
  member("levelEditImage2").image = image(52*16, 40*16, 16)
  member("levelEditImage3").image = image(52*16, 40*16, 16)
  member("levelEditImageShortCuts").image = image(52*16, 40*16, 16)
  
  member("ropePreview").image = image(52*16, 40*16, 1)
  sprite(269).loc = point(432, 336)
  
  TEdraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 1)
  TEdraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 2)
  TEdraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 3)
  lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 1)
  lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 2)
  lvlEditDraw(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 3)
  drawShortCutsImg(rect(1,1,gLOprops.size.loch,gLOprops.size.locv), 16)
  
  gDirectionKeys = [0,0,0,0]
  
  sprite(250).blend = 20
  sprite(251).blend = 20
  sprite(252).blend = 40
  sprite(253).blend = 40
  sprite(254).blend = 90
  sprite(255).blend = 90
  
  sprite(257).blend = 80
  sprite(257).visibility = 0
  -- sprite(8).loc = point(432, 336) --+ point(-5, 5)--*(gTEprops.workLayer = 2)
  
  gPEprops.workLayer = 1
  
  
  sprite(250).loc = point(432, 336)
  sprite(251).loc = point(432, 336)
  sprite(252).loc = point(432, 336)
  sprite(253).loc = point(432, 336)
  sprite(254).loc = point(432, 336)
  sprite(255).loc = point(432, 336)
  
  sprite(260).loc = point(432, 336)
  sprite(262).loc = point(432, 336)
  
  l = [#w:0, #a:0, #s:0, #d:0, #L:0, #n:0, #m1:0, m2:0, #c:0, #z:0]
  gPEprops.lastKeys = l.duplicate()
  gPEprops.keys = l.duplicate()
  
  --script("propEditor").updatePropMenu(point(0,0))
  
  peSavedFlip = point(0,0)
  peSavedStretch = point(0,0)
  
  peScrollPos  = 0
  
  gPEblink = 0
  
  peFreeQuad = [point(0,0), point(0,0), point(0,0), point(0,0)]
  settingCursor = 1
  
  gPEprops.pmSavPosL = []
  repeat with q = 1 to gProps.count then
    gPEprops.pmSavPosL.add(1)
  end repeat
  
  gPEprops.pmPos = point(1,1)
  
  --version fix
  repeat with q in gPeProps.props then
    actualSettings = q[5].settings
    idealSettings = gProps[q[3].loch].prps[q[3].locV].settings
    repeat with i = 1 to idealSettings.count then
      smbl = idealSettings.getPropAt(i)
      if(actualSettings.findpos(smbl) = void)then
        actualSettings.addProp(smbl, idealSettings[i])
      end if
    end repeat
  end repeat
  
  editSettingsProp = -1
  settingsPropType = void
  propSettings = void
  
  if gPEprops.color = 0 then
    member("Prop Color Text").text = "PROP COLOR: " &"NONE"
    sprite(270).color = color(150, 150, 150)
  else
    member("Prop Color Text").text = "PROP COLOR: " & gPEcolors[gPEprops.color][1]
    sprite(270).color = gPEcolors[gPEprops.color][2]
  end if
  
  script("propEditor").updateWorkLayerText()
  script("propEditor").renderPropsImage()
  call(#updatePropMenu, point(0,0))
  member("propMenu").text = member("propBaseMenu").text
end