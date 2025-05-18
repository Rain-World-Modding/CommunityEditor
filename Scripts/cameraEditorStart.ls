
global gCameraProps, gLOprops, gLeProps

on exitFrame me
  cols = gLOprops.size.loch
  rows = gLOprops.size.locv
  
  --  member("TEimg1").image = image(cols*16, rows*16, 16)
  --  member("TEimg2").image = image(cols*16, rows*16, 16)
  --  member("TEimg3").image = image(cols*16, rows*16, 16)
  
  member("levelEditImageShortCuts").image = image(cols*5, rows*5, 1)
  drawShortCutsImg(rect(1,1,cols,rows), 5, 1)
  
  repeat with q = 1 to 3 then
    miniLvlEditDraw(q)
  end repeat
  
  script("cameraEditor").drawAll()
end