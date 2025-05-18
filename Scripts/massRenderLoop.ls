global gMassRenderL, gViewRender

on exitFrame me
  if checkMinimize() then
    _player.appMinimize()
    
  end if
  if checkExit() then
    _player.quit()
  end if
  
  gMassRenderL.deleteAt(1)
  
  if gMassRenderL.count = 0 then
    alert("Mass Render Finished")
    _movie.go(1)
  else
    put "started rendering:" && gMassRenderL[1]
    script("loadLevel").loadLevel(gMassRenderL[1], 1)
    gViewRender = 0
    _movie.go(42)
  end if
end