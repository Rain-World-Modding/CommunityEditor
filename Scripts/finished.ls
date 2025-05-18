global levelName, gLoadedName, gFullRender, gViewRender, gDecalColors, DRFinalImage

on exitFrame me
  if (checkMinimize()) then
    _player.appMinimize()
  end if
  if (checkExit()) then
    _player.quit()
  end if
  if (checkExitRender()) then
    if (gViewRender) then
      _movie.go(9)
    end if
  end if
  if (gViewRender = 0) then
    levelName = gLoadedName
  end if
  repeat with q = 0 to gDecalColors.count - 1
    DRFinalImage.setPixel(q, 0, gDecalColors[q + 1])
  end repeat
end