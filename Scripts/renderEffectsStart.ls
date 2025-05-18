global vertRepeater, r, gEEprops, keepLooping

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
  repeat with q = 0 to 29
    sprq = sprite(50 - q)
    sprq.loc = point(683 - q, 384 - q)
    val = (q.float + 1.0) / 30.0 * 255
    sprq.color = color(val, val, val)
  end repeat
  sprite(57).visibility = 0
  sprite(58).visibility = 0
  vertRepeater = 100000
  if (gEEprops.effects.count > 0) then
    r = 0
    keepLooping = 1
  else 
    go(56)
  end if
end