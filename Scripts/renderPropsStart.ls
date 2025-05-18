global c, keepLooping, afterEffects, gLastImported, gRenderTrashProps, gCurrentlyRenderingTrash, softProp, propsToRender, gPEprops

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
  c = 1
  keepLooping = 1
  afterEffects = (_movie.frame > 51)
  gLastImported = ""
  gCurrentlyRenderingTrash = FALSE
  if (gRenderTrashProps.count > 0) then
    if (afterEffects = 0) then
      gCurrentlyRenderingTrash = TRUE
    end if
  end if
  repeat with q = 0 to 29
    sprq = sprite(50 - q)
    sprq.loc = point(683 - q, 384 - q)
    val = (q.float + 1.0) / 30.0 * 255
    sprq.color = color(val, val, val)
  end repeat
  propsToRender = []
  peprps = gPEprops.props
  repeat with a = 1 to peprps.count
    propsToRender.add(peprps[a])
    plst = propsToRender[propsToRender.count]
    plst.addAt(1, plst[5].settings.renderOrder)
  end repeat
  propsToRender.sort()
  repeat with a = 1 to propsToRender.count
    propsToRender[a].deleteAt(1)
  end repeat
  softProp = VOID
end