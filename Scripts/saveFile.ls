global gLEProps, levelName, gLEVEL, lightRects, gLOprops, gCurrentRenderCamera, gImgXtra, gLoadedName, gCameraProps, gPrioCam

on exitFrame me
  if checkMinimize() then
    _player.appMinimize()
    
  end if
  if checkExit() then
    _player.quit()
  end if

  props = ["image": member("finalImage").image, "filename":_movie.path&"Levels/"&gLoadedName & "_" & gCurrentRenderCamera & ".png"]
  ok = gImgXtra.ix_saveImage(props)
  if (gCurrentRenderCamera < gCameraProps.cameras.count) then
    put "sendback" && gCurrentRenderCamera
    _movie.go(44)
  else
    newMakeLevel(gLoadedName)
  end if

end



on changeToPlayMatrix()
  nMtrx = []
  repeat with q = 1 to gLOprops.size.loch then
    l = []
    repeat with c = 1 to gLOprops.size.locv then
      cell = [gLEProps.matrix[q][c][1].duplicate(), [([1,9].getPos(gLEProps.matrix[q][c][2][1])>0)*(gLEProps.matrix[q][c][2][2].getPos(11)=0), []] ]
      if cell[1][1]=9 then
        cell[1][1] = 1
        cell[1][2].add(8)
      end if
      if (cell[1][2].getPos(6)>0)or(cell[1][2].getPos(7)>0)or(cell[1][2].getPos(19)>0)or(cell[1][2].getPos(21)>0) then
        if cell[1][2].getPos(5)=0 then
          cell[1][2].add(5)
        end if
      end if

      if (cell[1][2].getPos(11)>0)then
        cell[1][1] = 0
        if (c>1)then
          if (gLEProps.matrix[q][c-1][1][1] = 0)and(gLEProps.matrix[q-1][c][1][1] = 1)and(gLEProps.matrix[q-1][c][1][2].getPos(11) = 0)and(gLEProps.matrix[q+1][c][1][1] = 1)and(gLEProps.matrix[q+1][c][1][2].getPos(11) = 0) then
            cell[1][1] = 6
          end if
        end if
      end if
      l.add(cell)
    end repeat
    nMtrx.add(l)
  end repeat
  return nMtrx
end















