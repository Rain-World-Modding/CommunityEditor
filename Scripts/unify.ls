global c, lightRects, gLevel, gImgXtra, gLoadedName, gCurrentRenderCamera, lvlPropOutput, DRFinalImage, DRFogImage, DRDpImage, DRShadowImage, DRRainbowMask, DRFlattenedGradientA, DRFlattenedGradientB, DRFinalDecalImage, DRPxl

on exitFrame me
  --  if checkMinimize() then
  --    _player.appMinimize()
  --  end if
  --  if checkExit() then
  --    _player.quit()
  --  end if
  --  if checkExitRender() then
  --    _movie.go(9)
  --  end if
  DRFinalImage = member("finalImage").image
  DRFogImage = member("fogImage").image
  DRDpImage = member("dpImage").image
  DRShadowImage = member("shadowImage").image
  DRRainbowMask = member("rainBowMask").image
  DRFlattenedGradientA = member("flattenedGradientA").image
  DRFlattenedGradientB = member("flattenedGradientB").image
  DRFinalDecalImage = member("finalDecalImage").image
  if (lvlPropOutput = TRUE) then
    member("GradientOutput").image = image(2800, 801, 32)
    gradOut = member("GradientOutput").image
    gradOut.copyPixels(DRFinalImage, rect(0, 1, 1400, 801), DRFinalImage.rect, {#ink:36})
    gradOut.copyPixels(DRFlattenedGradientA, rect(1400, 1, 2800, 801), DRFlattenedGradientA.rect, {#ink:36})
    gradOut.copyPixels(DRFlattenedGradientB, rect(1400, 1, 2800, 801), DRFlattenedGradientB.rect, {#ink:36}) 
    gradOut.copyPixels(DRPxl, rect(0, 0, 1, 1), DRPxl.rect, {#color:color(0, 0, 0), #ink:36}) 
    props = ["image": gradOut, "filename":_movie.path&"Props/"&gLoadedName & "_" & gCurrentRenderCamera & "_Prop.png"]
    ok = gImgXtra.ix_saveImage(props)
    
    objFileio = new xtra("fileio")
    objFileio.createFile(the moviePath & "Props/"&gLoadedName & "_" & gCurrentRenderCamera & "_Prop_Init.txt")
    objFileio.closeFile()
    
    fileOpener = new xtra("fileio")
    fileOpener.openFile(the moviePath & "Props/"&gLoadedName & "_" & gCurrentRenderCamera & "_Prop_Init.txt", 0)
    fileOpener.writeString("[#nm:" &QUOTE& gLoadedName & "_" & gCurrentRenderCamera & "_Prop"&QUOTE&", #tp:"&QUOTE& "standard" &QUOTE&", #colorTreatment:"&QUOTE&"standard"&QUOTE&", #bevel:1, #sz:point(70, 40), #repeatL:[1], #tags:["&QUOTE&"effectColorA"&QUOTE&", "&QUOTE&"effectColorB"&QUOTE&", "&QUOTE&"notTrashProp"&QUOTE&"], #layerExceptions:[], #notes:[]]")
    fileOpener.writeReturn(#windows)
  end if
end





