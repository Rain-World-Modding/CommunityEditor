
global gLOprops, gLevel

on mouseWithin(me)
  sprite(me.spriteNum).color = color(255,0,0)
  if _mouse.mouseDown then
    val = restrict(_mouse.mouseLoc.locH, 50, 450)-50
    sprite(me.spriteNum).loch = val+50
    case sprite(me.spriteNum).member.name of
      "tileSeedSlider":
        gLOprops.tileSeed = val
        the randomSeed = gLOprops.tileSeed
    end case
  end if
  
  case sprite(me.spriteNum).member.name of
   
    "tileSeedSlider":
      member("buttonText").text = "Tile random seed:" && string(gLOprops.tileSeed)
  end case
end mouseWithin

on mouseLeave(me)
  sprite(me.spriteNum).color = color(0,0,0)
  member("buttonText").text = ""
  sprite(20).quad = [point(-100,-100), point(-100,-100), point(-100,-100), point(-100,-100)]
end mouseLeave