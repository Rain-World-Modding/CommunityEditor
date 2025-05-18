global gSaveProps

on stopMovie me
   changeBack = baSetDisplay(gSaveProps[1], gSaveProps[2], gSaveProps[3], "perm", FALSE)
end