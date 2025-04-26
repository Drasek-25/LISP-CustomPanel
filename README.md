# LISP-CustomPanel

### Why?
I always wanted a tool that allowed on the fly creation of custom wall panels but with custom lengths, or specific settings like single or double sided, camlock locations, etc... However I have no faith AutoCAD will implement that feature, and I have been unable to find a third party tool that enables this either.

### How it works
This tool takes a custom black panel, except the block panel is built as 2D objects aligned to the X Y Z cordinations that those items typically exist on. Because the items are built in 2D they can be modified with paramtric settings allowing me to change its width, wether its single sided or double sided, and weather it features post holes and electrical holes.

Once the block is set to as desired specifications, this script can run and target the block. The script takes that block, explodes it, grabs all the contents inside, and then performs a long series of operations on them effectivly building the 3D wall panel from scratch, and then inserting it into the drawing with a custom set name, to make sure that the items can be grabbed from drawing data for proper quantity lists.