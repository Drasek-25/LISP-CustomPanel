; Written by Patrick Marschlowitz 2/19/2025
; Lisp takes a very specific paremtric block and converts the geometry inside of
; the block into a new panel with a custom width
; All of this as a workaround to 3D solids not being able to be adjusted with parameters
; 8 years of parametric blocks, still no way to resize solids, Autocad blows.

;its worth noting that the lisp needed special methods to get at the block title
;i believe its because the block is dynamic, but if the method of creating the block
;changes in the future it might cause problems.
(defun c:Cus () 
  (vl-load-com)
  (setq selEnt (ssget))
  (if selEnt 
    (progn 
      ; begin looping through selected items and exploding each one at a time
      (setq i 0)
      (while (< i (sslength selEnt)) 
        (progn 
          (setq ent (ssname selEnt i))

          (setq correctBlock T)
          (setq height nil)

          ; get information from block before explosion
          (if (and ent (eq (cdr (assoc 0 (entget ent))) "INSERT")) 
            (progn 
              (setq blkObj (vlax-ename->vla-object ent))
              (setq blkName (vlax-get blkObj 'EffectiveName))
              (if (> (strlen blkName) 15)  ; make sure string checks wont crash lisp
                (progn 
                  ; grab first 2 characters to verify block height
                  ; grab last 13 character to verify "PanelTemplate"
                  (setq blkNameBegin (substr blkName 1 2))
                  (setq blkNameEnd (substr blkName (- (strlen blkName) 12) 13))
                  (princ (strcat "\n" blkNameEnd))
                  (if (= blkNameEnd "PanelTemplate") 
                    (progn 
                      (if (= blkNameBegin "95") 
                        (setq height "95")
                        (setq height "119")
                      )
                    )
                    (progn 
                      (princ "\nNot Panel Template Block")
                      (setq correctBlock nil)
                    )
                  )
                )
                (progn 
                  (princ "\nIncorrect Block Name Length")
                  (setq correctBlock nil)
                )
              )
            )
            (progn 
              (princ "\nNot a Block")
              (setq correctBlock nil)
            )
          )

          ; break loop if object is not template block
          (if correctBlock 
            (progn 
              ; explode the block and save all items inside of block
              (command "_explode" ent)
              (setq explodedEnts (ssget "_P"))
              (princ 
                (strcat "\nEXPLODED ENTS:   " (itoa (sslength explodedEnts)) "\n")
              )

              ; instantiate lists needed to keep parts organized while processing
              (setq subtractions (list))
              (setq stylesNribs (list))
              (setq skins (list))
              (setq unmodified (list))

              ; instantiate properties needed to name block
              (setq width nil)
              (setq base nil)
              (setq sides nil)

              ; begin processing items inside of block
              (setq j 0)
              (while (< j (sslength explodedEnts)) 
                (setq obj (ssname explodedEnts j))
                (setq objData (entget obj))
                (princ (strcat "\nNEW OBJECT:   " (cdr (assoc 0 objData)) "\n"))
                (cond 
                  ;IF object is 3dsolid on layer 0
                  ((and objData 
                        (eq (strcase (cdr (assoc 0 objData))) "3DSOLID")
                        (eq (strcase (cdr (assoc 8 objData))) "0")
                   )

                   ; pool 3d objects to be subtractions later
                   (progn 
                     (setq subtractions (cons obj subtractions))
                   )
                  )
                  ; ELSE IF object is polyline and 3/4"
                  ((and objData 
                        (eq (strcase (cdr (assoc 0 objData))) "LWPOLYLINE")
                        (eq (strcase (cdr (assoc 8 objData)))
                            "CAD (.75) THICK MATERIAL LAYER"
                        )
                   )
                   ; Extrude styles and ribs to be 2.5" height and set layer again
                   (progn 
                     (command "_extrude" obj "" "2.5")
                     (setq newObj (entlast))
                     (setq objData (entget newObj))
                     (setq objData (subst 
                                     (cons 8 "CAD (.75) THICK MATERIAL LAYER")
                                     (assoc 8 objData)
                                     objData
                                   )
                     )
                     (entmod objData)
                     (setq stylesNribs (cons newObj stylesNribs))
                   )
                  )
                  ; ELSE IF object is polyline and 1/4"
                  ((and objData 
                        (eq (strcase (cdr (assoc 0 objData))) "LWPOLYLINE")
                        (eq (strcase (cdr (assoc 8 objData))) 
                            "CAD (.25) THICK MATERIAL LAYER"
                        )
                   )
                   ; Extrude panel skins to be 95" height and set layer again
                   (progn 
                     (command "_extrude" obj "" height)
                     (setq newObj (entlast))
                     (setq objData (entget newObj))
                     (setq objData (subst 
                                     (cons 8 "CAD (.25) THICK MATERIAL LAYER")
                                     (assoc 8 objData)
                                     objData
                                   )
                     )
                     (entmod objData)
                     (setq skins (cons newObj skins))
                   )
                  )
                  ; ELSE IF object is line and defpoints"
                  ((and objData 
                        (eq (strcase (cdr (assoc 0 objData))) "LINE")
                        (eq (strcase (cdr (assoc 8 objData))) "DEFPOINTS")
                   )
                   ;find the length of the line and set it to width for block title
                   (progn 
                     (setq linelength (distance (cdr (assoc 10 objData)) 
                                                (cdr (assoc 11 objData))
                                      )
                     )
                     (setq base (cdr (assoc 10 objData)))
                     ;sets width to a 2 whole and 3 decimal number
                     (setq width (rtos linelength 2 3))
                     (entdel obj)
                   )
                  )
                  ; ELSE item wont be modified at all
                  (T
                   (progn 
                     (setq unmodified (cons obj unmodified))
                   )
                  )
                )

                (setq j (1+ j))
              )

              ; merge all subtraction items into 1 blob
              (command "_union")
              (foreach obj subtractions (command obj))
              (command "")
              (setq subBlob (ssname (ssget "_P") 0))

              ; subtract from every rib a copy of the subtraction blob
              (foreach rib stylesNribs 
                (progn 
                  (entmake (entget subBlob))
                  (setq subCopy (entlast))
                  (command "_subtract" rib "" subCopy "")
                )
              )
              ;dump the blob
              (command "_erase" subblob "")

              ;create SS or DS identification for block title
              (if (> (length skins) 1) 
                (setq sides "DS")
                (setq sides "SS")
              )

              ; create block title
              (setq title (strcat "Custom " sides " " width "in x " height "in"))

              ; generate a block from all 3 lists of items
              ; -block, name, base point, item selection, enter
              (command "-block" title base)
              (foreach blk stylesNribs (command blk))
              (foreach blk skins (command blk))
              (foreach blk unmodified (command blk))
              (command "")

              ; insert newly created block in same location
              ; -insert, name, base, scale x, scale y, rotation
              (command "-insert" title base 1 1 0)
            )
            (princ "\nSkipping incorrect item")
          )
          (setq i (1+ i))
        )
      )
    )
    (prompt "\nDing Fries are done!")
  )
)