(defun c:S3 (/ ss i total ent entData dimVal)
  (setq total 0.0)
  (princ "\nSelect aligned dimensions: ")
  (setq ss (ssget '((0 . "DIMENSION"))))
  
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq dimVal (cdr (assoc 42 entData))) ; Actual measurement value
        
        (if dimVal
          (progn
            (setq total (+ total dimVal))
            (princ (strcat "\nDimension " (itoa (+ i 1)) ": " (rtos dimVal 2 2)))
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n\n================================="))
      (princ (strcat "\nTOTAL: " (rtos total 2 2)))
      (princ (strcat "\n================================="))
    )
    (princ "\nNo dimensions selected!")
  )
  (princ)
)

(princ "\nType S3 to sum aligned dimensions")
(princ)