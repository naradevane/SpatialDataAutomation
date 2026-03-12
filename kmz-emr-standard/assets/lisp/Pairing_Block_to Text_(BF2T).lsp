(defun c:BF2Tv4 (/ ss radius blkList txtList candidates sorted-candidates 
                   used-blks used-txts blk txt dist i ename etype 
                   pair b-ent t-ent unmatched-count
                   b-center t-center)

  (vl-load-com) 
  (princ "\n--- Block to Text Auto-Pairing V4 (Force ActiveX) ---")

  (prompt "\nSeleksi Block Reference dan Text/MText: ")
  (setq ss (ssget '((0 . "INSERT,TEXT,MTEXT"))))

  (if ss
    (progn
      ;; Init variables
      (setq blkList '() txtList '() candidates '() used-blks '() used-txts '())

      ;; 1. Pisahkan Block dan Text
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i))
        (setq etype (cdr (assoc 0 (entget ename))))
        (cond
          ((= etype "INSERT") (setq blkList (cons ename blkList)))
          ((or (= etype "TEXT") (= etype "MTEXT")) (setq txtList (cons ename txtList)))
        )
        (setq i (1+ i))
      )

      (princ (strcat "\nBlock: " (itoa (length blkList)) " | Text: " (itoa (length txtList))))

      ;; 2. Input Radius (Default 50.0)
      (setq radius (getdist "\nTentukan Radius Pencarian (Jarak Tengah ke Tengah) <50.0>: "))
      (if (null radius) (setq radius 50.0))

      ;; 3. Cari Kandidat (Logic V3: Center-to-Center)
      (foreach b blkList
        (setq b-center (Get-Center b)) 
        (foreach t-obj txtList
          (setq t-center (Get-Center t-obj))
          (setq dist (distance (list (car b-center) (cadr b-center)) 
                               (list (car t-center) (cadr t-center))))
          (if (<= dist radius)
            (setq candidates (cons (list dist b t-obj) candidates))
          )
        )
      )

      ;; 4. Sortir
      (setq sorted-candidates 
            (vl-sort candidates 
                     '(lambda (e1 e2) (< (car e1) (car e2)))))

      ;; 5. Eksekusi SNAP (MENGGUNAKAN VLA-OBJECT)
      (foreach pair sorted-candidates
        (setq b-ent (cadr pair))
        (setq t-ent (caddr pair))

        (if (and (not (member b-ent used-blks))
                 (not (member t-ent used-txts)))
          (progn
            ;; PANGGIL FUNGSI SNAP BARU
            (SnapTextToBlock_VLA b-ent t-ent)
            
            (setq used-blks (cons b-ent used-blks))
            (setq used-txts (cons t-ent used-txts))
          )
        )
      )

      ;; 6. Cek Unmatched
      (setq unmatched-count 0)
      (foreach b blkList
        (if (not (member b used-blks))
          (progn (DrawRectBox b) (setq unmatched-count (1+ unmatched-count)))
        ))
      (foreach t-obj txtList
        (if (not (member t-obj used-txts))
          (progn (DrawRectBox t-obj) (setq unmatched-count (1+ unmatched-count)))
        ))

      (if (> unmatched-count 0)
        (princ (strcat "\nSelesai. Ada " (itoa unmatched-count) " objek gagal."))
        (princ "\nSukses! Cek hasilnya.")
      )
    )
    (princ "\nTidak ada objek terpilih.")
  )
  (princ)
)

;; --- FUNGSI SNAP BARU (The Fix) ---
(defun SnapTextToBlock_VLA (blk txt / blkObj txtObj targetPt alignment)
  ;; Konversi ke VLA Object (ActiveX)
  (setq blkObj (vlax-ename->vla-object blk))
  (setq txtObj (vlax-ename->vla-object txt))
  
  ;; Ambil titik insert block sebagai target (Variant Type)
  (setq targetPt (vla-get-InsertionPoint blkObj))

  ;; Cek tipe object Text karena cara memindahkannya beda
  (if (= (vla-get-ObjectName txtObj) "AcDbText")
    (progn
      ;; KASUS TEXT BIASA (DTEXT)
      ;; Cek Alignment. Jika Left (0), pakai InsertionPoint. 
      ;; Jika Center/Right/dll, harus pakai TextAlignmentPoint.
      (setq alignment (vla-get-Alignment txtObj))
      (if (or (= alignment 0) (= alignment 3) (= alignment 5)) ; 0=Left, 3=Aligned, 5=Fit
          (vla-put-InsertionPoint txtObj targetPt)
          (vla-put-TextAlignmentPoint txtObj targetPt)
      )
    )
    (progn
      ;; KASUS MTEXT
      ;; MText selalu nurut kalau diganti InsertionPoint-nya
      (vla-put-InsertionPoint txtObj targetPt)
    )
  )
  (vla-update txtObj) ;; Paksa update visual
)

;; --- Helper: Get Center (Sama seperti V3) ---
(defun Get-Center (ent / obj minPt maxPt)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'minPt 'maxPt)
  (setq minPt (vlax-safearray->list minPt))
  (setq maxPt (vlax-safearray->list maxPt))
  (list (/ (+ (car minPt) (car maxPt)) 2.0)
        (/ (+ (cadr minPt) (cadr maxPt)) 2.0)
        (/ (+ (caddr minPt) (caddr maxPt)) 2.0))
)

;; --- Helper: Draw Box ---
(defun DrawRectBox (ent / obj minPt maxPt pt1 pt2 pt3 pt4 offset)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'minPt 'maxPt)
  (setq minPt (vlax-safearray->list minPt))
  (setq maxPt (vlax-safearray->list maxPt))
  (setq offset 0.5) 
  (setq pt1 (list (- (car minPt) offset) (- (cadr minPt) offset)))
  (setq pt3 (list (+ (car maxPt) offset) (+ (cadr maxPt) offset)))
  (setq pt2 (list (car pt3) (cadr pt1)))
  (setq pt4 (list (car pt1) (cadr pt3)))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") 
                 '(62 . 1) '(90 . 4) '(70 . 1) 
                 (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4)))
)