(defun C:T2M (/ ss i ent obj txt ins height rotation style layer color newmtxt spacecount spaces justify vmode attachpoint entdata posopt)
  (princ "\nT2M - Text to MText with Spacing")
  
  ;; Set default jumlah spasi = 2
  (if (not spacecount)
    (setq spacecount 2)
  )
  
  ;; Pilih posisi spasi
  (initget "Atas Bawah")
  (setq posopt (getkword "\nPosisi spasi [Atas/Bawah] <Bawah>: "))
  (if (not posopt)
    (setq posopt "Bawah")
  )
  
  ;; Input jumlah spasi dari user
  (initget 6)
  (setq spacecount 
    (cond
      ((getint (strcat "\nMasukkan jumlah baris spasi <" (itoa spacecount) ">: ")))
      (spacecount)
    )
  )
  
  ;; Buat string spasi PERSIS sesuai input user
  (setq spaces "")
  (repeat spacecount
    (setq spaces (strcat spaces "\n "))
  )
  
  (princ (strcat "\nJumlah baris spasi: " (itoa spacecount)))
  (princ (strcat " | Posisi: " posopt " (MTEXT akan " (if (= posopt "Atas") "naik ke atas" "turun ke bawah") ")"))
  (princ "\nPilih objek TEXT yang akan dikonversi...")
  
  ;; Select TEXT objects only
  (setq ss (ssget '((0 . "TEXT"))))
  
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entdata (entget ent))
        (setq obj (vlax-ename->vla-object ent))
        
        ;; Ambil properties dari TEXT
        (setq txt (vla-get-TextString obj))
        (setq height (vla-get-Height obj))
        (setq rotation (vla-get-Rotation obj))
        (setq style (vla-get-StyleName obj))
        (setq layer (vla-get-Layer obj))
        (setq color (vla-get-Color obj))
        
        ;; Ambil horizontal justification (DXF 72)
        (setq justify (cdr (assoc 72 entdata)))
        (if (not justify) (setq justify 0))
        
        ;; Ambil vertical mode (DXF 73)
        (setq vmode (cdr (assoc 73 entdata)))
        (if (not vmode) (setq vmode 0))
        
        ;; Ambil insertion point yang tepat
        (if (= justify 0)
          (setq ins (cdr (assoc 10 entdata)))  ;; Insertion point
          (setq ins (cdr (assoc 11 entdata)))  ;; Alignment point
        )
        
        ;; Konversi TEXT justification ke MTEXT attachment point
        (setq attachpoint
          (cond
            ;; Vertical = 0 (Baseline) atau 1 (Bottom) -> Bottom
            ((or (= vmode 0) (= vmode 1))
             (cond
               ((= justify 0) 7)  ;; Left -> Bottom Left
               ((= justify 1) 8)  ;; Center -> Bottom Center
               ((= justify 2) 9)  ;; Right -> Bottom Right
               ((= justify 4) 8)  ;; Middle -> Bottom Center
               (T 7)
             )
            )
            ;; Vertical = 2 (Middle)
            ((= vmode 2)
             (cond
               ((= justify 0) 4)  ;; Left -> Middle Left
               ((= justify 1) 5)  ;; Center -> Middle Center
               ((= justify 2) 6)  ;; Right -> Middle Right
               ((= justify 4) 5)  ;; Middle -> Middle Center
               (T 4)
             )
            )
            ;; Vertical = 3 (Top)
            ((= vmode 3)
             (cond
               ((= justify 0) 1)  ;; Left -> Top Left
               ((= justify 1) 2)  ;; Center -> Top Center
               ((= justify 2) 3)  ;; Right -> Top Right
               ((= justify 4) 2)  ;; Middle -> Top Center
               (T 1)
             )
            )
            ;; Default
            (T 7)
          )
        )
        
        ;; PERUBAHAN: Balik logic biar lebih intuitif
        ;; Atas = spasi di bawah text (MTEXT naik)
        ;; Bawah = spasi di atas text (MTEXT turun)
        (if (= posopt "Atas")
          (setq txt (strcat txt spaces))      ;; Spasi di bawah = MTEXT naik
          (setq txt (strcat spaces txt))      ;; Spasi di atas = MTEXT turun
        )
        
        ;; Buat MTEXT baru
        (setq newmtxt (vla-addMText 
                        (vla-get-ModelSpace 
                          (vla-get-ActiveDocument (vlax-get-acad-object))
                        )
                        (vlax-3d-point ins)
                        0.0
                        txt
                      )
        )
        
        ;; Set properties MTEXT
        (vla-put-Height newmtxt height)
        (vla-put-Rotation newmtxt rotation)
        (vla-put-StyleName newmtxt style)
        (vla-put-Layer newmtxt layer)
        (vla-put-Color newmtxt color)
        
        ;; Set attachment point sesuai justify TEXT
        (vla-put-AttachmentPoint newmtxt attachpoint)
        
        ;; Update insertion point setelah attachment berubah
        (vla-put-InsertionPoint newmtxt (vlax-3d-point ins))
        
        ;; Hapus TEXT lama
        (vla-delete obj)
        
        (setq i (1+ i))
      )
      (princ (strcat "\n" (itoa (sslength ss)) " TEXT berhasil dikonversi ke MTEXT."))
    )
    (princ "\nTidak ada TEXT yang dipilih.")
  )
  (princ)
)

(princ "\nT2M loaded - Convert TEXT to MTEXT with spacing (default: 2 lines)")
(princ)