(defun c:DrawLineAJa (/ selset ent entdata inspt textwidth textrot midpt endpt)
  (setq selset (ssget "_:L" '((0 . "TEXT,MTEXT")))) ; Memilih teks atau mtext
  (if selset
    (progn
      (setq endpt (getpoint "\nPilih titik akhir untuk garis: ")) ; Meminta titik akhir
      (repeat (sslength selset)
        (setq ent (ssname selset 0))
        (setq entdata (entget ent))
        (setq inspt (cdr (assoc 10 entdata))) ; Titik insert teks
        (setq textwidth (* (strlen (cdr (assoc 1 entdata))) (cdr (assoc 41 entdata))))
        (setq textrot (cdr (assoc 50 entdata))) ; Rotasi teks
        (setq midpt (polar inspt textrot (/ textwidth 2))) ; Menghitung titik tengah teks
        (entmake
          (list
            (cons 0 "LINE")
            (cons 10 midpt) ; Titik awal garis (tengah teks)
            (cons 11 endpt) ; Titik akhir garis
          )
        )
        (ssdel ent selset) ; Hapus entitas dari selection set
      )
      (princ "\nGaris berhasil ditambahkan dari tengah teks yang dipilih.")
    )
    (princ "\nTidak ada teks yang dipilih.")
  )
  (princ)
)

(princ "\nKetik DrawLineAJa untuk menjalankan perintah ini.")
(princ)
