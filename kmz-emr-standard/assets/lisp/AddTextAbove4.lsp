(defun c:AddTextAbove (/ selset ent entdata inspt textheight textrot offset newinspt newtextwidth oldtextwidth)
  (setq selset (ssget "_:L" '((0 . "TEXT,MTEXT")))) ; Memilih beberapa teks
  (if selset
    (progn
      (setq offset (getreal "\nMasukkan jarak offset antara teks asli dan teks 'NN': "))
      (if (not offset) (setq offset 1.0)) ; Default offset jika tidak diberikan
      (repeat (sslength selset)
        (setq ent (ssname selset 0))
        (setq entdata (entget ent))
        (setq inspt (cdr (assoc 10 entdata)))
        (setq textheight (cdr (assoc 40 entdata)))
        (setq textrot (cdr (assoc 50 entdata))) ; Mendapatkan rotasi teks
        
        ; Menghitung lebar teks asli dan teks baru
        (setq oldtextwidth (* (strlen (cdr (assoc 1 entdata))) (cdr (assoc 41 entdata))))
        (setq newtextwidth (* (strlen "NN") (cdr (assoc 41 entdata))))
        
        ; Menghitung titik penyisipan baru di atas teks asli, disesuaikan agar di tengah
        (setq newinspt
              (polar
                (polar inspt textrot (/ (- oldtextwidth newtextwidth) 2)) ; Pusatkan secara horizontal
                (+ textrot (/ pi 2)) ; Rotasi 90 derajat untuk bergerak ke atas
                (+ textheight offset))) ; Tambahkan offset
        
        ; Menambahkan teks baru dengan rotasi yang sama
        (entmake
          (list
            (cons 0 "TEXT")
            (cons 10 newinspt)
            (cons 40 textheight)
            (cons 50 textrot)
            (cons 1 "NN")
            (cons 7 (cdr (assoc 7 entdata))) ; Menggunakan gaya teks yang sama
          )
        )
        (ssdel ent selset) ; Hapus entitas dari selection set untuk iterasi berikutnya
      )
      (princ "\nTeks 'NN' berhasil ditambahkan di atas teks yang dipilih dan terpusat.")
    )
    (princ "\nTidak ada teks yang dipilih.")
  )
  (princ)
)

(princ "\nKetik AddTextAbove untuk menjalankan perintah ini.")
(princ)
