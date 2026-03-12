(defun c:SPMAUTO ( / prefix startNum counter ss ent newName exData oldData newData loop)
  ;; 1. Minta Prefix & Angka
  (setq prefix (getstring T "\nMasukkan Prefix (contoh: KLP.048.A-): "))
  (setq startNum (getint "\nMasukkan Angka Mulai (contoh: 1): "))
  (if (not startNum) (setq startNum 1))

  ;; 2. Mendaftarkan Tabel "KML_DATA" ke dalam sistem AutoCAD
  (if (not (tblsearch "APPID" "$SPM-[KML_DATA]"))
    (regapp "$SPM-[KML_DATA]")
  )

  (setq counter startNum)
  (setq loop T)
  
  ;; 3. Loop Tembak Target (Bisa di-klik atau di-blok kotak Ijo/Biru)
  (while loop
    (princ (strcat "\nPilih Polygon untuk " prefix (if (< counter 10) (strcat "0" (itoa counter)) (itoa counter)) " (Tekan Spasi/Enter untuk Stop): "))
    
    (if (setq ss (ssget ":S"))
      (progn
        (setq ent (ssname ss 0))
        (setq newName (strcat prefix (if (< counter 10) (strcat "0" (itoa counter)) (itoa counter))))

        ;; 4. RAKIT DNA SPATIAL MANAGER (Sesuai hasil hack lu)
        (setq exData
          (list -3
            (list "$SPM-[KML_DATA]"
              (cons 1002 "{")
              (cons 1000 "[name]")
              (cons 1000 newName)
              (cons 1002 "}")
            )
          )
        )

        ;; 5. Suntik ke Polygon
        (setq oldData (entget ent)) ; Ambil data polygon tanpa XData
        (setq newData (append oldData (list exData))) ; Gabungin sama DNA baru
        (entmod newData)

        (princ (strcat "\n✅ NAMA MASUK: " newName))
        (setq counter (1+ counter))
      )
      ;; Kalau Spasi/Enter di area kosong, program berhenti
      (setq loop nil) 
    )
  )
  (princ "\nSelesai! Saat Export, pilih opsi Data Field -> KML_DATA -> name.")
  (princ)
)