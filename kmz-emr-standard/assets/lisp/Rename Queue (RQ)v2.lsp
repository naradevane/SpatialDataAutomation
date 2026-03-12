;; ============================================================================
;; AutoLISP Script: RQ (Rename Queue) - Version 2
;; Fungsi: Rename objek TEXT/MTEXT/Block Attribute secara otomatis
;; berdasarkan urutan alfabetis dengan pola ID yang ditentukan user
;; Update: Support untuk prefix DAN suffix (depan & belakang nomor)
;; ============================================================================

(defun c:RQ (/ ss obj-list sorted-list prefix suffix start-num digit-count i new-id obj ent-data ent-name sort-choice text-value att-list start-num-str pattern-choice)
  
  ;; Fungsi untuk mengambil teks dari objek
  (defun get-object-text (ent / ent-data obj-type text-value)
    (setq ent-data (entget ent))
    (setq obj-type (cdr (assoc 0 ent-data)))
    (cond
      ((= obj-type "TEXT")
       (cdr (assoc 1 ent-data)))
      ((= obj-type "MTEXT")
       (cdr (assoc 1 ent-data)))
      ((= obj-type "INSERT")
       ;; Cari attribute POLE_ID
       (setq att-list (vlax-invoke (vlax-ename->vla-object ent) 'GetAttributes))
       (foreach att att-list
         (if (= (strcase (vlax-get att 'TagString)) "POLE_ID")
           (setq text-value (vlax-get att 'TextString))
         )
       )
       text-value)
      (t nil)
    )
  )
  
  ;; Fungsi untuk mengatur teks objek
  (defun set-object-text (ent new-text / ent-data obj-type)
    (setq ent-data (entget ent))
    (setq obj-type (cdr (assoc 0 ent-data)))
    (cond
      ((= obj-type "TEXT")
       (entmod (subst (cons 1 new-text) (assoc 1 ent-data) ent-data)))
      ((= obj-type "MTEXT")
       (entmod (subst (cons 1 new-text) (assoc 1 ent-data) ent-data)))
      ((= obj-type "INSERT")
       ;; Update attribute POLE_ID
       (setq att-list (vlax-invoke (vlax-ename->vla-object ent) 'GetAttributes))
       (foreach att att-list
         (if (= (strcase (vlax-get att 'TagString)) "POLE_ID")
           (vlax-put att 'TextString new-text)
         )
       ))
    )
  )
  
  ;; Fungsi sorting berdasarkan teks (A-Z)
  (defun sort-by-text-asc (lst / )
    (vl-sort lst 
      '(lambda (a b) 
         (< (strcase (car a)) (strcase (car b)))
       )
    )
  )
  
  ;; Fungsi sorting berdasarkan teks (Z-A)
  (defun sort-by-text-desc (lst / )
    (vl-sort lst 
      '(lambda (a b) 
         (> (strcase (car a)) (strcase (car b)))
       )
    )
  )
  
  ;; Fungsi untuk membuat nomor dengan padding
  (defun format-number (num digits / num-str)
    (setq num-str (itoa num))
    (while (< (strlen num-str) digits)
      (setq num-str (strcat "0" num-str))
    )
    num-str
  )
  
  ;; Main Program
  (princ "\n=== RQ - Rename Queue Script v2 ===")
  (princ "\nPilih objek TEXT, MTEXT, atau Block dengan attribute POLE_ID:")
  
  ;; Pilih objek
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
    (progn
      ;; Kumpulkan data objek dan teksnya
      (setq obj-list '())
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent-name (ssname ss i))
        (setq text-value (get-object-text ent-name))
        (if text-value
          (setq obj-list (cons (list text-value ent-name) obj-list))
        )
        (setq i (1+ i))
      )
      
      ;; Cek apakah ada objek yang valid
      (if obj-list
        (progn
          ;; Tanya urutan sorting
          (princ "\n\nPilih urutan sorting:")
          (princ "\n1. A-Z (Ascending)")
          (princ "\n2. Z-A (Descending)")
          (princ "\nMasukkan pilihan (1/2): ")
          (setq sort-choice (getstring))
          
          ;; Urutkan berdasarkan pilihan user
          (if (= sort-choice "2")
            (progn
              (setq sorted-list (sort-by-text-desc obj-list))
              (princ "\nUrutan: Z-A (Descending)")
            )
            (progn
              (setq sorted-list (sort-by-text-asc obj-list))
              (princ "\nUrutan: A-Z (Ascending)")
            )
          )
          
          (princ (strcat "\nDitemukan " (itoa (length sorted-list)) " objek."))
          (princ "\nUrutan saat ini:")
          (setq i 1)
          (foreach item sorted-list
            (princ (strcat "\n" (itoa i) ". " (car item)))
            (setq i (1+ i))
          )
          
          ;; Pilih pola penamaan
          (princ "\n\nPilih pola penamaan:")
          (princ "\n1. PREFIX + Nomor (contoh: MR.XXX.P001)")
          (princ "\n2. Nomor + SUFFIX (contoh: 1A, 2A, 3A)")
          (princ "\n3. PREFIX + Nomor + SUFFIX (contoh: MR.001.A)")
          (princ "\nMasukkan pilihan (1/2/3): ")
          (setq pattern-choice (getstring))
          
          ;; Set prefix dan suffix berdasarkan pilihan
          (cond
            ;; Pilihan 1: Prefix saja
            ((= pattern-choice "1")
             (princ "\nMasukkan PREFIX (contoh: MR.XXX.P): ")
             (setq prefix (getstring))
             (setq suffix ""))
            
            ;; Pilihan 2: Suffix saja
            ((= pattern-choice "2")
             (setq prefix "")
             (princ "\nMasukkan SUFFIX (contoh: A): ")
             (setq suffix (getstring)))
            
            ;; Pilihan 3: Prefix dan Suffix
            ((= pattern-choice "3")
             (princ "\nMasukkan PREFIX (contoh: MR.): ")
             (setq prefix (getstring))
             (princ "Masukkan SUFFIX (contoh: .A): ")
             (setq suffix (getstring)))
            
            ;; Default: Prefix saja
            (t
             (princ "\nMasukkan PREFIX (contoh: MR.XXX.P): ")
             (setq prefix (getstring))
             (setq suffix ""))
          )
          
          ;; Input digit awal
          (princ "\nMasukkan nomor awal (contoh: 001 atau 1): ")
          (setq start-num-str (getstring))
          (setq start-num (atoi start-num-str))
          (setq digit-count (strlen start-num-str))
          
          ;; Konfirmasi
          (princ "\n\n=== KONFIRMASI ===")
          (if (> (strlen prefix) 0)
            (princ (strcat "\nPrefix: " prefix))
            (princ "\nPrefix: (tidak ada)")
          )
          (if (> (strlen suffix) 0)
            (princ (strcat "\nSuffix: " suffix))
            (princ "\nSuffix: (tidak ada)")
          )
          (princ (strcat "\nNomor awal: " start-num-str " (" (itoa digit-count) " digit)"))
          (princ (strcat "\n\nContoh hasil:"))
          (princ (strcat "\n  " prefix (format-number start-num digit-count) suffix))
          (princ (strcat "\n  " prefix (format-number (1+ start-num) digit-count) suffix))
          (princ (strcat "\n  " prefix (format-number (+ start-num 2) digit-count) suffix))
          (princ "\n\nTekan ENTER untuk melanjutkan (atau ESC untuk batal)...")
          (getstring)
          
          ;; Proses rename
          (princ "\n\nMemproses...")
          (setq i 0)
          (foreach item sorted-list
            (setq new-id (strcat prefix (format-number (+ start-num i) digit-count) suffix))
            (setq obj (cadr item))
            (set-object-text obj new-id)
            (princ (strcat "\n" (car item) " → " new-id))
            (setq i (1+ i))
          )
          (princ (strcat "\n\n=== SELESAI ==="))
          (princ (strcat "\n" (itoa (length sorted-list)) " objek berhasil di-rename."))
        )
        (princ "\nTidak ada objek TEXT, MTEXT, atau Block dengan attribute POLE_ID yang ditemukan.")
      )
    )
    (princ "\nTidak ada objek yang dipilih.")
  )
  
  (princ)
)

;; Informasi command
(princ "\n=========================================")
(princ "\nAutoLISP Script loaded successfully!")
(princ "\nKetik RQ untuk menjalankan script rename.")
(princ "\n=========================================")
(princ)
