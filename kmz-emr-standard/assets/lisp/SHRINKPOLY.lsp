;; ========================================================================
;; AUTO CLOSE & SHRINK POLYLINE - VERSI PERBAIKAN
;; ========================================================================
;; Fungsi: Menutup polyline terbuka dan membuat polyline baru yang ter-shrink
;; ke arah dalam dengan jarak offset yang ditentukan user
;; ========================================================================

(defun C:SHRINKPOLY (/ ss i ent offset_dist layer_name obj_list new_obj)
  
  ;; Fungsi untuk membuat layer baru jika belum ada
  (defun create_layer (lname lcolor / ltable)
    (setq ltable (vla-get-layers 
                   (vla-get-activedocument (vlax-get-acad-object))))
    (if (not (tblsearch "LAYER" lname))
      (progn
        (vla-add ltable lname)
        (vla-put-color (vla-item ltable lname) lcolor)
        (princ (strcat "\nLayer '" lname "' dibuat."))
      )
      (princ (strcat "\nLayer '" lname "' sudah ada."))
    )
  )
  
  ;; Fungsi untuk mengecek apakah polyline tertutup
  (defun is_closed (ename / entdata)
    (setq entdata (entget ename))
    (= 1 (logand 1 (cdr (assoc 70 entdata))))
  )
  
  ;; Fungsi untuk menutup polyline
  (defun close_polyline (ename / entdata newdata)
    (if (not (is_closed ename))
      (progn
        (setq entdata (entget ename))
        (setq newdata (subst (cons 70 (logior 1 (cdr (assoc 70 entdata))))
                             (assoc 70 entdata)
                             entdata))
        (entmod newdata)
        (entupd ename)
        (princ "\n  - Polyline ditutup")
        T
      )
      (progn
        (princ "\n  - Polyline sudah tertutup")
        nil
      )
    )
  )
  
  ;; Fungsi untuk shrink polyline menggunakan OFFSET dengan center point
  (defun shrink_polyline (ename offset_val target_layer / old_layer cmd_echo 
                          center_pt obj_count_before obj_count_after 
                          new_ss new_ent vla_obj)
    (setq old_layer (getvar "CLAYER"))
    (setq cmd_echo (getvar "CMDECHO"))
    (setvar "CMDECHO" 0)
    
    ;; Hitung titik tengah polyline untuk menentukan arah offset
    (setq vla_obj (vlax-ename->vla-object ename))
    
    ;; Dapatkan bounding box untuk menentukan center point
    (vla-getboundingbox vla_obj 'minpt 'maxpt)
    (setq center_pt (list 
                      (/ (+ (vlax-safearray-get-element minpt 0)
                            (vlax-safearray-get-element maxpt 0)) 2.0)
                      (/ (+ (vlax-safearray-get-element minpt 1)
                            (vlax-safearray-get-element maxpt 1)) 2.0)
                      (/ (+ (vlax-safearray-get-element minpt 2)
                            (vlax-safearray-get-element maxpt 2)) 2.0)
                    ))
    
    ;; Set layer target sebagai current layer
    (setvar "CLAYER" target_layer)
    
    ;; Hitung jumlah objek sebelum offset
    (setq obj_count_before (if (setq new_ss (ssget "X" (list (cons 8 target_layer))))
                              (sslength new_ss)
                              0))
    
    ;; Gunakan command OFFSET dengan titik center sebagai referensi arah
    ;; Offset negatif (ke dalam) untuk shrink
    (command "._OFFSET" offset_val ename center_pt "")
    
    ;; Tunggu command selesai
    (while (> (getvar "CMDACTIVE") 0)
      (command "")
    )
    
    ;; Hitung jumlah objek setelah offset
    (setq obj_count_after (if (setq new_ss (ssget "X" (list (cons 8 target_layer))))
                             (sslength new_ss)
                             0))
    
    ;; Kembalikan layer semula
    (setvar "CLAYER" old_layer)
    (setvar "CMDECHO" cmd_echo)
    
    ;; Cek apakah objek baru berhasil dibuat
    (if (> obj_count_after obj_count_before)
      (progn
        (princ "\n  - Shrink polyline berhasil dibuat")
        T
      )
      (progn
        (princ "\n  - GAGAL: Polyline terlalu kecil atau offset terlalu besar!")
        nil
      )
    )
  )
  
  ;; ====================================================================
  ;; MAIN PROGRAM
  ;; ====================================================================
  
  (princ "\n========================================")
  (princ "\nAUTO CLOSE & SHRINK POLYLINE")
  (princ "\n========================================")
  
  ;; Minta user untuk memilih polyline
  (princ "\nPilih polyline yang akan di-shrink...")
  (setq ss (ssget '((0 . "*POLYLINE"))))
  
  (if ss
    (progn
      ;; Minta input jarak shrink dari user
      (initget 7) ;; Tidak boleh 0, negatif, atau null
      (setq offset_dist (getdist "\nMasukkan jarak shrink ke dalam (contoh: 10): "))
      
      (if offset_dist
        (progn
          ;; Tentukan nama layer untuk hasil shrink
          (setq layer_name "SHRINKED")
          
          ;; Buat layer baru jika belum ada (warna cyan = 4)
          (create_layer layer_name 4)
          
          ;; Proses setiap polyline yang dipilih
          (setq i 0)
          (setq success_count 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            
            (princ (strcat "\n\nMemproses polyline #" (itoa (+ i 1)) ":"))
            
            ;; Tutup polyline jika belum tertutup
            (close_polyline ent)
            
            ;; Regenerate untuk memastikan perubahan diterapkan
            (command "._REGEN")
            
            ;; Lakukan shrink dengan offset ke dalam
            (if (shrink_polyline ent offset_dist layer_name)
              (setq success_count (+ success_count 1))
            )
            
            (setq i (+ i 1))
          )
          
          (princ "\n========================================")
          (princ (strcat "\nSelesai! " (itoa success_count) " dari " 
                        (itoa (sslength ss)) " polyline berhasil di-shrink."))
          (if (< success_count (sslength ss))
            (princ "\nBeberapa polyline gagal (mungkin terlalu kecil atau offset terlalu besar)")
          )
          (princ "\n========================================")
        )
        (princ "\n*** Jarak shrink tidak valid! ***")
      )
    )
    (princ "\n*** Tidak ada polyline yang dipilih! ***")
  )
  
  (princ)
)

;; ========================================================================
;; PERINTAH ALTERNATIF: Shrink dengan validasi area
;; ========================================================================

(defun C:SHRINKPOLY2 (/ ss i ent offset_dist layer_name vla_obj area)
  
  (princ "\n========================================")
  (princ "\nAUTO CLOSE & SHRINK POLYLINE (dengan validasi)")
  (princ "\n========================================")
  
  ;; Minta user untuk memilih polyline
  (princ "\nPilih polyline yang akan di-shrink...")
  (setq ss (ssget '((0 . "*POLYLINE"))))
  
  (if ss
    (progn
      ;; Minta input jarak shrink
      (initget 7)
      (setq offset_dist (getdist "\nMasukkan jarak shrink ke dalam: "))
      
      ;; Minta input nama layer
      (setq layer_name (getstring T "\nNama layer untuk hasil shrink [SHRINKED]: "))
      (if (or (not layer_name) (= layer_name ""))
        (setq layer_name "SHRINKED")
      )
      
      (if offset_dist
        (progn
          ;; Buat layer jika belum ada
          (create_layer layer_name 3)
          
          ;; Proses batch dengan validasi area
          (setq i 0)
          (setq success_count 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (princ (strcat "\n\nMemproses polyline #" (itoa (+ i 1)) ":"))
            
            ;; Tutup polyline
            (close_polyline ent)
            (command "._REGEN")
            
            ;; Cek area polyline
            (setq vla_obj (vlax-ename->vla-object ent))
            (setq area (vla-get-area vla_obj))
            
            (princ (strcat "\n  - Area polyline: " (rtos area 2 2)))
            
            ;; Estimasi area minimum yang dibutuhkan
            (if (> area (* 4 offset_dist offset_dist))
              (progn
                (if (shrink_polyline ent offset_dist layer_name)
                  (setq success_count (+ success_count 1))
                )
              )
              (princ "\n  - LEWATI: Polyline terlalu kecil untuk di-shrink dengan jarak ini")
            )
            
            (setq i (+ i 1))
          )
          
          (princ "\n========================================")
          (princ (strcat "\nSelesai! " (itoa success_count) " dari " 
                        (itoa (sslength ss)) " polyline berhasil di-shrink."))
          (princ "\n========================================")
        )
        (princ "\n*** Jarak shrink tidak valid! ***")
      )
    )
    (princ "\n*** Tidak ada polyline yang dipilih! ***")
  )
  
  (princ)
)

;; ========================================================================
;; CARA PENGGUNAAN:
;; ========================================================================
;; 1. Load script ini dengan perintah APPLOAD di AutoCAD
;; 2. Ketik SHRINKPOLY di command line
;; 3. Pilih satu atau banyak polyline
;; 4. Masukkan jarak shrink (misalnya: 10)
;;    - Nilai ini SAMA seperti nilai offset di AutoCAD
;;    - Sesuai dengan unit drawing Anda (mm, cm, m, dll)
;; 5. Script akan otomatis:
;;    - Menutup polyline yang belum tertutup
;;    - Membuat polyline baru yang ter-shrink di layer "SHRINKED"
;;
;; CATATAN:
;; - Jika polyline terlalu kecil dibanding nilai offset, akan muncul pesan gagal
;; - Gunakan SHRINKPOLY2 untuk melihat validasi area sebelum shrink
;; ========================================================================

(princ "\n*** Script SHRINKPOLY loaded (FIXED VERSION) ***")
(princ "\nKetik SHRINKPOLY atau SHRINKPOLY2 untuk memulai")
(princ)