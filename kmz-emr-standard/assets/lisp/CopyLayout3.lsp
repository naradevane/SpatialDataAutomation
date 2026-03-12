(defun c:CopyLayout (/ layoutName newLayoutName i numCopies)
  ;; Meminta nama layout yang akan digandakan
  (setq layoutName (getstring "\nMasukkan nama layout yang ingin digandakan: "))
  
  ;; Meminta jumlah salinan yang diinginkan
  (setq numCopies (getint "\nMasukkan jumlah salinan yang diinginkan: "))
  
  ;; Menggandakan layout sebanyak numCopies kali
  (if (and layoutName numCopies (> numCopies 0))
    (progn
      (setq i 1)
      (while (<= i numCopies)
        (setq newLayoutName (strcat layoutName " (" (itoa i) ")"))
        (command "_.layout" "copy" layoutName newLayoutName)
        (setq i (1+ i))
      )
      (princ (strcat "\nLayout " layoutName " telah digandakan sebanyak " (itoa numCopies) " kali."))
    )
    (princ "\nProses digandakan dibatalkan.")
  )
  (princ)
)

(princ "\nKetik CopyLayout untuk menjalankan perintah ini.")
(princ)
