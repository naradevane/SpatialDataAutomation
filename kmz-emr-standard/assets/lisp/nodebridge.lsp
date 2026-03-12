(defun c:NODEBRIDGE (/ oldCmdecho oldPedit oldOsMode oldDelobj oldHpgaptol entList bridgeList txtList pt count 
                       maxGap i len j e1 e2 b1 b2 validLinks link p1 p2 ang w deep 
                       p1_tip p1_ma p1_mb p2_tip p2_ma p2_mb bridgeEnt ssReg newReg unionReg 
                       ssExploded ssJoined markEnt en maxArea maxEnt area eLast txtStr txtHeight
                       allOrigNodes finalPts cleanedPts fPt closestPt origPt minDist d finalCleanEnt cPt
                       tmpOs *error*)
  (vl-load-com) 

  ;; --- SISTEM KEAMANAN (ERROR HANDLER) ---
  (defun *error* (msg)
    (if oldOsMode (setvar "OSMODE" oldOsMode))
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if oldPedit (setvar "PEDITACCEPT" oldPedit))
    (if oldDelobj (setvar "DELOBJ" oldDelobj))
    (if oldHpgaptol (setvar "HPGAPTOL" oldHpgaptol))
    (command "._UNDO" "_E")
    (princ (strcat "\n[!] Perintah dibatalkan: " msg))
    (princ)
  )

  ;; Simpan settingan awal user
  (setq oldCmdecho (getvar "CMDECHO"))
  (setq oldPedit (getvar "PEDITACCEPT"))
  (setq oldOsMode (getvar "OSMODE"))
  (setq oldDelobj (getvar "DELOBJ"))
  (setq oldHpgaptol (getvar "HPGAPTOL"))

  ;; Terapkan settingan script
  (setvar "CMDECHO" 0)
  (setvar "DELOBJ" 1)
  
  ;; --- FITUR BARU: AUTO-IGNORE GAP < 1 METER ---
  (setvar "HPGAPTOL" 1.0) 
  
  (setq entList '() bridgeList '() txtList '() count 1 allOrigNodes '())

  ;; Fungsi: Ekstrak semua titik sudut
  (defun get-vertices (ent / pts entdata)
    (setq entdata (entget ent))
    (foreach item entdata
      (if (= (car item) 10) (setq pts (cons (cdr item) pts)))
    )
    (reverse pts)
  )

  ;; FUNGSI: Bounding Box untuk Speed Optimization
  (defun get-bbox (ent / minPt maxPt obj)
    (setq obj (vlax-ename->vla-object ent))
    (vla-GetBoundingBox obj 'minPt 'maxPt)
    (list (vlax-safearray->list minPt) (vlax-safearray->list maxPt))
  )

  ;; FUNGSI: Hitung jarak Bounding Box
  (defun bbox-dist (box1 box2 / dx dy)
    (setq dx (max 0.0 (- (car (car box1)) (car (cadr box2))) (- (car (car box2)) (car (cadr box1)))))
    (setq dy (max 0.0 (- (cadr (car box1)) (cadr (cadr box2))) (- (cadr (car box2)) (cadr (cadr box1)))))
    (sqrt (+ (* dx dx) (* dy dy)))
  )

  ;; FUNGSI: Omni-Bridge (Koneksikan SEMUA titik yg masuk toleransi)
  (defun get-omni-links (ent1 ent2 maxGap / pts1 pts2 v1 v2 dst links)
    (setq pts1 (get-vertices ent1) pts2 (get-vertices ent2) links '())
    (foreach v1 pts1
      (foreach v2 pts2
        (setq dst (distance v1 v2))
        (if (<= dst maxGap)
          (setq links (cons (list v1 v2 dst) links))
        )
      )
    )
    links
  )

  (princ "\n--- TAHAP 1: KLIK AREA PERSIL ---")
  (while (setq pt (getpoint "\nKlik di DALAM area persil (Tekan ENTER jika selesai): "))
    (setq eLast (entlast))
    
    (setq tmpOs (getvar "OSMODE"))
    (setvar "OSMODE" 0)
    (vl-cmdf "-BOUNDARY" "A" "O" "P" "" "_non" pt "")
    (setvar "OSMODE" tmpOs)

    (if (not (eq eLast (entlast)))
      (progn
        (setq eLast (entlast))
        (setq entList (cons eLast entList))
        (command "._CHPROP" eLast "" "_C" "3" "")
        
        (setq allOrigNodes (append allOrigNodes (get-vertices eLast)))
        
        (setq txtStr (if (< count 10) (strcat "0" (itoa count)) (itoa count)))
        (setq txtHeight 0.5) 
        (entmake (list '(0 . "TEXT") (cons 10 pt) (cons 11 pt) (cons 40 txtHeight) '(72 . 1) '(73 . 2) (cons 1 txtStr) '(62 . 1)))
        (setq txtList (cons (entlast) txtList))
        
        (princ (strcat "\n[+] Area " txtStr " berhasil ditangkap."))
        (setq count (1+ count))
      )
      (princ "\n[!] AREA BOCOR! Garis terbuka lebih besar dari toleransi 1 meter.")
    )
  )

  (if (> (length entList) 1)
    (progn
      (princ "\n--- TAHAP 2: LIGHTNING PROXIMITY SCAN ---")
      
      (setq maxGap (getdist "\nMasukkan maksimal Jarak Gap/Lebar Jalan <5>: "))
      (if (not maxGap) (setq maxGap 5.0)) 
      
      (command "._UNDO" "_BE")

      (setq i 0 len (length entList))
      (while (< i (1- len))
        (setq e1 (nth i entList) b1 (get-bbox e1))
        (setq j (1+ i))
        (while (< j len)
          (setq e2 (nth j entList) b2 (get-bbox e2))
          
          ;; FILTER SPEED
          (if (<= (bbox-dist b1 b2) maxGap)
            (progn
              ;; Omni-Bridge
              (setq validLinks (get-omni-links e1 e2 maxGap))
              
              (foreach link validLinks
                (setq p1 (car link) p2 (cadr link))
                (setq ang (angle p1 p2) w 0.005 deep 0.05)  
                
                ;; SPEARHEAD SHAPE
                (setq p1_tip (polar p1 (+ ang pi) deep))
                (setq p1_ma (polar p1 (+ ang (/ pi 2)) w))
                (setq p1_mb (polar p1 (- ang (/ pi 2)) w))
                
                (setq p2_tip (polar p2 ang deep))
                (setq p2_ma (polar p2 (+ ang (/ pi 2)) w))
                (setq p2_mb (polar p2 (- ang (/ pi 2)) w))

                (setq tmpOs (getvar "OSMODE"))
                (setvar "OSMODE" 0)
                (command "._PLINE" "_non" p1_tip "_non" p1_ma "_non" p2_ma "_non" p2_tip "_non" p2_mb "_non" p1_mb "_C")
                (setvar "OSMODE" tmpOs)

                (setq bridgeEnt (entlast))
                (setq bridgeList (cons bridgeEnt bridgeList))
                (command "._CHPROP" bridgeEnt "" "_C" "2" "")
              )
            )
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )

      (setq entList (append bridgeList entList))

      (princ "\nSedang memproses Penggabungan & Smart Simplification...")

      ;; PROSES UNION
      (setq ssReg (ssadd))
      (foreach e entList
         (setq eLast (entlast))
         (command "._REGION" e "")
         (setq newReg (entlast))
         (if (not (eq eLast newReg)) (ssadd newReg ssReg))
         (if (entget e) (entdel e))
      )

      (if (> (sslength ssReg) 0)
        (progn
          (command "._UNION" ssReg "")
          (setq unionReg (entlast))

          (entmake (list '(0 . "POINT") '(10 0 0 0)))
          (setq markEnt (entlast))

          (command "._EXPLODE" unionReg)
          (setq ssExploded (ssget "_P"))

          (setvar "PEDITACCEPT" 1)
          (command "._PEDIT" "_M" ssExploded "" "_J" "0.01" "")
          
          (setq ssJoined (ssadd))
          (setq en markEnt)
          (while (setq en (entnext en))
            (if (wcmatch (cdr (assoc 0 (entget en))) "*POLYLINE")
              (ssadd en ssJoined)
            )
          )
          (if (entget markEnt) (entdel markEnt))

          ;; FILTER AREA + SMART SIMPLIFIER
          (if (> (sslength ssJoined) 0)
            (progn
              (setq i 0 maxArea -1.0 maxEnt nil)
              (repeat (sslength ssJoined)
                (setq en (ssname ssJoined i))
                (setq area (vla-get-Area (vlax-ename->vla-object en)))
                (if (> area maxArea) (setq maxArea area maxEnt en))
                (setq i (1+ i))
              )
              
              (setq i 0)
              (repeat (sslength ssJoined)
                (setq en (ssname ssJoined i))
                (if (eq en maxEnt)
                  (progn
                    (setq finalPts (get-vertices en))
                    (setq cleanedPts nil)
                    
                    (foreach fPt finalPts
                      (setq closestPt fPt minDist 1e99)
                      (foreach origPt allOrigNodes
                        (setq d (distance fPt origPt))
                        (if (< d minDist)
                          (setq minDist d closestPt origPt)
                        )
                      )
                      (if (<= minDist 0.5) (setq fPt closestPt))
                      
                      (if (not (equal fPt (car cleanedPts) 1e-4))
                        (setq cleanedPts (cons fPt cleanedPts))
                      )
                    )
                    (setq cleanedPts (reverse cleanedPts))
                    
                    (if (equal (car cleanedPts) (last cleanedPts) 1e-4)
                      (setq cleanedPts (reverse (cdr (reverse cleanedPts))))
                    )
                    
                    (setq tmpOs (getvar "OSMODE"))
                    (setvar "OSMODE" 0)
                    (command "._PLINE")
                    (foreach cPt cleanedPts (command "_non" cPt))
                    (command "_C")
                    (setvar "OSMODE" tmpOs)
                    
                    (setq finalCleanEnt (entlast))
                    (command "._CHPROP" finalCleanEnt "" "_C" "1" "") 
                    (entdel en) 
                  )
                  (entdel en) 
                )
                (setq i (1+ i))
              )
            )
          )
        )
      )

      (foreach tEnt txtList (if (entget tEnt) (entdel tEnt)))

      (command "._UNDO" "_E")
      (princ "\n✅ MANTAP! Outer Boundary Berhasil, Area Bocor (<1m) Telah Ditambal!")
    )
    (princ "\n[!] Butuh minimal 2 area persil untuk bisa disambung.")
  )

  (setvar "CMDECHO" oldCmdecho)
  (setvar "PEDITACCEPT" oldPedit)
  (setvar "DELOBJ" oldDelobj)
  (setvar "OSMODE" oldOsMode) 
  (setvar "HPGAPTOL" oldHpgaptol) ;; Kembalikan toleransi seperti semula
  (princ)
)

(defun c:NB () (c:NODEBRIDGE))