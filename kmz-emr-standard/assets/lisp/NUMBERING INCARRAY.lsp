;;--------------------=={ CurveText.lsp }==-------------------;;
;;                                                            ;;
;;  Positions Text along a curve object (arc, circle, spline, ;;
;;  ellipse, line, lwpolyline, polyline), and rotates text    ;;
;;  to fit to the curve accordingly.                          ;;
;;                                                            ;;
;;  If run in versions > AutoCAD2000, the resultant text will ;;
;;  form an anonymous group.                                  ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version:  1.4    -    22-02-2012                          ;;
;;------------------------------------------------------------;;

(defun c:CurveText ( / sel str obj )
    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'CLAYER))))))
            (princ "\nCurrent Layer Locked.")
        )
        (   (and
                (setq sel
                    (LM:SelectionOrText "\nSpecify or Select Text String: "
                        (function
                            (lambda ( x ) (wcmatch (cdr (assoc 0 (entget x))) "*TEXT,ATTRIB"))
                        )
                    )
                )
                (or
                    (and
                        (eq 'STR (type sel))
                        (setq str sel)
                    )
                    (setq str (cdr (assoc 1 (entget sel))))
                )
                (setq obj (LM:SelectIf "\nSelect Curve: " 'LM:CurveObject-p))
            )
            (LM:CurveText str obj)
        )
    )
    (princ)
)

(defun LM:CurveText ( str ent / *error* 3pi/2 a1 a2 a3 acdoc acspc df di dr g1 g2 gr in ln lst ms obj p1 p2 p3 pi/2 ts )

    (defun *error* ( msg )
        (foreach obj lst
            (if
                (and
                    (not (vlax-erased-p obj))
                    (vlax-write-enabled-p obj)
                )
                (vla-delete obj)
            )
        )
        (if (null (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
          acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'paperspace 'modelspace))
    )
    (or *offset* (setq *offset* 0.0))
    (or *spacin* (setq *spacin* 1.1))

    (setq ts
        (/  (getvar 'textsize)
            (if (LM:isAnnotative (getvar 'textstyle))
                (cond ((getvar 'cannoscalevalue)) ( 1.0 ))
                1.0
            )
        )
    )

    (setq lst
        (mapcar
            (function
                (lambda ( c )
                    (setq obj (vla-addtext acspc (chr c) (vlax-3D-point (getvar 'VIEWCTR)) ts))
                    (vla-put-alignment obj acalignmentmiddlecenter)
                    obj
                )
            )
            (vl-string->list str)
        )
    )

    (setq ms    (princ "\nPosition Text: [+/-] Offset, [</>] Spacing")
          ln    (- (/ (1+ (strlen str)) 2.0))
          pi/2  (/ pi 2.0)
          3pi/2 (/ (* 3.0 pi) 2.0)
    )
    (while
        (progn
            (setq gr (grread t 15 0)
                  g1 (car  gr)
                  g2 (cadr gr)
            )
            (cond
                (   (or (= 05 g1) (= 03 g1))
                    (setq p1 (trans g2 1 0)
                          p2 (vlax-curve-getclosestpointto ent p1)
                          a1 (angle p2 p1)
                          di (vlax-curve-getdistatpoint ent p2)
                          dr (angle '(0.0 0.0 0.0) (vlax-curve-getfirstderiv ent (vlax-curve-getparamatpoint ent p2)))
                          df (- a1 dr)
                          in ln
                          a2 (cond
                                 (   (and (> dr pi/2) (<= dr pi))
                                     (- pi)
                                 )
                                 (   (and (> dr pi) (<= dr 3pi/2))
                                     pi
                                 )
                                 (   0.0   )
                             )
                    )
                    (foreach obj
                        (if (and (< pi/2 dr) (<= dr 3pi/2))
                            (reverse lst)
                            lst
                        )
                        (if (setq p3 (vlax-curve-getPointatDist ent (+ di (* (setq in (1+ in)) *spacin* ts))))
                            (progn
                                (setq a3 (angle '(0. 0. 0.) (vlax-curve-getfirstderiv ent (vlax-curve-getparamatpoint ent p3))))
                                (vla-put-TextAlignmentPoint obj
                                    (vlax-3D-point (polar p3 (+ a3 df) (* ts *offset*)))
                                )
                                (vla-put-rotation obj (+ a2 a3))
                            )
                        )
                    )
                    (= 05 g1)
                )
                (   (= 25 g1)
                    nil
                )
                (   (= 02 g1)
                    (cond
                        (   (member g2 '(13 32))
                            nil
                        )
                        (   (member g2 '(43 61))
                            (setq *offset* (+ *offset* 0.1))
                        )
                        (   (member g2 '(45 95))
                            (setq *offset* (- *offset* 0.1))
                        )
                        (   (member g2 '(46 62))
                            (setq *spacin* (+ *spacin* 0.05))
                        )
                        (   (member g2 '(44 60))
                            (setq *spacin* (- *spacin* 0.05))
                        )
                        (   (princ (strcat "\nInvalid Keypress." ms))
                        )
                    )
                )
                (   t  )
            )
        )        
    )
    
    (if (< 15.0 (atof (getvar 'ACADVER)))
        (vla-appenditems (vla-add (vla-get-groups acdoc) "*")
            (vlax-make-variant
                (vlax-safearray-fill
                    (vlax-make-safearray vlax-vbObject (cons 0 (1- (length lst))))
                    lst
                )
            )
        )
    )
    (princ)
)

(defun LM:CurveObject-p ( ent )
    (null
        (vl-catch-all-error-p
            (vl-catch-all-apply 'vlax-curve-getEndParam (list ent))
        )
    )
)

(defun LM:SelectIf ( msg pred )
    (
        (lambda ( f / e )
            (while
                (progn (setvar 'ERRNO 0) (setq e (car (entsel msg)))
                    (cond
                        (   (= 7 (getvar 'ERRNO))
                            (princ "\nMissed, try again.")
                        )
                        (   (eq 'ENAME (type e))
                            (if (and f (null (f e)))
                                (princ "\nInvalid Object.")
                            )
                        )
                    )
                )
            )
            e
        )
        (eval pred)
    )
)

(defun LM:isAnnotative ( style / obj xdt )
    (and
        (setq obj (tblobjname "STYLE" style))
        (setq xdt (cadr (assoc -3 (entget obj '("AcadAnnotative")))))
        (= 1 (cdr (assoc 1070 (reverse xdt))))
    )
)

(defun LM:SelectionOrText ( msg pred / en g1 g2 gr result )
    (setq pred (eval pred))
    
    (if msg
        (princ msg)
        (princ (setq msg "\nSelect Objects or Enter Text: "))
    )
    (setq result "")

    (while
        (progn
            (setq gr (grread t 13 2)
                  g1 (car  gr)
                  g2 (cadr gr)
            )
            (cond
                (   (= 03 g1)
                    (if (setq en (car (nentselp g2)))
                        (if (pred en)
                            (not (setq result en))
                            (princ (strcat "\nInvalid Object Selected." msg))
                        )
                        (princ (strcat "\nMissed, try again." msg))
                    )
                )
                (   (= 02 g1)
                    (cond
                        (   (< 31 g2 127)
                            (setq result (strcat result (princ (chr g2))))
                        )
                        (   (= 13 g2)
                            nil
                        )
                        (   (= 08 g2)
                            (if (< 0 (strlen result))
                                (progn
                                    (setq result (substr result 1 (1- (strlen result))))
                                    (princ (vl-list->string '(8 32 8)))
                                )
                            )
                            t
                        )
                        (   t   )
                    )
                )
                (   (= 25 g1)
                    nil
                )
                (   t   )
            )
        )
    )
    result
)

(vl-load-com)
(princ "\n:: CurveText.lsp | Version 1.4 | © Lee Mac 2012 www.lee-mac.com ::")
(princ "\n:: Type \"CurveText\" to Invoke ::")
(princ)

;;--------------------------=={ Object Align }==------------------------;;
;;                                                                      ;;
;;  This program will enable the user to dynamically align a selection  ;;
;;  of objects to a selected curve, with intuitive placement controls.  ;;
;;                                                                      ;;
;;  Upon starting the program with the command syntax 'OA', the user is ;;
;;  prompted to make a selection of objects to be aligned. Following a  ;;
;;  valid selection, the user is prompted to specify a base point to    ;;
;;  use during alignment; at this prompt, the program will use the      ;;
;;  center of the bounding box of the selection of objects by default.  ;;
;;                                                                      ;;
;;  The user is then prompted to select a curve object (this may be a   ;;
;;  Line, Polyline, Arc, Circle, Ellipse, XLine, Spline etc.) to which  ;;
;;  the objects are to be aligned. The selected curve may be a primary  ;;
;;  object, or nested with a Block or XRef to any level. After          ;;
;;  selection, the program offers several controls to aid with object   ;;
;;  placement displayed at the command line:                            ;;
;;                                                                      ;;
;;  [+/-] for [O]ffset | [</>] for [R]otation | [M]ultiple | <[E]xit>:  ;;
;;                                                                      ;;
;;  The offset of the objects from the curve may be controlled          ;;
;;  incrementally by a tenth of the object height using the '+' / '-'   ;;
;;  keys, or a specific offset may be entered upon pressing the 'O' or  ;;
;;  'o' key.                                                            ;;
;;                                                                      ;;
;;  The set of objects may be rotated anti-clockwise or clockwise by    ;;
;;  45 degrees relative to the curve by pressing the '<' or '>' keys    ;;
;;  respectively; alternatively, the user may enter a specific rotation ;;
;;  by pressing the 'R' or 'r' key.                                     ;;
;;                                                                      ;;
;;  The user may toggle 'Multiple mode' by pressing the 'M' or 'm' key; ;;
;;  when enabled, the user may continuously align multiple copies of    ;;
;;  the selected objects to the selected curve.                         ;;
;;                                                                      ;;
;;  Finally, the user may place the objects and exit the program by     ;;
;;  either clicking the left or right mouse buttons, pressing Enter or  ;;
;;  Space, or by pressing the 'E' or 'e' keys.                          ;;
;;                                                                      ;;
;;  The program should perform successfully in all UCS & Views, and in  ;;
;;  all versions of AutoCAD that have Visual LISP functions available   ;;
;;  (AutoCAD 2000 onwards running on a Windows OS).                     ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2010  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2010-05-01                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2011-05-07                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2012-12-11                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2012-12-14                                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2018-05-06                                      ;;
;;                                                                      ;;
;;  - Program modified to enable compatibility with all UCS & Views.    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2019-08-09                                      ;;
;;                                                                      ;;
;;  - Added 'Multiple' mode to allow the user to align multiple copies  ;;
;;    of the selected objects.                                          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2020-12-15                                      ;;
;;                                                                      ;;
;;  - Fixed bug causing divide by zero error when specifying the offset ;;
;;    for an object with zero height.                                   ;;
;;----------------------------------------------------------------------;;
;;  Version 1.7    -    2020-12-22                                      ;;
;;                                                                      ;;
;;  - Modified program to remove the final set of objects if the user   ;;
;;    exits the alignment using ENTER, SPACE, right-click, or Exit.     ;;
;;----------------------------------------------------------------------;;

(defun c:oa

    (
        /
        *error*
        bb1 bb2 blk bnm bpt
        def dis
        ent
        fac
        gr1 gr2
        idx inc
        llp lst
        mat msg
        obj ocs oss
        pi2 pt1 pt2 pt3 pt4
        sel
        tma tmp trm
        urp uxa
        vec 
    )

    (defun *error* ( msg )
        (if (and (= 'list (type trm)) (= 'ename (type ent)) (entget ent))
            (entdel ent)
        )
        (if (and (= 'vla-object (type blk)) (not (vlax-erased-p blk)))
            (vl-catch-all-apply 'vla-delete (list blk))
        )
        (if (and (= 'vla-object (type def)) (not (vlax-erased-p def)))
            (vl-catch-all-apply 'vla-delete (list def))
        )
        (foreach obj lst
            (if (not (vlax-erased-p obj))
                (vl-catch-all-apply 'vla-delete (list obj))
            )
        )
        (oa:endundo (oa:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (oa:startundo (oa:acdoc))
    (if (null oa|rot) (setq oa|rot 0.0))
    (if (null oa|off) (setq oa|off 0.0))
    
    (cond
        (   (or (oa:layerlocked (getvar 'clayer))
                (oa:layerlocked "0")
            )
            (princ "\nThe current layer or layer \"0\" is locked - please unlock these layers before using this program.")
        )
        (   (null (setq oss (oa:ssget "\nSelect objects to align: " '("_:L" ((0 . "~VIEWPORT"))))))
            (princ "\n*Cancel*")
        )
        (   (progn
                (setq bpt (getpoint "\nSpecify basepoint <center>: "))
                (while
                    (progn
                        (setvar 'errno 0)
                        (setq sel (nentselp "\nSelect curve to align objects <exit>: "))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\nMissed, try again.")
                            )
                            (   (= 'ename (type (car sel)))
                                (if
                                    (not
                                        (or (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                                            (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getendparam (list (car sel)))))
                                        )
                                    )
                                    (princ "\nInvalid object selected.")
                                )
                            )
                        )
                    )
                )
                (while (/= 5 (car (setq pt1 (grread t 13 1)))))
                (null sel)
            )
        )
        (   (not
                (or
                    (and
                        (setq trm (caddr sel))
                        (setq ent (oa:copynested (car sel) trm))
                    )
                    (and
                        (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                        (setq ent (cdr (assoc 330 (entget (car sel)))))
                    )
                    (setq ent (car sel))
                )
            )
            (princ "\nUnable to recreate nested entity.")
        )
        (   (progn
                (setq ocs (trans '(0 0 1) 1 0 t)
                      uxa (angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 ocs t))
                      mat (mxm
                              (list
                                  (list (cos uxa)     (sin uxa) 0.0)
                                  (list (- (sin uxa)) (cos uxa) 0.0)
                                 '(0.0 0.0 1.0)
                              )
                              (mapcar '(lambda ( a ) (trans a ocs 0 t))
                                 '(
                                      (1.0 0.0 0.0)
                                      (0.0 1.0 0.0)
                                      (0.0 0.0 1.0)
                                  )
                              )
                          )
                      vec (mapcar '- (mxv mat (trans '(0.0 0.0 0.0) ocs 0)))
                      tma (vlax-tmatrix (append (mapcar 'append mat (mapcar 'list vec)) '((0.0 0.0 0.0 1.0))))
                )
                (repeat (setq idx (sslength oss))
                    (setq idx (1- idx)
                          obj (vla-copy (vlax-ename->vla-object (ssname oss idx)))
                          lst (cons obj lst)
                    )
                    (vla-transformby obj tma)
                    (if (and (vlax-method-applicable-p obj 'getboundingbox)
                             (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
                        )
                        (setq bb1 (cons (vlax-safearray->list llp) bb1)
                              bb2 (cons (vlax-safearray->list urp) bb2)
                        )
                    )
                    (vla-put-visible obj :vlax-false)
                )
                (not (and bb1 bb2))
            )
            (*error* nil)
            (princ "\nUnable to calculate bounding box for the selection.")
        )
        (   t
            (setq bb1 (apply 'mapcar (cons 'min bb1))
                  bb2 (apply 'mapcar (cons 'max bb2))
                  bpt (cond ( bpt (mapcar '+ (mxv mat (trans bpt 1 0)) vec)) ((mapcar '(lambda ( a b ) (/ (+ a b) 2.0)) bb1 bb2)))
                  fac (/ (- (cadr bb2) (cadr bb1)) 2.0)
                  pi2 (/ pi -2.0)
                  inc 0
            )
            (if (equal 0.0 fac 1e-8)
                (if (equal bb1 bb2 1e-8)
                    (setq fac 1.0)
                    (setq fac (/ (- (car bb2) (car bb1)) 2.0))
                )
            )
            (while (tblsearch "block" (setq bnm (strcat "$tmp" (itoa (setq inc (1+ inc)))))))
            (foreach obj lst (vla-put-visible obj :vlax-true))
            (vla-copyobjects (oa:acdoc)
                (vlax-make-variant
                    (vlax-safearray-fill
                        (vlax-make-safearray vlax-vbobject (cons 0 (1- (length lst))))
                        lst
                    )
                )
                (setq def (vla-add (vla-get-blocks (oa:acdoc)) (vlax-3D-point bpt) bnm))
            )
            (foreach obj lst (vla-delete obj))
            (setq lst nil
                  blk
                (vla-insertblock
                    (vlax-get-property (oa:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                    (vlax-3D-point (trans (cadr pt1) 1 0))
                    bnm 1.0 1.0 1.0 0.0
                )
            )
            (vla-put-layer  blk "0")
            (vla-put-normal blk (vlax-3D-point ocs))
            (setq msg (princ "\n[+/-] for [O]ffset | [</>] for [R]otation | [M]ultiple | <[E]xit>: "))

            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (member gr1 '(3 5))
                            (setq pt2 (trans gr2 1 0)
                                  pt1 (vlax-curve-getclosestpointtoprojection ent pt2 ocs)
                                  pt3 (oa:2d (trans pt1 0 ocs))
                                  pt4 (oa:2d (trans pt2 0 ocs))
                            )
                            (if (not (equal pt3 pt4 1e-8))
                                (progn
                                    (setq dis (/ (* fac oa|off) (distance pt3 pt4)))
                                    (vla-put-insertionpoint blk
                                        (vlax-3D-point
                                            (trans
                                                (append
                                                    (mapcar '(lambda ( a b ) (+ a (* (- b a) dis))) pt3 pt4)
                                                    (list (caddr (trans pt1 0 ocs)))
                                                )
                                                ocs 0
                                            )
                                        )
                                    )
                                    (vla-put-rotation blk (+ (angle (trans pt1 0 ocs) (trans gr2 1 ocs)) oa|rot pi2))
                                )
                            )
                            (cond
                                (   (= 5 gr1))
                                (   (progn (vla-explode blk) oa|mtp))
                            )
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (member gr2 '(043 061))
                                    (setq oa|off (+ oa|off 0.1))
                                )
                                (   (member gr2 '(045 095))
                                    (setq oa|off (- oa|off 0.1))
                                )
                                (   (member gr2 '(044 060))
                                    (setq oa|rot (+ oa|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(046 062))
                                    (setq oa|rot (- oa|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(013 032 069 101))
                                    nil
                                )
                                (   (member gr2 '(082 114))
                                    (if (setq tmp (getangle (strcat "\nSpecify Rotation <" (angtos oa|rot) ">: ")))
                                        (setq oa|rot tmp)
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(079 111))
                                    (if (setq tmp (getdist (strcat "\nSpecify Offset <" (rtos (* fac oa|off)) ">: ")))
                                        (setq oa|off (/ tmp fac))
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(077 109))
                                    (if (setq oa|mtp (not oa|mtp))
                                        (princ "\n<Multiple mode on>")
                                        (princ "\n<Multiple mode off>")
                                    )
                                    (princ msg)
                                )
                                (   t   )
                            )
                        )
                        (   (member gr1 '(011 025))
                            nil
                        )
                        (   t   )
                    )
                )
            )
            (if trm (entdel ent))
            (vla-delete  blk)
            (vla-delete  def)
            (oa:endundo (oa:acdoc))
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun oa:2d ( x ) (list (car x) (cadr x)))

;;----------------------------------------------------------------------;;

(defun oa:layerlocked ( lay / def )
    (and
        (setq def (tblsearch "layer" lay))
        (= 4 (logand 4 (cdr (assoc 70 def))))
    )
)

;;----------------------------------------------------------------------;;

(defun oa:copynested ( ent mat / enx tmp )
    (if (= 1 (cdr (assoc 66 (setq enx (entget ent)))))
        (progn
            (oa:entmakex enx)
            (setq ent (entnext ent)
                  enx (entget  ent)
            )
            (while (/= "SEQEND" (cdr (assoc 0 enx)))
                (oa:entmakex enx)
                (setq ent (entnext ent)
                      enx (entget  ent)
                )
            )
            (setq tmp (cdr (assoc 330 (entget (oa:entmakex enx)))))
        )
        (setq tmp (oa:entmakex enx))
    )
    (if tmp (vla-transformby (vlax-ename->vla-object tmp) (vlax-tmatrix mat)))
    tmp
)

;;----------------------------------------------------------------------;;

(defun oa:entmakex ( enx )
    (entmakex
        (append
            (vl-remove-if
                (function
                    (lambda ( x )
                        (or (member (car x) '(005 006 008 039 048 062 102 370))
                            (= 'ename (type (cdr x)))
                        )
                    )
                )
                enx
            )
           '(
                (006 . "CONTINUOUS")
                (008 . "0")
                (039 . 0.0)
                (048 . 1.0)
                (062 . 7)
                (370 . 0)
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun oa:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;;----------------------------------------------------------------------;;

(defun oa:startundo ( doc )
    (oa:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun oa:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun oa:acdoc nil
    (eval (list 'defun 'oa:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (oa:acdoc)
)

;;----------------------------------------------------------------------;;

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)
    
;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ObjectAlign.lsp | Version 1.7 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2010")
        " www.lee-mac.com ::"
        "\n:: Type \"oa\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;-----------------------=={ Incremental Array }==----------------------;;
;;                                                                      ;;
;;  This program will array a selection of objects, whilst incrementing ;;
;;  any numerical content found in annotation objects within the        ;;
;;  selection.                                                          ;;
;;                                                                      ;;
;;  The program has two modes of operation: standard & dynamic. The     ;;
;;  standard command: 'incarray' will not display the dynamic preview,  ;;
;;  but in turn will run faster and smoother than the dynamic version   ;;
;;  - this difference is especially significant when attempting to      ;;
;;  array a large number of objects.                                    ;;
;;                                                                      ;;
;;  The dynamic mode: 'incarrayd' will display a preview of the         ;;
;;  arrayed objects as the mouse is dragged across the screen. However, ;;
;;  due to the method used to generate this preview, this mode is only  ;;
;;  suitable when using the program to array a small number of objects. ;;
;;                                                                      ;;
;;  Upon starting the program, the user is prompted to specify an       ;;
;;  increment value and then prompted to make a selection of objects    ;;
;;  to array. This selection may include any drawing object with the    ;;
;;  exception of viewports.                                             ;;
;;                                                                      ;;
;;  Following a valid selection, the user should specify a base point   ;;
;;  and array vector relative to the base point. The angle and length   ;;
;;  of this vector will determine the direction and density of the      ;;
;;  array respectively; a shorter vector will result in a denser array. ;;
;;                                                                      ;;
;;  The array may now be generated by dragging the mouse across the     ;;
;;  screen until the array reaches a desired size. If the object        ;;
;;  selection includes Text, MText, Attribute Definitions, Dimensions,  ;;
;;  or Multileader objects, any numerical data found in the text        ;;
;;  content of these objects will be automatically incremented by the   ;;
;;  given increment value as the object is arrayed.                     ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-07-27                                      ;;
;;                                                                      ;;
;;  - First Release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2011-07-29                                      ;;
;;                                                                      ;;
;;  - Fixed UCS bug by adding displacement flag to trans expressions.   ;;
;;    With thanks to Swamp user HighflyingBird for finding this.        ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2011-07-29                                      ;;
;;                                                                      ;;
;;  - Added non-dynamic version of the program.                         ;;
;;  - Added ability to increment Attribute Def Tags, Prompts & Text.    ;;
;;  - Improved increment functionality to retain all leading and        ;;
;;    trailing zeros.                                                   ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2011-08-05                                      ;;
;;                                                                      ;;
;;  - Added ability to array all objects, not just annotation objects.  ;;
;;    Any annotation objects in the selection which contain numerical   ;;
;;    data will still be incremented.                                   ;;
;;  - Attributes within arrayed attributed blocks are now incremented.  ;;
;;  - MLeader Text & Dimension Override Text are incremented.           ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2011-09-30                                      ;;
;;                                                                      ;;
;;  - Fixed bug when arraying attributes on locked layers.              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2014-04-13                                      ;;
;;                                                                      ;;
;;  - Program completely rewritten.                                     ;;
;;  - Added prompt for increment value.                                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2014-04-13                                      ;;
;;                                                                      ;;
;;  - Fixed variable name clash and grvecs bug present in AutoCAD 2006  ;;
;;    as reported by Swamp user CAB - many thanks.                      ;;
;;----------------------------------------------------------------------;;
;;  Version 1.7    -    2014-06-07                                      ;;
;;                                                                      ;;
;;  - Fixed accumulated rounding errors appearing at 44 onwards when    ;;
;;    incrementing a value of 1 by an increment of 1.                   ;;
;;----------------------------------------------------------------------;;
;;  Version 1.8    -    2016-10-26                                      ;;
;;                                                                      ;;
;;  - Fixed a bug causing the program to crash if parentheses or double ;;
;;    quotes are present in the text content.                           ;;
;;----------------------------------------------------------------------;;

(defun c:incarray  nil (LM:incarray nil)) ;; Standard version
(defun c:incarrayd nil (LM:incarray  t )) ;; Dynamic  version

;;----------------------------------------------------------------------;;

(defun LM:incarray ( dyn / *error* bpt dim dis ept inc lst obl qty tmp vxu vxw )

    (defun *error* ( msg )
        (if (= 'int (type dim))
            (setvar 'dimzin dim)
        )
        (foreach obj obl
            (if (and (= 'vla-object (type obj)) (not (vlax-erased-p obj)) (vlax-write-enabled-p obj))
                (vla-delete obj)
            )
        )
        (incarray:endundo (incarray:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )

    (if (not (and (setq inc (getenv "LMac\\incarray")) (setq inc (distof inc))))
        (setq inc 1)
    )
    (if (setq tmp (getreal (strcat "\nSpecify increment <" (incarray:num->str inc) ">: ")))
        (setenv "LMac\\incarray" (incarray:num->str (setq inc tmp)))
    )
    (incarray:startundo (incarray:acdoc))
    (setq dim (getvar 'dimzin))
    (setvar 'dimzin 0)
    (cond
        (   (not
                (and
                    (setq lst (incarray:selection->list (ssget "_:L" '((0 . "~VIEWPORT")))))
                    (setq bpt (getpoint "\nSpecify base point: "))
                    (progn
                        (while
                            (and
                                (setq vxu (getpoint "\nSpecify array vector: " bpt))
                                (equal bpt vxu 1e-8)
                            )
                            (princ "\nInvalid array vector.")
                        )
                        vxu
                    )
                    (setq vxu (mapcar '- vxu bpt)
                          vxw (trans vxu 1 0 t)
                          dis (distance '(0.0 0.0 0.0) vxw)
                    )
                )
            )
        )
        (   dyn
            (princ "\nSpecify array end point: ")
            (while (= 5 (car (setq ept (grread t 13 0))))
                (redraw)
                (foreach obj obl (vla-delete obj))
                (setq qty (/ (caddr (trans (mapcar '- (cadr ept) bpt) 1 vxw t)) dis)
                      obl (incarray:copyvector lst (mapcar (if (minusp qty) '- '+) vxw) (abs (fix qty)) inc)
                )
                (grvecs (list -3 bpt (mapcar '(lambda ( a b ) (+ (* a qty) b)) vxu bpt)))
            )
        )
        (   (setq ept (getpoint bpt "\nSpecify array end point: "))
            (setq qty (fix (/ (caddr (trans (mapcar '- ept bpt) 1 vxw t)) dis)))
            (incarray:copyvector lst (mapcar (if (minusp qty) '- '+) vxw) (abs (fix qty)) inc)
        )
    )
    (setvar 'dimzin dim)
    (incarray:endundo (incarray:acdoc))
    (redraw) (princ)
)

;;----------------------------------------------------------------------;;

(defun incarray:num->str ( x / dim rtn )
    (if (equal x (atof (rtos x 2 0)) 1e-8)
        (rtos x 2 0)
        (progn
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq rtn (vl-catch-all-apply 'rtos (list x 2 15)))
            (setvar 'dimzin dim)
            (if (not (vl-catch-all-error-p rtn)) rtn)
        )
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:copyvector ( lst vec qty inc / cnt obj obl org )
    (setq org (vlax-3D-point 0 0)
          cnt 1
    )
    (repeat qty
        (foreach itm lst
            (setq obj (vla-copy (car itm))
                  obl (cons obj obl)
            )
            (vla-move obj org (vlax-3D-point (mapcar '* vec (list cnt cnt cnt))))
            (if (= "AcDbBlockReference" (vla-get-objectname obj))
                (mapcar
                    (function
                        (lambda ( att prp )
                            (vl-catch-all-apply 'vlax-put-property
                                (list att (car prp)
                                    (apply 'strcat
                                        (mapcar '(lambda ( x ) (incarray:increment x (* cnt inc)))
                                            (cdr prp)
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (vlax-invoke obj 'getattributes)
                    (cdr itm)
                )
                (foreach prp (cdr itm)
                    (vlax-put-property obj (car prp)
                        (apply 'strcat
                            (mapcar '(lambda ( x ) (incarray:increment x (* cnt inc)))
                                (cdr prp)
                            )
                        )
                    )
                )
            )
        )
        (setq cnt (1+ cnt))
    )
    obl
)

;;----------------------------------------------------------------------;;

(defun incarray:selection->list ( sel / idx lst obj obn )
    (if sel
        (repeat (setq idx (sslength sel))
            (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx))))
                  obn (vla-get-objectname obj)
            )
            (if (and (= "AcDbBlockReference" obn) (= :vlax-true (vla-get-hasattributes obj)))
                (setq lst
                    (cons
                        (cons obj
                            (mapcar '(lambda ( a ) (vl-list* 'textstring (incarray:splitstring (vla-get-textstring a))))
                                (vlax-invoke obj 'getattributes)
                            )
                        )
                        lst
                    )
                )
                (setq lst
                    (cons
                        (cons obj
                            (mapcar '(lambda ( p ) (vl-list* p (incarray:splitstring (vlax-get-property obj p))))
                                (cond
                                    (   (wcmatch obn "AcDb*Text,AcDbMLeader") '(textstring))
                                    (   (wcmatch obn "AcDb*Dimension")        '(textoverride))
                                    (   (= "AcDbAttributeDefinition" obn)     '(tagstring promptstring textstring))
                                )
                            )
                        )
                        lst
                    )
                )
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:splitstring ( str / lst )
    (setq lst (vl-string->list str))
    (read (vl-list->string (vl-list* 40 34 (incarray:split lst (< 47 (car lst) 58)))))
)

;;----------------------------------------------------------------------;;

(defun incarray:split ( lst flg )
    (cond
        (   (null lst) '(34 41))
        (   (member (car lst) '(34 92))
            (if flg
                (vl-list* 34 32 34 92 (car lst) (incarray:split (cdr lst) nil))
                (vl-list* 92 (car lst) (incarray:split (cdr lst) flg))
            )
        )
        (   (or (< 47 (car lst) 58) (and (= 46 (car lst)) flg (< 47 (cadr lst) 58)))
            (if flg
                (vl-list* (car lst) (incarray:split (cdr lst) flg))
                (vl-list* 34 32 34 (car lst) (incarray:split (cdr lst) t))
            )
        )
        (   flg (vl-list* 34 32 34 (car lst) (incarray:split (cdr lst) nil)))
        (   (vl-list* (car lst) (incarray:split (cdr lst) nil)))
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:increment ( str inc / dci dcs len num )
    (if (distof str 2)
        (progn
            (setq num (+ (distof str) inc)
                  inc (incarray:num->str inc)
                  str (vl-string-left-trim "-" str)
                  inc (vl-string-left-trim "-" inc)
                  dci (incarray:decimalplaces inc)
                  dcs (incarray:decimalplaces str)
                  len (strlen str)
                  str (vl-string-left-trim "-" (rtos num 2 (max dci dcs)))
            )
            (cond
                (   (< 0 dcs) (setq len (+ (- len dcs) (max dci dcs))))
                (   (< 0 dci) (setq len (+ dci len 1)))
            )
            (repeat (- len (strlen str))
                (setq str (strcat "0" str))
            )
            (if (minusp num)
                (strcat "-" str)
                str
            )
        )
        str
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:decimalplaces ( str / pos )
    (if (setq pos (vl-string-position 46 str))
        (- (strlen str) pos 1)
        0
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:startundo ( doc )
    (incarray:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun incarray:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun incarray:acdoc nil
    (eval (list 'defun 'incarray:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (incarray:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: IncArray.lsp | Version 1.8 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"incarray\" - Standard | \"incarrayd\" - Dynamic ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;---------------------------=={ Arrow Arc }==--------------------------;;
;;                                                                      ;;
;;  This program enables the user to construct an arc with arrowheads   ;;
;;  at each end point or at both end points, with the arrowheads        ;;
;;  aligned with the arc.                                               ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'aarc' (Arrow Arc) at the AutoCAD   ;;
;;  command-line, the user may construct an arc with the same options   ;;
;;  available as the standard in-built AutoCAD ARC command.             ;;
;;                                                                      ;;
;;  If the constructed arc is long enough to accommodate one or two     ;;
;;  arrowheads, the program will proceed to generate a 2D Polyline      ;;
;;  (LWPolyline) arc segment with additional segments of varying width  ;;
;;  at the start and/or end point forming the arrowheads.               ;;
;;                                                                      ;;
;;  The dimensions of the resulting arrowheads and the option to        ;;
;;  determine whether the arrowheads are created at the start of the    ;;
;;  arc, end of the arc, or at both the start & end, may be altered     ;;
;;  using the 'aarcsettings' command; these parameters will be          ;;
;;  remembered between drawing sessions.                                ;;
;;                                                                      ;;
;;  This program will also perform successfully under all UCS & View    ;;
;;  configurations.                                                     ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2016  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2012-07-17                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2013-05-26                                      ;;
;;                                                                      ;;
;;  - Added 'aarcsettings' command to enable the user to alter the      ;;
;;    arrow length & width without modifying the code.                  ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2016-02-27                                      ;;
;;                                                                      ;;
;;  - Program modified to allow the user to specify whether to create   ;;
;;    an arrowhead at the start or end of the arc, or at both.          ;;
;;----------------------------------------------------------------------;;

(defun c:aarc ( / *error* an1 an2 an3 ang arl arw cen ent enx flg rad typ )
    
    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (and (setq arl (getenv "LMac\\aarcl")) (setq arl (distof arl 2))))
        (setq arl 1.0)
    )
    (if (not (and (setq arw (getenv "LMac\\aarcw")) (setq arw (distof arw 2))))
        (setq arw 0.5)
    )
    (if (not (and (setq typ (getenv "LMac\\aarct")) (member typ '("Start" "End" "Both"))))
        (setq typ "Both")
    )
    (princ
        (strcat
            "\nArrow Length: " (rtos arl 2) " | Width: " (rtos arw 2) " | Arrowheads: " typ
            "\nType \"aarcsettings\" to alter settings.\n"
        )
    )
    (setq ent (entlast))
    (command "_.arc")
    (while (= 1 (logand 1 (getvar 'cmdactive)))
        (command "\\")
    )
    (if (not (eq ent (setq ent (entlast))))
        (progn
            (setq enx (entget ent)
                  cen (cdr (assoc 10 enx))
                  rad (cdr (assoc 40 enx))
                  an1 (cdr (assoc 50 enx))
                  an2 (cdr (assoc 51 enx))
                  an3 (/ arl rad)
                  ang (rem (+ pi pi (- an2 an1)) (+ pi pi))
                  flg (equal (trans (getvar 'lastpoint) 1 ent) (polar cen an1 rad) 1e-3)
            )
            (if (< arl (* rad ang (if (= typ "Both") 0.5 1.0)))
                (if
                    (entmake
                        (append
                            (list
                               '(000 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                                (cons 90 (if (= "Both" typ) 4 3))
                               '(070 . 0)
                                (cons 010 (polar cen an1 rad))
                               '(040 . 0.0)
                            )
                            (cond
                                (   (or (and flg (= "Start" typ)) (and (not flg) (= "End" typ)))
                                    (list
                                       '(041 . 0.0)
                                        (cons 042 (tan (/ (- ang an3) 4.0)))
                                        (cons 010 (polar cen (- an2 an3) rad))
                                        (cons 040 arw)
                                       '(041 . 0.0)
                                        (cons 042 (tan (/ an3 4.0)))
                                    )
                                )
                                (   (= "Both" typ)
                                    (list
                                        (cons 041 arw)
                                        (cons 042 (tan (/ an3 4.0)))
                                        (cons 010 (polar cen (+ an1 an3) rad))
                                       '(040 . 0.0)
                                       '(041 . 0.0)                         
                                        (cons 042 (tan (/ (- ang an3 an3) 4.0)))
                                        (cons 010 (polar cen (- an2 an3) rad))
                                        (cons 040 arw)
                                       '(041 . 0.0)
                                        (cons 042 (tan (/ an3 4.0)))
                                    )
                                )
                                (   (list
                                        (cons 041 arw)
                                        (cons 042 (tan (/ an3 4.0)))
                                        (cons 010 (polar cen (+ an1 an3) rad))
                                       '(040 . 0.0)
                                       '(041 . 0.0)
                                        (cons 042 (tan (/ (- ang an3) 4.0)))
                                    )
                                )
                            )
                            (list
                                (cons 010 (polar cen an2 rad))
                                (cons 210 (trans '(0.0 0.0 1.0) 1 0 t))
                            )
                        )
                    )
                    (entdel ent)
                )
                (princ "\nArc too short to accommodate arrow(s).")
            )
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun c:aarcsettings ( / tmp )
    (initget 6)
    (if (setq tmp (getdist (strcat "\nSpecify Arrow Length <" (cond ((getenv "LMac\\aarcl")) ("1.0")) ">: ")))
        (setenv "LMac\\aarcl" (rtos tmp 2))
    )
    (initget 6)
    (if (setq tmp (getdist (strcat "\nSpecify Arrow Width <" (cond ((getenv "LMac\\aarcw")) ("0.5")) ">: ")))
        (setenv "LMac\\aarcw" (rtos tmp 2))
    )
    (initget "Start End Both")
    (if (setq tmp (getkword (strcat "\nArrows at [Start/End/Both] <" (cond ((getenv "LMac\\aarct")) ("Both")) ">: ")))
        (setenv "LMac\\aarct" tmp)
    )
    (princ)
)    
  
;;----------------------------------------------------------------------;;
  
(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-10))
        (/ (sin x) (cos x))
    )
)

;;----------------------------------------------------------------------;;

(princ
    (strcat
        "\n:: ArrowArc.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"AARC\" for Arrow Arc | \"AARCSETTINGS\" for Settings ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;----------------------=={ Length at Midpoint }==----------------------;;
;;                                                                      ;;
;;  This program prompts the user for a selection of objects to be      ;;
;;  labelled and proceeds to generate an MText object located at        ;;
;;  the midpoint of each object displaying a Field Expression           ;;
;;  referencing the length of the object.                               ;;
;;                                                                      ;;
;;  The program is compatible for use with Arcs, Circles, Lines,        ;;
;;  LWPolylines, 2D & 3D Polylines, and under all UCS & View settings.  ;;
;;                                                                      ;;
;;  The program will generate MText objects positioned directly over    ;;
;;  the midpoint of each object, and aligned with the object whilst     ;;
;;  preserving text readability. The MText will have a background mask  ;;
;;  enabled and will use the active Text Style and Text Height settings ;;
;;  at the time of running the program.                                 ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2013-11-12                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2016-01-16                                      ;;
;;                                                                      ;;
;;  - Modified LM:objectid function to account for 64-bit AutoCAD 2008. ;;
;;----------------------------------------------------------------------;;

(defun c:midlen ( / *error* ent fmt idx ins ocs par sel spc txt typ uxa )

    (setq fmt "%lu6") ;; Field Formatting

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if
        (setq sel
            (ssget
                (list
                   '(0 . "ARC,CIRCLE,LINE,*POLYLINE")
                   '(-4 . "<NOT")
                       '(-4 . "<AND")
                           '(0 . "POLYLINE")
                           '(-4 . "&")
                           '(70 . 80)
                       '(-4 . "AND>")
                   '(-4 . "NOT>")
                    (if (= 1 (getvar 'cvport))
                        (cons 410 (getvar 'ctab))
                       '(410 . "Model")
                    )
                )
            )
        )
        (progn
            (setq spc
                (vlax-get-property (LM:acdoc)
                    (if (= 1 (getvar 'cvport))
                        'paperspace
                        'modelspace
                    )
                )
            )
            (setq ocs (trans '(0.0 0.0 1.0) 1 0 t)
                  uxa (angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 ocs t))
            )
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (setq ent (ssname sel (setq idx (1- idx)))
                      par (vlax-curve-getparamatdist ent (/ (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent)) 2.0))
                      ins (vlax-curve-getpointatparam ent par)
                      typ (cdr (assoc 0 (entget ent)))
                )
                (setq txt
                    (vlax-invoke spc 'addmtext ins 0.0
                        (strcat
                            "%<\\AcObjProp Object(%<\\_ObjId " (LM:objectid (vlax-ename->vla-object ent)) ">%)."
                            (cond
                                (   (= "CIRCLE" typ) "Circumference")
                                (   (= "ARC"    typ) "ArcLength")
                                (   "Length"   )
                            )
                            " \\f \"" fmt "\">%"
                        )
                    )
                )
                (vla-put-backgroundfill  txt :vlax-true)
                (vla-put-attachmentpoint txt acattachmentpointmiddlecenter)
                (vla-put-insertionpoint  txt (vlax-3D-point ins))
                (vla-put-rotation txt (LM:readable (- (angle '(0.0 0.0 0.0) (trans (vlax-curve-getfirstderiv ent par) 0 ocs t)) uxa)))
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Readable  -  Lee Mac
;; Returns an angle corrected for text readability.

(defun LM:readable ( a )
    (   (lambda ( a )
            (if (and (< (* pi 0.5) a) (<= a (* pi 1.5)))
                (LM:readable (+ a pi))
                a
            )
        )
        (rem (+ a pi pi) (+ pi pi))
    )
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:objectid ( obj )
    (eval
        (list 'defun 'LM:objectid '( obj )
            (if (wcmatch (getenv "PROCESSOR_ARCHITECTURE") "*64*")
                (if (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                    (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
                   '(LM:ename->objectid (vlax-vla-object->ename obj))
                )
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:objectid obj)
)

;; Entity Name to ObjectID  -  Lee Mac
;; Returns the 32-bit or 64-bit ObjectID for a supplied entity name

(defun LM:ename->objectid ( ent )
    (LM:hex->decstr
        (setq ent (vl-string-right-trim ">" (vl-prin1-to-string ent))
              ent (substr ent (+ (vl-string-position 58 ent) 3))
        )
    )
)

;; Hex to Decimal String  -  Lee Mac
;; Returns the decimal representation of a supplied hexadecimal string

(defun LM:hex->decstr ( hex / foo bar )
    (defun foo ( lst rtn )
        (if lst
            (foo (cdr lst) (bar (- (car lst) (if (< 57 (car lst)) 55 48)) rtn))
            (apply 'strcat (mapcar 'itoa (reverse rtn)))
        )
    )
    (defun bar ( int lst )
        (if lst
            (if (or (< 0 (setq int (+ (* 16 (car lst)) int))) (cdr lst))
                (cons (rem int 10) (bar (/ int 10) (cdr lst)))
            )
            (bar int '(0))
        )
    )
    (foo (vl-string->list (strcase hex)) nil)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

(vl-load-com)
(princ
    (strcat
        "\n:: MidLen.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"midlen\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;-------------------=={ Circular Wipeout }==-----------------;;
;;                                                            ;;
;;  Enables the user to create a circular wipeout with a      ;;
;;  given center and radius. Works in all UCS & Views.        ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:cwipe ( / cen rad )
    (cond
        (   (not
                (or (member "acwipeout.arx" (arx)) (arxload "acwipeout.arx" nil)
                    (member "acismui.arx"   (arx)) (arxload "acismui.arx"   nil) ;; 2013
                )
            )
            (princ "\nUnable to load wipeout arx files.")
        )
        (   (and
                (setq cen (getpoint "\nSpecify Center: "))
                (setq rad (getdist  "\nSpecify Radius: " cen))
            )
            (LM:CircularWipeout cen rad)
        )
    )
    (princ)
)

;;-------------------=={ Circle to Wipeout }==----------------;;
;;                                                            ;;
;;  Enables the user to convert a selection of circles to     ;;
;;  wipeout objects matching the original circle properties.  ;;
;;  Works with circles constructed in any UCS.                ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:c2wipe ( / ent enx inc sel wip )
    (cond
        (   (not
                (or (member "acwipeout.arx" (arx)) (arxload "acwipeout.arx" nil)
                    (member "acismui.arx"   (arx)) (arxload "acismui.arx"   nil) ;; 2013
                )
            )
            (princ "\nUnable to load wipeout arx files.")
        )
        (   (setq sel (ssget "_:L" '((0 . "CIRCLE"))))
            (repeat (setq inc (sslength sel))
                (setq ent (ssname sel (setq inc (1- inc)))
                      enx (entget ent)
                      wip (LM:CircularWipeout (trans (cdr (assoc 10 enx)) ent 1) (cdr (assoc 40 enx)))
                )
                (if wip
                    (progn
                        (entmod (cons (cons -1 wip) (LM:defaultprops (entget wip))))
                        (entdel ent)
                    )
                )
            )
        )
    )
    (princ)
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups

(defun LM:defaultprops ( elist )
    (mapcar
        (function
            (lambda ( pair )
                (cond ((assoc (car pair) elist)) ( pair ))
            )
        )
       '(
            (008 . "0")
            (006 . "BYLAYER")
            (039 . 0.0)
            (062 . 256)
            (048 . 1.0)
            (370 . -1)
        )
    )
)

;; Circular Wipeout  -  Lee Mac
;; Creates a circular wipeout with the given center (UCS) & radius

(defun LM:CircularWipeout ( cen rad / ang inc lst )
    (setq acc 50
          inc (/ pi acc 0.5)
          ang 0.0
    )
    (repeat acc
        (setq lst (cons (list 14 (* 0.5 (cos ang)) (* 0.5 (sin ang))) lst)
              ang (+ ang inc)
        )
    )
    (entmakex
        (append
            (list
               '(000 . "WIPEOUT")
               '(100 . "AcDbEntity")
               '(100 . "AcDbWipeout")
                (cons 10 (trans (mapcar '- cen (list rad rad)) 1 0))
                (cons 11 (trans (list (+ rad rad) 0.0) 1 0 t))
                (cons 12 (trans (list 0.0 (+ rad rad)) 1 0 t))
               '(280 . 1)
               '(071 . 2)
            )
            (cons (last lst) lst)
        )
    )
)
(princ)

;;-----------------------=={ Viewport Outline }==-----------------------;;
;;                                                                      ;;
;;  This program allows the user to automatically generate a polyline   ;;
;;  in modelspace representing the outline of a selected paperspace     ;;
;;  viewport.                                                           ;;
;;                                                                      ;;
;;  The command is only available in paperspace (that is, when a        ;;
;;  layout tab other than the Model tab is the current layout, and no   ;;
;;  viewports are active).                                              ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'VPO' at the AutoCAD command-line,  ;;
;;  the user is prompted to select a viewport for which to construct    ;;
;;  the viewport outline in modelspace.                                 ;;
;;                                                                      ;;
;;  Following a valid selection, the boundary of the selected viewport  ;;
;;  is transformed appropriately to account for the position, scale,    ;;
;;  rotation, & orientation of the modelspace view displayed through    ;;
;;  the selected viewport, and a 2D polyline (LWPolyline) representing  ;;
;;  this transformed boundary is constructed in modelspace.             ;;
;;                                                                      ;;
;;  The program is compatible for use with all Rectangular, Polygonal & ;;
;;  Clipped Viewports (including those with Arc segments), and with all ;;
;;  views & construction planes.                                        ;;
;;                                                                      ;;
;;  The program also offers the ability to optionally offset the        ;;
;;  polyline outline to the interior of the viewport boundary by a      ;;
;;  predetermined number of paperspace units specified in the           ;;
;;  'Program Parameters' section of the program source code.            ;;
;;                                                                      ;;
;;  The program may also be configured to automatically apply a         ;;
;;  predefined set of properties (e.g. layer, colour, linetype, etc.)   ;;
;;  to the resulting polyline outline - these properties are also       ;;
;;  listed within the 'Program Parameters' section of the source code.  ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2015  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2015-01-02                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2016-08-11                                      ;;
;;                                                                      ;;
;;  - Program modified to account for polygonal viewports represented   ;;
;;    by 2D (Heavy) Polylines.                                          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2017-09-03                                      ;;
;;                                                                      ;;
;;  - Added the ability to specify an optional interior offset          ;;
;;    (relative to Paperspace Viewport dimensions).                     ;;
;;  - Added default polyline properties.                                ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2019-08-12                                      ;;
;;                                                                      ;;
;;  - Restructured program as a main function accepting a viewport      ;;
;;    entity argument.                                                  ;;
;;  - Added two additional custom commands:                             ;;
;;    - 'vpol' - outlines all viewports in the active Paperspace layout ;;
;;    - 'vpoa' - outlines all viewports in all Paperspace layouts       ;;
;;----------------------------------------------------------------------;;

;;----------------------------------------------------------------------;;
;;  VPO - Outline a selected viewport in the active Paperspace layout   ;;
;;----------------------------------------------------------------------;;

(defun c:vpo ( / *error* sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (cond
        (   (/= 1 (getvar 'cvport))
            (princ "\nCommand not available in Modelspace.")
        )
        (   (setq sel (LM:ssget "\nSelect viewport: " '("_+.:E:S" ((0 . "VIEWPORT")))))
            (vpo:main (ssname sel 0))
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;;----------------------------------------------------------------------;;
;;  VPOL - Outline all viewports in the active Paperspace layout        ;;
;;----------------------------------------------------------------------;;

(defun c:vpol ( / *error* idx sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (cond
        (   (/= 1 (getvar 'cvport))
            (princ "\nCommand not available in Modelspace.")
        )
        (   (setq sel (ssget "_X" (list '(0 . "VIEWPORT") '(-4 . "<>") '(69 . 1) (cons 410 (getvar 'ctab)))))
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (vpo:main (ssname sel (setq idx (1- idx))))
            )
            (LM:endundo (LM:acdoc))
        )
        (   (princ "\nNo viewports were found in the active layout."))
    )
    (princ)
)

;;----------------------------------------------------------------------;;
;;  VPOA - Outline all viewports in all Paperspace layouts              ;;
;;----------------------------------------------------------------------;;

(defun c:vpoa ( / *error* idx sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (cond
        (   (setq sel (ssget "_X" '((0 . "VIEWPORT") (-4 . "<>") (69 . 1) (410 . "~Model"))))
            (LM:startundo (LM:acdoc))
            (repeat (setq idx (sslength sel))
                (vpo:main (ssname sel (setq idx (1- idx))))
            )
            (LM:endundo (LM:acdoc))
        )
        (   (princ "\nNo viewports were found in any Paperspace layouts."))
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun vpo:main ( vpt / cen dpr ent lst ltp ocs ofe off tmp vpe )

    (setq

;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

        ;; Optional Interior Offset
        ;; Set this parameter to nil or 0.0 for no offset
        off 0.0

        ;; Default Polyline Properties
        ;; Omitted properties will use current settings when the program is run
        dpr
       '(
            (006 . "BYLAYER")   ;; Linetype (must be loaded)
           ;(008 . "VPOutline") ;; Layer (automatically created if not present in drawing)
            (039 . 0.0)         ;; Thickness
            (048 . 1.0)         ;; Linetype Scale
            (062 . 256)         ;; Colour (0 = ByBlock, 256 = ByLayer)
            (370 . -1)          ;; Lineweight (-1 = ByLayer, -2 = ByBlock, -3 = Default, 0.3 = 30 etc.)
        )
        
;;----------------------------------------------------------------------;;

    )
    
    (if (setq vpt (entget vpt)
              ent (cdr (assoc 340 vpt))
        )
        (setq lst (vpo:polyvertices ent))
        (setq cen (mapcar 'list (cdr (assoc 10 vpt))
                      (list
                          (/ (cdr (assoc 40 vpt)) 2.0)
                          (/ (cdr (assoc 41 vpt)) 2.0)
                      )
                  )
              lst (mapcar '(lambda ( a ) (cons (mapcar 'apply a cen) '(42 . 0.0))) '((- -) (+ -) (+ +) (- +)))
        )
    )
    (if (not (LM:listclockwise-p (mapcar 'car lst)))
        (setq lst (reverse (mapcar '(lambda ( a b ) (cons (car a) (cons 42 (- (cddr b))))) lst (cons (last lst) lst))))
    )
    (if (and (numberp off) (not (equal 0.0 off 1e-8)))
        (cond
            (   (null
                    (setq tmp
                        (entmakex
                            (append
                                (list
                                   '(000 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                    (cons 90 (length lst))
                                   '(070 . 1)
                                )
                                (apply 'append (mapcar '(lambda ( x ) (list (cons 10 (car x)) (cdr x))) lst))
                            )
                        )
                    )
                )
                (princ "\nUnable to generate Paperspace outline for offset.")
            )
            (   (vl-catch-all-error-p (setq ofe (vl-catch-all-apply 'vlax-invoke (list (vlax-ename->vla-object tmp) 'offset off))))
                (princ (strcat "\nViewport dimensions too small to offset outline by " (rtos off) " units."))
                (entdel tmp)
            )
            (   (setq ofe (vlax-vla-object->ename (car ofe))
                      lst (vpo:polyvertices ofe)
                )
                (entdel ofe)
                (entdel tmp)
            )
    	)
    )
    (setq vpe (cdr (assoc -1 vpt))
          ocs (cdr (assoc 16 vpt))
    )
    (entmakex
        (append
            (list
               '(000 . "LWPOLYLINE")
               '(100 . "AcDbEntity")
               '(100 . "AcDbPolyline")
                (cons 90 (length lst))
               '(070 . 1)
               '(410 . "Model")
            )
            (if (and (setq ltp (assoc 6 dpr)) (not (tblsearch "ltype" (cdr ltp))))
                (progn
                    (princ  (strcat "\n\"" (cdr ltp) "\" linetype not loaded - linetype set to \"ByLayer\"."))
                    (subst '(6 . "BYLAYER") ltp dpr)
                )
                dpr
            )
            (apply 'append (mapcar '(lambda ( x ) (list (cons 10 (trans (pcs2wcs (car x) vpe) 0 ocs)) (cdr x))) lst))
            (list (cons 210 ocs))
        )
    )
)

;;----------------------------------------------------------------------;;

(defun vpo:polyvertices ( ent )
    (apply '(lambda ( foo bar ) (foo bar))
        (if (= "LWPOLYLINE" (cdr (assoc 0 (entget ent))))
            (list
                (lambda ( enx )
                    (if (setq enx (member (assoc 10 enx) enx))
                        (cons (cons  (cdr (assoc 10 enx)) (assoc 42 enx)) (foo (cdr enx)))
                    )
                )
                (entget ent)
            )
            (list
                (lambda ( ent / enx )
                    (if (= "VERTEX" (cdr (assoc 0 (setq enx (entget ent)))))
                        (cons (cons (cdr (assoc 10 enx)) (assoc 42 enx)) (foo (entnext ent)))
                    )
            	)
                (entnext ent)
            )
        )
    )
)

;;----------------------------------------------------------------------;;

;; List Clockwise-p  -  Lee Mac
;; Returns T if the point list is clockwise oriented

(defun LM:listclockwise-p ( lst )
    (minusp
        (apply '+
            (mapcar
                (function
                    (lambda ( a b )
                        (- (* (car b) (cadr a)) (* (car a) (cadr b)))
                    )
                )
                lst (cons (last lst) lst)
            )
        )
    )
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; PCS2WCS (gile)
;; Translates a PCS point to WCS based on the supplied Viewport
;; (PCS2WCS pt vp) is the same as (trans (trans pt 3 2) 2 0) when vp is active
;; pnt : PCS point
;; ent : Viewport ename

(defun PCS2WCS ( pnt ent / ang enx mat nor scl )
    (setq pnt (trans pnt 0 0)
          enx (entget ent)
          ang (- (cdr (assoc 51 enx)))
          nor (cdr (assoc 16 enx))
          scl (/ (cdr (assoc 45 enx)) (cdr (assoc 41 enx)))
          mat (mxm
                  (mapcar (function (lambda ( v ) (trans v 0 nor t)))
                     '(   (1.0 0.0 0.0)
                          (0.0 1.0 0.0)
                          (0.0 0.0 1.0)
                      )
                  )
                  (list
                      (list (cos ang) (- (sin ang)) 0.0)
                      (list (sin ang)    (cos ang)  0.0)
                     '(0.0 0.0 1.0)
                  )
              )
    )
    (mapcar '+
        (mxv mat
            (mapcar '+
                (vxs pnt scl)
                (vxs (cdr (assoc 10 enx)) (- scl))
                (cdr (assoc 12 enx))
            )
        )
        (cdr (assoc 17 enx))
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Vector x Scalar  -  Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun vxs ( v s )
    (mapcar '(lambda ( n ) (* n s)) v)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(princ
    (strcat
        "\n:: VPOutline.lsp | Version 1.3 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2015")
        " www.lee-mac.com ::"
        "\n:: \"vpo\"  - Outline single viewport                ::"
        "\n:: \"vpol\" - Outline all viewports in active layout ::"
        "\n:: \"vpoa\" - Outline all viewports in all layouts   ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;-----------------------=={ Show Hatch Text }==------------------------;;
;;                                                                      ;;
;;  This program enables the user to clear the area of a hatch pattern  ;;
;;  surrounding selected Text or MText objects, or Text, MText or       ;;
;;  Attributes contained within selected Block References.              ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'sht' (Show Hatch Text) at the      ;;
;;  command-line, the user is first prompted to make a selection of     ;;
;;  Text, MText and/or Blocks for which to clear the surrounding hatch  ;;
;;  pattern, and then to select the obscuring hatch to be modified.     ;;
;;                                                                      ;;
;;  Following valid selections, the program will proceed to generate    ;;
;;  new hatch boundaries surrounding every selected Text and MText      ;;
;;  object, and furthermore for every Text, MText or Attribute object   ;;
;;  found within each selected block reference, including within any    ;;
;;  nested block references (nested to any depth) found within the      ;;
;;  selected block references.                                          ;;
;;                                                                      ;;
;;  In order to generate the appropriate hatch boundary for nested      ;;
;;  Text, MText or Attributes, the program will recreate the nested     ;;
;;  object as a temporary primary object, before adding the new hatch   ;;
;;  boundary and deleting the temporary object. As a consequence of     ;;
;;  this method, the hatch must become disassociative when nested       ;;
;;  objects are processed by the program.                               ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    14-11-2013                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:sht ( / cmd en1 en2 ent enx hat idx sel )

    (defun *error* ( msg )
        (foreach ent en2
            (if (entget ent) (entdel ent))
        )
        (if (= 'int (type cmd)) (setvar 'cmdecho cmd))
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (LM:startundo (LM:acdoc))
    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" "0")))))
            (princ "\nLayer \"0\" is locked.")
        )
        (   (and
                (setq sel (LM:ssget "\nSelect text and blocks: "   '(((0 . "INSERT,TEXT,MTEXT")))))
                (setq hat (LM:ssget "\nSelect hatch: " '("_+.:E:S:L" ((0 . "HATCH")))))
            )
            (repeat (setq idx (sslength sel))
                (setq ent (ssname sel (setq idx (1- idx)))
                      enx (entget ent)
                )
                (if (wcmatch (cdr (assoc 0 enx)) "*TEXT")
                    (setq en1 (cons ent en1))
                    (progn
                        (setq en2
                            (append en2
                                (fixhatch:processblock
                                    (apply 'fixhatch:tmatrix (refgeom ent))
                                    (cdr (assoc 2 enx))
                                )
                            )
                        )
                        (if (= 1 (cdr (assoc 66 enx)))
                            (setq en2 (append en2 (fixhatch:processattributes ent)))
                        )
                    )
                )
            )
            (if (or en1 en2)
                (progn
                    (setq cmd (getvar 'cmdecho))
                    (setvar 'cmdecho 0)
                    (if en2 (command "_.-hatchedit" (ssname hat 0) "_DI"))
                    (command "_.-hatchedit" (ssname hat 0) "_AD" "_S")
                    (apply 'command (append en1 en2))
                    (command "" "")
                    (setvar 'cmdecho cmd)
                    (foreach ent en2 (entdel ent))
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

(defun fixhatch:processblock ( mat blk / ent enx lst tmp )
    (if (setq ent (tblobjname "block" blk))
        (while (setq ent (entnext ent))
            (setq enx (entget ent))
            (cond
                (   (= 1 (cdr (assoc 60 enx))))
                (   (wcmatch (cdr (assoc 0 enx)) "TEXT,MTEXT")
                    (if (setq tmp (fixhatch:entmakex enx))
                        (setq lst (cons tmp lst))
                    )
                )
                (   (= "INSERT" (cdr (assoc 0 enx)))
                    (if (= 1 (cdr (assoc 66 enx)))
                        (setq lst (append lst (fixhatch:processattributes ent)))
                    )
                    (setq lst
                        (append lst
                            (fixhatch:processblock
                                (apply 'fixhatch:tmatrix (refgeom ent))
                                (cdr (assoc 2 enx))
                            )
                        )
                    )
                )
            )
        )
    )
    (foreach ent lst
        (vla-transformby (vlax-ename->vla-object ent) mat)
    )
    lst
)

(defun fixhatch:processattributes ( ent / att atx lst tmp )
    (setq att (entnext ent)
          atx (entget  att)
    )
    (while (= "ATTRIB" (cdr (assoc 0 atx)))
        (if
            (and (zerop (logand 1 (cdr (assoc 70 atx))))
                (setq tmp
                    (fixhatch:entmakex
                        (if (member '(101 . "Embedded Object") atx)
                            (append '((0 . "MTEXT") (100 . "AcDbEntity") (100 . "AcDbMText"))
                                (fixhatch:remove1stpairs  '(001 007 010 011 040 041 050 071 072 073 210)
                                    (fixhatch:removepairs '(000 002 042 043 051 070 074 100 101 102 280 330 360) atx)
                                )
                            )
                            (append '((0 . "TEXT"))
                                (fixhatch:removepairs '(000 002 070 074 100 280)
                                    (subst (cons 73 (cdr (assoc 74 atx))) (assoc 74 atx) atx)
                                )
                            )
                        )
                    )
                )
             )
             (setq lst (cons tmp lst))
        )
        (setq att (entnext att)
              atx (entget  att)
        )
    )
    lst
)

(defun fixhatch:tmatrix ( mat vec )
    (vlax-tmatrix
        (append
            (mapcar '(lambda ( a b ) (append a (list b))) mat vec)
           '((0.0 0.0 0.0 1.0))
        )
    )
)

(defun fixhatch:entmakex ( enx )
    (entmakex
        (append
            (vl-remove-if
               '(lambda ( x )
                    (or (member (car x) '(005 006 008 039 048 062 102 370))
                        (= 'ename (type (cdr x)))
                    )
                )
                enx
            )
           '(
                (006 . "CONTINUOUS")
                (008 . "0")
                (039 . 0.0)
                (048 . 1.0)
                (062 . 7)
                (370 . 0)
            )
        )
    )
)

(defun fixhatch:removepairs ( itm lst )
    (vl-remove-if '(lambda ( x ) (member (car x) itm)) lst)
)
 
(defun fixhatch:remove1stpairs ( itm lst )
    (vl-remove-if '(lambda ( x ) (if (member (car x) itm) (progn (setq itm (vl-remove (car x) itm)) t))) lst)
)

;; RefGeom (gile)
;; Returns a list whose first item is a 3x3 transformation matrix and
;; second item the object insertion point in its parent (xref, block or space)
 
(defun refgeom ( ent / ang enx mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (mapcar '(lambda ( v ) (trans v 0 ocs t))
                   '(
                        (1.0 0.0 0.0)
                        (0.0 1.0 0.0)
                        (0.0 0.0 1.0)
                    )
                )
                (mxm
                    (list
                        (list (cos ang) (- (sin ang)) 0.0)
                        (list (sin ang) (cos ang)     0.0)
                       '(0.0 0.0 1.0)
                    )
                    (list
                        (list (cdr (assoc 41 enx)) 0.0 0.0)
                        (list 0.0 (cdr (assoc 42 enx)) 0.0)
                        (list 0.0 0.0 (cdr (assoc 43 enx)))
                    )
                )
            )
        )
        (mapcar '- (trans (cdr (assoc 10 enx)) ocs 0)
            (mxv mat (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx))))))
        )
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - selection prompt
;; arg - list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ShowHatchText.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"sht\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;---------------------=={ Area Label }==---------------------;;
;;                                                            ;;
;;  Allows the user to label picked areas or objects and      ;;
;;  either display the area in an ACAD Table (if available),  ;;
;;  optionally using fields to link area numbers and objects; ;;
;;  or write it to file.                                      ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.9    -    29-10-2011                            ;;
;;------------------------------------------------------------;;

(defun c:AT nil (AreaLabel   t))  ;; Areas to Table
(defun c:AF nil (AreaLabel nil))  ;; Areas to File

;;------------------------------------------------------------;;

(defun AreaLabel ( flag / *error* _startundo _endundo _centroid _text _open _select _getobjectid _isannotative
                          acdoc acspc ap ar as cf cm el fd fl fo n of om p1 pf pt sf st t1 t2 tb th ts tx ucsxang ucszdir )

  ;;------------------------------------------------------------;;
  ;;                         Adjustments                        ;;
  ;;------------------------------------------------------------;;

  (setq h1 "Area Table"  ;; Heading
        t1 "Number"      ;; Number Title
        t2 "Area"        ;; Area Title
        pf ""            ;; Number Prefix (optional, "" if none)
        sf ""            ;; Number Suffix (optional, "" if none)
        ap ""            ;; Area Prefix (optional, "" if none)
        as ""            ;; Area Suffix (optional, "" if none)
        cf 1.0           ;; Area Conversion Factor (e.g. 1e-6 = mm2->m2)
        fd t             ;; Use fields to link numbers/objects to table (t=yes, nil=no)
        fo "%lu6%qf1"    ;; Area field formatting
  )

  ;;------------------------------------------------------------;;

  (defun *error* ( msg )
    (if cm (setvar 'CMDECHO cm))
    (if el (progn (entdel el) (setq el nil)))
    (if acdoc (_EndUndo acdoc))
    (if (and of (eq 'FILE (type of))) (close of))
    (if (and Shell (not (vlax-object-released-p Shell))) (vlax-release-object Shell))
    (if (null (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
        (princ (strcat "\n--> Error: " msg))
    )
    (princ)
  )

  ;;------------------------------------------------------------;;

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  ;;------------------------------------------------------------;;

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  ;;------------------------------------------------------------;;

  (defun _centroid ( space objs / reg cen )
    (setq reg (car (vlax-invoke space 'addregion objs))
          cen (vlax-get reg 'centroid)
    )
    (vla-delete reg) (trans cen 1 0)
  )

  ;;------------------------------------------------------------;;

  (defun _text ( space point string height rotation / text )
    (setq text (vla-addtext space string (vlax-3D-point point) height))
    (vla-put-alignment text acalignmentmiddlecenter)
    (vla-put-textalignmentpoint text (vlax-3D-point point))
    (vla-put-rotation text rotation)
    text
  )

  ;;------------------------------------------------------------;;

  (defun _Open ( target / Shell result )
    (if (setq Shell (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application"))
      (progn
        (setq result
          (and (or (eq 'INT (type target)) (setq target (findfile target)))
            (not
              (vl-catch-all-error-p
                (vl-catch-all-apply 'vlax-invoke (list Shell 'Open target))
              )
            )
          )
        )
        (vlax-release-object Shell)
      )
    )
    result
  )

  ;;------------------------------------------------------------;;

  (defun _Select ( msg pred func init / e ) (setq pred (eval pred)) 
    (while
      (progn (setvar 'ERRNO 0) (apply 'initget init) (setq e (func msg))
        (cond
          ( (= 7 (getvar 'ERRNO))
            (princ "\nMissed, try again.")
          )
          ( (eq 'STR (type e))
            nil
          )            
          ( (vl-consp e)
            (if (and pred (not (pred (setq e (car e)))))
              (princ "\nInvalid Object Selected.")
            )
          )
        )
      )
    )
    e
  )

  ;;------------------------------------------------------------;;

  (defun _GetObjectID ( doc obj )
    (if (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
      (vlax-invoke-method (vla-get-Utility doc) 'GetObjectIdString obj :vlax-false)
      (itoa (vla-get-Objectid obj))
    )
  )

  ;;------------------------------------------------------------;;

  (defun _isAnnotative ( style / object annotx )
    (and
      (setq object (tblobjname "STYLE" style))
      (setq annotx (cadr (assoc -3 (entget object '("AcadAnnotative")))))
      (= 1 (cdr (assoc 1070 (reverse annotx))))
    )
  )
  
  ;;------------------------------------------------------------;;

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
        acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'Paperspace 'Modelspace))

        ucszdir (trans '(0. 0. 1.) 1 0 t)
        ucsxang (angle '(0. 0. 0.) (trans (getvar 'UCSXDIR) 0 ucszdir))
  )
  (_StartUndo acdoc)
  (setq cm (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)
  (setq om (eq "1" (cond ((getenv "LMAC_AreaLabel")) ((setenv "LMAC_AreaLabel" "0")))))

  (setq ts
    (/ (getvar 'TEXTSIZE)
      (if (_isAnnotative (getvar 'TEXTSTYLE))
        (cond ( (getvar 'CANNOSCALEVALUE) ) ( 1.0 )) 1.0
      )
    )
  )

  (cond
    ( (not (vlax-method-applicable-p acspc 'addtable))

      (princ "\n--> Table Objects not Available in this Version.")
    )
    ( (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'CLAYER))))))

      (princ "\n--> Current Layer Locked.")
    )
    ( (not
        (setq *al:num
          (cond
            (
              (getint
                (strcat "\nSpecify Starting Number <"
                  (itoa (setq *al:num (1+ (cond ( *al:num ) ( 0 ))))) ">: "
                )
              )
            )
            ( *al:num )
          )
        )
      )
    )
    ( flag

      (setq th
        (* 2.
          (if
            (zerop
              (setq th
                (vla-gettextheight
                  (setq st
                    (vla-item
                      (vla-item
                        (vla-get-dictionaries acdoc) "ACAD_TABLESTYLE"
                      )
                      (getvar 'CTABLESTYLE)
                    )
                  )
                  acdatarow
                )
              )
            )
            ts
            (/ th
              (if (_isAnnotative (vla-gettextstyle st acdatarow))
                (cond ( (getvar 'CANNOSCALEVALUE) ) ( 1.0 )) 1.0
              )
            )
          )
        )
      )

      (if
        (cond
          (
            (progn (initget "Add")
              (vl-consp (setq pt (getpoint "\nPick Point for Table <Add to Existing>: ")))
            )
            (setq tb
              (vla-addtable acspc
                (vlax-3D-point (trans pt 1 0)) 2 2 th (* 0.8 th (max (strlen t1) (strlen t2)))
              )
            )
            (vla-put-direction tb (vlax-3D-point (getvar 'UCSXDIR)))
            (vla-settext tb 0 0 h1)
            (vla-settext tb 1 0 t1)
            (vla-settext tb 1 1 t2)
            
            (while
              (progn
                (if om
                  (setq p1
                    (_Select (strcat "\nSelect Object [Pick] <Exit>: ")
                     '(lambda ( x )
                        (and
                          (vlax-property-available-p (vlax-ename->vla-object x) 'area)
                          (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
                          (or (eq "REGION" (cdr (assoc 0 (entget x)))) (vlax-curve-isclosed x))
                        )
                      )
                      entsel '("Pick")
                    )
                  )
                  (progn (initget "Object") (setq p1 (getpoint "\nPick Area [Object] <Exit>: ")))
                )
                (cond
                  ( (null p1)

                    (vla-delete tb)
                  )
                  ( (eq "Pick" p1)

                    (setq om nil) t
                  )
                  ( (eq "Object" p1)

                    (setq om t)
                  )
                  ( (eq 'ENAME (type p1))

                    (setq tx
                      (cons
                        (_text acspc
                          (_centroid acspc (list (setq p1 (vlax-ename->vla-object p1))))
                          (strcat pf (itoa *al:num) sf)
                          ts
                          ucsxang
                        )
                        tx
                      )
                    )
                    (vla-insertrows tb (setq n 2) th 1)
                    (vla-settext tb n 1
                      (if fd
                        (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                          (_GetObjectID acdoc p1) ">%).Area \\f \"" fo "\">%"
                        )
                        (strcat ap (rtos (* cf (vla-get-area p1)) 2) as)
                      )
                    )
                    (vla-settext tb n 0
                      (if fd
                        (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                          (_GetObjectID acdoc (car tx)) ">%).TextString>%"
                        )
                        (strcat pf (itoa *al:num) sf)
                      )
                    )
                    nil
                  )                      
                  ( (vl-consp p1)

                    (setq el (entlast))
                    (vl-cmdf "_.-boundary" "_A" "_I" "_N" "" "_O" "_P" "" "_non" p1 "")

                    (if (not (equal el (setq el (entlast))))
                      (progn
                        (setq tx
                          (cons
                            (_text acspc
                              (_centroid acspc (list (vlax-ename->vla-object el)))
                              (strcat pf (itoa *al:num) sf)
                              ts
                              ucsxang
                            )
                            tx
                          )
                        )
                        (vla-insertrows tb (setq n 2) th 1)
                        (vla-settext tb n 1 (strcat ap (rtos (* cf (vlax-curve-getarea el)) 2) as))
                        (vla-settext tb n 0
                          (if fd
                            (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                              (_GetObjectID acdoc (car tx)) ">%).TextString>%"
                            )
                            (strcat pf (itoa *al:num) sf)
                          )
                        )
                        (redraw el 3)
                        nil
                      )
                      (vla-delete tb)
                    )
                  )
                )
              )
            )
            (not (vlax-erased-p tb))
          )
          (
            (and
              (setq tb
                (_Select "\nSelect Table to Add to: "
                 '(lambda ( x ) (eq "ACAD_TABLE" (cdr (assoc 0 (entget x))))) entsel nil
                )
              )
              (< 1 (vla-get-columns (setq tb (vlax-ename->vla-object tb))))
            )
            (setq n (1- (vla-get-rows tb)) *al:num (1- *al:num))
          )
        )
        (progn
          (while
            (if om
              (setq p1
                (_Select (strcat "\nSelect Object [" (if tx "Undo/" "") "Pick] <Exit>: ")
                 '(lambda ( x )
                    (and
                      (vlax-property-available-p (vlax-ename->vla-object x) 'area)
                      (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
                      (or (eq "REGION" (cdr (assoc 0 (entget x)))) (vlax-curve-isclosed x))
                    )
                  )
                  entsel (list (if tx "Undo Pick" "Pick"))
                )
              )
              (progn (initget (if tx "Undo Object" "Object"))
                (setq p1 (getpoint (strcat "\nPick Area [" (if tx "Undo/" "") "Object] <Exit>: ")))
              )
            )
            (cond
              ( (and tx (eq "Undo" p1))

                (if el (progn (entdel el) (setq el nil)))
                (vla-deleterows tb n 1)
                (vla-delete (car tx))
                (setq n (1- n) tx (cdr tx) *al:num (1- *al:num))
              )
              ( (eq "Undo" p1)

                (princ "\n--> Nothing to Undo.")
              )
              ( (eq "Object" p1)

                (if el (progn (entdel el) (setq el nil)))
                (setq om t)
              )
              ( (eq "Pick" p1)

                (setq om nil)
              )
              ( (and om (eq 'ENAME (type p1)))

                (setq tx
                  (cons
                    (_text acspc
                      (_centroid acspc (list (setq p1 (vlax-ename->vla-object p1))))
                      (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
                      ts
                      ucsxang
                    )
                    tx
                  )
                )
                (vla-insertrows tb (setq n (1+ n)) th 1)
                (vla-settext tb n 1
                  (if fd
                    (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                      (_GetObjectID acdoc p1) ">%).Area \\f \"" fo "\">%"
                    )
                    (strcat ap (rtos (* cf (vla-get-area p1)) 2) as)
                  )
                )
                (vla-settext tb n 0
                  (if fd
                    (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                      (_GetObjectID acdoc (car tx)) ">%).TextString>%"
                    )
                    (strcat pf (itoa *al:num) sf)
                  )
                )
              )               
              ( (vl-consp p1)      

                (if el (progn (entdel el) (setq el nil)))
                (setq el (entlast))
                (vl-cmdf "_.-boundary" "_A" "_I" "_N" "" "_O" "_P" "" "_non" p1 "")

                (if (not (equal el (setq el (entlast))))
                  (progn
                    (setq tx
                      (cons
                        (_text acspc
                          (_centroid acspc (list (vlax-ename->vla-object el)))
                          (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
                          ts
                          ucsxang
                        )
                        tx
                      )
                    )
                    (vla-insertrows tb (setq n (1+ n)) th 1)
                    (vla-settext tb n 1 (strcat ap (rtos (* cf (vlax-curve-getarea el)) 2) as))
                    (vla-settext tb n 0
                      (if fd
                        (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                          (_GetObjectID acdoc (car tx)) ">%).TextString>%"
                        )
                        (strcat pf (itoa *al:num) sf)
                      )
                    )
                    (redraw el 3)
                  )
                  (princ "\n--> Error Retrieving Area.")
                )
              )
            )
          )
          (if el (progn (entdel el) (setq el nil)))
        )
      )
    )
    (
      (and
        (setq fl (getfiled "Create Output File" (cond ( *file* ) ( "" )) "txt;csv;xls" 1))
        (setq of (open fl "w"))
      )
      (setq *file*  (vl-filename-directory fl)
            de      (cdr (assoc (strcase (vl-filename-extension fl) t) '((".txt" . "\t") (".csv" . ",") (".xls" . "\t"))))
            *al:num (1- *al:num)
      )
      (write-line h1 of)
      (write-line (strcat t1 de t2) of)

      (while
        (if om
          (setq p1
            (_Select (strcat "\nSelect Object [Pick] <Exit>: ")
             '(lambda ( x )
                (and
                  (vlax-property-available-p (vlax-ename->vla-object x) 'area)
                  (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
                  (or (eq "REGION" (cdr (assoc 0 (entget x)))) (vlax-curve-isclosed x))
                )
              )
              entsel '("Pick")
            )
          )
          (progn (initget "Object") (setq p1 (getpoint (strcat "\nPick Area [Object] <Exit>: "))))
        )
        (cond
          ( (eq "Object" p1)

            (if el (progn (entdel el) (setq el nil)))
            (setq om t)
          )
          ( (eq "Pick" p1)

            (setq om nil)
          )
          ( (eq 'ENAME (type p1))

            (_text acspc
              (_centroid acspc (list (setq p1 (vlax-ename->vla-object p1))))
              (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
              ts
              ucsxang
            )           
            (write-line (strcat pf (itoa *al:num) sf de ap (rtos (* cf (vla-get-area p1)) 2) as) of)
          )
          ( (vl-consp p1)
        
            (if el (progn (entdel el) (setq el nil)))
            (setq el (entlast))
            (vl-cmdf "_.-boundary" "_A" "_I" "_N" "" "_O" "_P" "" "_non" p1 "")

            (if (not (equal el (setq el (entlast))))
              (progn
                (_text acspc
                  (_centroid acspc (list (vlax-ename->vla-object el)))
                  (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
                  ts
                  ucsxang
                )
                (write-line (strcat pf (itoa *al:num) sf de ap (rtos (* cf (vlax-curve-getarea el)) 2) as) of)
                (redraw el 3)
              )
              (princ "\n--> Error Retrieving Area.")
            )
          )
        )
      )
      (if el (progn (entdel el) (setq el nil)))
      (setq of (close of))
      (_Open (findfile fl))
    )      
  )
  (setenv "LMAC_AreaLabel" (if om "1" "0"))
  (setvar 'CMDECHO cm)
  (_EndUndo acdoc)
  (princ)
)

;;------------------------------------------------------------;;

(vl-load-com)
(princ)
(princ "\n:: AreaLabel.lsp | Version 1.9 | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Commands: \"AT\" for ACAD Table, \"AF\" for File ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;


;;---------------------=={ Quick Mirror }==-------------------;;
;;                                                            ;;
;;  Provides functionality to mirror a selection of objects   ;;
;;  or a single object without the need to select two points  ;;
;;  defining a mirror axis.                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  single - if T, mirrors selected object using derivative   ;;
;;           at the selected edge to determine mirror axis,   ;;
;;           else prompts for selection set and object to use ;;
;;           as mirror axis.                                  ;;
;;  delete - if T, selected object or selection set is        ;;
;;           deleted following the mirror operation.          ;;
;;------------------------------------------------------------;;
;;  Returns:  Null.                                           ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    15-02-2011                            ;;
;;                                                            ;;
;;  First Release.                                            ;;
;;------------------------------------------------------------;;

;;------------------------------------------------------------;;
;;                      Program Shortcuts                     ;;
;;------------------------------------------------------------;;

;; Mirror SelectionSet about selected object
(defun c:QM   nil (QuickMirror nil nil))

;; Mirror SelectionSet about selected object, delete SelectionSet
(defun c:QMD  nil (QuickMirror nil   t))

;; Mirror Single Object about Selection Point
(defun c:QMO  nil (QuickMirror   t nil))

;; Mirror Single Object about Selection Point, delete Original Object
(defun c:QMOD nil (QuickMirror   t   t))

;;------------------------------------------------------------;;
;;                   Quick Mirror Subfunction                 ;;
;;------------------------------------------------------------;;

(defun QuickMirror ( single delete / *error* _StartUndo _EndUndo doc ss sel p1 p2 i o ) (vl-load-com)
  ;; © Lee Mac 2011

  (defun *error* ( msg )
    (if doc (_EndUndo doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (if (or single (setq ss (ssget "_:L")))
    (while
      (progn (setvar 'ERRNO 0) (setq sel (entsel "\nSelect Mirror Object: "))
        (cond
          (
            (=  7 (getvar 'ERRNO)) (princ "\n** Missed, Try Again **")
          )
          (
            (and sel
              (not
                (vl-catch-all-error-p
                  (setq p1
                    (vl-catch-all-apply 'vlax-curve-getClosestPointto
                      (list (car sel) (trans (cadr sel) 1 0))
                    )
                  )
                )
              )
            )

            (setq p2
              (polar p1
                (angle '(0. 0. 0.)
                  (vlax-curve-getFirstDeriv (car sel)
                    (vlax-curve-getParamatPoint (car sel) p1)
                  )
                )
                1.
              )
            )

            (setq p1 (vlax-3D-point p1) p2 (vlax-3D-point p2))

            (_StartUndo doc)
            (if ss
              (repeat (setq i (sslength ss))
                (vla-mirror (setq o (vlax-ename->vla-object (ssname ss (setq i (1- i))))) p1 p2)
                (if delete  (vla-delete o))
              )
              (progn
                (vla-mirror (setq o (vlax-ename->vla-object (car sel))) p1 p2)
                (if delete  (vla-delete o))
              )
            )
            (_EndUndo doc)
          )
        )
      )
    )
  )
  (princ)
)

;;------------------------------------------------------------;;
;;                          End of File                       ;;
;;------------------------------------------------------------;;


;;------------------------=={  Dynamic Text Align  }==---------------------------;;
;;                                                                               ;;
;;  Allows the user to dynamically align text to any angle. User is prompted to  ;;
;;  make a selection of Text or MText objects to align, and pick an alignment    ;;
;;  point, or select a text object to use for alignment. The selection of text   ;;
;;  is then aligned by either x or y coordinate, or dynamically stretched        ;;
;;  depending on the mode chosen.                                                ;;
;;                                                                               ;;
;;  The mode can be switched upon pressing TAB during alignment. Text and MText  ;;
;;  entities will be aligned in accordance with their respective justifications. ;;
;;                                                                               ;;
;;  The user can also specify a fixed text spacing, by pressing 'S' during text  ;;
;;  alignment. Holding Shift whilst aligning Text will alter Text Rotation, the  ;;
;;  user can also refine Rotation by pressing 'R' during text alignment. Text    ;;
;;  Justfication can be altered by pressing 'J' during text alignment.           ;;
;;                                                                               ;;
;;                                                                               ;;
;;  Object Alignment Mode:-                                                      ;;
;;  --------------------------                                                   ;;
;;  Text can be aligned to an object by pressing 'O' during text alignment. In   ;;
;;  this mode, the text spacing along the object can be adjusted by pressing     ;;
;;  'S' and the text offset from the object can also be altered by pressing 'O'. ;;
;;                                                                               ;;
;;  Text Rotation can be aligned to the tangent vector of the object at the      ;;
;;  point of alignment by holding Shift during text placement. The user can      ;;
;;  furthermore specify a text rotation by pressing 'R'.                         ;;
;;                                                                               ;;
;;  The order of the text entities along the object can be Reversed by pressing  ;;
;;  'V' during Text placement. The original order of these entities is           ;;
;;  determined by the drawing direction of the object.                           ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  TXALIGN                                                    ;;
;;                                                                               ;;
;;  Notes:-                                                                      ;;
;;  ---------                                                                    ;;
;;  Shift Functionality requires the user to have Express Tools installed.       ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright © October 2009 - www.lee-mac.com                  ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  12/10/2009  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  14/10/2009  -  Added ability to Specify fixed text spacing             ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  15/10/2009  -  Added Stretch Mode                                      ;;
;;                    -  Upgraded User messaging                                 ;;
;;-------------------------------------------------------------------------------;;
;;  1.3:  18/10/2009  -  Added Rotation Functionality                            ;;
;;-------------------------------------------------------------------------------;;
;;  1.4:  20/10/2009  -  Added functionality to align text to object.            ;;
;;-------------------------------------------------------------------------------;;
;;  1.5:  23/10/2009  -  Added Justification Options.                            ;;
;;-------------------------------------------------------------------------------;;
;;  1.6:  28/10/2009  -  Added Option to Select Text object at Alignment Point   ;;
;;                       prompt                                                  ;;
;;-------------------------------------------------------------------------------;;

(defun c:TxAlign (/ ;; --=={ Local Functions }==--

                      *error* GetProp Text_Rotation Text_Offset Text_Stretch

                    ;; --=={ Local Variables }==--

                      ANG BAR BDIS BPT BSANG BSDIS BSPT CANG CLST CMODE CODE
                      CODEC COL CPT DATA DATAC DER DIS DOC DSPC ENT ET FOO
                      FOOC GR GRC I IPT J JLST K MLST MSG MSGC OBJLST PLST
                      PROP PT PTO RANG RLST SS TMPLST

                    ;; --=={ Global Variables }==--

                    ; *txMode   ~  Mode Setting
                    ; *txSpc    ~  Default Text Spacing
                    ; *txRot    ~  Default Text Rotation
                    ; *txOff    ~  Default Text Curve Offset
                    ; *txJus    ~  Default Text Justification

                  )
  
  (vl-load-com)

  (defun *error* (err)
    (and doc (vla-EndUndoMark doc))
    (and rLst (mapcar
                (function
                  (lambda (values)
                    (vlax-put (car values) (cadr values) (cadddr values))
                    (vlax-put (car values) 'Rotation     (caddr  values)))) rLst))
    (or (wcmatch (strcase err) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\nError: " err)))
    (redraw)
    (princ))

  (defun GetProp (object_list)
    (mapcar
      (function
        (lambda (object / prop)
          (setq prop (if (eq "AcDbText" (vla-get-ObjectName object))
                       (if (eq acAlignmentLeft (vla-get-Alignment object))
                         'InsertionPoint 'TextAlignmentPoint)
                       'InsertionPoint))
          (list object prop (vlax-get object 'Rotation) (vlax-get object prop)))) object_list))

  (defun Text_Rotation (/ oStr msgR grR codeR dataR rPt rAng)
    (setq oStr "")

    (princ (setq msgR (strcat "\nSpecify Text Rotation [Reset] <" (vl-princ-to-string *txRot) "> : ")))

    (while
      (progn
        (setq grR (grread 't 15 0) codeR (car grR) dataR (cadr grR))
        (redraw)
        
        (cond (  (and (= codeR 5) (listp dataR))
                 (setq rPt (last (car (GetProp (list (car ObjLst))))))
               
                 (if (not (zerop (getvar "ORTHOMODE")))
                   (if (< (abs (- (car dataR) (car rPt))) (abs (- (cadr dataR) (cadr rPt))))
                     (setq dataR (list (car rPt) (cadr dataR) (caddr dataR)))
                     (setq dataR (list (car dataR) (cadr rPt) (caddr dataR)))))
               
                 (setq rAng (angle rPt dataR))
                 (mapcar
                   (function
                     (lambda (object) (vla-put-rotation object rAng))) ObjLst)
               
                 (grdraw rPt dataR 40 1) t)
              
              (  (and (= codeR 2) (< 46 dataR 123))
                 (princ (chr dataR))
                 (setq oStr (strcat oStr (chr dataR))))
              
              (  (and (= codeR 2) (= dataR 8) (< 0 (strlen oStr)))
                 (princ (vl-list->string '(8 32 8)))
                 (setq oStr (substr oStr 1 (1- (strlen oStr)))))
              
              (  (and (= codeR 2) (= 15 dataR))
                 (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))
              
              (  (or (and (= codeR 2) (vl-position dataR '(32 13)))
                     (= code 25))
               
                 (cond (  (< 0 (strlen oStr))
                      
                        (cond (  (vl-position oStr '("r" "R" "reset" "Reset" "RESET"))
                                 (setq rAng nil))
                            
                              (  (setq rAng (angtof oStr 0))
                                 (setq *txRot (* 180. (/ rAng pi))) nil)
                            
                              (  (princ "\nInvalid Angle Entered.")
                                 (setq oStr "")
                                 (princ msgR))))
                     
                       (t (setq rAng (* pi (/ *txRot 180.))) nil)))
              
              (  (and (= codeR 3) (listp dataR))
                 (setq *txRot (* 180. (/ rAng pi))) nil)
              
              (t (princ "\nInvalid Input.") (princ msgR)))))
    
      (if rAng
        (mapcar (function (lambda (object) (vla-put-rotation object rAng))) ObjLst)
        (mapcar (function (lambda (values) (vla-put-rotation (car values) (caddr values)))) rLst)))

  (defun Text_Offset (/ oStr BaseDis inc grLst tmpPt msgR grR codeR dataR cPt ang ptO der tmpOff k)
    (setq oStr "")

    (princ (setq msgR (strcat "\nSpecify Text Offset [Exit] <" (vl-princ-to-string *txOff) "> : ")))

    (setq BaseDis (vlax-curve-getDistatPoint ent
                    (vlax-curve-getClosestPointto ent
                      (vlax-get (caar pLst) (cadar pLst)))))

    (setq inc (/ (- (vlax-curve-getDistatPoint ent
                      (vlax-curve-getClosestPointto ent
                        (vlax-get (car (last pLst)) (cadr (last pLst))))) BaseDis) 50.))
    (while
      (progn
        (setq grR (grread 't 15 0) codeR (car grR) dataR (cadr grR))
        (redraw)
        
        (cond (  (and (= codeR 5) (listp dataR))                 

                 (setq cPt  (vlax-curve-getClosestPointto ent dataR) k -1 ang  (angle cPt dataR))
                 (grdraw cPt dataR 40 1)

                 (setq aFac (- (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                 (vlax-curve-getParamatPoint ent cPt))) ang))
                 (setq grLst nil i -1)
                 (repeat 50
                   (setq grLst (cons (polar (setq tmpPt (vlax-curve-getPointatDist ent (+ BaseDis (* (setq i (1+ i)) inc))))
                                            (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                              (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                             (vlax-curve-getParamatPoint ent tmpPt)))) aFac))
                                            (distance cPt dataR)) grLst)))
                 (grvecs (append '(-91) grLst))  

                 (foreach Obj pLst
                   (setq ptO (vlax-curve-getClosestPointto ent (vlax-get (car Obj) (cadr Obj))))
                   (vlax-put (car Obj) (cadr Obj)
                             (polar ptO (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                          (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                           (vlax-curve-getParamatPoint ent ptO)))) aFac))
                                    (setq tmpOff (distance cPt dataR)))))
               t)
                               
              (  (and (= codeR 2) (< 46 dataR 123))
                 (princ (chr dataR))
                 (setq oStr (strcat oStr (chr dataR))))
              
              (  (and (= codeR 2) (= dataR 8) (< 0 (strlen oStr)))
                 (princ (vl-list->string '(8 32 8)))
                 (setq oStr (substr oStr 1 (1- (strlen oStr)))))
              
              (  (and (= codeR 2) (= 15 dataR))
                 (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))
              
              (  (or (and (= codeR 2) (vl-position dataR '(32 13)))
                     (= code 25))
               
                 (cond (  (< 0 (strlen oStr))
                      
                          (cond (  (vl-position oStr '("e" "E" "EXIT" "Exit" "exit"))
                                   (setq tmpOff nil))
                            
                                (  (setq tmpOff (txt2num oStr))
                                   (setq *txOff tmpOff) nil)
                            
                                (  (princ "\nInvalid Distance Entered.")
                                   (setq oStr "")
                                   (princ msgR))))
                     
                       (t (setq tmpOff nil))))
              
              (  (and (= codeR 3) (listp dataR))
                 (setq *txOff tmpOff) nil)
              
              (t (princ "\nInvalid Input.") (princ msgR))))))

  (defun Text_Stretch (/ BaseDis BasePt oStr msgR grR codeR dataR cPt ang ptO der tmpspc k grLst i inc tmpPt)
    (setq oStr "")

    (princ (setq msgR (strcat "\nSpecify Text Spacing [Exit] <" (vl-princ-to-string dSpc) "> : ")))

    (setq BaseDis (vlax-curve-getDistatPoint ent
                    (setq BasePt
                      (vlax-curve-getClosestPointto ent
                        (vlax-get (caar pLst) (cadar pLst))))))
    (while
      (progn
        (setq grR (grread 't 15 0) codeR (car grR) dataR (cadr grR))
        (redraw)
        
        (cond (  (and (= codeR 5) (listp dataR))                 

                 (setq cPt    (vlax-curve-getClosestPointto ent dataR) k 0 ang (angle cPt dataR)
                       tmpspc (/ (* ((eval fooC) 0.)
                                    (- (vlax-curve-getDistatPoint ent cPt) BaseDis))
                                 (float (1- (length pLst)))))
               
                 (grdraw cPt dataR 40 1)
                 (setq aFac (- (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                 (vlax-curve-getParamatPoint ent cPt))) ang))
                 (grdraw BasePt (polar BasePt (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                                (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                               (vlax-curve-getParamatPoint ent BasePt)))) aFac))
                                       (distance cPt dataR)) 40 1)
                 (vlax-put (caar pLst) (cadar pLst)
                           (polar BasePt (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                           (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                          (vlax-curve-getParamatPoint ent BasePt)))) aFac)) *txOff))

                 (setq grLst nil i -1 inc (/ (- (vlax-curve-getDistatPoint ent cPt) BaseDis) 50.))
                 (repeat 50
                   (setq grLst (cons (polar (setq tmpPt (vlax-curve-getPointatDist ent (+ BaseDis (* (setq i (1+ i)) inc))))
                                            (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                              (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                             (vlax-curve-getParamatPoint ent tmpPt)))) aFac))
                                            (distance cPt dataR)) grLst)))
                 (grvecs (append '(-91) grLst))                                            

                 (foreach Obj (cdr pLst)
                   (if (setq ptO (vlax-curve-getPointatDist ent (+ bDis (* (setq k ((eval fooC) k)) tmpspc))))
                     (vlax-put (car Obj) (cadr Obj)
                               (polar ptO (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                            (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                           (vlax-curve-getParamatPoint ent ptO)))) aFac)) *txOff))))
               t)
                               
              (  (and (= codeR 2) (< 46 dataR 123))
                 (princ (chr dataR))
                 (setq oStr (strcat oStr (chr dataR))))
              
              (  (and (= codeR 2) (= dataR 8) (< 0 (strlen oStr)))
                 (princ (vl-list->string '(8 32 8)))
                 (setq oStr (substr oStr 1 (1- (strlen oStr)))))
              
              (  (and (= codeR 2) (= 15 dataR))
                 (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))
              
              (  (or (and (= codeR 2) (vl-position dataR '(32 13)))
                     (= code 25))
               
                 (cond (  (< 0 (strlen oStr))
                      
                          (cond (  (vl-position oStr '("e" "E" "EXIT" "Exit" "exit"))
                                   (setq tmpspc nil))
                            
                                (  (setq tmpspc (txt2num oStr))
                                   (setq dSpc tmpspc) nil)
                            
                                (  (princ "\nInvalid Distance Entered.")
                                   (setq oStr "")
                                   (princ msgR))))
                     
                       (t (setq tmpspc nil))))
              
              (  (and (= codeR 3) (listp dataR))
                 (setq dSpc tmpspc) nil)
              
              (t (princ "\nInvalid Input.") (princ msgR))))))

(defun txt2num  (txt)
  (cond ((distof txt 5)) ((distof txt 2))
        ((distof txt 1)) ((distof txt 4))
        ((distof txt 3))))

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  (and (not acet-sys-shift-down)
       (findfile "acetutil.arx")
       (arxload (findfile "acetutil.arx") "Failed to Load Express Tools"))
  (setq et  (not (vl-catch-all-error-p
                   (vl-catch-all-apply 'acet-sys-shift-down '( )))))

  (setq mLst '("HORIZONTAL" "VERTICAL" "STRETCH")
        cLst '("CURVE MOVE" "CURVE STRETCH" "CURVE OFFSET") cMode 0)
  (or *txMode (setq *txMode   0))
  (or *txRot  (setq *txRot  0.0))
  (or *txSpc  (setq *txSpc 10.0))
  (or *txOff  (setq *txOff  0.0))
  (or *txJus  (setq *txJus    1))

  (princ "\nSelect Text to Align...")
  (if (and (setq ss (ssget "_:L" '((0 . "*TEXT"))))
           (/= 1 (sslength ss)))
    (progn

      (while
        (progn
          (initget "Text")
          (or (vl-consp pt)
              (setq pt (getpoint "\nSpecify Alignment Point or [T]ext Object: ")))
          
          (cond (  (vl-consp pt) nil)
                
                (  (eq "Text" pt)

                   (while
                     (progn
                       (initget "Point")
                       (setq ent (entsel "\nSelect Text Object or [P]oint: "))

                       (cond (  (vl-consp ent)
                              
                                (if (wcmatch (cdr (assoc 0 (entget (car ent)))) "*TEXT")
                                  (not (setq pt (last (car (GetProp (list (vlax-ename->vla-object (car ent))))))))
                                  (princ "\nObject is not Text.")))

                             (  (eq "Point" ent) nil)

                             (t (princ "\nNothing Selected."))))) t))))

      (if (vl-consp pt)
        (progn
        
          (vla-StartUndoMark doc)

          (setq i -1 col 3)
          (while (setq ent (ssname ss (setq i (1+ i))))
            (setq ObjLst (cons (vlax-ename->vla-object ent) ObjLst)))
          (setq rLst (GetProp ObjLst))

          (or (and (= 1 *txMode) (setq foo 'car bar '<))
              (setq foo 'cadr bar '>))

          (setq ObjLst (mapcar 'car
                         (vl-sort rLst
                           (function
                             (lambda (a b)
                               ((eval bar) ((eval foo) (vlax-get (car a) (cadr a)))
                                           ((eval foo) (vlax-get (car b) (cadr b)))))))))

          (eval (setq msg '(princ (strcat "\n[TAB] to Change Mode, [S]pace Text, [SHIFT] Align Rotation"
                                          "\n[R]otation, [O]bject, [J]ustification"
                                          "\nCurrent Mode: " (nth *txMode MLst)))))
          
          (while
            (progn
              (setq gr (grread 't 15 0) code (car gr) data (cadr gr))
              (redraw)
              
              (cond (  (and (= 5 code) (listp data))

                       (setq bPt (cond ((= 2 *txMode) (last (car (GetProp (list (car ObjLst)))))) (pt)))
                     
                       (if (not (zerop (getvar "ORTHOMODE")))
                         (if (< (abs (- (car data) (car bPt))) (abs (- (cadr data) (cadr bPt))))
                           (setq data (list (car bPt) (cadr data) (caddr data)))
                           (setq data (list (car data) (cadr bPt) (caddr data)))))

                       (setq *tx (cond ((zerop *txMode) 0.) ((/ pi 2.))) j -1
                             ang (angle bPt data) dis (/ (distance bPt data) (1- (float (length ObjLst)))))

                       (if (and et (acet-sys-shift-down))
                         (mapcar (function (lambda (object) (vla-put-rotation object (+ ang (/ pi 2.))))) ObjLst))

                       (foreach obj ObjLst
                         (setq prop (if (eq "AcDbText" (vla-get-ObjectName obj))
                                      (if (eq acAlignmentLeft (vla-get-Alignment obj))
                                        'InsertionPoint 'TextAlignmentPoint)
                                      'InsertionPoint))

                         (cond (  (= 2 *txMode)
                                  (grdraw bPt data col 1)
                                  (vlax-put Obj prop (polar bPt ang (* (setq j (1+ j)) dis))))

                               (t (grdraw bPt data col 1)
                                  (setq bsPt (vlax-get obj prop))
                                  (if (setq iPt (inters bPt data (polar bsPt *tx 1) bsPt nil))
                                    (vlax-put Obj prop iPt)))))
                       t)

                    (  (= 2 code)
                     
                       (cond  (  (= 13 data) nil)
                              (  (= 32 data) nil)
                              (  (= 9  data)
                                 (cond ((= (1- (length mLst)) *txMode)
                                        (setq *txMode 0))
                                       ((setq *txMode (1+ *txMode))))
                                 (eval msg))
                              
                              (  (= 15 data) (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))
                              (  (vl-position data '(99 67)) (setq col (1+ (rem col 6))))
                              (  (vl-position data '(115 83))

                                 (if (= *txMode 2) (princ "\nText Cannot be Spaced in this Mode")
                                   (progn
                                     (initget 4)
                                     (setq *txSpc
                                       (cond ((getdist (strcat "\nSpecify Text Spacing <" (vl-princ-to-string *txSpc) "> : ")))
                                             (*txSpc)))

                                     (or (and (zerop *tx) (setq foo 'cadr bar '>))
                                         (setq foo 'car bar '<))

                                     (setq tmpLst (GetProp ObjLst))

                                     (setq ObjLst (mapcar 'car
                                                    (setq tmpLst (vl-sort tmpLst
                                                                   (function
                                                                     (lambda (a b)
                                                                       ((eval bar) ((eval foo) (vlax-get (car a) (cadr a)))
                                                                                   ((eval foo) (vlax-get (car b) (cadr b))))))))) j 0)

                                     (setq bsPt  (vlax-get (caar tmpLst) (cadar tmpLst))
                                           bsAng (angle (vlax-get (caar tmpLst) (cadar tmpLst))
                                                        (vlax-get (car (last tmpLst)) (cadr (last tmpLst)))))

                                     (foreach obj (cdr ObjLst)
                                       (setq prop (if (eq "AcDbText" (vla-get-ObjectName obj))
                                                    (if (eq acAlignmentLeft (vla-get-Alignment obj))
                                                      'InsertionPoint 'TextAlignmentPoint)
                                                    'InsertionPoint))

                                       (vlax-put Obj prop (polar bsPt bsAng (* (setq j (1+ j)) *txSpc))))))
                                   
                                     (eval msg))

                              (  (vl-position data '(114 82)) (Text_Rotation) (eval msg))

                              (  (vl-position data '(74 106))

                                 (setq jLst '("TL" "TC" "TR" "ML" "MC" "MR" "BL" "BC" "BR"))
                                 (initget "TL TC TR ML MC MR BL BC BR")
                                 (setq *txJus
                                   (1+
                                     (vl-position
                                       (cond
                                         ((getkword (strcat "\nSpecify Text Justifcation [TL/TC/TR/ML/MC/MR/BL/BC/BR] <"
                                                            (nth (1- *txJus) jLst) "> : ")))
                                         ((nth (1- *txJus) jLst))) jLst)))

                                 (mapcar
                                   (function
                                     (lambda (object / tmp)
                                       (if (eq "AcDbText" (vla-get-ObjectName object))
                                         (if (eq AcAlignmentLeft (vla-get-Alignment object))
                                           (progn
                                             (setq tmp (vla-get-InsertionPoint object))
                                             (vla-put-Alignment object (+ *txJus 5))
                                             (vla-put-TextAlignmentPoint object tmp))
                                           (vla-put-Alignment object (+ *txJus 5)))
                                         (vla-put-AttachmentPoint object *txJus)))) ObjLst)

                                 (eval msg))

                              (  (vl-position data '(79 111))

                                 (while
                                   (progn
                                     (setq ent (car (entsel "\nSelect Object to Align Text <Exit> : ")))

                                     (cond (  (eq 'ENAME (type ent))
                                            
                                              (if (vl-catch-all-error-p
                                                    (vl-catch-all-apply 'vlax-curve-getEndParam (list ent)))
                                                (princ "\nInvalid Object Type Selected.")))

                                           (t (eval msg) (setq ent nil)))))

                                 (if ent
                                   (progn

                                     (setq pLst (GetProp ObjLst) k 0 fooC '1+
                                           dSpc (/ (- (vlax-curve-getDistatParam ent (vlax-curve-getEndParam ent))
                                                      (vlax-curve-getDistatParam ent (vlax-curve-getStartParam ent)))
                                                   (* 2. (length ObjLst))))
                                   
                                     (vlax-put (caar pLst) (cadar pLst)
                                               (setq bsPt (vlax-curve-getClosestPointto ent
                                                            (vlax-get (caar pLst) (cadar pLst)))))
                                     (setq bsDis (vlax-curve-getDistatPoint ent bsPt))

                                     (foreach obj (cdr pLst)
                                       (if (setq ptO (vlax-curve-getPointatDist ent (+ (* (setq k ((eval fooC) k)) dSpc) bsDis)))
                                         (vlax-put (car obj) (cadr obj) ptO)))

                                     (princ (setq msgC "\n[E]xit, Re[V]erse, Text [O]ffset, [S]pace Text, [SHIFT] Align Rotation, [R]otation"))
                                     
                                     (while
                                       (progn
                                         (setq grC (grread 't 15 0) codeC (car grC) dataC (cadr grC))
                                         (redraw)

                                         (cond (  (and (= codeC 5) (listp dataC))

                                                  (setq cPt  (vlax-curve-getClosestPointto ent dataC) k 0
                                                        ang  (angle cPt dataC)
                                                        bDis (vlax-curve-getDistatPoint ent cPt))
                                                  (grdraw cPt dataC col 1)
                                                
                                                  (vlax-put (caar pLst) (cadar pLst) (polar cPt ang *txOff))
                                                  (if (and et (acet-sys-shift-down))
                                                    (vla-put-rotation (caar pLst) (- ang (/ pi 2.))))

                                                  (setq aFac (- (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                                  (vlax-curve-getParamatPoint ent cPt))) ang))
                                                
                                                  (foreach Obj (cdr pLst)
                                                    (if (setq ptO (vlax-curve-getPointatDist ent (+ bDis (* (setq k ((eval fooC) k)) dSpc))))
                                                      (vlax-put (car Obj) (cadr Obj)
                                                                (polar ptO (setq cAng (if (vl-position (cdr (assoc 0 (entget ent))) '("XLINE" "LINE")) ang
                                                                                        (- (setq der (angle '(0 0 0) (vlax-curve-getFirstDeriv ent
                                                                                                                       (vlax-curve-getParamatPoint ent ptO))))
                                                                                           aFac)))
                                                                       *txOff)))                                                  
                                                    
                                                    (if (and et (acet-sys-shift-down))
                                                      (vla-put-rotation (car Obj) (- cAng (/ pi 2.)))))
                                                t)

                                               (  (= codeC 2)

                                                  (cond (  (vl-position dataC '(114 82)) (Text_Rotation) (princ msgC))

                                                        (  (vl-position dataC '(99 67)) (setq col (1+ (rem col 6))))

                                                        (  (vl-position dataC '(118 86))
                                                           (setq fooC (cond ((eq fooC '1+) '1-) ('1+))))

                                                        (  (vl-position dataC '(79 111)) (Text_Offset)  (princ msgC))

                                                        (  (vl-position dataC '(83 115)) (Text_Stretch) (princ msgC))

                                                        (  (vl-position dataC '(13 32)) nil)

                                                        (  (vl-position dataC '(69 101)) (eval msg) nil)

                                                        (t )))

                                               (  (and (= codeC 3) (listp dataC)) nil)

                                               (  (= codeC 25) nil)

                                               (t ))))
                                     
                                     (cond ((vl-position dataC '(69 101))))) t))
               
                              (t )))

                    (  (= 25 code) nil)

                    (  (and (= 3 code) (listp data)) nil)

                    (t ))))

          (vla-EndUndoMark doc))

        (princ "\nNo Alignment Point Specified."))))

  (redraw)
  (princ))

(vl-load-com)
(princ "\n:: TxAlign.lsp | Version 1.6 | © Lee Mac 2009 www.lee-mac.com ::")
(princ "\n:: Type \"TxAlign\" to Invoke ::")
(princ)

;;-------------------------------------------------------------------------------;;
;;                                 End of File                                   ;;
;;-------------------------------------------------------------------------------;;


;;--------------=={ Count.lsp - Advanced Block Counter }==--------------;;
;;                                                                      ;;
;;  This program enables the user to record the quantities of a         ;;
;;  selection or all standard or dynamic blocks in the working drawing. ;;
;;  The results of the block count may be displayed at the AutoCAD      ;;
;;  command-line, written to a Text or CSV file, or displayed in an     ;;
;;  AutoCAD Table, where available.                                     ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'count' at the AutoCAD              ;;
;;  command-line, the user is prompted to make a selection of standard  ;;
;;  or dynamic blocks to be counted by the program. At this prompt,     ;;
;;  the user may right-click or press 'Enter' to automatically count    ;;
;;  all blocks in the drawing.                                          ;;
;;                                                                      ;;
;;  Depending on the output setting, the results may then be printed    ;;
;;  to the AutoCAD command-line and displayed in the Text Window, or    ;;
;;  the user will be prompted to specify an insertion point for the     ;;
;;  table, or a filename & location for the Text or CSV output file.    ;;
;;                                                                      ;;
;;  The program settings may be configured using the 'countsettings'    ;;
;;  command; this command will present the user with a dialog interface ;;
;;  through which the data output, table & file headings, displayed     ;;
;;  columns, sorting field & sort order may each be altered.            ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2010-06-05                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2010-06-06                                      ;;
;;                                                                      ;;
;;  - Updated code to include Settings dialog.                          ;;
;;  - Added Undo Marks.                                                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2010-06-06                                      ;;
;;                                                                      ;;
;;  - Fixed bug with 64-bit systems.                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2011-03-02                                      ;;
;;                                                                      ;;
;;  - Program completely rewritten.                                     ;;
;;  - Updated code to work without error on 64-bit systems by fixing    ;;
;;    bug with ObjectID subfunction - my thanks go to member 'Jeff M'   ;;
;;    at theSwamp.org forums for helping me solve this problem.         ;;
;;  - Added ability to write block count to Text/CSV Files.             ;;
;;----------------------------------------------------------------------;;
;;  Version 1.4    -    2014-06-15                                      ;;
;;                                                                      ;;
;;  - Program completely rewritten.                                     ;;
;;----------------------------------------------------------------------;;
;;  Version 1.5    -    2015-06-07                                      ;;
;;                                                                      ;;
;;  - Minor update to enable full compatibility with ZWCAD.             ;;
;;    (regeneratetablesuppressed property not available)                ;;
;;----------------------------------------------------------------------;;

(setq
    count:version "1-5"
    count:defaults
   '(
        (out "tab")
        (tg1 "1")
        (tg2 "1")
        (tg3 "1")
        (ed1 "Block Data")
        (ed2 "Preview")
        (ed3 "Block Name")
        (ed4 "Count")
        (srt "blk")
        (ord "asc")
    )
)

;;----------------------------------------------------------------------;;

(defun count:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

;;----------------------------------------------------------------------;;

(defun count:getsavepath ( / tmp )
    (cond      
        (   (setq tmp (getvar 'roamablerootprefix))
            (strcat (count:fixdir tmp) "\\Support")
        )
        (   (setq tmp (findfile "acad.pat"))
            (count:fixdir (vl-filename-directory tmp))
        )
        (   (count:fixdir (vl-filename-directory (vl-filename-mktemp))))
    )
)

;;----------------------------------------------------------------------;;

(setq count:savepath (count:getsavepath) ;; Save path for DCL & Config files
      count:dclfname (strcat count:savepath "\\LMAC_count_V" count:version ".dcl")
      count:cfgfname (strcat count:savepath "\\LMAC_count_V" count:version ".cfg")
)

;;----------------------------------------------------------------------;;

(defun c:blkcount

    (
        /
        *error*
        all
        col
        des dir
        ed1 ed2 ed3 ed4
        fil fnm fun
        hgt
        idx ins
        lst
        ord out
        row
        sel srt
        tab tg1 tg2 tg3 tmp
        xrf
    )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'vla-object (type tab))
                 (null (vlax-erased-p tab))
                 (= "AcDbTable" (vla-get-objectname tab))
                 (vlax-write-enabled-p tab)
                 (vlax-property-available-p tab 'regeneratetablesuppressed t)
            )
            (vla-put-regeneratetablesuppressed tab :vlax-false)
        )
        (if (and (= 'vla-object (type count:wshobject))
                 (not (vlax-object-released-p count:wshobject))
            )
            (progn
                (vlax-release-object count:wshobject)
                (setq count:wshobject nil)
            )
        )
        (count:endundo (count:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (findfile count:cfgfname))
        (count:writecfg count:cfgfname (mapcar 'cadr count:defaults))
    )
    (count:readcfg count:cfgfname (mapcar 'car count:defaults))
    (foreach sym count:defaults
        (if (not (boundp (car sym))) (apply 'set sym))
    )
    (if (and (= "tab" out) (not (vlax-method-applicable-p (vla-get-modelspace (count:acdoc)) 'addtable)))
        (setq out "txt")
    )
    
    (count:startundo (count:acdoc))

    (while (setq tmp (tblnext "block" (null tmp)))
        (if (= 4 (logand 4 (cdr (assoc 70 tmp))))
            (setq xrf (vl-list* "," (cdr (assoc 2 tmp)) xrf))
        )
    )
    (if xrf
        (setq fil  (list '(0 . "INSERT") '(-4 . "<NOT") (cons 2 (apply 'strcat (cdr xrf))) '(-4 . "NOT>")))
        (setq fil '((0 . "INSERT")))
    )
    
    (cond
        (   (null (setq all (ssget "_X" fil)))
            (count:popup
                "No Blocks Found" 64
                (princ "No blocks were found in the active drawing.")
            )
        )
        (   (and (= "tab" out) (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer)))))))
            (count:popup
                "Current Layer Locked" 64
                (princ "Please unlock the current layer before using this program.")
            )
        )
        (   (progn
                (setvar 'nomutt 1)
                (princ "\nSelect blocks to count <all>: ")
                (setq sel
                    (cond
                        (   (null (setq sel (vl-catch-all-apply 'ssget (list fil))))
                            all
                        )
                        (   (null (vl-catch-all-error-p sel))
                            sel
                        )
                    )
                )
                (setvar 'nomutt 0)
                (null sel)
            )
        )
        (   (or (= "com" out)
                (and (=  "tab" out) (setq ins (getpoint "\nSpecify point for table: ")))
                (and (/= "tab" out)
                    (setq fnm
                        (getfiled "Create Output File"
                            (cond
                                (   (and (setq dir (getenv "LMac\\countdir"))
                                         (vl-file-directory-p (setq dir (count:fixdir dir)))
                                    )
                                    (strcat dir "\\")
                                )
                                (   (getvar 'dwgprefix))
                            )
                            out 1
                        )
                    )
                )
            )
            (repeat (setq idx (sslength sel))
                (setq lst (count:assoc++ (count:effectivename (ssname sel (setq idx (1- idx)))) lst))
            )
            (if (= "blk" srt)
                (setq fun (eval (list 'lambda '( a b ) (list (if (= "asc" ord) '< '>) '(strcase (car a)) '(strcase (car b))))))
                (setq fun (eval (list 'lambda '( a b ) (list (if (= "asc" ord) '< '>) '(cdr a) '(cdr b)))))
            )
            (setq lst (vl-sort lst 'fun))
            (cond
                (   (= "com" out)
                    (defun prinn ( x ) (princ "\n") (princ x))
                    (prinn (count:padbetween "" "" "=" 60))
                    (if (= "1" tg1)
                        (progn
                            (prinn ed1)
                            (prinn (count:padbetween "" "" "-" 60))
                        )
                    )
                    (prinn (count:padbetween ed3 ed4 " " 55))
                    (prinn (count:padbetween "" "" "-"   60))
                    (if (= "1" tg3)
                        (foreach itm lst
                            (prinn (count:padbetween (car itm) (itoa (cdr itm)) "." 55))
                        )
                        (foreach itm lst (prinn (car itm)))
                    )
                    (prinn (count:padbetween "" "" "=" 60))
                    (textpage)
                )
                (   (= "tab" out)
                    (if (= "1" tg3)
                        (setq lst (mapcar '(lambda ( x ) (list (car x) (itoa (cdr x)))) lst))
                        (setq lst (mapcar '(lambda ( x ) (list (car x))) lst))
                    )
                    (setq hgt
                        (vla-gettextheight
                            (vla-item
                                (vla-item (vla-get-dictionaries (count:acdoc)) "acad_tablestyle")
                                (getvar 'ctablestyle)
                            )
                            acdatarow
                        )
                    )
                    (setq tab
                        (vla-addtable
                            (vlax-get-property (count:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                            (vlax-3D-point (trans ins 1 0))
                            (+ (length lst) 2)
                            (+ 1 (atoi tg2) (atoi tg3))
                            (* 2.5 hgt)
                            (* hgt
                                (max
                                    (apply 'max
                                        (mapcar 'strlen
                                            (append
                                                (if (= "1" tg2) (list ed2))
                                                (if (= "1" tg3) (list ed4))
                                                (cons ed3 (apply 'append lst))
                                            )
                                        )
                                    )
                                    (if (= "1" tg1) (/ (strlen ed1) (+ 1 (atoi tg2) (atoi tg3))) 0)
                                )
                            )
                        )
                    )
                    (if (vlax-property-available-p tab 'regeneratetablesuppressed t)
                        (vla-put-regeneratetablesuppressed tab :vlax-true)
                    )
                    (vla-put-stylename tab (getvar 'ctablestyle))
                    (setq col 0)
                    (mapcar
                       '(lambda ( a b ) (if (= "1" a) (progn (vla-settext tab 1 col b) (setq col (1+ col)))))
                        (list tg2 "1" tg3)
                        (list ed2 ed3 ed4)
                    )
                    (setq row 2)
                    (foreach itm lst
                        (if (= "1" tg2)
                            (count:setblocktablerecord tab row (setq col 0) (car itm))
                            (setq col -1)
                        )
                        (foreach txt itm
                            (vla-settext tab row (setq col (1+ col)) txt)
                        )
                        (setq row (1+ row))
                    )
                    (if (= "1" tg1)
                        (vla-settext tab 0 0 ed1)
                        (vla-deleterows tab 0 1)
                    )
                )
                (   (setenv "LMac\\countdir" (count:fixdir (vl-filename-directory fnm)))
                    (if
                        (
                            (if (= "txt" out)
                                count:writetxt
                                count:writecsv
                            )
                            (append
                                (if (= "1" tg1)
                                    (list (list ed1))
                                )
                                (if (= "1" tg3)
                                    (cons (list ed3 ed4) (mapcar '(lambda ( x ) (list (car x) (itoa (cdr x)))) lst))
                                    (cons (list ed3)     (mapcar '(lambda ( x ) (list (car x))) lst))
                                )
                            )
                            fnm
                        )
                        (princ (strcat "\nBlock data written to " fnm))
                        (count:popup "Unable to Create Output File" 48
                            (princ
                                (strcat
                                    "The program was unable to create the following file:\n\n"
                                    fnm
                                    "\n\nPlease ensure that you have write-permissions for the above directory."
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (*error* nil)
    (princ)
)

;;----------------------------------------------------------------------;;

(defun c:blkcountsettings

    (
        /
        *error*
        dch des
        ord out out-fun
        srt
        tg1 tg1-fun tg2 tg2-fun tg3 tg3-fun
    )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'int (type dch))
                 (< 0 dch)
            )
            (unload_dialog dch)
        )
        (if (and (= 'vla-object (type count:wshobject))
                 (not (vlax-object-released-p count:wshobject))
            )
            (progn
                (vlax-release-object count:wshobject)
                (setq count:wshobject nil)
            )
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (findfile count:cfgfname))
        (count:writecfg count:cfgfname (mapcar 'cadr count:defaults))
    )
    (count:readcfg count:cfgfname (mapcar 'car count:defaults))
    (foreach sym count:defaults
        (if (not (boundp (car sym))) (apply 'set sym))
    )
    (cond
        (   (not (count:writedcl count:dclfname))
            (count:popup "DCL file could not be written" 48
                (princ
                    (strcat
                        "The DCL file required by this program could not be written to the following location:\n\n"
                        count:dclfname
                        "\n\nPlease ensure that you have write-permissions for the above directory."
                    )
                )
            )
        )
        (   (<= (setq dch (load_dialog count:dclfname)) 0)
            (count:popup "DCL file could not be loaded" 48
                (princ
                    (strcat
                        "The following DCL file required by this program could not be loaded:\n\n"
                        count:dclfname
                        "\n\nPlease verify the integrity of this file."
                    )
                )
            )
        )
        (   (not (new_dialog "dia" dch))
            (count:popup "DCL file contains an error" 48
                (princ
                    (strcat
                        "The program dialog could not be displayed as the following DCL file file contains an error:\n\n"
                        count:dclfname
                        "\n\nPlease verify the integrity of this file."
                    )
                )
            )
        )
        (   t
            (set_tile "dcl"
                (strcat
                    "Count.lsp Version "
                    (vl-string-translate "-" "." count:version)
                    " \\U+00A9 Lee Mac "
                    (menucmd "m=$(edtime,0,yyyy)")
                )
            )
            (if (and (= "tab" out) (not (vlax-method-applicable-p (vla-get-modelspace (count:acdoc)) 'addtable)))
                (progn
                    (mode_tile "tab" 1)
                    (setq out "txt")
                )
            )
            (   (setq tg1-fun (lambda ( val ) (mode_tile "ed1" (- 1 (atoi (setq tg1 val)))))) (set_tile "tg1" tg1))
            (action_tile "tg1" "(tg1-fun $value)")

            (   (setq tg2-fun (lambda ( val ) (mode_tile "ed2" (- 1 (atoi (setq tg2 val)))))) (set_tile "tg2" tg2))
            (action_tile "tg2" "(tg2-fun $value)")

            (   (setq tg3-fun (lambda ( val ) (mode_tile "ed4" (- 1 (atoi (setq tg3 val)))))) (set_tile "tg3" tg3))
            (action_tile "tg3" "(tg3-fun $value)")

            (foreach key '("ed1" "ed2" "ed3" "ed4")
                (set_tile key (eval (read key)))
                (action_tile key (strcat "(setq " key " $value)"))
            )
            (set_tile out "1")
            (   (setq out-fun
                    (lambda ( val )
                        (if (= "tab" (setq out val))
                            (progn
                                (mode_tile "tg2" 0)
                                (mode_tile "ed2" (- 1 (atoi tg2)))
                            )
                            (progn
                                (mode_tile "tg2" 1)
                                (mode_tile "ed2" 1)
                            )
                        )
                    )
                )
                out
            )
            (foreach key '("tab" "txt" "csv" "com")
                (action_tile key "(out-fun $key)")
            )
            (set_tile srt "1")
            (foreach key '("blk" "qty")
                (action_tile key "(setq srt $key)")
            )
            (set_tile ord "1")
            (foreach key '("asc" "des")
                (action_tile key "(setq ord $key)")
            )
            (if (= 1 (start_dialog))
                (count:writecfg count:cfgfname (mapcar 'eval (mapcar 'car count:defaults)))
            )
        )
    )
    (*error* nil)
    (princ)
)

;;----------------------------------------------------------------------;;
                
(defun count:popup ( ttl flg msg / err )
    (setq err (vl-catch-all-apply 'vlax-invoke-method (list (count:wsh) 'popup msg 0 ttl flg)))
    (if (null (vl-catch-all-error-p err))
        err
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:wsh nil
    (cond (count:wshobject) ((setq count:wshobject (vlax-create-object "wscript.shell"))))
)

;;----------------------------------------------------------------------;;

(defun count:tostring ( arg / dim )
    (cond
        (   (= 'int (type arg))
            (itoa arg)
        )
        (   (= 'real (type arg))
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq arg (rtos arg 2 15))
            (setvar 'dimzin dim)
            arg
        )
        (   (vl-prin1-to-string arg))
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:writecfg ( cfg lst / des )
    (if (setq des (open cfg "w"))
        (progn
            (foreach itm lst (write-line (count:tostring itm) des))
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:readcfg ( cfg lst / des itm )
    (if
        (and
            (setq cfg (findfile cfg))
            (setq des (open cfg "r"))
        )
        (progn
            (foreach sym lst
                (if (setq itm (read-line des))
                    (set  sym (read itm))
                )
            )
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:writedcl ( dcl / des )
    (cond
        (   (findfile dcl))
        (   (setq des (open dcl "w"))
            (foreach itm
               '(
                    "//--------------------=={ Count Dialog Definition }==-------------------//"
                    "//                                                                      //"
                    "//  Dialog definition file for use in conjunction with Count.lsp        //"
                    "//----------------------------------------------------------------------//"
                    "//  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              //"
                    "//----------------------------------------------------------------------//"
                    ""
                    "b15 : edit_box"
                    "{"
                    "    edit_width = 16;"
                    "    edit_limit = 1024;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "    horizontal_margin = none;"
                    "    vertical_margin = none;"
                    "}"
                    "b30 : edit_box"
                    "{"
                    "    edit_width = 52;"
                    "    edit_limit = 1024;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "    horizontal_margin = none;"
                    "    vertical_margin = none;"
                    "}"
                    "tog : toggle"
                    "{"
                    "    vertical_margin = none;"
                    "    horizontal_margin = 0.2;"
                    "}"
                    "rwo : row"
                    "{"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    "rrw : radio_row"
                    "{"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    "dia : dialog"
                    "{"
                    "    key = \"dcl\";"
                    "    spacer_1;"
                    "    : boxed_column"
                    "    {"
                    "        label = \"Output\";"
                    "        : rrw"
                    "        {"
                    "            : radio_button { key = \"tab\"; label = \"Table\"; }"
                    "            : radio_button { key = \"txt\"; label = \"Text File\"; }"
                    "            : radio_button { key = \"csv\"; label = \"CSV File\"; }"
                    "            : radio_button { key = \"com\"; label = \"Command line\"; }"
                    "        }"
                    "        spacer;"
                    "    }"
                    "    : boxed_column"
                    "    {"
                    "        label = \"Headings\";"
                    "        spacer_1;"
                    "        : rwo"
                    "        {"
                    "            : tog { key = \"tg1\"; }"
                    "            : b30 { key = \"ed1\"; }"
                    "            : spacer"
                    "            {"
                    "                fixed_width = true;"
                    "                vertical_margin = none;"
                    "                width = 2.5;"
                    "            }"
                    "        }"
                    "        : rwo"
                    "        {"
                    "            spacer;"
                    "            : tog { key = \"tg2\"; }"
                    "            : b15 { key = \"ed2\"; }"
                    "            : b15 { key = \"ed3\"; }"
                    "            : b15 { key = \"ed4\"; }"
                    "            : tog { key = \"tg3\"; }"
                    "            spacer;"
                    "        }"
                    "        spacer_1;"
                    "    }"
                    "    : row"
                    "    {"
                    "        : boxed_column"
                    "        {"
                    "            label = \"Sort By\";"
                    "            : rrw"
                    "            {"
                    "                : radio_button { key = \"blk\"; label = \"Block Name\"; }"
                    "                : radio_button { key = \"qty\"; label = \"Quantity\"; }"
                    "            }"
                    "            spacer;"
                    "        }"
                    "        : boxed_column"
                    "        {"
                    "            label = \"Sort Order\";"
                    "            : rrw"
                    "            {"
                    "                : radio_button { key = \"asc\"; label = \"Ascending\"; }"
                    "                : radio_button { key = \"des\"; label = \"Descending\"; }"
                    "            }"
                    "            spacer;"
                    "        }"
                    "    }"
                    "    spacer_1; ok_cancel;"
                    "}"
                    ""
                    "//----------------------------------------------------------------------//"
                    "//                             End of File                              //"
                    "//----------------------------------------------------------------------//"
                )
                (write-line itm des)
            )
            (setq des (close des))
            (while (not (findfile dcl))) ;; for slow HDDs
            dcl
        )
    )
)

;;----------------------------------------------------------------------;;
 
(defun count:writecsv ( lst csv / des sep )
    (if (setq des (open csv "w"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (foreach row lst (write-line (count:lst->csv row sep) des))
            (close des)
            t
        )
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:lst->csv ( lst sep )
    (if (cdr lst)
        (strcat (count:csv-addquotes (car lst) sep) sep (count:lst->csv (cdr lst) sep))
        (count:csv-addquotes (car lst) sep)
    )
)

;;----------------------------------------------------------------------;;
 
(defun count:csv-addquotes ( str sep / pos )
    (cond
        (   (wcmatch str (strcat "*[`" sep "\"]*"))
            (setq pos 0)    
            (while (setq pos (vl-string-position 34 str pos))
                (setq str (vl-string-subst "\"\"" "\"" str pos)
                      pos (+ pos 2)
                )
            )
            (strcat "\"" str "\"")
        )
        (   str   )
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:writetxt ( lst txt / des )
    (if (setq des (open txt "w"))
        (progn
            (foreach itm lst (write-line (count:lst->str itm "\t") des))
            (close des)
            t
        )
    )
)
 
;;----------------------------------------------------------------------;;
 
(defun count:lst->str ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (count:lst->str (cdr lst) del))
        (car lst)
    )
)

;;----------------------------------------------------------------------;;

(defun count:padbetween ( s1 s2 ch ln )
    (
        (lambda ( a b c )
            (repeat (- ln (length b) (length c)) (setq c (cons a c)))
            (vl-list->string (append b c))
        )
        (ascii ch)
        (vl-string->list s1)
        (vl-string->list s2)
    )
)

;;----------------------------------------------------------------------;;

(defun count:setblocktablerecord ( obj row col blk )
    (eval
        (list 'defun 'count:setblocktablerecord '( obj row col blk )
            (cons
                (if (vlax-method-applicable-p obj 'setblocktablerecordid32)
                    'vla-setblocktablerecordid32
                    'vla-setblocktablerecordid
                )
                (list
                    'obj 'row 'col
                    (list 'count:objectid (list 'vla-item (vla-get-blocks (count:acdoc)) 'blk))
                    ':vlax-true
                )
            )
        )
    )
    (count:setblocktablerecord obj row col blk)
)

;;----------------------------------------------------------------------;;

(defun count:objectid ( obj )
    (eval
        (list 'defun 'count:objectid '( obj )
            (cond
                (   (not (wcmatch (getenv "PROCESSOR_ARCHITECTURE") "*64*"))
                   '(vla-get-objectid obj)
                )
                (   (= 'subr (type vla-get-objectid32))
                   '(vla-get-objectid32 obj)
                )
                (   (list 'vla-getobjectidstring (vla-get-utility (count:acdoc)) 'obj ':vlax-false))
            )
        )
    )
    (count:objectid obj)
)

;;----------------------------------------------------------------------;;

(defun count:assoc++ ( key lst / itm )
    (if (setq itm (assoc key lst))
        (subst (cons key (1+ (cdr itm))) itm lst)
        (cons  (cons key 1) lst)
    )
)

;;----------------------------------------------------------------------;;

(defun count:effectivename ( ent / blk rep )
    (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
        (if
            (and
                (setq rep
                    (cdadr
                        (assoc -3
                            (entget
                                (cdr
                                    (assoc 330
                                        (entget
                                            (tblobjname "block" blk)
                                        )
                                    )
                                )
                               '("AcDbBlockRepBTag")
                            )
                        )
                    )
                )
                (setq rep (handent (cdr (assoc 1005 rep))))
            )
            (setq blk (cdr (assoc 2 (entget rep))))
        )
    )
    blk
)

;;----------------------------------------------------------------------;;

(defun count:startundo ( doc )
    (count:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun count:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun count:acdoc nil
    (eval (list 'defun 'count:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (count:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Count.lsp | Version "
        (vl-string-translate "-" "." count:version)
        " | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"blkcount\" - Main Program | \"blkcountsettings\" - Settings ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;