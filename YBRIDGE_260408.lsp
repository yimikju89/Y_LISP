(vl-load-com)

;; =========================================================================
;; ★ 0. 글로벌 변수 초기화
;; =========================================================================
(setq *yb_sc_x* "0.001" *yb_sc_y* "0.001" *yb_sc_a* "1.0" *yb_off* "0" *yb_yoff* "0" *yb_hgap1* "0" *yb_dim_txt* "(PSC 계열)" *yb_spans* '() *yb_ls_sel* "")
(setq *yb_curve* nil *yb_gl* nil *yb_br* nil *yb_stPt* nil)
(setq *yb_db_cache* '() *YB_CLOUD_MODULES* '())
(setq next_focus "b1")

;; UI 복구용 글로벌 변수 (단위 통일)
(setq *yb_pop* "0" *yb_el* "" *yb_ehgap* "" *yb_ei* "")
(setq *yb_eghf* "" *yb_eghb* "")
(setq *yb_d1_d* "" *yb_d1_f* "" *yb_d1_b* "" *yb_d1_v* "0")
(setq *yb_d2_d* "" *yb_d2_f* "" *yb_d2_b* "" *yb_d2_v* "0")
(setq *yb_d3_d* "" *yb_d3_f* "" *yb_d3_b* "" *yb_d3_v* "0")
(setq *yb_d4_d* "" *yb_d4_f* "" *yb_d4_b* "" *yb_d4_v* "0")
(setq *yb_d5_d* "" *yb_d5_f* "" *yb_d5_b* "" *yb_d5_v* "0")
(setq *yb_eh* "" *yb_em* "" *yb_ecnt* "1" *yb_tset* "0")

;; =========================================================================
;; ★ 1. 만능 숫자 & 데이터 추출기
;; =========================================================================
(defun flatten-nums (lst)
  (cond
    ((null lst) nil)
    ((numberp lst) (list lst))
    ((atom lst) nil)
    (t (append (flatten-nums (car lst)) (flatten-nums (cdr lst))))
  )
)

(defun get-data-val (key data / res sym-name key-name)
  (setq key-name (strcase (vl-symbol-name key)))
  (foreach item data
    (if (and (listp item) (= (type (car item)) 'SYM))
      (progn
        (setq sym-name (strcase (vl-symbol-name (car item))))
        (if (or (= sym-name key-name)
                (= sym-name (strcat key-name ".")))
          (setq res (cdr item))
        )
      )
    )
  )
  res
)

(defun get-all-lines-by-key (key data / all_lines iname targetName lst)
  (setq all_lines '())
  (setq targetName (strcase (vl-symbol-name key)))
  (foreach item data
    (if (and (listp item) (= (type (car item)) 'SYM))
      (progn
        (setq iname (strcase (vl-symbol-name (car item))))
        (if (or (= iname targetName) (= iname (strcat targetName ".")))
          (progn
            (setq lst (cdr item))
            (if (and (listp lst) (listp (car lst)))
              (setq all_lines (append all_lines lst))
            )
          )
        )
      )
    )
  )
  all_lines
)

(defun add-ss-to-ss (ssSrc ssDest / j)
  (if ssSrc
    (progn
      (setq j 0)
      (while (< j (sslength ssSrc))
        (ssadd (ssname ssSrc j) ssDest)
        (setq j (1+ j))
      )
    )
  )
)

;; =========================================================================
;; ★ 2. 깃허브 DB 연동
;; =========================================================================
(defun yb-safe-string-less-p (s1 s2 / i len1 len2 c1 c2 res done)
  (setq i 1 len1 (strlen s1) len2 (strlen s2) res nil done nil)
  (while (and (<= i len1) (<= i len2) (not done))
    (setq c1 (ascii (substr s1 i 1)) c2 (ascii (substr s2 i 1)))
    (cond
      ((< c1 c2) (setq res t done t))
      ((> c1 c2) (setq res nil done t))
      (t (setq i (1+ i)))
    )
  )
  (if (not done) (< len1 len2) res)
)

(defun yb-extract-parts (s / i c pre num post inNum)
  (setq pre "" num "" post "" inNum nil i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (and (>= c "0") (<= c "9"))
      (progn (setq inNum t) (setq num (strcat num c)))
      (if inNum (setq post (strcat post c)) (setq pre (strcat pre c)))
    )
    (setq i (1+ i))
  )
  (list pre (if (= num "") 0 (atoi num)) post)
)

(defun yb-natural-less-p (s1 s2 / p1 p2)
  (setq p1 (yb-extract-parts s1) p2 (yb-extract-parts s2))
  (cond
    ((/= (car p1) (car p2)) (yb-safe-string-less-p (car p1) (car p2)))
    ((/= (cadr p1) (cadr p2)) (< (cadr p1) (cadr p2)))
    (t (yb-safe-string-less-p (caddr p1) (caddr p2)))
  )
)

(defun get-cloud-init ( / url_list httpObj res_list pos1 pos2 fname modList url_raw body stream res_text db data kName strType sepPos suffix)
  (setq url_list "https://api.github.com/repos/yimikju89/YBRIDGE_DB/contents/")
  (setq httpObj (vlax-create-object "WinHttp.WinHttpRequest.5.1"))
  (setq modList '() *yb_db_cache* '())
  
  (if httpObj 
    (progn
      (vlax-invoke-method httpObj 'Open "GET" url_list :vlax-false)
      (vlax-invoke-method httpObj 'SetRequestHeader "User-Agent" "Mozilla/5.0")
      (vlax-invoke-method httpObj 'Send "") 
      (setq res_list (vlax-get-property httpObj 'ResponseText))
      (setq pos1 0)
      
      (while (setq pos1 (vl-string-search "\"name\":" res_list pos1))
        (setq pos1 (vl-string-search "\"" res_list (+ pos1 7)))
        (setq pos2 (vl-string-search "\"" res_list (1+ pos1)))
        (setq fname (substr res_list (+ pos1 2) (- pos2 pos1 1)))
        
        (if (wcmatch (strcase fname) "*.TXT") 
          (progn
            (setq url_raw (strcat "https://raw.githubusercontent.com/yimikju89/YBRIDGE_DB/main/" fname))
            (vlax-invoke-method httpObj 'Open "GET" url_raw :vlax-false)
            (vlax-invoke-method httpObj 'Send "")
            (setq body (vlax-get-property httpObj 'ResponseBody))
            (setq stream (vlax-create-object "ADODB.Stream"))
            
            (if stream 
              (progn
                (vlax-put-property stream 'Type 1) 
                (vlax-invoke stream 'Open)
                (vlax-invoke-method stream 'Write body)
                (vlax-put-property stream 'Position 0) 
                (vlax-put-property stream 'Type 2)
                (vlax-put-property stream 'Charset "utf-8") 
                (setq res_text (vlax-invoke-method stream 'ReadText -1))
                
                (if (not (or (vl-string-search "교대" res_text) (vl-string-search "교각" res_text)))
                  (progn 
                    (vlax-put-property stream 'Position 0) 
                    (vlax-put-property stream 'Charset "euc-kr") 
                    (setq res_text (vlax-invoke-method stream 'ReadText -1))
                  )
                )
                (vlax-invoke stream 'Close) 
                (vlax-release-object stream)
              )
            )
            
            (if (and res_text (/= res_text "")) 
              (progn
                (setq db (vl-catch-all-apply 'read (list res_text)))
                (if (and (not (vl-catch-all-error-p db)) (listp db) (= (type (car db)) 'STR) (listp (cdr db))) 
                  (progn
                    (setq data (cdr db))
                    (setq kName (car db))
                    (setq strType (get-data-val 'TYPE data))
                    (setq *yb_db_cache* (append *yb_db_cache* (list (cons fname data))))
                    (setq sepPos (vl-string-search "_" kName))
                    (setq suffix (if sepPos (substr kName (+ sepPos 2)) kName))
                    
                    (if (equal strType "ABUT")
                      (setq modList (append modList (list (list (strcat "A1_" suffix) fname "ABUT") (list (strcat "A2_" suffix) fname "ABUT"))))
                      (setq modList (append modList (list (list kName fname "PIER"))))
                    )
                  )
                )
              )
            )
          )
        )
        (setq pos1 pos2)
      )
      (vlax-release-object httpObj) 
    )
  )
  (vl-sort modList (function (lambda (a b) (yb-natural-less-p (car a) (car b)))))
)

;; =========================================================================
;; ★ 3. 수학 및 헬퍼 함수
;; =========================================================================
(defun check-make-layer (layName) 
  (if (not (tblsearch "LAYER" layName)) 
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 layName) '(70 . 0)))
  )
)

(defun make-db-layers (layer_list / lays lname lcol ltype layObj)
  (setq lays (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (if layer_list
    (foreach l layer_list
      (setq lname (car l) lcol (cadr l) ltype (caddr l))
      (if (not (tblsearch "LAYER" lname))
        (setq layObj (vla-Add lays lname))
        (setq layObj (vla-Item lays lname))
      )
      (if lcol (vla-put-Color layObj lcol))
      (if (and ltype (/= ltype ""))
        (vl-catch-all-apply 'vla-put-Linetype (list layObj ltype))
      )
    )
  )
)

(defun get-ents-after (ent / ss next) 
  (setq ss (ssadd)) 
  (setq next (if ent (entnext ent) (entnext))) 
  (while next 
    (if (entget next) (ssadd next ss)) 
    (setq next (entnext next))
  ) 
  (if (> (sslength ss) 0) ss nil)
)

(defun yb-str->intlist (str / lst pos) 
  (setq lst '()) 
  (while (setq pos (vl-string-search " " str)) 
    (setq lst (append lst (list (atoi (substr str 1 pos))))) 
    (setq str (substr str (+ pos 2)))
  ) 
  (if (/= str "") (setq lst (append lst (list (atoi str))))) 
  lst
)

(defun get-y-on-curve (curveObj targetX / space tempLine ints yVal)
  (setq space (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq tempLine (vla-AddLine space (vlax-3d-point (list targetX -1000000.0 0.0)) (vlax-3d-point (list targetX 1000000.0 0.0))))
  (setq ints (vlax-invoke curveObj 'IntersectWith tempLine 0)) 
  (vla-Delete tempLine)
  (if (and ints (>= (length ints) 3)) 
    (setq yVal (cadr ints)) 
    (setq yVal nil)
  ) 
  yVal
)

(defun get-min-ground-y (glObj cX halfWidth / curY minY dx)
  (setq minY nil)
  (if glObj 
    (foreach dx (list (- halfWidth) (/ (- halfWidth) 2.0) 0.0 (/ halfWidth 2.0) halfWidth)
      (setq curY (get-y-on-curve glObj (+ cX dx))) 
      (if curY 
        (if (null minY) 
          (setq minY curY) 
          (if (< curY minY) (setq minY curY))
        )
      )
    )
  ) 
  minY
)

(defun get-arc-center (p1 p2 p3 / x1 y1 x2 y2 x3 y3 a b c d e f g cx cy r)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2) x3 (car p3) y3 (cadr p3))
  (setq a (- x2 x1) b (- y2 y1) c (- x3 x1) d (- y3 y1) 
        e (+ (* a (+ x1 x2)) (* b (+ y1 y2))) 
        f (+ (* c (+ x1 x3)) (* d (+ y1 y3))) 
        g (* 2.0 (- (* a d) (* b c))))
  (if (not (equal g 0.0 1e-6)) 
    (progn 
      (setq cx (/ (- (* d e) (* b f)) g) cy (/ (- (* a f) (* c e)) g) r (distance (list cx cy) p1)) 
      (list cx cy r)
    ) 
    nil
  )
)

(defun make-arc-3p (p1 p2 p3 layer / cdata cx cy r ang1 ang2 ang3 diff1 diff2 startAng endAng)
  (check-make-layer layer)
  (setq cdata (get-arc-center p1 p2 p3))
  (if cdata 
    (progn
      (setq cx (car cdata) cy (cadr cdata) r (caddr cdata))
      (setq ang1 (angle (list cx cy) p1) ang2 (angle (list cx cy) p2) ang3 (angle (list cx cy) p3))
      (setq diff1 (- ang3 ang1)) (if (< diff1 0.0) (setq diff1 (+ diff1 (* 2.0 pi)))) 
      (setq diff2 (- ang2 ang1)) (if (< diff2 0.0) (setq diff2 (+ diff2 (* 2.0 pi))))
      (if (and (> diff2 0.0) (< diff2 diff1)) 
        (setq startAng ang1 endAng ang3) 
        (setq startAng ang3 endAng ang1)
      )
      (entmake (list '(0 . "ARC") (cons 8 layer) (cons 62 256) (cons 6 "BYLAYER") (cons 10 (list cx cy 0.0)) (cons 40 r) (cons 50 startAng) (cons 51 endAng)))
    )
  )
)

(defun make-ellipse-arc (cx cy rad sx sy sang eang lay / rx ry major_pt ratio sparam eparam)
  (setq rx (abs (* rad sx)))
  (setq ry (abs (* rad sy)))
  
  (if (equal rx ry 1e-6)
    (entmake (list '(0 . "ARC") (cons 8 lay) (cons 62 256) (cons 6 "BYLAYER") 
                   (cons 10 (list cx cy 0.0)) (cons 40 rx) 
                   (cons 50 sang) (cons 51 eang)))
    (progn
      (if (>= rx ry)
        (progn
          (setq major_pt (list rx 0.0 0.0))
          (setq ratio (/ ry rx))
          (setq sparam sang)
          (setq eparam eang)
        )
        (progn
          (setq major_pt (list 0.0 ry 0.0))
          (setq ratio (/ rx ry))
          (setq sparam (- sang (/ pi 2.0)))
          (setq eparam (- eang (/ pi 2.0)))
        )
      )
      
      (while (< sparam 0.0) (setq sparam (+ sparam (* 2.0 pi))))
      (while (>= sparam (* 2.0 pi)) (setq sparam (- sparam (* 2.0 pi))))
      (while (< eparam 0.0) (setq eparam (+ eparam (* 2.0 pi))))
      (while (>= eparam (* 2.0 pi)) (setq eparam (- eparam (* 2.0 pi))))
      
      (entmake (list
                 '(0 . "ELLIPSE")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDbEllipse")
                 (cons 8 lay)
                 (cons 62 256)
                 (cons 6 "BYLAYER")
                 (cons 10 (list cx cy 0.0))
                 (cons 11 major_pt)
                 (cons 40 ratio)
                 (cons 41 sparam)
                 (cons 42 eparam)
                 '(210 0.0 0.0 1.0)
               ))
    )
  )
)

;; ★ 라벨 함수 (스케일 적용 시 하단 고정, 위로 확장)
(defun draw-label-circle (bx by text str_layer cir_layer R txt_h stick_len draw_stick target_ss / cy_center)
  (if draw_stick
    (setq cy_center (+ by stick_len R)) 
    (setq cy_center (+ by R))           
  )
  
  (entmake (list '(0 . "CIRCLE") (cons 8 cir_layer) (cons 62 256) (cons 10 (list bx cy_center 0.0)) (cons 40 R)))
  (ssadd (entlast) target_ss)
  
  (entmake (list '(0 . "TEXT") (cons 8 str_layer) (cons 62 256) 
                 (cons 10 (list bx cy_center 0.0)) 
                 (cons 11 (list bx cy_center 0.0)) 
                 (cons 40 txt_h) 
                 (cons 1 text) 
                 '(72 . 1) '(73 . 2))) 
  (ssadd (entlast) target_ss)
  
  (if draw_stick
    (progn
      (entmake (list '(0 . "LINE") (cons 8 cir_layer) (cons 62 256)
                     (cons 10 (list bx by 0.0))                    
                     (cons 11 (list bx (+ by stick_len) 0.0))))    
      (ssadd (entlast) target_ss)
    )
  )
)

;; =========================================================================
;; ★ 3-2. 치수선 스타일 VLA 자동 주입기 
;; =========================================================================
(defun apply-yb-dim-style (ent textOverride textH / obj)
  (setq obj (vlax-ename->vla-object ent))
  
  (if (tblsearch "DIMSTYLE" "YDIM")
    (vl-catch-all-apply 'vla-put-StyleName (list obj "YDIM"))
  )
  
  (vl-catch-all-apply 'vla-put-Layer (list obj "CS-DIMS"))
  
  (vl-catch-all-apply 'vla-put-TextColor (list obj 3))            
  (vl-catch-all-apply 'vla-put-DimensionLineColor (list obj 1))   
  (vl-catch-all-apply 'vla-put-ExtensionLineColor (list obj 1))   
  
  (vl-catch-all-apply 'vla-put-LinearScaleFactor (list obj 1000.0)) 
  (vl-catch-all-apply 'vla-put-TextHeight (list obj textH))           
  (vl-catch-all-apply 'vla-put-ArrowheadSize (list obj (* textH 0.66)))        
  
  (vl-catch-all-apply 'vla-put-Arrowhead1Type (list obj 0))
  (vl-catch-all-apply 'vla-put-Arrowhead2Type (list obj 0))

  (if (and textOverride (/= textOverride ""))
    (vl-catch-all-apply 'vla-put-TextOverride (list obj textOverride))
  )
  (vl-catch-all-apply 'vla-put-TextInside (list obj :vlax-true))
  (vl-catch-all-apply 'vla-put-TextInsideAlign (list obj :vlax-false))
  
  (vl-catch-all-apply 'vla-put-PrimaryUnitsPrecision (list obj 0))  
  (vl-catch-all-apply 'vla-put-ExtensionLineExtend (list obj (* textH 0.5)))
  (vl-catch-all-apply 'vla-put-ExtensionLineOffset (list obj 0.0))
  (vl-catch-all-apply 'vla-put-TextGap (list obj (* textH 0.33)))
)

;; =========================================================================
;; ★ 4. UI 핸들러 
;; =========================================================================
(defun parse-d-prop-num (data key / val down front back vline nums)
  (setq val (get-data-val key data))
  (if val
    (progn
      (setq nums (flatten-nums (list val)))
      (setq down (if (> (length nums) 0) (car nums) 0.0))
      (setq front (if (> (length nums) 1) (cadr nums) 0.0))
      (setq back (if (> (length nums) 2) (caddr nums) front))
      (setq vline "0")
      (foreach item (if (listp val) val (list val))
        (if (eq item 'Y) (setq vline "1"))
      )
    )
    (setq down nil front nil back nil vline "0")
  )
  (list down front back vline)
)

(defun get-val-safely (key data / nums) 
  (setq nums (flatten-nums (list (get-data-val key data))))
  (if nums (car nums) 0.0)
)

(defun parse-num (str) 
  (if (or (null str) (= str "-") (= str "")) 0.0 (atof str))
)

(defun yb-set-d-tile (keyStr val)
  (if val (set_tile keyStr (rtos val 2 3)) (set_tile keyStr ""))
)

(defun yb-ui-sync ( / cIdxStr cIdx cItem cFile data dProps isA1A2 isChasedae)
  (setq cIdxStr (get_tile "pop"))
  (if (or (null cIdxStr) (= cIdxStr "")) (setq cIdx 0) (setq cIdx (atoi cIdxStr)))
  (setq cItem (nth cIdx *YB_CLOUD_MODULES*))
  
  (if cItem 
    (progn
      (setq isA1A2 (or (wcmatch (car cItem) "A1_*") (wcmatch (car cItem) "A2_*")))
      (setq isChasedae (wcmatch (car cItem) "*차세대*"))

      (if isA1A2
        (progn
          (set_tile "ecnt" "1") (mode_tile "ecnt" 1)
          (set_tile "tset" "0") (mode_tile "tset" 1)
          (setq *yb_ecnt* "1" *yb_tset* "0")
        )
        (progn
          (mode_tile "ecnt" 0)
          (if isChasedae
            (mode_tile "tset" 0)
            (progn (set_tile "tset" "0") (mode_tile "tset" 1))
          )
        )
      )
      
      (mode_tile "el" 0) (mode_tile "ehgap" 0) (mode_tile "ei" 0)
      (mode_tile "eghf" 0) (mode_tile "eghb" 0)
      (mode_tile "d1_d" 0) (mode_tile "d1_f" 0) (mode_tile "d1_b" 0) (mode_tile "d1_v" 0)
      (mode_tile "d2_d" 0) (mode_tile "d2_f" 0) (mode_tile "d2_b" 0) (mode_tile "d2_v" 0)
      (mode_tile "d3_d" 0) (mode_tile "d3_f" 0) (mode_tile "d3_b" 0) (mode_tile "d3_v" 0)
      (mode_tile "d4_d" 0) (mode_tile "d4_f" 0) (mode_tile "d4_b" 0) (mode_tile "d4_v" 0)
      (mode_tile "d5_d" 0) (mode_tile "d5_f" 0) (mode_tile "d5_b" 0) (mode_tile "d5_v" 0)
      (mode_tile "eh" 0) (mode_tile "em" 0)
      
      (setq cFile (cadr cItem))
      (setq data (cdr (assoc cFile *yb_db_cache*)))
      (if data 
        (progn
          (set_tile "el" (rtos (get-val-safely 'DEFAULT_L data) 2 3))
          (set_tile "ehgap" (rtos (get-val-safely 'HGAP data) 2 3))
          (set_tile "eghf" (rtos (get-val-safely 'GHF data) 2 3))
          (set_tile "eghb" (rtos (get-val-safely 'GHB data) 2 3))
          (set_tile "ei" (rtos (get-val-safely 'PILE_EMBED data) 2 3))
          
          (setq dProps (parse-d-prop-num data 'D1)) (yb-set-d-tile "d1_d" (nth 0 dProps)) (yb-set-d-tile "d1_f" (nth 1 dProps)) (yb-set-d-tile "d1_b" (nth 2 dProps)) (set_tile "d1_v" (nth 3 dProps))
          (setq dProps (parse-d-prop-num data 'D2)) (yb-set-d-tile "d2_d" (nth 0 dProps)) (yb-set-d-tile "d2_f" (nth 1 dProps)) (yb-set-d-tile "d2_b" (nth 2 dProps)) (set_tile "d2_v" (nth 3 dProps))
          (setq dProps (parse-d-prop-num data 'D3)) (yb-set-d-tile "d3_d" (nth 0 dProps)) (yb-set-d-tile "d3_f" (nth 1 dProps)) (yb-set-d-tile "d3_b" (nth 2 dProps)) (set_tile "d3_v" (nth 3 dProps))
          (setq dProps (parse-d-prop-num data 'D4)) (yb-set-d-tile "d4_d" (nth 0 dProps)) (yb-set-d-tile "d4_f" (nth 1 dProps)) (yb-set-d-tile "d4_b" (nth 2 dProps)) (set_tile "d4_v" (nth 3 dProps))
          (setq dProps (parse-d-prop-num data 'D5)) (yb-set-d-tile "d5_d" (nth 0 dProps)) (yb-set-d-tile "d5_f" (nth 1 dProps)) (yb-set-d-tile "d5_b" (nth 2 dProps)) (set_tile "d5_v" (nth 3 dProps))
          
          (set_tile "eh" (rtos (get-val-safely 'HAE data) 2 3))
          (set_tile "em" (rtos (get-val-safely 'HAM data) 2 3)) 
        )
      )
    )
  )
)

(defun yb-update-span-list (selIdx / p_idx idx item tname numStr sepPos prefix dispStr len)
  (start_list "ls")
  (setq p_idx 1 idx 0)
  (foreach item *yb_spans*
    (setq tname (car item) sepPos (vl-string-search "_" tname) prefix (if sepPos (substr tname 1 sepPos) tname))
    (if (wcmatch prefix "교각*") (progn (setq numStr (strcat "P" (itoa p_idx))) (setq p_idx (1+ p_idx))) (setq numStr prefix))
    
    (setq len (rtos (nth 1 item) 2 3))
    (setq dispStr (strcat numStr "\t" tname "\tL:" len "m"))
    (add_list dispStr)
    (setq idx (1+ idx))
  )
  (end_list)
  
  (set_tile "ls" "")
  (if (and selIdx (/= selIdx ""))
    (progn (setq *yb_ls_sel* (if (= (type selIdx) 'STR) selIdx (itoa selIdx))) (set_tile "ls" *yb_ls_sel*))
    (setq *yb_ls_sel* "")
  )
)

(defun yb-click-list ( / sIdxStr firstIdx selItem tIdx i m cItem d1 d2 d3 d4 d5 isA1A2 isChasedae)
  (setq sIdxStr *yb_ls_sel*)
  (if (and sIdxStr (/= sIdxStr "")) 
    (progn
      (setq firstIdx (car (yb-str->intlist sIdxStr))) 
      (setq selItem (nth firstIdx *yb_spans*))
      (if selItem 
        (progn
          (setq tIdx 0 i 0)
          (foreach m *YB_CLOUD_MODULES* (if (= (car m) (car selItem)) (setq tIdx i)) (setq i (1+ i)))
          (set_tile "pop" (itoa tIdx))
          
          (setq cItem (nth tIdx *YB_CLOUD_MODULES*))
          (setq isA1A2 (or (wcmatch (car cItem) "A1_*") (wcmatch (car cItem) "A2_*")))
          (setq isChasedae (wcmatch (car cItem) "*차세대*"))

          (if isA1A2
            (progn (mode_tile "ecnt" 1) (set_tile "tset" "0") (mode_tile "tset" 1))
            (progn (mode_tile "ecnt" 0) (if isChasedae (mode_tile "tset" 0) (progn (set_tile "tset" "0") (mode_tile "tset" 1))))
          )
          
          (set_tile "el" (rtos (nth 1 selItem) 2 3))
          (set_tile "ehgap" (rtos (nth 2 selItem) 2 3))
          (set_tile "eghf" (rtos (nth 3 selItem) 2 3))
          (set_tile "eghb" (rtos (nth 4 selItem) 2 3))
          
          (setq d1 (nth 5 selItem) d2 (nth 6 selItem) d3 (nth 7 selItem) d4 (nth 8 selItem) d5 (nth 9 selItem))
          (yb-set-d-tile "d1_d" (nth 0 d1)) (yb-set-d-tile "d1_f" (nth 1 d1)) (yb-set-d-tile "d1_b" (nth 2 d1)) (set_tile "d1_v" (nth 3 d1))
          (yb-set-d-tile "d2_d" (nth 0 d2)) (yb-set-d-tile "d2_f" (nth 1 d2)) (yb-set-d-tile "d2_b" (nth 2 d2)) (set_tile "d2_v" (nth 3 d2))
          (yb-set-d-tile "d3_d" (nth 0 d3)) (yb-set-d-tile "d3_f" (nth 1 d3)) (yb-set-d-tile "d3_b" (nth 2 d3)) (set_tile "d3_v" (nth 3 d3))
          (yb-set-d-tile "d4_d" (nth 0 d4)) (yb-set-d-tile "d4_f" (nth 1 d4)) (yb-set-d-tile "d4_b" (nth 2 d4)) (set_tile "d4_v" (nth 3 d4))
          (yb-set-d-tile "d5_d" (nth 0 d5)) (yb-set-d-tile "d5_f" (nth 1 d5)) (yb-set-d-tile "d5_b" (nth 2 d5)) (set_tile "d5_v" (nth 3 d5))
          
          (set_tile "eh" (rtos (nth 10 selItem) 2 3))
          (set_tile "em" (rtos (nth 11 selItem) 2 3))
          (set_tile "ei" (rtos (nth 12 selItem) 2 3))
        )
      )
    )
  )
)

(defun get-d-props (idx / d f b v)
  (setq d (get_tile (strcat "d" (itoa idx) "_d")))
  (setq f (get_tile (strcat "d" (itoa idx) "_f")))
  (setq b (get_tile (strcat "d" (itoa idx) "_b")))
  (setq v (get_tile (strcat "d" (itoa idx) "_v")))
  (list (if (or (= d "") (= d nil)) nil (atof d))
        (if (or (= f "") (= f nil)) nil (atof f))
        (if (or (= b "") (= b nil)) nil (atof b))
        v)
)

(defun yb-add-span ( / sIdx sType kName cFile len hgap ghf ghb d1 d2 d3 d4 d5 hae ham emb cnt k insert_list i tmp isSet fIdx bunhal_mod ilban_mod itemBunhal itemIlban sIdx_num targetLenStr mName data dbBunhal dbIlban d1B d2B d3B d4B d5B haeB hamB d1I d2I d3I d4I d5I haeI hamI)
  (setq sIdx *yb_ls_sel* sType (nth (atoi (get_tile "pop")) *YB_CLOUD_MODULES*) kName (car sType) cFile (cadr sType))
  (setq cnt (atoi (get_tile "ecnt"))) (if (< cnt 1) (setq cnt 1)) (setq isSet (get_tile "tset"))
  
  (setq len (parse-num (get_tile "el")) hgap (parse-num (get_tile "ehgap")))
  (setq ghf (parse-num (get_tile "eghf")) ghb (parse-num (get_tile "eghb")) emb (parse-num (get_tile "ei")))
  (setq d1 (get-d-props 1) d2 (get-d-props 2) d3 (get-d-props 3) d4 (get-d-props 4) d5 (get-d-props 5))
  (setq hae (parse-num (get_tile "eh")) ham (parse-num (get_tile "em")))
  
  (setq insert_list '())
  (if (= isSet "1") 
    (progn 
      (setq bunhal_mod nil ilban_mod nil targetLenStr (strcat "*" (itoa (fix len)) "m*"))
      (foreach m *YB_CLOUD_MODULES* (setq mName (car m))
        (if (and (null bunhal_mod) (wcmatch mName "교각_*") (wcmatch mName "*_분할*") (wcmatch mName targetLenStr)) (setq bunhal_mod m))
        (if (and (null ilban_mod) (wcmatch mName "교각_*") (wcmatch mName "*_일반*") (wcmatch mName targetLenStr)) (setq ilban_mod m))
      )
      
      (if (and bunhal_mod ilban_mod) 
        (progn
          (setq dbBunhal (cdr (assoc (cadr bunhal_mod) *yb_db_cache*)))
          (setq dbIlban (cdr (assoc (cadr ilban_mod) *yb_db_cache*)))
          
          (setq d1B (parse-d-prop-num dbBunhal 'D1) d2B (parse-d-prop-num dbBunhal 'D2) d3B (parse-d-prop-num dbBunhal 'D3) d4B (parse-d-prop-num dbBunhal 'D4) d5B (parse-d-prop-num dbBunhal 'D5))
          (setq haeB (get-val-safely 'HAE dbBunhal) hamB (get-val-safely 'HAM dbBunhal))
          
          (setq d1I (parse-d-prop-num dbIlban 'D1) d2I (parse-d-prop-num dbIlban 'D2) d3I (parse-d-prop-num dbIlban 'D3) d4I (parse-d-prop-num dbIlban 'D4) d5I (parse-d-prop-num dbIlban 'D5))
          (setq haeI (get-val-safely 'HAE dbIlban) hamI (get-val-safely 'HAM dbIlban))
          
          (setq itemBunhal (list (car bunhal_mod) len hgap ghf ghb d1B d2B d3B d4B d5B haeB hamB emb (cadr bunhal_mod)))
          (setq itemIlban (list (car ilban_mod) len hgap ghf ghb d1I d2I d3I d4I d5I haeI hamI emb (cadr ilban_mod)))
          
          (setq k 0) 
          (while (< k cnt) 
            (setq insert_list (append insert_list (list itemBunhal itemIlban itemIlban))) 
            (setq k (1+ k))
          )
        ) 
        (alert "세트 구성을 위한 '교각' 모듈을 찾을 수 없습니다.")
      )
    ) 
    (progn 
      (setq tmp (list kName len hgap ghf ghb d1 d2 d3 d4 d5 hae ham emb cFile) k 0) 
      (while (< k cnt) (setq insert_list (append insert_list (list tmp))) (setq k (1+ k)))
    )
  )
  
  (if (or (null sIdx) (= sIdx "")) 
    (progn 
      (setq *yb_spans* (append *yb_spans* insert_list))
      (setq fIdx (1- (length *yb_spans*)))
    ) 
    (progn 
      (setq sIdx_num (car (yb-str->intlist sIdx)) tmp '() i 0) 
      (foreach item *yb_spans* (setq tmp (append tmp (list item))) 
        (if (= i sIdx_num) (setq tmp (append tmp insert_list))) 
        (setq i (1+ i))
      )
      (setq *yb_spans* tmp)
      (setq fIdx (+ sIdx_num (length insert_list)))
    )
  )
  (setq *yb_ls_sel* (itoa fIdx)) 
  (yb-update-span-list *yb_ls_sel*)
)

(defun yb-mod-span ( / sIdxStr idxList sType kName cFile len hgap ghf ghb d1 d2 d3 d4 d5 hae ham emb tmp i data)
  (setq sIdxStr *yb_ls_sel*)
  (if (and sIdxStr (/= sIdxStr "")) 
    (progn 
      (setq idxList (yb-str->intlist sIdxStr) sType (nth (atoi (get_tile "pop")) *YB_CLOUD_MODULES*) kName (car sType) cFile (cadr sType))
      
      (setq len (parse-num (get_tile "el")) hgap (parse-num (get_tile "ehgap")))
      (setq ghf (parse-num (get_tile "eghf")) ghb (parse-num (get_tile "eghb")) emb (parse-num (get_tile "ei")))
      (setq d1 (get-d-props 1) d2 (get-d-props 2) d3 (get-d-props 3) d4 (get-d-props 4) d5 (get-d-props 5))
      (setq hae (parse-num (get_tile "eh")) ham (parse-num (get_tile "em")))
      
      (setq tmp '() i 0)
      (foreach item *yb_spans* (if (vl-position i idxList) 
          (setq tmp (append tmp (list (list kName len hgap ghf ghb d1 d2 d3 d4 d5 hae ham emb cFile)))) 
          (setq tmp (append tmp (list item)))
        ) 
        (setq i (1+ i))
      )
      (setq *yb_spans* tmp) (yb-update-span-list sIdxStr)
    )
  )
)

(defun yb-del-span ( / sIdxStr idxList tmp i fIdx)
  (setq sIdxStr *yb_ls_sel*)
  (if (and sIdxStr (/= sIdxStr "")) 
    (progn 
      (setq idxList (yb-str->intlist sIdxStr))
      (setq tmp '() i 0) 
      (foreach item *yb_spans* (if (not (vl-position i idxList)) (setq tmp (append tmp (list item)))) (setq i (1+ i))) 
      (setq *yb_spans* tmp)
      (setq fIdx (car idxList))
      (if (>= fIdx (length *yb_spans*)) (setq fIdx (1- (length *yb_spans*)))) 
      (if (< fIdx 0) (setq fIdx nil))
      (setq *yb_ls_sel* (if fIdx (itoa fIdx) ""))
      (yb-update-span-list *yb_ls_sel*)
    )
  )
)

(defun yb-save-vars () 
  (setq *yb_sc_x* (get_tile "eb_sc_x") *yb_sc_y* (get_tile "eb_sc_y") *yb_sc_a* (get_tile "eb_sc_a") *yb_off* (get_tile "eb_off") *yb_yoff* (get_tile "eb_yoff") *yb_hgap1* (get_tile "eb_hgap1") *yb_pop* (get_tile "pop") *yb_dim_txt* (get_tile "eb_dim_txt"))
  (setq *yb_el* (get_tile "el") *yb_ehgap* (get_tile "ehgap") *yb_ei* (get_tile "ei"))
  (setq *yb_eghf* (get_tile "eghf") *yb_eghb* (get_tile "eghb"))
  (setq *yb_d1_d* (get_tile "d1_d") *yb_d1_f* (get_tile "d1_f") *yb_d1_b* (get_tile "d1_b") *yb_d1_v* (get_tile "d1_v"))
  (setq *yb_d2_d* (get_tile "d2_d") *yb_d2_f* (get_tile "d2_f") *yb_d2_b* (get_tile "d2_b") *yb_d2_v* (get_tile "d2_v"))
  (setq *yb_d3_d* (get_tile "d3_d") *yb_d3_f* (get_tile "d3_f") *yb_d3_b* (get_tile "d3_b") *yb_d3_v* (get_tile "d3_v"))
  (setq *yb_d4_d* (get_tile "d4_d") *yb_d4_f* (get_tile "d4_f") *yb_d4_b* (get_tile "d4_b") *yb_d4_v* (get_tile "d4_v"))
  (setq *yb_d5_d* (get_tile "d5_d") *yb_d5_f* (get_tile "d5_f") *yb_d5_b* (get_tile "d5_b") *yb_d5_v* (get_tile "d5_v"))
  (setq *yb_eh* (get_tile "eh") *yb_em* (get_tile "em") *yb_ecnt* (get_tile "ecnt") *yb_tset* (get_tile "tset"))
)

;; =========================================================================
;; ★ 5. DCL 생성
;; =========================================================================
(defun make-ybridge-dcl ( / f dcl)
  (setq dcl (vl-filename-mktemp "yb" nil ".dcl")) 
  (setq f (open dcl "w"))
  
  (write-line "yb_dlg : dialog { label=\"교량 자동 작도기\"; width=100;" f)
  
  ;; ★ 교량명 입력칸 삭제됨
  (write-line " : boxed_row { label=\"기본 설정\"; : row { alignment=left; : text { value=\"X스케일:\"; width=6; fixed_width=true; } : edit_box { key=\"eb_sc_x\"; width=6; fixed_width=true; } : text { value=\"Y스케일:\"; width=6; fixed_width=true; } : edit_box { key=\"eb_sc_y\"; width=6; fixed_width=true; } : text { value=\"주석스케일:\"; width=12; fixed_width=true; } : edit_box { key=\"eb_sc_a\"; width=6; fixed_width=true; } : text { value=\"하단문구:\"; width=8; fixed_width=true; } : edit_box { key=\"eb_dim_txt\"; width=12; fixed_width=true; } : spacer {width=5; fixed_width=true;} : button { key=\"btn_reload\"; label=\"DB 새로고침\"; fixed_width=true; } } }" f)
  
  (write-line " : boxed_row { label=\"위치 및 선형 설정\";" f)
  (write-line "  : row { alignment=left;" f)
  (write-line "   : column { alignment=left;" f)
  (write-line "     : row { : button { key=\"b1\"; label=\"종단 선형 선택\"; width=16; fixed_width=true; } : text { key=\"t1\"; value=\"선택 안됨\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : button { key=\"b2\"; label=\"시작점 선택\"; width=16; fixed_width=true; } : text { key=\"t2\"; value=\"선택 안됨\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : text { value=\"시작 이격거리 [m]:\"; width=16; fixed_width=true; } : edit_box { key=\"eb_off\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : text { value=\"상하 이격거리 [m]:\"; width=16; fixed_width=true; } : edit_box { key=\"eb_yoff\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : text { value=\"보정값1 (일괄 / 기둥 높이 기준점 ~ 기초 상단) [m]:\"; width=45; fixed_width=true; } : edit_box { key=\"eb_hgap1\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "   }" f)
  (write-line "   : spacer { width=4; }" f)
  (write-line "   (write-line "   : column { alignment=top;" f)
  (write-line "     : row { : button { key=\"b3\"; label=\"지반선 선택\"; width=16; fixed_width=true; } : text { key=\"t3\"; value=\"선택 안됨\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : button { key=\"b4\"; label=\"기반암 선택\"; width=16; fixed_width=true; } : text { key=\"t4\"; value=\"선택 안됨\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : button { key=\"brst\"; label=\"선택 초기화\"; width=16; fixed_width=true; } : spacer {} }" f)
  (write-line "     : spacer { }" f)
  (write-line "   }" f)
  (write-line "   : spacer {} " f)
  (write-line "  }" f)
  (write-line " }" f)
  
  (write-line " : boxed_column { label=\"경간 구성표\";" f)
  (write-line "  : row { alignment=left; : text { value=\"종류:\"; width=5; fixed_width=true; } : popup_list { key=\"pop\"; width=45; fixed_width=true; } : spacer {} }" f)
  
  (write-line "  : row { alignment=left;" f)
  (write-line "    : text { value=\"경간 [m]:\"; width=8; fixed_width=true; } : edit_box { key=\"el\"; width=6; fixed_width=true; } : spacer { width=4; }" f)
  (write-line "    : text { value=\"보정값2 (개별 / 기둥 높이 기준점 ~ 기초 상단) [m]:\"; width=45; fixed_width=true; } : edit_box { key=\"ehgap\"; width=6; fixed_width=true; } : spacer { width=4; }" f)
  (write-line "    : text { value=\"말뚝 근입 깊이 [m]:\"; width=18; fixed_width=true; } : edit_box { key=\"ei\"; width=6; fixed_width=true; }" f)
  (write-line "    : spacer {} " f) 
  (write-line "  }" f)

  (write-line "  : row { alignment=left;" f)
  (write-line "    : text { value=\"전 거더 높이 [m] (※ 거더 전용):\"; width=35; fixed_width=true; } : edit_box { key=\"eghf\"; width=6; fixed_width=true; } : spacer { width=4; }" f)
  (write-line "    : text { value=\"후 거더 높이 [m] (※ 거더 전용):\"; width=35; fixed_width=true; } : edit_box { key=\"eghb\"; width=6; fixed_width=true; } : spacer {}" f)
  (write-line "  }" f)

  (write-line "  : boxed_column { label=\"D1~D5 상세 설정\";" f)
  (write-line "    : row { alignment=left;" f)
  (write-line "      : column { : text{value=\"구분\"; width=4; fixed_width=true;} : text{value=\"D1:\"; height=1.45;} : text{value=\"D2:\"; height=1.45;} : text{value=\"D3:\"; height=1.45;} : text{value=\"D4:\"; height=1.45;} : text{value=\"D5:\"; height=1.45;} }" f)
  (write-line "      : spacer {width=4;}" f)
  (write-line "      : column { : text{value=\"수직 내림 [m]\"; width=15; fixed_width=true;} : edit_box{key=\"d1_d\"; width=8; fixed_width=true;} : edit_box{key=\"d2_d\"; width=8; fixed_width=true;} : edit_box{key=\"d3_d\"; width=8; fixed_width=true;} : edit_box{key=\"d4_d\"; width=8; fixed_width=true;} : edit_box{key=\"d5_d\"; width=8; fixed_width=true;} }" f)
  (write-line "      : spacer {width=4;}" f)
  (write-line "      : column { : text{value=\"전 이격거리 [m]\"; width=15; fixed_width=true;} : edit_box{key=\"d1_f\"; width=8; fixed_width=true;} : edit_box{key=\"d2_f\"; width=8; fixed_width=true;} : edit_box{key=\"d3_f\"; width=8; fixed_width=true;} : edit_box{key=\"d4_f\"; width=8; fixed_width=true;} : edit_box{key=\"d5_f\"; width=8; fixed_width=true;} }" f)
  (write-line "      : spacer {width=4;}" f)
  (write-line "      : column { : text{value=\"후 이격거리 [m]\"; width=15; fixed_width=true;} : edit_box{key=\"d1_b\"; width=8; fixed_width=true;} : edit_box{key=\"d2_b\"; width=8; fixed_width=true;} : edit_box{key=\"d3_b\"; width=8; fixed_width=true;} : edit_box{key=\"d4_b\"; width=8; fixed_width=true;} : edit_box{key=\"d5_b\"; width=8; fixed_width=true;} }" f)
  (write-line "      : spacer {width=4;}" f)
  (write-line "      : column { alignment=center; : text{value=\"세로선\"; width=8; fixed_width=true;} : toggle{key=\"d1_v\";} : toggle{key=\"d2_v\";} : toggle{key=\"d3_v\";} : toggle{key=\"d4_v\";} : toggle{key=\"d5_v\";} }" f)
  (write-line "      : spacer {}" f)
  (write-line "    }" f)
  (write-line "  }" f)

  (write-line "  : row { alignment=left;" f)
  (write-line "    : text { value=\"아치 양단 [m] (※차세대라멘 전용):\"; width=35; fixed_width=true; } : edit_box { key=\"eh\"; width=6; fixed_width=true; } : spacer { width=4; }" f)
  (write-line "    : text { value=\"아치 중간 [m] (※차세대라멘 전용):\"; width=35; fixed_width=true; } : edit_box { key=\"em\"; width=6; fixed_width=true; }" f)
  (write-line "    : spacer {}" f)
  (write-line "  }" f)

  (write-line "  : row { alignment=left; : text { value=\"추가 개수:\"; width=10; fixed_width=true; } : edit_box { key=\"ecnt\"; width=4; fixed_width=true; value=\"1\"; } : spacer { width=3; }" f)
  (write-line "           : toggle { key=\"tset\"; label=\"세트 (분할+일반+일반 , ※차세대라멘 전용)\"; width=45; fixed_width=true; } : spacer { width=5; }" f)
  (write-line "           : button { key=\"ba\"; label=\"추가\"; width=8; fixed_width=true; } : spacer {width=1;} : button { key=\"bm\"; label=\"수정\"; width=8; fixed_width=true; } : spacer {width=1;} : button { key=\"bd\"; label=\"삭제\"; width=8; fixed_width=true; }" f)
  (write-line "           : spacer {}" f)
  (write-line "  }" f)
  
  (write-line "  : list_box { key=\"ls\"; height=12; tabs=\"8 50\"; multiple_select = true; }" f)
  
  (write-line " }" f)
  (write-line " ok_cancel; }" f) 
  (close f) 
  dcl
)

;; =========================================================================
;; ★ 6. 구조 작도 엔진
;; =========================================================================

(defun yb-point-in-box (px py box)
  (if (and px py box (>= (length box) 4))
    (and (>= px (- (nth 0 box) 1e-4))
         (<= px (+ (nth 2 box) 1e-4))
         (>= py (- (nth 1 box) 1e-4))
         (<= py (+ (nth 3 box) 1e-4)))
    nil
  )
)

(defun yb-obj-fully-in-box (obj box / cx cy r sang eang sx sy ex ey)
  (cond
    ((= (car obj) "L")
     (and (yb-point-in-box (if (nth 2 obj) (nth 2 obj) 0.0) (if (nth 3 obj) (nth 3 obj) 0.0) box)
          (yb-point-in-box (if (nth 4 obj) (nth 4 obj) 0.0) (if (nth 5 obj) (nth 5 obj) 0.0) box)))
    ((= (car obj) "A")
     (setq cx (if (nth 2 obj) (nth 2 obj) 0.0) cy (if (nth 3 obj) (nth 3 obj) 0.0) r (if (nth 4 obj) (nth 4 obj) 0.0) sang (if (nth 5 obj) (nth 5 obj) 0.0) eang (if (nth 6 obj) (nth 6 obj) 0.0))
     (setq sx (+ cx (* r (cos sang))) sy (+ cy (* r (sin sang))))
     (setq ex (+ cx (* r (cos eang))) ey (+ cy (* r (sin eang))))
     (and (yb-point-in-box cx cy box)
          (yb-point-in-box sx sy box)
          (yb-point-in-box ex ey box)))
    (t nil)
  )
)

(defun yb-fuzzy-equal (a b)
  (if (and (numberp a) (numberp b))
    (< (abs (- a b)) 1e-4)
    (equal a b)
  )
)

(defun yb-fuzzy-equal-obj (obj1 obj2 / match idx)
  (if (= (length obj1) (length obj2))
    (progn
      (setq match t idx 0)
      (while (< idx (length obj1))
        (if (not (yb-fuzzy-equal (nth idx obj1) (nth idx obj2)))
          (setq match nil)
        )
        (setq idx (1+ idx))
      )
      match
    )
    nil
  )
)

(defun yb-obj-in-list (obj obj_lst / exist item)
  (setq exist nil)
  (foreach item obj_lst
    (if (yb-fuzzy-equal-obj obj item) (setq exist t))
  )
  exist
)

(defun yb-get-point-shift (px py obj box_data shift_val / box obj_lst flat_box)
  (if box_data
    (progn
      (setq flat_box (flatten-nums (list (car box_data))))
      (setq box (if (>= (length flat_box) 4) flat_box (list 0.0 0.0 0.0 0.0)))
      (setq obj_lst (cdr box_data))
      
      (if (yb-point-in-box px py box)
        (if (or (yb-obj-fully-in-box obj box)
                (yb-obj-in-list obj obj_lst))
          (if shift_val shift_val 0.0)
          0.0
        )
        0.0
      )
    )
    0.0
  )
)

(defun yb-get-shift (px py obj s_max s_front s_back h_max_val ghf_val ghb_val / s1 s2 s3)
  (setq s1 (yb-get-point-shift px py obj s_max h_max_val))
  (setq s2 (yb-get-point-shift px py obj s_front ghf_val))
  (setq s3 (yb-get-point-shift px py obj s_back ghb_val))
  (cond
    ((and s1 (> s1 0.0)) s1)
    ((and s2 (> s2 0.0)) s2)
    ((and s3 (> s3 0.0)) s3)
    (t 0.0)
  )
)

(defun draw-cloud-structure (insPt draw_lines strD_GL strD_BR mirror cad_sc_x cad_sc_y limitGL limitBR ghf ghb s_front s_back s_max / 
                              baseX baseY oType lay unX1 unY1 unX2 unY2 unCX unCY rad_unscaled sang eang 
                              shift1 shift2 shiftC h_max_val ghf_val ghb_val
                              x1 y1 x2 y2 absX1 absX2 absY1 absY2 absCX absCY temp_sang temp_eang
                              sGL1 sGL2 sBR1 sBR2)
  (setq baseX (if (car insPt) (car insPt) 0.0))
  (setq baseY (if (cadr insPt) (cadr insPt) 0.0))
  
  (setq ghf_val (* (if ghf ghf 0.0) cad_sc_y))
  (setq ghb_val (* (if ghb ghb 0.0) cad_sc_y))
  (setq h_max_val (max ghf_val ghb_val))

  (if draw_lines
    (foreach item draw_lines
      (setq oType (car item))
      (setq lay (cadr item)) 
      (if (not lay) (setq lay "0"))
      (check-make-layer lay)
      (cond
        ((= oType "L") 
          (setq unX1 (if (nth 2 item) (nth 2 item) 0.0))
          (setq unY1 (if (nth 3 item) (nth 3 item) 0.0))
          (setq unX2 (if (nth 4 item) (nth 4 item) 0.0))
          (setq unY2 (if (nth 5 item) (nth 5 item) 0.0))
          
          (setq shift1 (yb-get-shift unX1 unY1 item s_max s_front s_back h_max_val ghf_val ghb_val))
          (setq shift2 (yb-get-shift unX2 unY2 item s_max s_front s_back h_max_val ghf_val ghb_val))
          
          (setq y1 (- (* unY1 cad_sc_y) shift1))
          (setq y2 (- (* unY2 cad_sc_y) shift2))
          
          (setq x1 (* mirror (* unX1 cad_sc_x)))
          (setq x2 (* mirror (* unX2 cad_sc_x)))
          
          (setq absX1 (+ baseX x1))
          (setq absX2 (+ baseX x2))
          (setq absY1 (+ baseY y1))
          (setq absY2 (+ baseY y2))
          
          (setq sGL1 0.0 sBR1 0.0 sGL2 0.0 sBR2 0.0)
          
          (if (and limitGL (<= (* unY1 cad_sc_y) (+ limitGL 1e-4))) (setq sGL1 strD_GL))
          (if (and limitBR (<= (* unY1 cad_sc_y) (+ limitBR 1e-4))) (setq sBR1 strD_BR sGL1 strD_GL))
          
          (if (and limitGL (<= (* unY2 cad_sc_y) (+ limitGL 1e-4))) (setq sGL2 strD_GL))
          (if (and limitBR (<= (* unY2 cad_sc_y) (+ limitBR 1e-4))) (setq sBR2 strD_BR sGL2 strD_GL))
          
          (if (and (> (abs sBR1) 1e-5) (> (abs sBR2) 1e-5))
            (cond
              ((and (< (abs (- unX1 unX2)) 1e-3) (> (abs (- unY1 unY2)) 1e-3))
                (if (< unY1 unY2)
                  (setq sBR2 0.0)
                  (setq sBR1 0.0)
                )
              )
            )
          )
          
          (setq absY1 (- absY1 (+ sGL1 sBR1)))
          (setq absY2 (- absY2 (+ sGL2 sBR2)))
          
          (entmake (list '(0 . "LINE") (cons 8 lay) (cons 62 256) (cons 6 "BYLAYER") (cons 10 (list absX1 absY1 0.0)) (cons 11 (list absX2 absY2 0.0))))
        )
        
        ((= oType "GAP_L")
          (setq unX1 (if (nth 2 item) (nth 2 item) 0.0))
          (setq unY1 (if (nth 3 item) (nth 3 item) 0.0))
          (setq unX2 (if (nth 4 item) (nth 4 item) 0.0))
          (setq unY2 (if (nth 5 item) (nth 5 item) 0.0))
          
          (setq min_gh (min ghf_val ghb_val))
          (setq max_gh (max ghf_val ghb_val))

          (if (>= unY1 unY2)
            (progn (setq topY unY1 botY unY2)) 
            (progn (setq topY unY2 botY unY1)) 
          )

          (setq y1 (- (* topY cad_sc_y) min_gh))
          (setq y2 (- (* botY cad_sc_y) max_gh))
          
          (setq x1 (* mirror (* unX1 cad_sc_x)))
          (setq x2 (* mirror (* unX2 cad_sc_x)))

          (setq absX1 (+ baseX x1))
          (setq absX2 (+ baseX x2))
          (setq absY1 (+ baseY y1))
          (setq absY2 (+ baseY y2))
          
          (entmake (list '(0 . "LINE") (cons 8 lay) (cons 62 256) (cons 6 "BYLAYER") (cons 10 (list absX1 absY1 0.0)) (cons 11 (list absX2 absY2 0.0))))
        )
        
        ((= oType "A")
          (setq unCX (if (nth 2 item) (nth 2 item) 0.0))
          (setq unCY (if (nth 3 item) (nth 3 item) 0.0))
          (setq rad_unscaled (if (nth 4 item) (nth 4 item) 0.0))
          (setq sang (if (nth 5 item) (nth 5 item) 0.0))
          (setq eang (if (nth 6 item) (nth 6 item) 0.0))
          
          (setq shiftC (yb-get-shift unCX unCY item s_max s_front s_back h_max_val ghf_val ghb_val))
          
          (setq cy (- (* unCY cad_sc_y) shiftC))
          (setq cx (* mirror (* unCX cad_sc_x)))
          
          (if (= mirror -1.0)
            (progn
              (setq temp_sang (- pi eang))
              (setq temp_eang (- pi sang))
              (while (< temp_sang 0.0) (setq temp_sang (+ temp_sang (* 2.0 pi))))
              (while (< temp_eang 0.0) (setq temp_eang (+ temp_eang (* 2.0 pi))))
              (setq sang temp_sang)
              (setq eang temp_eang)
            )
          )
          
          (setq absCX (+ baseX cx))
          (setq absCY (+ baseY cy))
          
          (if (and limitBR (<= (* unCY cad_sc_y) limitBR)) 
            (setq absCY (- absCY (+ strD_GL strD_BR))) 
            (if (and limitGL (<= (* unCY cad_sc_y) limitGL)) (setq absCY (- absCY strD_GL)))
          )
          
          (make-ellipse-arc absCX absCY rad_unscaled cad_sc_x cad_sc_y sang eang lay)
        )
      )
    )
  )
)

(defun copy-and-trim-profile (curveObj startX endX offsetY lay / newObj en l1 l2 ptLeft ptRight old_os old_ce old_edge minX maxX cy testYL testYR rawY)
  (check-make-layer lay) 
  (setq old_os (getvar "OSMODE") old_ce (getvar "CMDECHO") old_edge (getvar "EDGEMODE")) 
  (setvar "OSMODE" 0) (setvar "CMDECHO" 0) (setvar "EDGEMODE" 1)
  
  (setq newObj (vla-copy curveObj)) 
  (vla-Move newObj (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point (list 0.0 (- offsetY) 0.0)))
  (vla-put-Layer newObj lay) 
  
  (vla-put-Color newObj 256) 
  (vl-catch-all-apply 'vla-put-Linetype (list newObj "ByLayer"))
  (vla-put-Visible newObj :vlax-true) 
  
  (setq en (vlax-vla-object->ename newObj))
  
  (entmake (list '(0 . "LINE") (cons 10 (list startX -1e6 0.0)) (cons 11 (list startX 1e6 0.0)))) 
  (setq l1 (entlast))
  (entmake (list '(0 . "LINE") (cons 10 (list endX -1e6 0.0)) (cons 11 (list endX 1e6 0.0)))) 
  (setq l2 (entlast))
  
  (setq rawY (get-y-on-curve curveObj (+ startX (/ (- endX startX) 2.0))))
  (setq cy (if rawY (- rawY offsetY) 0.0))
  
  (setq testYL (get-y-on-curve curveObj (- startX 0.1)))
  (setq testYR (get-y-on-curve curveObj (+ endX 0.1)))
  
  (setq ptLeft  (list (- startX 2.0) (if testYL (- testYL offsetY) cy) 0.0))
  (setq ptRight (list (+ endX 2.0) (if testYR (- testYR offsetY) cy) 0.0))

  (command "._VIEW" "_S" "temp_yb_view")
  (setq minX (- startX 5.0) maxX (+ endX 5.0)) 
  (command "._ZOOM" "_W" (list minX (- cy 20.0) 0.0) (list maxX (+ cy 20.0) 0.0))
  
  (if (and *yb_curve* (not (vlax-erased-p *yb_curve*))) (vla-put-Visible *yb_curve* :vlax-false))
  (if (and *yb_gl* (not (vlax-erased-p *yb_gl*))) (vla-put-Visible *yb_gl* :vlax-false))
  (if (and *yb_br* (not (vlax-erased-p *yb_br*))) (vla-put-Visible *yb_br* :vlax-false))
  (if (not (vlax-erased-p curveObj)) (vla-put-Visible curveObj :vlax-true))
  
  (command "._TRIM" l1 l2 "")
  (if testYL (command (list en ptLeft)))
  (if testYR (command (list en ptRight)))
  (command "")
  
  (if (and *yb_curve* (not (vlax-erased-p *yb_curve*))) (vla-put-Visible *yb_curve* :vlax-true))
  (if (and *yb_gl* (not (vlax-erased-p *yb_gl*))) (vla-put-Visible *yb_gl* :vlax-true))
  (if (and *yb_br* (not (vlax-erased-p *yb_br*))) (vla-put-Visible *yb_br* :vlax-true))
  (if (not (vlax-erased-p curveObj)) (vla-put-Visible curveObj :vlax-true))
  
  (entdel l1) 
  (entdel l2) 
  
  (command "._VIEW" "_R" "temp_yb_view") 
  (command "._VIEW" "_D" "temp_yb_view")
  (setvar "OSMODE" old_os) 
  (setvar "CMDECHO" old_ce) 
  (setvar "EDGEMODE" old_edge)
)

(defun draw-single-layer (curveObj curX nextX dimA dimB mirrorA mirrorB cad_sc_x cad_sc_y lay prevDimA prevDimB / 
                          downA frontA backA vlineA downB frontB backB vlineB 
                          prevDownA prevDownB prevOffY_A prevOffY_B
                          offA offB startX endX offsetY yL yR)
  (setq downA (nth 0 dimA) frontA (nth 1 dimA) backA (nth 2 dimA) vlineA (nth 3 dimA))
  (setq downB (nth 0 dimB) frontB (nth 1 dimB) backB (nth 2 dimB) vlineB (nth 3 dimB))
  
  (setq prevDownA (if (and prevDimA (nth 0 prevDimA)) (nth 0 prevDimA) 0.0))
  (setq prevDownB (if (and prevDimB (nth 0 prevDimB)) (nth 0 prevDimB) 0.0))
  (setq prevOffY_A (* prevDownA cad_sc_y))
  (setq prevOffY_B (* prevDownB cad_sc_y))

  (if (not (and (null downA) (null downB)))
    (progn
      (if (null downA) (setq downA 0.0 frontA 0.0 backA 0.0 vlineA "0"))
      (if (null downB) (setq downB 0.0 frontB 0.0 backB 0.0 vlineB "0"))
      (if (null frontA) (setq frontA 0.0))
      (if (null backA) (setq backA 0.0))
      (if (null frontB) (setq frontB 0.0))
      (if (null backB) (setq backB 0.0))

      (setq offA (if (= mirrorA -1.0) (* frontA -1.0) backA))
      (setq offB (if (= mirrorB -1.0) (* backB -1.0) frontB))

      (setq startX (+ curX (* offA cad_sc_x)))
      (setq endX (+ nextX (* offB cad_sc_x)))

      (setq offsetY (* downA cad_sc_y))

      (copy-and-trim-profile curveObj startX endX offsetY lay)
      
      (if (= vlineA "1")
        (progn
          (setq yL (get-y-on-curve curveObj startX))
          (if yL (entmake (list '(0 . "LINE") (cons 8 lay) (cons 62 256) (cons 6 "BYLAYER") (cons 10 (list startX (- yL offsetY) 0.0)) (cons 11 (list startX (- yL prevOffY_A) 0.0)))))
        )
      )
      
      (if (= vlineB "1")
        (progn
          (setq yR (get-y-on-curve curveObj endX))
          (if yR (entmake (list '(0 . "LINE") (cons 8 lay) (cons 62 256) (cons 6 "BYLAYER") (cons 10 (list endX (- yR offsetY) 0.0)) (cons 11 (list endX (- yR prevOffY_B) 0.0)))))
        )
      )
    )
  )
)

(defun get-hae-offset (data isPierA mirror / nums front back off)
  (setq nums (flatten-nums (list (get-data-val 'HAE data))))
  (if (and nums (> (length nums) 1))
    (progn
      (setq front (nth 1 nums))
      (setq back (if (> (length nums) 2) (nth 2 nums) front))
      (if isPierA
        (setq off (if (= mirror -1.0) (* front -1.0) back))
        (setq off (if (= mirror -1.0) (* back -1.0) front))
      )
    )
    (setq off 0.0)
  )
  off
)

(defun draw-upper-structure (curveObj curX dim1A dim2A dim3A dim4A dim5A mirrorA nextX dim1B dim2B dim3B dim4B dim5B mirrorB cad_sc_x cad_sc_y eh em dataA dataB / archOffA archOffB archX_A archX_B midX yFaceA yFaceB yMid yArc1 yArc2 lay lastValidA lastValidB)
  (setq lay "CS-CONC-MAJR") 
  (check-make-layer lay)
  
  (setq lastValidA nil lastValidB nil)
  
  (draw-single-layer curveObj curX nextX dim1A dim1B mirrorA mirrorB cad_sc_x cad_sc_y lay lastValidA lastValidB)
  (if (and dim1A (nth 0 dim1A)) (setq lastValidA dim1A))
  (if (and dim1B (nth 0 dim1B)) (setq lastValidB dim1B))
  
  (draw-single-layer curveObj curX nextX dim2A dim2B mirrorA mirrorB cad_sc_x cad_sc_y lay lastValidA lastValidB)
  (if (and dim2A (nth 0 dim2A)) (setq lastValidA dim2A))
  (if (and dim2B (nth 0 dim2B)) (setq lastValidB dim2B))
  
  (draw-single-layer curveObj curX nextX dim3A dim3B mirrorA mirrorB cad_sc_x cad_sc_y lay lastValidA lastValidB)
  (if (and dim3A (nth 0 dim3A)) (setq lastValidA dim3A))
  (if (and dim3B (nth 0 dim3B)) (setq lastValidB dim3B))
  
  (draw-single-layer curveObj curX nextX dim4A dim4B mirrorA mirrorB cad_sc_x cad_sc_y lay lastValidA lastValidB)
  (if (and dim4A (nth 0 dim4A)) (setq lastValidA dim4A))
  (if (and dim4B (nth 0 dim4B)) (setq lastValidB dim4B))
  
  (draw-single-layer curveObj curX nextX dim5A dim5B mirrorA mirrorB cad_sc_x cad_sc_y lay lastValidA lastValidB)
  
  (setq archOffA (get-hae-offset dataA T mirrorA))
  (if (and (= archOffA 0.0) dim2A (nth 0 dim2A) (> (nth 0 dim2A) 0))
      (setq archOffA (if (= mirrorA -1.0) (* (if (nth 1 dim2A) (nth 1 dim2A) 0.0) -1.0) (if (nth 2 dim2A) (nth 2 dim2A) 0.0))))

  (setq archOffB (get-hae-offset dataB nil mirrorB))
  (if (and (= archOffB 0.0) dim2B (nth 0 dim2B) (> (nth 0 dim2B) 0))
      (setq archOffB (if (= mirrorB -1.0) (* (if (nth 2 dim2B) (nth 2 dim2B) 0.0) -1.0) (if (nth 1 dim2B) (nth 1 dim2B) 0.0))))

  (setq archX_A (+ curX (* archOffA cad_sc_x)))
  (setq archX_B (+ nextX (* archOffB cad_sc_x)))
  (setq midX (/ (+ archX_A archX_B) 2.0))
  
  (setq yFaceA (get-y-on-curve curveObj archX_A))
  (setq yFaceB (get-y-on-curve curveObj archX_B))
  (setq yMid (get-y-on-curve curveObj midX))

  (if (and yFaceA yFaceB yMid (> eh 0) (> em 0)) 
    (progn
      (setq yArc1 (* eh cad_sc_y))
      (setq yArc2 (* em cad_sc_y))
      (make-arc-3p (list archX_A (- yFaceA yArc1) 0.0) (list midX (- yMid yArc2) 0.0) (list archX_B (- yFaceB yArc1) 0.0) lay)
    )
  )
)

;; =========================================================================
;; ★ 7. 메인 명령어 로직
;; =========================================================================
(defun c:YBRIDGE ( / doc dcl_id what_next ss pt currX i item sType spanLen db data cFile mirror currY oldCmd yOffCAD temp_curve hgap hgap_val
                     dim1A dim2A dim3A dim4A dim5A eh em ei ghf ghb nextType offCAD groundY limitGL limitGL_Y2 limitBR pilesX strD_GL strD_BR max_str curr_limit_Y curr_br_Y absX brY req_str targetY
                     lastEnt ssSub ssSup ssBridge ssDim blkPrefix blkCnt bName mName dName nextItem nextFile nextData nextMirror cPt numsGL fTopLineData fTopHalfW fTopY H1_current H_req factor ceilFactor H1_new px
                     pile_lines draw_lines gap_lines pile_bottom_Y y_a y_b min_y br_module s_front s_back s_max curr_actual_pile_tip_Y tmp_ghf min_target_Y x1 y1 x2 y2 ptx loop hgap1 hgap1_val diff_Y
                     cad_sc_x cad_sc_y anno_sc oldLayer pt1 pt2 textPt dimY startX_total base_dim_Y extY dimY_total bridge_finished endX_total
                     default_th dim_scale min_max_h span_model span_m max_h final_text_h old_dimlunit old_dimtix old_dimdec dim_text_1st pier_cnt span_cnt Cy_P Cy_S R_cir lbl span_lbl midX midY lbl_txt_h lbl_stick
                     j_idx tmp_item span_list group_str cur_val cur_cnt val L_total dim_txt str1 str2 str3 txt_h line_Y start_X line2_X line2_Y rot_rad vx vy ux uy t_base_X t_base_Y t1_X t1_Y t2_X t2_Y)
  
  (if (<= (fix (getvar "CDATE")) 20271231)
    (progn
      (setq oldCmd (getvar "CMDECHO"))
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))) 
      (setvar "CMDECHO" 0)
      
      (if (null *YB_CLOUD_MODULES*) (setq *YB_CLOUD_MODULES* (get-cloud-init))) 
      (if (null *YB_CLOUD_MODULES*) 
        (progn (alert "데이터 로드 실패") (exit))
      )
      
      (setq dcl_id (load_dialog (make-ybridge-dcl)))
      (setq what_next 5)
      (setq next_focus "b1") 
      
      (while (>= what_next 2)
        (if (not (new_dialog "yb_dlg" dcl_id)) (exit))
        
        (set_tile "eb_sc_x" *yb_sc_x*) 
        (set_tile "eb_sc_y" *yb_sc_y*) 
        (set_tile "eb_sc_a" *yb_sc_a*)
        (set_tile "eb_dim_txt" *yb_dim_txt*)
        (set_tile "eb_off" *yb_off*)
        (set_tile "eb_yoff" *yb_yoff*)
        (set_tile "eb_hgap1" *yb_hgap1*)
        
        (start_list "pop") 
        (foreach m *YB_CLOUD_MODULES* (add_list (car m))) 
        (end_list)
        
        (if (= what_next 5) 
          (progn 
            (set_tile "pop" "0") 
            (yb-ui-sync) 
            (setq *yb_ls_sel* "")
          )
          (progn 
            (set_tile "pop" *yb_pop*) (set_tile "el" *yb_el*) (set_tile "ehgap" *yb_ehgap*) (set_tile "ei" *yb_ei*)
            (set_tile "eghf" *yb_eghf*) (set_tile "eghb" *yb_eghb*)
            (set_tile "d1_d" *yb_d1_d*) (set_tile "d1_f" *yb_d1_f*) (set_tile "d1_b" *yb_d1_b*) (set_tile "d1_v" *yb_d1_v*)
            (set_tile "d2_d" *yb_d2_d*) (set_tile "d2_f" *yb_d2_f*) (set_tile "d2_b" *yb_d2_b*) (set_tile "d2_v" *yb_d2_v*)
            (set_tile "d3_d" *yb_d3_d*) (set_tile "d3_f" *yb_d3_f*) (set_tile "d3_b" *yb_d3_b*) (set_tile "d3_v" *yb_d3_v*)
            (set_tile "d4_d" *yb_d4_d*) (set_tile "d4_f" *yb_d4_f*) (set_tile "d4_b" *yb_d4_b*) (set_tile "d4_v" *yb_d4_v*)
            (set_tile "d5_d" *yb_d5_d*) (set_tile "d5_f" *yb_d5_f*) (set_tile "d5_b" *yb_d5_b*) (set_tile "d5_v" *yb_d5_v*)
            (set_tile "eh" *yb_eh*) (set_tile "em" *yb_em*) (set_tile "ecnt" *yb_ecnt*) (set_tile "tset" *yb_tset*)
            
            (setq cItem (nth (atoi *yb_pop*) *YB_CLOUD_MODULES*))
            (if cItem
              (progn
                (if (or (wcmatch (car cItem) "A1_*") (wcmatch (car cItem) "A2_*"))
                  (progn 
                    (mode_tile "ecnt" 1)
                    (set_tile "tset" "0") (mode_tile "tset" 1)
                  )
                  (progn
                    (mode_tile "ecnt" 0)
                    (if (wcmatch (car cItem) "*차세대*") (mode_tile "tset" 0) (progn (set_tile "tset" "0") (mode_tile "tset" 1)))
                  )
                )
              )
            )
          )
        )
        (yb-update-span-list *yb_ls_sel*)
        
        (if *yb_curve* (set_tile "t1" "선택 완료 (O)") (set_tile "t1" "선택 안됨")) 
        (if *yb_stPt* (set_tile "t2" "선택 완료 (O)") (set_tile "t2" "선택 안됨")) 
        (if *yb_gl* (set_tile "t3" "선택 완료 (O)") (set_tile "t3" "선택 안됨")) 
        (if *yb_br* (set_tile "t4" "선택 완료 (O)") (set_tile "t4" "선택 안됨"))
        
        (mode_tile next_focus 2) 
        
        (action_tile "b1" "(yb-save-vars) (done_dialog 2)") 
        (action_tile "b2" "(yb-save-vars) (done_dialog 3)") 
        (action_tile "b3" "(yb-save-vars) (done_dialog 4)") 
        (action_tile "b4" "(yb-save-vars) (done_dialog 6)") 
        (action_tile "brst" "(setq *yb_gl* nil *yb_br* nil) (set_tile \"t3\" \"선택 안됨\") (set_tile \"t4\" \"선택 안됨\")") 
        
        (action_tile "btn_reload" "(progn (setq *YB_CLOUD_MODULES* (get-cloud-init)) (start_list \"pop\") (foreach m *YB_CLOUD_MODULES* (add_list (car m))) (end_list) (set_tile \"pop\" \"0\") (yb-ui-sync) (alert \"클라우드 DB가 최신 상태로 갱신되었습니다!\"))")
        
        (action_tile "pop" "(yb-ui-sync)") 
        (action_tile "ba" "(yb-add-span)") 
        (action_tile "bm" "(yb-mod-span)") 
        (action_tile "bd" "(yb-del-span)")
        (action_tile "ls" "(setq *yb_ls_sel* $value) (yb-click-list)") 
        
        (action_tile "accept" "(if (and *yb_curve* *yb_stPt*) (progn (yb-save-vars) (done_dialog 1)) (alert \"선형과 시작점을 선택하세요.\"))") 
        (action_tile "cancel" "(done_dialog 0)")
        
        (setq what_next (start_dialog))
        
        (cond 
          ((= what_next 2) (setq ss (ssget ":S")) (if ss (setq *yb_curve* (vlax-ename->vla-object (ssname ss 0)))) (setq next_focus "b2")) 
          ((= what_next 3) 
            (if (null *yb_curve*) (progn (alert "종단 선형을 먼저 선택하세요!") (setq next_focus "b1"))
              (progn 
                (setq loop t)
                (while loop
                  (setq pt (getpoint "\n시작점 선택 (종료/UI복귀는 스페이스바 또는 엔터): "))
                  (if pt 
                    (progn 
                      (setq cPt (vlax-curve-getClosestPointTo *yb_curve* pt))
                      (if (and cPt (> (distance pt cPt) 0.01))
                        (alert "오류: 시작점이 종단 선형 위에 있지 않습니다. 다시 선택해주세요.")
                        (progn (setq *yb_stPt* pt) (setq loop nil) (setq next_focus "b3"))
                      )
                    ) 
                    (progn (setq loop nil) (setq next_focus "b2"))
                  )
                )
              )
            )
          ) 
          ((= what_next 4) (setq ss (ssget ":S")) (if ss (setq *yb_gl* (vlax-ename->vla-object (ssname ss 0)))) (setq next_focus "b4")) 
          ((= what_next 6) (setq ss (ssget ":S")) (if ss (setq *yb_br* (vlax-ename->vla-object (ssname ss 0)))) (setq next_focus "accept"))
          ((= what_next 5) (setq next_focus "b1"))
        )
      )
      
      (unload_dialog dcl_id) 
      
      (if (= what_next 1) 
        (progn
          (vla-StartUndoMark doc)
          
          (setq cad_sc_x (* 1000.0 (atof *yb_sc_x*)))
          (setq cad_sc_y (* 1000.0 (atof *yb_sc_y*)))
          
          (setq anno_sc (atof *yb_sc_a*))
          (if (<= anno_sc 0.0) (setq anno_sc 1.0))
          
          (setq default_th (getvar "DIMTXT"))
          (if (or (null default_th) (= default_th 0.0)) (setq default_th 3.0))
          (setq dim_scale (getvar "DIMSCALE"))
          (if (or (null dim_scale) (= dim_scale 0.0)) (setq dim_scale 1.0))
          (setq min_max_h default_th)
          
          (foreach item *yb_spans*
            (setq sType (car item))
            (if (not (wcmatch sType "*A2*"))
              (progn
                (setq span_m (nth 1 item))
                (if (> span_m 0.0)
                  (progn
                    (setq span_model (* span_m cad_sc_x))
                    (setq max_h (/ (- (/ span_model dim_scale) 4.0) 7.0))
                    (if (< max_h min_max_h) (setq min_max_h max_h))
                  )
                )
              )
            )
          )
          
          (setq final_text_h (* (fix (/ min_max_h 0.5)) 0.5))
          (if (< final_text_h 0.5) (setq final_text_h 0.5))
          (if (> final_text_h default_th) (setq final_text_h default_th))
          
          (setq R_cir (* 3.5 cad_sc_x anno_sc))
          (setq lbl_txt_h (* 3.0 cad_sc_x anno_sc))
          (setq lbl_stick (* 3.0 cad_sc_x anno_sc))
          
          (setq offCAD (* (atof *yb_off*) cad_sc_x))
          (setq yOffCAD (* (atof *yb_yoff*) cad_sc_y))
          
          (setq hgap1 (parse-num *yb_hgap1*))
          (setq hgap1_val (* hgap1 cad_sc_y))
          
          (setq currX (+ (car *yb_stPt*) offCAD))
          
          (setq temp_curve (vla-copy *yb_curve*))
          (vla-Move temp_curve (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point (list 0.0 yOffCAD 0.0)))
          
          (setq blkPrefix (rtos (* (getvar "CDATE") 1000000) 2 0))
          (setq blkCnt 1)
          
          (setq ssBridge (ssadd))
          (setq ssDim (ssadd))
          
          (setq i 0)
          
          (make-db-layers '(("CR-LEAL" 1 "Continuous") ("CR-LEAT" 3 "Continuous") ("CS-LEAD" 1 "Continuous") ("CS-TEXT" 3 "Continuous")))

          (setq startX_total currX)
          (setq endX_total currX)
          (setq bridge_finished nil)
          (setq pier_cnt 1 span_cnt 1)
          
          (setq base_dim_Y (get-y-on-curve temp_curve currX))
          (if (null base_dim_Y) (setq base_dim_Y (cadr *yb_stPt*))) 
          
          (setq extY (+ base_dim_Y (* 15.0 cad_sc_x))) 
          (setq dimY (+ base_dim_Y (* 25.0 cad_sc_x))) 
          (setq dimY_total (+ base_dim_Y (* 35.0 cad_sc_x))) 

          (while (< i (length *yb_spans*))
            (setq item (nth i *yb_spans*))
            (setq sType (car item))
            (setq spanLen (* (nth 1 item) cad_sc_x))
            
            (setq hgap (nth 2 item))
            (setq ghf (if (nth 3 item) (nth 3 item) 0.0))
            (setq ghb (if (nth 4 item) (nth 4 item) 0.0))
            
            (setq dim1A (nth 5 item) dim2A (nth 6 item) dim3A (nth 7 item) dim4A (nth 8 item) dim5A (nth 9 item))
            (setq eh (if (nth 10 item) (nth 10 item) 0.0))
            (setq em (if (nth 11 item) (nth 11 item) 0.0))
            (setq ei (* (if (nth 12 item) (nth 12 item) 0.0) cad_sc_y))
            (setq cFile (nth 13 item))
            
            (setq data (cdr (assoc cFile *yb_db_cache*)))

            (make-db-layers (get-data-val 'LAYERS data))
            
            (setq mirror 1.0)
            
            (if (wcmatch sType "*A2*") 
              (progn
                (setq mirror -1.0)
              )
            )
            
            (setq currY (get-y-on-curve temp_curve currX))

            (if (and currY data) 
              (progn
                (setq pile_lines (get-all-lines-by-key 'PILE_LINES data))
                (setq draw_lines (get-all-lines-by-key 'DRAW_LINES data))
                (setq gap_lines (get-all-lines-by-key 'GIRDER_GAP_LINE data))
                
                (if (null pile_lines) (setq pile_lines (get-all-lines-by-key 'LINES data)))
                (if (null draw_lines) (setq draw_lines (get-all-lines-by-key 'LINES data)))
                
                (if gap_lines
                  (foreach g gap_lines
                    (setq draw_lines (append draw_lines (list (cons "GAP_L" (cdr g)))))
                  )
                )
                
                (setq s_front (get-data-val 'STRETCH_FRONT data))
                (setq s_back (get-data-val 'STRETCH_BACK data))
                (setq s_max (get-data-val 'STRETCH_MAX data))
                
                (setq limitGL nil limitGL_Y2 0.0 fTopLineData nil)
                (setq numsGL (flatten-nums (list (get-data-val 'STR_LIMIT_GL data))))
                (if numsGL
                  (cond
                    ((= (length numsGL) 6)
                     (setq limitGL (* (nth 0 numsGL) cad_sc_y))
                     (setq limitGL_Y2 (* (nth 1 numsGL) cad_sc_y))
                     (setq fTopLineData (list (nth 2 numsGL) (nth 3 numsGL) (nth 4 numsGL) (nth 5 numsGL)))
                    )
                    ((= (length numsGL) 5)
                     (setq limitGL (* (nth 0 numsGL) cad_sc_y))
                     (setq limitGL_Y2 0.0)
                     (setq fTopLineData (list (nth 1 numsGL) (nth 2 numsGL) (nth 3 numsGL) (nth 4 numsGL)))
                    )
                    (t
                     (setq limitGL (* (car numsGL) cad_sc_y))
                     (if (> (length numsGL) 1) (setq fTopLineData (cdr numsGL)))
                    )
                  )
                )
                
                (setq numsBR (flatten-nums (list (get-data-val 'STR_LIMIT_BR data))))
                (setq limitBR (if (car numsBR) (* (car numsBR) cad_sc_y) nil))
                
                (setq pilesX '() pile_bottom_Y nil)
                (if pile_lines
                  (foreach line pile_lines
                    (if (= (car line) "L")
                      (progn
                        (setq x1 (if (nth 2 line) (nth 2 line) 0.0))
                        (setq y1 (if (nth 3 line) (nth 3 line) 0.0))
                        (setq x2 (if (nth 4 line) (nth 4 line) 0.0))
                        (setq y2 (if (nth 5 line) (nth 5 line) 0.0))
                        
                        (if (< (abs (- x1 x2)) 1e-3)
                          (progn
                            (setq ptx x1)
                            (if (not (vl-position ptx pilesX))
                              (setq pilesX (cons ptx pilesX))
                            )
                            
                            (setq y_a (* y1 cad_sc_y))
                            (setq y_b (* y2 cad_sc_y))
                            (setq min_y (min y_a y_b))
                            (if (null pile_bottom_Y)
                                (setq pile_bottom_Y min_y)
                                (if (< min_y pile_bottom_Y) (setq pile_bottom_Y min_y))
                            )
                          )
                        )
                      )
                    )
                  )
                )
                
                (setq strD_GL 0.0)
                (setq strD_BR 0.0)

                (if (and fTopLineData *yb_gl*)
                  (progn
                    (setq fTopHalfW (* (max (abs (if (nth 0 fTopLineData) (nth 0 fTopLineData) 0.0)) (abs (if (nth 2 fTopLineData) (nth 2 fTopLineData) 0.0))) cad_sc_x))
                    (setq fTopY (* (min (if (nth 1 fTopLineData) (nth 1 fTopLineData) 0.0) (if (nth 3 fTopLineData) (nth 3 fTopLineData) 0.0)) cad_sc_y))
                    (setq groundY (get-min-ground-y *yb_gl* currX fTopHalfW))
                    
                    (if groundY
                      (progn
                        (setq H1_current (abs (- fTopY limitGL_Y2)))
                        (setq h_max_val (max (* ghf cad_sc_y) (* ghb cad_sc_y)))
                        (setq H_req (- (+ currY limitGL_Y2 (- h_max_val)) groundY))
                        (setq H_current_compressed (- H1_current h_max_val))
                        
                        (setq module_size (* 0.500 cad_sc_y))
                        (setq factor (/ H_req module_size))
                        (setq ceilFactor (if (equal (fix factor) factor 1e-5) 
                                           (fix factor) 
                                           (if (> factor 0.0) (+ (fix factor) 1) (fix factor))))
                        (setq H1_new (* ceilFactor module_size))
                        (setq strD_GL (- H1_new H_current_compressed))
                      )
                    )
                  )
                  (if (and limitGL *yb_gl*)
                    (progn
                      (setq groundY (get-min-ground-y *yb_gl* currX (* 1.700 cad_sc_x)))
                      (if groundY 
                        (progn 
                          (setq curr_limit_Y (+ currY limitGL)) 
                          (setq diff_Y (- curr_limit_Y groundY))
                          (setq module_size (* 0.500 cad_sc_y))
                          (setq factor (/ diff_Y module_size))
                          (setq ceilFactor (if (equal (fix factor) factor 1e-5) 
                                             (fix factor) 
                                             (if (> factor 0.0) (+ (fix factor) 1) (fix factor))))
                          (setq strD_GL (* ceilFactor module_size))
                        )
                      )
                    )
                  )
                )

                (setq hgap_val (* (if hgap hgap 0.0) cad_sc_y))
                (setq strD_GL (+ strD_GL hgap1_val hgap_val))

                (if (and limitBR pilesX pile_bottom_Y *yb_br*) 
                  (progn
                    (setq min_target_Y nil)
                    
                    (foreach px pilesX
                      (setq absX (+ currX (* mirror px cad_sc_x)))
                      (setq brY (get-y-on-curve *yb_br* absX))
                      (if brY 
                        (progn 
                          (setq targetY (- brY ei))
                          (if (null min_target_Y)
                            (setq min_target_Y targetY)
                            (if (< targetY min_target_Y) (setq min_target_Y targetY))
                          )
                        )
                      )
                    )
                    
                    (if min_target_Y
                      (progn
                        (setq curr_actual_pile_tip_Y (+ currY (- pile_bottom_Y strD_GL)))
                        (setq req_str (- curr_actual_pile_tip_Y min_target_Y))
                        
                        (setq br_module (* 0.500 cad_sc_y))
                        (setq factor (/ req_str br_module))
                        
                        (setq ceilFactor (if (equal (fix factor) factor 1e-5) 
                                           (fix factor) 
                                           (if (> factor 0.0) (+ (fix factor) 1) (fix factor))
                                         ))
                        (setq strD_BR (* ceilFactor br_module))
                      )
                    )
                  )
                )

                (setq lastEnt (entlast))
                (draw-cloud-structure (list currX currY 0.0) draw_lines strD_GL strD_BR mirror cad_sc_x cad_sc_y limitGL limitBR ghf ghb s_front s_back s_max)
                (setq ssSub (get-ents-after lastEnt))
                (if ssSub (add-ss-to-ss ssSub ssBridge))

                (if (and (> spanLen 0.0) (not (wcmatch sType "*A2*"))) 
                  (progn
                    (setq nextMirror 1.0)
                    (if (< (+ i 1) (length *yb_spans*)) 
                      (progn
                        (setq nextItem (nth (+ i 1) *yb_spans*))
                        (setq nextType (car nextItem))
                      )
                    )
                    (if (wcmatch nextType "*A2*") (setq nextMirror -1.0))
                    
                    (setq lastEnt (entlast))
                    
                    (if nextItem
                      (progn 
                        (setq dim1B (nth 5 nextItem) dim2B (nth 6 nextItem) dim3B (nth 7 nextItem) dim4B (nth 8 nextItem) dim5B (nth 9 nextItem))
                        (setq nextData (cdr (assoc (nth 13 nextItem) *yb_db_cache*)))
                      )
                      (progn 
                        (setq dim1B '(nil nil nil "0") dim2B '(nil nil nil "0") dim3B '(nil nil nil "0") dim4B '(nil nil nil "0") dim5B '(nil nil nil "0") nextData nil)
                      )
                    )
                    
                    (draw-upper-structure temp_curve currX dim1A dim2A dim3A dim4A dim5A mirror (+ currX spanLen) dim1B dim2B dim3B dim4B dim5B nextMirror cad_sc_x cad_sc_y eh em data nextData)
                    
                    (setq ssSup (get-ents-after lastEnt))
                    (if ssSup (add-ss-to-ss ssSup ssBridge))
                  )
                )
              )
            )
            
            (if (not bridge_finished)
              (if (wcmatch sType "*A2*")
                (progn
                  (setq endX_total currX)
                  (setq bridge_finished t)
                  (setq Cy_P (+ currY (* 5.0 cad_sc_x)))
                  (draw-label-circle currX Cy_P "A2" "CR-LEAT" "CR-LEAL" R_cir lbl_txt_h lbl_stick t ssDim)
                )
                (if (> spanLen 0.0)
                  (progn
                    (setq Cy_P (+ currY (* 5.0 cad_sc_x)))
                    (setq lbl (if (= i 0) "A1" (strcat "P" (itoa pier_cnt))))
                    (if (> i 0) (setq pier_cnt (1+ pier_cnt)))
                    (draw-label-circle currX Cy_P lbl "CR-LEAT" "CR-LEAL" R_cir lbl_txt_h lbl_stick t ssDim)
                    
                    (setq midX (+ currX (/ spanLen 2.0)))
                    (setq midY (get-y-on-curve temp_curve midX))
                    (if (null midY) (setq midY currY)) 
                    (setq Cy_S (+ midY (* 2.5 cad_sc_x)))
                    
                    (setq span_lbl (strcat "S" (itoa span_cnt)))
                    (setq span_cnt (1+ span_cnt))
                    (draw-label-circle midX Cy_S span_lbl "CR-LEAT" "CR-LEAL" R_cir lbl_txt_h lbl_stick nil ssDim)

                    (check-make-layer "CS-DIMS")
                    (setq oldLayer (getvar "CLAYER"))
                    (setvar "CLAYER" "CS-DIMS")
                    
                    (setq old_dimlunit (getvar "DIMLUNIT"))
                    (setq old_dimtix (getvar "DIMTIX"))
                    (setq old_dimdec (getvar "DIMDEC"))
                    (setvar "DIMLUNIT" 6) 
                    (setvar "DIMTIX" 1)   
                    (setvar "DIMDEC" 0)   
                    
                    (setq pt1 (list currX extY 0.0))
                    (setq pt2 (list (+ currX spanLen) extY 0.0))
                    (setq textPt (list (+ currX (/ spanLen 2.0)) dimY 0.0))
                    
                    (command "._DIMALIGNED" "_non" pt1 "_non" pt2 "_non" textPt)
                    
                    (setq lastEnt (entlast))
                    (if lastEnt 
                      (progn
                        (ssadd lastEnt ssDim)
                        (setq dim_text_1st (if (and *yb_dim_txt* (/= *yb_dim_txt* "")) (strcat "<>\\X" *yb_dim_txt*) ""))
                        (apply-yb-dim-style lastEnt dim_text_1st final_text_h)
                      )
                    )
                    
                    (setvar "DIMLUNIT" old_dimlunit)
                    (setvar "DIMTIX" old_dimtix)
                    (setvar "DIMDEC" old_dimdec)
                    (setvar "CLAYER" oldLayer)
                    
                    (setq endX_total (+ currX spanLen))
                  )
                )
              )
            )
            (setq currX (+ currX spanLen))
            (setq i (1+ i))
          )

          ;; ========================================================
          ;; ★ 교량 제원 주석 (A1 위치) 및 총장 치수선
          ;; ========================================================
          (if (> (- endX_total startX_total) 0.0)
            (progn
              ;; 1) 총경간 문자 그룹핑 계산 (예: 30.000X2 + 15.000X1)
              (setq span_list '())
              (setq j_idx 0)
              (while (< j_idx (length *yb_spans*))
                (setq tmp_item (nth j_idx *yb_spans*))
                (if (not (wcmatch (car tmp_item) "*A2*"))
                  (setq span_list (append span_list (list (nth 1 tmp_item))))
                )
                (setq j_idx (1+ j_idx))
              )
              
              (setq group_str "")
              (if (> (length span_list) 0)
                (progn
                  (setq cur_val (car span_list))
                  (setq cur_cnt 1)
                  (setq j_idx 1)
                  (while (< j_idx (length span_list))
                    (setq val (nth j_idx span_list))
                    (if (equal val cur_val 1e-4)
                      (setq cur_cnt (1+ cur_cnt))
                      (progn
                        (setq group_str (strcat group_str (rtos cur_val 2 3) "X" (itoa cur_cnt) "+"))
                        (setq cur_val val)
                        (setq cur_cnt 1)
                      )
                    )
                    (setq j_idx (1+ j_idx))
                  )
                  (setq group_str (strcat group_str (rtos cur_val 2 3) "X" (itoa cur_cnt)))
                )
              )
              
              ;; 2) 30도 회전 주석 텍스트 조합 (교량명 입력 제거)
              (setq L_total (/ (- endX_total startX_total) cad_sc_x))
              (setq dim_txt (if (and *yb_dim_txt* (/= *yb_dim_txt* "")) *yb_dim_txt* ""))
              
              (setq str1 (strcat "B" dim_txt " 교량명"))
              (setq str2 group_str)
              (setq str3 (strcat "L=" (rtos L_total 2 3) "m"))

              ;; 3) A1 위치에 30도 지시선 및 다중 텍스트 작도
              (setq txt_h (* 3.0 cad_sc_x anno_sc))
              (setq line_Y (+ dimY_total (* 10.0 cad_sc_x)))
              (setq start_X startX_total)
              
              ;; 굴림체 텍스트 스타일 생성 및 등록
              (if (not (tblsearch "STYLE" "YB_GULIM"))
                (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") 
                               '(2 . "YB_GULIM") '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0) '(71 . 0) 
                               '(42 . 3.0) '(3 . "gulim.ttc") '(4 . "")))
              )
              
              ;; ★ [핵심] textbox 함수를 이용해 굴림체 폰트 형상 기준 실제 글자 폭(Width)을 정확히 스캔
              (setq box1 (textbox (list (cons 1 str1) (cons 40 txt_h) (cons 7 "YB_GULIM"))))
              (setq box2 (textbox (list (cons 1 str2) (cons 40 txt_h) (cons 7 "YB_GULIM"))))
              (setq box3 (textbox (list (cons 1 str3) (cons 40 txt_h) (cons 7 "YB_GULIM"))))
              
              (setq len1 (if box1 (- (car (cadr box1)) (car (car box1))) 0.0))
              (setq len2 (if box2 (- (car (cadr box2)) (car (car box2))) 0.0))
              (setq len3 (if box3 (- (car (cadr box3)) (car (car box3))) 0.0))
              
              (setq max_txt_len (max len1 len2 len3))
              
              ;; 30도 지시선의 길이는 가장 긴 글자의 폭 + 앞뒤 여유 공간(4유닛)
              (setq line_len (+ max_txt_len (* 4.0 cad_sc_x anno_sc)))
              
              ;; 30도 각도의 방향 벡터 (vx, vy) 및 수직 방향 벡터 (ux, uy)
              (setq rot_rad (/ (* 30.0 pi) 180.0))
              (setq vx (cos rot_rad))
              (setq vy (sin rot_rad))
              (setq ux (- vy))
              (setq uy vx)
              
              (setq line2_X (+ start_X (* line_len vx)))
              (setq line2_Y (+ line_Y (* line_len vy)))
              
              (check-make-layer "CS-LEAD")
              (check-make-layer "CS-TEXT")
              
              ;; 수직 지시선
              (entmake (list '(0 . "LINE") (cons 8 "CS-LEAD") (cons 62 256)
                             (cons 10 (list start_X base_dim_Y 0.0))
                             (cons 11 (list start_X line_Y 0.0))))
              (ssadd (entlast) ssDim)
              
              ;; 30도 지시선 (글자 폭에 정확히 맞춘 길이)
              (entmake (list '(0 . "LINE") (cons 8 "CS-LEAD") (cons 62 256)
                             (cons 10 (list start_X line_Y 0.0))
                             (cons 11 (list line2_X line2_Y 0.0))))
              (ssadd (entlast) ssDim)
              
              ;; 텍스트 삽입 기준점 (지시선 시작점에서 1.5만큼 들여쓰기)
              (setq t_base_X (+ start_X (* 1.5 cad_sc_x anno_sc vx)))
              (setq t_base_Y (+ line_Y  (* 1.5 cad_sc_x anno_sc vy)))

              ;; str1 (상단) : 지시선 윗쪽으로 2.2칸
              (setq t1_X (+ t_base_X (* 2.2 txt_h ux)))
              (setq t1_Y (+ t_base_Y (* 2.2 txt_h uy)))

              ;; str2 (중단) : 지시선 윗쪽으로 0.5칸
              (setq t2_X (+ t_base_X (* 0.5 txt_h ux)))
              (setq t2_Y (+ t_base_Y (* 0.5 txt_h uy)))

              ;; str3 (하단) : 지시선 아래쪽으로 1.2칸 내리기
              (setq t3_X (- t_base_X (* 1.2 txt_h ux)))
              (setq t3_Y (- t_base_Y (* 1.2 txt_h uy)))
              
              ;; str1 작도
              (entmake (list '(0 . "TEXT") (cons 8 "CS-TEXT") (cons 62 256) (cons 7 "YB_GULIM")
                             (cons 10 (list t1_X t1_Y 0.0))
                             (cons 40 txt_h) (cons 50 rot_rad) (cons 1 str1)))
              (ssadd (entlast) ssDim)
              
              ;; str2 작도
              (entmake (list '(0 . "TEXT") (cons 8 "CS-TEXT") (cons 62 256) (cons 7 "YB_GULIM")
                             (cons 10 (list t2_X t2_Y 0.0))
                             (cons 40 txt_h) (cons 50 rot_rad) (cons 1 str2)))
              (ssadd (entlast) ssDim)
              
              ;; str3 작도
              (entmake (list '(0 . "TEXT") (cons 8 "CS-TEXT") (cons 62 256) (cons 7 "YB_GULIM")
                             (cons 10 (list t3_X t3_Y 0.0))
                             (cons 40 txt_h) (cons 50 rot_rad) (cons 1 str3)))
              (ssadd (entlast) ssDim)

              ;; 4) 기존 총장 치수선 작도
              (check-make-layer "CS-DIMS")
              (setq oldLayer (getvar "CLAYER"))
              (setvar "CLAYER" "CS-DIMS")
              
              (setq old_dimlunit (getvar "DIMLUNIT"))
              (setq old_dimtix (getvar "DIMTIX"))
              (setq old_dimdec (getvar "DIMDEC"))
              (setvar "DIMLUNIT" 6) 
              (setvar "DIMTIX" 1)
              (setvar "DIMDEC" 0)
              
              (setq pt1 (list startX_total extY 0.0))
              (setq pt2 (list endX_total extY 0.0))
              (setq textPt (list (+ startX_total (/ (- endX_total startX_total) 2.0)) dimY_total 0.0))
              
              (command "._DIMALIGNED" "_non" pt1 "_non" pt2 "_non" textPt)
              
              (setq lastEnt (entlast))
              (if lastEnt 
                (progn
                  (ssadd lastEnt ssDim)
                  (apply-yb-dim-style lastEnt "" final_text_h)
                )
              )
              
              (setvar "DIMLUNIT" old_dimlunit)
              (setvar "DIMTIX" old_dimtix)
              (setvar "DIMDEC" old_dimdec)
              (setvar "CLAYER" oldLayer)
            )
          )
          ;; ========================================================

          (if (> (sslength ssBridge) 0) 
            (progn
              (setq bName (strcat "YB_BRIDGE_" blkPrefix))
              (command "._-BLOCK" bName "_non" *yb_stPt* ssBridge "")
              (command "._-INSERT" bName "_non" *yb_stPt* 1 1 0)
            )
          )
          
          (if (> (sslength ssDim) 0) 
            (progn
              (setq dName (strcat "YB_DIM_" blkPrefix))
              (command "._-BLOCK" dName "_non" *yb_stPt* ssDim "")
              (command "._-INSERT" dName "_non" *yb_stPt* 1 1 0)
            )
          )
          
          (if temp_curve (vla-Delete temp_curve))
          
          (vla-EndUndoMark doc)
          (princ "\n*** 작도 완료! ***\n")
        )
      )
      (setvar "CMDECHO" oldCmd) 
    )
  )
  (princ)
)