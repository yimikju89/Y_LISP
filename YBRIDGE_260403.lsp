(vl-load-com)

;; =========================================================================
;; ★ 0. 글로벌 변수 초기화
;; =========================================================================
(setq *yb_sc* "0.001" *yb_off* "0" *yb_yoff* "0" *yb_hgap1* "0" *yb_spans* '() *yb_ls_sel* "")
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
      (princ "\n[DB 동기화] 데이터를 분석 중입니다...\n")
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
      (princ "[완료] DB 연동 성공!\n")
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
      ;; 레이어 생성 또는 가져오기
      (if (not (tblsearch "LAYER" lname))
        (setq layObj (vla-Add lays lname))
        (setq layObj (vla-Item lays lname))
      )
      ;; 색상 적용
      (if lcol (vla-put-Color layObj lcol))
      ;; 선종류 적용 (캐드에 해당 선종류가 로드되어 있지 않을 경우 튕김 방지)
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
      (entmake (list '(0 . "ARC") (cons 8 layer) (cons 10 (list cx cy 0.0)) (cons 40 r) (cons 50 startAng) (cons 51 endAng)))
    )
  )
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
  
  ;; ★ 아이템 추가 시 항상 추가된 부분의 가장 밑바닥 인덱스로 포커싱 이동
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
  (setq *yb_sc* (get_tile "eb_sc") *yb_off* (get_tile "eb_off") *yb_yoff* (get_tile "eb_yoff") *yb_hgap1* (get_tile "eb_hgap1") *yb_pop* (get_tile "pop"))
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
  
  (write-line "yb_dlg : dialog { label=\"교량 자동 작도기 (Cloud V15.3)\"; width=100;" f)
  
  (write-line " : boxed_row { label=\"기본 설정\"; : row { alignment=left; : text { value=\"스케일:\"; width=8; fixed_width=true; } : edit_box { key=\"eb_sc\"; width=8; fixed_width=true; } : spacer {width=80; fixed_width=true;} } }" f)
  
  (write-line " : boxed_row { label=\"위치 및 선형 설정\";" f)
  (write-line "  : row { alignment=left;" f)
  (write-line "   : column { alignment=left;" f)
  (write-line "     : row { : button { key=\"b1\"; label=\"종단 선형 선택\"; width=16; fixed_width=true; } : text { key=\"t1\"; value=\"선택 안됨\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : button { key=\"b2\"; label=\"시작점 선택\"; width=16; fixed_width=true; } : text { key=\"t2\"; value=\"선택 안됨\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : text { value=\"시작 이격거리 [m]:\"; width=16; fixed_width=true; } : edit_box { key=\"eb_off\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : text { value=\"상하 이격거리 [m]:\"; width=16; fixed_width=true; } : edit_box { key=\"eb_yoff\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : text { value=\"보정값1(일괄) [m]:\"; width=16; fixed_width=true; } : edit_box { key=\"eb_hgap1\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "   }" f)
  (write-line "   : spacer { width=4; }" f)
  (write-line "   : column { alignment=left;" f)
  (write-line "     : row { : button { key=\"b3\"; label=\"지반선 선택\"; width=16; fixed_width=true; } : text { key=\"t3\"; value=\"선택 안됨\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : button { key=\"b4\"; label=\"기반암 선택\"; width=16; fixed_width=true; } : text { key=\"t4\"; value=\"선택 안됨\"; width=10; fixed_width=true; } : spacer {} }" f)
  (write-line "     : row { : button { key=\"brst\"; label=\"선택 초기화\"; width=16; fixed_width=true; } : spacer {} }" f)
  (write-line "   }" f)
  (write-line "   : spacer {} " f)
  (write-line "  }" f)
  (write-line " }" f)
  
  (write-line " : boxed_column { label=\"경간 구성표\";" f)
  (write-line "  : row { alignment=left; : text { value=\"종류:\"; width=5; fixed_width=true; } : popup_list { key=\"pop\"; width=45; fixed_width=true; } : spacer {} }" f)
  
  (write-line "  : row { alignment=left;" f)
  (write-line "    : text { value=\"경간 [m]:\"; width=8; fixed_width=true; } : edit_box { key=\"el\"; width=6; fixed_width=true; } : spacer { width=4; }" f)
  (write-line "    : text { value=\"보정값2(개별) [m]:\"; width=18; fixed_width=true; } : edit_box { key=\"ehgap\"; width=6; fixed_width=true; } : spacer { width=4; }" f)
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

(defun draw-cloud-structure (insPt draw_lines strD_GL strD_BR mirror cad_sc limitGL limitBR ghf ghb s_front s_back s_max / 
                              baseX baseY oType lay unX1 unY1 unX2 unY2 unCX unCY rad sang eang 
                              shift1 shift2 shiftC h_max_val ghf_val ghb_val
                              x1 y1 x2 y2 absX1 absX2 absY1 absY2 absCX absCY temp_sang temp_eang
                              sGL1 sGL2 sBR1 sBR2)
  (setq baseX (if (car insPt) (car insPt) 0.0))
  (setq baseY (if (cadr insPt) (cadr insPt) 0.0))
  
  (setq ghf_val (* (if ghf ghf 0.0) cad_sc))
  (setq ghb_val (* (if ghb ghb 0.0) cad_sc))
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
          
          (setq y1 (- (* unY1 cad_sc) shift1))
          (setq y2 (- (* unY2 cad_sc) shift2))
          
          (setq x1 (* mirror (* unX1 cad_sc)))
          (setq x2 (* mirror (* unX2 cad_sc)))
          
          (setq absX1 (+ baseX x1))
          (setq absX2 (+ baseX x2))
          (setq absY1 (+ baseY y1))
          (setq absY2 (+ baseY y2))
          
          (setq sGL1 0.0 sBR1 0.0 sGL2 0.0 sBR2 0.0)
          
          (if (and limitGL (<= (* unY1 cad_sc) (+ limitGL 1e-4))) (setq sGL1 strD_GL))
          (if (and limitBR (<= (* unY1 cad_sc) (+ limitBR 1e-4))) (setq sBR1 strD_BR sGL1 strD_GL))
          
          (if (and limitGL (<= (* unY2 cad_sc) (+ limitGL 1e-4))) (setq sGL2 strD_GL))
          (if (and limitBR (<= (* unY2 cad_sc) (+ limitBR 1e-4))) (setq sBR2 strD_BR sGL2 strD_GL))
          
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
          
          (entmake (list '(0 . "LINE") (cons 8 lay) (cons 10 (list absX1 absY1 0.0)) (cons 11 (list absX2 absY2 0.0))))
        )
        
        ((= oType "GAP_L")
          (setq unX1 (if (nth 2 item) (nth 2 item) 0.0))
          (setq unY1 (if (nth 3 item) (nth 3 item) 0.0))
          (setq unX2 (if (nth 4 item) (nth 4 item) 0.0))
          (setq unY2 (if (nth 5 item) (nth 5 item) 0.0))
          
          ;; 1. 거더 높이 중 작은값(위)과 큰값(아래) 판별
          (setq min_gh (min ghf_val ghb_val))
          (setq max_gh (max ghf_val ghb_val))

          ;; 2. 단차 라인의 두 점 중 위쪽(Y가 큰 점)과 아래쪽(Y가 작은 점) 판별
          ;; 원본 좌표(unY) 기준입니다.
          (if (>= unY1 unY2)
            (progn (setq topY unY1 botY unY2)) ;; p1이 위, p2가 아래
            (progn (setq topY unY2 botY unY1)) ;; p2가 위, p1이 아래
          )

          ;; 3. 작은 거더 높이는 상단점(topY)에, 큰 거더 높이는 하단점(botY)에 적용
          (setq y1 (- (* topY cad_sc) min_gh))
          (setq y2 (- (* botY cad_sc) max_gh))
          
          ;; 4. X좌표 및 최종 위치 연산 (수직 라인이므로 X는 동일하게 유지됨)
          (setq x1 (* mirror (* unX1 cad_sc)))
          (setq x2 (* mirror (* unX2 cad_sc)))

          (setq absX1 (+ baseX x1))
          (setq absX2 (+ baseX x2))
          (setq absY1 (+ baseY y1))
          (setq absY2 (+ baseY y2))
          
          (entmake (list '(0 . "LINE") (cons 8 lay) (cons 10 (list absX1 absY1 0.0)) (cons 11 (list absX2 absY2 0.0))))
        )
        
        ((= oType "A")
          (setq unCX (if (nth 2 item) (nth 2 item) 0.0))
          (setq unCY (if (nth 3 item) (nth 3 item) 0.0))
          (setq rad (* (if (nth 4 item) (nth 4 item) 0.0) cad_sc))
          (setq sang (if (nth 5 item) (nth 5 item) 0.0))
          (setq eang (if (nth 6 item) (nth 6 item) 0.0))
          
          (setq shiftC (yb-get-shift unCX unCY item s_max s_front s_back h_max_val ghf_val ghb_val))
          
          (setq cy (- (* unCY cad_sc) shiftC))
          (setq cx (* mirror (* unCX cad_sc)))
          
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
          
          (if (and limitBR (<= (* unCY cad_sc) limitBR)) 
            (setq absCY (- absCY (+ strD_GL strD_BR))) 
            (if (and limitGL (<= (* unCY cad_sc) limitGL)) (setq absCY (- absCY strD_GL)))
          )
          
          (entmake (list '(0 . "ARC") (cons 8 lay) (cons 10 (list absCX absCY 0.0)) (cons 40 (float rad)) (cons 50 (float sang)) (cons 51 (float eang))))
        )
      )
    )
  )
)

(defun copy-and-trim-profile (curveObj startX endX offsetY lay / newObj en l1 l2 ptLeft ptRight old_os old_ce old_edge minX maxX cy yL yR rawY)
  (check-make-layer lay) 
  (setq old_os (getvar "OSMODE") old_ce (getvar "CMDECHO") old_edge (getvar "EDGEMODE")) 
  (setvar "OSMODE" 0) (setvar "CMDECHO" 0) (setvar "EDGEMODE" 1)
  
  (setq newObj (vla-copy curveObj)) 
  (vla-Move newObj (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point (list 0.0 (- offsetY) 0.0)))
  (vla-put-Layer newObj lay) 
  (vla-put-Color newObj 256) 
  (vla-put-Visible newObj :vlax-true) 
  (setq en (vlax-vla-object->ename newObj))
  
  (entmake (list '(0 . "LINE") (cons 10 (list startX -1e6 0.0)) (cons 11 (list startX 1e6 0.0)))) 
  (setq l1 (entlast))
  (entmake (list '(0 . "LINE") (cons 10 (list endX -1e6 0.0)) (cons 11 (list endX 1e6 0.0)))) 
  (setq l2 (entlast))
  
  (setq rawY (get-y-on-curve curveObj (+ startX (/ (- endX startX) 2.0))))
  (setq cy (if rawY (- rawY offsetY) 0.0))
  (setq yL (get-y-on-curve curveObj (- startX 2.0)))
  (setq yR (get-y-on-curve curveObj (+ endX 2.0)))
  
  (setq ptLeft  (list (- startX 2.0) (if yL (- yL offsetY) cy) 0.0))
  (setq ptRight (list (+ endX 2.0) (if yR (- yR offsetY) cy) 0.0))

  (command "._VIEW" "_S" "temp_yb_view")
  (setq minX (- startX 5.0) maxX (+ endX 5.0)) 
  (command "._ZOOM" "_W" (list minX (- cy 20.0) 0.0) (list maxX (+ cy 20.0) 0.0))
  
  (if (and *yb_curve* (not (vlax-erased-p *yb_curve*))) (vla-put-Visible *yb_curve* :vlax-false))
  (if (and *yb_gl* (not (vlax-erased-p *yb_gl*))) (vla-put-Visible *yb_gl* :vlax-false))
  (if (and *yb_br* (not (vlax-erased-p *yb_br*))) (vla-put-Visible *yb_br* :vlax-false))
  (if (not (vlax-erased-p curveObj)) (vla-put-Visible curveObj :vlax-false))
  
  (command "._TRIM" l1 l2 "" (list en ptLeft) (list en ptRight) "")
  
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

;; ★ D1~D5 수직선 작도 시, 이전 D값(prevDim)의 하단선까지만 그려지게 하는 로직 추가
(defun draw-single-layer (curveObj curX nextX dimA dimB mirrorA mirrorB cad_sc lay prevDimA prevDimB / 
                          downA frontA backA vlineA downB frontB backB vlineB 
                          prevDownA prevDownB prevOffY_A prevOffY_B
                          offA offB startX endX offsetY yL yR)
  (setq downA (nth 0 dimA) frontA (nth 1 dimA) backA (nth 2 dimA) vlineA (nth 3 dimA))
  (setq downB (nth 0 dimB) frontB (nth 1 dimB) backB (nth 2 dimB) vlineB (nth 3 dimB))
  
  ;; 이전 D 객체의 내림값을 찾아 직전 수평선 레벨을 추적
  (setq prevDownA (if (and prevDimA (nth 0 prevDimA)) (nth 0 prevDimA) 0.0))
  (setq prevDownB (if (and prevDimB (nth 0 prevDimB)) (nth 0 prevDimB) 0.0))
  (setq prevOffY_A (* prevDownA cad_sc))
  (setq prevOffY_B (* prevDownB cad_sc))

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

      (setq startX (+ curX (* offA cad_sc)))
      (setq endX (+ nextX (* offB cad_sc)))

      (setq offsetY (* downA cad_sc))

      (copy-and-trim-profile curveObj startX endX offsetY lay)
      
      (if (= vlineA "1")
        (progn
          (setq yL (get-y-on-curve curveObj startX))
          (if yL (entmake (list '(0 . "LINE") (cons 8 lay) (cons 10 (list startX (- yL offsetY) 0.0)) (cons 11 (list startX (- yL prevOffY_A) 0.0)))))
        )
      )
      
      (if (= vlineB "1")
        (progn
          (setq yR (get-y-on-curve curveObj endX))
          (if yR (entmake (list '(0 . "LINE") (cons 8 lay) (cons 10 (list endX (- yR offsetY) 0.0)) (cons 11 (list endX (- yR prevOffY_B) 0.0)))))
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

;; ★ D1~D5 수직선을 연속해서 연결하기 위해 이전 단계(lastValid) 정보를 다음 함수로 넘김
(defun draw-upper-structure (curveObj curX dim1A dim2A dim3A dim4A dim5A mirrorA nextX dim1B dim2B dim3B dim4B dim5B mirrorB cad_sc eh em dataA dataB / archOffA archOffB archX_A archX_B midX yFaceA yFaceB yMid yArc1 yArc2 lay lastValidA lastValidB)
  (setq lay "CS-CONC-MAJR") 
  (check-make-layer lay)
  
  (setq lastValidA nil lastValidB nil)
  
  (draw-single-layer curveObj curX nextX dim1A dim1B mirrorA mirrorB cad_sc lay lastValidA lastValidB)
  (if (and dim1A (nth 0 dim1A)) (setq lastValidA dim1A))
  (if (and dim1B (nth 0 dim1B)) (setq lastValidB dim1B))
  
  (draw-single-layer curveObj curX nextX dim2A dim2B mirrorA mirrorB cad_sc lay lastValidA lastValidB)
  (if (and dim2A (nth 0 dim2A)) (setq lastValidA dim2A))
  (if (and dim2B (nth 0 dim2B)) (setq lastValidB dim2B))
  
  (draw-single-layer curveObj curX nextX dim3A dim3B mirrorA mirrorB cad_sc lay lastValidA lastValidB)
  (if (and dim3A (nth 0 dim3A)) (setq lastValidA dim3A))
  (if (and dim3B (nth 0 dim3B)) (setq lastValidB dim3B))
  
  (draw-single-layer curveObj curX nextX dim4A dim4B mirrorA mirrorB cad_sc lay lastValidA lastValidB)
  (if (and dim4A (nth 0 dim4A)) (setq lastValidA dim4A))
  (if (and dim4B (nth 0 dim4B)) (setq lastValidB dim4B))
  
  (draw-single-layer curveObj curX nextX dim5A dim5B mirrorA mirrorB cad_sc lay lastValidA lastValidB)
  
  (setq archOffA (get-hae-offset dataA T mirrorA))
  (if (and (= archOffA 0.0) dim2A (nth 0 dim2A) (> (nth 0 dim2A) 0))
      (setq archOffA (if (= mirrorA -1.0) (* (if (nth 1 dim2A) (nth 1 dim2A) 0.0) -1.0) (if (nth 2 dim2A) (nth 2 dim2A) 0.0))))

  (setq archOffB (get-hae-offset dataB nil mirrorB))
  (if (and (= archOffB 0.0) dim2B (nth 0 dim2B) (> (nth 0 dim2B) 0))
      (setq archOffB (if (= mirrorB -1.0) (* (if (nth 2 dim2B) (nth 2 dim2B) 0.0) -1.0) (if (nth 1 dim2B) (nth 1 dim2B) 0.0))))

  (setq archX_A (+ curX (* archOffA cad_sc)))
  (setq archX_B (+ nextX (* archOffB cad_sc)))
  (setq midX (/ (+ archX_A archX_B) 2.0))
  
  (setq yFaceA (get-y-on-curve curveObj archX_A))
  (setq yFaceB (get-y-on-curve curveObj archX_B))
  (setq yMid (get-y-on-curve curveObj midX))

  (if (and yFaceA yFaceB yMid (> eh 0) (> em 0)) 
    (progn
      (setq yArc1 (* eh cad_sc))
      (setq yArc2 (* em cad_sc))
      (make-arc-3p (list archX_A (- yFaceA yArc1) 0.0) (list midX (- yMid yArc2) 0.0) (list archX_B (- yFaceB yArc1) 0.0) lay)
    )
  )
)

;; =========================================================================
;; ★ 7. 메인 명령어 로직
;; =========================================================================
(defun c:YBRIDGE ( / doc dcl_id what_next ss pt currX i item sType spanLen db data cFile mirror currY oldCmd yOffCAD temp_curve hgap hgap_val
                     dim1A dim2A dim3A dim4A dim5A eh em ei ghf ghb nextType offCAD groundY limitGL limitGL_Y2 limitBR pilesX strD_GL strD_BR max_str curr_limit_Y curr_br_Y absX brY req_str targetY
                     lastEnt ssSub ssSup ssMaster blkPrefix blkCnt bName mName nextItem nextFile nextData nextMirror cPt numsGL fTopLineData fTopHalfW fTopY H1_current H_req factor ceilFactor H1_new px cad_sc
                     pile_lines draw_lines gap_lines pile_bottom_Y y_a y_b min_y br_module s_front s_back s_max curr_actual_pile_tip_Y tmp_ghf min_target_Y x1 y1 x2 y2 ptx loop hgap1 hgap1_val)
  
  ;; ★ [추가] 기간 만료 체크 (20261231 = 2026년 12월 31일까지 작동)
  (if (<= (fix (getvar "CDATE")) 20261231)
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
        
        (set_tile "eb_sc" *yb_sc*) 
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
          (setq cad_sc (* 1000.0 (atof *yb_sc*)))
          (setq offCAD (* (atof *yb_off*) cad_sc))
          (setq yOffCAD (* (atof *yb_yoff*) cad_sc))
          
          ;; ★ 보정값1(일괄적용) 값 확보
          (setq hgap1 (parse-num *yb_hgap1*))
          (setq hgap1_val (* hgap1 cad_sc))
          
          (setq currX (+ (car *yb_stPt*) offCAD))
          
          (setq temp_curve (vla-copy *yb_curve*))
          (vla-Move temp_curve (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point (list 0.0 yOffCAD 0.0)))
          
          (setq blkPrefix (rtos (* (getvar "CDATE") 1000000) 2 0))
          (setq blkCnt 1)
          (setq ssMaster (ssadd))
          (setq i 0)

          (while (< i (length *yb_spans*))
            (setq item (nth i *yb_spans*))
            (setq sType (car item))
            (setq spanLen (* (nth 1 item) cad_sc))
            
            ;; ★ 보정값2(개별적용) 값 확보
            (setq hgap (nth 2 item))
            
            (setq ghf (if (nth 3 item) (nth 3 item) 0.0))
            (setq ghb (if (nth 4 item) (nth 4 item) 0.0))
            
            (setq dim1A (nth 5 item) dim2A (nth 6 item) dim3A (nth 7 item) dim4A (nth 8 item) dim5A (nth 9 item))
            (setq eh (if (nth 10 item) (nth 10 item) 0.0))
            (setq em (if (nth 11 item) (nth 11 item) 0.0))
            (setq ei (* (if (nth 12 item) (nth 12 item) 0.0) cad_sc))
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
                     (setq limitGL (* (nth 0 numsGL) cad_sc))
                     (setq limitGL_Y2 (* (nth 1 numsGL) cad_sc))
                     (setq fTopLineData (list (nth 2 numsGL) (nth 3 numsGL) (nth 4 numsGL) (nth 5 numsGL)))
                    )
                    ((= (length numsGL) 5)
                     (setq limitGL (* (nth 0 numsGL) cad_sc))
                     (setq limitGL_Y2 0.0)
                     (setq fTopLineData (list (nth 1 numsGL) (nth 2 numsGL) (nth 3 numsGL) (nth 4 numsGL)))
                    )
                    (t
                     (setq limitGL (* (car numsGL) cad_sc))
                     (if (> (length numsGL) 1) (setq fTopLineData (cdr numsGL)))
                    )
                  )
                )
                
                (setq numsBR (flatten-nums (list (get-data-val 'STR_LIMIT_BR data))))
                (setq limitBR (if (car numsBR) (* (car numsBR) cad_sc) nil))
                
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
                            
                            (setq y_a (* y1 cad_sc))
                            (setq y_b (* y2 cad_sc))
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
                    (setq fTopHalfW (* (max (abs (if (nth 0 fTopLineData) (nth 0 fTopLineData) 0.0)) (abs (if (nth 2 fTopLineData) (nth 2 fTopLineData) 0.0))) cad_sc))
                    (setq fTopY (* (min (if (nth 1 fTopLineData) (nth 1 fTopLineData) 0.0) (if (nth 3 fTopLineData) (nth 3 fTopLineData) 0.0)) cad_sc))
                    (setq groundY (get-min-ground-y *yb_gl* currX fTopHalfW))
                    
                    (if groundY
                      (progn
                        (setq H1_current (abs (- fTopY limitGL_Y2)))
                        (setq h_max_val (max (* ghf cad_sc) (* ghb cad_sc)))
                        (setq H_req (- (+ currY limitGL_Y2 (- h_max_val)) groundY))
                        (setq H_current_compressed (- H1_current h_max_val))
                        (if (< H_req H_current_compressed) (setq H_req H_current_compressed))
                        
                        (setq module_size (* 0.500 cad_sc))
                        (setq factor (/ H_req module_size))
                        (setq ceilFactor (if (equal (fix factor) factor 1e-5) (fix factor) (+ (fix factor) 1)))
                        (setq H1_new (* ceilFactor module_size))
                        (setq strD_GL (- H1_new H_current_compressed))
                        (if (< strD_GL 0.0) (setq strD_GL 0.0))
                      )
                    )
                  )
                  (if (and limitGL *yb_gl*)
                    (progn
                      (setq groundY (get-min-ground-y *yb_gl* currX (* 1.700 cad_sc)))
                      (if groundY 
                        (progn 
                          (setq curr_limit_Y (+ currY limitGL)) 
                          (if (> curr_limit_Y groundY) 
                            (progn
                              (setq module_size (* 0.500 cad_sc))
                              (setq strD_GL (* (fix (+ (/ (- curr_limit_Y groundY) module_size) 0.999)) module_size))
                            )
                          )
                        )
                      )
                    )
                  )
                )

                ;; ★ 보정값1(일괄)과 보정값2(개별)를 모두 더해서 최종 스트레치 적용
                (setq hgap_val (* (if hgap hgap 0.0) cad_sc))
                (setq strD_GL (+ strD_GL hgap1_val hgap_val))

                (if (and limitBR pilesX pile_bottom_Y *yb_br*) 
                  (progn
                    (setq min_target_Y nil)
                    
                    (foreach px pilesX
                      (setq absX (+ currX (* mirror px cad_sc)))
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
                        
                        (setq br_module (* 0.500 cad_sc))
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
                (draw-cloud-structure (list currX currY 0.0) draw_lines strD_GL strD_BR mirror cad_sc limitGL limitBR ghf ghb s_front s_back s_max)
                (setq ssSub (get-ents-after lastEnt))
                (if ssSub (add-ss-to-ss ssSub ssMaster))

                (if (and (> spanLen 0.0) (not (wcmatch sType "*A2*"))) 
                  (progn
                    (setq nextType "일반 (15m)")
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
                    
                    (draw-upper-structure temp_curve currX dim1A dim2A dim3A dim4A dim5A mirror (+ currX spanLen) dim1B dim2B dim3B dim4B dim5B nextMirror cad_sc eh em data nextData)
                    
                    (setq ssSup (get-ents-after lastEnt))
                    (if ssSup (add-ss-to-ss ssSup ssMaster))
                  )
                )
                (princ (strcat "\n-> [" sType "] 작도 완료."))
              )
            )
            (setq currX (+ currX spanLen))
            (setq i (1+ i))
          )

          (if (> (sslength ssMaster) 0) 
            (progn
              (setq mName (strcat "YB_TOTAL_BRIDGE_" blkPrefix))
              (command "._-BLOCK" mName "_non" *yb_stPt* ssMaster "")
              (command "._-INSERT" mName "_non" *yb_stPt* 1 1 0)
            )
          )
          
          (if temp_curve (vla-Delete temp_curve))
          
          (vla-EndUndoMark doc)
          (princ "\n*** 작도 완료! (Ctrl+Z 1번으로 되돌리기 가능) ***\n")
        )
      )
      (setvar "CMDECHO" oldCmd) 
    )
  ) ;; 기간 만료 if 닫힘
  (princ)
)