(defun c:ydim ( / *error* sel dim-sel ref-obj direction offset text-offset tier-count convert-at coords i obj verts j x y startPt endPt cen cx cy direction-list coord_xy1 coord_xy2 dim_po_base max-internal tier1-dims k acDoc space cur-date limit-date offset2 text-offset2 done valid-ref dim-style dim-layer dim-color dim-text-size dim-arrow-size)
  
  (vl-load-com)

  ;; --------------------------------------------------------
  ;; 1. Date Limit Routine
  ;; --------------------------------------------------------
  (setq cur-date (atof (rtos (getvar "CDATE") 2 6)))
  (setq limit-date 20260630.0)
  
  (if (> cur-date limit-date)
    (progn
      (alert "License Expired (2026-06-30).\nPlease contact the administrator.")
      (exit)
    )
  )

  ;; --------------------------------------------------------
  ;; 2. Error Handler & Environment
  ;; --------------------------------------------------------
  (defun *error* (msg)
    (setvar "NOMUTT" 0) 
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\n[Error] Lisp stopped: " msg))
    )
    (if acDoc (vla-EndUndoMark acDoc))
    (princ)
  )

  (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark acDoc)

  (setq space (if (= (getvar 'cvport) 1)
                  (vla-get-paperspace acDoc)
                  (vla-get-modelspace acDoc)
            ))

  ;; --------------------------------------------------------
  ;; 3. Main Logic
  ;; --------------------------------------------------------
  (prompt "\nSelect objects (Line, Polyline, Circle): ")
  (setq sel (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE"))))

  (if sel
    (progn
      ;; (A) Select Reference Dimension (속성 값 저장 방식)
      (setq done nil)
      (setq valid-ref nil)
      
      (while (not done)
        (prompt "\nSelect reference dimension <Use Last Settings>: ")
        (setvar "NOMUTT" 1) 
        (setq dim-sel (ssget ":S" '((0 . "DIMENSION")))) ;; 단일 선택 즉시 종료
        (setvar "NOMUTT" 0) 
        
        (cond
          ;; 1. 치수선을 직접 선택한 경우 -> 속성을 전역 변수에 백업
          ((and dim-sel (> (sslength dim-sel) 0))
            (setq ref-obj (vlax-ename->vla-object (ssname dim-sel 0)))
            
            ;; [핵심] 객체가 아닌 '값'을 전역 변수에 저장
            (setq *gl-ydim-style* (vla-get-StyleName ref-obj))
            (setq *gl-ydim-layer* (vla-get-Layer ref-obj))
            (setq *gl-ydim-color* (vla-get-Color ref-obj))
            (setq *gl-ydim-text-height* (vla-get-TextHeight ref-obj))
            (setq *gl-ydim-arrow-size* (vla-get-ArrowheadSize ref-obj))
            
            (prompt "\nReference properties saved.")
            (setq valid-ref t)
            (setq done t)
          )
          ;; 2. 엔터 입력 (선택 없음) & 기존 설정 있음 -> 전역 변수 사용
          ((and (null dim-sel) (boundp '*gl-ydim-style*) *gl-ydim-style*)
            (prompt "\nUsing last settings.")
            (setq valid-ref t)
            (setq done t)
          )
          ;; 3. 엔터 입력 & 기존 설정 없음 -> 재요청
          (t
            (prompt "\nNo saved settings. Please select a dimension.")
          )
        )
      )
      
      ;; (B) Apply Properties to Local Variables (전역 변수 -> 지역 변수)
      (if valid-ref
        (progn
          (setq dim-style *gl-ydim-style*)
          (setq dim-layer *gl-ydim-layer*)
          (setq dim-color *gl-ydim-color*)
          (setq dim-text-size *gl-ydim-text-height*)
          (setq dim-arrow-size *gl-ydim-arrow-size*)
        )
      )

      ;; (C) User Inputs (Safe Defaults)
      (setq direction (get-direction-with-default))
      (setq offset (get-offset-with-default))
      (setq text-offset (get-text-offset-with-default))
      (setq tier-count (get-tier-count-with-default))
      (setq convert-at (get-convert-at-with-default))
      
      ;; (D) Collect Coordinates
      (setq coords (list))
      (setq i 0)
      (while (< i (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel i)))
        (setq obj-type (vla-get-ObjectName obj))

        (cond
          ;; Polyline
          ((vlax-property-available-p obj 'Coordinates)
            (setq verts (vlax-get obj 'Coordinates))
            (if (= (type verts) 'VARIANT) (setq verts (vlax-variant-value verts)))
            (if (= (type verts) 'SAFEARRAY) (setq verts (vlax-safearray->list verts)))
            
            (if (listp verts)
              (progn
                (setq j 0)
                (while (< j (length verts))
                  (setq x (convert-to-precise (nth j verts)))
                  (setq y (convert-to-precise (nth (1+ j) verts)))
                  (setq coords (cons (list x y) coords))
                  (setq j (+ j 2))
                )
              )
            )
          )
          ;; Line
          ((= obj-type "AcDbLine")
            (setq startPt (vlax-get obj 'StartPoint))
            (setq endPt (vlax-get obj 'EndPoint))
            (setq coords (cons (list (convert-to-precise (nth 0 startPt)) (convert-to-precise (nth 1 startPt))) coords))
            (setq coords (cons (list (convert-to-precise (nth 0 endPt)) (convert-to-precise (nth 1 endPt))) coords))
          )
          ;; Circle
          ((= obj-type "AcDbCircle")
            (setq cen (vlax-get obj 'Center))
            (setq cx (convert-to-precise (nth 0 cen)))
            (setq cy (convert-to-precise (nth 1 cen)))
            (setq coords (cons (list cx cy) coords))
          )
        )
        (setq i (+ i 1))
      )

      ;; (E) Process Drawing
      (if (> (length coords) 0)
        (progn
          (setq direction-list 
            (cond
              ((= direction 0) '(1 2 3 4))
              ((= direction 13) '(1 3)) ((= direction 14) '(1 4))
              ((= direction 23) '(2 3)) ((= direction 24) '(2 4))
              (t (list direction))
            )
          )
          
          (foreach dir direction-list
            (setq offset2 offset)
            (setq text-offset2 text-offset)
            (if (or (= dir 2) (= dir 3))
              (setq offset2 (* -1 offset) text-offset2 (* -1 text-offset))
            )

            (setq coord_xy1 (remove-duplicate-coord coords dir))   
            (setq coord_xy2 (if (> (length coord_xy1) 1) (list (car coord_xy1) (nth (1- (length coord_xy1)) coord_xy1)) coord_xy1))
            (setq dim_po_base (calculate-dimension-position coords dir offset2))
            (setq max-internal (max 0 (1- tier-count)))

            (setq tier1-dims '())           
            (setq *collect-tier1* T)
            (if (> max-internal 0)
              (process-dimensions space coord_xy1 dir dim_po_base text-offset2 valid-ref))
            (setq *collect-tier1* NIL)

            (if convert-at
              (convert-tier1-runs space dir dim_po_base text-offset2 valid-ref tier1-dims)
            )

            (setq k 1)
            (while (< k max-internal)
              (process-dimensions space coord_xy1 dir (+ dim_po_base (* k text-offset2)) text-offset2 valid-ref)
              (setq k (1+ k))
            )
            (process-dimensions space coord_xy2 dir (+ dim_po_base (* max-internal text-offset2)) text-offset2 valid-ref)
          )
        )
        (prompt "\n[Info] No valid coordinates found.")
      )
    )
    (prompt "\n[Info] Nothing selected.")
  )
  
  (vla-EndUndoMark acDoc)
  (princ)
)

;; --------------------------------------------------------
;; Helper Functions
;; --------------------------------------------------------

(defun get-direction-with-default ( / valid input)
  (setq valid nil)
  (if (null last-direction) (setq last-direction 1))
  (while (not valid)
    (initget 128)
    (setq input (getint (strcat "\nDirection (1:Up, 2:Down, 3:Left, 4:Right, 13:Up+Left, 14:Up+Right, 23:Down+Left, 24:Down+Right, 0:All) <" (itoa last-direction) ">: ")))
    (if (or (null input) (member input '(1 2 3 4 13 14 23 24 0)))
      (progn (setq valid t) (if input (setq last-direction input)))
      (prompt "\nInvalid input.")
    )
  )
  last-direction
)

(defun get-offset-with-default ( / input)
  (initget 128)
  (if (null last-offset) (setq last-offset 1000.0))
  (setq input (getreal (strcat "\nEnter Offset Value <" (rtos last-offset 2 0) ">: ")))
  (if input (setq last-offset input) last-offset)
)

(defun get-text-offset-with-default ( / input)
  (initget 128)
  (if (null last-text-offset) (setq last-text-offset 1000.0))
  (setq input (getreal (strcat "\nEnter Dimension Line Offset Value <" (rtos last-text-offset 2 0) ">: ")))
  (if input (setq last-text-offset input) last-text-offset)
)

(defun get-tier-count-with-default ( / valid input)
  (setq valid nil)
  (if (null last-tier-count) (setq last-tier-count 2))
  (while (not valid)
    (initget 6)
    (setq input (getint (strcat "\nEnter Number of Tiers (>=1) <" (itoa last-tier-count) ">: ")))
    (cond
      ((null input) (setq valid t)) 
      ((numberp input) (setq last-tier-count input) (setq valid t))
    )
  )
  last-tier-count
)

(defun get-convert-at-with-default ( / input)
  (initget "Y N")
  (if (null last-convert-at) (setq last-convert-at "N"))
  (setq input (getkword (strcat "\nConvert @ Style? [Y/N] <" last-convert-at ">: ")))
  (if input (setq last-convert-at input))
  (equal last-convert-at "Y")
)

(defun convert-to-precise (val)
  (if (numberp val) (atof (rtos val 2 1)) 0.0)
)

(defun remove-duplicate-coord (coords direction / coord_xy x y)
  (setq coord_xy '()) 
  (foreach pt coords
    (setq x (car pt)) (setq y (cadr pt))
    (cond
      ((or (= direction 1) (= direction 2)) 
        (if (not (member x coord_xy)) (setq coord_xy (cons x coord_xy))))
      ((or (= direction 3) (= direction 4)) 
        (if (not (member y coord_xy)) (setq coord_xy (cons y coord_xy))))
    )
  )
  (vl-sort coord_xy '<)
)

(defun calculate-dimension-position (coords direction offset)
  (cond
    ((= direction 1) (+ (apply 'max (mapcar 'cadr coords)) offset))
    ((= direction 2) (+ (apply 'min (mapcar 'cadr coords)) offset))
    ((= direction 3) (+ (apply 'min (mapcar 'car coords)) offset))
    ((= direction 4) (+ (apply 'max (mapcar 'car coords)) offset))
  )
)

;; --------------------------------------------------------
;; Process Dimensions (문자 회피 및 정렬 로직 포함)
;; --------------------------------------------------------
(defun process-dimensions (space coord_xy direction dim_po text-offset apply-props / i pt1 pt2 pt1-final pt2-final text-position new-dim dim-val should-avoid avoid-gap total-count)
  (setq i 0)
  (setq total-count (- (length coord_xy) 1))
  
  ;; 회피 거리: 전역 변수에 저장된 텍스트 크기 활용 (없으면 기본값)
  (setq avoid-gap (if (boundp '*gl-ydim-text-height*) (* *gl-ydim-text-height* 1.5) 250.0))

  (while (< i total-count)
    (setq pt1 (nth i coord_xy))
    (setq pt2 (nth (1+ i) coord_xy))
    
    (setq dim-val (abs (- pt2 pt1)))
    
    ;; [조건] 양 끝단(처음/마지막)이고 & 거리가 250 이하일 때
    (setq should-avoid (and (<= dim-val 250.0) (or (= i 0) (= i (1- total-count)))))

    ;; 치수선 위치
    (setq pt1-final 
      (cond
        ((or (= direction 1) (= direction 2)) (list pt1 dim_po))
        ((or (= direction 3) (= direction 4)) (list dim_po pt1))
      )
    )
    (setq pt2-final 
      (cond
        ((or (= direction 1) (= direction 2)) (list pt2 dim_po))
        ((or (= direction 3) (= direction 4)) (list dim_po pt2))
      )
    )
    
    ;; [텍스트 좌표 계산]
    (setq text-position 
      (cond
        ((or (= direction 1) (= direction 2)) ;; [가로]
          (cond
            ;; 첫번째(왼쪽) & 회피 -> 좌측(-X)
            ((and should-avoid (= i 0)) 
             (list (- pt1 avoid-gap) (+ dim_po text-offset)))
            ;; 마지막(오른쪽) & 회피 -> 우측(+X)
            ((and should-avoid (= i (1- total-count))) 
             (list (+ pt2 avoid-gap) (+ dim_po text-offset)))
            ;; 그 외
            (t 
             (list (/ (+ pt1 pt2) 2.0) (+ dim_po text-offset)))
          )
        )
        ((or (= direction 3) (= direction 4)) ;; [세로]
          (cond
            ;; 첫번째(아래) & 회피 -> 하단(-Y)
            ((and should-avoid (= i 0)) 
             (list (+ dim_po text-offset) (- pt1 avoid-gap)))
            ;; 마지막(위) & 회피 -> 상단(+Y)
            ((and should-avoid (= i (1- total-count))) 
             (list (+ dim_po text-offset) (+ pt2 avoid-gap)))
            ;; 그 외
            (t 
             (list (+ dim_po text-offset) (/ (+ pt1 pt2) 2.0)))
          )
        )
      )
    )
    
    (setq new-dim (vla-adddimaligned space (vlax-3d-point pt1-final) (vlax-3d-point pt2-final) (vlax-3d-point text-position)))
    
    (if *collect-tier1* (setq tier1-dims (cons (list pt1 pt2 new-dim) tier1-dims)))
    
    (if apply-props
      (progn
        (vla-put-StyleName new-dim dim-style)
        (vla-put-Color new-dim dim-color)
        (vla-put-Layer new-dim dim-layer)
        (vla-put-TextHeight new-dim dim-text-size)
        (vla-put-ArrowheadSize new-dim dim-arrow-size)
        
        ;; [회피 및 정렬 속성]
        (if should-avoid
          (progn
            (vla-put-TextInside new-dim :vlax-false)
            ;; 확장선 기준 정렬 (세로 치수에서 위/아래 이동 보장)
            (if (= i 0)
              (vla-put-HorizontalTextPosition new-dim 1) ;; 1 = First Ext Line (왼쪽/아래)
              (vla-put-HorizontalTextPosition new-dim 2) ;; 2 = Second Ext Line (오른쪽/위)
            )
          )
          (progn
            (vla-put-TextInside new-dim :vlax-true)
            (vla-put-HorizontalTextPosition new-dim 0) ;; 0 = Centered
          )
        )
      )
    )
    (setq i (+ i 1))
  )
)

(defun add-commas (s / len i out)
  (if (/= (type s) 'STR) (setq s (vl-princ-to-string s)))
  (setq s (vl-string-right-trim "." (vl-string-translate "," "" s)))
  (setq len (strlen s) i 0 out "")
  (while (< i len)
    (setq out (strcat (substr s (- len i) 1) out))
    (setq i (1+ i))
    (if (and (> i 0) (= 0 (rem i 3)) (< i len))
      (setq out (strcat "," out))
    )
  )
  out
)

(defun convert-tier1-runs (space dir dim_po_base text_offset apply-props tier1_dims / eps groups cur curlen rec s e vla len first last-item n unit total txt newdim)
  (setq eps 1e-6)
  (setq tier1_dims (reverse tier1_dims))
  
  (setq groups '() cur '() curlen nil)
  (foreach rec tier1_dims
    (setq s (car rec)) (setq e (cadr rec)) (setq vla (caddr rec))
    (setq len (abs (- e s)))

    (if (and curlen (<= (abs (- len curlen)) eps))
      (setq cur (append cur (list rec)))
      (progn
        (if cur (setq groups (append groups (list cur))))
        (setq cur (list rec) curlen len)
      )
    )
  )
  (if cur (setq groups (append groups (list cur))))

  (foreach run groups
    (if (> (length run) 1)
      (progn
        (foreach r run (vl-catch-all-apply 'vla-delete (list (caddr r))))
        (setq first (car run))
        (setq last-item (nth (1- (length run)) run)) 
        (setq s (car first))
        (setq e (cadr last-item))

        (process-dimensions space (list s e) dir dim_po_base text_offset apply-props)
        
        (setq n (length run))
        (setq unit (abs (- (cadr (car run)) (car (car run)))))
        (setq total (* n unit))
        (setq txt (strcat (itoa n) "@" (add-commas (rtos unit 2 0)) "=" (add-commas (rtos total 2 0))))
        
        (setq newdim (vlax-ename->vla-object (entlast)))
        (vla-put-TextOverride newdim txt)
      )
    )
  )
)