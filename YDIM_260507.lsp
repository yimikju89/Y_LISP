(defun c:ydim ( / *error* sel dim-sel ref-obj direction offset text-offset tier-count convert-at coords i obj verts j x y startPt endPt cen cx cy direction-list coord_xy1 coord_xy2 dim_po_base max-internal tier1-dims k acDoc space cur-date limit-date offset2 text-offset2 done valid-ref dim-style dim-layer dim-color dim-text-color dim-dimline-color dim-extline-color dim-text-size dim-arrow-size dim-scale-factor dim-prefix dim-suffix dim-text-override dim-linear-scale dim-vert-pos dim-precision dim-dec-sep dim-tz dim-lz dim-text-gap dim-exe dim-exo dim-text-style dim-arrow1-type dim-arrow2-type dim-dle dim-unit-format obj-queue objs-to-delete obj-type expl-list ex-obj)
  
  (vl-load-com)

  ;; --------------------------------------------------------
  ;; 1. Date Limit Routine (메세지 없이 조용히 종료)
  ;; --------------------------------------------------------
  (setq cur-date (atof (rtos (getvar "CDATE") 2 6)))
  (setq limit-date 20260630.0)
  
  (if (> cur-date limit-date)
    (progn
      (exit)
    )
  )

  ;; --------------------------------------------------------
  ;; 2. Error Handler & Environment
  ;; --------------------------------------------------------
  (defun *error* (msg)
    (setvar "NOMUTT" 0) 
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\n[오류] 리습 실행 중단: " msg))
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
  (prompt "\n객체 선택 (선, 폴리선, 원, 블록): ")
  (setq sel (ssget '((0 . "LINE,LWPOLYLINE,CIRCLE,INSERT"))))

  (if sel
    (progn
      ;; (A) Select Reference Dimension
      (setq done nil)
      (setq valid-ref nil)
      
      (while (not done)
        (prompt "\n참조 치수선 선택 <이전 설정 사용>: ")
        (setvar "NOMUTT" 1) 
        (setq dim-sel (ssget ":S" '((0 . "DIMENSION")))) 
        (setvar "NOMUTT" 0) 
        
        (cond
          ;; 1. 치수선을 직접 선택한 경우
          ((and dim-sel (> (sslength dim-sel) 0))
            (setq ref-obj (vlax-ename->vla-object (ssname dim-sel 0)))
            
            (setq *gl-ydim-style* (vla-get-StyleName ref-obj))
            (setq *gl-ydim-layer* (vla-get-Layer ref-obj))
            (setq *gl-ydim-color* (vla-get-Color ref-obj))
            
            (setq *gl-ydim-text-color* (vl-catch-all-apply 'vla-get-TextColor (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-text-color*) (setq *gl-ydim-text-color* *gl-ydim-color*))
            (setq *gl-ydim-dimline-color* (vl-catch-all-apply 'vla-get-DimensionLineColor (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-dimline-color*) (setq *gl-ydim-dimline-color* *gl-ydim-color*))
            (setq *gl-ydim-extline-color* (vl-catch-all-apply 'vla-get-ExtensionLineColor (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-extline-color*) (setq *gl-ydim-extline-color* *gl-ydim-color*))
            
            (setq *gl-ydim-text-style* (vl-catch-all-apply 'vla-get-TextStyle (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-text-style*) (setq *gl-ydim-text-style* "Standard"))
            (setq *gl-ydim-text-height* (vla-get-TextHeight ref-obj))
            (setq *gl-ydim-arrow-size* (vla-get-ArrowheadSize ref-obj))
            (setq *gl-ydim-arrow1-type* (vl-catch-all-apply 'vla-get-Arrowhead1Type (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-arrow1-type*) (setq *gl-ydim-arrow1-type* 0))
            (setq *gl-ydim-arrow2-type* (vl-catch-all-apply 'vla-get-Arrowhead2Type (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-arrow2-type*) (setq *gl-ydim-arrow2-type* 0))
            
            (setq *gl-ydim-scale-factor* (vla-get-ScaleFactor ref-obj))
            (setq *gl-ydim-linear-scale* (vl-catch-all-apply 'vla-get-LinearScaleFactor (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-linear-scale*) (setq *gl-ydim-linear-scale* 1.0))
            
            (setq *gl-ydim-text-override* (vla-get-TextOverride ref-obj))
            (setq *gl-ydim-prefix* (vla-get-TextPrefix ref-obj))
            (setq *gl-ydim-suffix* (vla-get-TextSuffix ref-obj))

            (setq *gl-ydim-unit-format* (vl-catch-all-apply 'vla-get-UnitsFormat (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-unit-format*) (setq *gl-ydim-unit-format* 2)) 
            
            (setq *gl-ydim-vert-pos* (vl-catch-all-apply 'vla-get-VerticalTextPosition (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-vert-pos*) (setq *gl-ydim-vert-pos* 1))
            (setq *gl-ydim-precision* (vl-catch-all-apply 'vla-get-PrimaryUnitsPrecision (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-precision*) (setq *gl-ydim-precision* 0))
            (setq *gl-ydim-dec-sep* (vl-catch-all-apply 'vla-get-DecimalSeparator (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-dec-sep*) (setq *gl-ydim-dec-sep* "."))
            (setq *gl-ydim-tz* (vl-catch-all-apply 'vla-get-SuppressTrailingZeros (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-tz*) (setq *gl-ydim-tz* :vlax-false))
            (setq *gl-ydim-lz* (vl-catch-all-apply 'vla-get-SuppressLeadingZeros (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-lz*) (setq *gl-ydim-lz* :vlax-false))
            
            (setq *gl-ydim-text-gap* (vl-catch-all-apply 'vla-get-TextGap (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-text-gap*) (setq *gl-ydim-text-gap* (getvar "DIMGAP")))
            (setq *gl-ydim-exe* (vl-catch-all-apply 'vla-get-ExtensionLineExtend (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-exe*) (setq *gl-ydim-exe* (getvar "DIMEXE")))
            (setq *gl-ydim-exo* (vl-catch-all-apply 'vla-get-ExtensionLineOffset (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-exo*) (setq *gl-ydim-exo* (getvar "DIMEXO")))
            (setq *gl-ydim-dle* (vl-catch-all-apply 'vla-get-DimensionLineExtend (list ref-obj)))
            (if (vl-catch-all-error-p *gl-ydim-dle*) (setq *gl-ydim-dle* 0.0))

            (prompt "\n[안내] 참조 치수선의 속성을 저장했습니다.")
            (setq valid-ref t)
            (setq done t)
          )
          ;; 2. 엔터 입력 (선택 없음) & 기존 설정 있음
          ((and (null dim-sel) (boundp '*gl-ydim-style*) *gl-ydim-style*)
            (prompt "\n[안내] 이전 설정을 사용합니다.")
            (setq valid-ref t)
            (setq done t)
          )
          ;; 3. 엔터 입력 & 기존 설정 없음
          (t
            (prompt "\n[안내] 저장된 설정이 없습니다. 참조 치수선을 선택해주세요.")
          )
        )
      )
      
      ;; (B) Apply Properties to Local Variables
      (if valid-ref
        (progn
          (setq dim-style *gl-ydim-style*)
          (setq dim-layer *gl-ydim-layer*)
          (setq dim-color *gl-ydim-color*)
          (setq dim-text-color (if *gl-ydim-text-color* *gl-ydim-text-color* dim-color))
          (setq dim-dimline-color (if *gl-ydim-dimline-color* *gl-ydim-dimline-color* dim-color))
          (setq dim-extline-color (if *gl-ydim-extline-color* *gl-ydim-extline-color* dim-color))
          (setq dim-text-style (if (eq (type *gl-ydim-text-style*) 'STR) *gl-ydim-text-style* "Standard"))
          (setq dim-text-size *gl-ydim-text-height*)
          (setq dim-arrow-size *gl-ydim-arrow-size*)
          (setq dim-arrow1-type (if (numberp *gl-ydim-arrow1-type*) *gl-ydim-arrow1-type* 0))
          (setq dim-arrow2-type (if (numberp *gl-ydim-arrow2-type*) *gl-ydim-arrow2-type* 0))
          (setq dim-scale-factor (if *gl-ydim-scale-factor* *gl-ydim-scale-factor* 1.0))
          (setq dim-linear-scale (if *gl-ydim-linear-scale* *gl-ydim-linear-scale* 1.0))
          (setq dim-text-override *gl-ydim-text-override*)
          (setq dim-prefix *gl-ydim-prefix*)
          (setq dim-suffix *gl-ydim-suffix*)
          (setq dim-unit-format (if (numberp *gl-ydim-unit-format*) *gl-ydim-unit-format* 2))
          (setq dim-vert-pos (if *gl-ydim-vert-pos* *gl-ydim-vert-pos* 1))
          (setq dim-precision (if (numberp *gl-ydim-precision*) *gl-ydim-precision* 0))
          (setq dim-dec-sep (if (and *gl-ydim-dec-sep* (/= *gl-ydim-dec-sep* "")) *gl-ydim-dec-sep* "."))
          (setq dim-tz (if *gl-ydim-tz* *gl-ydim-tz* :vlax-false))
          (setq dim-lz (if *gl-ydim-lz* *gl-ydim-lz* :vlax-false))
          (setq dim-text-gap (if (numberp *gl-ydim-text-gap*) *gl-ydim-text-gap* (getvar "DIMGAP")))
          (setq dim-exe (if (numberp *gl-ydim-exe*) *gl-ydim-exe* (getvar "DIMEXE")))
          (setq dim-exo (if (numberp *gl-ydim-exo*) *gl-ydim-exo* (getvar "DIMEXO")))
          (setq dim-dle (if (numberp *gl-ydim-dle*) *gl-ydim-dle* 0.0))
        )
      )

      ;; (C) User Inputs
      (setq direction (get-direction-with-default))
      (setq offset (get-offset-with-default))
      (setq text-offset (get-text-offset-with-default))
      (setq tier-count (get-tier-count-with-default))
      (setq convert-at (get-convert-at-with-default))
      
      ;; (D) Collect Coordinates 
      (setq coords (list))
      (setq obj-queue (list))
      (setq objs-to-delete (list))

      (setq i 0)
      (while (< i (sslength sel))
        (setq obj-queue (cons (vlax-ename->vla-object (ssname sel i)) obj-queue))
        (setq i (+ i 1))
      )

      (while obj-queue
        (setq obj (car obj-queue))
        (setq obj-queue (cdr obj-queue))
        
        (setq obj-type (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
        (if (not (vl-catch-all-error-p obj-type))
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
            ;; Block Reference (INSERT)
            ((= obj-type "AcDbBlockReference")
              (setq expl-list (vl-catch-all-apply 'vla-Explode (list obj)))
              (if (not (vl-catch-all-error-p expl-list))
                (progn
                  (if (= (type expl-list) 'VARIANT) (setq expl-list (vlax-variant-value expl-list)))
                  (if (= (type expl-list) 'SAFEARRAY) (setq expl-list (vlax-safearray->list expl-list)))
                  (if (listp expl-list)
                    (foreach ex-obj expl-list
                      (setq obj-queue (cons ex-obj obj-queue))
                      (setq objs-to-delete (cons ex-obj objs-to-delete))
                    )
                  )
                )
              )
            )
          )
        )
      )

      ;; 사용된 임시 객체 삭제
      (foreach ex-obj objs-to-delete
        (vl-catch-all-apply 'vla-delete (list ex-obj))
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
        (prompt "\n[안내] 유효한 좌표를 찾지 못했습니다.")
      )
    )
    (prompt "\n[안내] 선택된 객체가 없습니다.")
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
    (setq input (getint (strcat "\n방향 (1:상 / 2:하 / 3:좌 / 4:우 / 13:상+좌 / 14:상+우 / 23:하+좌 / 24:하+우 / 0:전체) <" (itoa last-direction) ">: ")))
    (if (or (null input) (member input '(1 2 3 4 13 14 23 24 0)))
      (progn (setq valid t) (if input (setq last-direction input)))
      (prompt "\n잘못된 입력입니다.")
    )
  )
  last-direction
)

(defun get-offset-with-default ( / input)
  (initget 128)
  (if (null last-offset) (setq last-offset 1000.0))
  (setq input (getreal (strcat "\n간격 띄우기 <" (rtos last-offset 2 0) ">: ")))
  (if input (setq last-offset input) last-offset)
)

(defun get-text-offset-with-default ( / input)
  (initget 128)
  (if (null last-text-offset) (setq last-text-offset 1000.0))
  (setq input (getreal (strcat "\n치수선 높이 <" (rtos last-text-offset 2 0) ">: ")))
  (if input (setq last-text-offset input) last-text-offset)
)

(defun get-tier-count-with-default ( / valid input)
  (setq valid nil)
  (if (null last-tier-count) (setq last-tier-count 2))
  (while (not valid)
    (initget 6)
    (setq input (getint (strcat "\n단 수 입력 (1 이상) <" (itoa last-tier-count) ">: ")))
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
  (setq input (getkword (strcat "\n@ 스타일로 변환하시겠습니까? [Y/N] <" last-convert-at ">: ")))
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

(defun add-commas (s sep / pos int-part dec-part len i out)
  (if (/= (type s) 'STR) (setq s (vl-princ-to-string s)))
  (if (setq pos (vl-string-position 46 s)) 
    (progn
      (setq int-part (substr s 1 pos))
      (setq dec-part (substr s (+ pos 2)))
    )
    (progn
      (setq int-part s)
      (setq dec-part "")
    )
  )
  
  (setq len (strlen int-part) i 0 out "")
  (while (< i len)
    (setq out (strcat (substr int-part (- len i) 1) out))
    (setq i (1+ i))
    (if (and (> i 0) (= 0 (rem i 3)) (< i len) (/= (substr int-part (- len i) 1) "-"))
      (setq out (strcat "," out))
    )
  )
  
  (if (> (strlen dec-part) 0)
    (strcat out sep dec-part)
    out
  )
)

;; --------------------------------------------------------
;; Process Dimensions
;; --------------------------------------------------------
(defun process-dimensions (space coord_xy direction dim_po text-offset apply-props / 
                           i pt1 pt2 pt1-final pt2-final text-position new-dim dim-val 
                           should-avoid avoid-gap total-count
                           cur-scale phys-txt-height phys-arrow-size phys-gap 
                           old-dimzin display-val char-count txt-width min-required-space)
  (setq i 0)
  (setq total-count (- (length coord_xy) 1))
  
  (setq cur-scale (if (and dim-scale-factor (> dim-scale-factor 0)) dim-scale-factor (getvar "DIMSCALE")))
  (if (= cur-scale 0.0) (setq cur-scale 1.0))
  (setq phys-txt-height (* dim-text-size cur-scale))
  (setq phys-arrow-size (* dim-arrow-size cur-scale))
  (setq phys-gap (* dim-text-gap cur-scale))

  (while (< i total-count)
    (setq pt1 (nth i coord_xy))
    (setq pt2 (nth (1+ i) coord_xy))
    
    (setq dim-val (abs (- pt2 pt1)))
    
    (setq old-dimzin (getvar "DIMZIN"))
    (setvar "DIMZIN" (if (= dim-tz :vlax-true) 8 0))
    (setq display-val (* dim-val (if dim-linear-scale dim-linear-scale 1.0)))
    (setq char-count (strlen (add-commas (rtos display-val 2 dim-precision) dim-dec-sep)))
    (setvar "DIMZIN" old-dimzin)
    
    (setq txt-width (* char-count phys-txt-height 0.85)) 
    (setq min-required-space (+ txt-width (* phys-arrow-size 2.0) (* phys-gap 2.0)))
    
    (setq should-avoid (and (< dim-val min-required-space) (or (= i 0) (= i (1- total-count)))))
    (setq avoid-gap (+ (/ txt-width 2.0) phys-arrow-size phys-gap))

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
    
    (setq text-position 
      (cond
        ((or (= direction 1) (= direction 2)) 
          (cond
            ((and should-avoid (= i 0)) 
             (list (- pt1 avoid-gap) (+ dim_po text-offset)))
            ((and should-avoid (= i (1- total-count))) 
             (list (+ pt2 avoid-gap) (+ dim_po text-offset)))
            (t 
             (list (/ (+ pt1 pt2) 2.0) (+ dim_po text-offset)))
          )
        )
        ((or (= direction 3) (= direction 4)) 
          (cond
            ((and should-avoid (= i 0)) 
             (list (+ dim_po text-offset) (- pt1 avoid-gap)))
            ((and should-avoid (= i (1- total-count))) 
             (list (+ dim_po text-offset) (+ pt2 avoid-gap)))
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
        (vla-put-Layer new-dim dim-layer)
        (vla-put-Color new-dim dim-color)
        
        (vl-catch-all-apply 'vla-put-TextStyle (list new-dim dim-text-style))
        (vl-catch-all-apply 'vla-put-TextColor (list new-dim dim-text-color))
        (vl-catch-all-apply 'vla-put-DimensionLineColor (list new-dim dim-dimline-color))
        (vl-catch-all-apply 'vla-put-ExtensionLineColor (list new-dim dim-extline-color))
        (vl-catch-all-apply 'vla-put-TextHeight (list new-dim dim-text-size))
        (vl-catch-all-apply 'vla-put-ArrowheadSize (list new-dim dim-arrow-size))
        (vl-catch-all-apply 'vla-put-Arrowhead1Type (list new-dim dim-arrow1-type))
        (vl-catch-all-apply 'vla-put-Arrowhead2Type (list new-dim dim-arrow2-type))
        (vl-catch-all-apply 'vla-put-TextGap (list new-dim dim-text-gap))
        (vl-catch-all-apply 'vla-put-ExtensionLineExtend (list new-dim dim-exe))
        (vl-catch-all-apply 'vla-put-ExtensionLineOffset (list new-dim dim-exo))
        (vl-catch-all-apply 'vla-put-DimensionLineExtend (list new-dim dim-dle))

        (if dim-linear-scale (vl-catch-all-apply 'vla-put-LinearScaleFactor (list new-dim dim-linear-scale)))
        (if dim-scale-factor (vl-catch-all-apply 'vla-put-ScaleFactor (list new-dim dim-scale-factor)))
        
        (vl-catch-all-apply 'vla-put-UnitsFormat (list new-dim dim-unit-format))
        (vl-catch-all-apply 'vla-put-VerticalTextPosition (list new-dim dim-vert-pos))
        (vl-catch-all-apply 'vla-put-PrimaryUnitsPrecision (list new-dim dim-precision))
        (vl-catch-all-apply 'vla-put-DecimalSeparator (list new-dim dim-dec-sep))
        (vl-catch-all-apply 'vla-put-SuppressTrailingZeros (list new-dim dim-tz))
        (vl-catch-all-apply 'vla-put-SuppressLeadingZeros (list new-dim dim-lz))
        
        (if (and dim-prefix (/= dim-prefix "")) (vla-put-TextPrefix new-dim dim-prefix))
        (if (and dim-suffix (/= dim-suffix "")) (vla-put-TextSuffix new-dim dim-suffix))
        (if (and dim-text-override (/= dim-text-override "")) (vla-put-TextOverride new-dim dim-text-override))
        
        (if should-avoid
          (progn
            (vla-put-TextInside new-dim :vlax-false)
            (if (= i 0)
              (vla-put-HorizontalTextPosition new-dim 1)
              (vla-put-HorizontalTextPosition new-dim 2)
            )
          )
          (progn
            (vla-put-TextInside new-dim :vlax-true)
            (vla-put-HorizontalTextPosition new-dim 0)
          )
        )
      )
    )
    (setq i (+ i 1))
  )
)

(defun convert-tier1-runs (space dir dim_po_base text_offset apply-props tier1_dims / eps groups cur curlen rec s e vla len first last-item n unit total txt newdim display-unit display-total old-dimzin new-dimzin)
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
        
        (setq display-unit (* unit (if dim-linear-scale dim-linear-scale 1.0)))
        (setq display-total (* total (if dim-linear-scale dim-linear-scale 1.0)))
        
        (setq old-dimzin (getvar "DIMZIN"))
        (setq new-dimzin 0)
        (if (= dim-lz :vlax-true) (setq new-dimzin (+ new-dimzin 4)))
        (if (= dim-tz :vlax-true) (setq new-dimzin (+ new-dimzin 8)))
        (setvar "DIMZIN" new-dimzin)
        
        (setq txt (strcat (itoa n) "@" (add-commas (rtos display-unit 2 dim-precision) dim-dec-sep) "=" (add-commas (rtos display-total 2 dim-precision) dim-dec-sep)))
        
        (setvar "DIMZIN" old-dimzin)
        
        (cond
          ((and dim-text-override (vl-string-search "<>" dim-text-override))
           (setq txt (vl-string-subst txt "<>" dim-text-override))
          )
          (t
           (if (and dim-prefix (/= dim-prefix "")) (setq txt (strcat dim-prefix txt)))
           (if (and dim-suffix (/= dim-suffix "")) (setq txt (strcat txt dim-suffix)))
          )
        )
        
        (setq newdim (vlax-ename->vla-object (entlast)))
        (vla-put-TextOverride newdim txt)
      )
    )
  )
)