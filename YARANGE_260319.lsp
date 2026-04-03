(defun c:YARRANGE ( / *error* acDoc space ss-path path-ent ent-type 
                      start-pt-ucs start-pt-wcs pt-on-curve start-dist 
                      dir-pt-ucs dir-pt-wcs dir-pt-on-curve dir-dist dir-sign total-len
                      sel-objs base-pt-ucs base-pt-wcs obj-dir-pt-ucs obj-dir-pt-wcs obj-base-angle 
                      align-opt start-offset pattern-str spacing-list 
                      items item pos count val i cur-dist p param d path-tangent-angle path-angle rot-angle e o n
                      cur-date limit-date use-prev) ; use-prev 지역변수 추가
  (vl-load-com)

  ;; ==========================================
  ;; [0단계] 사용 기한 설정 (Date Limit)
  ;; ==========================================
  (setq cur-date (atof (rtos (getvar "CDATE") 2 6)))
  
  ;; ★ 여기에 만료일을 YYYYMMDD.0 형식으로 입력하세요. (현재: 2026년 12월 31일)
  (setq limit-date 20271231.0) 
  
  ;; 날짜가 기한 내일 때만 아래의 메인 로직 전체를 실행 (만료 시 아무 반응 없음)
  (if (<= cur-date limit-date)
    (progn
      ;; ==========================================
      ;; 1. 에러 핸들러 및 Undo 시작
      ;; ==========================================
      (defun *error* (msg)
        (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
          (princ (strcat "\n[Error] Lisp stopped: " msg))
        )
        (if acDoc (vla-EndUndoMark acDoc))
        (princ)
      )

      (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))
      (vla-StartUndoMark acDoc)

      ;; ==========================================
      ;; [1단계] 경로 정보 입력
      ;; ==========================================
      
      (princ "\n(Step 1/9) 경로가 될 선(Line, Polyline, Arc 등)을 선택하세요: ")
      (setq ss-path (ssget '((0 . "LINE,*POLYLINE,ARC,CIRCLE,SPLINE,ELLIPSE"))))
      (if (not ss-path) (progn (princ "\n[취소됨] 경로를 선택하지 않았습니다.") (exit)))
      (setq path-ent (ssname ss-path 0))

      (setq start-pt-ucs (getpoint "\n(Step 2/9) 경로 위에서 '배치를 시작할 위치'를 지정하세요: "))
      (if (not start-pt-ucs) (progn (princ "\n[취소됨] 시작점을 지정하지 않았습니다.") (exit)))
      (setq start-pt-wcs (trans start-pt-ucs 1 0))
      
      (setq pt-on-curve (vlax-curve-getClosestPointTo path-ent start-pt-wcs))
      (setq start-dist (vlax-curve-getDistAtPoint path-ent pt-on-curve))

      (setq dir-pt-ucs (getpoint start-pt-ucs "\n(Step 3/9) '배치가 진행될 방향'을 지정하세요: "))
      (if (not dir-pt-ucs) (progn (princ "\n[취소됨] 진행 방향을 지정하지 않았습니다.") (exit)))
      (setq dir-pt-wcs (trans dir-pt-ucs 1 0))
      
      (setq dir-pt-on-curve (vlax-curve-getClosestPointTo path-ent dir-pt-wcs))
      (setq dir-dist (vlax-curve-getDistAtPoint path-ent dir-pt-on-curve))
      
      (if (>= dir-dist start-dist)
        (setq dir-sign 1.0)
        (setq dir-sign -1.0)
      )

      ;; ==========================================
      ;; [2단계] 객체 정보 입력 (이전 설정 재사용 로직 추가)
      ;; ==========================================
      (setq use-prev "N")
      
      ;; 이전에 저장된 데이터(*YARR-PREV-...*)가 존재하는지 확인
      (if (and *YARR-PREV-OBJS* (= (type *YARR-PREV-OBJS*) 'PICKSET) 
               *YARR-PREV-BASE-WCS* *YARR-PREV-ANGLE*)
        (progn
          (initget "Y N")
          (setq use-prev (getkword "\n(Step 4/9) 이전에 배열했던 객체와 설정을 그대로 사용하시겠습니까? [Y/N] <Y>: "))
          (if (null use-prev) (setq use-prev "Y"))
        )
      )

      (if (= use-prev "Y")
        ;; Y를 선택했을 때: 이전 값 불러오기
        (progn
          (setq sel-objs *YARR-PREV-OBJS*
                base-pt-wcs *YARR-PREV-BASE-WCS*
                obj-base-angle *YARR-PREV-ANGLE*)
          (princ "\n▶ 이전 객체 설정을 성공적으로 불러왔습니다.")
        )
        ;; N을 선택했거나 처음 실행할 때: 새로 지정하기
        (progn
          (princ "\n(Step 4/9) 배열할 객체(블럭, 선 등)를 선택하세요: ")
          (setq sel-objs (ssget))
          (if (not sel-objs) (progn (princ "\n[취소됨] 객체를 선택하지 않았습니다.") (exit)))

          (setq base-pt-ucs (getpoint "\n(Step 5/9) 선택한 객체의 '기준점(중심점)'을 지정하세요: "))
          (if (not base-pt-ucs) (progn (princ "\n[취소됨] 기준점을 지정하지 않았습니다.") (exit)))
          (setq base-pt-wcs (trans base-pt-ucs 1 0)) 

          (setq obj-dir-pt-ucs (getpoint base-pt-ucs "\n(Step 6/9) 이 객체의 '진행 방향(정면)'을 지정하세요: "))
          (if (not obj-dir-pt-ucs) (progn (princ "\n[취소됨] 객체의 방향을 지정하지 않았습니다.") (exit)))
          (setq obj-dir-pt-wcs (trans obj-dir-pt-ucs 1 0))
          
          (setq obj-base-angle (angle base-pt-wcs obj-dir-pt-wcs))
          
          ;; 다음 실행 시 사용할 수 있도록 전역 변수에 현재 설정 저장
          (setq *YARR-PREV-OBJS* sel-objs
                *YARR-PREV-BASE-WCS* base-pt-wcs
                *YARR-PREV-ANGLE* obj-base-angle)
        )
      )

      ;; ==========================================
      ;; [3단계] 배치 옵션 및 간격 입력
      ;; ==========================================

      (initget "Y N")
      (setq align-opt (getkword "\n(Step 7/9) 경로의 진행 방향에 맞춰 객체를 회전하시겠습니까? [Y/N] <Y>: "))
      (if (null align-opt) (setq align-opt "Y"))

      (setq start-offset (getreal "\n(Step 8/9) (선택사항) 시작점으로부터의 이격거리를 입력하세요 <0>: "))
      (if (null start-offset) (setq start-offset 0.0))

      (setq pattern-str (getstring t "\n▶ (Step 9/9) 배치 간격을 입력하세요 (예: 200, 3@500, 300): "))
      (if (or (= pattern-str "") (= pattern-str nil))
        (progn (princ "\n[취소됨] 간격이 입력되지 않았습니다.") (exit))
      )

      (setq spacing-list '())
      (while (vl-string-search " " pattern-str)
        (setq pattern-str (vl-string-subst "" " " pattern-str))
      )
      
      (defun split-str (s delim / temp-p)
        (if (setq temp-p (vl-string-search delim s))
          (cons (substr s 1 temp-p) (split-str (substr s (+ temp-p 1 (strlen delim))) delim))
          (list s)
        )
      )
      
      (setq items (split-str pattern-str ","))
      (foreach item items
        (if (setq pos (vl-string-search "@" item))
          (progn
            (setq count (atoi (substr item 1 pos)))
            (setq val (atof (substr item (+ pos 2))))
            (setq i 0)
            (while (< i count)
              (setq spacing-list (cons val spacing-list))
              (setq i (1+ i))
            )
          )
          (setq spacing-list (cons (atof item) spacing-list))
        )
      )
      (setq spacing-list (reverse spacing-list))


      ;; ==========================================
      ;; [4단계] 객체 배치 실행
      ;; ==========================================
      
      (setq total-len (vlax-curve-getDistAtParam path-ent (vlax-curve-getEndParam path-ent)))
      (setq cur-dist (+ start-dist (* start-offset dir-sign)))

      (defun do-place (dist / p param d path-tangent-angle path-angle rot-angle i e o n)
        (if (and (>= dist 0.0) (<= dist total-len))
          (progn
            (setq p (vlax-curve-getPointAtDist path-ent dist))
            (setq param (vlax-curve-getParamAtDist path-ent dist))
            (setq d (vlax-curve-getFirstDeriv path-ent param))
            
            (setq path-tangent-angle (if (and d (not (equal d '(0.0 0.0 0.0) 1e-8))) (angle '(0.0 0.0 0.0) d) 0.0))
            
            (if (= dir-sign -1.0)
              (setq path-angle (+ path-tangent-angle pi))
              (setq path-angle path-tangent-angle)
            )
            
            (setq rot-angle (- path-angle obj-base-angle))
            
            (setq i 0)
            (while (< i (sslength sel-objs))
              (setq e (ssname sel-objs i))
              (setq o (vlax-ename->vla-object e))
              
              (setq n (vla-copy o))
              (vla-move n (vlax-3d-point base-pt-wcs) (vlax-3d-point p))
              
              (if (= align-opt "Y")
                (vla-rotate n (vlax-3d-point p) rot-angle)
              )
              (setq i (1+ i))
            )
          )
        )
      )

      (do-place cur-dist)
      
      (foreach step spacing-list
        (setq cur-dist (+ cur-dist (* step dir-sign)))
        (do-place cur-dist)
      )

      (vla-EndUndoMark acDoc)
      (princ "\n[완료] YARRANGE - 객체 배치가 완료되었습니다.")
    )
  )

  (princ)
)









(defun c:YBC ( / *error* acDoc ss-target ent-new new-name add-angle ref-scale old-dimzin scale-str input-scale i ent edata cur-rot new-rot)
  (vl-load-com)

  ;; ==========================================
  ;; 1. 에러 핸들러 및 Undo 시작
  ;; ==========================================
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\n[Error] Lisp stopped: " msg))
    )
    (if acDoc (vla-EndUndoMark acDoc))
    (princ)
  )

  (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-StartUndoMark acDoc)

  ;; ==========================================
  ;; 2. 대상 블록 및 교체할 원본 블록 선택
  ;; ==========================================
  (princ "\n(Step 1/4) 교체 대상 블록들을 모두 선택하세요: ")
  (setq ss-target (ssget '((0 . "INSERT"))))
  
  (if ss-target
    (progn
      (setq ent-new (car (entsel "\n(Step 2/4) 새로운 블록을 선택하세요: ")))
      
      (if (and ent-new (= (cdr (assoc 0 (entget ent-new))) "INSERT"))
        (progn
          ;; 새로운 블록의 이름 추출
          (setq new-name (cdr (assoc 2 (entget ent-new))))
          
          ;; ==========================================
          ;; 3. 추가 회전 각도 입력
          ;; ==========================================
          (setq add-angle (getreal "\n(Step 3/4) 추가로 회전할 각도를 입력하세요 (도 단위, 없으면 0 입력) <0>: "))
          (if (null add-angle) (setq add-angle 0.0))
          
          ;; 사용자가 입력한 도(Degree) 단위를 캐드 내부 연산을 위해 라디안(Radian)으로 변환
          (setq add-angle (* pi (/ add-angle 180.0)))
          
          ;; ==========================================
          ;; 4. 블록 스케일 입력 (소수점 최적화)
          ;; ==========================================
          ;; 첫 번째 대상 블록의 X 스케일 값을 가져옵니다.
          (setq ref-scale (cdr (assoc 41 (entget (ssname ss-target 0)))))
          
          ;; 불필요한 뒤쪽 0을 제거하기 위해 DIMZIN 시스템 변수 임시 제어
          (setq old-dimzin (getvar "DIMZIN"))
          (setvar "DIMZIN" 8) ; 소수점 이하의 쓸데없는 0 숨김 처리
          (setq scale-str (rtos ref-scale 2 6)) ; 최대 6자리까지 표현
          (setvar "DIMZIN" old-dimzin) ; 설정 원래대로 복구
          
          ;; 최적화된 문자열(scale-str)을 프롬프트에 출력
          (setq input-scale (getreal (strcat "\n(Step 4/4) 적용할 스케일 값을 입력하세요 <" scale-str ">: ")))
          
          ;; ==========================================
          ;; 5. 블록 교체, 회전 및 스케일 실행
          ;; ==========================================
          (setq i 0)
          (while (< i (sslength ss-target))
            (setq ent (ssname ss-target i))
            (setq edata (entget ent))
            
            ;; --- 회전 각도 처리 ---
            (setq cur-rot (cdr (assoc 50 edata)))
            (if (not cur-rot) (setq cur-rot 0.0))
            (setq new-rot (+ cur-rot add-angle))
            
            ;; 블록 이름(DXF 2) 및 각도(DXF 50) 덮어쓰기
            (setq edata (subst (cons 2 new-name) (assoc 2 edata) edata))
            (setq edata (subst (cons 50 new-rot) (assoc 50 edata) edata))
            
            ;; --- 스케일 처리 ---
            ;; input-scale이 nil이 아닐 때만(사용자가 숫자를 입력했을 때만) 일괄 변경.
            (if input-scale
              (progn
                (setq edata (subst (cons 41 input-scale) (assoc 41 edata) edata)) ; X 스케일
                (setq edata (subst (cons 42 input-scale) (assoc 42 edata) edata)) ; Y 스케일
                (setq edata (subst (cons 43 input-scale) (assoc 43 edata) edata)) ; Z 스케일
              )
            )
            
            ;; 변경된 데이터 도면에 적용 및 업데이트
            (entmod edata)
            (entupd ent)
            
            (setq i (1+ i))
          )
          (princ (strcat "\n[완료] YBC - 총 " (itoa (sslength ss-target)) "개의 블록이 성공적으로 교체 및 수정되었습니다."))
        )
        (princ "\n[오류] 선택한 객체가 블록이 아니거나 선택이 취소되었습니다.")
      )
    )
    (princ "\n[취소됨] 대상 블록을 선택하지 않았습니다.")
  )

  (vla-EndUndoMark acDoc)
  (princ)
)