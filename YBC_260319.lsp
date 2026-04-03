(defun c:YBC ( / *error* acDoc ss-target ent-new new-name add-angle ref-scale old-dimzin scale-str input-scale i ent edata cur-rot new-rot cur-date limit-date)
  (vl-load-com)

  ;; ==========================================
  ;; [0단계] 사용 기한 설정 (Date Limit)
  ;; ==========================================
  (setq cur-date (atof (rtos (getvar "CDATE") 2 6)))
  
  ;; ★ 여기에 만료일을 YYYYMMDD.0 형식으로 입력하세요. (예: 2026년 12월 31일)
  (setq limit-date 20271231.0) 
  
  ;; 날짜가 기한 내일 때만 아래의 메인 로직 전체를 실행
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
    )
  )
  (princ)
)