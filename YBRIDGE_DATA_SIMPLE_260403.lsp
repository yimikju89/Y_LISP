(vl-load-com)

(defun c:YBRIDGE_DATA_SIMPLE ( / ptBase modeVal ss i obj oType lay sp ep cx cy rad sang eang 
                                 sx sy ex ey lineStr ptStr ptList outStr add-point rtos3 rtos4
                                 pt1 pt2 minX maxX minY maxY is-inside space boxPts boxObj
                                 coords j pts ints old_dimzin isClosed
                                 rel_minX rel_minY rel_maxX rel_maxY boxStr 
                                 layList layName layData layCol layLT resStr p)
  
  ;; ★ 기간 만료 체크 (2027년 12월 31일까지 작동)
  (if (<= (fix (getvar "CDATE")) 20271231)
    (progn
      ;; 소수점 자릿수 무조건 강제 고정 (끝자리 0 유지)
      (defun rtos3 (v / old_dimzin res)
        (setq old_dimzin (getvar "DIMZIN"))
        (setvar "DIMZIN" 0) 
        (setq res (rtos (float v) 2 3))
        (setvar "DIMZIN" old_dimzin)
        res
      )
      
      (defun rtos4 (v / old_dimzin res)
        (setq old_dimzin (getvar "DIMZIN"))
        (setvar "DIMZIN" 0)
        (setq res (rtos (float v) 2 4))
        (setvar "DIMZIN" old_dimzin)
        res
      )

      ;; 1. 출력 모드 선택
      (initget 1 "1 2 3 4")
      (setq modeVal (getkword "\n출력 모드를 선택하세요 (1:라인정보, 2:좌표스캔, 3:영역추출, 4:레이어정보) [1/2/3/4]: "))

      ;; 2. 모드 1, 2, 3인 경우에만 기준점(원점) 입력 요청
      (if (member modeVal '("1" "2" "3"))
        (progn
          (setq ptBase (getpoint "\nDB 기준점(원점)을 선택하세요: "))
          (if (null ptBase)
            (progn (princ "\n기준점이 선택되지 않았습니다.") (exit))
          )
        )
      )

      ;; ==============================================================
      ;; [모드 1] 라인 정보 추출 (폴리라인을 개별 라인으로 자동 분해)
      ;; ==============================================================
      (if (= modeVal "1")
        (progn
          (princ "\nDB에 추가할 객체들을 선택하세요 (Line, Arc, Polyline): ")
          (setq ss (ssget '((0 . "LINE,ARC,LWPOLYLINE,POLYLINE"))))
          (if ss
            (progn
              (setq i 0)
              (setq lineStr "\n;; ================= 라인 및 호 정보 (DB 포맷) =================\n")
              (while (< i (sslength ss))
                (setq obj (vlax-ename->vla-object (ssname ss i)))
                (setq oType (vla-get-ObjectName obj))
                (setq lay (vla-get-Layer obj))
                
                (cond
                  ;; (1) 일반 라인 처리
                  ((= oType "AcDbLine")
                   (setq sp (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint obj))))
                   (setq ep (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint obj))))
                   (setq lineStr (strcat lineStr "    (\"L\" \"" lay "\" " (rtos3 (- (car sp) (car ptBase))) " " (rtos3 (- (cadr sp) (cadr ptBase))) " " (rtos3 (- (car ep) (car ptBase))) " " (rtos3 (- (cadr ep) (cadr ptBase))) ")\n"))
                  )
                  ;; (2) 아크(호) 처리
                  ((= oType "AcDbArc")
                   (setq cp (vlax-safearray->list (vlax-variant-value (vla-get-Center obj))))
                   (setq lineStr (strcat lineStr "    (\"A\" \"" lay "\" " (rtos3 (- (car cp) (car ptBase))) " " (rtos3 (- (cadr cp) (cadr ptBase))) " " (rtos3 (vla-get-Radius obj)) " " (rtos4 (vla-get-StartAngle obj)) " " (rtos4 (vla-get-EndAngle obj)) ")\n"))
                  )
                  ;; (3) 폴리라인 처리 (선분 분할)
                  ((wcmatch oType "*Polyline")
                   (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
                   (setq isClosed (vla-get-Closed obj))
                   (setq j 0)
                   (while (< j (- (length coords) 2))
                     (setq lineStr (strcat lineStr "    (\"L\" \"" lay "\" " (rtos3 (- (nth j coords) (car ptBase))) " " (rtos3 (- (nth (1+ j) coords) (cadr ptBase))) " " (rtos3 (- (nth (+ j 2) coords) (car ptBase))) " " (rtos3 (- (nth (+ j 3) coords) (cadr ptBase))) ")\n"))
                     (setq j (+ j 2))
                   )
                   ;; 닫힌 폴리라인일 경우 마지막 점과 첫 점을 연결
                   (if (= isClosed :vlax-true)
                     (setq lineStr (strcat lineStr "    (\"L\" \"" lay "\" " (rtos3 (- (nth (- (length coords) 2) coords) (car ptBase))) " " (rtos3 (- (nth (- (length coords) 1) coords) (cadr ptBase))) " " (rtos3 (- (nth 0 coords) (car ptBase))) " " (rtos3 (- (nth 1 coords) (cadr ptBase))) ")\n"))
                   )
                  )
                )
                (setq i (1+ i))
              )
              (setq outStr (strcat lineStr ";; =============================================================\n"))
              (textscr)
              (princ outStr)
              (princ "\n출력이 완료되었습니다. F2 창의 내용을 복사해서 활용하세요.\n")
            )
            (princ "\n객체가 선택되지 않았습니다.")
          )
        )
      )

      ;; ==============================================================
      ;; [모드 2] 영역 내 좌표 스캔 (폴리라인 정점 포함)
      ;; ==============================================================
      (if (= modeVal "2")
        (progn
          (setq ptList '())
          
          ;; 좌표 중복 제거 및 리스트 추가 내부 함수
          (defun add-point (pt / px py rel_pt exist)
            (setq px (- (car pt) (car ptBase)))
            (setq py (- (cadr pt) (cadr ptBase)))
            (setq rel_pt (list (atof (rtos3 px)) (atof (rtos3 py))))
            (setq exist nil)
            (foreach p ptList
              (if (and (< (abs (- (car p) (car rel_pt))) 0.001)
                       (< (abs (- (cadr p) (cadr rel_pt))) 0.001))
                (setq exist T)
              )
            )
            (if (not exist) (setq ptList (append ptList (list rel_pt))))
          )

          (setq pt1 (getpoint "\n추출할 영역의 첫 번째 구석을 클릭하세요: "))
          (if pt1
            (progn
              (setq pt2 (getcorner pt1 "\n반대편 구석을 클릭하세요: "))
              (if pt2
                (progn
                  (setq minX (min (car pt1) (car pt2)) maxX (max (car pt1) (car pt2)))
                  (setq minY (min (cadr pt1) (cadr pt2)) maxY (max (cadr pt1) (cadr pt2)))
                  
                  (defun is-inside (pt)
                    (and (>= (car pt) (- minX 1e-5)) (<= (car pt) (+ maxX 1e-5))
                         (>= (cadr pt) (- minY 1e-5)) (<= (cadr pt) (+ maxY 1e-5)))
                  )
                  
                  (setq ss (ssget "_C" pt1 pt2))
                  (if ss
                    (progn
                      (setq space (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
                      (setq boxPts (vlax-make-safearray vlax-vbDouble '(0 . 7)))
                      (vlax-safearray-fill boxPts (list minX minY maxX minY maxX maxY minX maxY))
                      (setq boxObj (vla-AddLightWeightPolyline space boxPts))
                      (vla-put-Closed boxObj :vlax-true)
                      
                      (setq i 0)
                      (while (< i (sslength ss))
                        (setq obj (vlax-ename->vla-object (ssname ss i)))
                        (setq oType (vla-get-ObjectName obj))
                        (setq pts '())
                        
                        (cond
                          ((= oType "AcDbLine")
                           (setq pts (list (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint obj)))
                                           (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint obj))))))
                          ((= oType "AcDbArc")
                           (setq pts (list (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint obj)))
                                           (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint obj)))
                                           (vlax-safearray->list (vlax-variant-value (vla-get-Center obj))))))
                          ((wcmatch oType "*Polyline")
                           (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
                           (setq j 0)
                           (while (< j (length coords))
                             (setq pts (append pts (list (list (nth j coords) (nth (1+ j) coords) 0.0))))
                             (setq j (+ j 2))))
                          ((= oType "AcDbCircle")
                           (setq pts (list (vlax-safearray->list (vlax-variant-value (vla-get-Center obj))))))
                        )
                        
                        ;; 정점(Vertex) 좌표 추가
                        (foreach p pts (if (is-inside p) (add-point p)))
                        
                        ;; 박스와 교차하는 지점(Intersection) 좌표 추가
                        (if (not (wcmatch oType "*Text*,*Dimension*,*Hatch*,*Block*"))
                          (progn
                            (setq ints (vl-catch-all-apply 'vlax-invoke (list obj 'IntersectWith boxObj 0)))
                            (if (and (not (vl-catch-all-error-p ints)) ints)
                              (progn
                                (setq j 0)
                                (while (< j (length ints))
                                  (add-point (list (nth j ints) (nth (1+ j) ints) (nth (+ j 2) ints)))
                                  (setq j (+ j 3))
                                )
                              )
                            )
                          )
                        )
                        (setq i (1+ i))
                      )
                      
                      (vla-Delete boxObj)
                      
                      ;; 좌표를 X우선, Y차순으로 정렬
                      (setq ptList (vl-sort ptList 
                        (function (lambda (a b) 
                          (if (equal (car a) (car b) 0.001)
                            (< (cadr a) (cadr b))
                            (< (car a) (car b))
                          )
                        ))
                      ))
                      
                      (setq ptStr "\n;; ================= [영역 내] 상대 좌표 (X Y) =================\n")
                      (setq ptStr (strcat ptStr "  (\n"))
                      (foreach p ptList
                        (setq ptStr (strcat ptStr "    (" (rtos3 (car p)) " " (rtos3 (cadr p)) ")\n"))
                      )
                      (setq ptStr (strcat ptStr "  )\n;; ===============================================================\n"))
                      
                      (textscr)
                      (princ ptStr)
                      (princ "\n출력이 완료되었습니다. F2 창의 내용을 복사해서 활용하세요.\n")
                    )
                    (princ "\n선택된 영역 내에 객체가 없습니다.")
                  )
                )
              )
            )
          )
        )
      )

      ;; ==============================================================
      ;; [모드 3] 스트레치용 영역(Box) 좌표 추출
      ;; ==============================================================
      (if (= modeVal "3")
        (progn
          (setq pt1 (getpoint "\n스트레치 영역의 첫 번째 구석을 클릭하세요: "))
          (if pt1
            (progn
              (setq pt2 (getcorner pt1 "\n반대편 구석을 클릭하세요: "))
              (if pt2
                (progn
                  (setq rel_minX (min (- (car pt1) (car ptBase)) (- (car pt2) (car ptBase))))
                  (setq rel_maxX (max (- (car pt1) (car ptBase)) (- (car pt2) (car ptBase))))
                  (setq rel_minY (min (- (cadr pt1) (cadr ptBase)) (- (cadr pt2) (cadr ptBase))))
                  (setq rel_maxY (max (- (cadr pt1) (cadr ptBase)) (- (cadr pt2) (cadr ptBase))))
                  
                  (setq boxStr "\n;; ================= [3] 스트레치 영역(Box) 좌표 =================\n")
                  (setq boxStr (strcat boxStr "  (" (rtos3 rel_minX) " " (rtos3 rel_minY) " " (rtos3 rel_maxX) " " (rtos3 rel_maxY) ")\n"))
                  (setq boxStr (strcat boxStr ";; ===============================================================\n"))
                  
                  (textscr)
                  (princ boxStr)
                  (princ "\n출력이 완료되었습니다. 복사하여 DB에 붙여넣으세요.\n")
                )
              )
            )
          )
        )
      )

      ;; ==============================================================
      ;; [모드 4] 레이어 정보 추출 (DB 리스트 포맷)
      ;; ==============================================================
      (if (= modeVal "4")
        (progn
          (princ "\n레이어 정보를 확인할 객체들을 선택하세요: ")
          (setq ss (ssget))
          (if ss
            (progn
              (setq i 0 layList '())
              
              ;; 중복 없이 레이어 이름 수집
              (while (< i (sslength ss))
                (setq layName (cdr (assoc 8 (entget (ssname ss i)))))
                (if (not (member layName layList))
                  (setq layList (cons layName layList))
                )
                (setq i (1+ i))
              )
              
              ;; 알파벳 순 정렬
              (setq layList (vl-sort layList '<))
              
              ;; 출력 헤더 구성
              (setq resStr "\n;; ====================== 레이어 정보 (DB 포맷) =====================\n")
              
              ;; 레이어 테이블 검색 및 DB 포맷으로 병합
              (foreach l layList
                (setq layData (tblsearch "LAYER" l))
                (setq layCol (cdr (assoc 62 layData)))
                (setq layLT (cdr (assoc 6 layData)))
                
                ;; ★ LISP 리스트 포맷 적용: ("레이어명" 색상번호 "선종류")
                (setq resStr (strcat resStr "    (\"" l "\" " (itoa (abs layCol)) " \"" layLT "\")\n"))
              )
              
              (textscr)
              (princ resStr)
              (princ ";; ==================================================================\n")
              (princ "\n출력이 완료되었습니다. F2 창의 내용을 복사해서 DB에 활용하세요.\n")
            )
            (princ "\n선택된 객체가 없습니다.")
          )
        )
      )
      
    ) ;; ★ progn 종료 지점
  ) ;; ★ 기간 만료 if 종료 지점
  (princ)
)