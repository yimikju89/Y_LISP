(defun c:YBRIDGE_DATA ( / strName typeVal strType defL basePt ssBody ssPile ptBR str_limit_br pileEmbed k pStr hgapVal
                          dValMm dValM input frontX backX xStr ynVal ynStr hamValMm ent oType lay p1 p2 cp r stAng enAng out tmpFile f
                          ssGL entGL p1GL p2GL sxGL syGL exGL eyGL str_limit_gl_str pileLinesStr
                          ptGL glLimitY ptGLTopRef glLimitY2 skipGL ghfVal ghbVal gapPt1 gapPt2 gapLineStr gapX1 gapY1 gapX2 gapY2 lx1 ly1 lx2 ly2 isGapLine tmpHgap strHAE stream)
  (vl-load-com)
  
  (if (<= (fix (getvar "CDATE")) 20271231)
    (progn
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
      
      (princ "\n\n*** [YBRIDGE_DATA] 구조물 통합 DB 추출기 시작 ***\n")
      
      ;; [Step 1] 기본 정보
      (setq strName (getstring T "\n(1/8) 구조물 이름을 입력하세요 (예: 교각_거더 / 30m / Φ600): "))
      (if (= strName "") (setq strName "임시 구조물"))

      (initget 1 "1 2")
      (setq typeVal (getkword "\n(1/8) 구조물 타입을 선택하세요 (1: 교대, 2: 교각): "))
      (setq strType (if (= typeVal "1") "ABUT" "PIER"))

      (setq defL (getreal "\n(1/8) 디폴트 경간 길이(L)를 입력하세요 (m) <15.0>: "))
      (if (null defL) (setq defL 15.0))
      
      ;; [Step 2] 기준점 및 본체 선택
      (setq basePt (getpoint "\n(2/8) 구조물 기준점(선형 배치점 0,0 위치)을 클릭하세요: "))
      (if (null basePt) (progn (princ "\n[취소됨] 기준점을 선택해야 합니다.") (exit)))

      (princ "\n(2/8) 구조물 본체(말뚝 라인 포함, 중심선 제외)를 구성하는 모든 [선/호]를 드래그하여 선택하세요: ")
      (setq ssBody (ssget '((0 . "LINE,ARC"))))
      (if (null ssBody) (progn (princ "\n[취소됨] 선택된 본체 객체가 없습니다.") (exit)))
      
      ;; [Step 3] 지반선 연산 파라미터
      (setq hgapVal nil) ;; 입력 없으면 생략하기 위해 nil 초기화
      (setq ptGL (getpoint "\n(3/8) 지반선까지 늘어날(Stretch) 기준 높이를 클릭하세요 (사용 안 하면 엔터): "))
      (if ptGL
        (progn
          (setq glLimitY (- (cadr ptGL) (cadr basePt)))

          (setq ptGLTopRef (getpoint "\n(3/8) 전체 높이 연산 시 상단 기준 레벨을 클릭하세요 <0>: "))
          (if ptGLTopRef
            (setq glLimitY2 (- (cadr ptGLTopRef) (cadr basePt)))
            (setq glLimitY2 0.0) 
          )

          (princ "\n(3/8) 기초 상단 라인 객체를 선택하세요: ")
          (setq ssGL (ssget '((0 . "LINE"))))
          (if ssGL
            (progn
              (setq entGL (entget (ssname ssGL 0)))
              (setq p1GL (cdr (assoc 10 entGL)) p2GL (cdr (assoc 11 entGL)))
              (setq sxGL (- (car p1GL) (car basePt)))
              (setq syGL (- (cadr p1GL) (cadr basePt)))
              (setq exGL (- (car p2GL) (car basePt)))
              (setq eyGL (- (cadr p2GL) (cadr basePt)))
              
              (setq str_limit_gl_str (strcat "(" (rtos3 glLimitY) " " (rtos3 glLimitY2) " (" (rtos3 sxGL) " " (rtos3 syGL) " " (rtos3 exGL) " " (rtos3 eyGL) "))"))

              (setq tmpHgap (getreal "\n(3/8) 보정값 (선형~기초상단 + 보정값)을 입력하세요 (mm) (사용 안 하면 엔터): "))
              (if tmpHgap (setq hgapVal (/ tmpHgap 1000.0)))
            )
            (progn
              (initget "Yes No")
              (setq skipGL (getkword "\n객체가 선택되지 않았습니다. 지반선까지 Stretch를 사용하지 않으시겠습니까? [Yes/No] <Yes>: "))
              (if (or (null skipGL) (= skipGL "Yes") (= skipGL "Y"))
                (setq str_limit_gl_str "nil")
                (progn (princ "\n[취소됨] 기초 상단 라인을 다시 선택해야 합니다.") (exit))
              )
            )
          )
        )
        (setq str_limit_gl_str "nil") 
      )
      
      ;; [Step 4] 말뚝 및 기반암 연산
      (princ "\n(4/8) 말뚝 중심선(수직선) 객체들을 선택하세요 (드래그 가능, 직접기초면 엔터): ")
      (setq ssPile (ssget '((0 . "LINE"))))
      (setq pileLinesStr "")
      (setq str_limit_br nil pileEmbed nil) 
      
      (if ssPile
        (progn
          (setq k 0)
          (while (< k (sslength ssPile))
            (setq ent (entget (ssname ssPile k)))
            (setq lay (cdr (assoc 8 ent)))
            (setq p1 (cdr (assoc 10 ent)) p2 (cdr (assoc 11 ent)))
            (setq pileLinesStr (strcat pileLinesStr "    (\"L\" \"" lay "\" " (rtos3 (- (car p1) (car basePt))) " " (rtos3 (- (cadr p1) (cadr basePt))) " " (rtos3 (- (car p2) (car basePt))) " " (rtos3 (- (cadr p2) (cadr basePt))) ")\n"))
            (setq k (1+ k))
          )

          (setq ptBR (getpoint "\n(4/8) 기반암까지 늘어날 '말뚝 끝단 레벨(Y)'을 화면에서 클릭하세요 (사용 안 하면 엔터): "))
          (if ptBR
            (progn
              (setq str_limit_br (- (cadr ptBR) (cadr basePt)))
              (setq pileEmbed (getreal "\n(4/8) 말뚝 근입 깊이 디폴트값을 입력하세요 (m) <1.2>: "))
              (if (null pileEmbed) (setq pileEmbed 1.2))
            )
          )
        )
      )

      ;; =========================================
      ;; 문자열 조립 초기화
      ;; =========================================
      (setq out (strcat "(\"" strName "\" . (\n"))
      (setq out (strcat out "  (type . \"" strType "\")\n"))
      (setq out (strcat out "  (default_L . " (rtos3 defL) ")\n"))
      (if (/= str_limit_gl_str "nil") (setq out (strcat out "  (str_limit_gl . " str_limit_gl_str ")\n")))
      
      (if str_limit_br
        (setq out (strcat out "  (str_limit_br . " (rtos3 str_limit_br) ")\n  (pile_embed . " (rtos3 pileEmbed) ")\n"))
      )
      
      ;; ★ HGAP 생략 로직: 입력값이 있을 때만 출력
      (if hgapVal (setq out (strcat out "  (HGAP . " (rtos3 hgapVal) ")\n")))

      ;; [Step 5] D1~D5, HAE, HAM
      (defun process-dim (dimName desc isHae / dValMm dValM input frontX backX xStr ynVal ynStr resStr)
        (setq resStr "")
        (setq dValMm (getdist (strcat "\n(5/8) " dimName " - " desc "을 숫자입력 하거나 두 점을 찍으세요 (mm) (사용 안 하면 엔터): ")))
        (if dValMm
          (progn
            (setq dValM (/ dValMm 1000.0))
            
            ;; 1. 전 이격거리(앞) 입력
            (initget 128)
            (setq input (getpoint (strcat "\n(5/8) " dimName " - 전 이격거리 클릭 또는 X거리 입력 (0 / 스킵: 스페이스바): ")))
            (cond
              ((or (null input) (= input ""))
               (setq frontX 0.0)
               (princ " -> [ 0.000m ] (기본값) 입력됨\n")
              )
              ((listp input)
               (setq frontX (- (car input) (car basePt)))
               (princ (strcat " -> [ " (rtos3 frontX) "m ] (화면 클릭) 입력됨\n"))
              )
              ((= (type input) 'STR)
               (setq frontX (atof input))
               (princ (strcat " -> [ " (rtos3 frontX) "m ] (직접 입력) 입력됨\n"))
              )
            )

            ;; 2. 후 이격거리(뒤) 입력
            (initget 128)
            (setq input (getpoint (strcat "\n(5/8) " dimName " - 후 이격거리 클릭 또는 X거리 입력 (전 이격거리와 동일: 스페이스바): ")))
            (cond
              ((or (null input) (= input ""))
               (setq backX frontX)
               (princ (strcat " -> [ " (rtos3 backX) "m ] (전 이격거리와 동일) 입력됨\n"))
              )
              ((listp input)
               (setq backX (- (car input) (car basePt)))
               (princ (strcat " -> [ " (rtos3 backX) "m ] (화면 클릭) 입력됨\n"))
              )
              ((= (type input) 'STR)
               (setq backX (atof input))
               (princ (strcat " -> [ " (rtos3 backX) "m ] (직접 입력) 입력됨\n"))
              )
            )

            (setq xStr (strcat (rtos3 frontX) " " (rtos3 backX)))
            
            (if (not isHae)
              (progn
                (initget "Yes No")
                (setq ynVal (getkword (strcat "\n(5/8) " dimName " - 수직선 작도 여부 [Yes/No] <No>: ")))
                (if (or (= ynVal "Yes") (= ynVal "Y"))
                  (setq ynStr "Y")
                  (setq ynStr "N")
                )
                (setq resStr (strcat "  (" dimName " . (" (rtos3 dValM) " (" xStr ") " ynStr "))\n"))
              )
              (setq resStr (strcat "  (" dimName " . (" (rtos3 dValM) " (" xStr ")))\n"))
            )
          )
        )
        resStr
      )

      (setq out (strcat out (process-dim "D1" "내림값" nil)))
      (setq out (strcat out (process-dim "D2" "내림값" nil)))
      (setq out (strcat out (process-dim "D3" "내림값" nil)))
      (setq out (strcat out (process-dim "D4" "내림값" nil)))
      (setq out (strcat out (process-dim "D5" "내림값" nil)))
      
      ;; ★ HAE 입력 (스마트 스킵 대응)
      (setq strHAE (process-dim "HAE" "아치 양단 내림값" T))
      (setq out (strcat out strHAE))
      
      ;; ★ HAE가 스킵되지 않았을 때만 HAM 질문
      (if (/= strHAE "")
        (progn
          (setq hamValMm (getdist "\n(5/8) HAM - 아치 중간 내림값을 숫자입력 하거나 두 점을 찍으세요 (mm) (사용 안 하면 엔터): "))
          (if hamValMm (setq out (strcat out "  (HAM . " (rtos3 (/ hamValMm 1000.0)) ")\n")))
        )
      )

      ;; [Step 6] 거더 높이
      (setq ghfVal (getreal "\n(6/8) 전 거더 높이를 입력하세요 (m) (사용 안 하면 엔터): "))
      (if ghfVal (setq out (strcat out "  (GHF . " (rtos3 ghfVal) ")\n")))
      (setq ghbVal (getreal "\n(6/8) 후 거더 높이를 입력하세요 (m) (사용 안 하면 엔터): "))
      (if ghbVal (setq out (strcat out "  (GHB . " (rtos3 ghbVal) ")\n")))

      ;; [Step 7] 스마트 스트레치 영역 지정 
      (defun process-stretch (dimName labelStr / pt1 pt2 minX minY maxX maxY boxStr ssSt k obj oType lay sp ep cp r sang eang sx sy ex ey cx cy resStr)
        (setq pt1 (getpoint (strcat "\n(7/8) [" labelStr "] 스트레치 영역의 첫 번째 구석을 클릭하세요 (사용 안 하면 엔터): ")))
        (if pt1
          (progn
            (setq pt2 (getcorner pt1 (strcat "\n(7/8) [" labelStr "] 반대편 구석을 클릭하세요: ")))
            (if pt2
              (progn
                (setq minX (min (- (car pt1) (car basePt)) (- (car pt2) (car basePt))))
                (setq maxX (max (- (car pt1) (car basePt)) (- (car pt2) (car basePt))))
                (setq minY (min (- (cadr pt1) (cadr basePt)) (- (cadr pt2) (cadr basePt))))
                (setq maxY (max (- (cadr pt1) (cadr basePt)) (- (cadr pt2) (cadr basePt))))
                
                (setq boxStr (strcat "(" (rtos3 minX) " " (rtos3 minY) " " (rtos3 maxX) " " (rtos3 maxY) ")"))
                
                (princ (strcat "\n(7/8) [" labelStr "] 영역에 걸치는 객체 중 '스트레치(Stretch)를 적용할 객체'를 추가로 선택해주세요: "))
                (setq ssSt (ssget '((0 . "LINE,ARC"))))
                
                (setq resStr (strcat "\n  (" dimName " . (\n    " boxStr "\n"))
                (if ssSt
                  (progn
                    (setq k 0)
                    (while (< k (sslength ssSt))
                      (setq obj (vlax-ename->vla-object (ssname ssSt k)))
                      (setq oType (vla-get-ObjectName obj))
                      (setq lay (vla-get-Layer obj))
                      (cond
                        ((= oType "AcDbLine")
                         (setq sp (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint obj))))
                         (setq ep (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint obj))))
                         (setq sx (- (car sp) (car basePt)))
                         (setq sy (- (cadr sp) (cadr basePt)))
                         (setq ex (- (car ep) (car basePt)))
                         (setq ey (- (cadr ep) (cadr basePt)))
                         (setq resStr (strcat resStr "    (\"L\" \"" lay "\" " (rtos3 sx) " " (rtos3 sy) " " (rtos3 ex) " " (rtos3 ey) ")\n"))
                        )
                        ((= oType "AcDbArc")
                         (setq cp (vlax-safearray->list (vlax-variant-value (vla-get-Center obj))))
                         (setq rad (vla-get-Radius obj))
                         (setq sang (vla-get-StartAngle obj))
                         (setq eang (vla-get-EndAngle obj))
                         (setq cx (- (car cp) (car basePt)))
                         (setq cy (- (cadr cp) (cadr basePt)))
                         (setq resStr (strcat resStr "    (\"A\" \"" lay "\" " (rtos3 cx) " " (rtos3 cy) " " (rtos3 rad) " " (rtos4 sang) " " (rtos4 eang) ")\n"))
                        )
                      )
                      (setq k (1+ k))
                    )
                  )
                )
                (setq resStr (strcat resStr "  ))"))
                resStr
              )
              ""
            )
          )
          ""
        )
      )

      (setq out (strcat out (process-stretch "stretch_front" "Front")))
      (setq out (strcat out (process-stretch "stretch_back" "Back")))
      (setq out (strcat out (process-stretch "stretch_max" "Max")))

      ;; [Step 7.5] 거더 단차 라인 (girder_gap_line) 두 점 선택
      (setq gapPt1 (getpoint "\n(7.5/8) 거더 단차 라인의 시작점을 클릭하세요 (없으면 엔터): "))
      (setq gapLineStr "")
      (setq gapX1 nil gapY1 nil gapX2 nil gapY2 nil)
      (if gapPt1
        (progn
          (setq gapPt2 (getpoint gapPt1 "\n(7.5/8) 거더 단차 라인의 끝점을 클릭하세요 (점 1개짜리면 시작점과 동일한 위치 클릭): "))
          (if (null gapPt2) (setq gapPt2 gapPt1))
          
          (setq gapX1 (- (car gapPt1) (car basePt)))
          (setq gapY1 (- (cadr gapPt1) (cadr basePt)))
          (setq gapX2 (- (car gapPt2) (car basePt)))
          (setq gapY2 (- (cadr gapPt2) (cadr basePt)))
          
          (setq gapLineStr (strcat "    (\"L\" \"CS-CONC-MAJR\" " (rtos3 gapX1) " " (rtos3 gapY1) " " (rtos3 gapX2) " " (rtos3 gapY2) ")\n"))
        )
      )

      ;; 객체 리스트 파싱
      (if (/= pileLinesStr "")
        (setq out (strcat out "\n\n  ;; 말뚝 중심선\n  (pile_lines . (\n" pileLinesStr "  ))\n"))
      )
      
      (setq out (strcat out "\n  ;; 구조물 원본 라인 및 호 데이터 (L = Line, A = Arc)\n  (draw_lines . (\n"))
      (setq k 0)
      (while (< k (sslength ssBody))
        (setq ent (entget (ssname ssBody k)))
        (setq oType (cdr (assoc 0 ent)))
        (setq lay (cdr (assoc 8 ent)))
        (if (= oType "LINE")
          (progn
            (setq p1 (cdr (assoc 10 ent)) p2 (cdr (assoc 11 ent)))
            (setq lx1 (- (car p1) (car basePt)))
            (setq ly1 (- (cadr p1) (cadr basePt)))
            (setq lx2 (- (car p2) (car basePt)))
            (setq ly2 (- (cadr p2) (cadr basePt)))
            
            ;; 단차 라인 중복 제외
            (setq isGapLine nil)
            (if gapPt1
              (if (or (and (< (abs (- lx1 gapX1)) 1e-4) (< (abs (- ly1 gapY1)) 1e-4)
                           (< (abs (- lx2 gapX2)) 1e-4) (< (abs (- ly2 gapY2)) 1e-4))
                      (and (< (abs (- lx1 gapX2)) 1e-4) (< (abs (- ly1 gapY2)) 1e-4)
                           (< (abs (- lx2 gapX1)) 1e-4) (< (abs (- ly2 gapY1)) 1e-4)))
                (setq isGapLine T)
              )
            )
            
            (if (not isGapLine)
              (setq out (strcat out "    (\"L\" \"" lay "\" " (rtos3 lx1) " " (rtos3 ly1) " " (rtos3 lx2) " " (rtos3 ly2) ")\n"))
            )
          )
        )
        (if (= oType "ARC")
          (progn
            (setq cp (cdr (assoc 10 ent)) r (cdr (assoc 40 ent)) stAng (cdr (assoc 50 ent)) enAng (cdr (assoc 51 ent)))
            (setq out (strcat out "    (\"A\" \"" lay "\" " (rtos3 (- (car cp) (car basePt))) " " (rtos3 (- (cadr cp) (cadr basePt))) " " (rtos3 r) " " (rtos4 stAng) " " (rtos4 enAng) ")\n"))
          )
        )
        (setq k (1+ k))
      )
      (setq out (strcat out "  ))\n"))
      
      ;; 방금 추출한 단차 라인을 DB 최하단에 기록
      (if (/= gapLineStr "")
        (setq out (strcat out "\n  ;; 거더 높이 단차를 이어주는 라인\n  (girder_gap_line . (\n" gapLineStr "  ))\n"))
      )

      ;; ★ [추가] 사용된 모든 레이어 정보 추출
      (setq layList '())
      (if ssBody
        (progn
          (setq k 0)
          (while (< k (sslength ssBody))
            (setq layName (cdr (assoc 8 (entget (ssname ssBody k)))))
            (if (not (member layName layList)) (setq layList (cons layName layList)))
            (setq k (1+ k))
          )
        )
      )
      (if ssPile
        (progn
          (setq k 0)
          (while (< k (sslength ssPile))
            (setq layName (cdr (assoc 8 (entget (ssname ssPile k)))))
            (if (not (member layName layList)) (setq layList (cons layName layList)))
            (setq k (1+ k))
          )
        )
      )
      ;; (단차 라인 기본 레이어 포함 보장)
      (if (not (member "CS-CONC-MAJR" layList)) (setq layList (cons "CS-CONC-MAJR" layList)))

      ;; 추출한 레이어 정보 텍스트 포맷팅 및 DB 결합
      (if layList
        (progn
          (setq layList (vl-sort layList '<))
          (setq out (strcat out "\n  ;; 레이어\n  (layers . (\n"))
          (foreach l layList
            (setq layData (tblsearch "LAYER" l))
            (setq layCol (cdr (assoc 62 layData)))
            (setq layLT (cdr (assoc 6 layData)))
            (setq out (strcat out "    (\"" l "\" " (itoa (abs layCol)) " \"" layLT "\")\n"))
          )
          (setq out (strcat out "  ))\n"))
        )
      )

      (setq out (strcat out "\n))"))
      
      ;; [Step 8] 파일 생성 및 띄우기 (UTF-8 인코딩 강제 적용 및 에러 수정)
      (princ "\n(8/8) 구조물 DB 데이터 변환 중...")
      (setq tmpFile (vl-filename-mktemp "YB_DB_OUTPUT" "" ".txt"))
      
      ;; ★ 매개변수 부족 에러 수정: vlax-invoke-method -> vlax-invoke로 통일
      (setq stream (vlax-create-object "ADODB.Stream"))
      (if stream
        (progn
          (vlax-put-property stream 'Type 2) ; 텍스트 모드
          (vlax-put-property stream 'Charset "utf-8") ; 인코딩 강제
          (vlax-invoke stream 'Open)
          (vlax-invoke stream 'WriteText out)
          (vlax-invoke stream 'SaveToFile tmpFile 2) ; 덮어쓰기
          (vlax-invoke stream 'Close)
          (vlax-release-object stream)
        )
        ;; 만약 ADODB 객체 생성이 안 될 경우 기존 방식 폴백
        (progn
          (setq f (open tmpFile "w"))
          (write-line out f)
          (close f)
        )
      )

      (startapp "notepad" tmpFile)
      (princ "\n[완료] DB 추출이 완료되었습니다. (메모장 확인)\n")
    )
  ) ;; ★ 기간 만료 조건 if문을 닫는 괄호
  (princ)
)