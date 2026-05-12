(defun c:YEARTH_FRONT_FILLING (/ *error* exp_date cur_date dcl_id dcl_file f dcl_res
                                 tmp temp ang_total ang_start ang_end
                                 dcl_loop save_ui_state rad_list bands band
                                 old_osmode old_cmdecho old_cecolor ss_start ss_block ss_cleanup ent ent_data blk_name
                                 i j k r_in r_out r_half r_min r_max
                                 main_divs sub_divs cur_main_ang next_main_ang cur_sub_ang tmp_pts flag)

  ;; 1. 만료 로직 (2026년 12월 31일)
  (setq exp_date 20261231)
  (setq cur_date (fix (getvar "CDATE")))
  (if (> cur_date exp_date)
    (progn (alert "프로그램 사용 기간이 만료되었습니다.") (exit))
  )

  ;; 2. 에러 처리 및 환경 변수 저장 (색상 변수 추가)
  (setq old_osmode (getvar "OSMODE") 
        old_cmdecho (getvar "CMDECHO")
        old_cecolor (getvar "CECOLOR"))
        
  (defun *error* (msg)
    (if (and dcl_id (> dcl_id 0)) (unload_dialog dcl_id))
    (if (and dcl_file (findfile dcl_file)) (vl-file-delete dcl_file))
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_cecolor (setvar "CECOLOR" old_cecolor))
    (princ (strcat "\nError: " msg)) (princ)
  )
  (vl-load-com)

  ;; 3. 글로벌 변수 초기화 (기존 선택 기억 및 포커스 추적)
  (if (not *yff_main_cnt*) (setq *yff_main_cnt* 4))
  (if (not *yff_sub_cnt*) (setq *yff_sub_cnt* 1))
  (if (not *yff_use_ang*) (setq *yff_use_ang* "0"))
  (if (not *yff_ang_val*) (setq *yff_ang_val* 90.0))
  (if (not *yff_use_lay*) (setq *yff_use_lay* "1"))
  (if (not *yff_lay_ref*) (setq *yff_lay_ref* "CE-SLOP-FILL"))
  (if (not *yff_col_ref*) (setq *yff_col_ref* 256)) ; 256은 ByLayer 의미
  (if (not *yff_flip_dir*) (setq *yff_flip_dir* "0"))
  (if (not *yff_next_focus*) (setq *yff_next_focus* "btn_cen"))

  ;; 4. DCL 파일 생성
  (setq dcl_file (vl-filename-mktemp "slope.dcl") f (open dcl_file "w"))
  (write-line "slope_diag : dialog { label = \"앞성토 사면 그리기\"; " f)
  (write-line "  : row { " f)
  (write-line "    : boxed_column { label = \"객체 및 포인트 선택\"; width = 50;" f)
  (write-line "      : row { : button { label=\"시작점 (중심)\"; key=\"btn_cen\"; width=24;} : text { key=\"t_cen\"; } }" f)
  (write-line "      : row { : button { label=\"시작선 (점선택)\"; key=\"btn_st\"; width=24;} : text { key=\"t_st\"; } }" f)
  (write-line "      : row { : toggle { label=\"각도입력 (양수:CW, 음수:CCW)\"; key=\"cb_ang\"; } : edit_box { key=\"eb_ang\"; edit_width=6; } }" f)
  (write-line "      : row { : button { label=\"끝선 (점선택)\"; key=\"btn_ed\"; width=24;} : text { key=\"t_ed\"; } }" f)
  (write-line "      : row { : toggle { label=\"반대 (시작/끝 스왑)\"; key=\"cb_flip\"; } }" f)
  (write-line "      : row { : button { label=\"사면 시작점\"; key=\"btn_sl_st\"; width=24;} : text { key=\"t_sl_st\"; } }" f)
  (write-line "      : row { : button { label=\"소단 선택 (다중)\"; key=\"btn_bm\"; width=24;} : text { key=\"t_bm\"; } }" f)
  (write-line "      : row { : button { label=\"사면 끝점\"; key=\"btn_sl_ed\"; width=24;} : text { key=\"t_sl_ed\"; } }" f)
  (write-line "    }" f)
  (write-line "    : column { " f)
  (write-line "      : boxed_column { label = \"기본 설정\"; width = 28;" f)
  (write-line "        : edit_box { label=\"주 표시 개수:\"; key=\"eb_main\"; edit_width=5; }" f)
  (write-line "        : edit_box { label=\"보조 표시 개수:\"; key=\"eb_sub\"; edit_width=5; }" f)
  (write-line "      }" f)
  (write-line "      : boxed_column { label = \"레이어\"; " f)
  (write-line "        : toggle { label=\"표준 레이어 사용\"; key=\"cb_lay\"; }" f)
  (write-line "        : button { label=\"참고 레이어 선택\"; key=\"btn_lay\"; }" f)
  (write-line "        : text { key=\"t_lay_nm\"; }" f)
  (write-line "      }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : row { spacer_1; ok_cancel; }" f)
  (write-line "}" f)
  (close f)

  ;; 5. UI 상태 저장용 함수
  (defun save_ui_state ()
    (setq *yff_main_cnt* (atoi (get_tile "eb_main")) *yff_sub_cnt* (atoi (get_tile "eb_sub")))
    (setq *yff_use_ang* (get_tile "cb_ang") *yff_ang_val* (atof (get_tile "eb_ang")) *yff_use_lay* (get_tile "cb_lay"))
    (setq *yff_flip_dir* (get_tile "cb_flip"))
  )

  ;; 6. DCL 다이얼로그 루프
  (setq dcl_loop T dcl_res nil result nil)
  (while dcl_loop
    (setq dcl_id (load_dialog dcl_file))
    (if (not (new_dialog "slope_diag" dcl_id)) (exit))

    (set_tile "eb_main" (itoa *yff_main_cnt*)) (set_tile "eb_sub" (itoa *yff_sub_cnt*))
    (set_tile "cb_ang" *yff_use_ang*) (set_tile "eb_ang" (rtos *yff_ang_val* 2 2))
    (set_tile "cb_lay" *yff_use_lay*) (set_tile "t_lay_nm" (strcat "[" *yff_lay_ref* "]"))
    (set_tile "cb_flip" *yff_flip_dir*)
    
    (set_tile "t_cen" (if *yff_pt_center* "(선택 O)" "(선택 X)"))
    (set_tile "t_st" (if *yff_pt_start* "(선택 O)" "(선택 X)"))
    (set_tile "t_ed" (if *yff_pt_end* "(선택 O)" "(선택 X)"))
    (set_tile "t_sl_st" (if *yff_pt_slope_start* "(선택 O)" "(선택 안함 - 중심시작)"))
    (set_tile "t_bm" (if *yff_pts_berm* (strcat "(선택 " (itoa (length *yff_pts_berm*)) "개)") "(선택 X)"))
    (set_tile "t_sl_ed" (if *yff_pt_slope_end* "(선택 O)" "(선택 X)"))

    (mode_tile "eb_ang" (if (= *yff_use_ang* "1") 0 1))
    (mode_tile "btn_ed" (if (= *yff_use_ang* "1") 1 0))
    (mode_tile "cb_flip" (if (= *yff_use_ang* "1") 1 0))
    (mode_tile "btn_lay" (if (= *yff_use_lay* "1") 1 0))

    (if *yff_next_focus* (mode_tile *yff_next_focus* 2))

    (action_tile "cb_ang" "(mode_tile \"eb_ang\" (if (= $value \"1\") 0 1)) (mode_tile \"btn_ed\" (if (= $value \"1\") 1 0)) (mode_tile \"cb_flip\" (if (= $value \"1\") 1 0))")
    (action_tile "cb_lay" "(mode_tile \"btn_lay\" (if (= $value \"1\") 1 0))")
    
    (action_tile "btn_cen" "(save_ui_state) (done_dialog 2)")
    (action_tile "btn_st" "(save_ui_state) (done_dialog 3)")
    (action_tile "btn_ed" "(save_ui_state) (done_dialog 4)")
    (action_tile "btn_sl_st" "(save_ui_state) (done_dialog 5)")
    (action_tile "btn_bm" "(save_ui_state) (done_dialog 6)")
    (action_tile "btn_sl_ed" "(save_ui_state) (done_dialog 7)")
    (action_tile "btn_lay" "(save_ui_state) (done_dialog 8)")
    (action_tile "accept" "(save_ui_state) (done_dialog 1)")
    (action_tile "cancel" "(done_dialog 0)")

    (setq dcl_res (start_dialog))
    (unload_dialog dcl_id)

    (cond
      ((= dcl_res 0) (setq dcl_loop nil result nil))
      ((= dcl_res 1)
        (if (and *yff_pt_center* *yff_pt_start* (or *yff_pt_end* (= *yff_use_ang* "1")) *yff_pt_slope_end*)
          (setq dcl_loop nil result T)
          (progn (alert "필수 항목(시작점, 시작선, 끝선, 사면 끝점)이 모두 지정되지 않았습니다.") (setq *yff_next_focus* "btn_cen"))
        )
      )
      ((= dcl_res 2) (setq tmp (getpoint "\n시작점(호의 중심점) 선택: ")) (if tmp (setq *yff_pt_center* tmp)) (setq *yff_next_focus* "btn_st"))
      ((= dcl_res 3) (setq tmp (if *yff_pt_center* (getpoint *yff_pt_center* "\n시작선 방향 점 선택: ") (getpoint "\n시작선 방향 점 선택: "))) (if tmp (setq *yff_pt_start* tmp)) (setq *yff_next_focus* (if (= *yff_use_ang* "1") "btn_sl_st" "btn_ed")))
      ((= dcl_res 4) (setq tmp (if *yff_pt_center* (getpoint *yff_pt_center* "\n끝선 방향 점 선택: ") (getpoint "\n끝선 방향 점 선택: "))) (if tmp (setq *yff_pt_end* tmp)) (setq *yff_next_focus* "btn_sl_st"))
      ((= dcl_res 5) (setq tmp (if *yff_pt_center* (getpoint *yff_pt_center* "\n사면 시작점 지정 (엔터 시 중심점): ") (getpoint "\n사면 시작점 지정: "))) (setq *yff_pt_slope_start* tmp) (setq *yff_next_focus* "btn_bm"))
      ((= dcl_res 6)
        (princ "\n소단 점들을 연속으로 선택하세요. (★짝수 개 필수★, 종료: 우클릭/스페이스바)")
        (setq tmp_pts nil flag T)
        (while flag
          (setq tmp (getpoint "\n소단 점 선택: "))
          (if tmp (setq tmp_pts (cons tmp tmp_pts)) (setq flag nil))
        )
        (if (/= (rem (length tmp_pts) 2) 0)
          (progn (alert "소단 점은 반드시 짝수 개로 선택해야 합니다!\n다시 선택해주세요.") (setq *yff_next_focus* "btn_bm"))
          (progn (setq *yff_pts_berm* tmp_pts) (setq *yff_next_focus* "btn_sl_ed"))
        )
      )
      ((= dcl_res 7) (setq tmp (if *yff_pt_center* (getpoint *yff_pt_center* "\n사면 끝점 지정: ") (getpoint "\n사면 끝점 지정: "))) (if tmp (setq *yff_pt_slope_end* tmp)) (setq *yff_next_focus* "accept"))
      ((= dcl_res 8) 
        ;; ? 레이어 및 색상(DXF 62) 동시에 가져오기
        (setq tmp (car (entsel "\n참조할 객체(레이어/색상)를 선택하세요: "))) 
        (if tmp 
          (progn
            (setq ent_data (entget tmp))
            (setq *yff_lay_ref* (cdr (assoc 8 ent_data))) ; 레이어 추출
            (setq *yff_col_ref* (cdr (assoc 62 ent_data))) ; 색상 추출
            (if (not *yff_col_ref*) (setq *yff_col_ref* 256)) ; 고유 색상이 없으면 ByLayer(256) 처리
          )
        )
      )
    )
  )
  (if (and dcl_file (findfile dcl_file)) (vl-file-delete dcl_file))

  ;; 8. 작도 로직 시작
  (if result
    (progn
      (setvar "CMDECHO" 0) (setvar "OSMODE" 0)
      (command "_.undo" "_group")
      (setq ss_start (entlast))

      ;; 8-1. 각도 계산
      (setq ang_start (angle *yff_pt_center* *yff_pt_start*))

      (if (= *yff_use_ang* "1")
        (progn
          (setq ang_end (- ang_start (* *yff_ang_val* (/ pi 180.0))))
          (if (> *yff_ang_val* 0)
            (setq temp ang_start ang_start ang_end ang_end temp)
          )
        )
        (progn
          (setq ang_end (angle *yff_pt_center* *yff_pt_end*))
          (if (= *yff_flip_dir* "1")
            (setq temp ang_start ang_start ang_end ang_end temp)
          )
        )
      )
      
      (setq ang_total (- ang_end ang_start))
      (if (< ang_total 0) (setq ang_total (+ ang_total (* 2 pi))))

      ;; 8-2. 반지름 밴드 쌍 만들기
      (setq rad_list nil)
      (if *yff_pt_slope_start* (setq rad_list (cons (distance *yff_pt_center* *yff_pt_slope_start*) rad_list)) 
        (setq rad_list (cons 0.0 rad_list))
      )
      (setq rad_list (cons (distance *yff_pt_center* *yff_pt_slope_end*) rad_list))
      (if *yff_pts_berm* (foreach p *yff_pts_berm* (setq rad_list (cons (distance *yff_pt_center* p) rad_list))))
      
      (setq rad_list (vl-sort rad_list '<))
      (setq bands nil k 0)
      (while (< k (length rad_list))
        (setq bands (append bands (list (list (nth k rad_list) (nth (1+ k) rad_list)))))
        (setq k (+ k 2))
      )

      ;; 8-3. ? 레이어 및 현재 색상 세팅
      (if (= *yff_use_lay* "1")
        (progn
          (if (not (tblsearch "LAYER" "CE-SLOP-FILL")) (command "-layer" "m" "CE-SLOP-FILL" "c" 11 "" ""))
          (setvar "CLAYER" "CE-SLOP-FILL")
          (setvar "CECOLOR" "BYLAYER") ; 표준 레이어는 항상 ByLayer로 작도
        )
        (progn
          (setvar "CLAYER" *yff_lay_ref*)
          ;; 추출한 색상값에 맞추어 현재 색상(CECOLOR) 세팅
          (cond
            ((= *yff_col_ref* 256) (setvar "CECOLOR" "BYLAYER"))
            ((= *yff_col_ref* 0)   (setvar "CECOLOR" "BYBLOCK"))
            (T                     (setvar "CECOLOR" (itoa *yff_col_ref*)))
          )
        )
      )

      ;; 8-4. 외곽 호 그리기
      (foreach r rad_list
        (if (> r 0.001) (command "_.arc" "_C" *yff_pt_center* (polar *yff_pt_center* ang_start r) (polar *yff_pt_center* ang_end r)))
      )

      ;; 측면 마감선
      (foreach band bands
        (setq r_in (car band) r_out (cadr band))
        (if (> (abs (- r_out r_in)) 0.001)
          (progn
            (command "_.line" (polar *yff_pt_center* ang_start r_in) (polar *yff_pt_center* ang_start r_out) "")
            (command "_.line" (polar *yff_pt_center* ang_end r_in) (polar *yff_pt_center* ang_end r_out) "")
          )
        )
      )

      ;; 8-5. 사면 주/보조 표시
      (setq main_divs *yff_main_cnt* sub_divs (1+ *yff_sub_cnt*) i 0)
      (while (<= i main_divs)
        (setq cur_main_ang (+ ang_start (* ang_total (/ (float i) main_divs))))
        
        ;; 주 표시
        (foreach band bands
          (setq r_in (car band) r_out (cadr band))
          (if (> (abs (- r_out r_in)) 0.001)
            (command "_.line" (polar *yff_pt_center* cur_main_ang r_in) (polar *yff_pt_center* cur_main_ang r_out) "")
          )
        )

        (if (< i main_divs)
          (progn
            (setq next_main_ang (+ ang_start (* ang_total (/ (float (1+ i)) main_divs))) j 1)
            (while (< j sub_divs)
              (setq cur_sub_ang (+ cur_main_ang (* (- next_main_ang cur_main_ang) (/ (float j) sub_divs))))
              ;; 보조 표시
              (foreach band bands
                (setq r_in (car band) r_out (cadr band))
                (setq r_half (+ r_in (/ (- r_out r_in) 2.0)))
                (if (> (abs (- r_out r_in)) 0.001)
                  (command "_.line" (polar *yff_pt_center* cur_sub_ang r_in) (polar *yff_pt_center* cur_sub_ang r_half) "")
                )
              )
              (setq j (1+ j))
            )
          )
        )
        (setq i (1+ i))
      )
      
      ;; 8-6. 완벽한 단일 블록 생성 (블록 편집기 내부일 경우 우회)
      (setq ss_block (ssadd))
      (setq ent (if ss_start (entnext ss_start) (entnext)))
      (while ent (ssadd ent ss_block) (setq ent (entnext ent)))
      
      (if (> (sslength ss_block) 0)
        (if (= (getvar "BLOCKEDITOR") 1)
          (progn
            (setq *yff_next_focus* "btn_cen")
            (princ "\n블록 편집기 내부이므로 블록화 과정이 생략되었습니다.")
          )
          (progn
            (setq blk_name (strcat "FRONT_FILL_" (rtos (getvar "CDATE") 2 6) "_" (itoa (getvar "MILLISECS"))))
            (command "_.-block" blk_name "_non" *yff_pt_center* ss_block "")
            
            (setq ss_cleanup (ssadd))
            (setq ent (if ss_start (entnext ss_start) (entnext)))
            (while ent
              (if (entget ent) (ssadd ent ss_cleanup))
              (setq ent (entnext ent))
            )
            (if (> (sslength ss_cleanup) 0) (command "_.erase" ss_cleanup ""))
            
            (entmake (list '(0 . "INSERT") (cons 2 blk_name) (cons 10 *yff_pt_center*) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0)))
            
            (setq *yff_next_focus* "btn_cen")
            (princ (strcat "\n▶ 작도 및 블록 생성 완료 (블록명: " blk_name ")"))
          )
        )
      )
      (command "_.undo" "_end")
    )
  )
  
  ;; 9. 환경 변수 복구
  (if old_osmode (setvar "OSMODE" old_osmode))
  (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
  (if old_cecolor (setvar "CECOLOR" old_cecolor)) ; 색상 원상 복구
  (princ)
)