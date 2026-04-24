;--------------------------------------------------
;  MM - Merge PDF Utility
;  Encoding: ANSI (CP949)
;  Environment: ZWCAD / AutoCAD on Korean Windows
;  Note: Open and edit this file with ANSI encoding
;        to avoid broken Korean comments.
;--------------------------------------------------



(vl-load-com)

(defun _remove-ext (fname / pos)
  (if (and fname (setq pos (vl-string-position 46 fname 0 T)))
    (substr fname 1 pos)
    fname
  )
)

(defun _quote (s)
  (strcat "\"" s "\"")
)

(defun _file-exists-p (filepath / f)
  (if (and filepath (setq f (open filepath "r")))
    (progn
      (close f)
      T
    )
    nil
  )
)

(defun _rstrip-slash (s)
  (if (and s (> (strlen s) 0)
           (= (substr s (strlen s) 1) "\\"))
    (substr s 1 (1- (strlen s)))
    s
  )
)

(defun c:MM (/ exeapp dwgfolder dwgname args result)




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; merge_pdf.exe 경로
  (setq exeapp "C:\\lisp\\merge_pdf.exe")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




  ;; 현재 도면 정보
  (setq dwgfolder (_rstrip-slash (getvar "DWGPREFIX")))
  (setq dwgname   (_remove-ext (getvar "DWGNAME")))

  (cond
    ((not (_file-exists-p exeapp))
     (prompt (strcat "\nmerge_pdf.exe 파일을 찾을 수 없습니다: " exeapp))
    )

    ((or (null dwgfolder) (= dwgfolder ""))
     (prompt "\n현재 도면 폴더를 찾을 수 없습니다.")
    )

    ((or (null dwgname) (= dwgname ""))
     (prompt "\n현재 도면 이름을 찾을 수 없습니다.")
    )

    (T
      (prompt (strcat "\nEXE 파일: " exeapp))
      (prompt (strcat "\nDWG 폴더: " dwgfolder))
      (prompt (strcat "\nDWG 이름: " dwgname))

      ;; merge_pdf.exe 뒤에 전달할 인자 문자열
      (setq args
        (strcat
          (_quote dwgfolder)
          " "
          (_quote dwgname)
        )
      )

      (prompt (strcat "\n실행 인자: " args))

      ;; merge_pdf.exe 직접 실행
      (setq result (startapp exeapp args))

      (if result
        (prompt "\nPDF 병합용 EXE 실행 완료.")
        (prompt "\nEXE 실행 실패.")
      )
    )
  )

  (princ)
)
