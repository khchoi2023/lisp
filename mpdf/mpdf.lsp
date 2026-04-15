;--------------------------------------------------
;  MPDF - Multiple PDF Plot Utility
;  Encoding: ANSI (CP949)
;  Environment: ZWCAD / AutoCAD on Korean Windows
;  Note: Open and edit this file with ANSI encoding
;           to avoid broken Korean comments.
;--------------------------------------------------



(vl-load-com)

(setq *pdf-device* "DWG To PDF.pc5")
(setq *pdf-style*  "monochrome.ctb")
(setq *pdf-media*  "ISO full bleed A3 (420.00 x 297.00 MM)")

;; 같은 행으로 볼 하단 Y 허용오차(mm)
(setq *row-tol* 10.0)

;; enum
(setq acWindow 4)
(setq acScaleToFit 0)
(setq ac0degrees 0)
(setq ac90degrees 1)
(setq acAllViewports 2)

;; ActiveX 좌표계 enum
(setq acWorld 0)
(setq acUCS 1)
(setq acDisplayDCS 2)
(setq acPaperSpaceDCS 3)

(defun _remove-ext (fname / pos)
  (if (and fname (setq pos (vl-string-position 46 fname 0 T)))
    (substr fname 1 pos)
    fname
  )
)

(defun _to-list-safe (x / r)
  (setq r (vl-catch-all-apply 'vlax-variant-value (list x)))
  (if (not (vl-catch-all-error-p r))
    (setq x r)
  )
  (setq r (vl-catch-all-apply 'vlax-safearray->list (list x)))
  (if (not (vl-catch-all-error-p r))
    (setq x r)
  )
  x
)

(defun _get-bbox (obj / pmin pmax a b)
  (vla-GetBoundingBox obj 'pmin 'pmax)
  (setq a (_to-list-safe pmin))
  (setq b (_to-list-safe pmax))
  (if (and (listp a) (listp b))
    (list a b)
    nil
  )
)

(defun _pt->str (pt)
  (strcat (rtos (car pt) 2 6) "," (rtos (cadr pt) 2 6))
)

(defun _make-point-variant (pt / arr)
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-put-element arr 0 (car pt))
  (vlax-safearray-put-element arr 1 (cadr pt))
  (vlax-make-variant arr)
)

(defun _safe-call (label fn args / r)
  (setq r (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p r)
    (progn
      (prompt (strcat "\n[ERR] " label " -> " (vl-catch-all-error-message r)))
      nil
    )
    T
  )
)

(defun _num3 (n / s)
  (setq s (itoa n))
  (cond
    ((= (strlen s) 1) (setq s (strcat "00" s)))
    ((= (strlen s) 2) (setq s (strcat "0" s)))
  )
  s
)

(defun _get-block-name (obj / name)
  (cond
    ((vlax-property-available-p obj 'EffectiveName)
      (setq name (vla-get-EffectiveName obj))
    )
    (t
      (setq name (vla-get-Name obj))
    )
  )
  name
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _valid-titleblock-name-p (name)
  (and name
       (or (vl-string-search "표지" name)
           (vl-string-search "도곽" name)))
)


;; 필터링 할 단어 추가방법
;; (defun _valid-titleblock-name-p (name)
;;   (and name
;;        (or (vl-string-search "표지" name)
;;            (vl-string-search "도곽" name)
;;            (vl-string-search "sheet" name)
;;            (vl-string-search "title" name)
;;            (vl-string-search "frame" name)))
;; )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defun _translate-wcs-to-dcs (doc pt / util in out)
  (setq util (vla-get-Utility doc))
  (setq in (vlax-3d-point (list (car pt) (cadr pt) 0.0)))
  (setq out (vl-catch-all-apply
              'vlax-invoke-method
              (list util 'TranslateCoordinates in acWorld acDisplayDCS :vlax-false)))
  (if (vl-catch-all-error-p out)
    nil
    (_to-list-safe out)
  )
)

;; item 구조:
;; (list ename obj bbox p1 p2 xmin ymin xmax ymax name)
(defun _make-item (en / obj name bbox p1 p2)
  (setq obj  (vlax-ename->vla-object en))
  (setq name (_get-block-name obj))

  (if (_valid-titleblock-name-p name)
    (progn
      (setq bbox (_get-bbox obj))
      (if bbox
        (progn
          (setq p1 (car bbox))
          (setq p2 (cadr bbox))
          (list en obj bbox p1 p2
                (car p1)
                (cadr p1)
                (car p2)
                (cadr p2)
                name)
        )
        nil
      )
    )
    nil
  )
)

(defun _item-obj  (it) (nth 1 it))
(defun _item-bbox (it) (nth 2 it))
(defun _item-p1   (it) (nth 3 it))
(defun _item-p2   (it) (nth 4 it))
(defun _item-xmin (it) (nth 5 it))
(defun _item-ymin (it) (nth 6 it))
(defun _item-xmax (it) (nth 7 it))
(defun _item-ymax (it) (nth 8 it))
(defun _item-name (it) (nth 9 it))

(defun _item-less-p (a b / ya yb xa xb)
  (setq ya (_item-ymin a))
  (setq yb (_item-ymin b))
  (setq xa (_item-xmin a))
  (setq xb (_item-xmin b))

  (if (<= (abs (- ya yb)) *row-tol*)
    (< xa xb)
    (> ya yb)
  )
)

(defun _sort-items (lst)
  (vl-sort lst '_item-less-p)
)

(defun _plot_block_to_pdf (doc lay plotObj obj pdfpath / bbox p1 p2 p1dcs p2dcs w h rot ll ur ok org arr)
  (setq bbox (_get-bbox obj))

  (if (null bbox)
    (progn
      (prompt "\nBounding Box 추출 실패.")
      nil
    )
    (progn
      (setq p1 (car bbox))
      (setq p2 (cadr bbox))

      (setq w (- (car p2) (car p1)))
      (setq h (- (cadr p2) (cadr p1)))

      (if (>= w h)
        (setq rot ac0degrees)
        (setq rot ac90degrees)
      )

      ;; WCS -> DCS 변환
      (setq p1dcs (_translate-wcs-to-dcs doc p1))
      (setq p2dcs (_translate-wcs-to-dcs doc p2))

      (if (or (null p1dcs) (null p2dcs))
        (progn
          (setq ll (_make-point-variant p1))
          (setq ur (_make-point-variant p2))
        )
        (progn
          (setq ll (_make-point-variant (list (car p1dcs) (cadr p1dcs))))
          (setq ur (_make-point-variant (list (car p2dcs) (cadr p2dcs))))
        )
      )

      ;; PlotOrigin = (0,0)
      (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 1)))
      (vlax-safearray-put-element arr 0 0.0)
      (vlax-safearray-put-element arr 1 0.0)
      (setq org (vlax-make-variant arr))

      (setq ok T)

      (if ok (setq ok (_safe-call "ConfigName"
                            'vlax-put-property
                            (list lay 'ConfigName *pdf-device*))))

      (if ok (setq ok (_safe-call "RefreshPlotDeviceInfo"
                            'vlax-invoke-method
                            (list lay 'RefreshPlotDeviceInfo))))

      (if ok (setq ok (_safe-call "CanonicalMediaName"
                            'vlax-put-property
                            (list lay 'CanonicalMediaName *pdf-media*))))

      (if ok (setq ok (_safe-call "PlotType=acWindow"
                            'vlax-put-property
                            (list lay 'PlotType acWindow))))

      (if ok (setq ok (_safe-call "SetWindowToPlot"
                            'vlax-invoke-method
                            (list lay 'SetWindowToPlot ll ur))))

      (if ok (setq ok (_safe-call "PlotOrigin"
                            'vlax-put-property
                            (list lay 'PlotOrigin org))))

      (if ok (setq ok (_safe-call "UseStandardScale"
                            'vlax-put-property
                            (list lay 'UseStandardScale :vlax-true))))

      (if ok (setq ok (_safe-call "StandardScale"
                            'vlax-put-property
                            (list lay 'StandardScale acScaleToFit))))

      (if ok (setq ok (_safe-call "CenterPlot"
                            'vlax-put-property
                            (list lay 'CenterPlot :vlax-true))))

      (if ok (setq ok (_safe-call "PlotRotation"
                            'vlax-put-property
                            (list lay 'PlotRotation rot))))

      (if ok (setq ok (_safe-call "PlotWithPlotStyles"
                            'vlax-put-property
                            (list lay 'PlotWithPlotStyles :vlax-true))))

      (if ok (setq ok (_safe-call "StyleSheet"
                            'vlax-put-property
                            (list lay 'StyleSheet *pdf-style*))))

      (if ok (setq ok (_safe-call "PlotToFile"
                            'vlax-invoke-method
                            (list plotObj 'PlotToFile pdfpath))))

      ok
    )
  )
)

(defun c:MPDF (/ acad doc lay plotObj oldbg ss i en item items
                    dwgprefix dwgname pdfname pdfpath cnt total ok sorted obj)

  (vl-load-com)

  (setq acad    (vlax-get-acad-object))
  (setq doc     (vla-get-ActiveDocument acad))
  (setq lay     (vla-get-ActiveLayout doc))
  (setq plotObj (vla-get-Plot doc))
  (setq oldbg   (getvar "BACKGROUNDPLOT"))

  (setvar "BACKGROUNDPLOT" 0)

  (prompt "\n도곽 블럭들을 선택하세요: ")
  (setq ss (ssget '((0 . "INSERT"))))

  (cond
    ((null ss)
      (prompt "\n블럭이 선택되지 않았습니다.")
    )

    (T
      (setq dwgprefix (getvar "DWGPREFIX"))
      (setq dwgname   (_remove-ext (getvar "DWGNAME")))
      (setq total     (sslength ss))
      (setq cnt       0)
      (setq items     '())

      (prompt (strcat "\n선택 개수: " (itoa total)))

      (setq i 0)
      (while (< i total)
        (setq en   (ssname ss i))
        (setq item (_make-item en))

        (if item
          (setq items (cons item items))
        )

        (setq i (1+ i))
      )

      (if (null items)
        (prompt "\n선택된 블럭 중 출력 대상(표지/도곽)이 없습니다.")
        (progn
          (setq sorted (_sort-items items))
          (setq total (length sorted))

          (_safe-call "RefreshPlotDeviceInfo"
            'vlax-invoke-method
            (list lay 'RefreshPlotDeviceInfo))

          (setq i 0)
          (foreach item sorted
            (setq obj (_item-obj item))

            (setq pdfname (strcat dwgname "_" (_num3 (1+ i)) ".pdf"))
            (setq pdfpath (strcat dwgprefix pdfname))

            (prompt "\n----------------------------------------")
            (prompt (strcat "\n[" (itoa (1+ i)) "/" (itoa total) "] " pdfname))
            (prompt (strcat "\n블럭명: " (_item-name item)))
            (prompt (strcat "\n정렬기준 xmin=" (rtos (_item-xmin item) 2 3)
                            ", ymin=" (rtos (_item-ymin item) 2 3)))
            (prompt (strcat "\n저장 경로: " pdfpath))

            (_safe-call "Regen"
              'vlax-invoke-method
              (list doc 'Regen acAllViewports))

            (setq ok (_plot_block_to_pdf doc lay plotObj obj pdfpath))

            (if ok
              (progn
                (setq cnt (1+ cnt))
                (prompt "\nPDF 출력 완료.")
              )
              (prompt "\nPDF 출력 실패.")
            )

            (setq i (1+ i))
          )

          (prompt "\n========================================")
          (prompt (strcat "\n완료: " (itoa cnt) " / " (itoa total)))
        )
      )
    )
  )

  (setvar "BACKGROUNDPLOT" oldbg)
  (princ)
)
