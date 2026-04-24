;--------------------------------------------------
;  MPDF - Multiple PDF Plot Utility
;  Encoding: ANSI (CP949)
;  Environment: ZWCAD / AutoCAD on Korean Windows
;  Note: Open and edit this file with ANSI encoding
;           to avoid broken Korean comments.
;--------------------------------------------------



(vl-load-com)

;;; =========================
;;; ڿ и
;;; =========================
(defun xltbl-split (str delim / pos out dlen)
  (setq out '())
  (setq dlen (strlen delim))
  (while (setq pos (vl-string-search delim str))
    (setq out (cons (substr str 1 pos) out))
    (setq str (substr str (+ pos dlen 1)))
  )
  (reverse (cons str out))
)

;;; =========================
;;;  nth
;;; =========================
(defun xltbl-nth (n lst)
  (if (and (listp lst) (>= n 0) (< n (length lst)))
    (nth n lst)
    ""
  )
)

;;; =========================
;;;  
;;; =========================
(defun xltbl-last1 (lst)
  (if (and lst (listp lst))
    (car (reverse lst))
    0.0
  )
)

;;; =========================
;;;   
;;; =========================
(defun xltbl-rtrim (s)
  (while
    (and (> (strlen s) 0)
         (member (substr s (strlen s) 1) '(" " "\t" "\r" "\n")))
    (setq s (substr s 1 (1- (strlen s))))
  )
  s
)

;;; =========================
;;; Ŭ ؽƮ 
;;; =========================
(defun xltbl-get-clipboard-text (/ html win clip txt)
  (setq txt nil)
  (setq html (vlax-create-object "htmlfile"))
  (if html
    (progn
      (setq win  (vlax-get html 'ParentWindow))
      (setq clip (vlax-get win 'ClipboardData))
      (if clip
        (setq txt (vlax-invoke clip 'GetData "Text"))
      )
      (if clip (vlax-release-object clip))
      (if win  (vlax-release-object win))
      (vlax-release-object html)
    )
  )
  txt
)

;;; =========================
;;; Ŭ ؽƮ -> 2 Ʈ
;;; =========================
(defun xltbl-clipboard->table (txt / rows parsed row clean)
  (setq txt (vl-string-translate "\r" "" txt))
  (setq rows (xltbl-split txt "\n"))
  (setq parsed '())
  (foreach row rows
    (setq clean (xltbl-rtrim row))
    (if (/= clean "")
      (setq parsed (append parsed (list (xltbl-split row "\t"))))
    )
  )
  parsed
)

;;; =========================
;;; ִ  
;;; =========================
(defun xltbl-max-cols (data / m)
  (setq m 0)
  (foreach r data
    (if (> (length r) m)
      (setq m (length r))
    )
  )
  m
)

;;; =========================
;;; ؽƮ  
;;; =========================
(defun xltbl-estimate-width (s txtH / n)
  (setq n (strlen (vl-princ-to-string s)))
  ;; 뷫  
  ;; ѱ/ ȥ Ͽ ణ ˳ϰ
  (+ (* txtH 0.8 n) (* txtH 1.2))
)

;;; =========================
;;;  ʺ 
;;; =========================
(defun xltbl-calc-col-widths (data txtH margin / cols c row cell maxw widths w)
  (setq cols (xltbl-max-cols data))
  (setq c 0)
  (setq widths '())
  (while (< c cols)
    (setq maxw 0.0)
    (foreach row data
      (setq cell (xltbl-nth c row))
      (setq w (xltbl-estimate-width cell txtH))
      (if (> w maxw)
        (setq maxw w)
      )
    )
    (setq widths (append widths (list (+ maxw (* 2.0 margin)))))
    (setq c (1+ c))
  )
  widths
)

;;; =========================
;;;  
;;; : (10 20 30) -> (0 10 30 60)
;;; =========================
(defun xltbl-offsets (widths / out sum w)
  (setq out '(0.0))
  (setq sum 0.0)
  (foreach w widths
    (setq sum (+ sum w))
    (setq out (append out (list sum)))
  )
  out
)

;;; =========================
;;; ̾ 
;;; =========================
(defun xltbl-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmakex
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 name)
        (cons 70 0)
        (cons 62 color)
        (cons 6 "Continuous")
      )
    )
  )
)

;;; =========================
;;;  
;;; =========================
(defun xltbl-add-line (p1 p2 layer)
  (entmakex
    (list
      '(0 . "LINE")
      (cons 8 layer)
      (cons 10 p1)
      (cons 11 p2)
    )
  )
)

;;; =========================
;;;   (  TEXT)
;;; =========================
(defun xltbl-add-text-center (pt txt h layer)
  (entmakex
    (list
      '(0 . "TEXT")
      (cons 8 layer)
      (cons 10 pt)
      (cons 11 pt)
      (cons 40 h)
      (cons 1 (vl-princ-to-string txt))
      (cons 7 (getvar "TEXTSTYLE"))
      (cons 72 1)
      (cons 73 2)
    )
  )
)

;;; =========================
;;; ڵ  
;;; =========================
(defun xltbl-get-auto-text-height (/ h)
  (setq h (getvar "TEXTSIZE"))
  (if (or (null h) (<= h 0.0))
    (setq h 5.0)
  )
  h
)

;;; =========================
;;;  ɾ
;;; =========================
(defun c:XT (/ clip data ins txtH rowH margin cols rows widths offs totalW totalH
                  lineLayer textLayer r c row cell x y cx cy)

  (vl-load-com)

  (setq clip (xltbl-get-clipboard-text))

  (if (or (null clip) (= (xltbl-rtrim clip) ""))
    (progn
      (princ "\nŬ尡  ֽϴ.    Ctrl+C ϼ.")
      (princ)
    )
    (progn
      (setq data (xltbl-clipboard->table clip))

      (if (or (null data) (= (length data) 0))
        (progn
          (princ "\n ͸ ǥ ؼ  ϴ.")
          (princ)
        )
        (progn
          ;;  Է
          (setq ins (getpoint "\nǥ (») : "))
          (if (null ins)
            (setq ins '(0.0 0.0 0.0))
          )

          ;; ڵ  
          (setq txtH   (xltbl-get-auto-text-height))
          (setq rowH   (* txtH 2.0))
          (setq margin (* txtH 0.8))

          ;; ʿ  Ʒó  ٲ㵵 
          ;; (setq txtH 5.0)
          ;; (setq rowH 20.0)
          ;; (setq margin 3.0)

          (setq cols   (xltbl-max-cols data))
          (setq rows   (length data))
          (setq widths (xltbl-calc-col-widths data txtH margin))
          (setq offs   (xltbl-offsets widths))
          (setq totalW (xltbl-last1 offs))
          (setq totalH (* rows rowH))

          (setq lineLayer "XLTBL_LINE")
          (setq textLayer "XLTBL_TEXT")
          (xltbl-ensure-layer lineLayer 7)
          (xltbl-ensure-layer textLayer 2)

          ;; μ
          (setq r 0)
          (while (<= r rows)
            (setq y (- (cadr ins) (* r rowH)))
            (xltbl-add-line
              (list (car ins) y 0.0)
              (list (+ (car ins) totalW) y 0.0)
              lineLayer
            )
            (setq r (1+ r))
          )

          ;; μ
          (setq c 0)
          (while (<= c cols)
            (setq x (+ (car ins) (xltbl-nth c offs)))
            (xltbl-add-line
              (list x (cadr ins) 0.0)
              (list x (- (cadr ins) totalH) 0.0)
              lineLayer
            )
            (setq c (1+ c))
          )

          ;; 
          (setq r 0)
          (foreach row data
            (setq c 0)
            (while (< c cols)
              (setq cell (xltbl-nth c row))
              (setq cx (+ (car ins)
                          (xltbl-nth c offs)
                          (/ (xltbl-nth c widths) 2.0)))
              (setq cy (- (cadr ins)
                          (* r rowH)
                          (/ rowH 2.0)))
              (xltbl-add-text-center (list cx cy 0.0) cell txtH textLayer)
              (setq c (1+ c))
            )
            (setq r (1+ r))
          )

          (princ
            (strcat
              "\nǥ  Ϸ: "
              (itoa rows) " x "
              (itoa cols) ""
              " / ڳ="
              (rtos txtH 2 2)
            )
          )
          (princ)
        )
      )
    )
  )
)