(defconstant L (list 1 1 1 1 0 1 0 0 0 0 0 0 1 1))
; Функция сжатия списка
(defuncompress (x)
(if (consp x)
(compr (car x) 1 (cdr x))x))
; compr - рекурсивная функция
; elt - последний встреченный элемент
; lst - остаток списка, подлежащий дальнейшей компрессии
(defuncompr (elt n lst)
(if (nulllst)
(list (n-eltselt n))
(let ((next (carlst))) ; car от списка - это первый элемент в списке, (nth n list) возвращает n-нный элемент списка list.
(if (eqlnextelt)
(comprelt (+ n 1) (cdrlst)) ; cdr от списка - это оставшаяся часть списка
(cons (n-eltselt n) ; Функция (CONS O1 O2) возвращает точечную пару, у которой CAR-элемент указывает на OB1, а CDR-элемент - на OB2.
(comprnext 1 (cdrlst)))))))
; n-elts - возвращающая сжатое представление N элементов elt
(defun n-elts (elt n)
(if (> n 1)
(list n elt)elt))
; Функция расжатия списка
(defununcompress (lst)
(if (nulllst)
nil
(let ((elt (carlst))
(rest (uncompress (cdrlst))))
(if (conspelt)
; Функция (APPEND L1 L2 ... Ln) создает и возвращает список, состоящий из элементов списков, начиная со списка L1 и по список Ln.
(append (apply #'list-ofelt)rest) ; Синтаксис #'является синтаксической оберткой для FUNCTION.
(cons elt rest)))))
; listof - копированиеатомаираскрытиесписков
(defun list-of (n elt)
(if (zerop n)
nil
(cons elt (list-of (- n 1) elt))))
; Вызовфункцийсжатияирасжатия
(print (compress L))
(print (uncompress (compress L)))
