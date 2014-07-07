;; An implementation of the kmp string-search algorithm
;; in Scheme.
;;
;; The function takes two strings as arguments, S and W,
;; and, if S is found within W, returns the index in W
;; where the match begins. If no match is found,
;; it returns the length of S.

(define (kmp s w)
  (let* ((sl (string-length s)) (wl (string-length w)) (t (make-vector sl)))
    (define (populate t)
      (vector-set! t 0 -1)
      (vector-set! t 1 0)
      (let loop ((pos 2) (cnd 0))
        (if (< pos wl)
            (cond ((char=? (string-ref w (- pos 1)) (string-ref w cnd))
                   (loop (+ pos 1) (+ cnd 1))
                   (vector-set! t pos cnd))
                  ((> cnd 0) (loop pos (vector-ref t cnd)))
                  (else (loop (+ pos 1) cnd)
                        (vector-set! t pos 0))))))
    (populate t)
    ;; search
    (let loop ((m 0) (i 0))
      (define mi (+ m i))
      (if (< mi sl)
          (if (not (char=? (string-ref w i) (string-ref s mi)))
              (let ((ti (vector-ref t i)))
                (if (> ti -1)
                    (loop (- mi ti) ti)
                    (loop (+ m 1) 0)))
              (if (not (= i (- wl 1)))
                  (loop m (+ i 1))
                  m))
          sl))))

(display (kmp "foo bar raz wat" "raz")) ;; 8
