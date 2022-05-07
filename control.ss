;;;; Several control structures implemented by `call/cc`.
;;;; Author: Wasiv <Matheritasiv@gmail.com>
;;{{{ generator/yield
(begin
;;{{{ Implementation 1
(begin
(define ($generator proc)
  (letrec ([resume (lambda (cont) (proc
      (lambda (ret)
        (call/cc (lambda (k)
            (set! resume (lambda (kont)
                (set! cont kont) (k (void))))
            (cont ret))))
      (lambda (ret)
        (set! resume (lambda (_) ret))
        (cont ret))))])
    (lambda () (call/cc (lambda (k) (resume k))))))

(define-syntax generator
  (lambda (x) (syntax-case x ()
      [(name body ...)
       (with-syntax ([yield (datum->syntax #'name 'yield)])
         #'($generator (lambda (yield end)
               (end (begin body ...)))))])))
) ;;}}}
;;{{{ Implementation 2
#;(begin
(define ($generator proc)
  (letrec ([resume (lambda (cont) (proc
      (lambda (ret)
        (call/cc (lambda (k)
            (set! resume (lambda (kont)
                (set! cont kont) (k (void))))
            (cont ret))))
      (lambda ()
        (set! resume (lambda (_)
            (error #f "generator exhausted.")))
        (resume cont))))])
    (lambda () (call/cc (lambda (k) (resume k))))))

(define-syntax generator
  (lambda (x) (syntax-case x ()
      [(name body ...)
       (with-syntax ([yield (datum->syntax #'name 'yield)])
         #'($generator (lambda (yield end)
               body ... (end))))])))
) ;;}}}
;;{{{ Test
(define G (generator
  (let loop ([q 1] [r 180] [t 60] [i 2])
    (let* ([3i+1 (1+ (* i 3))] [u (* 3 3i+1 (1+ 3i+1))]
           [y (quotient (+ (* q (- (* 27 i) 12)) (* 5 r)) (* 5 t))])
      (yield y)
      (loop (* 10 q i (1- (* 2 i)))
            (* 10 u (+ (* q (- (* 5 i) 2)) r (- (* y t))))
            (* t u) (1+ i))))))

(define (G-bounded n) (generator
  (let loop ([n n])
    (when (positive? n)
      (yield (G)) (loop (1- n))))
  (yield #\$) #\.))

(display "Test for generator/yield:") (newline)
(display (G)) (display #\.)
(define GB (G-bounded 50))
(let loop ([n 100])
  (when (positive? n)
    (display (GB))
    (loop (1- n)))) (newline)
(newline)
;;}}}
) ;;}}}
;;{{{ coroutine/resume
(begin
;;{{{ Implementation
(define ($coroutine proc)
  (letrec* (
      [resume0 (lambda (_) (proc
          (lambda (cor ret)
            (call/cc (lambda (k)
                (set! resume k) (cor ret))))
          (lambda ()
            (set! resume (lambda (_)
                (error #f "coroutine ended."))))))]
      [resume resume0])
    (case-lambda
      [() (set! resume resume0) (resume (void))]
      [(x) (resume x)])))

(define-syntax coroutine
  (lambda (x) (syntax-case x ()
      [(name body ...)
       (with-syntax ([resume (datum->syntax #'name 'resume)])
         #'($coroutine (lambda (resume end)
               body ... (end))))])))
;;}}}
;;{{{ Test
;;{{{ Calculation coroutine
(define (make-calc-coroutine caller b factor coeff)
  (coroutine
    (let-values ([(factor coeff)
        (let* ([t2 (* factor factor)] [/1+t2 (/ (1+ t2))] [f (* 2 t2 /1+t2)]
               [c (* coeff factor (denominator f) /1+t2)])
          (values f c))])
      (let* ([numer (numerator factor)]
             [denom (denominator factor)] [2denom (* 2 denom)]
             [h (denominator coeff)] [a (expt 10 b)]
             [u (exact (floor
                  (/ (+ (* (1+ b) (log 10)) (log coeff))
                     (- (log 2denom) (log numer)))))])
        (let loop ([i 0] [f (list 0)])
          (let ([f (append! (make-list (* u 2) (numerator coeff)) f)])
            (let loop ([f
                (let loop ([j i] [f (list-tail f u)])
                  (if (zero? j) f (begin
                    (let loop ([f f] [d 0] [j (* u 3)] [b (* (+ i j 1) u)]
                               [g (* denom (1- (* 2 (+ i j 1) u)))])
                      (if (<= j 1) (set-car! f (+ (car f) (* d b numer)))
                        (let-values ([(x y) (div-and-mod
                            (+ (* (car f) (if (> j u) a 1)) (* d b numer)) g)])
                          (set-car! f y)
                          (loop (cdr f) x (1- j) (1- b) (- g 2denom)))))
                    (loop (1- j) (list-tail f u)))))]
                [d 0] [b (* (1+ i) u)] [g (* denom (1- (* 2 (1+ i) u)))])
              (if (null? (cdr f))
                (let-values ([(x y) (div-and-mod
                    (+ (* (car f) a) d) h)])
                  (set-car! f y)
                  (resume caller x))
                (let-values ([(x y) (div-and-mod
                    (+ (* (car f) a) (* d b numer)) g)])
                  (set-car! f y)
                  (loop (cdr f) x (1- b) (- g 2denom)))))
            (loop (1+ i) f)))))))
;;}}}
;;{{{ Print coroutine
(define (make-print-coroutine count b comb . calc-list)
  (coroutine
    (let ([a (expt 10 b)] [dot #t])
      (let loop ([count count] [next 0] [e #f])
        (unless (zero? count)
          (let ([l (map (lambda (c) (resume c 'nil)) calc-list)])
            (if e
              (let-values ([(s t) (div-and-mod next a)])
                (if dot (begin
                    (printf "~d." (+ s e))
                    (set! dot #f))
                  (printf "~v,'0d" b (+ s e)))
                (loop (1- count) (apply comb l) t))
              (loop (1- count) (apply comb l) 0))))))))
;;}}}
(display "Test for coroutine/resume:") (newline)
(let ([b 1000])
  (letrec* ([pr (lambda (x) (printer x))]
            [calc1 (make-calc-coroutine pr b 1/7 20)]
            [calc2 (make-calc-coroutine pr b 3/79 8)]
            [printer (make-print-coroutine 3 b + calc1 calc2)])
    (printer) (newline) (printer) (newline)))
(newline)
;;}}}
) ;;}}}
;;{{{ reset/shift
(begin
;;{{{ Implementation
;;{{{ Way 1
(define ($reset proc)
  (call/cc (lambda (k)
      (let* ([ks k] [end (lambda (x) (ks x))])
        (proc
          (lambda (cont)
            (call/cc (lambda (kont) (let ([prev ks])
                (end (cont (lambda (x)
                    (call/cc (lambda (k)
                        (set! ks (lambda (x)
                            (set! ks prev) (k x)))
                        (kont x))))))))))
          end)))))
;;}}}
;;{{{ Way 2
#;(define ($reset proc)
  (let* ([klist '()] [end (lambda (ret)
      (let ([k (car klist)])
        (set! klist (cdr klist))
        (k ret)))])
    (call/cc (lambda (k)
        (set! klist (cons k klist))
        (proc
          (lambda (cont)
            (call/cc (lambda (kont) (end
                (cont (lambda (x)
                    (call/cc (lambda (k)
                        (set! klist (cons k klist))
                        (kont x)))))))))
          end)))))
;;}}}
(define-syntax reset
  (lambda (x) (syntax-case x ()
      [(name body ...)
       (with-syntax ([shift (datum->syntax #'name 'shift)])
         #'($reset (lambda ($shift end)
             (let-syntax ([shift (syntax-rules ()
                 [(_ k bd (... ...))
                  ($shift (lambda (k) bd (... ...)))])])
               (end (begin body ...))))))])))
;;}}}
;;{{{ Test
(define-syntax show
  (syntax-rules ()
    ((_ expr ...)
     (begin
       (begin
         (display expr)
         (newline))
       ...))))
(show "Test for reset/shift:"
  (1+ (reset (* 2 (shift k (k (k 4))))))     ;==> 17
  (reset
    (shift k (cons 1 (k (void))))
    (shift k (cons 2 (k (void))))
    (shift k (cons 3 (k (void))))
    '())                                     ;==> (1 2 3)
  (let ([i 0])
    (reset
      (set! i (1+ i))
      (shift k (+ i (k i)))
      (set! i (1+ i))
      (shift k (+ i (k i)))))                ;==> 5
  (let ([i 0])
    (reset
      (set! i (1+ i))
      (shift k (+ i (k i) (k i)))
      (set! i (1+ i))
      (shift k (+ i (k i)))))                ;==> 11 | 12 | 13
  (let ([i 0])
    (reset
      (set! i (1+ i))
      (shift k (+ i (k i)))
      (set! i (1+ i))
      (shift k (+ i (k i) (k i)))))          ;==> 7
  (let ([i 0])
    (reset
      (set! i (1+ i))
      (shift k (+ i (k i) (k i)))
      (set! i (1+ i))
      (shift k (+ i (k i) (k i)))))          ;==> 16 | 17 | 18
) (newline)
;;}}}
) ;;}}}
;;{{{ prompt/control
(begin
;;{{{ Implementation
(define ($prompt proc)
  (let ([klist '()])
    (call/cc (lambda (k)
        (proc
          (lambda (cont)
            (call/cc (lambda (kont)
                (let ([kl klist] [id (length klist)])
                  (set! klist (append klist (list (void))))
                  ((lambda (x)
                     ((if (<= (length klist) (1+ id)) k
                        (list-ref klist (1+ id))) x))
                   (cont (lambda (x)
                       ((lambda (x)
                          (set! klist (append kl
                              (list-tail klist id))) x)
                        (call/cc (lambda (k)
                            (set-car! (list-tail klist id) k)
                            (kont x)))))))))))
          (lambda (ret)
            ((if (null? klist) k (car klist)) ret)))))))

(define-syntax prompt
  (lambda (x) (syntax-case x ()
      [(name body ...)
       (with-syntax ([control (datum->syntax #'name 'control)])
         #'($prompt (lambda ($control end)
             (let-syntax ([control (syntax-rules ()
                 [(_ k bd (... ...))
                  ($control (lambda (k) bd (... ...)))])])
               (end (begin body ...))))))])))
;;}}}
;;{{{ Test
(define-syntax show-with-engine
  (syntax-rules ()
    ((_ expr ...)
     (begin
       ((make-engine (lambda () expr)) 1000
        (lambda (t v)
          (display v) (newline))
        (lambda (k)
          (printf "\x1b;[31mTime exceeded.\x1b;[m~%")))
       ...))))
(show-with-engine "Test for prompt/control:"
  (1+ (prompt (* 2 (control k (k (k 4))))))  ;==> 17
  (prompt
    (control k (cons 1 (k (void))))
    (control k (cons 2 (k (void))))
    (control k (cons 3 (k (void))))
    '())                                     ;==> (3 2 1)
  (let ([i 0])
    (prompt
      (set! i (1+ i))
      (control k (+ i (k i)))
      (set! i (1+ i))
      (control k (+ i (k i)))))              ;==> 5
  (let ([i 0])
    (prompt
      (set! i (1+ i))
      (control k (+ i (k i) (k i)))
      (set! i (1+ i))
      (control k (+ i (k i)))))              ;==> 11 | 12 | 13
  (let ([i 0])
    (prompt
      (set! i (1+ i))
      (control k (+ i (k i)))
      (set! i (1+ i))
      (control k (+ i (k i) (k i)))))        ;==> 8
  (let ([i 0])
    (prompt
      (set! i (1+ i))
      (control k (+ i (k i) (k i)))
      (set! i (1+ i))
      (control k (+ i (k i) (k i)))))        ;==> ⊥
) (newline)
;;}}}
) ;;}}}
;;{{{ amb
(begin
;;{{{ Implementation 1
(begin
(define-syntax with-amb
  (lambda (x)
    (syntax-case x ()
      [(name body ...)
       (with-syntax ([amb (datum->syntax #'name 'amb)])
         #'(let ([amb-fail (list (lambda ()
               (error #f "amb tree exhausted")))])
             (let-syntax ([amb (lambda (x) (syntax-case x ()
                 [(_ alts (... ...))
                  #'(call/cc (lambda (cont)
                        ((lambda _ ((car amb-fail)))
                         (call/cc (lambda (k)
                             (set! amb-fail (cons (lambda ()
                                 (set! amb-fail (cdr amb-fail))
                                 (k (void))) amb-fail))
                             (cont alts)))
                         (... ...))))]))])
               body ...)))])))
) ;;}}}
;;{{{ Implementation 2
#;(begin
(define-syntax with-amb
  (lambda (x)
    (syntax-case x ()
      [(name body ...)
       (with-syntax ([amb (datum->syntax #'name 'amb)])
         #'(let ([amb-fail (lambda ()
               (error #f "amb tree exhausted"))])
             (let-syntax ([amb (lambda (x) (letrec ([shuffle! (lambda (l)
                 (if (null? l) l
                   (let* ([n (length l)] [m (random n)])
                     (if (zero? m)
                       (begin (set-cdr! l (shuffle! (cdr l))) l)
                       (let* ([r (list-tail l (1- m))]
                              [p (cdr r)] [n (cdr l)])
                         (set-cdr! l (cdr p))
                         (set-cdr! p (shuffle!
                             (if (= m 1) l
                               (begin (set-cdr! r l) n))))
                         p)))))])
               (random-seed (time-nanosecond (current-time)))
               (syntax-case x ()
                 [(_ alts (... ...))
                  (with-syntax ([(alts (... ...))
                      (shuffle! (syntax->list #'(alts (... ...))))])
                    #'(let ([prev amb-fail])
                        (call/cc (lambda (cont)
                            (call/cc (lambda (k)
                                (set! amb-fail (lambda ()
                                    (set! amb-fail prev)
                                    (k (void))))
                                (cont alts))) (... ...)
                            (prev)))))])))])
               body ...)))])))
) ;;}}}
;;{{{ Test
(display "Test for amb:") (newline)
(display
  (with-amb
    (let ([a (amb 1 2 3)])
      (if (not (= a 3)) (amb))
      (amb (amb) (with-amb (amb))
        (let ([amb (lambda () a)])
          (amb)))))                          ;==> 3 | ⊥
) (newline)
(newline)
;;}}}
) ;;}}}
