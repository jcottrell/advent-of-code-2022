#lang racket

(require threading)

; Part 1
;
(define (file-name->last-position-of-marker file-name)
  (let* ([in (open-input-file file-name #:mode 'text)])
    (call/cc (lambda (quick-exit)
               (letrec ([consume (lambda (counter so-far)
                                   (cond [(eof-object? in) (close-input-port in)]
                                         [(and (= 4 (length so-far))
                                               (not (check-duplicates so-far)))
                                          (quick-exit (begin (close-input-port in)
                                                             counter))]
                                         [else (consume (add1 counter)
                                                        (take-right (append so-far (list (read-string 1 in)))
                                                                    (min (add1 (length so-far)) 4)))]))])
                 (consume 1 (list (read-string 1 in)))))))))

;(file-name->last-position-of-marker "example1.txt") ; 7
;(file-name->last-position-of-marker "example2.txt") ; 5
;(file-name->last-position-of-marker "example3.txt") ; 6
;(file-name->last-position-of-marker "example4.txt") ; 10
;(file-name->last-position-of-marker "example5.txt") ; 11
;(file-name->last-position-of-marker "input.txt")    ; 1356
