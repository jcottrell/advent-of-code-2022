#lang racket

(require threading)

; Part 1
;
(define (file-name->last-position-of-marker marker-window-size file-name)
  (let* ([in (open-input-file file-name #:mode 'text)])
    (call/cc (lambda (quick-exit)
               (letrec ([consume (lambda (counter so-far)
                                   (cond [(eof-object? in) (close-input-port in)]
                                         [(and (= marker-window-size (length so-far))
                                               (not (check-duplicates so-far)))
                                          (quick-exit (begin (close-input-port in)
                                                             counter))]
                                         [else (consume (add1 counter)
                                                        (take-right (append so-far (list (read-string 1 in)))
                                                                    (min (add1 (length so-far))
                                                                         marker-window-size)))]))])
                 (consume 1 (list (read-string 1 in))))))))

;(file-name->last-position-of-marker 4 "example1.txt") ; 7
;(file-name->last-position-of-marker 4 "example2.txt") ; 5
;(file-name->last-position-of-marker 4 "example3.txt") ; 6
;(file-name->last-position-of-marker 4 "example4.txt") ; 10
;(file-name->last-position-of-marker 4 "example5.txt") ; 11
;(file-name->last-position-of-marker 4 "input.txt")    ; 1356

; Part 2
;(file-name->last-position-of-marker 14 "example1.txt") ; 19
;(file-name->last-position-of-marker 14 "example2.txt") ; 23
;(file-name->last-position-of-marker 14 "example3.txt") ; 23
;(file-name->last-position-of-marker 14 "example4.txt") ; 29
;(file-name->last-position-of-marker 14 "example5.txt") ; 26
;(file-name->last-position-of-marker 14 "input.txt")    ; 2564
