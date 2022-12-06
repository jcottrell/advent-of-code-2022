#lang racket

(require threading)

; Part 1
;
(define (line->pairs line)
  (map (lambda (dashed-pair)
         (let ([both (string-split dashed-pair "-")])
           (cons (string->number (first both)) (string->number (second both)))))
       (string-split line ",")))

(define (wrapping-pair? two-pair)
  (let ([left  (first two-pair)]
        [right (second two-pair)])
    (or (and (<= (car left)  (car right))
             (>= (cdr left)  (cdr right)))
        (and (<= (car right) (car left))
             (>= (cdr right) (cdr left))))))

(define (file-name->wrapping-count file-name)
  (~>> file-name
       file->lines
       (map line->pairs)
       (filter wrapping-pair?)
       length))

;(file-name->wrapping-count "example.txt") ; 2
;(file-name->wrapping-count "input.txt")   ; 582

; Part 2
;
(define (between test-value minimum maximum)
  (and (>= test-value minimum)
       (<= test-value maximum)))

(define (overlapping-pair? two-pair)
  (let ([left  (first two-pair)]
        [right (second two-pair)])
    (or (between (car left)  (car right) (cdr right))
        (between (cdr left)  (car right) (cdr right))
        (between (car right) (car left)  (cdr left))
        (between (cdr right) (car left)  (cdr left)))))

(define (file-name->overlapping-count file-name)
  (~>> file-name
       file->lines
       (map line->pairs)
       (filter overlapping-pair?)
       length))

;(file-name->overlapping-count "example.txt") ; 4
;(file-name->overlapping-count "input.txt")   ; 893
