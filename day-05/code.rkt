#lang racket

(require threading)

(define (file-name->stacks-and-instructions file-name)
  (let* ([whole-file       (file->string file-name)]
         [both             (string-split whole-file "\n\n")]
         [raw-stacks       (first both)]
         [raw-instructions (second both)]
         [stacks           (raw->parsed-stacks raw-stacks)]
         [instructions     (raw->parsed-instructions raw-instructions)])
    (for ([instruction instructions])
      (execute-move-instruction reverser instruction stacks))
    (apply string-append (map last (rest (vector->list stacks))))))

(define (raw->parsed-stacks raw-stacks)
  (let* ([stack-layers (string-split raw-stacks "\n")]
         [stacks       (layers->stacks stack-layers)])
    stacks))

(define (layers->stacks stack-layers)
  (list->vector (cons '()
                      (map (lambda (stack)
                             (filter non-empty-string? stack))
                           (pivot-lists (reverse (map layer->stacks stack-layers)))))))

(define (layer->stacks layer)
  (let* ([pieces (string->list layer)]
         [groups (group-count pieces 4)])
    (~>> groups
         (map (lambda~>> list->string
                         string-trim
                         (string-replace _ "[" "")
                         (string-replace _ "]" ""))))))

(define (group-count full-list count-per-group)
  (letrec ([accumulate (lambda (remaining)
                         (let ([number-left (length remaining)])
                           (cond [(zero? number-left) '()]
                                 [(< number-left count-per-group) (list remaining)]
                                 [else (cons (take remaining count-per-group)
                                             (accumulate (drop remaining count-per-group)))])))])
    (accumulate full-list)))

(define (pivot-lists list-of-lists)
  (cond [(empty? list-of-lists) '()]
        [(empty? (first list-of-lists)) '()]
        [else (cons (map first list-of-lists)
                    (pivot-lists (map rest list-of-lists)))]))

(struct move (number from to) #:transparent)

(define (raw->parsed-instructions raw-instructions)
  (map (lambda (raw-instruction)
         (let* ([instruction-line (string-split raw-instruction " ")]
                [number-of-boxes  (string->number (second instruction-line))]
                [from-stack       (string->number (fourth instruction-line))]
                [to-stack         (string->number (sixth  instruction-line))])
         (move number-of-boxes from-stack to-stack)))
       (filter non-empty-string? (string-split raw-instructions "\n"))))

(define (execute-move-instruction final-direction-fun move-instruction current-vector)
  (let* ([in-transit-boxes (final-direction-fun (take-right (vector-ref current-vector (move-from move-instruction))
                                                (move-number move-instruction)))])
    (begin (vector-set! current-vector
                        (move-from move-instruction)
                        (drop-right (vector-ref current-vector (move-from move-instruction))
                                    (move-number move-instruction)))
           (vector-set! current-vector
                        (move-to move-instruction)
                        (append (vector-ref current-vector (move-to move-instruction))
                                in-transit-boxes))
           current-vector)))

;(file-name->stacks-and-instructions "example.txt") ; "CMZ"
;(file-name->stacks-and-instructions "input.txt")   ; "RTGWZTHLD"

; Part 2

(define (reverser xs)
  (reverse xs))
(define (id xs) xs)

(define (file-name->queues-and-instructions file-name)
  (let* ([whole-file       (file->string file-name)]
         [both             (string-split whole-file "\n\n")]
         [raw-queues       (first both)]
         [raw-instructions (second both)]
         [queues           (raw->parsed-stacks raw-queues)]
         [instructions     (raw->parsed-instructions raw-instructions)])
    (for ([instruction instructions])
      (execute-move-instruction id instruction queues))
    (apply string-append (map last (rest (vector->list queues))))))

;(file-name->queues-and-instructions "example.txt") ; "MCD"
;(file-name->queues-and-instructions "input.txt")   ; "STHGRZZFR"
