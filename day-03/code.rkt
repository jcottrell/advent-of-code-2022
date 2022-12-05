#lang racket

(require threading)

; Part 1
;
(define item-values
  (let ([components "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"])
    (make-hash (map (lambda (item priority)
                    (cons item priority))
                  (filter non-empty-string? (string-split components ""))
                  (inclusive-range 1 (string-length components))))))

(define (line->split-sack line)
  (let-values ([(compartment-a compartment-b)
                (let* ([items (list->vector (filter non-empty-string? (string-split line "")))]
                       [compartment-total (/ (vector-length items) 2)])
                  (vector-split-at items compartment-total))])
    (list (vector->list compartment-a) (vector->list compartment-b))))

(define (sack->sack-priority-total sack)
  (~> sack
      split-sack->shared-item
      item->priority))

(define (split-sack->shared-item split-sack)
  (let ([compartment-a (first split-sack)]
        [compartment-b (second split-sack)])
    (first (filter (lambda (a) (member a compartment-b)) compartment-a))))

(define (item->priority item)
  (hash-ref item-values item))

(define (file-name->split-sack-priority-total file-name)
  (~>> file-name
       file->lines
       (map line->split-sack)
       (map sack->sack-priority-total)
       (apply +)))

;(file-name->split-sack-priority-total "example.txt") ; 157
;(file-name->split-sack-priority-total "input.txt")   ; 8298
;
; Part 2
;
(define (line->sack line)
  (filter non-empty-string? (string-split line "")))

(define (group-count full-list count-per-group)
  (letrec ([accumulate (lambda (remaining)
                         (let ([number-left (length remaining)])
                           (cond [(zero? number-left) '()]
                                 [(< number-left count-per-group) (list remaining)]
                                 [else (cons (take remaining count-per-group)
                                             (accumulate (drop remaining count-per-group)))])))])
    (accumulate full-list)))

(define (sacks->groups-of-sacks sacks-per-group sacks)
  (group-count sacks sacks-per-group))

(define (group-of-sacks->shared-item group-of-sacks)
  (first (filter (lambda (item)
                   (andmap (lambda (other-sack)
                             (member item other-sack))
                           (rest group-of-sacks)))
                 (first group-of-sacks))))

(define (file-name->team-priority-total file-name)
  (~>> file-name
       file->lines
       (map line->sack)
       (sacks->groups-of-sacks 3)
       (map group-of-sacks->shared-item)
       (map item->priority)
       (apply +)))

;
;(file-name->team-priority-total "example.txt") ; 70
;(file-name->team-priority-total "input.txt") ; 2708
