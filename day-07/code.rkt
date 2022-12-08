#lang racket

(require threading)

; Part 1
;
(define (starts-with beginning entire)
  (equal? beginning (substring entire 0 (min (string-length entire)
                                             (string-length beginning)))))

(define (string-filter-split str separator)
  (filter non-empty-string? (string-split str separator)))

(define (parse-lines remaining-lines working-directory result-so-far)
  (cond [(empty? remaining-lines) result-so-far]
        [else (let* ([line    (first remaining-lines)]
                     [is-file (string->number (substring line 0 1))])
                (cond [(equal? line "$ cd ..")   (parse-lines (rest remaining-lines)
                                                              (move-up-directory working-directory)
                                                              result-so-far)]
                      [(starts-with "$ cd" line) (parse-lines (rest remaining-lines)
                                                              (change-directory working-directory (cd-line->directory line))
                                                              result-so-far)]
                      [is-file                   (parse-lines (rest remaining-lines)
                                                              working-directory
                                                              (add-file-size working-directory (file-line->file-size line) result-so-far))]
                      [else                      (parse-lines (rest remaining-lines)
                                                              working-directory
                                                              result-so-far)]))]))

(define (change-directory working-directory directory)
  (cond [(equal? working-directory directory) working-directory]
        [else (string-append working-directory
                 (if (equal? "/" working-directory) "" "/")
                 directory)]))
(define (move-up-directory working-directory)
  (cond [(or (zero?  (string-length working-directory))
             (equal? "/" working-directory))
         "/"]
        [else (string-append "/"
                             (string-join (drop-right (string-filter-split working-directory "/") 1)
                                          "/"))]))
(define (cd-line->directory cd-line)
  (last (filter non-empty-string? (string-split cd-line " "))))
(define (file-line->file-size file-line)
  (string->number (first (string-filter-split file-line " "))))
(define (add-file-size directory size result-so-far)
  (cond [(zero? (string-length directory)) result-so-far]
        [else   (add-file-size (if (equal? "/" directory) "" (move-up-directory directory))
                               size
                               (hash-update result-so-far
                                            directory
                                            (lambda (current-file-size)
                                              (+ current-file-size size))
                                            0))]))

(define (file-name->directory-hash file-name)
  (~>> file-name
       file->lines
       (parse-lines _ "/" (make-immutable-hash))))

(define (file-name->total-of-smaller-files file-name smaller-than)
  (~>> file-name
       file-name->directory-hash
       hash->list
       (filter (lambda (pair)
                 (<= (cdr pair) smaller-than)))
       (map (lambda (pair) (cdr pair)))
       (apply +)))

;(file-name->directory-hash "example.txt")
;(file-name->total-of-smaller-files "example.txt" 100000) ; 95437
;(file-name->directory-hash "input.txt")
;(file-name->total-of-smaller-files "input.txt" 100000)   ; 2031851

; Part 2
;
(define system-size 70000000)
(define needed-size 30000000)
(define (file-name->smallest-size-needed file-name total-size update-size)
  (let* ([directory-hash (file-name->directory-hash file-name)]
         [free-space     (- total-size (hash-ref directory-hash "/"))]
         [diff-size      (- update-size free-space)])
    (cond [(> free-space update-size) (error "Enough space is available:" free-space)]
          [else (~>> (hash->list directory-hash)
                     (sort _ (lambda (a b)
                               (< (cdr a) (cdr b))))
                     (findf (lambda (d)
                              (> (cdr d) diff-size)))
                     cdr)])))

;(file-name->smallest-size-needed "example.txt" system-size needed-size) ; 24933642
;(file-name->smallest-size-needed "input.txt" system-size needed-size) ;
