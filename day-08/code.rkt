#lang racket

(require threading)

(struct tree (height visible) #:transparent)

(define (pivot-lists list-of-lists)
  (cond [(empty? list-of-lists) '()]
        [(empty? (first list-of-lists)) '()]
        [else (cons (map first list-of-lists)
                    (pivot-lists (map rest list-of-lists)))]))

(define (file-name->visible-tree-count file-name)
  (~>> file-name
       file-name->forest
       forest->parsed-visibility-forest
       count-visible-trees))

(define (file-name->forest file-name)
  (~>> file-name
       file->lines
       (map (lambda (line)
              (filter non-empty-string?
                      (string-split line ""))))
       (map (lambda (string-digit-list) (map string->number string-digit-list)))
       (map tree-heights->initial-forest)))

(define (forest->parsed-visibility-forest forest)
  (~> forest
      record-visible-edge-trees
      record-visible-interior-trees))

(define (count-visible-trees forest)
  (~>> forest
      flatten
      (filter (lambda (a-tree) (tree-visible a-tree)))
      length))

(define (tree-heights->initial-forest list-of-tree-measurements)
  (map (lambda (measurement)
         (tree measurement #f))
       list-of-tree-measurements))

(define (record-visible-edge-trees forest)
  (~>> forest
       record-top-row-visible
       record-bottom-row-visible
       record-left-column-visible
       record-right-column-visible))

(define (record-top-row-visible forest)
  (append (list (map record-visible-tree (first forest)))
          (rest forest)))

(define (record-bottom-row-visible forest)
  (append (drop-right forest 1)
          (list (map record-visible-tree (last forest)))))

(define (record-left-column-visible forest)
  (let* ([turned (pivot-lists forest)])
    (pivot-lists (record-top-row-visible turned))))


(define (record-right-column-visible forest)
  (let* ([turned (pivot-lists forest)])
    (pivot-lists (record-bottom-row-visible turned))))

(define (record-visible-interior-trees forest)
  (~>> forest
      record-visible-interior-rows ; left-to-right
      (map reverse)
      record-visible-interior-rows ; right-to-left
      (map reverse)
      pivot-lists
      record-visible-interior-rows ; top-to-bottom
      (map reverse)
      record-visible-interior-rows ; bottom-to-top
      (map reverse)
      pivot-lists))

(define (record-visible-interior-rows forest)
  (map record-visible-tall-trees forest))

(define (record-visible-tall-trees tree-row)
  (letrec ([evaluate-tree (lambda (tallest-tree-so-far remaining-trees)
                            (cond [(empty? remaining-trees) '()]
                                  [else (let* ([current-tree (first remaining-trees)]
                                               [taller-tree  (if (> (tree-height current-tree)
                                                                    (tree-height tallest-tree-so-far))
                                                                 current-tree
                                                                 tallest-tree-so-far)])
                                          (cons (tree (tree-height current-tree)
                                                      (record-visible-tall-tree current-tree tallest-tree-so-far))
                                                (evaluate-tree taller-tree (rest remaining-trees))))]))])
    (cons (first tree-row) (evaluate-tree (first tree-row) (rest tree-row)))))

(define (record-visible-tree t)
  (tree (tree-height t) #t))

(define (record-visible-tall-tree tree-to-evaluate tallest-neighbor)
  (or (tree-visible tree-to-evaluate);; visible from another angle => still visible
      (> (tree-height tree-to-evaluate)
         (tree-height tallest-neighbor))))

(define example (list
 (list (tree 3 #f) (tree 0 #f) (tree 3 #f) (tree 7 #f) (tree 3 #f))
 (list (tree 2 #f) (tree 5 #f) (tree 5 #f) (tree 1 #f) (tree 2 #f))
 (list (tree 6 #f) (tree 5 #f) (tree 3 #f) (tree 3 #f) (tree 2 #f))
 (list (tree 3 #f) (tree 3 #f) (tree 5 #f) (tree 4 #f) (tree 9 #f))
 (list (tree 3 #f) (tree 5 #f) (tree 3 #f) (tree 9 #f) (tree 0 #f))))

;(file-name->visible-tree-count "example.txt") ; 21
;(file-name->visible-tree-count "input.txt")   ; 1763
