#lang racket

(require threading)

(struct shot (name amount beats loses))
(define rock     (shot 'rock      1 '(scissors) '(paper)))
(define paper    (shot 'paper     2 '(rock)     '(scissors)))
(define scissors (shot 'scissors  3 '(paper)    '(rock)))

(struct status (outcome value))
(define win  (status 'win  6))
(define loss (status 'lose 0))
(define draw (status 'draw 3))

(define them
  (hash "A" rock
        "B" paper
        "C" scissors))
(define us
  (hash "X" rock
        "Y" paper
        "Z" scissors))
(define outcomes
  (hash "X" (hash 'rock     (hash-ref us "Z")
                  'paper    (hash-ref us "X")
                  'scissors (hash-ref us "Y"))
        "Y" (hash 'rock     (hash-ref us "X")
                  'paper    (hash-ref us "Y")
                  'scissors (hash-ref us "Z"))
        "Z" (hash 'rock     (hash-ref us "Y")
                  'paper    (hash-ref us "Z")
                  'scissors (hash-ref us "X"))))

(define (get-us their-throw outcome)
  (hash-ref (hash-ref outcomes outcome) (shot-name (hash-ref them their-throw))))

(define (get-score their-throw outcome)
  (let* ([our-full-throw    (get-us their-throw outcome)]
         [their-full-throw  (hash-ref them their-throw)]
         [our-throw-score   (shot-amount    our-full-throw)]
         [our-win-score     (our-win-amount our-full-throw
                                            their-full-throw)])
    (+ our-throw-score our-win-score)))

(define (our-win-amount us them)
  (status-value (cond [(member (shot-name us) (shot-loses them)) win]
                      [(member (shot-name us) (shot-beats them)) loss]
                      [else draw])))

(define (game-line->score game-line)
  (let ([pair (string-split game-line " ")])
    (get-score (first pair) (second pair))))

(define (game-lines->total-score lines)
  (apply + (map game-line->score lines)))

(define (file-name->total-score file-name)
  (~>> file-name
       file->lines
       game-lines->total-score))

(define example '("A Y" "B X" "C Z"))
;;part one:
;(game-lines->total-score example) ; 15
;(file-name->total-score "input.txt") ; 12276
;;part two:
;(game-lines->total-score example) ; 12
;(file-name->total-score "input.txt") ; 9975
