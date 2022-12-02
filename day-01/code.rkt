#lang racket

(require threading)

(define (file->elves file-name)
  (~> file-name
      file->string
      (string-split _ "\n\n" #:trim? #f)
      (map elf-string->elf-number _)))

(define (elf-string->elf-number elf-string)
  (~> elf-string
      (string-split _ "\n")
      (map string->number _)))

(define (calories->total calories)
  (apply + calories))

(define (file-name->max-calorie-total file-name)
  (~> file-name
      file->elves
      (map calories->total _)
      (apply max _)))
;(file-name->max-calorie-total "input.txt") ;67622

(define (file-name->max-calorie-totals file-name number-to-find)
  (~> file-name
      file->elves
      (map (lambda (elf) (apply + elf)) _)
      (sort _ (lambda (a b) (> a b)))
      (take _ number-to-find)
      calories->total))
;(file-name->max-calorie-totals "input.txt" 3) ;201491
