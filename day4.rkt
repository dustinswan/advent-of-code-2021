#lang racket

; util

(define (split-on lst x) ; like splitOn in haskell. couldn't find this in the std lib..
  (foldr (λ (element next)
           (if (equal? element x)
               (cons empty next)
               (cons (cons element (first next)) (rest next))))
         (list empty) lst))

(define (map-map f lst) ; why tf can't i do (compose map map)
  (map (λ (x) (map (λ (y) (f y)) x)) lst))

(define (map-map-map f lst) ; (map . map . map)
  (map (λ (x) (map-map f x)) lst))

(define (transpose xss)
  (apply map list xss))

(define (all? f list)
  (not (findf (λ (x) (not (f x))) list)))

; IO and setup

(define lines
  (file->lines "day4.txt"))

(define numbers ; grab first line of file, split into a list on ,
  (map string->number (string-split (first lines) ",")))

(define boards
  (let* ([str-lists (split-on (drop lines 2) "")] ; split on empty lines
         [split-up (map-map string-split str-lists)] ; convert '("1 2 3") to '("1" "2" "3")
         [num-lists (map-map-map string->number split-up)]) ; convert '("1" "2" "3") to '(1 2 3)
    (map-map-map (λ (x) (cons x #f)) num-lists))); change each cell to (<number>, <picked>)

; part 1

(define (mark-board board number) ; mark up a board with #t or #f based on the number
  (map-map (λ (cell)
             (match cell
               [(cons num on) (cons num (or on (= num number)))]))
           board))

(define (mark-boards boards number) ; mark all boards based on the number
  (map (λ (board) (mark-board board number)) boards))

(define (all-winners? lst) ; check if all pairs in the list are #t
  (all? cdr lst))

(define (is-winner? board) ; check if any row or column in the board is a winner
  (let ([cols (transpose board)])
    (or
     (findf all-winners? board)
     (findf all-winners? cols))))

(define (mark-all-until-win boards numbers)
  (foldl (λ (number acc)
           (match acc
             ([cons boards last-number]
              (if (findf is-winner? boards) ; if we have any winners
                  (cons boards last-number) ; just return the boards
                  (cons (mark-boards boards number) number))))) ; otherwise keep marking
         (cons boards 0) numbers))

(define (sum-unmarked board)
  (let* ([cells (apply append board)] ; flatten it
         [unmarked (filter (λ (x) (not (cdr x))) cells)] ; filter out marked cells
         [nums (map car unmarked)])
         (foldl + 0 nums)))

(define part1
  (let* ([marked-up (mark-all-until-win boards numbers)] ; get (the final board state . last number)
         [marked-boards (car marked-up)] ; car out the boards
         [last-number (cdr marked-up)] ; cdr out the last number
         [winning-board (findf is-winner? marked-boards)] ; find the winning board
         [unmarked-sum (sum-unmarked winning-board)]) ; sum up the unmarked cells
    (* unmarked-sum last-number)))

part1

; part 2

(define (mark-all-until-one boards numbers)
  (foldl (λ (number acc)
           (match acc
             ([cons boards last-number]
              (if (= (length boards) 1) ; if we have only have 1 board left
                  (cons boards last-number) ; just return it
                  (let ([filtered-boards (filter (λ (x) (not (is-winner? x))) boards)]) ; otherwise filter out the winners
                    (cons (mark-boards filtered-boards number) number)))))) ; and then keep marking
         (cons boards 0) numbers))

(define part2
  (let* ([marked-up (mark-all-until-one boards numbers)] ; get (the final board state . last number)
         [marked-boards (car marked-up)] ; car out the boards
         [last-number (cdr marked-up)] ; cdr out the last number
         [winning-board (findf is-winner? marked-boards)] ; find the winning board
         [unmarked-sum (sum-unmarked winning-board)]) ; sum up the unmarked cells
    (* unmarked-sum last-number)))

part2
