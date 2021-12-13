#lang racket

(define connections
  (map (λ (s) (string-split s "-"))
       (file->lines "day12.txt")))

(define (small-cave? cave)
  (and (equal? (string-downcase cave) cave)
       (not (equal? cave "start"))))

(define (find-connections cns cave)
  (filter (λ (x) (not (equal? "start" x)))
          (append (map second (filter (λ (x) (equal? cave (first x))) cns))
                  (map first (filter (λ (x) (equal? cave (second x))) cns)))))

(define (go cns cave seent)
  (define nexts (find-connections cns cave)) ; find every cave you can reach next from node
  (define unvisited (filter (λ (x) (not (member x seent))) nexts)) ; filter out the caves we've seen't
  (define new-seent (if (small-cave? cave) (cons cave seent) seent)) ; if it's a small cave, add it to the seen't list
  (define paths (map (λ (x) (go cns x new-seent)) unvisited)) ; recurse over all of the connections
  (cons cave (if (equal? "end" cave) ; if we've reach the end..
                 '() ; don't go back up the tree
                 paths)))

(define part1
  (let* ([tree (go connections "start" '())]
         [flat (flatten tree)]
         [ends (length (filter (λ (x) (equal? "end" x)) flat))])
    ends))

part1

; PART 2

(define (go2 cns cave seent revisitable)
  (define nexts (find-connections cns cave)) ; find every cave you can reach next from node
  (define new-seent (if (small-cave? cave) (cons cave seent) seent))
  (if (equal? "end" cave)
      1 ; add one when we get to the end
      (foldl (λ (x acc) (+ acc (if (and (member x new-seent) revisitable) ; if it's a small cave, and we haven't revisited yet
                                   (go2 cns x new-seent #f) ; recurse setting revisitable to #f
                                   (if (not (member x seent)) ; otherwise, if we hadn't seen it yet
                                       (go2 cns x new-seent revisitable) ; recurse
                                       0)))) ; otherwise, it doesn't end. add 0
             0 nexts)))

(define part2
  (go2 connections "start" '() #t))

part2