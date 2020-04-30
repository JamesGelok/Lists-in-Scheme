;=================================================================;
;   _____      _                           _      _     _         ;
;  / ____|    | |                         | |    (_)   | |        ;
; | (___   ___| |__   ___ _ __ ___   ___  | |     _ ___| |_       ;
;  \___ \ / __| '_ \ / _ \ '_ ` _ \ / _ \ | |    | / __| __|      ;
;  ____) | (__| | | |  __/ | | | | |  __/ | |____| \__ \ |_       ;
; |_____/ \___|_| |_|\___|_| |_| |_|\___| |______|_|___/\__|      ;
; |  \/  |           (_)           | |     | | (_)                ;
; | \  / | __ _ _ __  _ _ __  _   _| | __ _| |_ _  ___  _ __  ___ ;
; | |\/| |/ _` | '_ \| | '_ \| | | | |/ _` | __| |/ _ \| '_ \/ __|;
; | |  | | (_| | | | | | |_) | |_| | | (_| | |_| | (_) | | | \__ \;
; |_|  |_|\__,_|_| |_|_| .__/ \__,_|_|\__,_|\__|_|\___/|_| |_|___/;
;                      | |                                        ;
;                      |_|                                        ;
;=================================================================;

(define (list-extract-evens integer-list)
  (if (null? integer-list) '()
    (if (not (list? integer-list)) integer-list
      (if (even? (car integer-list))
        (cons (car integer-list) (list-extract-evens (cdr integer-list)))
        (list-extract-evens (cdr integer-list))))))
        
(define (list-extract-odds integer-list)
  (if (null? integer-list) '()
    (if (not (list? integer-list)) integer-list
      (if (odd? (car integer-list))
        (cons (car integer-list) (list-extract-odds (cdr integer-list)))
        (list-extract-odds (cdr integer-list))))))

(define (vowel? var)
  (cond ((equal? var '#\a) #t)
        ((equal? var '#\e) #t)
        ((equal? var '#\i) #t)
        ((equal? var '#\o) #t)
        ((equal? var '#\u) #t)
        (else #f)))

(define (nonvowel? var) (not (vowel? var)))

(define (list-extract-vowels char-list)
  (if (null? char-list) '()
    (if (not (list? char-list)) char-list
      (if (vowel? (car char-list))
        (cons (car char-list) (list-extract-vowels (cdr char-list)))
        (list-extract-vowels (cdr char-list))))))

(define (list-extract-consonants char-list)
  (if (null? char-list) '()
    (if (not (list? char-list)) char-list
      (if (nonvowel? (car char-list))
        (cons (car char-list) (list-extract-consonants (cdr char-list)))
        (list-extract-consonants (cdr char-list))))))

(define (list-extract-onset syllable)
  (cond ((null? syllable) '() )
        ((not (list? syllable)) syllable)
        ((vowel? (car syllable)) '())
        ((nonvowel? (car syllable)) (cons (car syllable) (list-extract-onset (cdr syllable))))))

(define (list-extract-rime syllable)
  (cond ((null? syllable) '())
        ((not (list syllable)) syllable)
        ((vowel? (car syllable)) syllable)
        (else (list-extract-rime (cdr syllable)))))

(define (list-pig-latinize var)
  (append (append (list-extract-rime var) (list-extract-onset var)) '(#\a #\y)))

(define (string-pig-latinize var)
  (list->string (list-pig-latinize (string->list var))))
