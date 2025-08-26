#lang racket

(require racket/date
         racket/format
         racket/string
         racket/list)

;; Compatibility for string-blank?
(define (string-blank? s)
  (or (string=? s "")
      (regexp-match? #px"^\\s*$" s)))


;; ------------------------------
;; Data models
;; ------------------------------
(struct expense  (id date amount method supplier category note) #:transparent)
(struct supplier (id name phone account)                         #:transparent)
(struct db       (expenses suppliers next-expense-id next-supplier-id) #:transparent)

;; ------------------------------
;; Database handling
;; ------------------------------
(define db-file "lipabill-data.rktd")

(define (save-db! D)
  (call-with-output-file db-file
    (lambda (out)
      (write D out))
    #:exists 'replace))

(define (load-db)
  (if (file-exists? db-file)
      (call-with-input-file db-file
        (lambda (in) (read in)))
      (db '() '() 1 1)))

;; ------------------------------
;; Prompt helper
;; ------------------------------
(define (prompt msg [default ""])
  (display (format "~a: " msg))
  (flush-output)
  (define input (read-line))
  (if (string-blank? input)
      default
      input))

;; ------------------------------
;; Supplier functions
;; ------------------------------
(define (add-supplier-cli D)
  (printf "\n-- Add Supplier --\n")
  (define name    (prompt "Name"))
  (define phone   (prompt "Phone"))
  (define account (prompt "Account/Paybill"))
  (define sup (supplier (db-next-supplier-id D) name phone account))
  (define new-suppliers (cons sup (db-suppliers D)))
  (define D* (db (db-expenses D) new-suppliers (db-next-expense-id D) (add1 (db-next-supplier-id D))))
  (save-db! D*)
  (printf "✅ Saved supplier ~a (id ~a)\n" name (supplier-id sup))
  D*)

(define (list-suppliers D)
  (printf "\n-- Suppliers --\n")
  (for ([s (in-list (reverse (db-suppliers D)))])
    (printf "#~a | ~a | ~a | ~a\n"
            (supplier-id s) (supplier-name s) (supplier-phone s) (supplier-account s)))
  D)

;; ------------------------------
;; Expense functions
;; ------------------------------
(define (add-expense-cli D)
  (printf "\n-- Add Expense --\n")
  (define date (prompt "Date YYYY-MM-DD" (format "~a-~a-~a" (date-year (current-date)) (date-month (current-date)) (date-day (current-date)))))
  (define amount-str (prompt "Amount (e.g., 1200.50)"))
  (define amount (string->number amount-str))
  (when (or (not amount) (negative? amount))
    (error 'add-expense "Amount must be a positive number"))
  (define method (string-downcase (prompt "Method (cash/mpesa/airtel/tkash)" "cash")))
  (define supplier-name (prompt "Supplier name (leave blank if none)" ""))
  (define category (prompt "Category (e.g., Inventory, Transport)" "General"))
  (define note (prompt "Note" ""))
  (define exp (expense (db-next-expense-id D) date amount method supplier-name category note))
  (define new-expenses (cons exp (db-expenses D)))
  (define D* (db new-expenses (db-suppliers D) (add1 (db-next-expense-id D)) (db-next-supplier-id D)))
  (save-db! D*)
  (printf "✅ Saved expense id ~a: ~a ~a to ~a (~a)\n"
          (expense-id exp) date amount supplier-name method)
  D*)

(define (list-expenses D)
  (printf "\n-- Expenses --\n")
  (for ([e (in-list (reverse (db-expenses D)))])
    (printf "#~a | ~a | ~a | ~a | ~a | ~a | ~a\n"
            (expense-id e)
            (expense-date e)
            (expense-amount e)
            (expense-method e)
            (expense-supplier e)
            (expense-category e)
            (expense-note e)))
  D)

;; ------------------------------
;; CLI Loop
;; ------------------------------
(define (main)
  (define D (load-db))
  (let loop ()
    (printf "\nLipabill CLI — choose an option:\n")
    (printf "1) Add supplier\n")
    (printf "2) List suppliers\n")
    (printf "3) Add expense\n")
    (printf "4) List expenses\n")
    (printf "0) Exit\n")
    (define choice (prompt "Enter choice" "0"))
    (define D* 
      (case (string->number choice)
        [(1) (add-supplier-cli D)]
        [(2) (list-suppliers D)]
        [(3) (add-expense-cli D)]
        [(4) (list-expenses D)]
        [(0) (begin (printf "Exiting...\n") (exit 0))]
        [else (begin (printf "Invalid choice\n") D)]))
    (loop)))

;; ------------------------------
;; Start program
;; ------------------------------
(main)
