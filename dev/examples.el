;; -*- lexical-binding: t; eval: (font-lock-add-keywords nil '(("defexamples\\|def-example-group\\| => " (0 'font-lock-keyword-face)))); -*-


(require 'po-mode)
(require 'assist-po-mode)

(def-example-group "Generate sequence"
  (defexamples generate-seq
    (generate-seq 1 5) => '(1 2 3 4 5)
    (generate-seq 3 1) => '(1)
    (generate-seq 1.4 3) => '(1 2 3)))
