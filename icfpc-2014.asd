;;; -*- ICFP Contest 2014 -*-

(defpackage #:icfpc-2014-asd
  (:use :cl :asdf))

(in-package #:icfpc-2014-asd)

(defsystem icfpc-2014
  :name "icfpc-2014"
  :version "0.1"
  :author "skobochka"
  :depends-on (:gcc))
