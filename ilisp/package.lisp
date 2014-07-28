(cl:in-package #:cl-user)

(defpackage #:ilisp.impl
  (:use :cl :iterate :metatilities)
  (:shadowing-import-from :metatilities minimize finish)
  (:export #:build-ai-core))


