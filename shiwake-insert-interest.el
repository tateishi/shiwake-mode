;;; shiwake-insert-interest.el --- PoC 用コード -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'cl)
(require 'org)
(require 'ledger-mode)
;;(require 'shiwake-mode)

(defvar shiwake--national-tax-ratio 15.315 "国税税率.")
(defvar shiwake--local-tax-ratio 5 "地方税税率.")

(defvar shiwake--tx-template)
(setq shiwake--tx-template
      "%1$s %2$s
    収入:投資収入:受取利息                       -%3$d %7$s
    支出:租税公課:源泉国税                        %4$d %7$s
    支出:租税公課:源泉地方税                      %5$d %7$s
    %6$s")

(defvar shiwake--tx-template-frac)
(setq shiwake--tx-template-frac
      "%1$s %2$s
    収入:投資収入:受取利息                       -%3$.2f %7$s
    支出:租税公課:源泉国税                        %4$.2f %7$s
    支出:租税公課:源泉地方税                      %5$.2f %7$s
    %6$s")

(defvar shiwake--commodities)
(setq shiwake--commodities
      '("" "USD" "EUR" "AUD" "NZD" "BRL" "ZAR"))

(defvar commodities-with-fractions)
(setq commodities-with-fractions
      '("USD" "EUR" "AUD" "NZD" "BRL" "ZAR"))


(defun shiwake--commoditi-has-fraction (commodity)
  "補助通貨がある場合 t その他の場合 nil.

COMMODITY: 判定する通貨"
  (member commodity commodities-with-fractions))


(defun shiwake--days-between (start-date end-date)
  "二つの日付の間の日数を計算する.

START-DATEからEND-DATEの間の日数を返す."
  (let ((start (date-to-time (replace-regexp-in-string "/" "-" start-date)))
        (end (date-to-time (replace-regexp-in-string "/" "-" end-date))))
    (- (time-to-days end) (time-to-days start))))


(defun shiwake--truncate (number)
  "小数点2桁で切り捨てた数を返す.

NUMBER の小数点2桁で切り捨てた数を返す."
  (/ (truncate (* number 100)) 100.0))


(defun shiwake--calc-tax (principal interest-ratio start-date end-date &optional has-fraction)
  "指定の条件で利息、国税、地方税を計算する.

PRINCIPAL: 元金
INTEREST-RATIO: 利率(%)
START-DATE: 預入日
END-DATE: 満期日
HAS-FRACTION: 補助通貨あり

返値は下記のリスト
日数 利息 国税 地方税 税引き後利息"
  (let ((trunc (if has-fraction
                   (lambda (num) (shiwake--truncate num))
                 (lambda (num) (truncate num)))))

    (let* ((days (shiwake--days-between start-date end-date))
           (interest (funcall trunc (/ (* principal interest-ratio 0.01 days) 365)))
           (national-tax (funcall trunc (* interest shiwake--national-tax-ratio 0.01)))
           (local-tax (funcall trunc (* interest shiwake--local-tax-ratio 0.01)))
           (interest-after-tax (- interest national-tax local-tax)))
      (list days interest national-tax local-tax interest-after-tax))))


(defun shiwake--interest-tx (principal interest-ratio start-date end-date payee account commodity)
  "トランザクションを出力する.

PRINCIPAL: 元金
INTEREST-RATIO: 利率(%)
START-DATE: 預入日
END-DATE: 満期日
PAYEE: 相手先
ACCOUNT: 科目
COMMODITY: 通貨"

;;      (template (replace-regexp-in-string "{}" (if has-fraction "%.2f" "%d") shiwake--tx-template))
  (let* (
         (has-fraction (shiwake--commoditi-has-fraction commodity))
         (template (if has-fraction shiwake--tx-template-frac shiwake--tx-template)))
    (cl-destructuring-bind (days interest national-tax local-tax interest-after-tax)
        (shiwake--calc-tax principal interest-ratio start-date end-date has-fraction)
;;      (message "%d %.4f %s %s %s %s %s %d %.2f %.2f %.2f %s" principal interest-ratio start-date end-date payee account commodity days interest national-tax local-tax has-fraction)
;;      (message "tmepl: %s" template)
      (format template end-date payee interest national-tax local-tax account commodity))))


(defun shiwake-insert-interest-transaction (principal commodity rate start-date end-date payee account)
  "利息受け取りの時の取引を記録.
PRINCIPAL: 元金
COMMODITY: 通貨
RATE: 利率
START-DATE: 預入日
END-DATE: 満期日
PAYEE: 相手先
ACCOUNT: 科目"
  (interactive (list
                (read-number "元金: ")
                (completing-read "通貨: " shiwake--commodities)
                (read-number "利率(%): ")
                (replace-regexp-in-string "-" "/" (org-read-date nil nil nil "預入日: "))
                (replace-regexp-in-string "-" "/" (org-read-date nil nil nil "満期日: "))
                (ledger-read-payee-with-prompt "相手先: ")
                (ledger-read-account-with-prompt "科目: ")))
;;  (message "%d %s %.4f %s %s %s %s" principal commodity rate start-date end-date payee account)
  (insert (shiwake--interest-tx principal rate start-date end-date payee account commodity))
  )

(provide 'shiwake-insert-interest)
;;; shiwake-insert-interest.el ends here
